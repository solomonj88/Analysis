# spotseq_run_analysis.R
# Run this in your Analysis project: source("spotseq_run_analysis.R")
# This script produces the filled Excel workbook and uploads it to Google Drive.
# It does NOT require the planning template file.

library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)
library(googledrive)
library(glue)

data_path       <- Sys.getenv("DATA_PATH")
drive_folder_id <- Sys.getenv("DRIVE_FOLDER_ID")

stopifnot(
  "DATA_PATH not set in .Renviron"       = nchar(data_path) > 0,
  "DRIVE_FOLDER_ID not set in .Renviron" = nchar(drive_folder_id) > 0
)

# ── Load & clean ──────────────────────────────────────────────────────────────
raw <- read_excel(
  file.path(data_path, "SpotSeq_Project_Dataset_deidentified_2.1.xlsx")
)
df_raw    <- raw |> clean_names()
df        <- df_raw |> filter(!is.na(patient_id) & patient_id != "")
n_raw     <- nrow(raw)
n_spacer  <- n_raw - nrow(df)
n_records <- nrow(df)
n_pts     <- n_distinct(df$patient_id)

cat(glue("Loaded {n_records} records ({n_pts} unique patients); {n_spacer} spacer rows excluded\n\n"))

# ── Specimen source standardisation ───────────────────────────────────────────
blood_vals <- toupper(c("Blood","Blood, venous","Lesional blood"))
lymph_vals <- toupper(c(
  "Lymphatic fluid","Lymphatic fluid, Site A","Lymphatic fluid, Site B",
  "Lymphatic fluid from left groin","Lymphatic cyst fluid",
  "Lymphatic cyst fluid from lymph node","Cyst fluid",
  "Lymphatic Malformation","Lymphatic malformation",
  "Chest, Lymphatic malformation"
))
other_vals <- toupper(c(
  "Aspirated Cyst Fluid","aspirated cyst fluid",
  "Aspirate from the cyst in the abdomen",
  "Abdomen, Lymphatic Cyst Fluid","Ankle, Left","Neck",
  "Tracheocutaneous fistula","Peritoneal fluid"
))

df <- df |>
  mutate(
    specimen_source_raw = toupper(trimws(specimen_source)),
    specimen_source_std = case_when(
      specimen_source_raw %in% blood_vals ~ "Blood",
      specimen_source_raw %in% lymph_vals ~ "Lymphatic Fluid",
      specimen_source_raw %in% other_vals ~ "Other",
      TRUE                                ~ NA_character_
    )
  )

# ── Result / validity classification ─────────────────────────────────────────
df <- df |>
  mutate(
    result_clean = toupper(trimws(result_pos_neg)),
    status_clean = toupper(trimws(status)),
    is_qns       = status_clean == "QNS" |
                   result_clean %in% c("QNS","QNS FOR REPEAT"),
    is_cancelled = status_clean %in% c("CANCELLED","CANCELED") |
                   result_clean %in% c("CANCELLED","CANCELED"),
    is_valid     = !is_qns & !is_cancelled &
                   status_clean == "COMPLETE" & !is.na(result_clean),
    is_pos       = is_valid & grepl("^POS", result_clean),
    is_neg       = is_valid & grepl("^NEG", result_clean),
    assay_std    = toupper(trimws(assay_ordered)),
    age_val      = suppressWarnings(as.numeric(age_at_date_of_collection_in_years)),
    sex_val      = assigned_sex_at_birth,
    internal_val = toupper(trimws(internal_patient_y_n)),
    vol_val      = suppressWarnings({
      v <- trimws(fluid_volume_extracted)
      ifelse(grepl("/", v),
             sapply(strsplit(v, "/"), function(x)
               sum(as.numeric(trimws(x)), na.rm = TRUE)),
             as.numeric(v))
    }),
    dna_conc_val = suppressWarnings(
      as.numeric(dna_stock_ng_ul_blood_0_185ng_ul_reject_cyst_fluid_0_37ng_ul_reject)
    ),
    dna_used_val = suppressWarnings(as.numeric(total_ng_dna_used_in_assay)),
    drop_val     = suppressWarnings(as.numeric(exon_21_wt_1500_avg_number_of_droplets)),
    ex21_val     = suppressWarnings(as.numeric(exon_21_wt_1500_avg_number_of_droplets)),
    ex10_val     = suppressWarnings(as.numeric(exon_10_wt_1500_avg_number_droplets))
  )

df_valid  <- df |> filter(is_valid)
df_qns    <- df |> filter(is_qns)
df_cancel <- df |> filter(is_cancelled)

# ── Table 1 ───────────────────────────────────────────────────────────────────
age_n      <- sum(!is.na(df$age_val))
age_mean   <- mean(df$age_val, na.rm = TRUE)
age_sd     <- sd(df$age_val,   na.rm = TRUE)
n_peds     <- sum(df$age_val <  18, na.rm = TRUE)
n_adult    <- sum(df$age_val >= 18, na.rm = TRUE)
pct_peds   <- n_peds  / age_n * 100
pct_adult  <- n_adult / age_n * 100
sex_clean  <- toupper(trimws(df$sex_val))
n_female   <- sum(sex_clean == "F", na.rm = TRUE)
sex_n      <- sum(!is.na(df$sex_val))
pct_female <- n_female / sex_n * 100
n_tests    <- nrow(df)
n_internal <- sum(df$internal_val == "Y", na.rm = TRUE)
pct_int    <- n_internal / n_tests * 100

fmt_np  <- function(n, p) sprintf("%d (%.1f%%)", as.integer(n), p)
fmt_ms  <- function(m, s) sprintf("%.1f (%.1f)", m, s)

# ── Table 2 ───────────────────────────────────────────────────────────────────
n_total     <- nrow(df)
n_valid     <- nrow(df_valid)
n_qns_n     <- nrow(df_qns)
n_can       <- nrow(df_cancel)
pct_qns     <- n_qns_n / n_total * 100
pct_can     <- n_can   / n_total * 100
n_pos_all   <- sum(df_valid$is_pos)
n_neg_all   <- sum(df_valid$is_neg)
pct_pos_all <- n_pos_all / n_valid * 100
pct_neg_all <- n_neg_all / n_valid * 100

assay_summary <- df_valid |>
  mutate(assay_grp = case_when(
    str_detect(assay_std, "PIK3CA|PIK") ~ "PIK3CA Multiplex",
    str_detect(assay_std, "TEK")        ~ "TEK",
    str_detect(assay_std, "BRAF")       ~ "BRAF",
    TRUE                                ~ "Other"
  )) |>
  group_by(assay_grp) |>
  summarise(n=n(), n_pos=sum(is_pos), n_neg=sum(is_neg), .groups="drop") |>
  mutate(pct_pos=n_pos/n*100, pct_neg=n_neg/n*100)

spec_summary <- df_valid |>
  filter(!is.na(specimen_source_std)) |>
  group_by(specimen_source_std) |>
  summarise(n=n(), n_pos=sum(is_pos), n_neg=sum(is_neg), .groups="drop") |>
  mutate(pct_pos=n_pos/n*100, pct_neg=n_neg/n*100)

enc_both <- df_valid |>
  group_by(patient_id, collection_date) |>
  summarise(types=n_distinct(specimen_source_std), .groups="drop") |>
  filter(types > 1) |> nrow()

df_valid2   <- df_valid |>
  mutate(spec_grp = case_when(
    specimen_source_std == "Blood"           ~ "Blood only",
    specimen_source_std == "Lymphatic Fluid" ~ "Lymphatic fluid only",
    TRUE                                     ~ "Other"
  ))
n_blood_only  <- sum(df_valid2$spec_grp == "Blood only",          na.rm=TRUE)
n_lymph_only  <- sum(df_valid2$spec_grp == "Lymphatic fluid only",na.rm=TRUE)
n_both        <- enc_both
pct_blood     <- n_blood_only / n_valid * 100
pct_lymph     <- n_lymph_only / n_valid * 100
pct_both      <- n_both       / n_valid * 100

vol_n    <- sum(!is.na(df_valid$vol_val))
conc_n   <- sum(!is.na(df_valid$dna_conc_val))
used_n   <- sum(!is.na(df_valid$dna_used_val))
drop_n   <- sum(!is.na(df_valid$drop_val))
med_vol  <- median(df_valid$vol_val,      na.rm=TRUE)
med_conc <- median(df_valid$dna_conc_val, na.rm=TRUE)
mean_used<- mean(df_valid$dna_used_val,   na.rm=TRUE)
mean_drop<- mean(df_valid$drop_val,       na.rm=TRUE)

cell2 <- function(n, p) sprintf("%d (%.1f%%)", as.integer(n), p)

# ── Table 3 ───────────────────────────────────────────────────────────────────
iqr_s <- function(x) {
  if (sum(!is.na(x))==0) return("—")
  q <- quantile(x, c(.25,.75), na.rm=TRUE)
  sprintf("%.1f (%.1f\u2013%.1f)", median(x,na.rm=TRUE), q[1], q[2])
}
msd_s <- function(x) {
  if (sum(!is.na(x))==0) return("—")
  sprintf("%.1f (%.1f)", mean(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}
spec_bk <- function(s, nn) {
  nb <- sum(s$specimen_source_std=="Blood",          na.rm=TRUE)
  nl <- sum(s$specimen_source_std=="Lymphatic Fluid",na.rm=TRUE)
  bt <- s |> group_by(patient_id,collection_date) |>
    summarise(t=n_distinct(specimen_source_std),.groups="drop") |>
    filter(t>1) |> nrow()
  list(
    blood = if(nn>0) cell2(nb,nb/nn*100) else "—",
    lymph = if(nn>0) cell2(nl,nl/nn*100) else "—",
    both  = if(nn>0) cell2(bt,bt/nn*100) else "—"
  )
}

t3 <- list()
for (ai in list(
  list(l="PIK3CA MULTIPLEX", p="PIK3CA|PIK", ex=TRUE),
  list(l="TEK",              p="TEK",         ex=FALSE),
  list(l="BRAF",             p="BRAF",        ex=FALSE)
)) {
  sub <- df_valid |> filter(str_detect(assay_std, ai$p))
  for (res in c("Pos","Neg")) {
    s  <- if(res=="Pos") filter(sub,is_pos) else filter(sub,is_neg)
    nn <- nrow(s); sp <- spec_bk(s,nn)
    t3[[length(t3)+1]] <- tibble(
      Assay                        = if(res=="Pos") ai$l else "",
      Result                       = res,
      `Fluid Vol (µL)`             = iqr_s(s$vol_val),
      `DNA Conc (ng/µL)`           = iqr_s(s$dna_conc_val),
      `DNA Used (ng)`              = msd_s(s$dna_used_val),
      `Droplets/Well`              = msd_s(s$drop_val),
      `EXON 21 WT med(IQR)`       = if(ai$ex) iqr_s(s$ex21_val) else "N/A",
      `EXON 10 WT med(IQR)`       = if(ai$ex) iqr_s(s$ex10_val) else "N/A",
      `Blood Only n(%)`            = sp$blood,
      `Lymphatic Fluid Only n(%)`  = sp$lymph,
      `Both n(%)`                  = sp$both
    )
  }
}
table3_df <- bind_rows(t3)

# ── Print summary to console ──────────────────────────────────────────────────
cat("=== TABLE 1 ===\n")
cat("Patients:", n_pts, "| Tests:", n_tests, "\n")
cat("Age:", fmt_ms(age_mean, age_sd), "(N=", age_n, ")\n")
cat("Female:", fmt_np(n_female, pct_female), "(N=", sex_n, ")\n")
cat("Internal:", fmt_np(n_internal, pct_int), "\n\n")

cat("=== TABLE 2 ===\n")
cat("Valid:", n_valid, "| QNS:", n_qns_n, "| Cancelled:", n_can, "\n")
cat("POS:", n_pos_all, "| NEG:", n_neg_all, "\n")
print(assay_summary); print(spec_summary)

cat("\n=== TABLE 3 ===\n")
print(table3_df, width=120)

# ── Build Excel ───────────────────────────────────────────────────────────────
wb      <- createWorkbook()
bld     <- createStyle(textDecoration="bold", fontSize=12)
sec_st  <- createStyle(textDecoration="bold", fgFill="#D9E1F2")
norm_st <- createStyle(fontSize=11)
fn_st   <- createStyle(fontSize=9, fontColour="#595959", wrapText=TRUE)
title_st<- createStyle(textDecoration="bold", fontSize=13)

wr <- function(sheet, row, col, val)
  writeData(wb, sheet, val, startRow=row, startCol=col, colNames=FALSE)

# --- Table 1 sheet ---
addWorksheet(wb, "Table 1")
setColWidths(wb, "Table 1", cols=1:3, widths=c(44,20,20))
wr("Table 1",1,1, glue("Table 1. Patient and Cohort Characteristics (N={n_records}; {n_pts} unique patients)"))
addStyle(wb,"Table 1",title_st,rows=1,cols=1); mergeCells(wb,"Table 1",1:3,1)
mapply(wr, "Table 1", 3, 1:3, list("Characteristic","n (%)","Value"))
addStyle(wb,"Table 1",bld,rows=3,cols=1:3,gridExpand=TRUE)

t1 <- list(
  list("Demographics","",""),
  list("  Total patients", as.character(n_pts), ""),
  list("  Age, mean (SD), years","", fmt_ms(age_mean,age_sd)),
  list("  Age group","",""),
  list("    Pediatric (< 18 years)", fmt_np(n_peds,pct_peds), ""),
  list("    Adult (\u2265 18 years)", fmt_np(n_adult,pct_adult), ""),
  list("  Female sex", fmt_np(n_female,pct_female), ""),
  list("Testing Overview","",""),
  list("  Total tests ordered", as.character(n_tests), ""),
  list("  Internally processed", fmt_np(n_internal,pct_int), "")
)
for (i in seq_along(t1)) {
  r  <- i+3; st <- if(i %in% c(1,8)) sec_st else norm_st
  mapply(wr,"Table 1", r, 1:3, t1[[i]])
  addStyle(wb,"Table 1",st,rows=r,cols=1:3,gridExpand=TRUE)
}
fn <- length(t1)+5
wr("Table 1",fn,1,"SD = standard deviation. Age at date of collection. Internally processed = run in-house.")
addStyle(wb,"Table 1",fn_st,rows=fn,cols=1); mergeCells(wb,"Table 1",1:3,fn)

# --- Table 2 sheet ---
addWorksheet(wb,"Table 2")
setColWidths(wb,"Table 2",cols=1:5,widths=c(46,12,18,18,22))
wr("Table 2",1,1,"Table 2. Test Volume, Validity, and Results by Assay and Specimen Type")
addStyle(wb,"Table 2",title_st,rows=1,cols=1); mergeCells(wb,"Table 2",1:5,1)
mapply(wr,"Table 2",3,1:5,list("Category","Total n","Positive n (%)","Negative n (%)","QNS/Cancelled n (%)"))
addStyle(wb,"Table 2",bld,rows=3,cols=1:5,gridExpand=TRUE)

ga <- function(name) {
  row <- assay_summary |> filter(assay_grp==name)
  if(nrow(row)==0) return(list(paste0("  ",name),"—","—","—","—"))
  list(paste0("  ",name), as.character(row$n),
       cell2(row$n_pos,row$pct_pos), cell2(row$n_neg,row$pct_neg),"—")
}
gs <- function(name, label) {
  row <- spec_summary |> filter(specimen_source_std==name)
  if(nrow(row)==0) return(list(label,"—","—","—","—"))
  list(label, as.character(row$n),
       cell2(row$n_pos,row$pct_pos), cell2(row$n_neg,row$pct_neg),"—")
}

t2 <- list(
  list("Test Validity","","","",""),
  list("  Total tests ordered", as.character(n_total),"","",""),
  list("  Analyzable (valid)",  as.character(n_valid),
       cell2(n_pos_all,pct_pos_all), cell2(n_neg_all,pct_neg_all),"—"),
  list("  QNS",       as.character(n_qns_n),"—","—",cell2(n_qns_n,pct_qns)),
  list("  Cancelled", as.character(n_can),  "—","—",cell2(n_can,pct_can)),
  list("Results by Assay (valid tests only)","","","",""),
  ga("PIK3CA Multiplex"), ga("TEK"), ga("BRAF"),
  list("Results by Specimen Type (valid tests only)","","","",""),
  list("  Blood only",              as.character(n_blood_only),cell2(n_blood_only,pct_blood),"—","—"),
  list("  Lymphatic fluid only",    as.character(n_lymph_only),cell2(n_lymph_only,pct_lymph),"—","—"),
  list("  Both blood & lymphatic",  as.character(n_both),      cell2(n_both,pct_both),       "—","—"),
  list("Lab QC Metrics (valid tests only)","","","",""),
  list(sprintf("  Median fluid volume (N=%d/%d)",vol_n,n_valid),
       sprintf("%.1f \u00b5L",med_vol),"","",""),
  list(sprintf("  Median DNA stock conc. (N=%d/%d)",conc_n,n_valid),
       sprintf("%.2f ng/\u00b5L",med_conc),"","",""),
  list(sprintf("  Mean total DNA used (N=%d/%d)",used_n,n_valid),
       sprintf("%.1f ng",mean_used),"","",""),
  list(sprintf("  Mean droplets/well (N=%d/%d)",drop_n,n_valid),
       sprintf("%.0f",mean_drop),"","",""),
  list("Results by Specimen Source Category (valid tests only)","","","",""),
  gs("Blood",          "  Blood (venous / lesional)"),
  gs("Lymphatic Fluid","  Lymphatic fluid"),
  gs("Other",          "  Other anatomic site")
)
sec_rows_t2 <- c(1,6,10,14,19)
for (i in seq_along(t2)) {
  r <- i+3; st <- if(i %in% sec_rows_t2) sec_st else norm_st
  mapply(wr,"Table 2",r,1:5,t2[[i]])
  addStyle(wb,"Table 2",st,rows=r,cols=1:5,gridExpand=TRUE)
}
fn2 <- length(t2)+5
wr("Table 2",fn2,1,"QNS = quantity not sufficient. % for QNS/cancelled = of total tests; all other % = of valid tests.")
addStyle(wb,"Table 2",fn_st,rows=fn2,cols=1); mergeCells(wb,"Table 2",1:5,fn2)

# --- Table 3 sheet ---
addWorksheet(wb,"Table 3")
setColWidths(wb,"Table 3",cols=1:11,widths=c(20,8,15,15,13,14,19,19,13,19,10))
wr("Table 3",1,1,"Table 3. Test Performance by Assay Type")
addStyle(wb,"Table 3",title_st,rows=1,cols=1)
hdrs <- c("Assay","Result","Fluid Vol (µL)","DNA Conc (ng/µL)","DNA Used (ng)",
          "Droplets/Well","EXON 21 WT med(IQR)","EXON 10 WT med(IQR)",
          "Blood Only n(%)","Lymphatic Only n(%)","Both n(%)")
for(j in seq_along(hdrs)) wr("Table 3",3,j,hdrs[j])
addStyle(wb,"Table 3",bld,rows=3,cols=1:11,gridExpand=TRUE)
for(i in seq_len(nrow(table3_df))) {
  for(j in 1:11) wr("Table 3",i+3,j,table3_df[[j]][i])
  addStyle(wb,"Table 3",norm_st,rows=i+3,cols=1:11,gridExpand=TRUE)
}
fn3 <- nrow(table3_df)+5
wr("Table 3",fn3,1,
   "IQR = interquartile range. EXON 21/10 WT droplets apply to PIK3CA Multiplex only (N/A for TEK and BRAF). Values: median (IQR) or mean (SD).")
addStyle(wb,"Table 3",fn_st,rows=fn3,cols=1); mergeCells(wb,"Table 3",1:11,fn3)

# ── Save & upload ─────────────────────────────────────────────────────────────
out_file <- file.path(tempdir(), paste0("SpotSeq_Tables_", Sys.Date(), ".xlsx"))
saveWorkbook(wb, out_file, overwrite=TRUE)
cat("\n\u2705 Excel saved to:", out_file, "\n")

drive_auth(scopes="https://www.googleapis.com/auth/drive", cache="~/.gargle-cache")
drive_upload(
  media     = out_file,
  path      = as_id(drive_folder_id),
  name      = paste0("SpotSeq_Tables_", Sys.Date(), ".xlsx"),
  overwrite = TRUE
)
cat("\u2705 Uploaded to Google Drive folder:", drive_folder_id, "\n")
cat("\n\U0001f516 GitHub checkpoint:\n")
cat('  git add . && git commit -m "feat: complete Tables 1-3 with correct column mappings"\n')
