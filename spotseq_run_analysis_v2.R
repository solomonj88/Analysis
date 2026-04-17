# spotseq_run_analysis_v2.R
# Run this in your Analysis project: source("spotseq_run_analysis_v2.R")
# Produces a dated Excel workbook with Tables 1-4 and uploads to Google Drive.
# Also saves 4 Word (.docx) files to SpotSeq_Word_Tables/ folder.

library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)
library(googledrive)
library(flextable)
library(officer)
library(glue)

data_path       <- Sys.getenv("DATA_PATH")
drive_folder_id <- Sys.getenv("DRIVE_FOLDER_ID")

stopifnot(
  "DATA_PATH not set in .Renviron"      = nchar(data_path) > 0,
  "DRIVE_FOLDER_ID not set in .Renviron" = nchar(drive_folder_id) > 0
)

# =============================================================================
# Load & clean
# =============================================================================
raw <- read_excel(
  file.path(data_path, "SpotSeq_Project_Dataset_deidentified_2.1.xlsx"),
  col_types = "text"
)

df_raw    <- raw |> clean_names()
df        <- df_raw |> filter(!is.na(patient_id) & patient_id != "")
n_raw     <- nrow(raw)
n_spacer  <- n_raw - nrow(df)
n_records <- nrow(df)
n_pts     <- n_distinct(df$patient_id)
cat(glue("Loaded {n_records} records ({n_pts} unique patients); {n_spacer} spacer rows excluded\n\n"))

# =============================================================================
# Specimen source standardisation
# =============================================================================
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

# =============================================================================
# Result / validity classification
# =============================================================================
df <- df |>
  mutate(
    result_clean = toupper(trimws(result_pos_neg)),
    status_clean = toupper(trimws(status)),
    is_qns       = status_clean %in% c("QNS","QNS FOR REPEAT","REPEAT") |
                   result_clean %in% c("QNS","QNS FOR REPEAT"),
    is_cancelled = status_clean %in% c("CANCELLED","CANCELED") |
                   result_clean %in% c("CANCELLED","CANCELED"),
    is_blank     = is.na(result_pos_neg) | trimws(result_pos_neg) == "",
    is_valid     = !is_qns & !is_cancelled & !is_blank &
                   !is.na(result_clean) & result_clean != "" &
                   (grepl("^POS", result_clean) | grepl("^NEG", result_clean)),
    is_pos       = is_valid & grepl("^POS", result_clean),
    is_neg       = is_valid & grepl("^NEG", result_clean),
    assay_std    = toupper(trimws(assay_ordered)),
    age_days     = suppressWarnings(as.numeric(age_at_date_of_collection_in_days)),
    age_years    = age_days / 365.25,
    sex_val      = assigned_sex_at_birth,
    internal_val = toupper(trimws(internal_patient_y_n)),
    vol_val      = suppressWarnings(as.numeric(fluid_volume_extracted_converted)),
    dna_conc_val = suppressWarnings(as.numeric(
                     dna_stock_ng_ul_blood_0_185ng_ul_reject_cyst_fluid_0_37ng_ul_reject)),
    dna_used_val = suppressWarnings(as.numeric(total_ng_dna_used_in_assay)),
    ex21_val     = suppressWarnings(as.numeric(exon_21_wt_1500_avg_number_of_droplets)),
    ex10_val     = suppressWarnings(as.numeric(exon_10_wt_1500_avg_number_droplets)),
    specimen_type_std = case_when(
      toupper(trimws(specimen_type)) == "BLOOD"     ~ "Blood",
      grepl("CYST", toupper(trimws(specimen_type))) ~ "Cyst Fluid",
      TRUE                                          ~ "Other"
    ),
    mgmt_change        = toupper(trimws(any_documented_management_change_in_management_post_testing_y_n)),
    therapy_initiated  = toupper(trimws(targeted_therapy_initiated_post_testing_e_g_alpelisib_sirolimus_y_n)),
    therapy_modified   = toupper(trimws(existing_therapy_modified_or_dose_adjusted_post_testing_y_n_na)),
    procedure_changed  = toupper(trimws(surgical_or_procedural_plan_changed_post_testing_y_n_na)),
    additional_testing = toupper(trimws(patient_underwent_additional_testing_based_on_spot_seq_results_y_n_van)),
    mgmt_unavailable   = toupper(trimws(management_documentation_post_testing_unavailable_not_reported_y_n_na)),
    record_category = case_when(
      is_valid     ~ "valid",
      is_qns       ~ "qns",
      is_cancelled ~ "cancelled",
      is_blank     ~ "blank",
      TRUE         ~ "unclassified"
    )
  )

df_valid  <- df |> filter(record_category == "valid")
df_qns    <- df |> filter(record_category == "qns")
df_cancel <- df |> filter(record_category == "cancelled")
n_valid   <- nrow(df_valid)
n_qns_n   <- nrow(df_qns)
n_can     <- nrow(df_cancel)

# Deduplicate to one row per patient (first row per sample_id_for_di_dataset)
df_patients <- df_valid |>
  mutate(row_order = row_number()) |>
  group_by(patient_id) |>
  slice_min(row_order, n = 1, with_ties = FALSE) |>
  ungroup()

n_pts_valid <- n_distinct(df_patients$patient_id)



# =============================================================================
# Helper functions
# =============================================================================
fmt_ms  <- function(m, s) sprintf("%.1f (%.1f)", m, s)
fmt_med <- function(med, lo, hi) sprintf("%.1f [%.1f-%.1f]", med, lo, hi)
fmt_np  <- function(n, p) sprintf("%d (%.1f%%)", as.integer(n), p)
cell2   <- function(n, p) sprintf("%d (%.1f%%)", as.integer(n), p)
iqr_s   <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("--")
  sprintf("%.1f [%.1f-%.1f]", median(x), quantile(x,.25), quantile(x,.75))
}
msd_s <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("--")
  sprintf("%.1f (%.1f)", mean(x), sd(x))
}

# =============================================================================
# TABLE 1
# =============================================================================
age_n     <- sum(!is.na(df_patients$age_years))
age_mean  <- mean(df_patients$age_years,   na.rm=TRUE)
age_sd    <- sd(df_patients$age_years,     na.rm=TRUE)
age_med   <- median(df_patients$age_years, na.rm=TRUE)
age_q1    <- quantile(df_patients$age_years, .25, na.rm=TRUE)
age_q3    <- quantile(df_patients$age_years, .75, na.rm=TRUE)
age_min   <- min(df_patients$age_years,    na.rm=TRUE)
age_max   <- max(df_patients$age_years,    na.rm=TRUE)
n_peds    <- sum(df_patients$age_years < 18,  na.rm=TRUE)
n_adult   <- sum(df_patients$age_years >= 18, na.rm=TRUE)
pct_peds  <- n_peds  / age_n * 100
pct_adult <- n_adult / age_n * 100

sex_clean  <- toupper(trimws(df_patients$sex_val))
sex_n      <- sum(!is.na(df_patients$sex_val))
n_female   <- sum(sex_clean == "F", na.rm=TRUE)
pct_female <- n_female / sex_n * 100

internal_clean <- toupper(trimws(df_patients$internal_val))
n_internal     <- sum(internal_clean == "Y", na.rm=TRUE)
n_external     <- sum(internal_clean == "N", na.rm=TRUE)
n_int_ext      <- sum(!is.na(df_patients$internal_val) & df_patients$internal_val != "")
pct_int        <- if(n_int_ext>0) n_internal/n_int_ext*100 else 0
pct_ext        <- if(n_int_ext>0) n_external/n_int_ext*100 else 0

t1 <- list(
  list("Demographics",                          "",                  ""),
  list("  Total patients",                      as.character(n_pts), ""),
  list("  Age, mean (SD), years",               "",                  fmt_ms(age_mean, age_sd)),
  list("  Age, median [IQR]",                   "",                  fmt_med(age_med, age_q1, age_q3)),
  list("  Age group",                           "",                  ""),
  list("    Pediatric (< 18 years)",            "",                  fmt_np(n_peds, pct_peds)),
  list("    Adult (>=18 years)",                "",                  fmt_np(n_adult, pct_adult)),
  list("  Female sex assigned at birth, n (%)", "",                  fmt_np(n_female, pct_female)),
  list("Patient Origin",                        "",                  ""),
  list("  Internally processed",                "",                  fmt_np(n_internal, pct_int)),
  list("  Externally referred",                 "",                  fmt_np(n_external, pct_ext)),
  list("Testing Overview",                      "",                  ""),
  list("  Total tests ordered",                 "",                  as.character(n_records)),
  list("  Unique patients",                     "",                  as.character(n_pts)),
  list("  Internally processed tests",          "",                  fmt_np(n_internal, pct_int))
)
sec_rows_t1 <- c(1, 9, 12)

# =============================================================================
# TABLE 2
# =============================================================================
n_pos_all   <- sum(df_valid$is_pos, na.rm=TRUE)
n_neg_all   <- sum(df_valid$is_neg, na.rm=TRUE)
pct_pos_all <- n_pos_all / n_valid * 100
pct_neg_all <- n_neg_all / n_valid * 100
pct_qns     <- n_qns_n / n_records * 100
pct_can     <- n_can   / n_records * 100

assay_summary <- df_valid |>
  mutate(assay_grp = case_when(
    str_detect(assay_std,"PIK3CA|PIK") ~ "PIK3CA Multiplex",
    str_detect(assay_std,"TEK")        ~ "TEK",
    str_detect(assay_std,"BRAF")       ~ "BRAF",
    TRUE                               ~ "Other"
  )) |>
  group_by(assay_grp) |>
  summarise(n=n(), n_pos=sum(is_pos), n_neg=sum(is_neg), .groups="drop") |>
  mutate(pct_pos=n_pos/n*100, pct_neg=n_neg/n*100)

spec_summary <- df_valid |>
  filter(!is.na(specimen_source_std)) |>
  group_by(specimen_source_std) |>
  summarise(n=n(), n_pos=sum(is_pos), n_neg=sum(is_neg), .groups="drop") |>
  mutate(pct_pos=n_pos/n*100, pct_neg=n_neg/n*100)

type_summary <- df_valid |>
  filter(!is.na(specimen_type_std)) |>
  group_by(specimen_type_std) |>
  summarise(n=n(), n_pos=sum(is_pos), n_neg=sum(is_neg), .groups="drop") |>
  mutate(pct_pos=n_pos/n*100, pct_neg=n_neg/n*100)

# Fluid volume: deduplicate df_valid by patient_id, exclude zeros and blanks
df_vol <- df_valid |>
  filter(!is.na(vol_val) & vol_val > 0) |>
  group_by(patient_id) |>
  slice_min(row_number(), n = 1, with_ties = FALSE) |>
  ungroup()
vol_n   <- nrow(df_vol)
med_vol <- median(df_vol$vol_val, na.rm = TRUE)

# DNA and droplet metrics at test level
conc_n    <- sum(!is.na(df_valid$dna_conc_val))
used_n    <- sum(!is.na(df_valid$dna_used_val))
med_conc  <- median(df_valid$dna_conc_val, na.rm=TRUE)
mean_used <- mean(df_valid$dna_used_val,   na.rm=TRUE)
sd_used   <- sd(df_valid$dna_used_val,     na.rm=TRUE)
mean_ex21 <- mean(df_valid$ex21_val, na.rm=TRUE)
sd_ex21   <- sd(df_valid$ex21_val,   na.rm=TRUE)
mean_ex10 <- mean(df_valid$ex10_val, na.rm=TRUE)
sd_ex10   <- sd(df_valid$ex10_val,   na.rm=TRUE)

# Fluid volume pos/neg — unique sample_id_for_di_dataset, exclude zeros/blanks
df_vol_pos  <- df_valid |> filter(is_pos, !is.na(vol_val), vol_val > 0) |>
               distinct(sample_id_for_di_dataset, .keep_all = TRUE)
df_vol_neg  <- df_valid |> filter(is_neg, !is.na(vol_val), vol_val > 0) |>
               distinct(sample_id_for_di_dataset, .keep_all = TRUE)
med_vol_pos <- median(df_vol_pos$vol_val, na.rm=TRUE)
med_vol_neg <- median(df_vol_neg$vol_val, na.rm=TRUE)
vol_pos_n   <- nrow(df_vol_pos)
vol_neg_n   <- nrow(df_vol_neg)

# DNA/EXON pos/neg — all valid tests by result
med_conc_pos  <- median(df_valid$dna_conc_val[df_valid$is_pos], na.rm=TRUE)
med_conc_neg  <- median(df_valid$dna_conc_val[df_valid$is_neg], na.rm=TRUE)
mean_used_pos <- mean(df_valid$dna_used_val[df_valid$is_pos],   na.rm=TRUE)
sd_used_pos   <- sd(df_valid$dna_used_val[df_valid$is_pos],     na.rm=TRUE)
mean_used_neg <- mean(df_valid$dna_used_val[df_valid$is_neg],   na.rm=TRUE)
sd_used_neg   <- sd(df_valid$dna_used_val[df_valid$is_neg],     na.rm=TRUE)
mean_ex21_pos <- mean(df_valid$ex21_val[df_valid$is_pos],       na.rm=TRUE)
sd_ex21_pos   <- sd(df_valid$ex21_val[df_valid$is_pos],         na.rm=TRUE)
mean_ex21_neg <- mean(df_valid$ex21_val[df_valid$is_neg],       na.rm=TRUE)
sd_ex21_neg   <- sd(df_valid$ex21_val[df_valid$is_neg],         na.rm=TRUE)
mean_ex10_pos <- mean(df_valid$ex10_val[df_valid$is_pos],       na.rm=TRUE)
sd_ex10_pos   <- sd(df_valid$ex10_val[df_valid$is_pos],         na.rm=TRUE)
mean_ex10_neg <- mean(df_valid$ex10_val[df_valid$is_neg],       na.rm=TRUE)
sd_ex10_neg   <- sd(df_valid$ex10_val[df_valid$is_neg],         na.rm=TRUE)

ga <- function(name) {
  row <- assay_summary |> filter(assay_grp == name)
  if (!nrow(row)) return(list(paste0("  ",name),"--","--","--"))
  list(paste0("  ",name), as.character(row$n),
       cell2(row$n_pos,row$pct_pos), cell2(row$n_neg,row$pct_neg))
}
gtype <- function(name, label) {
  row <- type_summary |> filter(specimen_type_std == name)
  if (!nrow(row)) return(list(label,"--","--","--"))
  list(label, as.character(row$n),
       cell2(row$n_pos,row$pct_pos), cell2(row$n_neg,row$pct_neg))
}
gs <- function(name, label) {
  row <- spec_summary |> filter(specimen_source_std == name)
  if (!nrow(row)) return(list(label,"--","--","--"))
  list(label, as.character(row$n),
       cell2(row$n_pos,row$pct_pos), cell2(row$n_neg,row$pct_neg))
}

t2 <- list(
  list("Test Validity",                       "",                     "",                           ""),
  list("  Total tests ordered",               as.character(n_records),"",                           ""),
  list("  Analyzable (valid)", fmt_np(n_valid, n_valid/n_records*100),  cell2(n_pos_all,pct_pos_all), cell2(n_neg_all,pct_neg_all)),
  list("  QNS", fmt_np(n_qns_n, n_qns_n/n_records*100), "", ""),
  list("  Cancelled", fmt_np(n_can, n_can/n_records*100), "", ""),
  list("Results by Assay (valid tests only)", "",                     "",                           ""),
  ga("PIK3CA Multiplex"), ga("TEK"), ga("BRAF"),
  list("Specimen Type (valid tests only)",    "",                     "",                           ""),
  gtype("Blood",      "  Blood"),
  gtype("Cyst Fluid", "  Cyst Fluid"),
  gtype("Other",      "  Other"),
  list("Specimen Source (valid tests only)",  "",                     "",                           ""),
  gs("Blood",           "  Blood"),
  gs("Lymphatic Fluid", "  Lymphatic Fluid"),
  gs("Other",           "  Other"),
  list("Specimen Collection Metrics",         "",                     "",                           ""),
  list(sprintf("  Fluid volume, mL -- median (N=%d patients)", vol_n),
       sprintf("%.1f", med_vol),
       sprintf("%.1f (N=%d)", med_vol_pos, vol_pos_n),
       sprintf("%.1f (N=%d)", med_vol_neg, vol_neg_n)),
  list(sprintf("  DNA concentration, ng/uL -- median (N=%d)", conc_n),
       sprintf("%.1f", med_conc),
       sprintf("%.1f", med_conc_pos),
       sprintf("%.1f", med_conc_neg)),
  list(sprintf("  DNA used in assay, ng -- mean (SD) (N=%d)", used_n),
       fmt_ms(mean_used, sd_used),
       fmt_ms(mean_used_pos, sd_used_pos),
       fmt_ms(mean_used_neg, sd_used_neg)),
  list(sprintf("  EXON 21 WT droplets -- mean (SD) (N=%d)", sum(!is.na(df_valid$ex21_val))),
       fmt_ms(mean_ex21, sd_ex21),
       fmt_ms(mean_ex21_pos, sd_ex21_pos),
       fmt_ms(mean_ex21_neg, sd_ex21_neg)),
  list(sprintf("  EXON 10 WT droplets -- mean (SD) (N=%d)", sum(!is.na(df_valid$ex10_val))),
       fmt_ms(mean_ex10, sd_ex10),
       fmt_ms(mean_ex10_pos, sd_ex10_pos),
       fmt_ms(mean_ex10_neg, sd_ex10_neg))
)
sec_rows_t2 <- c(1, 6, 10, 14, 18)

# =============================================================================
# TABLE 3
# =============================================================================
assay_list <- list(
  list(l="PIK3CA Multiplex", p="PIK3CA|PIK", ex=TRUE),
  list(l="TEK",              p="TEK",        ex=FALSE),
  list(l="BRAF",             p="BRAF",       ex=FALSE)
)

t3_rows <- list()
for (ai in assay_list) {
  sub <- df_valid |> filter(str_detect(assay_std, ai$p))
  nn  <- nrow(sub)
  n_bl  <- sum(sub$specimen_type_std=="Blood",      na.rm=TRUE)
  n_ly  <- sum(sub$specimen_type_std=="Cyst Fluid", na.rm=TRUE)
  n_oth <- sum(sub$specimen_type_std=="Other",      na.rm=TRUE)
  pct_bl  <- if(nn>0) n_bl/nn*100 else 0
  pct_ly  <- if(nn>0) n_ly/nn*100 else 0
  pct_oth <- if(nn>0) n_oth/nn*100 else 0

  t3_rows[[length(t3_rows)+1]] <- list(ai$l,"","","","","","","","","","")

  for (res in c("Positive","Negative")) {
    s       <- if(res=="Positive") filter(sub,is_pos) else filter(sub,is_neg)
    n_res   <- nrow(s)
    pct_res <- if(nn>0) n_res/nn*100 else 0
    # Fluid vol: unique sample_id_for_di_dataset, exclude zeros/blanks
    s_vol <- s |> filter(!is.na(vol_val) & vol_val > 0) |>
                  distinct(sample_id_for_di_dataset, .keep_all = TRUE)
    t3_rows[[length(t3_rows)+1]] <- list(
      "", res, fmt_np(n_res, pct_res),
      iqr_s(s_vol$vol_val), iqr_s(s$dna_conc_val), msd_s(s$dna_used_val),
      if(ai$ex) iqr_s(s$ex21_val) else "N/A",
      if(ai$ex) iqr_s(s$ex10_val) else "N/A",
      fmt_np(n_bl, pct_bl), fmt_np(n_ly, pct_ly), fmt_np(n_oth, pct_oth)
    )
  }
}
table3_df <- t3_rows
sec_rows_t3 <- which(sapply(table3_df, function(r) r[[1]] != ""))

# =============================================================================
# TABLE 4
# =============================================================================
df_t4      <- df_valid |> filter(is_pos | is_neg)
all_mgmt   <- toupper(trimws(df_t4$mgmt_change))
n_t4_total <- sum(!is.na(all_mgmt) & all_mgmt != "" & all_mgmt != "NA")

count_yn <- function(col, values_to_count = "Y") {
  vals  <- toupper(trimws(col))
  nb    <- vals[!is.na(vals) & vals != "" & vals != "NA"]
  n_yes <- sum(nb %in% values_to_count)
  pct   <- if(n_t4_total > 0) n_yes / n_t4_total * 100 else 0
  list(n = n_yes, fmt = fmt_np(n_yes, pct))
}

mgmt_res <- count_yn(df_t4$mgmt_change)
ti_res   <- count_yn(df_t4$therapy_initiated)
tm_res   <- count_yn(df_t4$therapy_modified)
pc_res   <- count_yn(df_t4$procedure_changed)
no_res   <- count_yn(df_t4$mgmt_change, values_to_count="N")
un_res   <- count_yn(df_t4$mgmt_unavailable)
at_res   <- count_yn(df_t4$additional_testing, values_to_count=c("Y","VAN"))

t4 <- list(
  list("  Any documented management change",                    mgmt_res$fmt),
  list("    Targeted therapy initiated (e.g., alpelisib, sirolimus)", ti_res$fmt),
  list("    Existing therapy modified or dose-adjusted",        tm_res$fmt),
  list("    Surgical or procedural plan changed",               pc_res$fmt),
  list("  No documented management change",                     no_res$fmt),
  list("  Management documentation unavailable / not reported", un_res$fmt),
  list("Patients who underwent additional testing",             at_res$fmt)
)

# =============================================================================
# Console summary
# =============================================================================
cat("=== TABLE 1 ===\n")
for (r in t1) cat(sprintf("%-45s %s %s\n", r[[1]], r[[2]], r[[3]]))
cat("\n=== TABLE 2 ===\n")
for (r in t2) cat(sprintf("%-50s %-12s %-18s %s\n", r[[1]], r[[2]], r[[3]], r[[4]]))
cat("\n=== TABLE 3 ===\n")
for (r in table3_df) cat(paste(sapply(r, as.character), collapse=" | "), "\n")
cat("\n=== TABLE 4 ===\n")
for (r in t4) cat(sprintf("%-55s %s\n", r[[1]], r[[2]]))

# =============================================================================
# Build Excel workbook
# =============================================================================
wb       <- createWorkbook()
bld      <- createStyle(textDecoration="bold", fontSize=12)
sec_st   <- createStyle(textDecoration="bold", fgFill="#D9E1F2")
norm_st  <- createStyle(fontSize=11)
fn_st    <- createStyle(fontSize=9, fontColour="#595959", wrapText=TRUE)
title_st <- createStyle(textDecoration="bold", fontSize=13)
wr       <- function(sheet, row, col, val)
              writeData(wb, sheet, val, startRow=row, startCol=col, colNames=FALSE)

# Table 1
addWorksheet(wb, "Table 1")
setColWidths(wb, "Table 1", cols=1:3, widths=c(44,20,20))
wr("Table 1",1,1, glue("Table 1. Patient and Cohort Characteristics (N={n_records}; {n_pts} unique patients)"))
addStyle(wb,"Table 1",title_st,rows=1,cols=1); mergeCells(wb,"Table 1",1:3,1)
mapply(wr,"Table 1",3,1:3,list("Characteristic","","n (%) / Value"))
addStyle(wb,"Table 1",bld,rows=3,cols=1:3,gridExpand=TRUE)
for (i in seq_along(t1)) {
  r <- i+3; st <- if(i %in% sec_rows_t1) sec_st else norm_st
  mapply(wr,"Table 1",r,1:3,t1[[i]]); addStyle(wb,"Table 1",st,rows=r,cols=1:3,gridExpand=TRUE)
}
fn1 <- length(t1)+4
wr("Table 1",fn1,1,"SD = standard deviation. IQR = interquartile range. Age from column AP (days) / 365.25. Age and sex N exclude missing values.")
addStyle(wb,"Table 1",fn_st,rows=fn1,cols=1); mergeCells(wb,"Table 1",1:3,fn1)

# Table 2
addWorksheet(wb, "Table 2")
setColWidths(wb, "Table 2", cols=1:4, widths=c(44,14,18,18))
wr("Table 2",1,1, glue("Table 2. Test Volume, Validity, and Results (N={n_records} tests)"))
addStyle(wb,"Table 2",title_st,rows=1,cols=1); mergeCells(wb,"Table 2",1:4,1)
mapply(wr,"Table 2",3,1:4,list("Category","n / Value","Positive n (%)","Negative n (%)"))
addStyle(wb,"Table 2",bld,rows=3,cols=1:4,gridExpand=TRUE)
for (i in seq_along(t2)) {
  r <- i+3; st <- if(i %in% sec_rows_t2) sec_st else norm_st
  mapply(wr,"Table 2",r,1:4,t2[[i]]); addStyle(wb,"Table 2",st,rows=r,cols=1:4,gridExpand=TRUE)
}
fn2 <- length(t2)+4
wr("Table 2",fn2,1,"QNS = quantity not sufficient. NEG* treated as Negative. Blank result entries excluded from all counts. Specimen collection metrics: positive/negative breakdown not applicable.")
addStyle(wb,"Table 2",fn_st,rows=fn2,cols=1); mergeCells(wb,"Table 2",1:4,fn2)

# Table 3
addWorksheet(wb, "Table 3")
t3_hdrs <- c("Assay","Result","n (%)","Fluid Vol (mL) Median [IQR]",
             "DNA Conc (ng/uL) Median [IQR]","DNA Used (ng) Mean (SD)",
             "Mean EXON 21 WT Droplets Median [IQR]","Mean EXON 10 WT Droplets Median [IQR]",
             "Specimen Type: Blood n (%)","Specimen Type: Cyst Fluid n (%)","Specimen Type: Other n (%)")
setColWidths(wb,"Table 3",cols=1:11,widths=c(20,8,10,15,15,13,19,19,14,18,10))
wr("Table 3",1,1,"Table 3. Assay Performance Metrics by Result (valid tests only)")
addStyle(wb,"Table 3",title_st,rows=1,cols=1); mergeCells(wb,"Table 3",1:11,1)
for (j in seq_along(t3_hdrs)) wr("Table 3",3,j,t3_hdrs[j])
addStyle(wb,"Table 3",bld,rows=3,cols=1:11,gridExpand=TRUE)
for (i in seq_along(table3_df)) {
  r <- i+3; st <- if(i %in% sec_rows_t3) sec_st else norm_st
  for (j in 1:11) wr("Table 3",r,j,table3_df[[i]][[j]])
  addStyle(wb,"Table 3",st,rows=r,cols=1:11,gridExpand=TRUE)
}
fn3 <- length(table3_df)+4
wr("Table 3",fn3,1,"IQR = interquartile range. SD = standard deviation. Mean EXON 21/10 WT droplet counts reported as Median [IQR]; N/A for TEK and BRAF. Specimen type breakdown reflects all valid tests for that assay.")
addStyle(wb,"Table 3",fn_st,rows=fn3,cols=1); mergeCells(wb,"Table 3",1:11,fn3)

# Table 4
addWorksheet(wb, "Table 4")
setColWidths(wb,"Table 4",cols=1:2,widths=c(55,18))
wr("Table 4",1,1, glue("Table 4. Clinical Impact of Test Results (Total patients assessed, N = {n_t4_total})"))
addStyle(wb,"Table 4",title_st,rows=1,cols=1); mergeCells(wb,"Table 4",1:2,1)
mapply(wr,"Table 4",3,1:2,list("Clinical Action / Outcome","n (%)"))
addStyle(wb,"Table 4",bld,rows=3,cols=1:2,gridExpand=TRUE)
for (i in seq_along(t4)) {
  r <- i+3
  mapply(wr,"Table 4",r,1:2,t4[[i]]); addStyle(wb,"Table 4",norm_st,rows=r,cols=1:2,gridExpand=TRUE)
}
fn4 <- length(t4)+4
wr("Table 4",fn4,1,"Clinical actions were assessed in internally managed patients with valid (non-QNS, non-cancelled) results, as complete clinical documentation was available only for this subgroup. Y and VAN counted as positive for additional testing. Blank entries excluded from denominators. Targeted therapies listed are examples; actual agents confirmed by chart review.")
addStyle(wb,"Table 4",fn_st,rows=fn4,cols=1); mergeCells(wb,"Table 4",1:2,fn4)

# =============================================================================
# Word (.docx) exports
# =============================================================================
hdr_color <- "#D9E1F2"
body_font <- "Calibri"
body_size <- 10

style_ft <- function(ft, section_rows, footnote = NULL) {
  ft <- ft |>
    bold(part = "header") |>
    fontsize(size = body_size, part = "all") |>
    font(fontname = body_font, part = "all") |>
    border_outer(part = "all", border = fp_border(color="black", width=1)) |>
    border_inner_h(part = "body", border = fp_border(color="#CCCCCC", width=0.5)) |>
    autofit()
  if (length(section_rows) > 0) {
    ft <- ft |>
      bg(i = section_rows, bg = hdr_color, part = "body") |>
      bold(i = section_rows, part = "body")
  }
  if (!is.null(footnote)) {
    ft <- ft |>
      add_footer_lines(footnote) |>
      fontsize(size = 8, part = "footer") |>
      font(fontname = body_font, part = "footer") |>
      color(color = "#595959", part = "footer")
  }
  ft
}

make_ft_t1 <- function() {
  tbl <- tibble(Characteristic = sapply(t1,`[[`,1), `n (%) / Value` = sapply(t1,`[[`,3))
  flextable(tbl) |>
    add_header_lines(glue("Table 1. Patient and Cohort Characteristics (N = {n_records} records; {n_pts} unique patients)")) |>
    style_ft(sec_rows_t1, "SD = standard deviation. IQR = interquartile range. Age from column AP (days) / 365.25. Age and sex N exclude missing values.")
}

make_ft_t2 <- function() {
  tbl <- tibble(
    Category = sapply(t2,`[[`,1), `n / Value` = sapply(t2,`[[`,2),
    `Positive n (%)` = sapply(t2,`[[`,3), `Negative n (%)` = sapply(t2,`[[`,4))
  sec2 <- which(tbl$Category %in% c(
    "Test Validity","Results by Assay (valid tests only)",
    "Specimen Type (valid tests only)","Specimen Source (valid tests only)","Specimen Collection Metrics"))
  flextable(tbl) |>
    add_header_lines(glue("Table 2. Test Volume, Validity, and Results (N = {n_records} tests)")) |>
    style_ft(sec2, "QNS = quantity not sufficient. NEG* treated as Negative. Blank result entries excluded from all counts. Specimen collection metrics: positive/negative breakdown not applicable.")
}

make_ft_t3 <- function() {
  tbl <- bind_rows(lapply(table3_df, function(r) tibble(
    Assay = r[[1]], Result = r[[2]], `n (%)` = r[[3]],
    `Fluid Vol (mL) Median [IQR]` = r[[4]], `DNA Conc (ng/uL) Median [IQR]` = r[[5]],
    `DNA Used (ng) Mean (SD)` = r[[6]],
    `Mean EXON 21 WT Droplets Median [IQR]` = r[[7]],
    `Mean EXON 10 WT Droplets Median [IQR]` = r[[8]],
    `Specimen Type: Blood n (%)` = r[[9]],
    `Specimen Type: Cyst Fluid n (%)` = r[[10]],
    `Specimen Type: Other n (%)` = r[[11]]
  )))
  sec3 <- which(tbl$Assay != "")
  flextable(tbl) |>
    add_header_lines("Table 3. Assay Performance Metrics by Result (valid tests only)") |>
    style_ft(sec3, "IQR = interquartile range. SD = standard deviation. Mean EXON 21/10 WT droplet counts reported as Median [IQR]; N/A for TEK and BRAF. Specimen type breakdown reflects all valid tests for that assay.") |>
    fontsize(size=8, part="all") |>
    width(j=1,width=0.9) |> width(j=2,width=0.6) |> width(j=3,width=0.6) |>
    width(j=4,width=0.8) |> width(j=5,width=0.9) |> width(j=6,width=0.8) |>
    width(j=7,width=1.1) |> width(j=8,width=1.1) |>
    width(j=9,width=0.8) |> width(j=10,width=0.9) |> width(j=11,width=0.6)
}

make_ft_t4 <- function() {
  tbl <- tibble(`Clinical Action / Outcome` = sapply(t4,`[[`,1), `n (%)` = sapply(t4,`[[`,2))
  flextable(tbl) |>
    add_header_lines(glue("Table 4. Clinical Impact of Test Results (Total patients assessed, N = {n_t4_total})")) |>
    style_ft(integer(0), "Clinical actions were assessed in internally managed patients with valid (non-QNS, non-cancelled) results, as complete clinical documentation was available only for this subgroup. Y and VAN counted as positive for additional testing. Blank entries excluded from denominators. Targeted therapies listed are examples; actual agents confirmed by chart review.")
}

docx_dir <- file.path(dirname(data_path), "SpotSeq_Word_Tables")
dir.create(docx_dir, showWarnings = FALSE)
date_str <- as.character(Sys.Date())

save_ft_docx <- function(ft, filename) {
  doc <- read_docx() |> body_add_flextable(ft)
  print(doc, target = file.path(docx_dir, filename))
  cat("\u2705 Saved:", filename, "\n")
}

save_ft_t3_docx <- function(ft, filename) {
  doc <- read_docx() |>
    body_add_par("") |> body_end_section_continuous() |>
    body_add_flextable(ft) |> body_end_section_landscape()
  print(doc, target = file.path(docx_dir, filename))
  cat("\u2705 Saved:", filename, "(landscape)\n")
}

save_ft_docx(make_ft_t1(), paste0("Table1_", date_str, ".docx"))
save_ft_docx(make_ft_t2(), paste0("Table2_", date_str, ".docx"))
save_ft_t3_docx(make_ft_t3(), paste0("Table3_", date_str, ".docx"))
save_ft_docx(make_ft_t4(), paste0("Table4_", date_str, ".docx"))
cat("\n\u2705 Word tables saved to:", docx_dir, "\n")

# =============================================================================
# Excel to Google Drive
# =============================================================================
out_file <- file.path(tempdir(), paste0("SpotSeq_Tables_", date_str, ".xlsx"))
saveWorkbook(wb, out_file, overwrite=TRUE)
cat("\u2705 Excel saved to:", out_file, "\n")

drive_auth(
  scopes = "https://www.googleapis.com/auth/drive",
  cache  = "~/.gargle-cache",
  email  = "solomonj88@gmail.com"
)
drive_upload(
  media     = out_file,
  path      = as_id(drive_folder_id),
  name      = paste0("SpotSeq_Tables_", date_str, ".xlsx"),
  overwrite = TRUE
)
cat("\u2705 Uploaded to Google Drive folder:", drive_folder_id, "\n")
cat("\n\U0001f516 GitHub checkpoint:\n")
cat('  git add . && git commit -m "feat: v2 analysis complete Tables 1-4"\n')
