# =============================================================================
# SpotSeq Project Dataset - Comprehensive Statistical Analysis
# =============================================================================
# Requirements: readxl, tidyverse, purrr, furrr, future, janitor, broom,
#               rstatix, scales, openxlsx
#
# Install if needed:
#   install.packages(c("readxl","tidyverse","furrr","future","janitor",
#                      "broom","rstatix","scales","parallelly","openxlsx"))
# =============================================================================

library(readxl)
library(tidyverse)
library(purrr)
library(furrr)
library(future)
library(janitor)
library(broom)
library(rstatix)
library(scales)
library(openxlsx)

# ── Capture all console output to a text file on the Desktop ─────────────────
sink("~/Desktop/SpotSeq_Analysis_Output.txt", split = TRUE)
cat("SpotSeq Analysis Output\n")
cat("Run date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# ── Multicore setup ──────────────────────────────────────────────────────────
n_cores <- max(1L, parallelly::availableCores() - 1L)
plan(multisession, workers = n_cores)
message("Parallel backend: ", n_cores, " workers")

# =============================================================================
# 1. DATA INGESTION
# =============================================================================

FILE_PATH <- "/Users/sjoh23/Library/CloudStorage/OneDrive-SCH/SCH\ research\ projects/Spot\ seq\ update\ project/SpotSeq_Project_Dataset_deidentified_2.1.xlsx"

raw <- read_excel(FILE_PATH, sheet = 1)

# Suppress µ warning from janitor — expected, harmless
df_names <- suppressWarnings(raw |> clean_names())

cat("\n=== CLEANED COLUMN NAMES ===\n")
print(names(df_names))

# ── Dynamically locate key columns ───────────────────────────────────────────
COL_PATIENT_ID    <- grep("patient.*id|id.*patient",           names(df_names), value = TRUE, ignore.case = TRUE)[1]
COL_RESULT_RAW    <- grep("result.*pos|pos.*neg",              names(df_names), value = TRUE, ignore.case = TRUE)[1]
COL_SPECIMEN_TYPE <- grep("specimen.*type",                    names(df_names), value = TRUE, ignore.case = TRUE)[1]
COL_ASSAY         <- grep("assay.*order|order.*assay",         names(df_names), value = TRUE, ignore.case = TRUE)[1]
COL_DNA_STOCK     <- grep("dna.*stock|stock.*ng",              names(df_names), value = TRUE, ignore.case = TRUE)[1]
COL_TOTAL_NG      <- grep("total.*ng.*dna|total.*ng.*used|ng.*dna.*used", names(df_names), value = TRUE, ignore.case = TRUE)[1]
COL_FLUID_VOL     <- grep("fluid.*volume|volume.*extract",    names(df_names), value = TRUE, ignore.case = TRUE)[1]
COL_ANTICIPATED   <- grep("anticipated|minimum",               names(df_names), value = TRUE, ignore.case = TRUE)[1]
COL_HEMOLYSIS     <- grep("hemolysis",                         names(df_names), value = TRUE, ignore.case = TRUE)[1]
COL_SPECIMEN_SRC  <- grep("specimen.*source|source.*specimen", names(df_names), value = TRUE, ignore.case = TRUE)[1]

cat("\n=== KEY COLUMN MAPPINGS ===\n")
cat("Patient ID col    :", COL_PATIENT_ID,    "\n")
cat("Result col        :", COL_RESULT_RAW,    "\n")
cat("Specimen type col :", COL_SPECIMEN_TYPE, "\n")
cat("Assay col         :", COL_ASSAY,         "\n")
cat("DNA stock col     :", COL_DNA_STOCK,     "\n")
cat("Total ng col      :", COL_TOTAL_NG,      "\n")
cat("Fluid volume col  :", COL_FLUID_VOL,     "\n")
cat("Anticipated col   :", COL_ANTICIPATED,   "\n")
cat("Hemolysis col     :", COL_HEMOLYSIS,     "\n")
cat("Specimen source   :", COL_SPECIMEN_SRC,  "\n")

# ── Build clean working dataframe ─────────────────────────────────────────────
df <- df_names |>
  mutate(
    across(where(is.character), str_trim),
    result_clean          = str_to_upper(.data[[COL_RESULT_RAW]]),
    specimen_type_clean   = str_to_upper(.data[[COL_SPECIMEN_TYPE]]),
    assay_clean           = str_to_upper(.data[[COL_ASSAY]]),
    specimen_source_clean = str_to_upper(.data[[COL_SPECIMEN_SRC]]),
    dna_stock_num = suppressWarnings(as.numeric(.data[[COL_DNA_STOCK]])),
    total_ng_num  = suppressWarnings(as.numeric(.data[[COL_TOTAL_NG]])),
    fluid_vol_num = suppressWarnings(as.numeric(.data[[COL_FLUID_VOL]])),
    hemolysis_num = suppressWarnings(as.numeric(
      str_remove_all(as.character(.data[[COL_HEMOLYSIS]]), "[^0-9\\.]")
    ))
  )

cat("\n=== DIMENSIONS ===\n")
cat(nrow(df), "rows x", ncol(df), "columns\n")

# =============================================================================
# 2. HELPER: SMART CATEGORICAL TEST
# Auto-selects chi-square or Fisher's exact based on expected cell counts
# =============================================================================

smart_cat_test <- function(data, row_var, col_var) {
  tbl <- data |>
    count(.data[[row_var]], .data[[col_var]]) |>
    pivot_wider(names_from  = all_of(col_var),
                values_from = n,
                values_fill = 0) |>
    column_to_rownames(row_var) |>
    as.matrix()

  expected   <- outer(rowSums(tbl), colSums(tbl)) / sum(tbl)
  use_fisher <- any(expected < 5)

  if (use_fisher) {
    result <- tryCatch(
      fisher.test(tbl, simulate.p.value = TRUE, B = 10000) |> tidy() |>
        mutate(test_used = "Fisher's Exact Test (simulated; expected cell < 5)"),
      error = function(e) tibble(note = as.character(e))
    )
  } else {
    result <- tryCatch(
      chisq.test(tbl) |> tidy() |>
        mutate(test_used = "Pearson's Chi-squared Test"),
      error = function(e) tibble(note = as.character(e))
    )
  }
  result
}

# =============================================================================
# 3. ASSAYS OF INTEREST
# =============================================================================

ASSAYS_OF_INTEREST <- c(
  "PIK3CA MULTIPLEX",
  "BRAF",
  "TEK",
  "PIK3CA MULTIPLEX, TEK",
  "PIK3CA MULTIPLEX, BRAF"
)

# =============================================================================
# 4. ANALYSIS FUNCTIONS
# =============================================================================

analyse_individuals <- function(data) {
  total_unique <- data |>
    distinct(.data[[COL_PATIENT_ID]]) |>
    nrow()

  pos_neg_counts <- data |>
    filter(result_clean %in% c("POS", "NEG")) |>
    count(result = result_clean, name = "n_tests")

  list(total_unique_patients = total_unique,
       pos_neg_counts         = pos_neg_counts)
}

analyse_specimen_type <- function(data) {
  blood_only <- data |>
    filter(specimen_type_clean == "BLOOD") |>
    distinct(.data[[COL_PATIENT_ID]]) |> nrow()

  cyst_only <- data |>
    filter(specimen_type_clean == "CYST FLUID") |>
    distinct(.data[[COL_PATIENT_ID]]) |> nrow()

  both <- data |>
    group_by(.data[[COL_PATIENT_ID]]) |>
    summarise(types = list(unique(specimen_type_clean)), .groups = "drop") |>
    filter(map_lgl(types, ~ all(c("BLOOD", "CYST FLUID") %in% .x))) |>
    nrow()

  counts <- tibble(
    specimen   = c("Blood only", "Cyst fluid only", "Both"),
    n_patients = c(blood_only, cyst_only, both)
  )

  cat_test <- data |>
    filter(result_clean %in% c("POS", "NEG"),
           specimen_type_clean %in% c("BLOOD", "CYST FLUID")) |>
    smart_cat_test("specimen_type_clean", "result_clean")

  list(counts = counts, categorical_test = cat_test)
}

analyse_assay <- function(data) {
  counts <- data |>
    filter(assay_clean %in% ASSAYS_OF_INTEREST) |>
    count(assay = assay_clean, name = "n_tests") |>
    arrange(desc(n_tests))

  cat_test <- data |>
    filter(result_clean %in% c("POS", "NEG"),
           assay_clean %in% ASSAYS_OF_INTEREST) |>
    smart_cat_test("assay_clean", "result_clean")

  list(counts = counts, categorical_test = cat_test)
}

analyse_cancelled <- function(data) {
  n <- data |> filter(str_detect(result_clean, "CANCEL")) |> nrow()
  tibble(cancelled_tests = n)
}

analyse_qns <- function(data) {
  n <- data |> filter(result_clean == "QNS") |> nrow()
  tibble(qns_failures = n)
}

analyse_dna_stock <- function(data) {
  pos_neg <- data |>
    filter(result_clean %in% c("POS", "NEG"), !is.na(dna_stock_num)) |>
    group_by(result = result_clean) |>
    summarise(mean_ng_ul   = mean(dna_stock_num,   na.rm = TRUE),
              median_ng_ul = median(dna_stock_num, na.rm = TRUE),
              sd           = sd(dna_stock_num,     na.rm = TRUE),
              n            = n(), .groups = "drop")

  ttest <- tryCatch({
    pos_v <- data |> filter(result_clean == "POS", !is.na(dna_stock_num)) |> pull(dna_stock_num)
    neg_v <- data |> filter(result_clean == "NEG", !is.na(dna_stock_num)) |> pull(dna_stock_num)
    t.test(pos_v, neg_v) |> tidy()
  }, error = function(e) tibble(note = as.character(e)))

  list(summary = pos_neg, welch_ttest = ttest)
}

analyse_total_ng <- function(data) {
  base <- data |>
    filter(result_clean %in% c("POS", "NEG"), !is.na(total_ng_num))

  overall <- base |>
    group_by(result = result_clean) |>
    summarise(mean_total_ng   = mean(total_ng_num,   na.rm = TRUE),
              median_total_ng = median(total_ng_num, na.rm = TRUE),
              sd              = sd(total_ng_num,     na.rm = TRUE),
              n               = n(), .groups = "drop")

  by_assay <- base |>
    filter(assay_clean %in% ASSAYS_OF_INTEREST) |>
    group_by(assay = assay_clean, result = result_clean) |>
    summarise(mean_total_ng = mean(total_ng_num, na.rm = TRUE),
              n             = n(), .groups = "drop")

  ttest_overall <- tryCatch({
    pos_v <- base |> filter(result_clean == "POS") |> pull(total_ng_num)
    neg_v <- base |> filter(result_clean == "NEG") |> pull(total_ng_num)
    t.test(pos_v, neg_v) |> tidy()
  }, error = function(e) tibble(note = as.character(e)))

  anova_result <- tryCatch({
    aov_data <- base |>
      filter(assay_clean %in% ASSAYS_OF_INTEREST) |>
      mutate(result_f = factor(result_clean), assay_f = factor(assay_clean))
    fit <- aov(total_ng_num ~ result_f + assay_f, data = aov_data)
    tidy(fit)
  }, error = function(e) tibble(note = as.character(e)))

  list(overall        = overall,
       by_assay       = by_assay,
       ttest_overall  = ttest_overall,
       anova_by_assay = anova_result)
}

analyse_fluid_volume <- function(data) {
  pos_neg <- data |>
    filter(result_clean %in% c("POS", "NEG"), !is.na(fluid_vol_num)) |>
    group_by(result = result_clean) |>
    summarise(mean_vol   = mean(fluid_vol_num,   na.rm = TRUE),
              median_vol = median(fluid_vol_num, na.rm = TRUE),
              sd         = sd(fluid_vol_num,     na.rm = TRUE),
              n          = n(), .groups = "drop")

  ttest <- tryCatch({
    pos_v <- data |> filter(result_clean == "POS", !is.na(fluid_vol_num)) |> pull(fluid_vol_num)
    neg_v <- data |> filter(result_clean == "NEG", !is.na(fluid_vol_num)) |> pull(fluid_vol_num)
    t.test(pos_v, neg_v) |> tidy()
  }, error = function(e) tibble(note = as.character(e)))

  list(summary = pos_neg, welch_ttest = ttest)
}

analyse_rejects <- function(data) {
  n <- data |>
    filter(str_detect(str_to_upper(as.character(.data[[COL_ANTICIPATED]])), "REJECT")) |>
    nrow()
  tibble(reject_count = n)
}

analyse_hemolysis <- function(data) {
  hem_data <- data |>
    filter(result_clean %in% c("POS", "NEG"), !is.na(hemolysis_num))

  summary_tbl <- hem_data |>
    group_by(result = result_clean) |>
    summarise(mean_hemolysis   = mean(hemolysis_num,   na.rm = TRUE),
              median_hemolysis = median(hemolysis_num, na.rm = TRUE),
              n                = n(), .groups = "drop")

  wilcox <- tryCatch({
    pos_v <- hem_data |> filter(result_clean == "POS") |> pull(hemolysis_num)
    neg_v <- hem_data |> filter(result_clean == "NEG") |> pull(hemolysis_num)
    wilcox.test(pos_v, neg_v) |> tidy()
  }, error = function(e) tibble(note = as.character(e)))

  list(summary = summary_tbl, wilcoxon_test = wilcox)
}

analyse_specimen_source <- function(data) {
  overall <- data |>
    count(source = specimen_source_clean, name = "n") |>
    mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) |>
    arrange(desc(n))

  by_result <- data |>
    filter(result_clean %in% c("POS", "NEG")) |>
    count(source = specimen_source_clean, result = result_clean, name = "n") |>
    group_by(source) |>
    mutate(pct_within_source = scales::percent(n / sum(n), accuracy = 0.1)) |>
    ungroup() |>
    arrange(source, result)

  cat_test <- data |>
    filter(result_clean %in% c("POS", "NEG")) |>
    smart_cat_test("specimen_source_clean", "result_clean")

  list(overall = overall, by_result = by_result, categorical_test = cat_test)
}

# =============================================================================
# 5. RUN ALL ANALYSES IN PARALLEL WITH furrr
# =============================================================================

analysis_fns <- list(
  individuals     = analyse_individuals,
  specimen_type   = analyse_specimen_type,
  assay           = analyse_assay,
  cancelled       = analyse_cancelled,
  qns             = analyse_qns,
  dna_stock       = analyse_dna_stock,
  total_ng        = analyse_total_ng,
  fluid_volume    = analyse_fluid_volume,
  rejects         = analyse_rejects,
  hemolysis       = analyse_hemolysis,
  specimen_source = analyse_specimen_source
)

cat("\n=== RUNNING PARALLEL ANALYSES ===\n")
results <- furrr::future_map(
  analysis_fns,
  ~ .x(df),
  .options = furrr_options(seed = TRUE)
)

# =============================================================================
# 6. PRINT RESULTS
# =============================================================================

sep <- function(title) {
  cat("\n", strrep("=", 70), "\n", title, "\n", strrep("=", 70), "\n", sep = "")
}

sep("1. UNIQUE INDIVIDUALS TESTED")
cat("Total unique patients:", results$individuals$total_unique_patients, "\n\n")
cat("POS / NEG test counts:\n")
print(results$individuals$pos_neg_counts)

sep("2. SPECIMEN TYPE (Blood / Cyst Fluid / Both)")
print(results$specimen_type$counts)
cat("\nCategorical test (auto-selected: chi-square or Fisher's exact):\n")
print(results$specimen_type$categorical_test)

sep("3. ASSAY ORDERED")
print(results$assay$counts)
cat("\nCategorical test (auto-selected: chi-square or Fisher's exact):\n")
print(results$assay$categorical_test)

sep("4. CANCELLED TESTS")
print(results$cancelled)

sep("5. QNS FAILURES (assay problem)")
print(results$qns)

sep("6. DNA STOCK CONCENTRATION (ng/uL) — POS vs NEG")
cat("Descriptive statistics:\n")
print(results$dna_stock$summary)
cat("\nWelch two-sample t-test:\n")
print(results$dna_stock$welch_ttest)

sep("7. TOTAL NG DNA USED IN ASSAY — POS vs NEG")
cat("Overall descriptive statistics:\n")
print(results$total_ng$overall)
cat("\nOverall Welch t-test (POS vs NEG):\n")
print(results$total_ng$ttest_overall)
cat("\nMean total ng by Assay and Result:\n")
print(results$total_ng$by_assay)
cat("\nTwo-way ANOVA (Result + Assay):\n")
print(results$total_ng$anova_by_assay)

sep("8. FLUID VOLUME EXTRACTED — POS vs NEG")
cat("Descriptive statistics:\n")
print(results$fluid_volume$summary)
cat("\nWelch two-sample t-test:\n")
print(results$fluid_volume$welch_ttest)

sep("9. REJECT COUNT (Anticipated ng of DNA column)")
print(results$rejects)

sep("10. HEMOLYSIS GRADE — POS vs NEG")
cat("Descriptive statistics:\n")
print(results$hemolysis$summary)
cat("\nWilcoxon rank-sum test (non-parametric; hemolysis is ordinal):\n")
print(results$hemolysis$wilcoxon_test)

sep("11. SPECIMEN SOURCE — Overall % and POS vs NEG subgroups")
cat("Overall percentages:\n")
print(results$specimen_source$overall)
cat("\nBreakdown by Result (POS / NEG):\n")
print(results$specimen_source$by_result)
cat("\nCategorical test (auto-selected: chi-square or Fisher's exact):\n")
print(results$specimen_source$categorical_test)

# =============================================================================
# 7. EXPORT ALL RESULTS TO EXCEL
# =============================================================================

wb <- createWorkbook()

sheet_data <- list(
  "1_POS_NEG"           = results$individuals$pos_neg_counts,
  "2_SpecimenType"      = results$specimen_type$counts,
  "2_SpecType_Test"     = results$specimen_type$categorical_test,
  "3_Assay"             = results$assay$counts,
  "3_Assay_Test"        = results$assay$categorical_test,
  "4_Cancelled"         = results$cancelled,
  "5_QNS"               = results$qns,
  "6_DNAStock"          = results$dna_stock$summary,
  "6_DNAStock_ttest"    = results$dna_stock$welch_ttest,
  "7_TotalNG_Overall"   = results$total_ng$overall,
  "7_TotalNG_ttest"     = results$total_ng$ttest_overall,
  "7_TotalNG_ByAssay"   = results$total_ng$by_assay,
  "7_TotalNG_ANOVA"     = results$total_ng$anova_by_assay,
  "8_FluidVol"          = results$fluid_volume$summary,
  "8_FluidVol_ttest"    = results$fluid_volume$welch_ttest,
  "9_Rejects"           = results$rejects,
  "10_Hemolysis"        = results$hemolysis$summary,
  "10_Hemolysis_Wilcox" = results$hemolysis$wilcoxon_test,
  "11_SpecSrc_Overall"  = results$specimen_source$overall,
  "11_SpecSrc_ByResult" = results$specimen_source$by_result,
  "11_SpecSrc_Test"     = results$specimen_source$categorical_test
)

walk2(sheet_data, names(sheet_data), function(tbl, nm) {
  addWorksheet(wb, nm)
  writeDataTable(wb, nm, as.data.frame(tbl))
})

excel_path <- "~/Desktop/SpotSeq_Analysis_Results.xlsx"
saveWorkbook(wb, excel_path, overwrite = TRUE)
cat("\nResults exported to:", excel_path, "\n")

# =============================================================================
# 8. CLOSE SINK AND CLEANUP
# =============================================================================

plan(sequential)
cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Text output saved to: ~/Desktop/SpotSeq_Analysis_Output.txt\n")
cat("Excel output saved to: ~/Desktop/SpotSeq_Analysis_Results.xlsx\n")

sink()  # Close the text file sink — must be last line
