# =============================================================================
# SpotSeq Project Dataset - Comprehensive Statistical Analysis
# =============================================================================
# Requirements: readxl, tidyverse, purrr, furrr, future, janitor, broom,
#               rstatix, scales, knitr (optional for formatted output)
#
# Install if needed:
#   install.packages(c("readxl","tidyverse","furrr","future","janitor",
#                      "broom","rstatix","scales"))
# =============================================================================

library(readxl)
library(tidyverse)
library(purrr)
library(furrr)       # purrr + future for multicore
library(future)
library(janitor)     # clean_names()
library(broom)       # tidy() for stat test outputs
library(rstatix)     # pipe-friendly stats
library(scales)      # percent()

# ── Multicore setup ──────────────────────────────────────────────────────────
# Uses all available cores minus 1 to keep system responsive
n_cores <- max(1L, parallelly::availableCores() - 1L)
plan(multisession, workers = n_cores)
message(glue::glue("Parallel backend: {n_cores} workers"))

# =============================================================================
# 1. DATA INGESTION
# =============================================================================

FILE_PATH <- "/Users/sjoh23/Library/CloudStorage/OneDrive-SCH/SCH\ research\ projects/Spot\ seq\ update\ project/SpotSeq_Project_Dataset_deidentified_2.1.xlsx"

raw <- read_excel(FILE_PATH, sheet = 1)

# Clean column names to snake_case for easier referencing
df <- raw |>
  clean_names() |>
  # Standardise key text columns: trim whitespace, uppercase for grouping
  mutate(
    across(where(is.character), str_trim),
    result_clean = str_to_upper(result_pos_neg),      # POS / NEG / QNS / CANCEL etc.
    specimen_type_clean = str_to_upper(specimen_type),
    assay_clean = str_to_upper(assay_ordered),
    specimen_source_clean = str_to_upper(specimen_source)
  )

# Preview
cat("\n=== COLUMN NAMES (cleaned) ===\n")
print(names(df))
cat("\n=== DIMENSIONS ===\n")
cat(nrow(df), "rows ×", ncol(df), "columns\n")

# Helper: identify the exact column names after clean_names()
# (Long column names get truncated/modified by clean_names – adjust below if needed)
# Run names(df) after loading and update these aliases as required:

COL_PATIENT_ID   <- "patient_id"                        # adjust if different
COL_RESULT       <- "result_clean"
COL_SPECIMEN_TYPE <- "specimen_type_clean"
COL_ASSAY        <- "assay_clean"
COL_DNA_STOCK    <- grep("dna.*stock|stock.*ng_ul", names(df), value = TRUE, ignore.case = TRUE)[1]
COL_TOTAL_NG     <- grep("total_ng.*dna|total.*ng.*used", names(df), value = TRUE, ignore.case = TRUE)[1]
COL_FLUID_VOL    <- grep("fluid_volume|volume_extracted", names(df), value = TRUE, ignore.case = TRUE)[1]
COL_ANTICIPATED  <- grep("anticipated.*ng|minimum", names(df), value = TRUE, ignore.case = TRUE)[1]
COL_HEMOLYSIS    <- grep("hemolysis", names(df), value = TRUE, ignore.case = TRUE)[1]
COL_SPECIMEN_SRC <- "specimen_source_clean"

cat("\n=== KEY COLUMN MAPPINGS ===\n")
cat("DNA stock col    :", COL_DNA_STOCK, "\n")
cat("Total ng col     :", COL_TOTAL_NG,  "\n")
cat("Fluid volume col :", COL_FLUID_VOL, "\n")
cat("Anticipated col  :", COL_ANTICIPATED, "\n")
cat("Hemolysis col    :", COL_HEMOLYSIS, "\n")

# =============================================================================
# 2. DEFINE ANALYSIS FUNCTIONS  (will be run in parallel via furrr)
# =============================================================================

# ── 2a. Unique individuals ────────────────────────────────────────────────────
analyse_individuals <- function(data) {
  total_unique <- data |> distinct(.data[[COL_PATIENT_ID]]) |> nrow()

  pos_neg_counts <- data |>
    filter(result_clean %in% c("POS", "NEG")) |>
    count(result_clean, name = "n_individuals") |>
    rename(result = result_clean)

  list(total_unique_patients = total_unique,
       pos_neg_counts = pos_neg_counts)
}

# ── 2b. Specimen type breakdown ───────────────────────────────────────────────
analyse_specimen_type <- function(data) {
  blood_only  <- data |> filter(specimen_type_clean == "BLOOD") |>
    distinct(.data[[COL_PATIENT_ID]]) |> nrow()
  cyst_only   <- data |> filter(specimen_type_clean == "CYST FLUID") |>
    distinct(.data[[COL_PATIENT_ID]]) |> nrow()
  both        <- data |>
    group_by(.data[[COL_PATIENT_ID]]) |>
    summarise(types = list(unique(specimen_type_clean)), .groups = "drop") |>
    filter(map_lgl(types, ~ all(c("BLOOD", "CYST FLUID") %in% .x))) |>
    nrow()

  tibble(specimen = c("Blood only", "Cyst fluid only", "Both"),
         n_patients = c(blood_only, cyst_only, both))
}

# ── 2c. Assay ordered breakdown ───────────────────────────────────────────────
ASSAYS_OF_INTEREST <- c(
  "PIK3CA MULTIPLEX",
  "BRAF",
  "TEK",
  "PIK3CA MULTIPLEX, TEK",
  "PIK3CA MULTIPLEX, BRAF"
)

analyse_assay <- function(data) {
  data |>
    filter(assay_clean %in% ASSAYS_OF_INTEREST) |>
    count(assay_clean, name = "n_tests") |>
    rename(assay = assay_clean) |>
    arrange(desc(n_tests))
}

# ── 2d. Cancelled tests ───────────────────────────────────────────────────────
analyse_cancelled <- function(data) {
  n <- data |> filter(str_detect(result_clean, "CANCEL")) |> nrow()
  tibble(cancelled_tests = n)
}

# ── 2e. QNS failures ─────────────────────────────────────────────────────────
analyse_qns <- function(data) {
  n <- data |> filter(result_clean == "QNS") |> nrow()
  tibble(qns_failures = n)
}

# ── 2f. Average DNA [stock] by result ─────────────────────────────────────────
analyse_dna_stock <- function(data) {
  pos_neg <- data |>
    filter(result_clean %in% c("POS", "NEG"),
           !is.na(.data[[COL_DNA_STOCK]])) |>
    group_by(result = result_clean) |>
    summarise(mean_dna_stock_ng_ul = mean(.data[[COL_DNA_STOCK]], na.rm = TRUE),
              median_dna_stock     = median(.data[[COL_DNA_STOCK]], na.rm = TRUE),
              n = n(), .groups = "drop")

  # t-test POS vs NEG
  ttest <- tryCatch({
    t.test(
      data |> filter(result_clean == "POS") |> pull(.data[[COL_DNA_STOCK]]),
      data |> filter(result_clean == "NEG") |> pull(.data[[COL_DNA_STOCK]])
    ) |> tidy()
  }, error = function(e) tibble(note = as.character(e)))

  list(summary = pos_neg, test = ttest)
}

# ── 2g. Average total ng DNA used – overall + by assay ───────────────────────
analyse_total_ng <- function(data) {
  base_data <- data |>
    filter(result_clean %in% c("POS", "NEG"),
           !is.na(.data[[COL_TOTAL_NG]]))

  overall <- base_data |>
    group_by(result = result_clean) |>
    summarise(mean_total_ng = mean(.data[[COL_TOTAL_NG]], na.rm = TRUE),
              median_total_ng = median(.data[[COL_TOTAL_NG]], na.rm = TRUE),
              n = n(), .groups = "drop")

  by_assay <- base_data |>
    filter(assay_clean %in% ASSAYS_OF_INTEREST) |>
    group_by(assay = assay_clean, result = result_clean) |>
    summarise(mean_total_ng = mean(.data[[COL_TOTAL_NG]], na.rm = TRUE),
              n = n(), .groups = "drop")

  # ANOVA: does mean total ng differ by result across assays?
  anova_model <- tryCatch({
    aov_data <- base_data |>
      filter(assay_clean %in% ASSAYS_OF_INTEREST) |>
      mutate(result_f = factor(result_clean), assay_f = factor(assay_clean))
    fit <- aov(reformulate(c("result_f", "assay_f"), COL_TOTAL_NG), data = aov_data)
    tidy(fit)
  }, error = function(e) tibble(note = as.character(e)))

  # t-test overall POS vs NEG
  ttest_overall <- tryCatch({
    t.test(
      base_data |> filter(result_clean == "POS") |> pull(.data[[COL_TOTAL_NG]]),
      base_data |> filter(result_clean == "NEG") |> pull(.data[[COL_TOTAL_NG]])
    ) |> tidy()
  }, error = function(e) tibble(note = as.character(e)))

  list(overall = overall, by_assay = by_assay,
       ttest_overall = ttest_overall, anova_by_assay = anova_model)
}

# ── 2h. Average fluid volume by result ───────────────────────────────────────
analyse_fluid_volume <- function(data) {
  pos_neg <- data |>
    filter(result_clean %in% c("POS", "NEG"),
           !is.na(.data[[COL_FLUID_VOL]])) |>
    group_by(result = result_clean) |>
    summarise(mean_vol  = mean(.data[[COL_FLUID_VOL]], na.rm = TRUE),
              median_vol = median(.data[[COL_FLUID_VOL]], na.rm = TRUE),
              n = n(), .groups = "drop")

  ttest <- tryCatch({
    t.test(
      data |> filter(result_clean == "POS") |> pull(.data[[COL_FLUID_VOL]]),
      data |> filter(result_clean == "NEG") |> pull(.data[[COL_FLUID_VOL]])
    ) |> tidy()
  }, error = function(e) tibble(note = as.character(e)))

  list(summary = pos_neg, test = ttest)
}

# ── 2i. REJECT count in anticipated ng column ────────────────────────────────
analyse_rejects <- function(data) {
  n <- data |>
    filter(str_detect(str_to_upper(.data[[COL_ANTICIPATED]]), "REJECT")) |>
    nrow()
  tibble(reject_count = n)
}

# ── 2j. Average hemolysis grade by result ─────────────────────────────────────
analyse_hemolysis <- function(data) {
  # Hemolysis grade may be stored as text (e.g. "1", "2+"); coerce to numeric
  hem_data <- data |>
    filter(result_clean %in% c("POS", "NEG")) |>
    mutate(hem_num = suppressWarnings(
      as.numeric(str_remove_all(.data[[COL_HEMOLYSIS]], "[^0-9\\.]"))
    )) |>
    filter(!is.na(hem_num))

  summary_tbl <- hem_data |>
    group_by(result = result_clean) |>
    summarise(mean_hemolysis   = mean(hem_num, na.rm = TRUE),
              median_hemolysis = median(hem_num, na.rm = TRUE),
              n = n(), .groups = "drop")

  # Wilcoxon rank-sum (hemolysis is ordinal → non-parametric preferred)
  wilcox <- tryCatch({
    wilcox.test(
      hem_data |> filter(result_clean == "POS") |> pull(hem_num),
      hem_data |> filter(result_clean == "NEG") |> pull(hem_num)
    ) |> tidy()
  }, error = function(e) tibble(note = as.character(e)))

  list(summary = summary_tbl, wilcoxon_test = wilcox)
}

# ── 2k. Specimen source percentages + chi-square POS vs NEG ──────────────────
analyse_specimen_source <- function(data) {
  overall <- data |>
    count(source = specimen_source_clean, name = "n") |>
    mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) |>
    arrange(desc(n))

  by_result <- data |>
    filter(result_clean %in% c("POS", "NEG")) |>
    count(source = specimen_source_clean, result = result_clean, name = "n") |>
    group_by(source) |>
    mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) |>
    ungroup() |>
    arrange(source, result)

  # Chi-square: independence of specimen source and result
  chi_tbl <- data |>
    filter(result_clean %in% c("POS", "NEG")) |>
    count(specimen_source_clean, result_clean) |>
    pivot_wider(names_from = result_clean, values_from = n, values_fill = 0) |>
    column_to_rownames("specimen_source_clean") |>
    as.matrix()

  chi_test <- tryCatch(
    chisq.test(chi_tbl) |> tidy(),
    error = function(e) tibble(note = as.character(e))
  )

  list(overall = overall, by_result = by_result, chi_square = chi_test)
}

# =============================================================================
# 3. RUN ALL ANALYSES IN PARALLEL WITH furrr
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
results <- furrr::future_map(analysis_fns, ~ .x(df),
                              .options = furrr_options(seed = TRUE))

# =============================================================================
# 4. PRINT RESULTS
# =============================================================================

sep <- function(title) cat("\n", strrep("─", 70), "\n", title, "\n", strrep("─", 70), "\n")

# ── 4a. Individuals ───────────────────────────────────────────────────────────
sep("1. UNIQUE INDIVIDUALS TESTED")
cat("Total unique patients:", results$individuals$total_unique_patients, "\n\n")
cat("POS / NEG breakdown:\n")
print(results$individuals$pos_neg_counts)

# ── 4b. Specimen type ─────────────────────────────────────────────────────────
sep("2. SPECIMEN TYPE (Blood / Cyst Fluid / Both)")
print(results$specimen_type)

# Chi-square: Blood vs Cyst Fluid result distribution
chi_spec <- tryCatch({
  tbl <- df |>
    filter(result_clean %in% c("POS", "NEG"),
           specimen_type_clean %in% c("BLOOD", "CYST FLUID")) |>
    count(specimen_type_clean, result_clean) |>
    pivot_wider(names_from = result_clean, values_from = n, values_fill = 0) |>
    column_to_rownames("specimen_type_clean") |> as.matrix()
  chisq.test(tbl) |> tidy()
}, error = function(e) tibble(note = as.character(e)))
cat("\nChi-square (Specimen Type × Result):\n"); print(chi_spec)

# ── 4c. Assay ordered ─────────────────────────────────────────────────────────
sep("3. ASSAY ORDERED")
print(results$assay)

# Chi-square: assay × result
chi_assay <- tryCatch({
  tbl <- df |>
    filter(result_clean %in% c("POS","NEG"),
           assay_clean %in% ASSAYS_OF_INTEREST) |>
    count(assay_clean, result_clean) |>
    pivot_wider(names_from = result_clean, values_from = n, values_fill = 0) |>
    column_to_rownames("assay_clean") |> as.matrix()
  chisq.test(tbl) |> tidy()
}, error = function(e) tibble(note = as.character(e)))
cat("\nChi-square (Assay × Result):\n"); print(chi_assay)

# ── 4d. Cancelled tests ───────────────────────────────────────────────────────
sep("4. CANCELLED TESTS")
print(results$cancelled)

# ── 4e. QNS failures ─────────────────────────────────────────────────────────
sep("5. QNS FAILURES")
print(results$qns)

# ── 4f. DNA stock concentration ───────────────────────────────────────────────
sep("6. DNA [STOCK] ng/uL — POS vs NEG")
cat("Summary:\n"); print(results$dna_stock$summary)
cat("\nWelch Two-Sample t-test:\n"); print(results$dna_stock$test)

# ── 4g. Total ng DNA used ────────────────────────────────────────────────────
sep("7. TOTAL NG DNA USED IN ASSAY — POS vs NEG")
cat("Overall summary:\n"); print(results$total_ng$overall)
cat("\nOverall t-test:\n"); print(results$total_ng$ttest_overall)
cat("\nBy Assay (mean):\n"); print(results$total_ng$by_assay)
cat("\nTwo-way ANOVA (Result + Assay):\n"); print(results$total_ng$anova_by_assay)

# ── 4h. Fluid volume ─────────────────────────────────────────────────────────
sep("8. FLUID VOLUME EXTRACTED — POS vs NEG")
cat("Summary:\n"); print(results$fluid_volume$summary)
cat("\nWelch Two-Sample t-test:\n"); print(results$fluid_volume$test)

# ── 4i. REJECTs ───────────────────────────────────────────────────────────────
sep("9. REJECT COUNT (Anticipated ng column)")
print(results$rejects)

# ── 4j. Hemolysis grade ───────────────────────────────────────────────────────
sep("10. HEMOLYSIS GRADE — POS vs NEG")
cat("Summary:\n"); print(results$hemolysis$summary)
cat("\nWilcoxon Rank-Sum Test (non-parametric, ordinal data):\n")
print(results$hemolysis$wilcoxon_test)

# ── 4k. Specimen source ───────────────────────────────────────────────────────
sep("11. SPECIMEN SOURCE — Overall % and POS vs NEG subgroups")
cat("Overall percentages:\n"); print(results$specimen_source$overall)
cat("\nBy Result (POS / NEG):\n"); print(results$specimen_source$by_result)
cat("\nChi-square (Specimen Source × Result):\n")
print(results$specimen_source$chi_square)

# =============================================================================
# 5. EXPORT RESULTS TO EXCEL
# =============================================================================
# Uncomment to write all summary tables to a single xlsx workbook:
#
# library(openxlsx)
# wb <- createWorkbook()
# walk2(
#   list(results$individuals$pos_neg_counts,
#        results$specimen_type,
#        results$assay,
#        results$cancelled,
#        results$qns,
#        results$dna_stock$summary,
#        results$total_ng$overall,
#        results$total_ng$by_assay,
#        results$fluid_volume$summary,
#        results$rejects,
#        results$hemolysis$summary,
#        results$specimen_source$overall,
#        results$specimen_source$by_result),
#   c("POS_NEG","SpecimenType","Assay","Cancelled","QNS",
#     "DNA_Stock","TotalNG_Overall","TotalNG_ByAssay","FluidVol",
#     "Rejects","Hemolysis","SpecSrc_Overall","SpecSrc_ByResult"),
#   function(tbl, nm) {
#     addWorksheet(wb, nm)
#     writeDataTable(wb, nm, tbl)
#   }
# )
# saveWorkbook(wb, "SpotSeq_Analysis_Results.xlsx", overwrite = TRUE)

# ── Cleanup ───────────────────────────────────────────────────────────────────
plan(sequential)
cat("\n=== ANALYSIS COMPLETE ===\n")
