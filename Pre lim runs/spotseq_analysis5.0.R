# =============================================================================
# SpotSeq Project Dataset - Comprehensive Statistical Analysis
# Single-sheet Excel export with stat tests to the right of each table
# =============================================================================
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

# ── Capture console output ────────────────────────────────────────────────────
sink("~/Desktop/SpotSeq_Analysis_Output.txt", split = TRUE)
cat("SpotSeq Analysis Output\n")
cat("Run date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# ── Multicore setup ───────────────────────────────────────────────────────────
n_cores <- max(1L, parallelly::availableCores() - 1L)
plan(multisession, workers = n_cores)
message("Parallel backend: ", n_cores, " workers")

# =============================================================================
# 1. DATA INGESTION
# =============================================================================

FILE_PATH <- "/Users/sjoh23/Library/CloudStorage/OneDrive-SCH/SCH\ research\ projects/Spot\ seq\ update\ project/SpotSeq_Project_Dataset_deidentified_2.1.xlsx"

raw      <- read_excel(FILE_PATH, sheet = 1)
df_names <- suppressWarnings(raw |> clean_names())

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

cat("\nRows:", nrow(df), "| Columns:", ncol(df), "\n")

# =============================================================================
# 2. HELPERS
# =============================================================================

ASSAYS_OF_INTEREST <- c(
  "PIK3CA MULTIPLEX", "BRAF", "TEK",
  "PIK3CA MULTIPLEX, TEK", "PIK3CA MULTIPLEX, BRAF"
)

smart_cat_test <- function(data, row_var, col_var) {
  tbl <- data |>
    count(.data[[row_var]], .data[[col_var]]) |>
    pivot_wider(names_from = all_of(col_var), values_from = n, values_fill = 0) |>
    column_to_rownames(row_var) |>
    as.matrix()
  expected   <- outer(rowSums(tbl), colSums(tbl)) / sum(tbl)
  use_fisher <- any(expected < 5)
  if (use_fisher) {
    tryCatch(
      fisher.test(tbl, simulate.p.value = TRUE, B = 10000) |> tidy() |>
        mutate(test_used = "Fisher's Exact Test (simulated; min expected cell < 5)"),
      error = function(e) tibble(note = as.character(e))
    )
  } else {
    tryCatch(
      chisq.test(tbl) |> tidy() |>
        mutate(test_used = "Pearson's Chi-squared Test"),
      error = function(e) tibble(note = as.character(e))
    )
  }
}

welch_ttest <- function(data, val_col, group_col = "result_clean",
                        pos = "POS", neg = "NEG") {
  tryCatch({
    pv <- data |> filter(.data[[group_col]] == pos,  !is.na(.data[[val_col]])) |> pull(.data[[val_col]])
    nv <- data |> filter(.data[[group_col]] == neg,  !is.na(.data[[val_col]])) |> pull(.data[[val_col]])
    t.test(pv, nv) |> tidy() |> mutate(test_used = "Welch Two-Sample t-test")
  }, error = function(e) tibble(note = as.character(e)))
}

# =============================================================================
# 3. ANALYSIS FUNCTIONS
# =============================================================================

analyse_individuals <- function(data) {
  total <- data |> distinct(.data[[COL_PATIENT_ID]]) |> nrow()
  counts <- data |> filter(result_clean %in% c("POS","NEG")) |>
    count(Result = result_clean, name = "N Tests")
  list(
    summary = tibble(`Metric` = "Total unique patients", Value = as.character(total)),
    counts  = counts,
    test    = NULL
  )
}

analyse_specimen_type <- function(data) {
  counts <- tibble(
    `Specimen Type` = c("Blood only", "Cyst fluid only", "Both Blood & Cyst Fluid"),
    `N Patients` = c(
      data |> filter(specimen_type_clean == "BLOOD")      |> distinct(.data[[COL_PATIENT_ID]]) |> nrow(),
      data |> filter(specimen_type_clean == "CYST FLUID") |> distinct(.data[[COL_PATIENT_ID]]) |> nrow(),
      data |> group_by(.data[[COL_PATIENT_ID]]) |>
        summarise(types = list(unique(specimen_type_clean)), .groups = "drop") |>
        filter(map_lgl(types, ~ all(c("BLOOD","CYST FLUID") %in% .x))) |> nrow()
    )
  )
  test <- data |>
    filter(result_clean %in% c("POS","NEG"),
           specimen_type_clean %in% c("BLOOD","CYST FLUID")) |>
    smart_cat_test("specimen_type_clean", "result_clean")
  list(summary = counts, test = test)
}

analyse_assay <- function(data) {
  counts <- data |>
    filter(assay_clean %in% ASSAYS_OF_INTEREST) |>
    count(`Assay Ordered` = assay_clean, name = "N Tests") |>
    arrange(desc(`N Tests`))
  test <- data |>
    filter(result_clean %in% c("POS","NEG"), assay_clean %in% ASSAYS_OF_INTEREST) |>
    smart_cat_test("assay_clean", "result_clean")
  list(summary = counts, test = test)
}

analyse_cancelled <- function(data) {
  n <- data |> filter(str_detect(result_clean, "CANCEL")) |> nrow()
  list(summary = tibble(`Metric` = "Cancelled tests", `N` = n), test = NULL)
}

analyse_qns <- function(data) {
  n <- data |> filter(result_clean == "QNS") |> nrow()
  list(summary = tibble(`Metric` = "QNS failures", `N` = n), test = NULL)
}

analyse_dna_stock <- function(data) {
  summ <- data |>
    filter(result_clean %in% c("POS","NEG"), !is.na(dna_stock_num)) |>
    group_by(Result = result_clean) |>
    summarise(`Mean ng/uL` = round(mean(dna_stock_num, na.rm=TRUE), 3),
              `Median ng/uL` = round(median(dna_stock_num, na.rm=TRUE), 3),
              `SD` = round(sd(dna_stock_num, na.rm=TRUE), 3),
              `N` = n(), .groups = "drop")
  test <- welch_ttest(data, "dna_stock_num")
  list(summary = summ, test = test)
}

analyse_total_ng <- function(data) {
  base <- data |> filter(result_clean %in% c("POS","NEG"), !is.na(total_ng_num))

  overall <- base |>
    group_by(Result = result_clean) |>
    summarise(`Mean Total ng` = round(mean(total_ng_num, na.rm=TRUE), 3),
              `Median Total ng` = round(median(total_ng_num, na.rm=TRUE), 3),
              `SD` = round(sd(total_ng_num, na.rm=TRUE), 3),
              `N` = n(), .groups = "drop")

  by_assay <- base |>
    filter(assay_clean %in% ASSAYS_OF_INTEREST) |>
    group_by(`Assay` = assay_clean, Result = result_clean) |>
    summarise(`Mean Total ng` = round(mean(total_ng_num, na.rm=TRUE), 3),
              `N` = n(), .groups = "drop") |>
    arrange(Assay, Result)

  ttest <- welch_ttest(data, "total_ng_num")

  anova_res <- tryCatch({
    aov_data <- base |>
      filter(assay_clean %in% ASSAYS_OF_INTEREST) |>
      mutate(result_f = factor(result_clean), assay_f = factor(assay_clean))
    fit <- aov(total_ng_num ~ result_f + assay_f, data = aov_data)
    tidy(fit) |> mutate(test_used = "Two-way ANOVA (Result + Assay)")
  }, error = function(e) tibble(note = as.character(e)))

  list(overall = overall, by_assay = by_assay,
       ttest = ttest, anova = anova_res)
}

analyse_fluid_volume <- function(data) {
  summ <- data |>
    filter(result_clean %in% c("POS","NEG"), !is.na(fluid_vol_num)) |>
    group_by(Result = result_clean) |>
    summarise(`Mean Volume` = round(mean(fluid_vol_num, na.rm=TRUE), 3),
              `Median Volume` = round(median(fluid_vol_num, na.rm=TRUE), 3),
              `SD` = round(sd(fluid_vol_num, na.rm=TRUE), 3),
              `N` = n(), .groups = "drop")
  test <- welch_ttest(data, "fluid_vol_num")
  list(summary = summ, test = test)
}

analyse_rejects <- function(data) {
  n <- data |>
    filter(str_detect(str_to_upper(as.character(.data[[COL_ANTICIPATED]])), "REJECT")) |>
    nrow()
  list(summary = tibble(`Metric` = "REJECT count (Anticipated ng column)", `N` = n), test = NULL)
}

analyse_hemolysis <- function(data) {
  hem <- data |> filter(result_clean %in% c("POS","NEG"), !is.na(hemolysis_num))
  summ <- hem |>
    group_by(Result = result_clean) |>
    summarise(`Mean Hemolysis Grade` = round(mean(hemolysis_num, na.rm=TRUE), 3),
              `Median Hemolysis Grade` = round(median(hemolysis_num, na.rm=TRUE), 3),
              `N` = n(), .groups = "drop")
  test <- tryCatch({
    pv <- hem |> filter(result_clean == "POS") |> pull(hemolysis_num)
    nv <- hem |> filter(result_clean == "NEG") |> pull(hemolysis_num)
    wilcox.test(pv, nv) |> tidy() |>
      mutate(test_used = "Wilcoxon Rank-Sum Test (non-parametric; ordinal data)")
  }, error = function(e) tibble(note = as.character(e)))
  list(summary = summ, test = test)
}

analyse_specimen_source <- function(data) {
  overall <- data |>
    count(`Specimen Source` = specimen_source_clean, name = "N") |>
    mutate(`% of Total` = scales::percent(N / sum(N), accuracy = 0.1)) |>
    arrange(desc(N))
  by_result <- data |>
    filter(result_clean %in% c("POS","NEG")) |>
    count(`Specimen Source` = specimen_source_clean, Result = result_clean, name = "N") |>
    group_by(`Specimen Source`) |>
    mutate(`% within Source` = scales::percent(N / sum(N), accuracy = 0.1)) |>
    ungroup() |>
    arrange(`Specimen Source`, Result)
  test <- data |>
    filter(result_clean %in% c("POS","NEG")) |>
    smart_cat_test("specimen_source_clean", "result_clean")
  list(overall = overall, by_result = by_result, test = test)
}

# =============================================================================
# 4. RUN ANALYSES IN PARALLEL
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

# Print to console / text file
sep <- function(t) cat("\n", strrep("=",70), "\n", t, "\n", strrep("=",70), "\n", sep="")
sep("1. UNIQUE INDIVIDUALS");        print(results$individuals$summary);  print(results$individuals$counts)
sep("2. SPECIMEN TYPE");             print(results$specimen_type$summary); print(results$specimen_type$test)
sep("3. ASSAY ORDERED");             print(results$assay$summary);         print(results$assay$test)
sep("4. CANCELLED TESTS");           print(results$cancelled$summary)
sep("5. QNS FAILURES");              print(results$qns$summary)
sep("6. DNA STOCK ng/uL");           print(results$dna_stock$summary);     print(results$dna_stock$test)
sep("7. TOTAL NG DNA — OVERALL");    print(results$total_ng$overall);      print(results$total_ng$ttest)
sep("7b. TOTAL NG DNA — BY ASSAY");  print(results$total_ng$by_assay);     print(results$total_ng$anova)
sep("8. FLUID VOLUME");              print(results$fluid_volume$summary);  print(results$fluid_volume$test)
sep("9. REJECTS");                   print(results$rejects$summary)
sep("10. HEMOLYSIS GRADE");          print(results$hemolysis$summary);     print(results$hemolysis$test)
sep("11. SPECIMEN SOURCE — OVERALL");print(results$specimen_source$overall)
sep("11b. SPECIMEN SOURCE — BY RESULT"); print(results$specimen_source$by_result); print(results$specimen_source$test)

# =============================================================================
# 5. SINGLE-SHEET EXCEL EXPORT
#    Each section: results table on the left, stat test to its right (gap col)
# =============================================================================

wb   <- createWorkbook()
ws   <- "SpotSeq Analysis"
addWorksheet(wb, ws)

# ── Style definitions ─────────────────────────────────────────────────────────
title_style <- createStyle(
  fontName = "Arial", fontSize = 12, fontColour = "#FFFFFF",
  fgFill = "#2E4057", textDecoration = "bold",
  halign = "left", valign = "center",
  border = "Bottom", borderColour = "#FFFFFF"
)
header_style <- createStyle(
  fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
  fgFill = "#4A7C9E", textDecoration = "bold",
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", borderColour = "#FFFFFF",
  wrapText = TRUE
)
stat_header_style <- createStyle(
  fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
  fgFill = "#5B6B4E", textDecoration = "bold",
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", borderColour = "#FFFFFF",
  wrapText = TRUE
)
data_style <- createStyle(
  fontName = "Arial", fontSize = 10,
  border = "TopBottomLeftRight", borderColour = "#D0D0D0",
  halign = "left", valign = "center"
)
num_style <- createStyle(
  fontName = "Arial", fontSize = 10, numFmt = "0.000",
  border = "TopBottomLeftRight", borderColour = "#D0D0D0",
  halign = "right", valign = "center"
)
alt_style <- createStyle(
  fontName = "Arial", fontSize = 10,
  fgFill = "#F2F7FB",
  border = "TopBottomLeftRight", borderColour = "#D0D0D0",
  halign = "left", valign = "center"
)
alt_num_style <- createStyle(
  fontName = "Arial", fontSize = 10, numFmt = "0.000",
  fgFill = "#F2F7FB",
  border = "TopBottomLeftRight", borderColour = "#D0D0D0",
  halign = "right", valign = "center"
)
pval_style <- createStyle(
  fontName = "Arial", fontSize = 10, numFmt = "0.0000",
  fgFill = "#FFF8E7",
  border = "TopBottomLeftRight", borderColour = "#D0D0D0",
  halign = "right", valign = "center"
)

# ── Core writer function ──────────────────────────────────────────────────────
# Writes a results table at (start_row, col=1) and its stat test at (start_row, stat_col)
# Returns the next available row after both tables

write_section <- function(wb, ws, start_row, section_title,
                          result_df, stat_df = NULL,
                          stat_col_offset = NULL) {

  # ── Section title bar ──
  writeData(wb, ws, section_title, startRow = start_row, startCol = 1)
  n_res_cols <- ncol(result_df)
  mergeCells(wb, ws, rows = start_row, cols = 1:max(n_res_cols, 4))
  addStyle(wb, ws, title_style, rows = start_row, cols = 1:max(n_res_cols, 4), stack = FALSE)
  setRowHeights(wb, ws, start_row, 22)

  # ── Result table header ──
  hdr_row <- start_row + 1
  writeData(wb, ws, result_df, startRow = hdr_row, startCol = 1,
            headerStyle = header_style, borders = "all", borderStyle = "thin")

  # ── Alternating row colours for result table ──
  for (i in seq_len(nrow(result_df))) {
    r   <- hdr_row + i
    sty <- if (i %% 2 == 0) alt_style else data_style
    addStyle(wb, ws, sty, rows = r, cols = 1:n_res_cols, stack = FALSE, gridExpand = TRUE)
  }

  result_end_row <- hdr_row + nrow(result_df)

  # ── Stat test table (to the right) ──
  if (!is.null(stat_df) && nrow(stat_df) > 0) {
    gap        <- 2L                          # one blank column between tables
    stat_start <- if (!is.null(stat_col_offset)) stat_col_offset else n_res_cols + gap

    # Stat table header
    writeData(wb, ws, stat_df, startRow = hdr_row, startCol = stat_start,
              headerStyle = stat_header_style, borders = "all", borderStyle = "thin")

    # Style stat table rows
    n_stat_cols <- ncol(stat_df)
    for (i in seq_len(nrow(stat_df))) {
      r <- hdr_row + i
      addStyle(wb, ws, pval_style, rows = r,
               cols = stat_start:(stat_start + n_stat_cols - 1),
               stack = FALSE, gridExpand = TRUE)
    }
  }

  # Return row after section + 2-row gap
  result_end_row + 3L
}

# ── Set column widths ─────────────────────────────────────────────────────────
setColWidths(wb, ws, cols = 1,  widths = 35)
setColWidths(wb, ws, cols = 2,  widths = 14)
setColWidths(wb, ws, cols = 3,  widths = 14)
setColWidths(wb, ws, cols = 4,  widths = 14)
setColWidths(wb, ws, cols = 5,  widths = 14)
setColWidths(wb, ws, cols = 6,  widths = 2)   # gap
setColWidths(wb, ws, cols = 7,  widths = 14)
setColWidths(wb, ws, cols = 8,  widths = 14)
setColWidths(wb, ws, cols = 9,  widths = 14)
setColWidths(wb, ws, cols = 10, widths = 14)
setColWidths(wb, ws, cols = 11, widths = 40)
setColWidths(wb, ws, cols = 12, widths = 14)

# ── Freeze top row navigation ─────────────────────────────────────────────────
freezePane(wb, ws, firstActiveRow = 2)

# ── Write all sections ────────────────────────────────────────────────────────
cur_row <- 1L

# 1. Unique individuals
indiv_tbl <- bind_rows(
  results$individuals$summary |> rename(Metric = Metric, Value = Value) |>
    mutate(across(everything(), as.character)),
  results$individuals$counts  |>
    mutate(across(everything(), as.character)) |>
    rename(Metric = result, Value = `N Tests`)
)
cur_row <- write_section(wb, ws, cur_row,
                         "1. Unique Individuals Tested",
                         indiv_tbl, stat_df = NULL)

# 2. Specimen type
cur_row <- write_section(wb, ws, cur_row,
                         "2. Specimen Type (Blood / Cyst Fluid / Both)",
                         results$specimen_type$summary,
                         stat_df = results$specimen_type$test,
                         stat_col_offset = 4)

# 3. Assay ordered
cur_row <- write_section(wb, ws, cur_row,
                         "3. Assay Ordered",
                         results$assay$summary,
                         stat_df = results$assay$test,
                         stat_col_offset = 4)

# 4. Cancelled
cur_row <- write_section(wb, ws, cur_row,
                         "4. Cancelled Tests",
                         results$cancelled$summary)

# 5. QNS
cur_row <- write_section(wb, ws, cur_row,
                         "5. QNS Failures (Assay Problem)",
                         results$qns$summary)

# 6. DNA stock
cur_row <- write_section(wb, ws, cur_row,
                         "6. DNA Stock Concentration (ng/uL) — POS vs NEG",
                         results$dna_stock$summary,
                         stat_df = results$dna_stock$test,
                         stat_col_offset = 6)

# 7a. Total ng — overall
cur_row <- write_section(wb, ws, cur_row,
                         "7a. Total ng DNA Used in Assay — Overall (POS vs NEG)",
                         results$total_ng$overall,
                         stat_df = results$total_ng$ttest,
                         stat_col_offset = 7)

# 7b. Total ng — by assay with ANOVA
cur_row <- write_section(wb, ws, cur_row,
                         "7b. Total ng DNA Used in Assay — By Assay Subgroup",
                         results$total_ng$by_assay,
                         stat_df = results$total_ng$anova,
                         stat_col_offset = 5)

# 8. Fluid volume
cur_row <- write_section(wb, ws, cur_row,
                         "8. Fluid Volume Extracted — POS vs NEG",
                         results$fluid_volume$summary,
                         stat_df = results$fluid_volume$test,
                         stat_col_offset = 6)

# 9. Rejects
cur_row <- write_section(wb, ws, cur_row,
                         "9. REJECT Count (Anticipated ng of DNA Column)",
                         results$rejects$summary)

# 10. Hemolysis
cur_row <- write_section(wb, ws, cur_row,
                         "10. Hemolysis Grade — POS vs NEG",
                         results$hemolysis$summary,
                         stat_df = results$hemolysis$test,
                         stat_col_offset = 5)

# 11a. Specimen source overall
cur_row <- write_section(wb, ws, cur_row,
                         "11a. Specimen Source — Overall Percentages",
                         results$specimen_source$overall)

# 11b. Specimen source by result + chi-square
cur_row <- write_section(wb, ws, cur_row,
                         "11b. Specimen Source — POS vs NEG Subgroups",
                         results$specimen_source$by_result,
                         stat_df = results$specimen_source$test,
                         stat_col_offset = 6)

# ── Save workbook ─────────────────────────────────────────────────────────────
excel_path <- "~/Desktop/SpotSeq_Analysis_Results.xlsx"
saveWorkbook(wb, excel_path, overwrite = TRUE)
cat("\nExcel saved to:", excel_path, "\n")

# ── Cleanup ───────────────────────────────────────────────────────────────────
plan(sequential)
cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Text output : ~/Desktop/SpotSeq_Analysis_Output.txt\n")
cat("Excel output: ~/Desktop/SpotSeq_Analysis_Results.xlsx\n")
sink()
