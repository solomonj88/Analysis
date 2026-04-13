# fix_spotseq_columns.R
# Run this script in your Analysis project to patch spotseq_analysis.qmd
# in place with the correct column names from your dataset.
#
# Usage: source("fix_spotseq_columns.R")

qmd <- "spotseq_analysis.qmd"
stopifnot("spotseq_analysis.qmd not found - make sure you are in the Analysis project" = file.exists(qmd))

txt <- readLines(qmd, warn = FALSE)

# ── 1. Replace column candidate lists with exact column names ─────────────────
old_cols <- c(
  'col_result     <- pick_col(df, c("result", "final_result", "test_result",',
  '                                  "assay_result"))',
  'col_assay      <- pick_col(df, c("assay_type", "assay", "test_type",',
  '                                  "assay_name"))',
  'col_sex        <- pick_col(df, c("sex", "gender", "patient_sex"))',
  'col_age        <- pick_col(df, c("age", "age_years", "patient_age"))',
  'col_internal   <- pick_col(df, c("internally_processed", "processed_in_house",',
  '                                  "internal", "in_house"))',
  'col_vol        <- pick_col(df, c("fluid_volume_extracted_ul",',
  '                                  "fluid_volume_ul", "volume_extracted_ul",',
  '                                  "volume_ul", "fluid_volume"))',
  'col_dna_conc   <- pick_col(df, c("dna_stock_concentration_ng_ul",',
  '                                  "dna_concentration", "dna_conc_ng_ul",',
  '                                  "stock_conc"))',
  'col_dna_used   <- pick_col(df, c("total_dna_used_ng", "dna_used_ng",',
  '                                  "dna_used", "total_dna"))',
  'col_droplets   <- pick_col(df, c("droplets_per_well", "mean_droplets",',
  '                                  "droplets"))',
  'col_ex21       <- pick_col(df, c("exon_21_wt_droplets", "exon21_wt",',
  '                                  "ex21_wt", "wt_droplets_exon21"))',
  'col_ex10       <- pick_col(df, c("exon_10_wt_droplets", "exon10_wt",',
  '                                  "ex10_wt", "wt_droplets_exon10"))'
)

new_cols <- c(
  '# Exact column names from SpotSeq_Project_Dataset_deidentified_2.1.xlsx',
  'col_result   <- "result_pos_neg"',
  'col_assay    <- "type_of_test"',
  'col_sex      <- "assigned_sex_at_birth"',
  'col_age      <- "age_at_date_of_collection_in_years"',
  'col_internal <- "internal_patient_y_n"',
  'col_vol      <- "fluid_volume_extracted"',
  'col_dna_conc <- "dna_stock_ng_ul_blood_0_185ng_ul_reject_cyst_fluid_0_37ng_ul_reject"',
  'col_dna_used <- "total_ng_dna_used_in_assay"',
  'col_droplets <- "single_probe_braf_tek_etc_hex_pos_1500_avg_number_droplets"',
  'col_ex21     <- "exon_21_wt_1500_avg_number_of_droplets"',
  'col_ex10     <- "exon_10_wt_1500_avg_number_droplets"'
)

# Find start of old block
start_idx <- which(trimws(txt) == trimws(old_cols[1]))
if (length(start_idx) == 0) {
  message("Column block already patched or not found - skipping column fix")
} else {
  start_idx <- start_idx[1]
  end_idx   <- start_idx + length(old_cols) - 1
  txt <- c(txt[seq_len(start_idx - 1)], new_cols, txt[(end_idx + 1):length(txt)])
  message("✅ Column names patched")
}

# ── 2. Fix is_pos / is_neg operator precedence ────────────────────────────────
txt <- gsub(
  'is_pos       = is_valid & result_std == "POSITIVE" \\|\n.*is_valid & result_std == "POS",',
  'is_pos       = is_valid & (result_std %in% c("POSITIVE", "POS")),',
  paste(txt, collapse = "\n")
)
txt <- gsub(
  'is_neg       = is_valid & result_std == "NEGATIVE" \\|\n.*is_valid & result_std == "NEG",',
  'is_neg       = is_valid & (result_std %in% c("NEGATIVE", "NEG")),',
  txt
)
txt <- strsplit(txt, "\n")[[1]]

# ── 3. Fix col_enc to use collection_date ─────────────────────────────────────
txt <- gsub(
  'col_enc <- pick_col\\(df_valid, c\\("encounter_id", "visit_id", "order_date",.*?"date"\\)\\)',
  'col_enc <- "collection_date"',
  paste(txt, collapse = "\n")
)
txt <- strsplit(txt, "\n")[[1]]

# ── 4. Remove loadWorkbook / template references from export chunk ─────────────
# Find export-excel chunk boundaries
chunk_start <- which(grepl("^```\\{r export-excel\\}", txt))
chunk_end   <- which(grepl("^```$", txt))
chunk_end   <- chunk_end[chunk_end > chunk_start[1]][1]

if (length(chunk_start) > 0) {
  chunk_lines <- txt[chunk_start:chunk_end]
  # Replace any loadWorkbook line with createWorkbook
  chunk_lines <- gsub(
    'wb <- loadWorkbook.*',
    'wb <- createWorkbook()',
    chunk_lines
  )
  # Remove template-finding lines
  keep <- !grepl("Pre-lim|template_file|project_path.*dirname|candidates|file.exists\\(template|stop\\(.*template", chunk_lines)
  txt[chunk_start:chunk_end] <- chunk_lines
  message("✅ Export chunk cleaned")
}

# ── 5. Write back ──────────────────────────────────────────────────────────────
writeLines(txt, qmd)
message("✅ spotseq_analysis.qmd updated successfully!")
message("\nNow run: quarto::quarto_render('spotseq_analysis.qmd')")
