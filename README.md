# SpotSeq Dataset Analysis

Analysis pipeline for the SpotSeq study — produces Tables 1–4 (cohort
characteristics, test validity, assay performance, clinical impact) for the
manuscript and supplementary materials.

The entire analysis lives in a single Quarto file:
[`spotseq_analysis_v2.qmd`](spotseq_analysis_v2.qmd).

---

## Quick start

### Preview HTML report (fast, no exports)

```bash
quarto preview spotseq_analysis_v2.qmd
```

Or in RStudio: open the QMD and click **Render**.

### Generate full deliverable bundle (Excel + Word + Google Drive upload)

```bash
quarto render spotseq_analysis_v2.qmd -P export:true
```

Or in R:

```r
quarto::quarto_render("spotseq_analysis_v2.qmd",
                      execute_params = list(export = TRUE))
```

Outputs land in `outputs/`:

- `SpotSeq_Tables_YYYY-MM-DD.xlsx` — 4-sheet workbook (also uploaded to Google Drive)
- `Table1_YYYY-MM-DD.docx` … `Table4_YYYY-MM-DD.docx` — Word-formatted tables for the manuscript

The xlsx is also uploaded to the Google Drive folder specified by
`DRIVE_FOLDER_ID` (see Environment Variables below).

---

## Environment setup

### Required environment variables

These are read from `.Renviron` (in the project folder or your home directory).

| Variable           | Required          | Purpose                                                                      |
| ------------------ | ----------------- | ---------------------------------------------------------------------------- |
| `DATA_PATH`        | always            | Folder containing the deidentified Excel dataset (kept outside Git for PHI). |
| `DRIVE_FOLDER_ID`  | only for export   | Google Drive folder ID for the uploaded .xlsx.                               |
| `DATA_FILE`        | optional          | Override default filename. Otherwise uses `SpotSeq_Project_Dataset_deidentified_2.1.xlsx` or the most recent matching file in `DATA_PATH`.        |

Example `.Renviron`:

```
DATA_PATH=~/Library/CloudStorage/OneDrive-SCH/SCH research projects/Spot seq update project/Data
DRIVE_FOLDER_ID=1xY...replace-with-real-id...
```

### R package dependencies

The project uses `renv` for reproducible package versions. After cloning:

```r
renv::restore()
```

This installs the exact versions recorded in `renv.lock`. To snapshot
package updates after `install.packages()`:

```r
renv::snapshot()
```

---

## Project structure

```
Analysis/
├── spotseq_analysis_v2.qmd     ← single source of truth — analysis lives here
├── README.md                   ← this file
├── outputs/                    ← versioned deliverables (Excel, Word)
├── renv.lock                   ← R package version manifest
├── .Renviron                   ← env vars (NOT in git; create locally)
├── .gitignore
└── Analysis.Rproj
```

Data files are stored separately in `DATA_PATH` (typically OneDrive) and are
**not** committed to this repository.

---

## Workflow & version control

### Day-to-day

```bash
# After making changes to the QMD:
quarto preview spotseq_analysis_v2.qmd      # verify HTML still renders correctly
git add spotseq_analysis_v2.qmd
git commit -m "describe change"             # meaningful commit messages help future-you
git push
```

### When producing a manuscript-ready deliverable

```bash
quarto render spotseq_analysis_v2.qmd -P export:true
git add outputs/                            # commit the exported tables alongside the code
git commit -m "outputs: <date> — manuscript draft v<n>"
git tag manuscript-draft-vN                 # optional: mark this snapshot for easy retrieval
git push --tags
```

The tag lets you (or an auditor) jump back to exactly the code + data state
that produced a specific manuscript version with `git checkout manuscript-draft-vN`.

### Audit trail

Every committed version of the analysis is permanently retrievable via Git
history:

```bash
git log spotseq_analysis_v2.qmd                          # full change history
git show <commit>:spotseq_analysis_v2.qmd                # view file at any past commit
git checkout <commit>                                    # restore project to past state
```

Deletions, moves, and refactors do not lose the prior content — they are
recorded as commits, and the prior versions remain accessible.

---

## Adding new data

When new rows are added to the source Excel file:

1. Place the updated file in `DATA_PATH` (overwriting the old one is fine; or
   bump the version number in the filename — the QMD auto-detects the most
   recent matching file).
2. Run `quarto preview spotseq_analysis_v2.qmd` and check the **Data Diagnostics**
   section near the bottom of the rendered HTML. It surfaces:
   - Unhandled assays (not matching PIK3CA/TEK/BRAF/GNAQ)
   - Specimen types grouped under "Other"
   - PIK3CA-positive tests with non-canonical variants
   - Missing clinical-impact columns

   Any flagged values mean the analysis logic should be updated to handle them
   before the tables are trusted for publication.
3. If everything looks correct, run the full export and commit the new
   outputs.

---

## Statistical methods (summary)

- **Continuous variables** (DNA concentration, droplet counts, fluid volume):
  reported as median [IQR]; compared between Positive and Negative groups
  with the Mann–Whitney U test.
- **Categorical variables** (specimen type Blood vs Cyst Fluid by result):
  compared with Fisher's exact test, given small expected cell counts.
- **Two-sided p-values < 0.05** considered statistically significant. No
  adjustment for multiple comparisons; p-values are exploratory.

Full methods text is in the manuscript draft.

---

## Contact

Issues or questions: solomonj88@gmail.com
