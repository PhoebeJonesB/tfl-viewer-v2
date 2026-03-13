# Clinical TFL Viewer v2

An interactive R Shiny application for exploring, filtering, and investigating
Tables, Figures, and Listings (TFLs) produced from CDISC ADaM datasets.

---

## What This App Does

| Capability | Description |
|---|---|
| **TFL Registry** | Scans a folder for TFL scripts, runs them, and lists every output in a searchable registry table |
| **Automatic dataset tracking** | Each TFL script declares its source datasets; the app captures them and makes them available for exploration |
| **TFL Output viewer** | Displays formatted tables (interactive DT) and publication-quality figures (ggplot2) |
| **Dual code view** | Shows the full TFL script and the dataset preparation/filter code side-by-side |
| **Live filter code** | As you adjust population flags, the equivalent dplyr code updates in real time |
| **Population flag filters** | Any column ending in `FL` (SAFFL, ITTFL, RANDFL, PPROTFL, …) is auto-detected and exposed as an interactive filter |
| **Dataset explorer** | Browse the underlying source data, filtered by your population selections |
| **Investigate mode** | Unique value counts + bar charts, per-subject patient listings, and a free-form plot builder |
| **Re-run TFL** | Re-execute a TFL script against your current population filter to see how results change |
| **Excel download** | Export the currently filtered dataset as `.xlsx` |

---

## Quick Start

### 1. Install required packages

```r
install.packages(c(
  "shiny", "bslib", "DT", "dplyr", "ggplot2",
  "survival", "scales", "writexl", "forcats"
))
```

### 2. Open the project

Open `tfl-viewer-v2.Rproj` in RStudio, then click **Run App** (or run `shiny::runApp()`).

### 3. What happens at startup

`global.R` runs once per worker and:
1. Sources all helper files in `R/`
2. Calls `generate_adam_data()` to create synthetic CDISC ADaM datasets
3. Calls `discover_tfls("tfls/", adam)` to find and execute all `tfl_*.R` scripts
4. Builds the TFL registry data frame

The app opens on the **TFL Registry** tab showing all loaded TFLs.

---

## Project Structure

```
tfl-viewer-v2/
├── global.R                 # Package loading, data generation, TFL discovery
├── ui.R                     # Full Shiny UI (page_navbar layout)
├── server.R                 # All reactive logic
├── tfl-viewer-v2.Rproj      # RStudio project
│
├── R/
│   ├── data_gen.R           # Synthetic ADaM data generator
│   ├── runner.R             # TFL script runner and registry builder
│   └── utils.R              # Shared helpers (fmt_n_pct, km_to_df, etc.)
│
├── tfls/
│   ├── _template.R          # Blank template – copy to create new TFLs
│   ├── tfl_t14_1_1.R        # Table 14.1.1 – Subject Disposition (ADSL)
│   ├── tfl_t14_3_1.R        # Table 14.3.1 – Adverse Events (ADSL + ADAE)
│   ├── tfl_f14_1_1.R        # Figure 14.1.1 – Mean ALT Change (ADSL + ADLB)
│   └── tfl_f14_2_1.R        # Figure 14.2.1 – KM Plot (ADTTE)
│
└── docs/
    └── tfl-viewer-guide.Rmd # Renders to Word .docx or HTML
```

---

## The TFL Script Contract

Every script in `tfls/` must define **four objects** before it ends:

| Object | Type | Purpose |
|---|---|---|
| `tfl_metadata` | named list | Machine-readable identity (id, name, type, datasets, description) |
| `source_datasets` | named list of data frames | The datasets this TFL reads from |
| `filter_code` | character string | Human-readable dataset preparation code (shown in Filter Code tab) |
| `tfl_output` | `data.frame` or `ggplot` | Final output displayed in the Output tab |

Scripts are executed inside a child environment that already contains `adsl`, `adae`, `adlb`, and `adtte`, so you reference them directly without loading.

### Adding a New TFL

1. Copy `tfls/_template.R` to `tfls/tfl_<your_id>.R`
2. Fill in the four sections (METADATA → SOURCE DATASETS → FILTER CODE → TFL OUTPUT)
3. Click **Refresh** in the TFL Registry tab (or restart the app)

---

## Example TFLs Included

### Table 14.1.1 – Subject Disposition (`tfl_t14_1_1.R`)
- **Datasets**: ADSL only
- **Population**: ITT (`ITTFL == "Y"`)
- **Output**: Tabulation of disposition reasons (Completed, AE, Withdrawal, etc.) with n (%) per treatment arm and total

### Table 14.3.1 – Adverse Events by SOC/PT (`tfl_t14_3_1.R`)
- **Datasets**: ADSL + ADAE
- **Population**: Safety (`SAFFL == "Y"`)
- **Output**: Any AE, any serious AE, then each SOC and Preferred Term with subject counts and percentages

### Figure 14.1.1 – Mean Change from Baseline in ALT (`tfl_f14_1_1.R`)
- **Datasets**: ADSL + ADLB
- **Population**: Safety, `ANL01FL == "Y"`, `PARAMCD == "ALT"`, post-baseline visits
- **Output**: Grouped bar chart with ± 1 SE error bars by scheduled visit and treatment

### Figure 14.2.1 – KM: Time to First Adverse Event (`tfl_f14_2_1.R`)
- **Datasets**: ADTTE
- **Population**: Safety, `PARAMCD == "TTFAE"`
- **Output**: Kaplan-Meier step curves with 95% CI ribbon, censoring ticks, median annotations

---

## Using the App

### TFL Registry Tab

- Browse all TFLs in a sortable, searchable table
- Status column shows ✅ OK or ❌ ERROR for each script
- Click a row and it loads automatically in the TFL Viewer
- **Refresh** button re-runs all scripts from disk (picks up new or edited TFLs without restarting)

### TFL Viewer Tab

**Sidebar controls:**

1. **TFL selector** – switch between TFLs
2. **Dataset selector** – when a TFL uses multiple datasets (e.g. ADSL + ADAE), toggle between them for the explorer
3. **Population flags** – auto-detected from the selected dataset; each flag gets its own dropdown (All / Y / N)
4. **Quick stats** – live row count, subject count, and active filter count
5. **Download** – exports the currently filtered dataset as Excel

**Output tab** – displays the pre-computed TFL output (table or figure). If you have re-run the TFL with filters, a badge appears indicating the output has been refreshed.

**Dataset Explorer tab:**
- *Data sub-tab*: full DT table of the filtered dataset with column-level search boxes
- *Investigate sub-tab*:
  - **Unique Values**: pick any column → count/percentage table + bar chart
  - **Patient Listing**: pick a USUBJID → see all rows for that subject in the current dataset
  - **Exploratory Plot**: choose plot type (Bar/Histogram/Box/Scatter), X-axis, Y-axis, colour-by

**TFL Code tab** – full source of the TFL script as it exists on disk

**Filter Code tab:**
- *Script Filter Recipe* – the `filter_code` string from the TFL script
- *Live Filter Code* – dplyr equivalent of your current population flag selections (updates in real time)
- **Re-run TFL with Current Filters** button – re-executes the script using only subjects passing your current flags; updates the Output tab

---

## Connecting to Real Data

`global.R` currently calls `generate_adam_data()`.  Replace that block with your own data loading:

```r
# Option A – SAS datasets (requires haven)
library(haven)
adsl  <- read_sas("path/to/adsl.sas7bdat")
adae  <- read_sas("path/to/adae.sas7bdat")
adlb  <- read_sas("path/to/adlb.sas7bdat")
adtte <- read_sas("path/to/adtte.sas7bdat")
adam  <- list(adsl = adsl, adae = adae, adlb = adlb, adtte = adtte)

# Option B – Parquet (requires arrow)
library(arrow)
adsl <- read_parquet("path/to/adsl.parquet")
# …etc
```

The runner passes this `adam` list to each TFL script. No changes to the TFL scripts are needed as long as the column names follow CDISC ADaM conventions.

---

## What Works Well

| Area | Notes |
|---|---|
| Auto-discovery | Drop a new `tfl_*.R` file in `tfls/` and click Refresh — no other changes needed |
| Graceful error handling | Failed scripts are shown in the registry with ❌ and an error message; the rest of the app still functions |
| Population flag detection | Any `*FL` column is automatically surfaced as an interactive filter |
| Dual code display | Static script code and live dplyr filter code update independently |
| Mixed output types | Tables (DT) and figures (ggplot) are both handled with a single `tfl_output` object |
| Re-run capability | Scripts can be re-executed against filtered populations with the Re-run button |
| Download | Filtered datasets export cleanly to `.xlsx` |
| Isolated environments | Each script runs in a child environment; side-effects do not leak between scripts |

---

## Known Limitations and Areas to Improve

### High Priority

1. **Re-run button applies population flags only** — column-level filters set inside the DT explorer are not yet passed back to the re-run environment. A more complete implementation would expose a unified filter state and pass it to `rerun_tfl_with_population()`.

2. **forcats dependency** — the Investigate → Unique Values bar chart uses `forcats::fct_infreq()`. Add `library(forcats)` to `global.R` or replace with base R `reorder()`.

3. **No persistent filter state** — population flag selections reset when you switch TFLs. URL-based bookmarking (`enableBookmarking("url")`) would preserve state across TFL changes.

4. **Dataset explorer column filters are visual only** — the DT column search boxes filter the display but the "Quick Stats" and Download still reflect only the population flag filters, not the column filters. Unifying these into a single reactive filter chain would fix this.

### Medium Priority

5. **No formal table formatting** — `tfl_output` for tables is a raw data frame rendered in DT. For publication-quality output with spanning headers and footnotes, integrate `gt` or `flextable`.

6. **KM plot at-risk table** — the KM figure lacks a number-at-risk table below the x-axis. Adding one (e.g. via `ggsurvfit`) would make the figure more interpretable.

7. **Plot download** — figures can only be exported by right-clicking the plot in the browser. A proper `downloadHandler` with `ggsave()` would improve the workflow.

8. **Multi-select population flags** — the current selectInput allows only one value per flag. `selectizeInput(multiple = TRUE)` would support "show SAFFL == Y OR SAFFL == N" use cases.

9. **No authentication** — for real clinical data, add `shinymanager` or Posit Connect's built-in auth before deployment.

### Low Priority / Nice to Have

10. **Listing TFL type** — only Table and Figure types are currently demonstrated. Listings follow the same contract but may benefit from pagination-specific DT options.

11. **Parallel TFL execution** — `discover_tfls()` runs scripts sequentially. For large studies (50+ TFLs), wrapping with `parallel::mclapply()` or `future.apply::future_lapply()` would reduce startup time.

12. **Progress indicator during startup** — a loading spinner or progress bar on `global.R` execution would improve first-load UX.

13. **Study metadata header** — a configurable study title/sponsor/data cut-off date banner across all tabs.

14. **Test suite** — no automated tests. Adding `testthat` unit tests for `data_gen.R`, `runner.R`, and `utils.R` would improve robustness, especially for real-data connections.

---

## Package Dependencies

| Package | Version | Use |
|---|---|---|
| shiny | ≥ 1.7 | Framework |
| bslib | ≥ 0.6 | Bootstrap 5 UI components |
| DT | ≥ 0.28 | Interactive tables |
| dplyr | ≥ 1.1 | Data manipulation |
| ggplot2 | ≥ 3.4 | Figures |
| survival | ≥ 3.5 | Kaplan-Meier (KM figure) |
| scales | ≥ 1.2 | Percent axis labels (KM figure) |
| writexl | ≥ 1.4 | Excel export |
| forcats | ≥ 1.0 | Frequency-ordered bar charts (Investigate tab) |

---

## Rendering This Guide as a Word Document

```r
# From the R console (requires pandoc on PATH):
rmarkdown::render("docs/tfl-viewer-guide.Rmd", output_format = "word_document")

# Or from the terminal:
# pandoc README.md -o docs/tfl-viewer-guide.docx
```

---

*Built with R Shiny + bslib. All data in this demo is entirely synthetic.*
