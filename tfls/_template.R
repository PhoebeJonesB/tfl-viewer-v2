# =============================================================================
# TFL SCRIPT TEMPLATE
# =============================================================================
#
# HOW TO USE THIS TEMPLATE
# ------------------------
# 1. Copy this file to a new file named  tfl_<id>.R  in the tfls/ folder.
#    Naming convention:
#      t  = Table     e.g.  tfl_t14_1_1.R
#      f  = Figure    e.g.  tfl_f14_2_1.R
#      l  = Listing   e.g.  tfl_l16_1_1.R
#
# 2. Fill in the four sections below:
#      [1] METADATA        – machine-readable TFL identity
#      [2] SOURCE DATASETS – declare which ADaM datasets are used
#      [3] FILTER CODE     – a character string for display in the app
#      [4] TFL OUTPUT      – the final table (data.frame) or figure (ggplot)
#
# 3. The app (runner.R) executes this script in a child environment that
#    already contains the ADaM datasets (adsl, adae, adlb, adtte).
#    You do NOT need to load data; just reference it by name.
#
# CONTRACT OBJECTS (must be defined before the end of the script)
# ---------------------------------------------------------------
#   tfl_metadata    - named list: id, name, type, datasets, description
#   source_datasets - named list of data frames used as analysis inputs
#   filter_code     - character string shown in "Filter Code" tab
#   tfl_output      - data.frame (Table/Listing) OR ggplot object (Figure)
#
# PACKAGES
# --------
#   dplyr and ggplot2 are pre-loaded by global.R.
#   For survival analyses, library(survival) is available.
#   Do NOT call install.packages() inside a TFL script.
#
# =============================================================================

# Study:       STUDY001
# TFL ID:      t00_0_0          ← change me
# TFL Name:    Table 00.0.0 – [Descriptive Title]
# Type:        Table            ← "Table" | "Figure" | "Listing"
# Datasets:    adsl             ← comma-separated dataset names
# Description: One sentence describing what this TFL shows.
# Author:      [Your Name]
# Date:        YYYY-MM-DD


# =============================================================================
# SECTION 1 – METADATA
# =============================================================================
# Used by the TFL Registry table and the viewer header.

tfl_metadata <- list(
  id          = "t00_0_0",
  name        = "Table 00.0.0 \u2013 [Descriptive Title]",
  type        = "Table",          # "Table" | "Figure" | "Listing"
  datasets    = c("adsl"),        # must match names in generate_adam_data()
  description = "Brief plain-English description of this TFL."
)


# =============================================================================
# SECTION 2 – SOURCE DATASETS
# =============================================================================
# List every ADaM dataset this TFL reads from.
# The app uses this list to populate the Dataset Explorer and Investigate tabs.
# Names must match objects injected by the runner (adsl, adae, adlb, adtte).

source_datasets <- list(
  adsl = adsl
  # adae  = adae,    # uncomment if used
  # adlb  = adlb,    # uncomment if used
  # adtte = adtte    # uncomment if used
)


# =============================================================================
# SECTION 3 – FILTER / ANALYSIS POPULATION CODE  (display string)
# =============================================================================
# Write the dataset preparation steps as a readable character string.
# This is shown verbatim in the "Filter Code" tab so keep it clean.
# The actual filtering (Section 4) should match this string exactly.

filter_code <- "
# Analysis population: Intent-to-Treat (ITT)
analysis_data <- adsl |>
  filter(ITTFL == 'Y')
"


# =============================================================================
# SECTION 4 – APPLY FILTERS  (must produce object `analysis_data`)
# =============================================================================
# Mirror the logic described in filter_code above.
# Use base R or dplyr – both work inside the runner environment.

analysis_data <- source_datasets$adsl |>
  dplyr::filter(ITTFL == "Y")


# =============================================================================
# SECTION 5 – CREATE TFL OUTPUT  (must produce object `tfl_output`)
# =============================================================================
# For Tables / Listings: produce a data.frame or tibble.
# For Figures:           produce a ggplot object (do NOT call print() or ggsave()).
#
# The app detects the output type automatically:
#   is.data.frame(tfl_output) → renders as an interactive DT table
#   inherits(tfl_output, "ggplot") → renders as a plot via renderPlot()

tfl_output <- analysis_data   # ← replace with your summary / visualisation code

# Example Table skeleton:
# tfl_output <- analysis_data |>
#   dplyr::group_by(TRT01A, DCSREAS) |>
#   dplyr::summarise(n = dplyr::n(), .groups = "drop")

# Example Figure skeleton:
# tfl_output <- ggplot2::ggplot(analysis_data, ggplot2::aes(x = TRT01A)) +
#   ggplot2::geom_bar() +
#   ggplot2::theme_bw()
