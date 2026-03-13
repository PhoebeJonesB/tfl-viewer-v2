# =============================================================================
# global.R
# Clinical TFL Viewer v2 – App Startup
# =============================================================================
# Purpose : Executed ONCE per R worker process before any session begins.
#           Loads packages, sources helpers, generates synthetic ADaM data,
#           and runs all TFL scripts to populate the registry.
#
# Execution order (Shiny convention):
#   global.R  →  ui.R  →  server.R (once per user session)
# =============================================================================


# -----------------------------------------------------------------------------
# 1. PACKAGES
# Required packages – install any missing ones before launching the app:
#   install.packages(c("shiny", "bslib", "DT", "dplyr", "ggplot2",
#                       "survival", "scales", "writexl"))
# -----------------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(ggplot2)
library(survival)   # Kaplan-Meier (tfl_f14_2_1.R)
library(scales)     # percent_format() used in KM plot
library(writexl)    # server-side Excel export
library(forcats)    # frequency-ordered bar charts (Investigate tab)


# -----------------------------------------------------------------------------
# 2. SOURCE HELPER MODULES
# All files in R/ are auto-sourced so functions are available globally.
# -----------------------------------------------------------------------------

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, source))


# -----------------------------------------------------------------------------
# 3. GENERATE SYNTHETIC ADaM DATA
# generate_adam_data() is defined in R/data_gen.R.
# Replace this block with haven::read_sas() or arrow::read_parquet() calls
# when connecting to real study data.
# -----------------------------------------------------------------------------

message("[global] Generating synthetic ADaM datasets ...")
adam <- generate_adam_data(n_subjects = 200, seed = 4729)

# Expose datasets as top-level objects (TFL scripts reference them by name)
adsl  <- adam$adsl
adae  <- adam$adae
adlb  <- adam$adlb
adtte <- adam$adtte

message(sprintf(
  "[global] ADSL: %d subjects | ADAE: %d rows | ADLB: %d rows | ADTTE: %d rows",
  nrow(adsl), nrow(adae), nrow(adlb), nrow(adtte)
))


# -----------------------------------------------------------------------------
# 4. DISCOVER AND RUN ALL TFL SCRIPTS
# discover_tfls() is defined in R/runner.R.
# tfl_list  – named list; one element per TFL script, containing metadata,
#             source datasets, code strings, and the rendered output object.
# tfl_registry – flat data frame used in the Registry tab DT table.
# -----------------------------------------------------------------------------

TFL_DIR <- "tfls"   # relative to the project root (where global.R lives)

message("[global] Discovering TFL scripts in '", TFL_DIR, "' ...")
tfl_list     <- discover_tfls(TFL_DIR, adam)
tfl_registry <- build_registry(tfl_list)

message(sprintf(
  "[global] %d TFL(s) loaded: %d OK, %d ERROR",
  nrow(tfl_registry),
  sum(tfl_registry$status == "\u2705 OK"),
  sum(tfl_registry$status == "\u274C ERROR")
))


# -----------------------------------------------------------------------------
# 5. APP-WIDE CONSTANTS
# -----------------------------------------------------------------------------

APP_TITLE   <- "Clinical TFL Viewer v2"
APP_VERSION <- "2.0.0"
STUDY_ID    <- unique(adsl$STUDYID)[1]

# Colour palette for treatment arms (used across app and TFL figures)
TRT_COLOURS <- c(
  "Placebo"      = "#4E79A7",
  "Active 10mg"  = "#F28E2B",
  "Active 20mg"  = "#59A14F"
)
