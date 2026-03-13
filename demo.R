# =============================================================================
# demo.R
# Clinical TFL Viewer v2 – Standalone Demo
# =============================================================================
# Run this script from the project root to explore the app's core machinery
# without launching the Shiny UI.  It demonstrates:
#
#   1. Synthetic ADaM data generation
#   2. TFL script discovery and execution
#   3. Inspecting the TFL registry
#   4. Viewing a table output
#   5. Viewing a figure output
#   6. Re-running a TFL with a custom population filter
#   7. Live filter code generation
#
# Prerequisites: all packages listed in global.R must be installed.
# Run from the project root (same directory as global.R):
#   source("demo.R")
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(survival)
library(scales)
library(forcats)

# Source helper modules
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

cat("\n====================================================\n")
cat("  Clinical TFL Viewer v2 – Demo\n")
cat("====================================================\n\n")


# -----------------------------------------------------------------------------
# 1. Generate synthetic ADaM datasets
# -----------------------------------------------------------------------------

cat("-- Step 1: Generating synthetic ADaM datasets (n=200 subjects) ...\n")
adam <- generate_adam_data(n_subjects = 200, seed = 4729)

adsl  <- adam$adsl
adae  <- adam$adae
adlb  <- adam$adlb
adtte <- adam$adtte

cat(sprintf(
  "   ADSL : %d subjects x %d columns\n",
  nrow(adsl), ncol(adsl)
))
cat(sprintf(
  "   ADAE : %d rows    x %d columns\n",
  nrow(adae), ncol(adae)
))
cat(sprintf(
  "   ADLB : %d rows    x %d columns\n",
  nrow(adlb), ncol(adlb)
))
cat(sprintf(
  "   ADTTE: %d rows    x %d columns\n\n",
  nrow(adtte), ncol(adtte)
))

# Quick look at treatment arm distribution
cat("-- Treatment arm breakdown (ADSL):\n")
print(adsl |> count(TRT01A))
cat("\n")


# -----------------------------------------------------------------------------
# 2. Discover and run all TFL scripts
# -----------------------------------------------------------------------------

cat("-- Step 2: Discovering TFL scripts in tfls/ ...\n")
tfl_list <- discover_tfls("tfls", adam)
cat("\n")


# -----------------------------------------------------------------------------
# 3. Inspect the TFL registry
# -----------------------------------------------------------------------------

cat("-- Step 3: TFL Registry\n")
registry <- build_registry(tfl_list)
print(registry[, c("name", "type", "datasets", "status", "run_time_secs")])
cat("\n")


# -----------------------------------------------------------------------------
# 4. View a table output – Subject Disposition (t14_1_1)
# -----------------------------------------------------------------------------

cat("-- Step 4: Table output – Subject Disposition (tfl_t14_1_1)\n")
tfl_disp <- tfl_list[["tfl_t14_1_1"]]

if (tfl_disp$success) {
  cat("   Script ran OK in", round(tfl_disp$run_time, 3), "seconds\n")
  cat("   Output preview:\n\n")
  print(head(tfl_disp$tfl_output, 10))
} else {
  cat("   ERROR:", tfl_disp$error, "\n")
}
cat("\n")


# -----------------------------------------------------------------------------
# 5. View a figure output – Kaplan-Meier (f14_2_1)
# -----------------------------------------------------------------------------

cat("-- Step 5: Figure output – Kaplan-Meier survival curve (tfl_f14_2_1)\n")
tfl_km <- tfl_list[["tfl_f14_2_1"]]

if (tfl_km$success && inherits(tfl_km$tfl_output, "ggplot")) {
  cat("   ggplot object ready. Printing to active device ...\n")
  print(tfl_km$tfl_output)
} else {
  cat("   Skipped (script failed or output is not a ggplot)\n")
}
cat("\n")


# -----------------------------------------------------------------------------
# 6. Re-run a TFL with a custom population filter
#    Here: re-run the AE table (t14_3_1) restricting to Safety population only
# -----------------------------------------------------------------------------

cat("-- Step 6: Re-run AE table with SAFFL == 'Y' filter\n")

tfl_ae <- tfl_list[["tfl_t14_3_1"]]

if (tfl_ae$success) {
  # Build filtered versions of the source datasets
  filtered_ds <- lapply(tfl_ae$source_datasets, function(ds) {
    if ("SAFFL" %in% names(ds)) {
      ds[!is.na(ds$SAFFL) & ds$SAFFL == "Y", ]
    } else {
      ds
    }
  })

  script_path <- file.path("tfls", "tfl_t14_3_1.R")
  rerun_result <- rerun_tfl_with_population(script_path, filtered_ds, adam)

  if (rerun_result$success) {
    cat("   Re-run successful. Preview of re-run output:\n\n")
    print(head(rerun_result$tfl_output, 10))
  } else {
    cat("   Re-run failed:", rerun_result$error, "\n")
  }
} else {
  cat("   Skipped (original TFL failed)\n")
}
cat("\n")


# -----------------------------------------------------------------------------
# 7. Live filter code generation
#    Shows the dplyr code snippet the Shiny app would display in the
#    Filter Code tab when a user sets SAFFL = "Y" and ITTFL = "Y".
# -----------------------------------------------------------------------------

cat("-- Step 7: Live filter code snippet\n")
cat("   (Equivalent to what the app shows in the Filter Code tab)\n\n")

live_code <- build_filter_code(
  dataset_name = "adsl",
  pop_filters  = list(SAFFL = "Y", ITTFL = "Y")
)
cat(live_code, "\n\n")


# -----------------------------------------------------------------------------
# Done
# -----------------------------------------------------------------------------

cat("====================================================\n")
cat("  Demo complete.\n")
cat("  To launch the full Shiny app, run:\n")
cat("    shiny::runApp()\n")
cat("====================================================\n")
