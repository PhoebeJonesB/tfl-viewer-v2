# =============================================================================
# R/runner.R
# TFL Script Discovery and Execution Engine
# =============================================================================
# Purpose : Scan the tfls/ directory for TFL scripts, execute each one in an
#           isolated child environment that has access to the shared ADaM data,
#           and return a structured list of results including metadata, source
#           datasets, display-ready code strings, and the final TFL output.
#
# Convention: Scripts must be named  tfl_*.R  (the _template.R is excluded).
#             Each script must define the four contract objects:
#               tfl_metadata    - named list (id, name, type, datasets, description)
#               source_datasets - named list of data frames used as inputs
#               filter_code     - character string of dataset preparation code
#               tfl_output      - data.frame (Table/Listing) or ggplot (Figure)
#
# Usage:
#   adam      <- generate_adam_data()
#   tfl_list  <- discover_tfls("tfls/", adam)
#   registry  <- build_registry(tfl_list)
# =============================================================================


# -----------------------------------------------------------------------------
# run_tfl_script()
# Execute a single TFL script and capture its outputs safely.
# -----------------------------------------------------------------------------
#' Run one TFL script in an isolated environment
#'
#' @param script_path  Full path to the .R script file.
#' @param adam_data    Named list of ADaM data frames (adsl, adae, adlb, adtte).
#'
#' @return A named list with:
#'   \item{success}       TRUE if script ran without error.
#'   \item{metadata}      tfl_metadata list from the script.
#'   \item{source_datasets} Named list of raw input data frames.
#'   \item{filter_code}   Character string shown in the Filter Code tab.
#'   \item{tfl_output}    data.frame or ggplot object for display.
#'   \item{script_lines}  Raw source lines for the TFL Code tab.
#'   \item{error}         Error message string (NULL on success).
#'   \item{run_time_secs} Elapsed execution time in seconds.
run_tfl_script <- function(script_path, adam_data) {

  # Child environment inherits from globalenv so all loaded packages are
  # available, but any new assignments stay inside this isolated env.
  env <- new.env(parent = globalenv())

  # Inject the shared ADaM datasets so scripts can reference adsl, adae, etc.
  list2env(adam_data, envir = env)

  # Read source lines before execution (for the Code tab display)
  script_lines <- tryCatch(
    readLines(script_path, warn = FALSE),
    error = function(e) character(0)
  )

  # Time and execute the script; catch any errors gracefully
  t_start <- proc.time()["elapsed"]

  result <- tryCatch({
    source(script_path, local = env, echo = FALSE, verbose = FALSE)

    list(
      success        = TRUE,
      metadata       = env$tfl_metadata,
      source_datasets = env$source_datasets,
      filter_code    = env$filter_code,
      tfl_output     = env$tfl_output,
      script_lines   = script_lines,
      error          = NULL,
      run_time_secs  = unname(round(proc.time()["elapsed"] - t_start, 2))
    )
  }, error = function(e) {
    # Build a minimal metadata stub so the registry can still show the entry
    list(
      success        = FALSE,
      metadata       = list(
        id          = sub("\\.R$", "", basename(script_path)),
        name        = paste("ERROR \u2013", basename(script_path)),
        type        = "Error",
        datasets    = character(0),
        description = conditionMessage(e)
      ),
      source_datasets = NULL,
      filter_code    = NULL,
      tfl_output     = NULL,
      script_lines   = script_lines,
      error          = conditionMessage(e),
      run_time_secs  = unname(round(proc.time()["elapsed"] - t_start, 2))
    )
  })

  result
}


# -----------------------------------------------------------------------------
# discover_tfls()
# Scan a directory and run all matching TFL scripts.
# -----------------------------------------------------------------------------
#' Discover and run all TFL scripts in a folder
#'
#' @param tfl_dir   Path to the folder containing TFL scripts.
#' @param adam_data Named list of ADaM data frames passed to each script.
#'
#' @return A named list; each element is the result of run_tfl_script().
#'         Names are the script basenames without the .R extension.
discover_tfls <- function(tfl_dir, adam_data) {

  # Find scripts matching tfl_*.R (excludes _template.R and similar)
  scripts <- list.files(
    tfl_dir,
    pattern    = "^tfl_.*\\.R$",
    full.names = TRUE
  )

  if (length(scripts) == 0L) {
    warning("No TFL scripts found in: ", tfl_dir)
    return(list())
  }

  # Sort alphabetically so tables appear before figures in the registry
  scripts <- sort(scripts)

  message(sprintf("[runner] Discovering %d TFL script(s) in '%s'", length(scripts), tfl_dir))

  results <- lapply(scripts, function(s) {
    message(sprintf("[runner]   Running: %s", basename(s)))
    run_tfl_script(s, adam_data)
  })

  names(results) <- sub("\\.R$", "", basename(scripts))
  results
}


# -----------------------------------------------------------------------------
# build_registry()
# Flatten the TFL list into a display-ready data frame for the Registry tab.
# -----------------------------------------------------------------------------
#' Build a flat registry data frame from the TFL results list
#'
#' @param tfl_list Named list returned by discover_tfls().
#'
#' @return A data frame with one row per TFL and columns:
#'   tfl_key, id, name, type, datasets, description, status, run_time_secs
build_registry <- function(tfl_list) {

  if (length(tfl_list) == 0L) {
    return(data.frame(
      tfl_key      = character(0),
      id           = character(0),
      name         = character(0),
      type         = character(0),
      datasets     = character(0),
      description  = character(0),
      status       = character(0),
      run_time_secs = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(names(tfl_list), function(key) {
    tfl <- tfl_list[[key]]
    m   <- tfl$metadata
    data.frame(
      tfl_key       = key,
      id            = m$id,
      name          = m$name,
      type          = m$type,
      datasets      = paste(m$datasets, collapse = ", "),
      description   = m$description,
      status        = ifelse(tfl$success, "\u2705 OK", "\u274C ERROR"),
      run_time_secs = tfl$run_time_secs,
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, rows)
  row.names(result_df) <- NULL
  result_df
}


# -----------------------------------------------------------------------------
# rerun_tfl_with_population()
# Re-execute a TFL script after replacing source datasets with user-filtered
# versions. Used by the "Re-run with Current Filters" button.
# -----------------------------------------------------------------------------
#' Re-run a TFL script against user-filtered datasets
#'
#' @param script_path    Full path to the TFL .R script.
#' @param filtered_datasets Named list of already-filtered data frames.
#'        Names must match the dataset keys the script expects (adsl, adae, …).
#' @param adam_data      Full ADaM list (fallback for datasets not being filtered).
#'
#' @return Same structure as run_tfl_script().
rerun_tfl_with_population <- function(script_path, filtered_datasets, adam_data) {

  # Merge: filtered datasets override the full datasets where supplied
  combined <- adam_data
  for (nm in names(filtered_datasets)) {
    combined[[nm]] <- filtered_datasets[[nm]]
  }

  run_tfl_script(script_path, combined)
}
