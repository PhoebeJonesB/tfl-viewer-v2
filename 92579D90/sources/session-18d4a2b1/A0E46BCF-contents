# =============================================================================
# server.R
# Clinical TFL Viewer v2 – Server Logic
# =============================================================================
# Reactive graph summary:
#
#  [btn_refresh_tfls] ──► rv$tfl_list, rv$registry
#  [sel_tfl]          ──► current_tfl()
#  [sel_dataset]      ──► raw_dataset()
#  [pop_flag inputs]  ──► pop_filtered()
#  [explorer DT col]  ──► col_filtered()  (via DT column search)
#  pop_filtered()     ──► explorer_table, inv_* outputs, live filter code
#  col_filtered()     ──► quick stats, download
#  [btn_rerun_tfl]    ──► rv$rerun_output
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # 0. Reactive values – mutable app state
  # ---------------------------------------------------------------------------

  rv <- reactiveValues(
    tfl_list     = tfl_list,      # from global.R (initial load)
    registry     = tfl_registry,  # from global.R (initial load)
    rerun_output = NULL,          # result of manual "Re-run TFL" button
    rerun_msg    = NULL           # status message for the re-run button
  )

  # Populate the TFL selector dropdown once registry is ready
  observe({
    req(rv$registry)
    choices <- setNames(rv$registry$tfl_key, rv$registry$name)
    # Pre-select the first TFL
    updateSelectInput(session, "sel_tfl",
                      choices  = choices,
                      selected = choices[1])
  })


  # ---------------------------------------------------------------------------
  # 1. REFRESH – re-run all TFL scripts from disk
  # ---------------------------------------------------------------------------

  observeEvent(input$btn_refresh_tfls, {
    showModal(modalDialog(
      title  = "Refreshing TFLs...",
      "Re-running all TFL scripts. This may take a few seconds.",
      footer = NULL,
      easyClose = FALSE
    ))

    withCallingHandlers(
      {
        new_list <- discover_tfls(TFL_DIR, adam)
        rv$tfl_list <- new_list
        rv$registry <- build_registry(new_list)
        # Rebuild dropdown
        choices <- setNames(rv$registry$tfl_key, rv$registry$name)
        updateSelectInput(session, "sel_tfl",
                          choices  = choices,
                          selected = isolate(input$sel_tfl))
      },
      message = function(m) invokeRestart("muffleMessage")
    )

    removeModal()
  })


  # ---------------------------------------------------------------------------
  # 2. REGISTRY TAB
  # ---------------------------------------------------------------------------

  # Summary banner above the registry table
  output$registry_summary_ui <- renderUI({
    req(rv$registry)
    r  <- rv$registry
    ok <- sum(r$status == "\u2705 OK")
    er <- sum(r$status == "\u274C ERROR")

    layout_column_wrap(
      width = 1 / 4,
      value_box(title = "Total TFLs",  value = nrow(r),
                theme = "secondary",   showcase = icon("table-list")),
      value_box(title = "Tables",      value = sum(r$type == "Table"),
                theme = "info",        showcase = icon("table")),
      value_box(title = "Figures",     value = sum(r$type == "Figure"),
                theme = "success",     showcase = icon("chart-bar")),
      value_box(title = "Loaded OK",   value = ok,
                theme = ifelse(er > 0, "danger", "primary"),
                showcase = icon("circle-check"))
    )
  })

  # Registry DT table (single-row selection)
  output$registry_table <- renderDT({
    req(rv$registry)
    disp <- rv$registry[, c("name", "type", "datasets",
                             "description", "status", "run_time_secs")]
    colnames(disp) <- c("TFL Name", "Type", "Datasets Used",
                         "Description", "Status", "Run (s)")
    datatable(
      disp,
      selection = "single",
      rownames  = FALSE,
      options   = list(
        pageLength = 20,
        dom        = "frtip",
        columnDefs = list(list(width = "35%", targets = 0))
      )
    )
  })

  # When a row is selected in the registry, switch to viewer and load that TFL
  observeEvent(input$registry_table_rows_selected, {
    row <- input$registry_table_rows_selected
    req(row, rv$registry)
    key <- rv$registry$tfl_key[row]
    updateSelectInput(session, "sel_tfl", selected = key)
    nav_select("main_nav", "tab_viewer", session = session)
  })


  # ---------------------------------------------------------------------------
  # 3. CURRENT TFL  (reactive – changes with sel_tfl dropdown)
  # ---------------------------------------------------------------------------

  current_tfl <- reactive({
    req(input$sel_tfl, rv$tfl_list)
    rv$tfl_list[[input$sel_tfl]]
  })


  # ---------------------------------------------------------------------------
  # 4. SIDEBAR: TFL INFO CARD
  # ---------------------------------------------------------------------------

  output$tfl_info_card_ui <- renderUI({
    tfl <- current_tfl()
    req(tfl)
    m   <- tfl$metadata
    status_class <- if (tfl$success) "text-success" else "text-danger"

    card(
      class = "mt-2 mb-0",
      card_body(
        class = "py-2 px-3",
        tags$p(class = "fw-semibold mb-1 small", m$name),
        tags$p(class = "text-muted small mb-1", m$description),
        tags$div(
          class = "d-flex gap-2 flex-wrap",
          tags$span(class = "badge bg-secondary",
                    icon(tfl_type_icon(m$type)), " ", m$type),
          lapply(m$datasets, function(ds)
            tags$span(class = "badge bg-light text-dark border", ds)
          )
        ),
        if (!tfl$success)
          tags$p(class = "text-danger small mt-1 mb-0",
                 icon("triangle-exclamation"), " Error: ", tfl$error)
      )
    )
  })


  # ---------------------------------------------------------------------------
  # 5. SIDEBAR: DATASET SELECTOR
  # ---------------------------------------------------------------------------
  # When a TFL uses multiple datasets, let the user pick which one to explore.

  # Which dataset is currently selected for exploration
  sel_dataset_name <- reactiveVal(NULL)

  output$dataset_selector_ui <- renderUI({
    tfl <- current_tfl()
    req(tfl, tfl$source_datasets)
    ds_names <- names(tfl$source_datasets)

    # Initialise (or reset) the selected dataset reactiveVal
    if (is.null(sel_dataset_name()) || !sel_dataset_name() %in% ds_names) {
      sel_dataset_name(ds_names[1])
    }

    if (length(ds_names) == 1L) return(NULL)   # No selector needed for 1 dataset

    tagList(
      tags$label(class = "fw-semibold text-muted small text-uppercase mb-1",
                 "Explore Dataset"),
      div(
        class = "btn-group w-100",
        lapply(ds_names, function(nm) {
          active <- if (!is.null(sel_dataset_name()) && sel_dataset_name() == nm)
            " active" else ""
          actionButton(
            inputId = paste0("btn_ds_", nm),
            label   = toupper(nm),
            class   = paste0("btn btn-sm btn-outline-primary", active),
            width   = "100%"
          )
        })
      )
    )
  })

  # Update sel_dataset_name when a dataset button is clicked
  observe({
    tfl <- current_tfl()
    req(tfl, tfl$source_datasets)
    ds_names <- names(tfl$source_datasets)
    lapply(ds_names, function(nm) {
      observeEvent(input[[paste0("btn_ds_", nm)]], {
        sel_dataset_name(nm)
      }, ignoreInit = TRUE)
    })
  })

  # Reset when TFL changes
  observeEvent(input$sel_tfl, {
    sel_dataset_name(NULL)
    rv$rerun_output <- NULL
    rv$rerun_msg    <- NULL
  })

  # The raw (unfiltered) dataset currently selected for exploration
  raw_dataset <- reactive({
    tfl <- current_tfl()
    req(tfl, tfl$source_datasets)
    nm <- sel_dataset_name()
    req(nm, nm %in% names(tfl$source_datasets))
    tfl$source_datasets[[nm]]
  })


  # ---------------------------------------------------------------------------
  # 6. SIDEBAR: POPULATION FLAGS
  # ---------------------------------------------------------------------------
  # Detect any column ending in "FL" in the selected dataset and render
  # a selectInput per flag.

  pop_flags <- reactive({
    detect_pop_flags(raw_dataset())
  })

  output$pop_flags_ui <- renderUI({
    flags <- pop_flags()
    if (length(flags) == 0L) return(NULL)
    df    <- raw_dataset()

    tagList(
      tags$label(class = "fw-semibold text-muted small text-uppercase mb-1",
                 icon("flag"), " Population Flags"),
      lapply(flags, function(fl) {
        selectInput(
          inputId  = paste0("popfl_", fl),
          label    = fl,
          choices  = safe_unique_vals(df, fl),
          selected = "All",
          width    = "100%"
        )
      })
    )
  })

  # Collect active flag selections (excluding "All")
  active_pop_flags <- reactive({
    flags <- pop_flags()
    if (length(flags) == 0L) return(list())
    vals <- lapply(flags, function(fl) input[[paste0("popfl_", fl)]])
    setNames(vals, flags)
  })

  # Apply population flag filters to the raw dataset
  pop_filtered <- reactive({
    df    <- raw_dataset()
    flags <- active_pop_flags()
    for (fl in names(flags)) {
      val <- flags[[fl]]
      if (!is.null(val) && val != "All") {
        df <- df[!is.na(df[[fl]]) & df[[fl]] == val, ]
      }
    }
    df
  })


  # ---------------------------------------------------------------------------
  # 7. SIDEBAR: QUICK STATS
  # ---------------------------------------------------------------------------

  output$quick_stats_ui <- renderUI({
    df   <- pop_filtered()
    nrow_df <- nrow(df)
    n_flags <- sum(sapply(active_pop_flags(), function(v) !is.null(v) && v != "All"))
    n_subj  <- if ("USUBJID" %in% names(df)) dplyr::n_distinct(df$USUBJID) else NA_integer_

    tagList(
      tags$div(
        class = "d-flex justify-content-between small",
        tags$span(class = "text-muted", "Rows shown"),
        tags$strong(format(nrow_df, big.mark = ","))
      ),
      if (!is.na(n_subj))
        tags$div(
          class = "d-flex justify-content-between small",
          tags$span(class = "text-muted", "Unique subjects"),
          tags$strong(format(n_subj, big.mark = ","))
        ),
      tags$div(
        class = "d-flex justify-content-between small",
        tags$span(class = "text-muted", "Active flag filters"),
        tags$strong(n_flags)
      )
    )
  })


  # ---------------------------------------------------------------------------
  # 8. TFL VIEWER: OUTPUT TAB
  # ---------------------------------------------------------------------------

  output$tfl_header_ui <- renderUI({
    tfl <- current_tfl()
    req(tfl)
    m   <- tfl$metadata
    # If a re-run result is available, note that in the header
    rerun_badge <- if (!is.null(rv$rerun_output) && rv$rerun_output$success)
      tags$span(class = "badge bg-warning text-dark ms-2",
                icon("arrows-rotate"), " Re-run output")
    else NULL

    tagList(
      tags$h5(class = "mb-0", m$name, rerun_badge),
      tags$p(class = "text-muted small mb-0", m$description)
    )
  })

  # Decide what to render (DT table vs plot) and build the appropriate output
  output$tfl_output_ui <- renderUI({
    tfl <- current_tfl()
    req(tfl)

    if (!tfl$success) {
      return(div(
        class = "alert alert-danger mt-2",
        icon("triangle-exclamation"), " ",
        strong("TFL script failed: "), tfl$error
      ))
    }

    # Prefer re-run output if available and successful
    out <- if (!is.null(rv$rerun_output) && rv$rerun_output$success)
      rv$rerun_output$tfl_output
    else
      tfl$tfl_output

    if (inherits(out, "ggplot")) {
      plotOutput("tfl_plot_output", height = "520px")
    } else if (is.data.frame(out)) {
      DTOutput("tfl_datatable_output")
    } else {
      div(class = "alert alert-warning",
          "Unrecognised output type: ", class(out)[1])
    }
  })

  # Render the plot (only evaluated when tfl_output_ui requests it)
  output$tfl_plot_output <- renderPlot({
    tfl <- current_tfl()
    req(tfl, tfl$success)
    out <- if (!is.null(rv$rerun_output) && rv$rerun_output$success)
      rv$rerun_output$tfl_output else tfl$tfl_output
    req(inherits(out, "ggplot"))
    print(out)
  })

  # Render the DT table
  output$tfl_datatable_output <- renderDT({
    tfl <- current_tfl()
    req(tfl, tfl$success)
    out <- if (!is.null(rv$rerun_output) && rv$rerun_output$success)
      rv$rerun_output$tfl_output else tfl$tfl_output
    req(is.data.frame(out))
    datatable(
      out,
      rownames  = FALSE,
      extensions = c("Buttons", "Scroller"),
      options   = list(
        dom        = "Bfrtip",
        buttons    = list("excel", "csv", "pdf"),
        scrollX    = TRUE,
        scrollY    = "420px",
        scroller   = TRUE,
        pageLength = 25
      ),
      filter = "top"
    )
  })


  # ---------------------------------------------------------------------------
  # 9. DATASET EXPLORER: DATA SUB-TAB
  # ---------------------------------------------------------------------------

  output$explorer_dataset_info_ui <- renderUI({
    df <- pop_filtered()
    nm <- sel_dataset_name()
    tags$div(
      class = "d-flex gap-3 align-items-center small text-muted mb-1",
      tags$strong(toupper(nm %||% "—")),
      tags$span(format(nrow(df), big.mark = ","), " rows"),
      tags$span(ncol(df), " columns"),
      if ("USUBJID" %in% names(df))
        tags$span(dplyr::n_distinct(df$USUBJID), " subjects")
    )
  })

  output$explorer_table <- renderDT({
    df <- pop_filtered()
    datatable(
      df,
      rownames   = FALSE,
      extensions = c("Buttons", "Scroller"),
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("excel", "csv"),
        scrollX    = TRUE,
        scrollY    = "450px",
        scroller   = TRUE,
        pageLength = 25
      ),
      filter = "top"
    )
  })


  # ---------------------------------------------------------------------------
  # 10. INVESTIGATE: UNIQUE VALUES
  # ---------------------------------------------------------------------------

  # Populate column selector with all columns from the filtered dataset
  observe({
    df <- pop_filtered()
    updateSelectInput(session, "inv_col",
                      choices  = names(df),
                      selected = names(df)[1])
  })

  # Count table
  output$inv_unique_table <- renderDT({
    req(input$inv_col)
    df  <- pop_filtered()
    col <- input$inv_col
    req(col %in% names(df))

    cnt <- as.data.frame(table(df[[col]], useNA = "ifany"),
                         stringsAsFactors = FALSE)
    colnames(cnt) <- c("Value", "Count")
    cnt$`%` <- round(100 * cnt$Count / sum(cnt$Count), 1)
    cnt      <- cnt[order(-cnt$Count), ]

    datatable(
      cnt,
      rownames  = FALSE,
      options   = list(dom = "tip", pageLength = 10, scrollY = "200px",
                       scroller = TRUE)
    )
  })

  # Bar chart for the selected column
  output$inv_unique_plot <- renderPlot({
    req(input$inv_col)
    df  <- pop_filtered()
    col <- input$inv_col
    req(col %in% names(df), nrow(df) > 0)

    # Only plot if <= 30 unique values (avoids illegible charts)
    uniq <- unique(na.omit(df[[col]]))
    if (length(uniq) > 30) {
      plot.new()
      text(0.5, 0.5, paste("Too many unique values\nto display (", length(uniq), ")"),
           cex = 1.1, col = "grey50")
      return(invisible(NULL))
    }

    ggplot(df, aes(y = forcats::fct_rev(forcats::fct_infreq(.data[[col]])))) +
      geom_bar(fill = "#4E79A7") +
      labs(x = "Count", y = col) +
      theme_bw(base_size = 11) +
      theme(panel.grid.major.y = element_blank())
  })


  # ---------------------------------------------------------------------------
  # 11. INVESTIGATE: PATIENT LISTING
  # ---------------------------------------------------------------------------

  # Populate USUBJID selector
  observe({
    df <- pop_filtered()
    choices <- if ("USUBJID" %in% names(df))
      sort(unique(df$USUBJID))
    else
      character(0)
    updateSelectInput(session, "inv_usubjid",
                      choices  = choices,
                      selected = choices[1])
  })

  output$inv_patient_info_ui <- renderUI({
    req(input$inv_usubjid)
    # Show how many rows this subject has in the current dataset
    df   <- pop_filtered()
    subj <- input$inv_usubjid
    n    <- sum(df$USUBJID == subj, na.rm = TRUE)
    tags$p(class = "text-muted small mb-1",
           n, " row(s) for subject ", tags$strong(subj))
  })

  output$inv_patient_table <- renderDT({
    req(input$inv_usubjid)
    df   <- pop_filtered()
    subj <- input$inv_usubjid
    req("USUBJID" %in% names(df))
    patient_df <- df[df$USUBJID == subj, ]
    datatable(
      patient_df,
      rownames = FALSE,
      options  = list(dom = "tip", pageLength = 15,
                       scrollX = TRUE, scrollY = "220px", scroller = TRUE)
    )
  })


  # ---------------------------------------------------------------------------
  # 12. INVESTIGATE: EXPLORATORY PLOT BUILDER
  # ---------------------------------------------------------------------------

  # Update axis selectors when the dataset changes
  observe({
    df       <- pop_filtered()
    all_cols <- names(df)
    num_cols <- names(df)[sapply(df, is.numeric)]

    updateSelectInput(session, "inv_plot_x",     choices = all_cols, selected = all_cols[1])
    updateSelectInput(session, "inv_plot_y",     choices = c("—" = "", num_cols), selected = "")
    updateSelectInput(session, "inv_plot_color", choices = c("None" = "", all_cols), selected = "")
  })

  output$inv_custom_plot <- renderPlot({
    req(input$inv_plot_x)
    df    <- pop_filtered()
    req(nrow(df) > 0)
    x     <- input$inv_plot_x
    y     <- input$inv_plot_y
    col   <- input$inv_plot_color
    ptype <- input$inv_plot_type

    req(x %in% names(df))

    # Limit categorical axis to ≤50 levels to keep plots legible
    if (!is.numeric(df[[x]]) && dplyr::n_distinct(df[[x]]) > 50) {
      plot.new()
      text(0.5, 0.5, "X-axis has > 50 unique values.\nChoose a column with fewer categories.",
           cex = 1.1, col = "grey50")
      return(invisible(NULL))
    }

    aes_base <- if (!is.null(col) && col != "" && col %in% names(df)) {
      ggplot2::aes(x = .data[[x]], colour = .data[[col]], fill = .data[[col]])
    } else {
      ggplot2::aes(x = .data[[x]])
    }

    p <- ggplot2::ggplot(df, aes_base)

    p <- switch(ptype,
      "Bar" = {
        p + ggplot2::geom_bar(position = "dodge")
      },
      "Histogram" = {
        if (!is.numeric(df[[x]])) {
          return(plot.new())
        }
        p + ggplot2::geom_histogram(bins = 25, colour = "white", alpha = 0.8)
      },
      "Box" = {
        req(!is.null(y) && y != "" && y %in% names(df) && is.numeric(df[[y]]))
        ggplot2::ggplot(
          df,
          if (!is.null(col) && col != "" && col %in% names(df))
            ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[col]])
          else
            ggplot2::aes(x = .data[[x]], y = .data[[y]])
        ) +
          ggplot2::geom_boxplot(outlier.size = 1, alpha = 0.8) +
          ggplot2::labs(y = y)
      },
      "Scatter" = {
        req(!is.null(y) && y != "" && y %in% names(df) && is.numeric(df[[y]]))
        ggplot2::ggplot(
          df,
          if (!is.null(col) && col != "" && col %in% names(df))
            ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = .data[[col]])
          else
            ggplot2::aes(x = .data[[x]], y = .data[[y]])
        ) +
          ggplot2::geom_point(alpha = 0.5, size = 1.5) +
          ggplot2::labs(y = y)
      },
      p   # fallback
    )

    p + ggplot2::labs(x = x) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position  = "bottom",
        axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1),
        panel.grid.minor = ggplot2::element_blank()
      )
  })


  # ---------------------------------------------------------------------------
  # 13. CODE TABS
  # ---------------------------------------------------------------------------

  output$tfl_script_code <- renderText({
    tfl <- current_tfl()
    req(tfl)
    paste(tfl$script_lines, collapse = "\n")
  })

  output$filter_code_static <- renderText({
    tfl <- current_tfl()
    req(tfl, tfl$filter_code)
    tfl$filter_code
  })

  output$filter_code_live <- renderText({
    nm     <- sel_dataset_name()
    flags  <- active_pop_flags()
    build_filter_code(
      dataset_name = nm %||% "dataset",
      pop_filters  = flags
    )
  })


  # ---------------------------------------------------------------------------
  # 14. RE-RUN TFL WITH CURRENT FILTERS
  # ---------------------------------------------------------------------------

  observeEvent(input$btn_rerun_tfl, {
    tfl <- isolate(current_tfl())
    req(tfl, tfl$success)

    rv$rerun_msg <- "running"

    # Build filtered versions of every source dataset for this TFL
    filtered_ds <- lapply(names(tfl$source_datasets), function(nm) {
      ds    <- tfl$source_datasets[[nm]]
      flags <- isolate(active_pop_flags())
      for (fl in names(flags)) {
        val <- flags[[fl]]
        if (!is.null(val) && val != "All" && fl %in% names(ds)) {
          ds <- ds[!is.na(ds[[fl]]) & ds[[fl]] == val, ]
        }
      }
      ds
    })
    names(filtered_ds) <- names(tfl$source_datasets)

    # Find the script path on disk
    script_path <- file.path(TFL_DIR, paste0(isolate(input$sel_tfl), ".R"))

    if (!file.exists(script_path)) {
      rv$rerun_msg <- paste("Script not found:", script_path)
      return()
    }

    result <- tryCatch(
      rerun_tfl_with_population(script_path, filtered_ds, adam),
      error = function(e) list(success = FALSE, error = conditionMessage(e))
    )

    rv$rerun_output <- result
    rv$rerun_msg    <- if (result$success) "success" else result$error

    # Switch to Output tab so user can see the result
    nav_select("viewer_tabs", "vtab_output", session = session)
  })

  output$rerun_status_ui <- renderUI({
    msg <- rv$rerun_msg
    if (is.null(msg)) return(NULL)
    if (msg == "running")
      tags$span(class = "text-warning small",
                icon("spinner"), " Running...")
    else if (msg == "success")
      tags$span(class = "text-success small",
                icon("circle-check"), " Re-run successful. Output tab updated.")
    else
      tags$span(class = "text-danger small",
                icon("triangle-exclamation"), " Error: ", msg)
  })


  # ---------------------------------------------------------------------------
  # 15. DOWNLOAD: FILTERED DATA
  # ---------------------------------------------------------------------------

  output$dl_filtered_data <- downloadHandler(
    filename = function() {
      nm   <- sel_dataset_name() %||% "data"
      date <- format(Sys.Date(), "%Y-%m-%d")
      paste0(nm, "_filtered_", date, ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(pop_filtered(), file)
    }
  )


  # ---------------------------------------------------------------------------
  # 16. ABOUT TAB
  # ---------------------------------------------------------------------------

  output$about_study_id <- renderText(STUDY_ID)

  output$about_dataset_summary <- renderTable({
    data.frame(
      Dataset     = c("ADSL", "ADAE", "ADLB", "ADTTE"),
      Description = c(
        "Subject Level Analysis",
        "Adverse Events Analysis",
        "Laboratory Data Analysis",
        "Time-to-Event Analysis"
      ),
      Rows        = c(nrow(adsl), nrow(adae), nrow(adlb), nrow(adtte)),
      Columns     = c(ncol(adsl), ncol(adae), ncol(adlb), ncol(adtte)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

}
