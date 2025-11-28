# ================================================================================
# INSELE CAPITAL PARTNERS - TREASURY HOLDINGS MODULE
# Shiny Module for SA Government Bond Institutional Ownership Analysis
# ================================================================================

# ================================================================================
# MODULE UI FUNCTION
# ================================================================================

#' Treasury Holdings Module UI
#'
#' Creates the UI components for the Treasury Holdings tab
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
#' @export
treasury_holdings_ui <- function(id) {
    ns <- NS(id)

    # Define Insele palette for styling
    insele_primary <- "#1B3A6B"
    insele_accent <- "#FFD700"

    fluidPage(
        # Custom CSS for this module
        tags$head(
            tags$style(HTML(sprintf("
                .treasury-status-box {
                    background: linear-gradient(135deg, %s 0%%, #2A4D8C 100%%);
                    color: white;
                    border-radius: 10px;
                    padding: 15px;
                    margin-bottom: 20px;
                }
                .treasury-status-box h4 {
                    color: %s;
                    margin-top: 0;
                }
                .treasury-metric {
                    display: inline-block;
                    margin-right: 20px;
                    padding: 8px 12px;
                    background: rgba(255,255,255,0.1);
                    border-radius: 6px;
                }
                .treasury-metric-value {
                    font-size: 18px;
                    font-weight: bold;
                }
                .treasury-metric-label {
                    font-size: 11px;
                    opacity: 0.8;
                }
                .filter-panel {
                    background: #f8f9fa;
                    border-radius: 8px;
                    padding: 15px;
                    margin-bottom: 15px;
                }
                .btn-treasury-update {
                    background: %s;
                    border: none;
                    color: %s;
                    font-weight: 600;
                }
                .btn-treasury-update:hover {
                    background: #e6c200;
                    color: %s;
                }
            ", insele_primary, insele_accent, insele_accent, insele_primary, insele_primary)))
        ),

        # Status and Update Section
        fluidRow(
            column(12,
                   div(class = "treasury-status-box",
                       fluidRow(
                           column(8,
                                  h4("Treasury Holdings Data"),
                                  div(
                                      div(class = "treasury-metric",
                                          div(class = "treasury-metric-value", textOutput(ns("status_data_date"), inline = TRUE)),
                                          div(class = "treasury-metric-label", "Latest Data")
                                      ),
                                      div(class = "treasury-metric",
                                          div(class = "treasury-metric-value", textOutput(ns("status_file_count"), inline = TRUE)),
                                          div(class = "treasury-metric-label", "Monthly Files")
                                      ),
                                      div(class = "treasury-metric",
                                          div(class = "treasury-metric-value", textOutput(ns("status_last_update"), inline = TRUE)),
                                          div(class = "treasury-metric-label", "Last Updated")
                                      )
                                  )
                           ),
                           column(4,
                                  div(style = "text-align: right; padding-top: 10px;",
                                      actionButton(
                                          ns("btn_check_updates"),
                                          "Check for Updates",
                                          icon = icon("sync"),
                                          class = "btn-treasury-update"
                                      ),
                                      br(), br(),
                                      actionButton(
                                          ns("btn_reprocess_data"),
                                          "Reprocess Data",
                                          icon = icon("cogs"),
                                          class = "btn btn-sm btn-outline-light"
                                      )
                                  )
                           )
                       )
                   )
            )
        ),

        # Main Content Area with Tabs
        fluidRow(
            # Left Column - Filters
            column(3,
                   div(class = "filter-panel",
                       h5("Data Filters", style = sprintf("color: %s; margin-top: 0;", insele_primary)),

                       # Date Range Selector
                       dateRangeInput(
                           ns("date_range"),
                           "Date Range:",
                           start = Sys.Date() - 365 * 5,  # 5 years ago
                           end = Sys.Date(),
                           format = "M yyyy",
                           startview = "year"
                       ),

                       # Quick date presets
                       fluidRow(
                           column(4, actionButton(ns("btn_1y"), "1Y", class = "btn-sm btn-block")),
                           column(4, actionButton(ns("btn_3y"), "3Y", class = "btn-sm btn-block")),
                           column(4, actionButton(ns("btn_all"), "All", class = "btn-sm btn-block"))
                       ),

                       hr(),

                       # Bond Type Filter
                       selectInput(
                           ns("bond_type_filter"),
                           "Bond Type:",
                           choices = c(
                               "All Bond Types" = "all",
                               "Fixed Rate" = "Fixed Rate",
                               "Inflation Linked (ILB)" = "ILB",
                               "Floating Rate (FRN)" = "FRN",
                               "Sukuk" = "Sukuk"
                           ),
                           selected = "Fixed Rate"
                       ),

                       # Sector Multi-Select
                       selectizeInput(
                           ns("sector_filter"),
                           "Sectors:",
                           choices = NULL,  # Populated dynamically
                           selected = NULL,
                           multiple = TRUE,
                           options = list(
                               placeholder = "Select sectors...",
                               plugins = list("remove_button")
                           )
                       ),
                       actionLink(ns("select_all_sectors"), "Select All"),
                       span(" | "),
                       actionLink(ns("clear_sectors"), "Clear All"),

                       hr(),

                       # Change Period Selector
                       selectInput(
                           ns("change_period"),
                           "Change Period:",
                           choices = c(
                               "1 Month" = 1,
                               "3 Months" = 3,
                               "6 Months" = 6,
                               "12 Months" = 12
                           ),
                           selected = 3
                       )
                   ),

                   # Summary Statistics Box
                   div(class = "filter-panel",
                       h5("Summary Statistics", style = sprintf("color: %s; margin-top: 0;", insele_primary)),
                       uiOutput(ns("summary_stats"))
                   )
            ),

            # Right Column - Charts and Tables
            column(9,
                   tabBox(
                       id = ns("main_tabs"),
                       width = 12,

                       # Tab 1: Time Series View
                       tabPanel(
                           title = "Time Series",
                           icon = icon("chart-area"),
                           value = "time_series",

                           fluidRow(
                               column(12,
                                      box(
                                          title = "Aggregate Holdings Over Time",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 12,
                                          collapsible = TRUE,
                                          withSpinner(
                                              plotOutput(ns("holdings_area_chart"), height = "450px"),
                                              type = 4,
                                              color = insele_primary
                                          ),
                                          downloadButton(ns("download_area_chart"), "Download Chart", class = "btn-sm btn-primary")
                                      )
                               )
                           ),

                           fluidRow(
                               column(12,
                                      box(
                                          title = "Single Sector Trend",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 12,
                                          collapsible = TRUE,
                                          collapsed = TRUE,
                                          fluidRow(
                                              column(4,
                                                     selectInput(
                                                         ns("single_sector_select"),
                                                         "Select Sector:",
                                                         choices = NULL
                                                     )
                                              )
                                          ),
                                          withSpinner(
                                              plotOutput(ns("sector_trend_chart"), height = "350px"),
                                              type = 4,
                                              color = insele_primary
                                          ),
                                          downloadButton(ns("download_trend_chart"), "Download Chart", class = "btn-sm btn-primary")
                                      )
                               )
                           )
                       ),

                       # Tab 2: Current Snapshot
                       tabPanel(
                           title = "Current Snapshot",
                           icon = icon("chart-bar"),
                           value = "snapshot",

                           fluidRow(
                               column(12,
                                      box(
                                          title = "Bond Holdings by Sector",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 12,
                                          fluidRow(
                                              column(3,
                                                     selectInput(
                                                         ns("snapshot_bond_type"),
                                                         "Bond Type:",
                                                         choices = c("Fixed Rate", "ILB", "FRN", "Sukuk"),
                                                         selected = "Fixed Rate"
                                                     )
                                              ),
                                              column(3,
                                                     selectInput(
                                                         ns("snapshot_date"),
                                                         "Data Date:",
                                                         choices = NULL
                                                     )
                                              )
                                          ),
                                          withSpinner(
                                              plotOutput(ns("bond_holdings_bar_chart"), height = "600px"),
                                              type = 4,
                                              color = insele_primary
                                          ),
                                          downloadButton(ns("download_holdings_chart"), "Download Chart", class = "btn-sm btn-primary")
                                      )
                               )
                           )
                       ),

                       # Tab 3: Changes View
                       tabPanel(
                           title = "Changes",
                           icon = icon("exchange-alt"),
                           value = "changes",

                           fluidRow(
                               column(6,
                                      box(
                                          title = "Aggregate Ownership Changes",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 12,
                                          withSpinner(
                                              plotOutput(ns("ownership_change_chart"), height = "400px"),
                                              type = 4,
                                              color = insele_primary
                                          ),
                                          downloadButton(ns("download_change_chart"), "Download Chart", class = "btn-sm btn-primary")
                                      )
                               ),
                               column(6,
                                      box(
                                          title = "Bond-Level Changes",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 12,
                                          fluidRow(
                                              column(6,
                                                     selectInput(
                                                         ns("change_bond_type"),
                                                         "Bond Type:",
                                                         choices = c("Fixed Rate", "ILB", "FRN", "Sukuk"),
                                                         selected = "Fixed Rate"
                                                     )
                                              ),
                                              column(6,
                                                     selectInput(
                                                         ns("change_period_select"),
                                                         "Period:",
                                                         choices = c("1 Month" = 1, "3 Months" = 3, "6 Months" = 6, "12 Months" = 12),
                                                         selected = 3
                                                     )
                                              )
                                          ),
                                          withSpinner(
                                              plotOutput(ns("diverging_change_chart"), height = "400px"),
                                              type = 4,
                                              color = insele_primary
                                          ),
                                          downloadButton(ns("download_diverging_chart"), "Download Chart", class = "btn-sm btn-primary")
                                      )
                               )
                           )
                       ),

                       # Tab 4: Data Table
                       tabPanel(
                           title = "Data Table",
                           icon = icon("table"),
                           value = "data_table",

                           fluidRow(
                               column(12,
                                      box(
                                          title = "Holdings Data",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 12,
                                          fluidRow(
                                              column(3,
                                                     selectInput(
                                                         ns("table_view"),
                                                         "View:",
                                                         choices = c(
                                                             "Aggregate Holdings" = "aggregate",
                                                             "Bond-Level Holdings" = "bonds"
                                                         )
                                                     )
                                              ),
                                              column(3,
                                                     conditionalPanel(
                                                         condition = sprintf("input['%s'] == 'bonds'", ns("table_view")),
                                                         selectInput(
                                                             ns("table_bond_type"),
                                                             "Bond Type:",
                                                             choices = c("Fixed Rate", "ILB", "FRN", "Sukuk"),
                                                             selected = "Fixed Rate"
                                                         )
                                                     )
                                              ),
                                              column(6,
                                                     div(style = "text-align: right; padding-top: 25px;",
                                                         downloadButton(ns("download_table_csv"), "Export CSV", class = "btn-sm btn-success"),
                                                         downloadButton(ns("download_table_excel"), "Export Excel", class = "btn-sm btn-primary")
                                                     )
                                              )
                                          ),
                                          DT::dataTableOutput(ns("holdings_table"))
                                      )
                               )
                           )
                       )
                   )
            )
        )
    )
}

# ================================================================================
# MODULE SERVER FUNCTION
# ================================================================================

#' Treasury Holdings Module Server
#'
#' Server logic for the Treasury Holdings tab
#'
#' @param id Module namespace ID
#' @return None (modifies reactive values)
#' @export
treasury_holdings_server <- function(id) {
    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        # ==================================================================
        # REACTIVE VALUES
        # ==================================================================

        treasury_data <- reactiveValues(
            holdings_ts = NULL,           # holdings_historical_long
            bond_holdings = NULL,         # all_bonds_pct_long
            bond_values = NULL,           # all_bonds_values_long
            last_update = NULL,
            download_status = NULL,
            data_loaded = FALSE,
            available_dates = NULL,
            available_sectors = NULL
        )

        # ==================================================================
        # DATA LOADING (on module initialization)
        # ==================================================================

        observe({
            # Try to load data from RDS files
            load_treasury_data()
        })

        load_treasury_data <- function() {
            # Define paths (relative to app working directory)
            rds_paths <- list(
                holdings_ts = "bond_holdings_rds/holdings_historical_long.rds",
                bond_pct = "bond_holdings_rds/all_bonds_pct_long.rds",
                bond_values = "bond_holdings_rds/all_bonds_values_long.rds"
            )

            # Check alternative paths
            if (!file.exists(rds_paths$holdings_ts)) {
                alt_paths <- c(
                    "../bond_holdings_rds/holdings_historical_long.rds",
                    "data/bond_holdings_rds/holdings_historical_long.rds"
                )
                for (path in alt_paths) {
                    if (file.exists(path)) {
                        rds_paths$holdings_ts <- path
                        rds_paths$bond_pct <- gsub("holdings_historical_long", "all_bonds_pct_long", path)
                        rds_paths$bond_values <- gsub("holdings_historical_long", "all_bonds_values_long", path)
                        break
                    }
                }
            }

            # Load holdings time series
            tryCatch({
                if (file.exists(rds_paths$holdings_ts)) {
                    treasury_data$holdings_ts <- readRDS(rds_paths$holdings_ts)
                    message(sprintf("Loaded holdings_historical_long: %d rows", nrow(treasury_data$holdings_ts)))
                } else {
                    showNotification("Holdings time series data not found. Please download data.", type = "warning", duration = 5)
                }
            }, error = function(e) {
                showNotification(paste("Error loading holdings data:", e$message), type = "error", duration = 5)
            })

            # Load bond-level holdings (percentage)
            tryCatch({
                if (file.exists(rds_paths$bond_pct)) {
                    treasury_data$bond_holdings <- readRDS(rds_paths$bond_pct)
                    message(sprintf("Loaded all_bonds_pct_long: %d rows", nrow(treasury_data$bond_holdings)))
                } else {
                    showNotification("Bond-level holdings data not found. Please download data.", type = "warning", duration = 5)
                }
            }, error = function(e) {
                showNotification(paste("Error loading bond holdings data:", e$message), type = "error", duration = 5)
            })

            # Load bond-level values
            tryCatch({
                if (file.exists(rds_paths$bond_values)) {
                    treasury_data$bond_values <- readRDS(rds_paths$bond_values)
                    message(sprintf("Loaded all_bonds_values_long: %d rows", nrow(treasury_data$bond_values)))
                }
            }, error = function(e) {
                warning(paste("Error loading bond values data:", e$message))
            })

            # Update available dates and sectors
            if (!is.null(treasury_data$holdings_ts)) {
                treasury_data$available_dates <- sort(unique(treasury_data$holdings_ts$date), decreasing = TRUE)
                treasury_data$available_sectors <- unique(treasury_data$holdings_ts$sector)
                treasury_data$data_loaded <- TRUE
                treasury_data$last_update <- Sys.time()
            }

            if (!is.null(treasury_data$bond_holdings)) {
                bond_dates <- sort(unique(treasury_data$bond_holdings$file_date), decreasing = TRUE)
                treasury_data$bond_available_dates <- bond_dates
            }
        }

        # ==================================================================
        # UI UPDATES
        # ==================================================================

        # Update sector selector when data loads
        observe({
            req(treasury_data$available_sectors)
            updateSelectizeInput(
                session,
                "sector_filter",
                choices = treasury_data$available_sectors,
                selected = treasury_data$available_sectors
            )
            updateSelectInput(
                session,
                "single_sector_select",
                choices = treasury_data$available_sectors,
                selected = "Non-residents"
            )
        })

        # Update snapshot date selector
        observe({
            req(treasury_data$bond_holdings)
            dates <- sort(unique(treasury_data$bond_holdings$file_date), decreasing = TRUE)
            date_choices <- setNames(
                as.character(dates),
                format(dates, "%B %Y")
            )
            updateSelectInput(
                session,
                "snapshot_date",
                choices = date_choices,
                selected = as.character(dates[1])
            )
        })

        # ==================================================================
        # STATUS OUTPUTS
        # ==================================================================

        output$status_data_date <- renderText({
            if (!is.null(treasury_data$holdings_ts)) {
                max_date <- max(treasury_data$holdings_ts$date, na.rm = TRUE)
                format(max_date, "%B %Y")
            } else {
                "No data"
            }
        })

        output$status_file_count <- renderText({
            if (!is.null(treasury_data$holdings_ts)) {
                n_months <- length(unique(treasury_data$holdings_ts$date))
                as.character(n_months)
            } else {
                "0"
            }
        })

        output$status_last_update <- renderText({
            if (!is.null(treasury_data$last_update)) {
                format(treasury_data$last_update, "%d %b %H:%M")
            } else {
                "Never"
            }
        })

        # ==================================================================
        # SUMMARY STATISTICS
        # ==================================================================

        output$summary_stats <- renderUI({
            req(treasury_data$holdings_ts)

            # Get latest data
            latest_date <- max(treasury_data$holdings_ts$date, na.rm = TRUE)
            latest_data <- treasury_data$holdings_ts %>%
                filter(date == latest_date)

            # Calculate stats
            top_sector <- latest_data %>%
                arrange(desc(percentage)) %>%
                slice(1)

            tagList(
                tags$div(
                    tags$strong("As of ", format(latest_date, "%B %Y"), ":"),
                    tags$br(), tags$br(),
                    tags$table(
                        style = "width: 100%; font-size: 12px;",
                        tags$tr(
                            tags$td("Largest Holder:"),
                            tags$td(style = "text-align: right;", top_sector$sector)
                        ),
                        tags$tr(
                            tags$td(""),
                            tags$td(style = "text-align: right; font-weight: bold;",
                                    sprintf("%.1f%%", top_sector$percentage * 100))
                        )
                    )
                )
            )
        })

        # ==================================================================
        # DATE PRESET BUTTONS
        # ==================================================================

        observeEvent(input$btn_1y, {
            updateDateRangeInput(
                session, "date_range",
                start = Sys.Date() - 365,
                end = Sys.Date()
            )
        })

        observeEvent(input$btn_3y, {
            updateDateRangeInput(
                session, "date_range",
                start = Sys.Date() - 365 * 3,
                end = Sys.Date()
            )
        })

        observeEvent(input$btn_all, {
            req(treasury_data$holdings_ts)
            min_date <- min(treasury_data$holdings_ts$date, na.rm = TRUE)
            updateDateRangeInput(
                session, "date_range",
                start = min_date,
                end = Sys.Date()
            )
        })

        # Sector selection helpers
        observeEvent(input$select_all_sectors, {
            req(treasury_data$available_sectors)
            updateSelectizeInput(
                session, "sector_filter",
                selected = treasury_data$available_sectors
            )
        })

        observeEvent(input$clear_sectors, {
            updateSelectizeInput(session, "sector_filter", selected = character(0))
        })

        # ==================================================================
        # DOWNLOAD AND REPROCESS BUTTONS
        # ==================================================================

        observeEvent(input$btn_check_updates, {
            showModal(modalDialog(
                title = "Checking for Updates",
                "Connecting to National Treasury website...",
                footer = NULL,
                easyClose = FALSE
            ))

            # Source the download function
            tryCatch({
                # Check for source files
                download_script <- "Modules/download/download_sa_bond_holdings.R"
                if (!file.exists(download_script)) {
                    download_script <- "bond-dashboard/Modules/download/download_sa_bond_holdings.R"
                }
                if (!file.exists(download_script)) {
                    download_script <- "../Modules/download/download_sa_bond_holdings.R"
                }

                if (file.exists(download_script)) {
                    source(download_script)

                    # Call download function
                    results <- download_sa_bond_holdings(
                        download_latest = TRUE,
                        verbose = FALSE
                    )

                    removeModal()

                    if (!is.null(results) && nrow(results) > 0) {
                        n_downloaded <- sum(results$status == "Downloaded")
                        if (n_downloaded > 0) {
                            showNotification(
                                sprintf("Downloaded %d new file(s). Reprocessing data...", n_downloaded),
                                type = "message",
                                duration = 5
                            )
                            # Trigger reprocessing
                            shinyjs::click("btn_reprocess_data")
                        } else {
                            showNotification(
                                "No new files available. Data is up to date!",
                                type = "message",
                                duration = 3
                            )
                        }
                    } else {
                        showNotification(
                            "Data is already up to date!",
                            type = "message",
                            duration = 3
                        )
                    }
                } else {
                    removeModal()
                    showNotification(
                        "Download script not found. Please check installation.",
                        type = "error",
                        duration = 5
                    )
                }
            }, error = function(e) {
                removeModal()
                showNotification(
                    paste("Download error:", e$message),
                    type = "error",
                    duration = 5
                )
            })
        })

        observeEvent(input$btn_reprocess_data, {
            showModal(modalDialog(
                title = "Reprocessing Data",
                "Processing Excel files...",
                footer = NULL,
                easyClose = FALSE
            ))

            tryCatch({
                # Source the processing functions
                process_scripts <- c(
                    "Modules/download/process_holdings_timeseries.R",
                    "Modules/download/process_bond_holdings_tidy.R"
                )

                # Try alternative paths
                for (i in seq_along(process_scripts)) {
                    if (!file.exists(process_scripts[i])) {
                        alt <- c(
                            paste0("bond-dashboard/", process_scripts[i]),
                            paste0("../", process_scripts[i])
                        )
                        for (a in alt) {
                            if (file.exists(a)) {
                                process_scripts[i] <- a
                                break
                            }
                        }
                    }
                }

                # Source files
                for (script in process_scripts) {
                    if (file.exists(script)) {
                        source(script)
                    }
                }

                # Run processing
                if (exists("process_all_sa_bond_data")) {
                    process_all_sa_bond_data(
                        source_folder = "bond_holdings",
                        output_folder = "bond_holdings_rds",
                        overwrite = TRUE,
                        verbose = FALSE
                    )

                    # Reload data
                    load_treasury_data()

                    removeModal()
                    showNotification(
                        "Data reprocessed successfully!",
                        type = "message",
                        duration = 3
                    )
                } else {
                    removeModal()
                    showNotification(
                        "Processing function not found.",
                        type = "error",
                        duration = 5
                    )
                }
            }, error = function(e) {
                removeModal()
                showNotification(
                    paste("Processing error:", e$message),
                    type = "error",
                    duration = 5
                )
            })
        })

        # ==================================================================
        # PLOT OUTPUTS
        # ==================================================================

        # 1. Holdings Area Chart
        output$holdings_area_chart <- renderPlot({
            req(treasury_data$holdings_ts)
            req(input$sector_filter)

            generate_holdings_area_chart(
                holdings_long = treasury_data$holdings_ts,
                date_range = input$date_range,
                sectors_selected = input$sector_filter
            )
        }, res = 96)

        # 2. Single Sector Trend Chart
        output$sector_trend_chart <- renderPlot({
            req(treasury_data$holdings_ts)
            req(input$single_sector_select)

            generate_sector_trend_chart(
                holdings_long = treasury_data$holdings_ts,
                sector_name = input$single_sector_select,
                date_range = input$date_range
            )
        }, res = 96)

        # 3. Bond Holdings Bar Chart (Snapshot)
        output$bond_holdings_bar_chart <- renderPlot({
            req(treasury_data$bond_holdings)
            req(input$snapshot_bond_type)
            req(input$snapshot_date)

            generate_bond_holdings_bar_chart(
                bond_pct_long = treasury_data$bond_holdings,
                selected_bond_type = input$snapshot_bond_type,
                target_date = as.Date(input$snapshot_date),
                show_labels = TRUE
            )
        }, res = 96)

        # 4. Ownership Change Chart
        output$ownership_change_chart <- renderPlot({
            req(treasury_data$holdings_ts)

            generate_ownership_change_chart(
                holdings_long = treasury_data$holdings_ts,
                periods = c(1, 3, 12)
            )
        }, res = 96)

        # 5. Diverging Change Chart
        output$diverging_change_chart <- renderPlot({
            req(treasury_data$bond_holdings)
            req(input$change_bond_type)
            req(input$change_period_select)

            generate_holdings_change_diverging(
                bond_pct_long = treasury_data$bond_holdings,
                period_months = as.integer(input$change_period_select),
                selected_bond_type = input$change_bond_type,
                top_n = 12
            )
        }, res = 96)

        # ==================================================================
        # DATA TABLE OUTPUT
        # ==================================================================

        output$holdings_table <- DT::renderDataTable({
            if (input$table_view == "aggregate") {
                req(treasury_data$holdings_ts)

                # Create wide format table for display
                # Multiply by 100 to convert decimal percentages (0.25) to display percentages (25%)
                table_data <- treasury_data$holdings_ts %>%
                    filter(date >= input$date_range[1], date <= input$date_range[2]) %>%
                    mutate(
                        date_label = format(date, "%b %Y"),
                        percentage = sprintf("%.1f%%", percentage * 100)
                    ) %>%
                    select(Date = date_label, Sector = sector, Holdings = percentage) %>%
                    pivot_wider(
                        names_from = Sector,
                        values_from = Holdings
                    ) %>%
                    arrange(desc(row_number()))

            } else {
                req(treasury_data$bond_holdings)
                req(input$table_bond_type)

                # Multiply by 100 to convert decimal percentages to display percentages
                table_data <- treasury_data$bond_holdings %>%
                    filter(bond_type == input$table_bond_type) %>%
                    filter(!grepl("TOTAL", bond, ignore.case = TRUE)) %>%
                    filter(!grepl("TOTAL", sector, ignore.case = TRUE)) %>%
                    mutate(
                        date_label = format(file_date, "%b %Y"),
                        value = sprintf("%.1f%%", value * 100)
                    ) %>%
                    select(Date = date_label, Bond = bond, Sector = sector, Holdings = value) %>%
                    arrange(desc(Date), Bond, Sector)
            }

            DT::datatable(
                table_data,
                options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel')
                ),
                filter = "top",
                rownames = FALSE
            )
        })

        # ==================================================================
        # DOWNLOAD HANDLERS
        # ==================================================================

        # Area chart download
        output$download_area_chart <- downloadHandler(
            filename = function() {
                paste0("treasury_holdings_timeseries_", Sys.Date(), ".png")
            },
            content = function(file) {
                p <- generate_holdings_area_chart(
                    holdings_long = treasury_data$holdings_ts,
                    date_range = input$date_range,
                    sectors_selected = input$sector_filter
                )
                ggsave(file, p, width = 12, height = 7, dpi = 300)
            }
        )

        # Trend chart download
        output$download_trend_chart <- downloadHandler(
            filename = function() {
                paste0("treasury_sector_trend_", input$single_sector_select, "_", Sys.Date(), ".png")
            },
            content = function(file) {
                p <- generate_sector_trend_chart(
                    holdings_long = treasury_data$holdings_ts,
                    sector_name = input$single_sector_select,
                    date_range = input$date_range
                )
                ggsave(file, p, width = 10, height = 6, dpi = 300)
            }
        )

        # Holdings bar chart download
        output$download_holdings_chart <- downloadHandler(
            filename = function() {
                paste0("treasury_bond_holdings_", input$snapshot_bond_type, "_", Sys.Date(), ".png")
            },
            content = function(file) {
                p <- generate_bond_holdings_bar_chart(
                    bond_pct_long = treasury_data$bond_holdings,
                    selected_bond_type = input$snapshot_bond_type,
                    target_date = as.Date(input$snapshot_date),
                    show_labels = TRUE
                )
                ggsave(file, p, width = 10, height = 12, dpi = 300)
            }
        )

        # Ownership change chart download
        output$download_change_chart <- downloadHandler(
            filename = function() {
                paste0("treasury_ownership_changes_", Sys.Date(), ".png")
            },
            content = function(file) {
                p <- generate_ownership_change_chart(
                    holdings_long = treasury_data$holdings_ts,
                    periods = c(1, 3, 12)
                )
                ggsave(file, p, width = 10, height = 7, dpi = 300)
            }
        )

        # Diverging chart download
        output$download_diverging_chart <- downloadHandler(
            filename = function() {
                paste0("treasury_bond_changes_", input$change_bond_type, "_", Sys.Date(), ".png")
            },
            content = function(file) {
                p <- generate_holdings_change_diverging(
                    bond_pct_long = treasury_data$bond_holdings,
                    period_months = as.integer(input$change_period_select),
                    selected_bond_type = input$change_bond_type,
                    top_n = 12
                )
                ggsave(file, p, width = 10, height = 8, dpi = 300)
            }
        )

        # CSV export
        output$download_table_csv <- downloadHandler(
            filename = function() {
                paste0("treasury_holdings_data_", Sys.Date(), ".csv")
            },
            content = function(file) {
                if (input$table_view == "aggregate") {
                    data <- treasury_data$holdings_ts %>%
                        filter(date >= input$date_range[1], date <= input$date_range[2])
                } else {
                    data <- treasury_data$bond_holdings %>%
                        filter(bond_type == input$table_bond_type)
                }
                write.csv(data, file, row.names = FALSE)
            }
        )

        # Excel export
        output$download_table_excel <- downloadHandler(
            filename = function() {
                paste0("treasury_holdings_data_", Sys.Date(), ".xlsx")
            },
            content = function(file) {
                if (input$table_view == "aggregate") {
                    data <- treasury_data$holdings_ts %>%
                        filter(date >= input$date_range[1], date <= input$date_range[2])
                } else {
                    data <- treasury_data$bond_holdings %>%
                        filter(bond_type == input$table_bond_type)
                }
                openxlsx::write.xlsx(data, file)
            }
        )

    })
}

# ================================================================================
# MODULE INITIALIZATION MESSAGE
# ================================================================================
message("Treasury Holdings module loaded successfully")
