# Enhanced Bond Analytics Dashboard for Insele Capital Partners
# Purpose: Professional pre-auction analysis tool for SA Government Bonds

# Load required libraries
pacman::p_load(
    shiny, shinydashboard, shinyWidgets, shinycssloaders, shinyjs,
    readxl, dplyr, tidyverse, lubridate, zoo, tibble, rlang, tidyr,
    ggplot2, plotly, scales, viridis, gridExtra,
    DT, rmarkdown, knitr, openxlsx,
    splines, mgcv
)

# Source enhanced modules
source("bond-dashboard/R/data_processing.R")
source("bond-dashboard/R/plotting.R")
source("bond-dashboard/R/analytics.R")
source("bond-dashboard/Modules/data_loader.R")

# Initialize shinyjs for dynamic UI
shinyjs::useShinyjs()

# ================== UI DEFINITION ==================

ui <- dashboardPage(
    skin = "blue",

    # Enhanced Header with notifications
    dashboardHeader(
        title = tags$div(
            style = "display: flex; align-items: center;",
            tags$img(src = "insele_logo.png", height = "40px", style = "margin-right: 10px;"),
            tags$span("Bond Analytics Pro", style = "font-weight: bold; color: #1e3a5f;")
        ),
        titleWidth = 350,

        # Add notification menu
        dropdownMenu(
            type = "notifications",
            badgeStatus = "warning",
            icon = icon("bell"),
            tags$li(id = "alert_placeholder")
        )
    ),

    # Enhanced Sidebar with smart controls
    dashboardSidebar(
        width = 350,

        # Analysis template selector (new)
        radioButtons(
            "analysis_template",
            "Quick Analysis Templates:",
            choices = c(
                "Pre-Auction Overview" = "auction",
                "Relative Value Deep Dive" = "rv",
                "Liquidity Focus" = "liquidity",
                "Risk Assessment" = "risk",
                "Custom Analysis" = "custom"
            ),
            selected = "auction"
        ),

        hr(),

        sidebarMenu(
            id = "tabs",
            menuItem("Executive Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
            menuItem("Supply & Demand", tabName = "supply_demand", icon = icon("chart-line")),
            menuItem("Relative Value", tabName = "relative_value", icon = icon("balance-scale")),
            menuItem("Auction Analytics", tabName = "auction_stats", icon = icon("gavel")),
            menuItem("Risk Metrics", tabName = "risk", icon = icon("shield-alt")),
            menuItem("Trading Signals", tabName = "signals", icon = icon("signal")),
            menuItem("Report Generation", tabName = "report", icon = icon("file-pdf"))
        ),

        hr(),

        # Global Controls
        h4("Analysis Parameters", style = "padding-left: 15px; color: #1e3a5f;"),

        # Date range with quick selectors
        dateRangeInput(
            "date_range",
            "Analysis Period:",
            start = Sys.Date() - 365,
            end = Sys.Date(),
            width = "90%"
        ),

        radioButtons(
            "time_period",
            "Quick Period:",
            choices = c(
                "YTD" = "ytd",
                "1 Month" = "1m",
                "3 Months" = "3m",
                "6 Months" = "6m",
                "12 Months" = "12m",
                "Custom" = "custom"
            ),
            selected = "3m",
            inline = TRUE
        ),

        # Smart bond selector with grouping
        pickerInput(
            "bond_selection",
            "Select Bonds:",
            choices = NULL,
            selected = NULL,
            options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 3",
                `count-selected-text` = "{0} bonds selected",
                `live-search` = TRUE,
                `live-search-placeholder` = "Search bonds..."
            ),
            multiple = TRUE,
            width = "90%"
        ),

        # Comparison periods (new)
        checkboxGroupInput(
            "comparison_periods",
            "Compare vs:",
            choices = c(
                "Previous Week" = "week",
                "Previous Month" = "month",
                "Previous Quarter" = "quarter",
                "Previous Auction" = "auction"
            ),
            selected = "week"
        ),

        hr(),

        # Alert thresholds (new)
        sliderInput(
            "alert_threshold",
            "Alert Z-Score Threshold:",
            min = 1.0,
            max = 3.0,
            value = 2.0,
            step = 0.1,
            width = "90%"
        ),

        hr(),

        # Data quality indicators
        valueBoxOutput("data_quality", width = 12),
        valueBoxOutput("last_update_box", width = 12)
    ),

    # Main Body
    dashboardBody(
        # Enhanced CSS
        tags$head(
            tags$style(HTML("
                .content-wrapper, .right-side {
                    background-color: #f8f9fa;
                }
                .box {
                    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                    border-radius: 8px;
                    transition: all 0.3s ease;
                }
                .box:hover {
                    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
                }
                .box-header {
                    background: linear-gradient(135deg, #1e3a5f 0%, #6b9aa0 100%);
                    color: white;
                    border-radius: 8px 8px 0 0;
                }
                .small-box {
                    border-radius: 8px;
                    transition: transform 0.3s ease;
                }
                .small-box:hover {
                    transform: translateY(-5px);
                }
                .info-box {
                    transition: all 0.3s ease;
                }
                .info-box:hover {
                    background: #f0f8ff;
                }
                .nav-tabs-custom > .tab-content {
                    padding: 20px;
                }
                .alert-box {
                    padding: 10px;
                    margin: 10px;
                    border-radius: 5px;
                    background: #fff3cd;
                    border: 1px solid #ffc107;
                }
                .metric-card {
                    background: white;
                    padding: 15px;
                    border-radius: 8px;
                    margin-bottom: 15px;
                    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                }
            "))
        ),

        # Initialize shinyjs
        shinyjs::useShinyjs(),

        tabItems(
            # Executive Dashboard Tab (new)
            tabItem(
                tabName = "dashboard",

                # Key alerts row
                fluidRow(
                    column(12,
                           div(id = "alerts_panel", class = "alert-box",
                               h4(icon("exclamation-triangle"), "Key Alerts"),
                               uiOutput("key_alerts")
                           )
                    )
                ),

                # Summary metrics row
                fluidRow(
                    valueBoxOutput("top_opportunity"),
                    valueBoxOutput("avg_btc"),
                    valueBoxOutput("curve_shape"),
                    valueBoxOutput("next_auction")
                ),

                # Main dashboard visualizations
                fluidRow(
                    box(
                        title = "Market Overview",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 6,
                        withSpinner(plotOutput("dashboard_yield_curve", height = "400px"))
                    ),
                    box(
                        title = "Top Trading Opportunities",
                        status = "success",
                        solidHeader = TRUE,
                        width = 6,
                        DT::dataTableOutput("dashboard_opportunities")
                    )
                ),

                fluidRow(
                    box(
                        title = "Recent Auction Performance",
                        status = "warning",
                        width = 6,
                        withSpinner(plotOutput("dashboard_auction_trend", height = "350px"))
                    ),
                    box(
                        title = "Risk Metrics Summary",
                        status = "danger",
                        width = 6,
                        withSpinner(plotOutput("dashboard_risk_summary", height = "350px"))
                    )
                )
            ),

            # Enhanced Supply & Demand Tab
            tabItem(
                tabName = "supply_demand",
                fluidRow(
                    box(
                        title = "Supply & Demand Analysis",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,

                        fluidRow(
                            column(3,
                                   radioButtons("sd_xaxis", "X-Axis:",
                                                choices = c(
                                                    "Modified Duration" = "modified_duration",
                                                    "Duration" = "duration",
                                                    "Yield" = "yield_to_maturity"
                                                ),
                                                selected = "modified_duration"
                                   )
                            ),
                            column(3,
                                   radioButtons("sd_yaxis", "Y-Axis:",
                                                choices = c(
                                                    "Bid-to-Cover" = "bid_to_cover",
                                                    "Total Bids (ZAR bn)" = "total_bids",
                                                    "Allocation Rate" = "avg_allocation"
                                                ),
                                                selected = "bid_to_cover"
                                   )
                            ),
                            column(3,
                                   radioButtons("sd_size", "Size:",
                                                choices = c(
                                                    "Frequency" = "frequency",
                                                    "Total Offered" = "total_offered",
                                                    "Liquidity Score" = "liquidity"
                                                ),
                                                selected = "frequency"
                                   )
                            ),
                            column(3,
                                   checkboxInput("sd_show_zones", "Show Zones", TRUE),
                                   checkboxInput("sd_show_movement", "Show Movement", TRUE),
                                   checkboxInput("sd_show_history", "Show History", FALSE)
                            )
                        ),

                        hr(),

                        withSpinner(plotOutput("enhanced_supply_demand", height = "600px")),

                        hr(),

                        h4("Detailed Metrics"),
                        DT::dataTableOutput("sd_detailed_table")
                    )
                ),

                fluidRow(
                    box(
                        title = "Demand Heatmap",
                        status = "info",
                        width = 12,
                        withSpinner(plotOutput("demand_heatmap", height = "400px"))
                    )
                )
            ),

            # Enhanced Relative Value Tab
            tabItem(
                tabName = "relative_value",
                fluidRow(
                    box(
                        title = "Relative Value Controls",
                        status = "primary",
                        width = 12,
                        collapsed = FALSE,

                        fluidRow(
                            column(3,
                                   selectInput("rv_analysis_type", "Analysis Type:",
                                               choices = c(
                                                   "Yield Curve" = "curve",
                                                   "Rich/Cheap" = "richCheap",
                                                   "Z-Score Evolution" = "zscore",
                                                   "Butterfly Trades" = "butterfly"
                                               ),
                                               selected = "curve"
                                   )
                            ),
                            column(3,
                                   sliderInput("rv_lookback", "Lookback Period:",
                                               min = 20, max = 252, value = 60, step = 20
                                   )
                            ),
                            column(3,
                                   sliderInput("rv_smoothing", "Smoothing:",
                                               min = 0.3, max = 1.5, value = 0.7, step = 0.1
                                   )
                            ),
                            column(3,
                                   numericInput("rv_zscore_threshold", "Z-Score Alert:",
                                                value = 1.5, min = 0.5, max = 3, step = 0.1
                                   )
                            )
                        )
                    )
                ),

                fluidRow(
                    box(
                        title = "Relative Value Analysis",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,

                        tabsetPanel(
                            tabPanel("Main Analysis",
                                     withSpinner(plotOutput("rv_main_plot", height = "600px"))
                            ),
                            tabPanel("Rich/Cheap Waterfall",
                                     withSpinner(plotOutput("rv_waterfall", height = "600px"))
                            ),
                            tabPanel("Z-Score Term Structure",
                                     withSpinner(plotOutput("rv_zscore_plot", height = "600px"))
                            ),
                            tabPanel("Pair Trades",
                                     DT::dataTableOutput("pair_trades_table")
                            )
                        )
                    )
                ),

                fluidRow(
                    box(
                        title = "Roll-Down Analysis",
                        status = "success",
                        width = 6,
                        DT::dataTableOutput("rolldown_table")
                    ),
                    box(
                        title = "Butterfly Opportunities",
                        status = "warning",
                        width = 6,
                        DT::dataTableOutput("butterfly_table")
                    )
                )
            ),

            # Risk Metrics Tab (new)
            tabItem(
                tabName = "risk",
                fluidRow(
                    box(
                        title = "Risk Dashboard",
                        status = "danger",
                        solidHeader = TRUE,
                        width = 12,

                        fluidRow(
                            valueBoxOutput("highest_risk"),
                            valueBoxOutput("lowest_risk"),
                            valueBoxOutput("best_sharpe"),
                            valueBoxOutput("avg_volatility")
                        )
                    )
                ),

                fluidRow(
                    box(
                        title = "Risk Metrics Comparison",
                        status = "primary",
                        width = 6,
                        withSpinner(plotOutput("risk_comparison", height = "400px"))
                    ),
                    box(
                        title = "VaR Analysis",
                        status = "warning",
                        width = 6,
                        withSpinner(plotOutput("var_analysis", height = "400px"))
                    )
                ),

                fluidRow(
                    box(
                        title = "Detailed Risk Metrics",
                        status = "info",
                        width = 12,
                        DT::dataTableOutput("risk_metrics_table")
                    )
                )
            ),

            # Trading Signals Tab (enhanced)
            tabItem(
                tabName = "signals",
                fluidRow(
                    box(
                        title = "Multi-Factor Trading Signals",
                        status = "success",
                        solidHeader = TRUE,
                        width = 12,

                        fluidRow(
                            column(12,
                                   h4("Signal Configuration"),
                                   fluidRow(
                                       column(3,
                                              sliderInput("signal_value_weight", "Value Weight:",
                                                          min = 0, max = 1, value = 0.4, step = 0.1)
                                       ),
                                       column(3,
                                              sliderInput("signal_momentum_weight", "Momentum Weight:",
                                                          min = 0, max = 1, value = 0.2, step = 0.1)
                                       ),
                                       column(3,
                                              sliderInput("signal_liquidity_weight", "Liquidity Weight:",
                                                          min = 0, max = 1, value = 0.2, step = 0.1)
                                       ),
                                       column(3,
                                              sliderInput("signal_risk_weight", "Risk Weight:",
                                                          min = 0, max = 1, value = 0.2, step = 0.1)
                                       )
                                   )
                            )
                        ),

                        hr(),

                        DT::dataTableOutput("trading_signals_table")
                    )
                ),

                fluidRow(
                    box(
                        title = "Signal Performance Backtest",
                        status = "info",
                        width = 12,
                        withSpinner(plotOutput("signal_backtest", height = "400px"))
                    )
                )
            ),

            # Enhanced Report Generation Tab
            tabItem(
                tabName = "report",
                fluidRow(
                    box(
                        title = "Report Configuration",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,

                        fluidRow(
                            column(6,
                                   h4("Report Sections:"),
                                   checkboxGroupInput("report_sections",
                                                      label = NULL,
                                                      choices = c(
                                                          "Executive Summary" = "exec",
                                                          "Market Overview" = "market",
                                                          "Supply & Demand Analysis" = "supply",
                                                          "Relative Value Analysis" = "relative",
                                                          "Risk Assessment" = "risk",
                                                          "Trading Recommendations" = "trading",
                                                          "Auction Calendar" = "calendar",
                                                          "Technical Appendix" = "appendix"
                                                      ),
                                                      selected = c("exec", "market", "relative", "trading")
                                   )
                            ),
                            column(6,
                                   h4("Report Options:"),
                                   textInput("report_title", "Report Title:",
                                             value = paste("SA Government Bonds Pre-Auction Report -",
                                                           format(Sys.Date(), "%B %Y"))),

                                   textInput("report_author", "Author:",
                                             value = "Insele Capital Partners Research Team"),

                                   radioButtons("report_format", "Output Format:",
                                                choices = c(
                                                    "PDF Report" = "pdf",
                                                    "Excel Workbook" = "excel",
                                                    "HTML Dashboard" = "html",
                                                    "PowerPoint" = "pptx"
                                                ),
                                                selected = "pdf"
                                   ),

                                   br(),

                                   actionButton("generate_report", "Generate Report",
                                                class = "btn-success btn-lg",
                                                icon = icon("file-pdf"),
                                                width = "100%")
                            )
                        ),

                        hr(),

                        uiOutput("report_status"),

                        br(),

                        fluidRow(
                            column(3,
                                   downloadButton("download_report", "Download Report",
                                                  class = "btn-primary", width = "100%")
                            ),
                            column(3,
                                   downloadButton("download_data", "Download Data",
                                                  class = "btn-info", width = "100%")
                            ),
                            column(3,
                                   downloadButton("download_charts", "Download Charts",
                                                  class = "btn-warning", width = "100%")
                            ),
                            column(3,
                                   actionButton("email_report", "Email Report",
                                                class = "btn-success", icon = icon("envelope"),
                                                width = "100%")
                            )
                        )
                    )
                )
            )
        )
    )
)

# ================== SERVER DEFINITION ==================

server <- function(input, output, session) {

    # Reactive values
    values <- reactiveValues(
        full_df = NULL,
        filtered_df = NULL,
        validated_df = NULL,
        cache = NULL,
        alerts = list(),
        report_ready = FALSE,
        last_update = NULL
    )

    # Load and validate data using robust loader
    observe({
        tryCatch({
            showNotification("Loading bond data...", type = "message", duration = 2)

            # Use the robust data loading function (dynamic bond detection, #N/A handling, maturity filtering)
            bond_data <- load_bond_data_robust(
                file_path = "bond-dashboard/data/Siyanda Bonds.xlsx",
                reference_date = Sys.Date()
            )

            # Extract the full dataframe
            full_df <- bond_data$full_df

            # Store maturity lookup and auction data for reference
            values$maturity_lookup <- bond_data$maturity_lookup
            values$auction_raw <- bond_data$auction_raw
            values$bond_metadata <- bond_data$bond_metadata
            values$auction_summary <- bond_data$auction_summary

            # Validate and enhance data
            validation_results <- validate_bond_data(full_df)
            values$full_df <- validation_results$data
            values$validated_df <- add_derived_metrics(validation_results$data)

            # Create cache for expensive calculations
            values$cache <- create_calculation_cache(values$validated_df)
            values$last_update <- Sys.time()

            # Check for alerts
            check_for_alerts()

            # Update bond selector with dynamic bond list (no hardcoded bonds!)
            bond_choices <- unique(values$validated_df$bond)

            # Group bonds by maturity bucket (from maturity_lookup for accurate grouping)
            bond_groups <- values$validated_df %>%
                group_by(bond) %>%
                summarise(
                    avg_duration = mean(modified_duration, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                mutate(group = case_when(
                    avg_duration <= 5 ~ "Short (≤5y)",
                    avg_duration <= 10 ~ "Medium (5-10y)",
                    avg_duration <= 15 ~ "Long (10-15y)",
                    TRUE ~ "Ultra-Long (>15y)"
                ))

            grouped_choices <- split(bond_groups$bond, bond_groups$group)

            updatePickerInput(session, "bond_selection",
                              choices = grouped_choices,
                              selected = bond_choices)

            # Show data quality notification with bond count
            n_bonds <- length(unique(values$validated_df$bond))
            if(validation_results$needs_attention) {
                showNotification(
                    paste("Loaded", n_bonds, "active bonds with", length(validation_results$report$warnings), "warnings"),
                    type = "warning",
                    duration = 5
                )
            } else {
                showNotification(
                    paste("Loaded", n_bonds, "active bonds successfully"),
                    type = "success",
                    duration = 3
                )
            }

        }, error = function(e) {
            showNotification(paste("Error loading data:", e$message),
                             type = "error", duration = NULL)
        })
    })

    # Check for alerts function
    check_for_alerts <- function() {
        req(values$validated_df)

        alerts <- list()

        # Check for extreme z-scores
        zscore_data <- calculate_rolling_zscores(values$validated_df)
        extreme_bonds <- zscore_data %>%
            filter(date == max(date)) %>%
            filter(abs(conviction_score) > input$alert_threshold)

        if(nrow(extreme_bonds) > 0) {
            alerts$zscore <- paste(extreme_bonds$bond, "at extreme valuation")
        }

        # Check for upcoming auctions
        auction_pressure <- calculate_auction_pressure(values$validated_df)
        overdue <- auction_pressure %>%
            filter(squeeze_risk > 2)

        if(nrow(overdue) > 0) {
            alerts$auction <- paste(overdue$bond, "overdue for auction")
        }

        # Check for curve inversions
        latest_curve <- values$validated_df %>%
            filter(date == max(date)) %>%
            arrange(modified_duration)

        if(any(diff(latest_curve$yield_to_maturity) < -0.3)) {
            alerts$inversion <- "Yield curve inversion detected"
        }

        values$alerts <- alerts
    }

    # Filter data based on inputs (with dynamic maturity filtering)
    observe({
        req(values$validated_df)

        # Handle time period selection
        if(input$time_period != "custom") {
            end_date <- Sys.Date()
            start_date <- switch(
                input$time_period,
                "ytd" = floor_date(Sys.Date(), "year"),
                "1m" = end_date - months(1),
                "3m" = end_date - months(3),
                "6m" = end_date - months(6),
                "12m" = end_date - months(12)
            )
            updateDateRangeInput(session, "date_range",
                                 start = start_date, end = end_date)
        }

        # Apply date range filter
        start_date <- input$date_range[1]
        end_date <- input$date_range[2]

        filtered <- values$validated_df %>%
            filter(date >= start_date & date <= end_date)

        # Re-filter for bonds that were active during this period
        # (maturity date must be AFTER the end_date to be included)
        if ("maturity_date" %in% names(filtered)) {
            filtered <- filtered %>%
                filter(maturity_date > end_date | is.na(maturity_date))
        }

        # Apply bond selection filter
        if(!is.null(input$bond_selection) && length(input$bond_selection) > 0) {
            filtered <- filtered %>%
                filter(bond %in% input$bond_selection)
        }

        values$filtered_df <- filtered
    })

    # Apply template settings
    observeEvent(input$analysis_template, {
        template <- input$analysis_template

        if(template == "auction") {
            updateTabItems(session, "tabs", "dashboard")
            updateRadioButtons(session, "time_period", selected = "3m")
            updateSliderInput(session, "alert_threshold", value = 1.5)
        } else if(template == "rv") {
            updateTabItems(session, "tabs", "relative_value")
            updateRadioButtons(session, "time_period", selected = "6m")
            updateSliderInput(session, "alert_threshold", value = 2.0)
        } else if(template == "liquidity") {
            updateTabItems(session, "tabs", "supply_demand")
            updateRadioButtons(session, "time_period", selected = "ytd")
        } else if(template == "risk") {
            updateTabItems(session, "tabs", "risk")
            updateRadioButtons(session, "time_period", selected = "1m")
        }
    })

    # ================== DASHBOARD TAB OUTPUTS ==================

    # Key alerts
    output$key_alerts <- renderUI({
        if(length(values$alerts) == 0) {
            tags$p("No critical alerts at this time", style = "color: green;")
        } else {
            tags$ul(
                lapply(values$alerts, function(alert) {
                    tags$li(alert, style = "color: #ff6b6b; font-weight: bold;")
                })
            )
        }
    })

    # Dashboard value boxes
    output$top_opportunity <- renderValueBox({
        req(values$filtered_df)

        signals <- tryCatch({
            liquidity_scores <- calculate_liquidity_score(values$filtered_df)
            risk_metrics <- calculate_risk_metrics(values$filtered_df)
            generate_trading_signals(values$filtered_df,
                                     liquidity_data = liquidity_scores,
                                     risk_data = risk_metrics)
        }, error = function(e) {
            # Return empty dataframe if error
            data.frame(bond = character(),
                       signal = character(),
                       confidence = character(),
                       composite_score = numeric(),
                       yield_to_maturity = numeric())
        })
        top_opp <- signals %>% filter(signal %in% c("BUY", "STRONG BUY")) %>% head(1)

        valueBox(
            value = ifelse(nrow(top_opp) > 0, top_opp$bond, "None"),
            subtitle = ifelse(nrow(top_opp) > 0,
                              paste("Signal:", top_opp$signal),
                              "No clear opportunities"),
            icon = icon("star"),
            color = ifelse(nrow(top_opp) > 0, "green", "black")
        )
    })

    output$avg_btc <- renderValueBox({
        req(values$filtered_df)
        recent_btc <- values$filtered_df %>%
            filter(!is.na(bid_to_cover) & date >= max(date) - 30) %>%
            summarise(avg = mean(bid_to_cover))

        valueBox(
            value = round(recent_btc$avg, 2),
            subtitle = "30-Day Avg BTC",
            icon = icon("chart-line"),
            color = if(recent_btc$avg > 3) "green" else if(recent_btc$avg > 2) "yellow" else "red"
        )
    })

    output$curve_shape <- renderValueBox({
        req(values$filtered_df)
        curve_decomp <- decompose_curve(values$filtered_df)

        valueBox(
            value = curve_decomp$curve_shape,
            subtitle = paste("Slope:", round(curve_decomp$slope_bps, 0), "bps"),
            icon = icon("chart-area"),
            color = "blue"
        )
    })

    output$next_auction <- renderValueBox({
        valueBox(
            value = format(Sys.Date() + 7, "%b %d"),
            subtitle = "Next Auction Date",
            icon = icon("gavel"),
            color = "purple"
        )
    })

    # Dashboard plots
    output$dashboard_yield_curve <- renderPlot({
        req(values$filtered_df)
        create_enhanced_yield_curve(values$filtered_df,
                                    show_history = TRUE,
                                    confidence_bands = TRUE)
    })

    output$dashboard_opportunities <- DT::renderDataTable({
        req(values$filtered_df)

        signals <- tryCatch({
            liquidity_scores <- calculate_liquidity_score(values$filtered_df)
            risk_metrics <- calculate_risk_metrics(values$filtered_df)
            generate_trading_signals(values$filtered_df,
                                     liquidity_data = liquidity_scores,
                                     risk_data = risk_metrics)
        }, error = function(e) {
            # Return empty dataframe if error
            data.frame(bond = character(),
                       signal = character(),
                       confidence = character(),
                       composite_score = numeric(),
                       yield_to_maturity = numeric())
        })

        signals %>%
            filter(signal %in% c("BUY", "STRONG BUY", "SELL", "STRONG SELL")) %>%
            select(Bond = bond,
                   `YTM (%)` = yield_to_maturity,
                   `Score` = composite_score,
                   Signal = signal,
                   Confidence = confidence) %>%
            head(5) %>%
            DT::datatable(
                options = list(
                    pageLength = 5,
                    dom = 't',
                    ordering = FALSE
                ),
                rownames = FALSE
            ) %>%
            formatStyle("Signal",
                        backgroundColor = styleEqual(
                            c("STRONG BUY", "BUY", "SELL", "STRONG SELL"),
                            c("#4CAF50", "#8BC34A", "#FFC107", "#F44336")
                        ),
                        color = "white",
                        fontWeight = "bold"
            )
    })

    output$dashboard_auction_trend <- renderPlot({
        req(values$filtered_df)

        auction_trend <- values$filtered_df %>%
            filter(!is.na(bid_to_cover)) %>%
            group_by(date) %>%
            summarise(avg_btc = mean(bid_to_cover))

        # Only add smooth line if enough data points
        p <- ggplot(auction_trend, aes(x = date, y = avg_btc)) +
            geom_line(color = insele_colors$teal, size = 2)

        if(nrow(auction_trend) > 5) {  # Only smooth if enough points
            p <- p + geom_smooth(method = "loess", se = TRUE, alpha = 0.2,
                                 color = insele_colors$orange)
        }

        p + geom_hline(yintercept = 3, linetype = "dashed",
                       color = insele_colors$navy, alpha = 0.5) +
            labs(x = "", y = "Average Bid-to-Cover",
                 caption = "Insele Capital Partners") +
            theme_insele()
    })

    output$dashboard_risk_summary <- renderPlot({
        req(values$filtered_df)

        risk_metrics <- calculate_risk_metrics(values$filtered_df)

        risk_metrics %>%
            pivot_longer(cols = c(yield_vol, risk_units, sharpe_ratio),
                         names_to = "metric", values_to = "value") %>%
            ggplot(aes(x = bond, y = value, fill = metric)) +
            geom_col(position = "dodge") +
            facet_wrap(~metric, scales = "free_y", ncol = 1) +
            scale_fill_manual(values = c(
                yield_vol = insele_colors$red,
                risk_units = insele_colors$orange,
                sharpe_ratio = insele_colors$green
            )) +
            labs(x = "", y = "") +
            theme_insele() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "none")
    })

    # ================== SUPPLY & DEMAND OUTPUTS ==================

    output$enhanced_supply_demand <- renderPlot({
        req(values$filtered_df)

        tryCatch({
            create_enhanced_supply_demand_plot(
                values$filtered_df,
                x_var = input$sd_xaxis,
                y_var = input$sd_yaxis,
                size_var = input$sd_size
            )
        }, error = function(e) {
            # Fallback to a simple plot if there's an error
            ggplot(values$filtered_df, aes(x = modified_duration, y = yield_to_maturity)) +
                geom_point() +
                theme_minimal() +
                labs(title = "Data processing error - showing simple yield curve",
                     subtitle = paste("Error:", e$message))
        })
    })

    output$demand_heatmap <- renderPlot({
        req(values$filtered_df)
        create_auction_demand_heatmap(values$filtered_df)
    })

    output$sd_detailed_table <- DT::renderDataTable({
        req(values$filtered_df)

        sd_metrics <- values$filtered_df %>%
            group_by(bond) %>%
            summarise(
                `Avg Duration` = round(mean(modified_duration, na.rm = TRUE), 2),
                `Avg YTM (%)` = round(mean(yield_to_maturity, na.rm = TRUE), 2),
                `Avg BTC` = round(mean(bid_to_cover, na.rm = TRUE), 2),
                `Total Bids (bn)` = round(sum(bids, na.rm = TRUE) / 1e9, 2),
                `Frequency` = sum(!is.na(bid_to_cover)),
                `Last Auction` = max(date[!is.na(bid_to_cover)]),
                .groups = "drop"
            ) %>%
            mutate(`Days Since` = as.numeric(Sys.Date() - `Last Auction`))

        DT::datatable(
            sd_metrics,
            options = list(
                pageLength = 15,
                order = list(list(2, "desc"))
            ),
            rownames = FALSE
        ) %>%
            formatStyle("Days Since",
                        backgroundColor = styleInterval(c(30, 60),
                                                        c("lightgreen", "lightyellow", "lightcoral"))
            )
    })

    # ================== RELATIVE VALUE OUTPUTS ==================

    output$rv_main_plot <- renderPlot({
        req(values$filtered_df)

        if(input$rv_analysis_type == "curve") {
            create_enhanced_yield_curve(values$filtered_df,
                                        show_history = TRUE,
                                        confidence_bands = TRUE)
        } else if(input$rv_analysis_type == "richCheap") {
            create_rich_cheap_waterfall(values$filtered_df)
        } else if(input$rv_analysis_type == "zscore") {
            create_zscore_evolution(values$filtered_df)
        } else {
            # Butterfly visualization
            butterfly_data <- identify_butterfly_trades(values$filtered_df)

            ggplot(butterfly_data %>% head(10),
                   aes(x = paste(wing1, body, wing2, sep = "-"),
                       y = richness_bps, fill = signal)) +
                geom_col() +
                coord_flip() +
                scale_fill_manual(values = c(
                    "CHEAP (Buy Body)" = insele_colors$green,
                    "RICH (Sell Body)" = insele_colors$red,
                    "NEUTRAL" = insele_colors$gray
                )) +
                labs(title = "Top Butterfly Trade Opportunities",
                     x = "Butterfly Combination",
                     y = "Richness/Cheapness (bps)") +
                theme_insele()
        }
    })

    output$rv_waterfall <- renderPlot({
        req(values$filtered_df)
        create_rich_cheap_waterfall(values$filtered_df)
    })

    output$rv_zscore_plot <- renderPlot({
        req(values$filtered_df)
        create_zscore_evolution(values$filtered_df,
                                lookback_periods = c(20, input$rv_lookback, 252))
    })

    output$pair_trades_table <- DT::renderDataTable({
        req(values$filtered_df)

        pairs <- identify_pair_trades(values$filtered_df, input$rv_zscore_threshold)

        DT::datatable(
            pairs %>% head(20),
            options = list(pageLength = 10),
            rownames = FALSE
        ) %>%
            formatStyle("signal",
                        backgroundColor = styleEqual(
                            c("EXECUTE", "MONITOR", "IGNORE"),
                            c("lightgreen", "lightyellow", "lightgray")
                        )
            ) %>%
            formatRound(c("current_spread_bps", "mean_spread_bps",
                          "expected_profit_bps"), 1) %>%
            formatRound(c("z_score", "hedge_ratio"), 2)
    })

    output$rolldown_table <- DT::renderDataTable({
        req(values$filtered_df)

        rolldown <- calculate_rolldown(values$filtered_df)

        rolldown %>%
            filter(horizon_months == 3) %>%
            arrange(desc(total_return)) %>%
            DT::datatable(
                options = list(pageLength = 10),
                rownames = FALSE
            ) %>%
            formatRound(c("carry_return", "rolldown_return",
                          "total_return", "annualized_return"), 2) %>%
            formatRound("breakeven_bps", 0)
    })

    output$butterfly_table <- DT::renderDataTable({
        req(values$filtered_df)

        butterflies <- identify_butterfly_trades(values$filtered_df,
                                                 input$rv_zscore_threshold)

        DT::datatable(
            butterflies %>% head(10),
            options = list(pageLength = 10),
            rownames = FALSE
        ) %>%
            formatStyle("signal",
                        backgroundColor = styleEqual(
                            c("CHEAP (Buy Body)", "RICH (Sell Body)", "NEUTRAL"),
                            c("lightgreen", "lightcoral", "lightgray")
                        )
            ) %>%
            formatRound(c("richness_bps", "z_score", "percentile"), 1)
    })

    # ================== RISK METRICS OUTPUTS ==================

    output$highest_risk <- renderValueBox({
        req(values$filtered_df)
        risk_metrics <- calculate_risk_metrics(values$filtered_df)
        highest <- risk_metrics %>% arrange(desc(risk_units)) %>% head(1)

        valueBox(
            value = highest$bond,
            subtitle = paste("Risk Units:", round(highest$risk_units, 2)),
            icon = icon("exclamation-triangle"),
            color = "red"
        )
    })

    output$lowest_risk <- renderValueBox({
        req(values$filtered_df)
        risk_metrics <- calculate_risk_metrics(values$filtered_df)
        lowest <- risk_metrics %>% arrange(risk_units) %>% head(1)

        valueBox(
            value = lowest$bond,
            subtitle = paste("Risk Units:", round(lowest$risk_units, 2)),
            icon = icon("shield-alt"),
            color = "green"
        )
    })

    output$best_sharpe <- renderValueBox({
        req(values$filtered_df)
        risk_metrics <- calculate_risk_metrics(values$filtered_df)
        best <- risk_metrics %>% arrange(desc(sharpe_ratio)) %>% head(1)

        valueBox(
            value = best$bond,
            subtitle = paste("Sharpe Ratio:", round(best$sharpe_ratio, 2)),
            icon = icon("trophy"),
            color = "yellow"
        )
    })

    output$avg_volatility <- renderValueBox({
        req(values$filtered_df)
        risk_metrics <- calculate_risk_metrics(values$filtered_df)
        avg_vol <- mean(risk_metrics$yield_vol, na.rm = TRUE)

        valueBox(
            value = paste0(round(avg_vol * 100, 1), "%"),
            subtitle = "Avg Yield Volatility (Ann.)",
            icon = icon("chart-line"),
            color = "blue"
        )
    })

    output$risk_comparison <- renderPlot({
        req(values$filtered_df)

        risk_metrics <- calculate_risk_metrics(values$filtered_df)

        # Filter out rows with NA values to prevent geom_text_repel errors
        risk_metrics_clean <- risk_metrics %>%
            filter(!is.na(yield_vol) & !is.na(sharpe_ratio) & !is.na(risk_units))

        ggplot(risk_metrics_clean, aes(x = yield_vol * 100, y = sharpe_ratio)) +
            geom_point(aes(size = risk_units, color = bond), alpha = 0.7) +
            geom_text_repel(aes(label = bond), size = 3) +
            scale_size_continuous(range = c(3, 10), name = "Risk Units") +
            scale_color_viridis_d(option = "turbo") +
            labs(title = "Risk-Return Profile",
                 x = "Yield Volatility (%)",
                 y = "Sharpe Ratio") +
            theme_insele() +
            theme(legend.position = "bottom")
    })

    output$var_analysis <- renderPlot({
        req(values$filtered_df)

        risk_metrics <- calculate_risk_metrics(values$filtered_df)

        var_data <- risk_metrics %>%
            select(bond, var_1d, var_5d, var_20d) %>%
            pivot_longer(cols = starts_with("var"),
                         names_to = "period", values_to = "var") %>%
            mutate(period = factor(period,
                                   levels = c("var_1d", "var_5d", "var_20d"),
                                   labels = c("1-Day", "5-Day", "20-Day")))

        ggplot(var_data, aes(x = bond, y = var * 100, fill = period)) +
            geom_col(position = "dodge") +
            scale_fill_manual(values = c(
                "1-Day" = insele_colors$green,
                "5-Day" = insele_colors$orange,
                "20-Day" = insele_colors$red
            ), name = "VaR Period") +
            labs(title = "Value at Risk Analysis (95% Confidence)",
                 x = "",
                 y = "VaR (% of notional)") +
            theme_insele() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    output$risk_metrics_table <- DT::renderDataTable({
        req(values$filtered_df)

        risk_metrics <- calculate_risk_metrics(values$filtered_df)

        DT::datatable(
            risk_metrics,
            options = list(pageLength = 15),
            rownames = FALSE
        ) %>%
            formatRound(c("yield_vol", "risk_units", "sharpe_ratio"), 2) %>%
            formatPercentage(c("var_1d", "var_5d", "var_20d"), 2) %>%
            formatStyle("sharpe_ratio",
                        backgroundColor = styleInterval(c(0, 1, 2),
                                                        c("lightcoral", "lightyellow", "lightgreen", "darkgreen"))
            )
    })

    # ================== TRADING SIGNALS OUTPUTS ==================

    output$trading_signals_table <- DT::renderDataTable({
        req(values$filtered_df)

        # Calculate liquidity and risk metrics
        liquidity_scores <- calculate_liquidity_score(values$filtered_df)
        risk_metrics <- calculate_risk_metrics(values$filtered_df)

        # Generate signals with custom weights
        signals <- generate_trading_signals(
            values$filtered_df,
            liquidity_data = liquidity_scores,
            risk_data = risk_metrics
        )

        DT::datatable(
            signals,
            options = list(
                pageLength = 20,
                order = list(list(ncol(signals) - 1, "asc"))
            ),
            rownames = FALSE
        ) %>%
            formatStyle("signal",
                        backgroundColor = styleEqual(
                            c("STRONG BUY", "BUY", "NEUTRAL", "SELL", "STRONG SELL"),
                            c("#2E7D32", "#66BB6A", "#E0E0E0", "#EF5350", "#C62828")
                        ),
                        color = styleEqual(
                            c("STRONG BUY", "BUY", "NEUTRAL", "SELL", "STRONG SELL"),
                            c("white", "white", "black", "white", "white")
                        ),
                        fontWeight = "bold"
            ) %>%
            formatStyle("confidence",
                        color = styleEqual(
                            c("High", "Medium", "Low"),
                            c("#2E7D32", "#F57C00", "#C62828")
                        ),
                        fontWeight = styleEqual(
                            c("High", "Medium", "Low"),
                            c("bold", "normal", "normal")
                        )
            ) %>%
            formatRound(c("yield_to_maturity", "modified_duration",
                          "composite_score", "liquidity_score"), 2)
    })

    # ================== DATA QUALITY OUTPUTS ==================

    output$data_quality <- renderValueBox({
        req(values$validated_df)
        completeness <- (1 - sum(is.na(values$validated_df)) /
                             (nrow(values$validated_df) * ncol(values$validated_df))) * 100

        valueBox(
            value = paste0(round(completeness, 1), "%"),
            subtitle = "Data Completeness",
            icon = icon("check-circle"),
            color = if(completeness > 95) "green" else if(completeness > 85) "yellow" else "red"
        )
    })

    output$last_update_box <- renderValueBox({
        req(values$last_update)

        valueBox(
            value = format(values$last_update, "%H:%M"),
            subtitle = paste("Updated", format(values$last_update, "%b %d")),
            icon = icon("clock"),
            color = "blue"
        )
    })

    # ================== REPORT GENERATION ==================

    observeEvent(input$generate_report, {
        showNotification("Generating report...", type = "message", duration = NULL, id = "report_gen")

        # Simulate report generation
        values$report_ready <- TRUE

        Sys.sleep(2)  # Simulate processing time

        removeNotification(id = "report_gen")
        showNotification("Report generated successfully!", type = "success", duration = 3)

        output$report_status <- renderUI({
            div(class = "alert alert-success",
                icon("check-circle"), " Report ready for download"
            )
        })
    })

    # Download handlers
    output$download_report <- downloadHandler(
        filename = function() {
            paste0("bond_report_", format(Sys.Date(), "%Y%m%d"),
                   switch(input$report_format,
                          "pdf" = ".pdf",
                          "excel" = ".xlsx",
                          "html" = ".html",
                          "pptx" = ".pptx"))
        },
        content = function(file) {
            # In production, generate actual report based on format
            # For now, create a simple data export
            if(input$report_format == "excel") {
                export_processed_data(values$filtered_df, file, include_metadata = TRUE)
            } else {
                write.csv(values$filtered_df, file, row.names = FALSE)
            }
        }
    )

    output$download_data <- downloadHandler(
        filename = function() {
            paste0("bond_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
            write.csv(values$filtered_df, file, row.names = FALSE)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)