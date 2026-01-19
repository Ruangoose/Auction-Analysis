# Insele Capital Partners - Institutional-Grade SA Government Bond Analytics Dashboard
# Bloomberg Terminal-Inspired Professional Fixed Income Analysis Platform

# Load comprehensive libraries
pacman::p_load(
    shiny, shinydashboard, shinyWidgets, shinycssloaders,
    tidyverse, ggplot2, lubridate, scales, readxl, splines, zoo,
    DT, rmarkdown, knitr, TTR, forecast, memoise,
    ggrepel, ggiraph, ggtext, shadowtext, patchwork,  # Enhanced visualization
    quantmod, PerformanceAnalytics,                    # Financial analytics
    plotly, htmlwidgets,                               # Interactive elements
    shinyjs, shinyBS,                                  # Enhanced UI
    future, promises                                    # Async processing
)

# Source the advanced processing functions
source("bond-dashboard/enhanced_bond_data_processor.R")

# Enable asynchronous processing
plan(multiprocess)

# ============================================================================
# ENHANCED UI WITH BLOOMBERG AESTHETIC
# ============================================================================

ui <- dashboardPage(
    skin = "black",  # Dark theme for Bloomberg look

    # Enhanced Header
    dashboardHeader(
        title = tags$div(
            style = "display: flex; align-items: center; width: 100%; height: 50px;",
            tags$img(src = "insele_logo.png", height = "40px", style = "margin-right: 15px;"),
            tags$div(
                tags$span("INSELE CAPITAL PARTNERS",
                          style = "font-weight: 600; font-size: 18px; color: #FFFFFF; letter-spacing: 1px;"),
                tags$br(),
                tags$span("Institutional Fixed Income Analytics | ",
                          style = "font-size: 11px; color: #B0BEC5;"),
                tags$span(id = "market_status",
                          style = "font-size: 11px; color: #4CAF50;")
            ),
            tags$div(
                style = "margin-left: auto; margin-right: 20px;",
                tags$span(id = "system_time",
                          style = "font-size: 12px; color: #B0BEC5; font-family: 'Consolas', monospace;"),
                tags$br(),
                tags$span(id = "data_quality_badge",
                          style = "font-size: 11px; padding: 2px 8px; border-radius: 3px;")
            )
        ),
        titleWidth = 600,

        # Enhanced notification panel
        tags$li(
            class = "dropdown",
            style = "margin-right: 10px;",
            tags$a(
                href = "#",
                class = "dropdown-toggle",
                `data-toggle` = "dropdown",
                tags$span(class = "badge badge-warning", id = "alert_count", "0"),
                tags$i(class = "fa fa-bell")
            ),
            tags$ul(
                class = "dropdown-menu dropdown-menu-lg",
                style = "width: 400px; max-height: 500px; overflow-y: auto;",
                tags$li(class = "header", "Market Alerts & Anomalies"),
                tags$li(
                    tags$ul(
                        class = "menu",
                        id = "notification_list"
                    )
                )
            )
        )
    ),

    # Enhanced Sidebar
    dashboardSidebar(
        width = 300,

        # Custom CSS for Bloomberg aesthetic
        tags$head(
            tags$style(HTML("
                /* Bloomberg Terminal Aesthetic */
                .skin-black .main-header .navbar {
                    background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
                    border-bottom: 2px solid #FF6B35;
                }

                .skin-black .main-sidebar {
                    background: linear-gradient(180deg, #0f0f1e 0%, #1a1a2e 100%);
                }

                .sidebar-menu > li > a {
                    color: #B0BEC5;
                    font-size: 12px;
                    letter-spacing: 0.5px;
                    border-left: 3px solid transparent;
                    transition: all 0.3s ease;
                }

                .sidebar-menu > li:hover > a,
                .sidebar-menu > li.active > a {
                    color: #FFFFFF;
                    background: rgba(255, 107, 53, 0.1);
                    border-left-color: #FF6B35;
                }

                .content-wrapper {
                    background: #0a0a14;
                }

                .box {
                    background: #1a1a2e;
                    border: 1px solid #2a2a3e;
                    border-radius: 4px;
                    box-shadow: 0 2px 8px rgba(0,0,0,0.3);
                }

                .box-header {
                    background: linear-gradient(90deg, #16213e 0%, #1a1a2e 100%);
                    color: #FFFFFF;
                    border-bottom: 1px solid #FF6B35;
                    padding: 8px 15px;
                }

                .box-title {
                    font-size: 13px;
                    font-weight: 600;
                    letter-spacing: 1px;
                    text-transform: uppercase;
                }

                .nav-tabs-custom > .nav-tabs {
                    background: #16213e;
                    border-bottom: 2px solid #FF6B35;
                }

                .nav-tabs-custom > .nav-tabs > li.active {
                    border-top-color: #4CAF50;
                }

                .nav-tabs-custom > .nav-tabs > li > a {
                    color: #B0BEC5;
                    font-size: 12px;
                    font-weight: 500;
                    letter-spacing: 0.5px;
                }

                .btn-primary {
                    background: linear-gradient(135deg, #003366 0%, #0066CC 100%);
                    border: 1px solid #0066CC;
                    font-size: 11px;
                    font-weight: 600;
                    letter-spacing: 0.5px;
                    text-transform: uppercase;
                }

                .btn-primary:hover {
                    background: linear-gradient(135deg, #0066CC 0%, #003366 100%);
                }

                .selectize-input {
                    background: #16213e !important;
                    border: 1px solid #2a2a3e !important;
                    color: #FFFFFF !important;
                    font-size: 11px;
                }

                .selectize-dropdown {
                    background: #1a1a2e !important;
                    border: 1px solid #2a2a3e !important;
                    color: #B0BEC5 !important;
                }

                .small-box {
                    background: linear-gradient(135deg, #16213e 0%, #1a1a2e 100%) !important;
                    border: 1px solid #2a2a3e;
                    border-radius: 4px;
                    color: #FFFFFF !important;
                }

                .small-box .icon {
                    color: rgba(255, 255, 255, 0.15) !important;
                }

                .info-box {
                    background: #1a1a2e;
                    border: 1px solid #2a2a3e;
                    color: #B0BEC5;
                }

                .info-box-icon {
                    background: linear-gradient(135deg, #FF6B35 0%, #F39B3C 100%) !important;
                }

                /* Data tables styling */
                .dataTables_wrapper {
                    color: #B0BEC5;
                    font-size: 11px;
                }

                .table-striped > tbody > tr:nth-of-type(odd) {
                    background-color: rgba(255, 255, 255, 0.02);
                }

                .table-bordered {
                    border: 1px solid #2a2a3e !important;
                }

                .table-bordered > thead > tr > th,
                .table-bordered > tbody > tr > td {
                    border: 1px solid #2a2a3e !important;
                    color: #B0BEC5;
                }

                /* Progress indicators */
                .progress {
                    background: #16213e;
                    border: 1px solid #2a2a3e;
                    height: 20px;
                }

                .progress-bar {
                    background: linear-gradient(90deg, #FF6B35 0%, #4CAF50 100%);
                }

                /* Tooltips */
                .tooltip-inner {
                    background: #1a1a2e;
                    border: 1px solid #FF6B35;
                    color: #FFFFFF;
                    font-size: 11px;
                    max-width: 300px;
                }

                /* Loading animation */
                .loading-spinner {
                    border: 3px solid #16213e;
                    border-top: 3px solid #FF6B35;
                    border-radius: 50%;
                    width: 40px;
                    height: 40px;
                    animation: spin 1s linear infinite;
                }

                @keyframes spin {
                    0% { transform: rotate(0deg); }
                    100% { transform: rotate(360deg); }
                }

                /* ════════════════════════════════════════════════════════════════════
                   CHART WIDTH FIX - Ensure all plots fill their containers
                   This prevents charts from being too narrow with empty space
                   ════════════════════════════════════════════════════════════════════ */
                .shiny-plot-output {
                    width: 100% !important;
                    max-width: 100% !important;
                }

                .shiny-plot-output img {
                    width: 100% !important;
                    max-width: 100% !important;
                }

                /* Ensure plotOutput containers expand */
                .box-body .shiny-plot-output,
                .panel-body .shiny-plot-output,
                .well .shiny-plot-output {
                    width: 100% !important;
                }

                /* Technical Analysis specific chart panels */
                .panel .shiny-plot-output {
                    width: 100% !important;
                }

                /* Skeleton screens */
                .skeleton {
                    background: linear-gradient(90deg, #1a1a2e 25%, #2a2a3e 50%, #1a1a2e 75%);
                    background-size: 200% 100%;
                    animation: loading 1.5s infinite;
                }

                @keyframes loading {
                    0% { background-position: 200% 0; }
                    100% { background-position: -200% 0; }
                }
            "))
        ),

        # Market Overview Section
        tags$div(
            style = "padding: 15px; border-bottom: 1px solid #2a2a3e;",
            h5("MARKET OVERVIEW", style = "color: #FF6B35; font-size: 11px; letter-spacing: 1px; margin-bottom: 10px;"),
            tags$div(id = "market_metrics",
                     tags$div(class = "info-box-mini",
                              tags$span("10Y Benchmark:", style = "color: #757575; font-size: 10px;"),
                              tags$span(id = "benchmark_yield", "-.--", style = "color: #FFFFFF; font-size: 14px; font-weight: bold;")
                     ),
                     tags$div(class = "info-box-mini",
                              tags$span("Curve Slope:", style = "color: #757575; font-size: 10px;"),
                              tags$span(id = "curve_slope", "---bps", style = "color: #FFFFFF; font-size: 14px; font-weight: bold;")
                     ),
                     tags$div(class = "info-box-mini",
                              tags$span("Market Regime:", style = "color: #757575; font-size: 10px;"),
                              tags$span(id = "market_regime", "----", style = "color: #4CAF50; font-size: 12px; font-weight: bold;")
                     )
            )
        ),

        # Enhanced Date Selection
        tags$div(
            style = "padding: 15px; border-bottom: 1px solid #2a2a3e;",
            h5("ANALYSIS PERIOD", style = "color: #FF6B35; font-size: 11px; letter-spacing: 1px; margin-bottom: 10px;"),

            dateRangeInput(
                "date_range",
                NULL,
                start = floor_date(today() - years(1), "year"),
                end = today(),
                format = "dd-M-yy",
                width = "100%"
            ),

            tags$div(
                style = "margin-top: 10px;",
                fluidRow(
                    column(3, actionButton("ytd_btn", "YTD",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;")),
                    column(3, actionButton("qtd_btn", "QTD",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;")),
                    column(3, actionButton("mtd_btn", "MTD",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;")),
                    column(3, actionButton("wtd_btn", "WTD",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;"))
                ),
                tags$br(),
                fluidRow(
                    column(4, actionButton("1m_btn", "1M",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;")),
                    column(4, actionButton("3m_btn", "3M",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;")),
                    column(4, actionButton("6m_btn", "6M",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;"))
                ),
                tags$br(),
                fluidRow(
                    column(4, actionButton("1y_btn", "1Y",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;")),
                    column(4, actionButton("2y_btn", "2Y",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;")),
                    column(4, actionButton("5y_btn", "5Y",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;"))
                )
            )
        ),

        # Bond Selection with Enhanced Filters
        tags$div(
            style = "padding: 15px; border-bottom: 1px solid #2a2a3e;",
            h5("BOND UNIVERSE", style = "color: #FF6B35; font-size: 11px; letter-spacing: 1px; margin-bottom: 10px;"),

            pickerInput(
                "selected_bonds",
                NULL,
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    `selected-text-format` = "count > 3",
                    `count-selected-text` = "{0} bonds selected",
                    `live-search` = TRUE,
                    `live-search-placeholder` = "Search bonds...",
                    `style` = "btn-sm",
                    `width` = "100%"
                )
            ),

            tags$div(
                style = "margin-top: 10px;",
                h6("QUICK FILTERS", style = "color: #757575; font-size: 10px; margin-bottom: 5px;"),
                fluidRow(
                    column(6, actionButton("select_liquid", "LIQUID",
                                           class = "btn-xs btn-warning",
                                           style = "width: 100%; font-size: 10px;")),
                    column(6, actionButton("select_benchmarks", "BENCHMARKS",
                                           class = "btn-xs btn-success",
                                           style = "width: 100%; font-size: 10px;"))
                ),
                tags$br(),
                fluidRow(
                    column(4, actionButton("select_short", "SHORT",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;")),
                    column(4, actionButton("select_medium", "MED",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;")),
                    column(4, actionButton("select_long", "LONG",
                                           class = "btn-xs",
                                           style = "width: 100%; font-size: 10px;"))
                )
            )
        ),

        # Advanced Analytics Options
        tags$div(
            style = "padding: 15px; border-bottom: 1px solid #2a2a3e;",
            h5("ANALYTICS SETTINGS", style = "color: #FF6B35; font-size: 11px; letter-spacing: 1px; margin-bottom: 10px;"),

            selectInput(
                "curve_model",
                "Curve Model:",
                choices = list(
                    "Nelson-Siegel-Svensson" = "nss",
                    "Smooth Spline" = "smooth.spline",
                    "Cubic Spline" = "cubic",
                    "LOESS" = "loess"
                ),
                selected = "nss",
                width = "100%"
            ),

            sliderInput(
                "confidence_level",
                "Confidence Level:",
                min = 90,
                max = 99,
                value = 95,
                step = 1,
                post = "%",
                width = "100%"
            ),

            checkboxGroupInput(
                "display_options",
                "Display Options:",
                choices = list(
                    "Show Confidence Bands" = "confidence",
                    "Show Anomalies" = "anomalies",
                    "Show Annotations" = "annotations",
                    "Show Grid" = "grid",
                    "Interactive Mode" = "interactive"
                ),
                selected = c("confidence", "annotations", "grid"),
                width = "100%"
            ),

            selectInput(
                "color_scheme",
                "Color Scheme:",
                choices = list(
                    "Bloomberg Terminal" = "bloomberg",
                    "Risk Heat Map" = "risk",
                    "Diverging Blue-Red" = "diverging",
                    "Viridis" = "viridis"
                ),
                selected = "bloomberg",
                width = "100%"
            )
        ),

        # System Controls
        tags$div(
            style = "padding: 15px;",
            h5("SYSTEM CONTROLS", style = "color: #FF6B35; font-size: 11px; letter-spacing: 1px; margin-bottom: 10px;"),

            actionButton("refresh_data", "↻ REFRESH DATA",
                         class = "btn-primary btn-sm",
                         style = "width: 100%; margin-bottom: 10px;"),

            actionButton("export_workspace", "⬇ EXPORT WORKSPACE",
                         class = "btn-info btn-sm",
                         style = "width: 100%; margin-bottom: 10px;"),

            actionButton("generate_report", "📊 GENERATE REPORT",
                         class = "btn-success btn-sm",
                         style = "width: 100%;")
        )
    ),

    # Enhanced Dashboard Body
    dashboardBody(
        # Enable shinyjs for enhanced interactivity
        useShinyjs(),

        # Include custom JavaScript for real-time updates
        tags$script(HTML("
            // Update system time every second
            setInterval(function() {
                var now = new Date();
                var timeString = now.toLocaleString('en-GB', {
                    day: '2-digit',
                    month: 'short',
                    year: 'numeric',
                    hour: '2-digit',
                    minute: '2-digit',
                    second: '2-digit',
                    hour12: false
                }).toUpperCase();
                $('#system_time').text(timeString);
            }, 1000);

            // Market status update
            setInterval(function() {
                var now = new Date();
                var hours = now.getHours();
                var status = (hours >= 9 && hours < 17) ? 'MARKET OPEN' : 'MARKET CLOSED';
                var color = (hours >= 9 && hours < 17) ? '#4CAF50' : '#F44336';
                $('#market_status').text(status).css('color', color);
            }, 1000);
        ")),

        # Top-level KPIs and Alerts
        fluidRow(
            # Market Intelligence Panel
            column(12,
                   box(
                       title = tags$span(icon("brain"), "MARKET INTELLIGENCE SYSTEM",
                                         style = "font-size: 12px; letter-spacing: 1px;"),
                       status = "primary",
                       solidHeader = TRUE,
                       width = 12,
                       collapsible = TRUE,

                       fluidRow(
                           column(8,
                                  tags$div(id = "intelligence_panel",
                                           withSpinner(
                                               uiOutput("market_intelligence_content"),
                                               type = 8,
                                               color = "#FF6B35",
                                               size = 0.5
                                           )
                                  )
                           ),
                           column(4,
                                  tags$div(
                                      h5("ANOMALY RADAR", style = "color: #FF6B35; font-size: 11px; margin-bottom: 10px;"),
                                      plotOutput("anomaly_radar", height = "150px")
                                  )
                           )
                       )
                   )
            )
        ),

        # Main KPI Cards
        fluidRow(
            valueBoxOutput("portfolio_value_box"),
            valueBoxOutput("daily_pnl_box"),
            valueBoxOutput("var_box"),
            valueBoxOutput("sharpe_ratio_box")
        ),

        # Main Analysis Tabs
        fluidRow(
            column(12,
                   tabBox(
                       id = "main_tabs",
                       width = 12,

                       # Yield Curve Analysis Tab
                       tabPanel(
                           tags$span(icon("chart-line"), "YIELD CURVE", style = "font-size: 11px;"),

                           fluidRow(
                               box(
                                   title = "TERM STRUCTURE ANALYSIS",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   height = "600px",

                                   tags$div(
                                       style = "position: relative;",

                                       # Control buttons overlay
                                       tags$div(
                                           style = "position: absolute; top: 10px; right: 10px; z-index: 1000;",
                                           actionButton("toggle_nss", "NSS",
                                                        class = "btn-xs",
                                                        style = "margin-right: 5px;"),
                                           actionButton("toggle_pca", "PCA",
                                                        class = "btn-xs",
                                                        style = "margin-right: 5px;"),
                                           actionButton("toggle_scenarios", "SCENARIOS",
                                                        class = "btn-xs",
                                                        style = "margin-right: 5px;"),
                                           downloadButton("download_curve", "⬇",
                                                          class = "btn-xs")
                                       ),

                                       withSpinner(
                                           ggiraphOutput("yield_curve_interactive", height = "500px"),
                                           type = 8,
                                           color = "#FF6B35",
                                           size = 0.5
                                       )
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "RELATIVE VALUE MATRIX",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   withSpinner(
                                       plotOutput("relative_value_heatmap", height = "400px"),
                                       type = 8,
                                       color = "#FF6B35"
                                   )
                               ),
                               box(
                                   title = "BUTTERFLY OPPORTUNITIES",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   withSpinner(
                                       plotOutput("butterfly_chart", height = "400px"),
                                       type = 8,
                                       color = "#FF6B35"
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "CURVE DYNAMICS",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("curve_metrics_table")
                               )
                           )
                       ),

                       # Market Microstructure Tab
                       tabPanel(
                           tags$span(icon("microscope"), "MICROSTRUCTURE", style = "font-size: 11px;"),

                           fluidRow(
                               box(
                                   title = "AUCTION DEPTH ANALYSIS",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   fluidRow(
                                       column(6,
                                              withSpinner(
                                                  plotOutput("auction_depth_chart", height = "400px"),
                                                  type = 8,
                                                  color = "#FF6B35"
                                              )
                                       ),
                                       column(6,
                                              withSpinner(
                                                  plotOutput("bid_distribution", height = "400px"),
                                                  type = 8,
                                                  color = "#FF6B35"
                                              )
                                       )
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "FLOW DYNAMICS",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 8,
                                   withSpinner(
                                       plotOutput("flow_treemap", height = "400px"),
                                       type = 8,
                                       color = "#FF6B35"
                                   )
                               ),
                               box(
                                   title = "AUCTION CALENDAR",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 4,
                                   withSpinner(
                                       uiOutput("auction_calendar"),
                                       type = 8,
                                       color = "#FF6B35"
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "MICROSTRUCTURE METRICS",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("microstructure_table")
                               )
                           )
                       ),

                       # Risk Analytics Tab
                       tabPanel(
                           tags$span(icon("shield-alt"), "RISK ANALYTICS", style = "font-size: 11px;"),

                           fluidRow(
                               box(
                                   title = "RISK DECOMPOSITION",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   fluidRow(
                                       column(4,
                                              h5("VALUE AT RISK", style = "color: #FF6B35; font-size: 11px;"),
                                              withSpinner(
                                                  plotOutput("var_chart", height = "300px"),
                                                  type = 8,
                                                  color = "#FF6B35"
                                              )
                                       ),
                                       column(4,
                                              h5("STRESS TESTING", style = "color: #FF6B35; font-size: 11px;"),
                                              withSpinner(
                                                  plotOutput("stress_test_chart", height = "300px"),
                                                  type = 8,
                                                  color = "#FF6B35"
                                              )
                                       ),
                                       column(4,
                                              h5("FACTOR ATTRIBUTION", style = "color: #FF6B35; font-size: 11px;"),
                                              withSpinner(
                                                  plotOutput("factor_attribution", height = "300px"),
                                                  type = 8,
                                                  color = "#FF6B35"
                                              )
                                       )
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "DURATION & CONVEXITY PROFILE",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   withSpinner(
                                       plotOutput("duration_ladder", height = "400px"),
                                       type = 8,
                                       color = "#FF6B35"
                                   )
                               ),
                               box(
                                   title = "CORRELATION STRUCTURE",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   withSpinner(
                                       plotOutput("correlation_network", height = "400px"),
                                       type = 8,
                                       color = "#FF6B35"
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "RISK METRICS DASHBOARD",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("risk_metrics_table")
                               )
                           )
                       ),

                       # Advanced Analytics Tab
                       tabPanel(
                           tags$span(icon("chart-bar"), "ADVANCED ANALYTICS", style = "font-size: 11px;"),

                           fluidRow(
                               box(
                                   title = "PRINCIPAL COMPONENT ANALYSIS",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   withSpinner(
                                       plotOutput("pca_loadings", height = "400px"),
                                       type = 8,
                                       color = "#FF6B35"
                                   )
                               ),
                               box(
                                   title = "REGIME ANALYSIS",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   withSpinner(
                                       plotOutput("regime_chart", height = "400px"),
                                       type = 8,
                                       color = "#FF6B35"
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "CARRY & ROLL OPTIMIZER",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   fluidRow(
                                       column(8,
                                              withSpinner(
                                                  plotOutput("carry_roll_surface", height = "400px"),
                                                  type = 8,
                                                  color = "#FF6B35"
                                              )
                                       ),
                                       column(4,
                                              h5("OPTIMIZATION PARAMETERS", style = "color: #FF6B35; font-size: 11px;"),
                                              numericInput("target_return", "Target Return (%):",
                                                           value = 8, min = 0, max = 20, step = 0.5),
                                              numericInput("max_duration", "Max Duration:",
                                                           value = 10, min = 1, max = 20, step = 0.5),
                                              selectInput("optimization_method", "Method:",
                                                          choices = list(
                                                              "Mean-Variance" = "mv",
                                                              "Black-Litterman" = "bl",
                                                              "Risk Parity" = "rp",
                                                              "Maximum Sharpe" = "ms"
                                                          ),
                                                          selected = "mv"),
                                              actionButton("run_optimization", "OPTIMIZE",
                                                           class = "btn-success btn-sm",
                                                           style = "width: 100%; margin-top: 10px;")
                                       )
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "ADVANCED METRICS",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("advanced_metrics_table")
                               )
                           )
                       ),

                       # Reporting Tab
                       tabPanel(
                           tags$span(icon("file-pdf"), "REPORTING", style = "font-size: 11px;"),

                           fluidRow(
                               box(
                                   title = "REPORT CONFIGURATION",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   fluidRow(
                                       column(6,
                                              h5("REPORT PARAMETERS", style = "color: #FF6B35; font-size: 11px;"),

                                              selectInput("report_type", "Report Type:",
                                                          choices = list(
                                                              "Daily Trading Report" = "daily",
                                                              "Weekly Analysis" = "weekly",
                                                              "Monthly Performance" = "monthly",
                                                              "Risk Report" = "risk",
                                                              "Custom Report" = "custom"
                                                          ),
                                                          selected = "daily"),

                                              dateInput("report_date", "Report Date:",
                                                        value = today()),

                                              textInput("report_recipient", "Recipients:",
                                                        placeholder = "email@example.com"),

                                              checkboxGroupInput("report_sections", "Include Sections:",
                                                                 choices = list(
                                                                     "Executive Summary" = "exec",
                                                                     "Market Overview" = "market",
                                                                     "Yield Curve Analysis" = "curve",
                                                                     "Relative Value" = "rv",
                                                                     "Auction Analysis" = "auction",
                                                                     "Risk Metrics" = "risk",
                                                                     "Technical Analysis" = "tech",
                                                                     "Recommendations" = "reco"
                                                                 ),
                                                                 selected = c("exec", "market", "curve", "rv"))
                                       ),
                                       column(6,
                                              h5("REPORT PREVIEW", style = "color: #FF6B35; font-size: 11px;"),

                                              tags$div(
                                                  id = "report_preview",
                                                  style = "border: 1px solid #2a2a3e; padding: 15px; height: 400px; overflow-y: auto;",
                                                  withSpinner(
                                                      uiOutput("report_preview_content"),
                                                      type = 8,
                                                      color = "#FF6B35"
                                                  )
                                              )
                                       )
                                   ),

                                   tags$hr(),

                                   fluidRow(
                                       column(12,
                                              actionButton("generate_report_btn", "GENERATE REPORT",
                                                           class = "btn-success btn-lg",
                                                           style = "width: 100%;"),
                                              tags$br(), tags$br(),
                                              downloadButton("download_report_btn", "DOWNLOAD REPORT",
                                                             class = "btn-primary btn-lg",
                                                             style = "width: 100%;")
                                       )
                                   )
                               )
                           )
                       )
                   )
            )
        ),

        # Footer with system status
        tags$footer(
            style = "background: #16213e; color: #B0BEC5; padding: 10px; text-align: center; margin-top: 20px;",
            tags$span("© 2025 Insele Capital Partners | "),
            tags$span(id = "connection_status", "● CONNECTED", style = "color: #4CAF50;"),
            tags$span(" | Data Quality: "),
            tags$span(id = "footer_quality", style = "color: #FFA000;"),
            tags$span(" | Last Update: "),
            tags$span(id = "last_update", style = "font-family: 'Consolas', monospace;")
        )
    )
)