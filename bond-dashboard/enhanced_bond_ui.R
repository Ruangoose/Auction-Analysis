# ================================================================================
# ENHANCED UI DEFINITION
# ================================================================================

# Create base64 logo string (with flexible path resolution)
logo_base64 <- NULL
logo_paths <- c(
    "www/logo.png",                          # Standard Shiny www directory
    "bond-dashboard/www/logo.png",           # Old structure
    "logo.png",                              # Same directory
    file.path("assets", "logo.png")          # Assets directory
)

for (logo_path in logo_paths) {
    if (file.exists(logo_path)) {
        tryCatch({
            logo_base64 <- base64enc::dataURI(file = logo_path, mime = "image/png")
            message(sprintf("✓ Logo loaded from: %s", logo_path))
            break
        }, error = function(e) {
            warning(sprintf("Failed to load logo from %s: %s", logo_path, e$message))
        })
    }
}

if (is.null(logo_base64)) {
    message("ℹ No logo found - continuing without logo")
}

ui <- dashboardPage(
    dashboardHeader(
        title = tags$div(
            style = "display: flex; align-items: center; width: 100%; padding: 5px 0;",
            # Direct base64 embed
            if(!is.null(logo_base64)) {
                tags$img(src = logo_base64, height = "35px",
                         style = "margin-right: 10px; filter: drop-shadow(0 2px 4px rgba(0,0,0,0.1));")
            } else {
                NULL
            },
            tags$span("INSELE CAPITAL PARTNERS",
                      style = "font-weight: 600; font-size: 18px; color: white; letter-spacing: 0.5px;"),
            tags$span("The Power of Partnership",
                      style = "color: #FFD700; margin-left: 15px; font-style: italic; font-size: 14px;")
        ),
        titleWidth = 500,

        # Enhanced notification dropdown
        dropdownMenu(
            type = "notifications",
            badgeStatus = "warning",
            icon = icon("bell"),
            .list = list(
                tags$li(
                    tags$div(
                        id = "live_metrics",
                        style = "padding: 10px; min-width: 300px;",
                        uiOutput("live_market_metrics")
                    )
                )
            )
        )
    ),

    dashboardSidebar(
        width = 300,

        # Add shinyjs for enhanced interactivity
        shinyjs::useShinyjs(),

        tags$head(
            tags$style(HTML(paste0("
        .sidebar {
          background: linear-gradient(180deg, ", insele_palette$primary, " 0%, ",
                                   insele_palette$primary_dark, " 100%) !important;
        }
        .sidebar a {
          color: white !important;
        }
        .sidebar .form-control {
          background-color: rgba(255, 255, 255, 0.1);
          border: 1px solid rgba(255, 255, 255, 0.2);
          color: white;
        }
        .sidebar .selectize-input {
          background-color: rgba(255, 255, 255, 0.1) !important;
          border: 1px solid rgba(255, 255, 255, 0.2) !important;
          color: white !important;
        }
        .sidebar .btn {
          background-color: ", insele_palette$accent, ";
          border: none;
          transition: all 0.3s ease;
        }
        .sidebar .btn:hover {
          background-color: ", insele_palette$accent_dark, ";
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
        .sidebar .btn:active {
          transform: translateY(0px);
          box-shadow: 0 2px 4px rgba(0,0,0,0.2);
        }
        .sidebar .btn:disabled {
          opacity: 0.5;
          cursor: not-allowed;
        }

        /* Fix for bond selection dropdown visibility */
        .bootstrap-select .dropdown-menu {
          background-color: white !important;
        }

        .bootstrap-select .dropdown-menu li a {
          color: #333 !important;
          background-color: white !important;
        }

        .bootstrap-select .dropdown-menu li a:hover {
          background-color: #f5f5f5 !important;
          color: #1B3A6B !important;
        }

        .bootstrap-select .dropdown-menu li.selected a {
          background-color: #e8f4f8 !important;
          color: #1B3A6B !important;
          font-weight: 600;
        }

        .bootstrap-select .dropdown-menu .bs-searchbox input {
          color: #333 !important;
          background-color: #f8f9fa !important;
          border: 1px solid #ddd !important;
        }
      ")))
        ),

        # Analysis Parameters Section
        tags$div(
            style = "padding: 15px;",
            tags$div(
                style = paste0("background: rgba(255,255,255,0.1); border-radius: 10px; ",
                               "padding: 15px; margin-bottom: 20px;"),
                h4("Analysis Parameters",
                   style = "color: white; margin-top: 0; font-weight: 600;"),

                # Enhanced date range input with presets
                dateRangeInput(
                    "date_range",
                    "Date Range:",
                    start = floor_date(today() - years(1), "year"),
                    end = today(),
                    format = "dd M yyyy",
                    width = "100%"
                ),

                tags$div(
                    style = "margin-top: 10px;",
                    fluidRow(
                        column(3, tags$div(
                            title = "Year to date",
                            actionButton("ytd_btn", "YTD",
                                         class = "btn-sm", width = "100%")
                        )),
                        column(3, tags$div(
                            title = "Current quarter",
                            actionButton("qtr_btn", "QTR",
                                         class = "btn-sm", width = "100%")
                        )),
                        column(3, tags$div(
                            title = "Last 1 month",
                            actionButton("mth_btn", "1M",
                                         class = "btn-sm", width = "100%")
                        )),
                        column(3, tags$div(
                            title = "Last 1 year",
                            actionButton("yr_btn", "1Y",
                                         class = "btn-sm", width = "100%")
                        ))
                    ),
                    fluidRow(
                        style = "margin-top: 5px;",
                        column(6, tags$div(
                            title = "Month to date",
                            actionButton("mtd_btn", "MTD",
                                         class = "btn-sm", width = "100%")
                        )),
                        column(6, tags$div(
                            title = "Select custom date range with more options",
                            actionButton("custom_range_btn", "Custom",
                                         class = "btn-sm", width = "100%",
                                         icon = icon("calendar-alt"))
                        ))
                    )
                )
            )
        ),

        # Bond Selection Section
        tags$div(
            style = "padding: 15px;",
            tags$div(
                style = paste0("background: rgba(255,255,255,0.1); border-radius: 10px; ",
                               "padding: 15px; margin-bottom: 20px;"),
                h4("Bond Selection",
                   style = "color: white; margin-top: 0; font-weight: 600;"),

                pickerInput(
                    "selected_bonds",
                    NULL,
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(
                        `actions-box` = TRUE,
                        `selected-text-format` = "count > 3",
                        `count-selected-text` = "{0} bonds",
                        `live-search` = TRUE,
                        `style` = "btn-outline-light"
                    )
                ),

                fluidRow(
                    column(4, tags$div(
                        title = "Select bonds with duration < 5 years",
                        actionButton("select_short", "Short",
                                     class = "btn-sm", width = "100%")
                    )),
                    column(4, tags$div(
                        title = "Select bonds with duration 5-10 years",
                        actionButton("select_medium", "Med",
                                     class = "btn-sm", width = "100%")
                    )),
                    column(4, tags$div(
                        title = "Select bonds with duration > 10 years",
                        actionButton("select_long", "Long",
                                     class = "btn-sm", width = "100%")
                    ))
                )
            )
        ),

        # Advanced Options Section
        tags$div(
            style = "padding: 15px;",
            tags$div(
                style = paste0("background: rgba(255,255,255,0.1); border-radius: 10px; ",
                               "padding: 15px; margin-bottom: 20px;"),
                h4("Advanced Options",
                   style = "color: white; margin-top: 0; font-weight: 600;"),

                selectInput(
                    "curve_model",
                    "Curve Model:",
                    choices = list(
                        "Smooth Spline" = "spline",
                        "Nelson-Siegel-Svensson" = "nss",
                        "Cubic Spline" = "cubic",
                        "LOESS" = "loess"
                    ),
                    selected = "nss",
                    width = "100%"
                ),

                selectInput(
                    "risk_measure",
                    "Risk Metric:",
                    choices = list(
                        "Modified Duration" = "mod_dur",
                        "DV01" = "dv01",
                        "Convexity-Adjusted" = "conv_adj",
                        "Key Rate Duration" = "key_rate"
                    ),
                    selected = "mod_dur",
                    width = "100%"
                ),

                # ADD THIS - Lookback period for historical calculations
                sliderInput(
                    "lookback_days",
                    "Lookback Period:",
                    min = 20,
                    max = 120,
                    value = 60,
                    step = 10,
                    post = " days",
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

                br(),
                actionButton(
                    "reset_defaults",
                    "Reset to Defaults",
                    icon = icon("undo"),
                    class = "btn-sm",
                    width = "100%",
                    style = "margin-top: 10px;"
                )
            )
        ),

        # Export Section
        tags$div(
            style = "padding: 15px;",
            tags$div(
                style = paste0("background: rgba(255,255,255,0.1); border-radius: 10px; ",
                               "padding: 15px;"),
                h4("Export Options",
                   style = "color: white; margin-top: 0; font-weight: 600;"),

                tags$div(
                    title = "Export current data to Excel format",
                    downloadButton("export_data", "Export Data",
                                   class = "btn btn-light", width = "100%")
                ),
                br(), br(),
                tags$div(
                    title = "Send current analysis via email",
                    actionButton("generate_email_report", "Email Report",
                                 class = "btn btn-light", width = "100%",
                                 icon = icon("envelope"))
                )
            )
        )
    ),

    dashboardBody(
        # Enhanced CSS with animations and sophisticated styling
        tags$head(
            tags$style(HTML(paste0("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');

        * {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        }

        .content-wrapper, .right-side {
          background: linear-gradient(135deg, ", insele_palette$gradient_start, " 0%, white 100%);
          min-height: 100vh;
        }

        .box {
          border-radius: 12px;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.07), 0 0 0 1px rgba(0, 0, 0, 0.04);
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          border: none;
          overflow: hidden;
        }

        .box:hover {
          box-shadow: 0 10px 20px rgba(0, 0, 0, 0.1), 0 0 0 1px rgba(0, 0, 0, 0.04);
          transform: translateY(-2px);
        }

        .box-header {
          background: linear-gradient(135deg, ", insele_palette$primary, " 0%, ",
                                   insele_palette$primary_light, " 100%);
          color: white;
          border: none;
          padding: 15px 20px;
          font-weight: 600;
        }

        .box-header .box-title {
          color: white;
          font-size: 16px;
          font-weight: 600;
        }

        .nav-tabs-custom > .nav-tabs {
          background: transparent;
          border-bottom: 2px solid ", insele_palette$light_gray, ";
        }

        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: ", insele_palette$accent, ";
          border-top-width: 3px;
        }

        .nav-tabs-custom > .nav-tabs > li > a {
          color: ", insele_palette$medium_gray, ";
          font-weight: 500;
          transition: all 0.3s ease;
        }

        .nav-tabs-custom > .nav-tabs > li.active > a {
          color: ", insele_palette$primary, ";
          font-weight: 600;
        }

        .small-box {
          border-radius: 12px;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.07);
          transition: all 0.3s ease;
          position: relative;
          overflow: hidden;
        }

        .small-box::before {
          content: '';
          position: absolute;
          top: 0;
          right: 0;
          width: 100%;
          height: 100%;
          background: linear-gradient(135deg, transparent 0%, rgba(255,255,255,0.1) 100%);
          pointer-events: none;
        }

        .small-box:hover {
          transform: translateY(-5px);
          box-shadow: 0 8px 16px rgba(0, 0, 0, 0.12);
        }

        .market-intelligence-box {
          background: linear-gradient(135deg, ", insele_palette$primary, " 0%, ",
                                   insele_palette$secondary, " 100%);
          color: white;
          border-radius: 16px;
          padding: 25px;
          margin-bottom: 25px;
          box-shadow: 0 10px 30px rgba(27, 58, 107, 0.3);
          position: relative;
          overflow: hidden;
        }

        .market-intelligence-box::before {
          content: '';
          position: absolute;
          top: -50%;
          right: -50%;
          width: 200%;
          height: 200%;
          background: radial-gradient(circle, rgba(255,255,255,0.1) 0%, transparent 70%);
          animation: pulse 4s ease-in-out infinite;
        }

        @keyframes pulse {
          0%, 100% { transform: scale(1); opacity: 0.5; }
          50% { transform: scale(1.1); opacity: 0.3; }
        }

        .insight-item {
          background: rgba(255, 255, 255, 0.15);
          backdrop-filter: blur(10px);
          border: 1px solid rgba(255, 255, 255, 0.2);
          border-radius: 10px;
          padding: 12px 16px;
          margin: 8px 0;
          display: flex;
          align-items: center;
          transition: all 0.3s ease;
          animation: slideIn 0.5s ease-out;
        }

        @keyframes slideIn {
          from {
            opacity: 0;
            transform: translateX(-20px);
          }
          to {
            opacity: 1;
            transform: translateX(0);
          }
        }

        .insight-item:hover {
          background: rgba(255, 255, 255, 0.25);
          transform: translateX(5px);
        }

        .insight-category {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 12px;
          background: rgba(255, 255, 255, 0.2);
          font-size: 11px;
          font-weight: 600;
          margin-left: auto;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }

        .loading-spinner {
          display: inline-block;
          width: 20px;
          height: 20px;
          border: 3px solid rgba(255,255,255,0.3);
          border-radius: 50%;
          border-top-color: ", insele_palette$accent, ";
          animation: spin 1s ease-in-out infinite;
        }

        @keyframes spin {
          to { transform: rotate(360deg); }
        }

        /* DataTable styling */
        .dataTables_wrapper {
          font-size: 14px;
        }

        table.dataTable thead {
          background: linear-gradient(135deg, ", insele_palette$primary, " 0%, ",
                                   insele_palette$primary_light, " 100%);
          color: white;
        }

        table.dataTable tbody tr:hover {
          background-color: ", insele_palette$gradient_start, " !important;
        }

        /* Tooltips */
        .tooltip-inner {
          background-color: ", insele_palette$dark_gray, ";
          border-radius: 6px;
          padding: 8px 12px;
          font-size: 13px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }

        /* Value boxes enhancement */
        .small-box .inner h3 {
          font-size: 32px;
          font-weight: 700;
          margin: 0 0 5px 0;
        }

        .small-box .inner p {
          font-size: 14px;
          font-weight: 500;
          opacity: 0.9;
        }

        .small-box .icon {
          opacity: 0.3;
          font-size: 70px;
        }

        /* Enhanced button styling */
        .btn {
          transition: all 0.2s ease-in-out;
        }
        .btn:hover:not(:disabled) {
          transform: translateY(-1px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.15);
        }
        .btn:active:not(:disabled) {
          transform: translateY(0px);
        }
        .btn:disabled {
          opacity: 0.6;
          cursor: not-allowed;
        }

        /* Loading state for buttons */
        .btn-loading {
          position: relative;
          pointer-events: none;
        }
        .btn-loading::after {
          content: '';
          position: absolute;
          width: 16px;
          height: 16px;
          top: 50%;
          left: 50%;
          margin-left: -8px;
          margin-top: -8px;
          border: 2px solid #ffffff;
          border-radius: 50%;
          border-top-color: transparent;
          animation: spin 0.6s linear infinite;
        }

        /* Tooltip improvements */
        [title]:hover::after {
          content: attr(title);
          position: absolute;
          bottom: 100%;
          left: 50%;
          transform: translateX(-50%);
          background: #333;
          color: white;
          padding: 5px 10px;
          border-radius: 4px;
          font-size: 12px;
          white-space: nowrap;
          z-index: 1000;
        }
      ")))
        ),

        # Add waiter for loading screens
        useWaiter(),
        waiterPreloader(color = insele_palette$primary),

        # Market Intelligence Dashboard
        fluidRow(
            column(12,
                   div(class = "market-intelligence-box",
                       h3("Market Intelligence Dashboard",
                          style = "margin-top: 0; margin-bottom: 20px; font-weight: 700; font-size: 24px;"),
                       uiOutput("advanced_insights_content")
                   )
            )
        ),

        # Enhanced Key Metrics Row
        fluidRow(
            valueBoxOutput("market_regime_box"),
            valueBoxOutput("portfolio_var_box"),
            valueBoxOutput("best_carry_box"),
            valueBoxOutput("signal_strength_box")
        ),

        # Main tabbed interface with enhanced styling
        fluidRow(
            column(12,
                   tabBox(
                       id = "main_tabs",
                       width = 12,

                       # Enhanced Relative Value Tab
                       tabPanel(
                           "Relative Value Analytics",
                           icon = icon("chart-line"),

                           fluidRow(
                               box(
                                   title = "Yield Curve & Fair Value Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   collapsible = TRUE,
                                   fluidRow(
                                       column(3,
                                              selectInput("xaxis_choice", "X-Axis:",
                                                          choices = list(
                                                              "Modified Duration" = "modified_duration",
                                                              "Duration" = "duration",
                                                              "Time to Maturity" = "time_to_maturity"
                                                          ),
                                                          selected = "modified_duration",
                                                          width = "100%"
                                              )
                                       )
                                   ),
                                   fluidRow(
                                       column(9,
                                              withSpinner(
                                                  plotOutput("enhanced_yield_curve", height = "500px"),
                                                  type = 4,
                                                  color = insele_palette$primary
                                              )
                                       ),
                                       column(3,
                                              tags$div(
                                                  style = "background: #f8f9fa; border-radius: 8px; padding: 15px; margin-top: 20px;",
                                                  h5("Curve Metrics", style = "margin-top: 0; color: #1B3A6B;"),
                                                  uiOutput("curve_metrics_summary")
                                              )
                                       )
                                   ),

                                   fluidRow(
                                       column(12,
                                              tags$div(
                                                  style = "margin-top: 15px;",
                                                  downloadButton("download_yield_curve", "Export Chart",
                                                                 class = "btn-sm btn-primary"),
                                                  actionButton("show_curve_settings", "Advanced Settings",
                                                               class = "btn-sm btn-default",
                                                               icon = icon("cog"))
                                              )
                                       )
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Relative Value Heatmap",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("relative_value_heatmap", height = "400px"),
                                   # === ADDED: download_relative_value_heatmap ===
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_relative_value_heatmap", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               ),
                               box(
                                   title = "Z-Score Distribution",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("enhanced_zscore_plot", height = "400px"),
                                   # === ADDED: download_enhanced_zscore ===
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_enhanced_zscore", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               )
                           ),

                           # Spread warning (conditionally rendered)
                           fluidRow(
                               column(12,
                                      uiOutput("spread_warning")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Relative Value Opportunities",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   # Filter controls row
                                   fluidRow(
                                       column(4,
                                              selectInput(
                                                  inputId = "rv_table_filter",
                                                  label = "Show Signals:",
                                                  choices = c(
                                                      "All Bonds" = "all",
                                                      "Actionable Only (Buy/Sell)" = "actionable",
                                                      "Strong Signals Only" = "strong"
                                                  ),
                                                  selected = "all"
                                              )
                                       ),
                                       column(8,
                                              tags$div(
                                                  class = "pull-right small text-muted",
                                                  style = "margin-top: 25px;",
                                                  tags$strong("Score: "),
                                                  "0-10 scale combining spread magnitude (0-4), Z-Score confirmation (0-4), and liquidity (0-2)."
                                              )
                                       )
                                   ),

                                   # Table
                                   DT::dataTableOutput("relative_value_opportunities")
                               )
                           )
                       ),

                       # Risk Analytics Tab
                       tabPanel(
                           "Risk Analytics",
                           icon = icon("shield-alt"),

                           # Understanding VaR Info Box
                           fluidRow(
                               column(12,
                                      tags$div(
                                          class = "info-box",
                                          style = "background-color: #1B3A6B; color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;",

                                          tags$h4(icon("shield-alt"), " Understanding Value-at-Risk (VaR)", style = "margin-top: 0;"),

                                          tags$p("VaR measures the potential loss in ", tags$strong("bond price"),
                                                 " (in basis points) that could occur on the worst trading days:"),

                                          tags$ul(
                                              tags$li(tags$strong("95% VaR (e.g., 350 bps):"), " On 95% of days, losses will be less than 3.5%. ",
                                                      tags$em("Expect to exceed this ~1 day per month.")),
                                              tags$li(tags$strong("99% VaR (e.g., 500 bps):"), " On 99% of days, losses will be less than 5.0%. ",
                                                      tags$em("Expect to exceed this ~2-3 days per year.")),
                                              tags$li(tags$strong("CVaR (Expected Shortfall):"), " When VaR IS breached, this is the ",
                                                      tags$em("average"), " loss you can expect.")
                                          ),

                                          tags$p(
                                              tags$strong("Key Insight: "),
                                              "Compare 99% VaR to 95% VaR. A large gap indicates ", tags$strong("fat tails"),
                                              " - the bond has more extreme risk than a normal distribution would suggest."
                                          )
                                      )
                               )
                           ),

                           # Reading the Risk Ladder Info Box
                           fluidRow(
                               column(12,
                                      tags$div(
                                          class = "info-box",
                                          style = "background-color: #FF9800; color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;",

                                          tags$h5(icon("chart-bar"), " Reading the Risk Metrics", style = "margin-top: 0;"),

                                          tags$p(tags$strong("VaR Bars:")),
                                          tags$ul(
                                              tags$li(tags$strong("Wide (orange) bar:"), " 95% VaR - exceeded ~1 day per month"),
                                              tags$li(tags$strong("Narrow (red) bar:"), " 99% VaR - exceeded ~2-3 days per year"),
                                              tags$li(tags$strong("Blue diamond:"), " CVaR - average loss when VaR is breached")
                                          ),

                                          tags$p(tags$strong("Distribution Statistics:")),
                                          tags$ul(
                                              tags$li(tags$strong("Skewness < -0.3:"), " Left tail risk - larger losses more likely than gains"),
                                              tags$li(tags$strong("Excess Kurtosis > 1:"), " Fat tails - extreme moves more likely than normal (Normal distribution = 0)"),
                                              tags$li(tags$strong("Tail Ratio (99%/95% > 1.5):"), " Significant tail risk present")
                                          ),

                                          tags$p(
                                              style = "margin-top: 10px; font-style: italic;",
                                              HTML("&#9888; Values shown as positive losses for clarity. Watch for bonds with high kurtosis AND negative skew.")
                                          )
                                      )
                               )
                           ),

                           # VaR Control Panel
                           fluidRow(
                               column(12,
                                      tags$div(
                                          class = "well",
                                          style = "background-color: #F5F5F5; margin-bottom: 15px; padding: 15px; border-radius: 5px;",

                                          fluidRow(
                                              column(3,
                                                     selectInput(
                                                         "var_horizon",
                                                         "VaR Horizon:",
                                                         choices = c("1 Day" = 1, "5 Days" = 5, "10 Days" = 10, "21 Days (1 Month)" = 21),
                                                         selected = 1
                                                     )
                                              ),

                                              column(3,
                                                     sliderInput(
                                                         "var_lookback",
                                                         "Lookback Period (days):",
                                                         min = 60, max = 504, value = 252, step = 21,
                                                         post = " days"
                                                     )
                                              ),

                                              column(3,
                                                     selectInput(
                                                         "var_method",
                                                         "Calculation Method:",
                                                         choices = c(
                                                             "Historical Simulation" = "historical",
                                                             "Parametric (Normal)" = "parametric",
                                                             "Cornish-Fisher" = "cornish-fisher"
                                                         ),
                                                         selected = "historical"
                                                     )
                                              ),

                                              column(3,
                                                     checkboxInput(
                                                         "var_show_stats_table",
                                                         "Show Statistics Table",
                                                         value = TRUE
                                                     )
                                              )
                                          )
                                      )
                               )
                           ),

                           # VaR Plots Box
                           fluidRow(
                               box(
                                   title = "Value-at-Risk Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   fluidRow(
                                       column(6,
                                              plotOutput("var_distribution_plot", height = "450px")
                                       ),
                                       column(6,
                                              plotOutput("var_ladder_plot", height = "450px")
                                       )
                                   ),

                                   # Download buttons
                                   fluidRow(
                                       column(12,
                                              tags$div(
                                                  style = "margin-top: 15px;",
                                                  downloadButton("download_var_distribution", "Download VaR Distribution",
                                                                 class = "btn-sm btn-primary"),
                                                  downloadButton("download_var_ladder", "Download VaR Ladder",
                                                                 class = "btn-sm btn-primary",
                                                                 style = "margin-left: 10px;")
                                              )
                                       )
                                   )
                               )
                           ),

                           # VaR Statistics Table (conditional)
                           fluidRow(
                               conditionalPanel(
                                   condition = "input.var_show_stats_table == true",
                                   box(
                                       title = "VaR Statistics Summary",
                                       status = "info",
                                       solidHeader = TRUE,
                                       width = 12,
                                       DT::dataTableOutput("var_statistics_table")
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "DV01 Ladder",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("dv01_ladder_plot", height = "400px"),
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_dv01_ladder", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               ),
                               box(
                                   title = "Convexity Profile",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("enhanced_convexity_plot", height = "400px"),
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_convexity", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Risk Metrics Summary",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("risk_metrics_table")
                               )
                           )
                       ),

                       # Technical Analysis Tab
                       tabPanel(
                           "Technical Analysis",
                           icon = icon("chart-area"),

                           fluidRow(
                               box(
                                   title = "Advanced Technical Indicators",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   fluidRow(
                                       column(3,
                                              selectInput("tech_bond_select", "Select Bond:",
                                                          choices = NULL,
                                                          width = "100%")
                                       ),
                                       column(3,
                                              selectInput("tech_indicator_type", "Indicator Type:",
                                                          choices = list(
                                                              "Momentum" = "momentum",
                                                              "Volatility" = "volatility",
                                                              "Mean Reversion" = "mean_reversion",
                                                              "All" = "all"
                                                          ),
                                                          selected = "all",
                                                          width = "100%")
                                       ),
                                       column(6,
                                              tags$div(
                                                  style = "padding: 8px; background: #e8f4f8; border-radius: 6px;",
                                                  uiOutput("technical_indicators_summary")
                                              )
                                       )
                                   ),

                                   plotOutput("advanced_technical_plot", height = "500px"),

                                   # ADD THIS LINE
                                   downloadButton("download_technical_plot", "Download Chart", class = "btn-sm btn-primary")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Trading Signal Matrix",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotOutput("signal_matrix_heatmap", height = "400px"),
                                   # === ADDED: download_signal_matrix ===
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_signal_matrix", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Technical Indicators Summary",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("technical_indicators_enhanced_table")
                               )
                           )
                       ),

                       # Carry & Roll Analytics Tab
                       tabPanel(
                           "Carry & Roll Analytics",
                           icon = icon("calculator"),

                           # ADD THIS: Information/Explanation Panel
                           fluidRow(
                               column(12,
                                      tags$div(
                                          class = "alert alert-info",
                                          style = "margin-bottom: 20px;",
                                          tags$h4("Understanding Carry & Roll Analysis", style = "margin-top: 0;"),
                                          tags$p(
                                              tags$strong("What This Analysis Shows:"),
                                              "This table calculates the expected total return from holding SA government bonds over different time periods (30, 90, 180, and 360 days), broken down into component parts."
                                          ),
                                          tags$div(
                                              style = "margin-top: 15px;",
                                              tags$h5("Return Components:", style = "color: #1B3A6B;"),
                                              tags$ul(
                                                  tags$li(tags$strong("Carry Income:"),
                                                          "The coupon income earned during the holding period. For a 10% coupon bond held 90 days: 10% × (90/365) = 2.47%"),
                                                  tags$li(tags$strong("Roll-Down Return:"),
                                                          "The price appreciation as the bond 'rolls down' the yield curve. As time passes and duration decreases, bonds typically move to lower-yielding parts of the curve, generating capital gains."),
                                                  tags$li(tags$strong("Funding Cost:"),
                                                          "The cost to finance the position, typically at the repo rate (currently around 8.25% annually)."),
                                                  tags$li(tags$strong("Net Return:"),
                                                          "Total return after subtracting funding costs: (Carry + Roll) - Funding")
                                              )
                                          ),
                                          tags$div(
                                              style = "margin-top: 15px;",
                                              tags$h5("How to Read the Results:", style = "color: #1B3A6B;"),
                                              tags$ul(
                                                  tags$li(tags$strong("Green cells:"), "Positive returns - bond generates profit after funding costs"),
                                                  tags$li(tags$strong("Red cells:"), "Negative returns - funding costs exceed income"),
                                                  tags$li(tags$strong("Return Type Options:"),
                                                          tags$ul(
                                                              tags$li("Gross: Total return before funding costs"),
                                                              tags$li("Net: Return after funding costs (most realistic)"),
                                                              tags$li("Risk-Adjusted: Return per unit of duration risk")
                                                          ))
                                              )
                                          ),
                                          tags$div(
                                              style = "margin-top: 15px; padding: 10px; background: #1B3A6B; border-radius: 5px;",
                                              tags$strong("💡 Key Insight:"),
                                              "Bonds with coupons above the funding rate generate positive carry. Longer holding periods typically show higher returns due to compound effects and roll-down benefits."
                                          )
                                      )
                               )
                           ),

                           # Main Carry & Roll Box
                           fluidRow(
                               box(
                                   title = "Carry & Roll Optimization",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   collapsible = TRUE,

                                   fluidRow(
                                       column(8,
                                              plotOutput("enhanced_carry_roll_heatmap", height = "450px")
                                       ),
                                       column(4,
                                              tags$div(
                                                  style = "background: #f8f9fa; border-radius: 8px; padding: 15px;",
                                                  h5("Optimization Parameters", style = "color: #1B3A6B; margin-top: 0;"),

                                                  # Funding rate input with helper text
                                                  tags$div(
                                                      sliderInput("funding_rate", "Funding Rate:",
                                                                  min = 5, max = 12, value = 8.25,
                                                                  step = 0.25, post = "%"),
                                                      tags$small("Current repo rate: ~8.25%",
                                                                 style = "color: #666; display: block; margin-top: -10px; margin-bottom: 15px;")
                                                  ),

                                                  # Return type selector with descriptions
                                                  tags$div(
                                                      radioButtons("return_type", "Return Type:",
                                                                   choices = list(
                                                                       "Gross Return" = "gross",
                                                                       "Net Return" = "net",
                                                                       "Risk-Adjusted" = "risk_adj"
                                                                   ),
                                                                   selected = "net"),
                                                      tags$small("Net return is most realistic for trading decisions",
                                                                 style = "color: #666; display: block; margin-top: -10px; margin-bottom: 15px;")
                                                  ),

                                                  # Recalculate button
                                                  actionButton("recalculate_carry", "Recalculate",
                                                               class = "btn-primary btn-block",
                                                               icon = icon("refresh")),

                                                  hr(),

                                                  # Quick summary metrics
                                                  tags$div(
                                                      style = "margin-top: 15px;",
                                                      h6("Quick Metrics", style = "color: #1B3A6B;"),
                                                      uiOutput("carry_roll_summary")
                                                  )
                                              )
                                       )
                                   ),

                                   # Download button
                                   fluidRow(
                                       column(12,
                                              tags$div(
                                                  style = "margin-top: 15px;",
                                                  downloadButton("download_carry_roll", "Download Chart",
                                                                 class = "btn-sm btn-primary"),
                                                  actionButton("show_carry_methodology", "View Methodology",
                                                               class = "btn-sm btn-info",
                                                               icon = icon("info-circle"),
                                                               style = "margin-left: 10px;")
                                              )
                                       )
                                   )
                               )
                           ),

                           # Scenario and Optimization boxes remain the same
                           fluidRow(
                               box(
                                   title = "Scenario Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("scenario_analysis_plot", height = "400px"),
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_scenario_analysis", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               ),
                               box(
                                   title = "Optimal Holding Period Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("optimal_holding_enhanced_plot", height = "400px"),
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_optimal_holding", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Forward Rate Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   collapsible = TRUE,

                                   # Add visualization
                                   plotOutput("forward_curve_plot", height = "350px"),

                                   # Add download button for the plot
                                   tags$div(
                                       style = "margin: 15px 0;",
                                       downloadButton("download_forward_curve", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   ),

                                   # Data table
                                   DT::dataTableOutput("forward_rate_table")
                               )
                           )
                       ),

                       # Auction Intelligence Tab
                       tabPanel(
                           "Auction Intelligence",
                           icon = icon("gavel"),

                           fluidRow(
                               box(
                                   title = "ML-Powered Auction Predictions",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   fluidRow(
                                       column(4,
                                              tags$div(
                                                  style = "background: #f8f9fa; border-radius: 8px; padding: 15px;",
                                                  h4("Prediction Configuration", style = "color: #1B3A6B;"),

                                                  # Bond selection for next auction
                                                  pickerInput(
                                                      "auction_bonds_select",
                                                      "Select Bonds for Next Auction:",
                                                      choices = NULL,  # Will be populated dynamically
                                                      selected = NULL,
                                                      multiple = TRUE,
                                                      options = list(
                                                          `actions-box` = TRUE,
                                                          `selected-text-format` = "count > 2",
                                                          `count-selected-text` = "{0} bonds selected",
                                                          `live-search` = TRUE
                                                      )
                                                  ),

                                                  # Auction parameters
                                                  dateInput(
                                                      "next_auction_date",
                                                      "Expected Auction Date:",
                                                      value = today() + days(7),
                                                      min = today() + days(1)
                                                  ),

                                                  numericInput(
                                                      "expected_offer_size",
                                                      "Expected Offer (R millions):",
                                                      value = 5000,
                                                      min = 100,
                                                      max = 20000,
                                                      step = 500
                                                  ),

                                                  selectInput(
                                                      "prediction_model",
                                                      "Prediction Model:",
                                                      choices = list(
                                                          "ARIMA (Auto)" = "arima_auto",
                                                          "Machine Learning Ensemble" = "ml_ensemble",
                                                          "Neural Network" = "neural_net",
                                                          "Hybrid Model" = "hybrid"
                                                      ),
                                                      selected = "arima_auto"
                                                  ),

                                                  hr(),

                                                  tags$div(
                                                      title = "Recalculate auction predictions with current parameters",
                                                      actionButton(
                                                          "update_predictions",
                                                          "Update Predictions",
                                                          class = "btn-primary btn-block",
                                                          icon = icon("sync-alt")
                                                      )
                                                  ),

                                                  hr(),

                                                  h5("Model Performance", style = "color: #1B3A6B; margin-top: 15px;"),
                                                  uiOutput("model_performance_metrics")
                                              )
                                       ),
                                       column(4,
                                              tags$div(
                                                  style = "background: #f8f9fa; border-radius: 8px; padding: 15px;",
                                                  h4("Auction Forecasts", style = "color: #1B3A6B;"),
                                                  uiOutput("ml_auction_predictions"),
                                                  hr(),
                                                  h5("Market Sentiment", style = "color: #1B3A6B;"),
                                                  plotOutput("auction_sentiment_gauge", height = "200px"),
                                                  # === ADDED: download_auction_sentiment ===
                                                  tags$div(
                                                      style = "margin-top: 10px;",
                                                      downloadButton("download_auction_sentiment", "Download Sentiment",
                                                                     class = "btn-sm btn-primary")
                                                  )
                                              )
                                       ),
                                       column(4,
                                              tags$div(
                                                  style = "background: white; border-radius: 8px; padding: 15px;",
                                                  plotOutput("auction_forecast_plot", height = "400px")
                                              )
                                       )
                                   ),

                                   fluidRow(
                                       column(12,
                                              tags$div(style = "margin-top: 20px;"),
                                              plotOutput("prediction_confidence_plot", height = "250px")
                                       )
                                   )
                               )
                           ),

                           # Additional sophisticated analytics
                           fluidRow(
                               box(
                                   title = "Advanced Auction Analytics",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   collapsible = TRUE,

                                   fluidRow(
                                       column(6,
                                              h4("Demand Elasticity Analysis", style = "color: #1B3A6B;"),
                                              plotOutput("demand_elasticity_plot", height = "350px")
                                       ),
                                       column(6,
                                              h4("Auction Success Probability", style = "color: #1B3A6B;"),
                                              plotOutput("success_probability_plot", height = "350px")
                                       )
                                   ),
                                   # === ADDED: download_demand_elasticity and download_success_probability ===
                                   fluidRow(
                                       column(12,
                                              tags$div(
                                                  style = "margin-top: 15px;",
                                                  downloadButton("download_demand_elasticity", "Download Elasticity Chart",
                                                                 class = "btn-sm btn-primary",
                                                                 style = "margin-right: 10px;"),
                                                  downloadButton("download_success_probability", "Download Probability Chart",
                                                                 class = "btn-sm btn-primary")
                                              )
                                       )
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Historical Pattern Recognition",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("auction_pattern_analysis", height = "400px"),
                                   # === ADDED: download_auction_pattern ===
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_auction_pattern", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               ),
                               box(
                                   title = "Bid Distribution Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("bid_distribution_plot", height = "400px"),
                                   # === ADDED: download_bid_distribution ===
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_bid_distribution", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Auction Performance Analytics",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   style = "overflow-y: auto; max-height: 650px;",  # Add this line
                                   plotOutput("enhanced_auction_analytics", height = "650px"),
                                   # Add this download button
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_auction_performance", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               )
                           ),


                           # Cumulative Bond Issuance Section
                           fluidRow(
                               box(
                                   title = "Cumulative Government Bond Issuance",
                                   status = "success",
                                   solidHeader = TRUE,
                                   width = 12,

                                   # Main chart
                                   plotOutput("ytd_bond_issuance_chart", height = "500px"),

                                   # Download button
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_ytd_issuance_chart", "Download Chart",
                                                      class = "btn-sm btn-success",
                                                      style = "margin-right: 10px;"),
                                       downloadButton("download_ytd_issuance_table", "Download Table (CSV)",
                                                      class = "btn-sm btn-info")
                                   )
                               )
                           ),

                           # Data Table for YTD Issuance
                           fluidRow(
                               box(
                                   title = "Issuance Details by Bond",
                                   status = "info",
                                   solidHeader = TRUE,
                                   width = 12,
                                   collapsible = TRUE,
                                   collapsed = FALSE,

                                   DT::dataTableOutput("ytd_issuance_data_table")
                               )
                           ),
                           fluidRow(
                               box(
                                   title = "Auction Success Factors",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("auction_success_factors", height = "400px"),
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_auction_success", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               ),
                               box(
                                   title = "Bid-to-Cover Decomposition",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("btc_decomposition", height = "400px"),
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_btc_decomposition", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               )
                           )
                       ),

                       # Market Intelligence Tab
                       tabPanel(
                           "Market Intelligence",
                           icon = icon("brain"),

                           fluidRow(
                               box(
                                   title = "Market Regime Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   fluidRow(
                                       column(8,
                                              plotOutput("regime_analysis_plot", height = "450px"),
                                              # ADD THE DOWNLOAD BUTTON HERE
                                              tags$div(
                                                  style = "margin-top: 15px;",
                                                  downloadButton("download_regime_analysis", "Download Chart",
                                                                 class = "btn-sm btn-primary")
                                              )
                                       ),
                                       column(4,
                                              tags$div(
                                                  style = "background: #f8f9fa; border-radius: 8px; padding: 15px;",
                                                  h5("Current Regime", style = "color: #1B3A6B;"),
                                                  uiOutput("regime_summary"),
                                                  hr(),
                                                  h5("Regime Probabilities", style = "color: #1B3A6B;"),
                                                  plotOutput("regime_probability_gauge", height = "200px")
                                              )
                                       )
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Cross-Asset Correlations",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("enhanced_correlation_plot", height = "400px"),
                                   # === ADDED: download_correlation ===
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_correlation", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               ),
                               box(
                                   title = "Term Structure Dynamics",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("term_structure_3d", height = "400px"),
                                   # === ADDED: download_term_structure ===
                                   tags$div(
                                       style = "margin-top: 15px;",
                                       downloadButton("download_term_structure", "Download Chart",
                                                      class = "btn-sm btn-primary")
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Market Microstructure",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("market_microstructure_table")
                               )
                           )
                       ),

                       # Treasury Holdings Tab (NEW)
                       tabPanel(
                           "Treasury Holdings",
                           icon = icon("building-columns"),
                           treasury_holdings_ui("treasury_module")
                       ),

                       # Report Generation Tab
                       tabPanel(
                           "Reports & Export",
                           icon = icon("file-pdf"),

                           fluidRow(
                               box(
                                   title = "Professional Report Generation",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   fluidRow(
                                       column(6,
                                              tags$div(
                                                  style = "background: #f8f9fa; border-radius: 8px; padding: 20px;",
                                                  h4("Report Configuration", style = "color: #1B3A6B;"),

                                                  selectInput("report_type", "Report Type:",
                                                              choices = list(
                                                                  "Executive Summary" = "executive",
                                                                  "Trading Desk Report" = "trading",
                                                                  "Risk Committee Report" = "risk",
                                                                  "Client Portfolio Review" = "client",
                                                                  "Treasury Holdings Report" = "treasury",
                                                                  "Custom Report" = "custom"
                                                              ),
                                                              selected = "executive"),

                                                  textInput("report_title", "Report Title:",
                                                            value = paste("SA Government Bond Analysis -",
                                                                          format(today(), "%B %Y"))),

                                                  textInput("client_name", "Client/Recipient:",
                                                            placeholder = "Enter client or recipient name"),

                                                  dateInput("report_date", "Report Date:",
                                                            value = today()),

                                                  tags$hr(),

                                                  # Enhanced section/plot selector with nested checkboxes
                                                  tags$div(
                                                      id = "section_plot_selector",
                                                      style = "max-height: 600px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 5px;",

                                                      # ═══════════════════════════════════════════════════════════
                                                      # MARKET OVERVIEW SECTION
                                                      # ═══════════════════════════════════════════════════════════
                                                      tags$div(
                                                          class = "section-container",
                                                          style = "margin-bottom: 10px; border: 2px solid #1B3A6B; border-radius: 5px; padding: 10px; background: white;",
                                                          checkboxInput("section_overview",
                                                                        tags$b(style = "color: #1B3A6B; font-size: 14px;", HTML("&#128202; Market Overview")),
                                                                        value = TRUE),
                                                          conditionalPanel(
                                                              condition = "input.section_overview == true",
                                                              style = "margin-left: 20px; margin-top: 10px;",
                                                              checkboxInput("plot_regime_plot", "Regime Analysis Plot", value = TRUE)
                                                          )
                                                      ),

                                                      # ═══════════════════════════════════════════════════════════
                                                      # RELATIVE VALUE SECTION
                                                      # ═══════════════════════════════════════════════════════════
                                                      tags$div(
                                                          class = "section-container",
                                                          style = "margin-bottom: 10px; border: 2px solid #1B3A6B; border-radius: 5px; padding: 10px; background: white;",
                                                          checkboxInput("section_relative",
                                                                        tags$b(style = "color: #1B3A6B; font-size: 14px;", HTML("&#128200; Relative Value Analysis")),
                                                                        value = TRUE),
                                                          conditionalPanel(
                                                              condition = "input.section_relative == true",
                                                              style = "margin-left: 20px; margin-top: 10px;",
                                                              checkboxInput("plot_yield_curve", "Yield Curve", value = TRUE),
                                                              checkboxInput("plot_relative_heatmap", "Relative Value Heatmap", value = TRUE),
                                                              checkboxInput("plot_zscore_plot", "Z-Score Distribution", value = TRUE),
                                                              checkboxInput("plot_convexity", HTML("&#10024; Enhanced Convexity Plot"), value = FALSE)
                                                          )
                                                      ),

                                                      # ═══════════════════════════════════════════════════════════
                                                      # RISK ANALYTICS SECTION
                                                      # ═══════════════════════════════════════════════════════════
                                                      tags$div(
                                                          class = "section-container",
                                                          style = "margin-bottom: 10px; border: 2px solid #1B3A6B; border-radius: 5px; padding: 10px; background: white;",
                                                          checkboxInput("section_risk",
                                                                        tags$b(style = "color: #1B3A6B; font-size: 14px;", HTML("&#9888; Risk Analytics")),
                                                                        value = TRUE),
                                                          conditionalPanel(
                                                              condition = "input.section_risk == true",
                                                              style = "margin-left: 20px; margin-top: 10px;",
                                                              checkboxInput("plot_var_distribution", "VaR Distribution", value = TRUE),
                                                              checkboxInput("plot_var_ladder", "VaR Ladder", value = TRUE),
                                                              checkboxInput("plot_dv01_ladder", "DV01 Analysis", value = TRUE)
                                                          )
                                                      ),

                                                      # ═══════════════════════════════════════════════════════════
                                                      # TECHNICAL ANALYSIS SECTION
                                                      # ═══════════════════════════════════════════════════════════
                                                      tags$div(
                                                          class = "section-container",
                                                          style = "margin-bottom: 10px; border: 2px solid #1B3A6B; border-radius: 5px; padding: 10px; background: white;",
                                                          checkboxInput("section_technical",
                                                                        tags$b(style = "color: #1B3A6B; font-size: 14px;", HTML("&#128201; Technical Analysis")),
                                                                        value = FALSE),
                                                          conditionalPanel(
                                                              condition = "input.section_technical == true",
                                                              style = "margin-left: 20px; margin-top: 10px;",
                                                              checkboxInput("plot_technical_plot", "Technical Indicators (RSI, MACD, Bollinger)", value = TRUE),
                                                              checkboxInput("plot_signal_matrix", "Signal Matrix Heatmap", value = TRUE)
                                                          )
                                                      ),

                                                      # ═══════════════════════════════════════════════════════════
                                                      # CARRY & ROLL SECTION
                                                      # ═══════════════════════════════════════════════════════════
                                                      tags$div(
                                                          class = "section-container",
                                                          style = "margin-bottom: 10px; border: 2px solid #1B3A6B; border-radius: 5px; padding: 10px; background: white;",
                                                          checkboxInput("section_carry",
                                                                        tags$b(style = "color: #1B3A6B; font-size: 14px;", HTML("&#128176; Carry & Roll Analysis")),
                                                                        value = FALSE),
                                                          conditionalPanel(
                                                              condition = "input.section_carry == true",
                                                              style = "margin-left: 20px; margin-top: 10px;",
                                                              checkboxInput("plot_carry_heatmap", "Carry & Roll Heatmap", value = TRUE),
                                                              checkboxInput("plot_scenario_analysis", "Scenario Analysis (Rate Shocks)", value = TRUE),
                                                              checkboxInput("plot_optimal_holding", HTML("&#10024; Optimal Holding Period"), value = FALSE),
                                                              checkboxInput("plot_forward_curve", HTML("&#10024; Forward Curve Analysis"), value = FALSE)
                                                          )
                                                      ),

                                                      # ═══════════════════════════════════════════════════════════
                                                      # AUCTION ANALYTICS SECTION (EXPANDED)
                                                      # ═══════════════════════════════════════════════════════════
                                                      tags$div(
                                                          class = "section-container",
                                                          style = "margin-bottom: 10px; border: 2px solid #1B3A6B; border-radius: 5px; padding: 10px; background: white;",
                                                          checkboxInput("section_auction",
                                                                        tags$b(style = "color: #1B3A6B; font-size: 14px;", HTML("&#128296; Auction Analytics")),
                                                                        value = FALSE),
                                                          conditionalPanel(
                                                              condition = "input.section_auction == true",
                                                              style = "margin-left: 20px; margin-top: 10px;",

                                                              # Existing plots
                                                              checkboxInput("plot_auction_performance", "Enhanced Auction Analytics", value = TRUE),
                                                              checkboxInput("plot_auction_patterns", "Historical Auction Patterns", value = TRUE),

                                                              # NEW plots
                                                              tags$hr(style = "margin: 10px 0; border-color: #ddd;"),
                                                              tags$div(style = "color: #666; font-size: 12px; margin-bottom: 5px;",
                                                                       tags$em(HTML("&#10024; Additional Auction Analytics:"))),
                                                              checkboxInput("plot_auction_forecast", HTML("&#10024; Auction Forecast (ARIMA)"), value = FALSE),
                                                              checkboxInput("plot_demand_elasticity", HTML("&#10024; Demand Elasticity"), value = FALSE),
                                                              checkboxInput("plot_success_probability", HTML("&#10024; Success Probability"), value = FALSE),
                                                              checkboxInput("plot_bid_distribution", HTML("&#10024; Bid Distribution"), value = FALSE),
                                                              checkboxInput("plot_ytd_issuance", HTML("&#10024; YTD Bond Issuance Chart"), value = FALSE),
                                                              checkboxInput("plot_auction_sentiment", HTML("&#10024; Auction Sentiment Gauge"), value = FALSE),
                                                              checkboxInput("plot_auction_success_factors", HTML("&#10024; Auction Success Factors"), value = FALSE),
                                                              checkboxInput("plot_btc_decomposition", HTML("&#10024; Bid-to-Cover Decomposition"), value = FALSE)
                                                          )
                                                      ),

                                                      # ═══════════════════════════════════════════════════════════
                                                      # MARKET INTELLIGENCE SECTION
                                                      # ═══════════════════════════════════════════════════════════
                                                      tags$div(
                                                          class = "section-container",
                                                          style = "margin-bottom: 10px; border: 2px solid #1B3A6B; border-radius: 5px; padding: 10px; background: white;",
                                                          checkboxInput("section_intelligence",
                                                                        tags$b(style = "color: #1B3A6B; font-size: 14px;", HTML("&#129504; Market Intelligence")),
                                                                        value = FALSE),
                                                          conditionalPanel(
                                                              condition = "input.section_intelligence == true",
                                                              style = "margin-left: 20px; margin-top: 10px;",
                                                              checkboxInput("plot_correlation", "Cross-Bond Correlation Matrix", value = TRUE),
                                                              checkboxInput("plot_term_structure", "3D Term Structure Evolution", value = TRUE)
                                                          )
                                                      ),

                                                      # ═══════════════════════════════════════════════════════════
                                                      # TREASURY HOLDINGS SECTION
                                                      # ═══════════════════════════════════════════════════════════
                                                      tags$div(
                                                          class = "section-container",
                                                          style = "margin-bottom: 10px; border: 2px solid #1B3A6B; border-radius: 5px; padding: 10px; background: white;",
                                                          checkboxInput("section_treasury",
                                                                        tags$b(style = "color: #1B3A6B; font-size: 14px;", HTML("&#127974; Treasury Holdings")),
                                                                        value = FALSE),
                                                          conditionalPanel(
                                                              condition = "input.section_treasury == true",
                                                              style = "margin-left: 20px; margin-top: 10px;",
                                                              checkboxInput("plot_holdings_area", "Holdings Time Series (Area)", value = TRUE),
                                                              checkboxInput("plot_sector_trend", "Single Sector Trend", value = TRUE),
                                                              checkboxInput("plot_holdings_fixed", "Fixed Rate Holdings", value = TRUE),
                                                              checkboxInput("plot_holdings_ilb", "ILB Holdings", value = TRUE),
                                                              checkboxInput("plot_holdings_frn", "FRN Holdings", value = FALSE),
                                                              checkboxInput("plot_holdings_sukuk", "Sukuk Holdings", value = FALSE),
                                                              checkboxInput("plot_ownership_changes", "Ownership Changes", value = TRUE),
                                                              checkboxInput("plot_holdings_diverging_fixed", "Fixed Rate Changes (Diverging)", value = FALSE),
                                                              checkboxInput("plot_holdings_diverging_ilb", "ILB Changes (Diverging)", value = FALSE)
                                                          )
                                                      ),

                                                      # ═══════════════════════════════════════════════════════════
                                                      # RECOMMENDATIONS SECTION
                                                      # ═══════════════════════════════════════════════════════════
                                                      tags$div(
                                                          class = "section-container",
                                                          style = "margin-bottom: 10px; border: 2px solid #1B3A6B; border-radius: 5px; padding: 10px; background: white;",
                                                          checkboxInput("section_recommendations",
                                                                        tags$b(style = "color: #1B3A6B; font-size: 14px;", HTML("&#128161; Trading Recommendations")),
                                                                        value = TRUE),
                                                          tags$div(
                                                              style = "margin-left: 20px; margin-top: 5px; font-size: 12px; color: #666;",
                                                              tags$em("(Text-based AI-generated insights, no charts)")
                                                          )
                                                      ),

                                                      # ═══════════════════════════════════════════════════════════
                                                      # SUMMARY INFO
                                                      # ═══════════════════════════════════════════════════════════
                                                      tags$hr(style = "margin: 15px 0;"),
                                                      tags$div(
                                                          style = "background: #e8f4f8; padding: 10px; border-radius: 5px; border-left: 4px solid #1B3A6B;",
                                                          tags$strong(style = "color: #1B3A6B;", HTML("&#128204; Total Available: 35 Plots")),
                                                          tags$br(),
                                                          tags$small(style = "color: #666;", HTML("&#10024; = Newly added to report generation")),
                                                          tags$br(),
                                                          tags$small(style = "color: #666;", HTML("&#127974; = Treasury Holdings section"))
                                                      )
                                                  )
                                              )
                                       ),
                                       column(6,
                                              tags$div(
                                                  style = "background: #f8f9fa; border-radius: 8px; padding: 20px;",
                                                  h4("Report Preview", style = "color: #1B3A6B;"),

                                                  uiOutput("report_preview_content"),

                                                  tags$hr(),

                                                  fluidRow(
                                                      column(6,
                                                             downloadButton("generate_pdf_report",
                                                                            "Generate PDF",
                                                                            class = "btn-primary btn-block")
                                                      ),
                                                      column(6,
                                                             downloadButton("generate_html_report",
                                                                            "Generate HTML",
                                                                            class = "btn-success btn-block")
                                                      )
                                                  ),

                                                  tags$div(style = "margin-top: 15px;"),

                                                  actionButton("email_report", "Email Report",
                                                               class = "btn-info btn-block",
                                                               icon = icon("envelope")),

                                                  tags$div(style = "margin-top: 15px;"),

                                                  actionButton("schedule_report", "Schedule Reports",
                                                               class = "btn-warning btn-block",
                                                               icon = icon("clock"))
                                              )
                                       )
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Data Export",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   fluidRow(
                                       column(3,
                                              tags$div(
                                                  title = "Export all data and charts to Excel workbook",
                                                  downloadButton("export_excel", "Export to Excel",
                                                                 class = "btn-success btn-block")
                                              )
                                       ),
                                       column(3,
                                              tags$div(
                                                  title = "Export data to CSV format for spreadsheet applications",
                                                  downloadButton("export_csv", "Export to CSV",
                                                                 class = "btn-info btn-block")
                                              )
                                       ),
                                       column(3,
                                              tags$div(
                                                  title = "Export data in JSON format for APIs and databases",
                                                  downloadButton("export_json", "Export to JSON",
                                                                 class = "btn-warning btn-block")
                                              )
                                       ),
                                       column(3,
                                              tags$div(
                                                  title = "Export in Bloomberg-compatible format",
                                                  downloadButton("export_bloomberg", "Bloomberg Format",
                                                                 class = "btn-danger btn-block")
                                              )
                                       )
                                   ),

                                   tags$div(style = "margin-top: 20px;"),

                                   tags$div(
                                       style = "background: #e8f4f8; border-radius: 8px; padding: 15px;",
                                       h5("Export Settings", style = "color: #1B3A6B;"),
                                       checkboxInput("include_metadata", "Include Metadata", value = TRUE),
                                       checkboxInput("include_calculations", "Include Calculations", value = TRUE),
                                       checkboxInput("include_charts", "Include Charts", value = FALSE)
                                   )
                               )
                           )
                       )
                   )
            )
        )
    )
)