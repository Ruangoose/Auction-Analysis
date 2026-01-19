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
            # FIX: Improved header styling to prevent tagline truncation
            # Added flex-shrink: 0 to prevent compression, and responsive handling
            style = "display: flex; align-items: center; width: 100%; padding: 5px 0; overflow: visible;",
            # Direct base64 embed
            if(!is.null(logo_base64)) {
                tags$img(src = logo_base64, height = "35px",
                         style = "margin-right: 10px; filter: drop-shadow(0 2px 4px rgba(0,0,0,0.1)); flex-shrink: 0;")
            } else {
                NULL
            },
            tags$span("INSELE CAPITAL PARTNERS",
                      style = "font-weight: 600; font-size: 18px; color: white; letter-spacing: 0.5px; white-space: nowrap; flex-shrink: 0;"),
            # FIX: Tagline with proper overflow handling and full visibility
            tags$span("The Power of Partnership",
                      class = "header-tagline",
                      style = "color: #FFD700; margin-left: 15px; font-style: italic; font-size: 14px; white-space: nowrap; flex-shrink: 1; min-width: 0;")
        ),
        titleWidth = 550,  # Increased from 500 to accommodate full tagline

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

        /* ════════════════════════════════════════════════════════════════════════
           FIX: Header tagline responsive handling
           Ensures tagline is fully visible on larger screens, hidden on small
           ════════════════════════════════════════════════════════════════════════ */
        .header-tagline {
          display: inline-block;
          overflow: visible;
          text-overflow: clip;
        }

        /* On screens smaller than 1200px, hide the tagline to prevent truncation */
        @media (max-width: 1200px) {
          .header-tagline {
            display: none !important;
          }
        }

        /* Ensure header logo area has enough width */
        .main-header .logo {
          overflow: visible !important;
        }

        .main-header .navbar-brand {
          overflow: visible !important;
        }

        /* ════════════════════════════════════════════════════════════════════════
           Butterfly Spread Analyzer Table Styling
           ════════════════════════════════════════════════════════════════════════ */

        /* Row selection highlighting */
        #butterfly_table tbody tr.selected {
          background-color: #E3F2FD !important;
          border-left: 4px solid #1B3A6B;
        }

        /* Hover effect for clickable rows */
        #butterfly_table tbody tr:hover {
          background-color: #F5F5F5 !important;
          cursor: pointer;
        }

        /* Filter input styling */
        #butterfly_table_filter input {
          border: 1px solid #dee2e6;
          border-radius: 4px;
          padding: 5px 10px;
        }

        /* Column header tooltips styling */
        #butterfly_table th[title] {
          cursor: help;
        }

        /* Summary card styling */
        .butterfly-summary-card {
          transition: box-shadow 0.2s ease;
        }

        .butterfly-summary-card:hover {
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
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

                # Exclude bonds maturing within analysis period
                checkboxInput(
                    inputId = "exclude_maturing_bonds",
                    label = "Exclude bonds maturing in period",
                    value = FALSE,
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
                                                      "Actionable (|Z| \u2265 1.0)" = "actionable",
                                                      "Strong Only (|Z| \u2265 2.0)" = "strong"
                                                  ),
                                                  selected = "all"
                                              )
                                       ),
                                       column(8,
                                              tags$div(
                                                  class = "pull-right small text-muted",
                                                  style = "margin-top: 25px;",
                                                  tags$strong("Signals: "),
                                                  "|Z| \u2265 2.0 = Strong, \u2265 1.5 = Regular, \u2265 1.0 = Weak. ",
                                                  tags$strong("Score: "),
                                                  "Spread (0-4) + Z-Score (0-4) + Liquidity (0-2) shown as X.X (a+b+c)."
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

                                          tags$p(tags$strong("Tail Risk Classification:")),
                                          tags$ul(
                                              tags$li(tags$strong("High:"), " Multiple risk factors - fat tails (>3.1) AND left skew (<-0.15) or extreme tail ratio (>1.45)"),
                                              tags$li(tags$strong("Elevated:"), " Top 25% within current bond universe for composite tail risk"),
                                              tags$li(tags$strong("Moderate:"), " Middle range (40-75th percentile) - monitor but not concerning"),
                                              tags$li(tags$strong("Low:"), " Below average tail risk relative to peers (bottom 40%)")
                                          ),

                                          tags$p(tags$strong("Key Metrics:")),
                                          tags$ul(
                                              tags$li(tags$strong("Skewness < -0.3:"), " Left tail risk - larger losses more likely"),
                                              tags$li(tags$strong("Excess Kurtosis:"), " SA bonds typically 2.5-3.5 (all fat-tailed vs normal dist)"),
                                              tags$li(tags$strong("Tail Ratio:"), " 99%/95% VaR ratio (normal = 1.41)")
                                          ),

                                          tags$p(
                                              class = "small",
                                              style = "margin-top: 10px; font-style: italic;",
                                              HTML("&#9888; Classification is relative to current SA government bond universe, not absolute thresholds.")
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

                                   # Warning displayed when bonds are excluded
                                   uiOutput("var_exclusion_warning"),

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

                           # Notional Input Control
                           fluidRow(
                               column(12,
                                   tags$div(
                                       style = "background: #E3F2FD; padding: 10px 15px; border-radius: 5px; margin-bottom: 15px; display: flex; align-items: center;",
                                       tags$span(
                                           style = "font-weight: bold; color: #1B3A6B; margin-right: 15px;",
                                           "Portfolio Notional:"
                                       ),
                                       selectInput(
                                           "dv01_notional",
                                           NULL,
                                           choices = c(
                                               "R10 million" = 10000000,
                                               "R50 million" = 50000000,
                                               "R100 million" = 100000000,
                                               "R500 million" = 500000000,
                                               "R1 billion" = 1000000000
                                           ),
                                           selected = 10000000,
                                           width = "200px"
                                       ),
                                       tags$small(
                                           style = "color: #666; margin-left: 15px;",
                                           "DV01 = Notional × Modified Duration × 0.01%"
                                       )
                                   )
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "DV01 Risk Ladder",
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

                                   # VaR Units Explanation Panel
                                   tags$div(
                                       class = "panel panel-default",
                                       style = "margin-bottom: 15px;",
                                       tags$div(
                                           class = "panel-body",
                                           style = "background-color: #F5F5F5; padding: 12px; border-radius: 5px;",

                                           tags$h5(
                                               icon("info-circle"),
                                               " Understanding the Risk Metrics",
                                               style = "margin-top: 0; color: #1B3A6B; font-weight: bold;"
                                           ),

                                           tags$table(
                                               class = "table table-condensed",
                                               style = "margin-bottom: 0; font-size: 12px;",
                                               tags$tbody(
                                                   tags$tr(
                                                       tags$td(tags$strong("DV01 (R mn)")),
                                                       tags$td("Rand value change per 1 basis point (0.01%) move in yields. Based on selected notional.")
                                                   ),
                                                   tags$tr(
                                                       tags$td(tags$strong("95% VaR (bps)")),
                                                       tags$td("Potential daily price loss in basis points (100 bps = 1%). Exceeded ~1 day/month.")
                                                   ),
                                                   tags$tr(
                                                       tags$td(tags$strong("99% VaR (bps)")),
                                                       tags$td("Extreme daily price loss in basis points. Exceeded ~2-3 days/year.")
                                                   ),
                                                   tags$tr(
                                                       tags$td(tags$strong("CVaR (bps)")),
                                                       tags$td("Expected Shortfall - average loss when VaR IS breached. Measures tail risk severity.")
                                                   ),
                                                   tags$tr(
                                                       tags$td(tags$strong("Volatility (%)")),
                                                       tags$td("Daily standard deviation of price returns (not annualized).")
                                                   )
                                               )
                                           ),

                                           tags$p(
                                               class = "text-muted small",
                                               style = "margin-bottom: 0; margin-top: 8px;",
                                               tags$strong("Example: "),
                                               "A bond with 95% VaR of 271 bps means on 95% of days, the price loss will be less than 2.71%. ",
                                               "On the worst 5% of days (about 1 per month), losses may exceed this threshold."
                                           )
                                       )
                                   ),

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

                                   # ═══════════════════════════════════════════════════════════════
                                   # TOP ROW: Controls (left) + Technical Summary (right)
                                   # ═══════════════════════════════════════════════════════════════
                                   fluidRow(
                                       # LEFT COLUMN - Controls + Bond Profile
                                       column(4,
                                              # Controls Panel
                                              wellPanel(
                                                  style = "background-color: #F8F9FA; padding: 15px; border-radius: 8px;",
                                                  selectInput(
                                                      "tech_bond_select",
                                                      "Select Bond:",
                                                      choices = NULL,
                                                      width = "100%"
                                                  ),
                                                  selectInput(
                                                      "tech_indicator_type",
                                                      "Indicator Type:",
                                                      choices = list(
                                                          "All Indicators" = "all",
                                                          "Momentum" = "momentum",
                                                          "Volatility" = "volatility",
                                                          "Mean Reversion" = "mean_reversion"
                                                      ),
                                                      selected = "all",
                                                      width = "100%"
                                                  ),
                                                  hr(style = "margin: 10px 0;"),
                                                  # Quick Actions
                                                  actionButton(
                                                      "tech_refresh",
                                                      "Refresh Data",
                                                      icon = icon("sync"),
                                                      class = "btn-sm btn-outline-primary",
                                                      width = "100%"
                                                  ),
                                                  br(), br(),
                                                  downloadButton(
                                                      "download_technical_plot",
                                                      "Download Analysis",
                                                      class = "btn-sm btn-outline-secondary",
                                                      style = "width: 100%;"
                                                  )
                                              ),

                                              # Bond Profile Card (stays in left column)
                                              uiOutput("bond_profile_card")
                                       ),

                                       # RIGHT COLUMN - Technical Summary Panel
                                       column(8,
                                              uiOutput("technical_summary_panel")
                                       )
                                   ),

                                   # ═══════════════════════════════════════════════════════════════
                                   # MIDDLE ROW: Signal History + Yield Technicals (side by side)
                                   # Moved OUT of columns to prevent white space from height mismatch
                                   # ═══════════════════════════════════════════════════════════════
                                   fluidRow(
                                       column(6,
                                              # Signal History Chart (60 days)
                                              uiOutput("signal_history_mini")
                                       ),
                                       column(6,
                                              # Yield Chart with Technical Levels (90 days)
                                              uiOutput("yield_technicals_panel")
                                       )
                                   ),

                                   # ═══════════════════════════════════════════════════════════════
                                   # BOTTOM: Main Technical Chart - Full Width
                                   # ═══════════════════════════════════════════════════════════════
                                   plotOutput("advanced_technical_plot", height = "500px", width = "100%")
                               )
                           ),

                           # Signal Summary Stats Panel
                           fluidRow(
                               column(12,
                                   uiOutput("signal_summary_stats")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Trading Signal Matrix",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotOutput("signal_matrix_heatmap", height = "400px", width = "100%"),
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
                                   DT::dataTableOutput("technical_indicators_enhanced_table"),

                                   # Bond Investor Color Guide
                                   tags$div(
                                       style = "margin-top: 15px; padding: 12px; background: #FFF8E1; border-left: 4px solid #FF9800; border-radius: 4px;",

                                       tags$p(
                                           style = "margin: 0 0 8px 0; font-weight: bold; color: #E65100;",
                                           icon("info-circle"),
                                           " Bond Investor Color Guide"
                                       ),

                                       tags$p(
                                           style = "margin: 0; font-size: 13px;",
                                           "This table analyzes ", tags$strong("yield"), " trends. For bond holders, the relationship is inverse:"
                                       ),

                                       tags$ul(
                                           style = "margin: 8px 0; font-size: 13px;",
                                           tags$li(
                                               tags$span(style = "display: inline-block; width: 14px; height: 14px; background: #4CAF50; border-radius: 2px; margin-right: 6px; vertical-align: middle;"),
                                               tags$strong("Green = Bullish for Prices"),
                                               " \u2014 Yields falling/bearish \u2192 Bond prices rising \u2192 ",
                                               tags$span(style = "color: #1B5E20;", "GOOD")
                                           ),
                                           tags$li(
                                               tags$span(style = "display: inline-block; width: 14px; height: 14px; background: #F44336; border-radius: 2px; margin-right: 6px; vertical-align: middle;"),
                                               tags$strong("Red = Bearish for Prices"),
                                               " \u2014 Yields rising/bullish \u2192 Bond prices falling \u2192 ",
                                               tags$span(style = "color: #C62828;", "BAD")
                                           )
                                       ),

                                       tags$p(
                                           style = "margin: 8px 0 0 0; font-size: 12px; color: #666;",
                                           tags$em("Remember: Yields and Prices move in opposite directions!")
                                       )
                                   )
                               )
                           )
                       ),

                       # Carry & Roll Analytics Tab
                       tabPanel(
                           "Carry & Roll Analytics",
                           icon = icon("calculator"),

                           # DYNAMIC Information/Explanation Panel - now reactive to funding rate
                           fluidRow(
                               column(12,
                                      uiOutput("carry_roll_info_box")
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

                                   # ROW 1: Compact control bar
                                   fluidRow(
                                       column(12,
                                              tags$div(
                                                  style = "display: flex; align-items: center; flex-wrap: wrap; gap: 20px; padding: 12px 15px;
                                                           background: #F8F9FA; border-radius: 6px; margin-bottom: 15px;",

                                                  # Funding Rate
                                                  tags$div(
                                                      style = "flex: 0 0 280px;",
                                                      tags$div(
                                                          style = "display: flex; align-items: center; gap: 10px;",
                                                          tags$label("Funding Rate:", style = "margin: 0; white-space: nowrap; font-weight: 500;"),
                                                          tags$div(
                                                              style = "flex: 1; min-width: 150px;",
                                                              sliderInput("funding_rate", NULL, min = 5, max = 12, value = 8.25,
                                                                          step = 0.25, post = "%", width = "100%")
                                                          )
                                                      )
                                                  ),

                                                  # Return Type
                                                  tags$div(
                                                      style = "flex: 0 0 auto;",
                                                      radioButtons("return_type", NULL,
                                                                   choices = c("Gross" = "gross", "Net" = "net", "Risk-Adj" = "risk_adj"),
                                                                   selected = "net", inline = TRUE)
                                                  ),

                                                  # Recalculate Button
                                                  actionButton("recalculate_carry", "Recalculate",
                                                               icon = icon("sync"), class = "btn-primary btn-sm"),

                                                  # Spacer
                                                  tags$div(style = "flex: 1;"),

                                                  # Download and Methodology buttons
                                                  tags$div(
                                                      style = "display: flex; gap: 8px;",
                                                      downloadButton("download_carry_roll", "Download", class = "btn-sm btn-outline-secondary"),
                                                      actionButton("show_carry_methodology", "Methodology",
                                                                   icon = icon("info-circle"), class = "btn-sm btn-outline-info")
                                                  )
                                              )
                                       )
                                   ),

                                   # ROW 2: Full-width heatmap
                                   fluidRow(
                                       column(12,
                                              plotOutput("enhanced_carry_roll_heatmap", height = "450px")
                                       )
                                   ),

                                   tags$hr(style = "margin: 20px 0;"),

                                   # ROW 3: Three cards below
                                   fluidRow(
                                       # Card 1: Quick Metrics
                                       column(4,
                                              tags$div(
                                                  class = "panel panel-default",
                                                  style = "height: 100%; margin-bottom: 0;",
                                                  tags$div(
                                                      class = "panel-heading",
                                                      style = "background: #1B3A6B; color: white; padding: 10px 15px; font-weight: 500;",
                                                      "Quick Metrics"
                                                  ),
                                                  tags$div(
                                                      class = "panel-body",
                                                      style = "padding: 15px;",
                                                      uiOutput("carry_roll_summary")
                                                  )
                                              )
                                       ),

                                       # Card 2: Risk Metrics
                                       column(4,
                                              tags$div(
                                                  class = "panel panel-default",
                                                  style = "height: 100%; margin-bottom: 0;",
                                                  tags$div(
                                                      class = "panel-heading",
                                                      style = "background: #455A64; color: white; padding: 10px 15px; font-weight: 500;",
                                                      "Risk Metrics"
                                                  ),
                                                  tags$div(
                                                      class = "panel-body",
                                                      style = "padding: 15px;",
                                                      uiOutput("risk_metrics_panel")
                                                  )
                                              )
                                       ),

                                       # Card 3: Return Decomposition
                                       column(4,
                                              tags$div(
                                                  class = "panel panel-default",
                                                  style = "height: 100%; margin-bottom: 0;",
                                                  tags$div(
                                                      class = "panel-heading",
                                                      style = "background: #37474F; color: white; padding: 10px 15px; font-weight: 500;",
                                                      "Return Decomposition"
                                                  ),
                                                  tags$div(
                                                      class = "panel-body",
                                                      style = "padding: 15px;",

                                                      fluidRow(
                                                          column(6,
                                                              selectInput("selected_carry_bond", "Bond:",
                                                                          choices = NULL, width = "100%")
                                                          ),
                                                          column(6,
                                                              selectInput("decomp_period", "Period:",
                                                                          choices = c("30d" = "30d", "90d" = "90d",
                                                                                      "180d" = "180d", "360d" = "360d"),
                                                                          selected = "90d", width = "100%")
                                                          )
                                                      ),

                                                      uiOutput("carry_decomposition")
                                                  )
                                              )
                                       )
                                   )
                               )
                           ),

                           # Scenario Analysis with enhanced controls
                           fluidRow(
                               box(
                                   title = "Scenario Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   # Controls container
                                   tags$div(
                                       class = "scenario-controls",
                                       style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",

                                       fluidRow(
                                           column(5,
                                               tags$div(
                                                   style = "position: relative;",
                                                   selectInput(
                                                       "scenario_bonds_select",
                                                       label = tags$span(
                                                           "Select Bonds to Compare",
                                                           tags$small(" (max 8)", class = "text-muted")
                                                       ),
                                                       choices = NULL,
                                                       selected = NULL,
                                                       multiple = TRUE,
                                                       width = "100%"
                                                   ),
                                                   # Quick selection buttons
                                                   tags$div(
                                                       style = "margin-top: -10px;",
                                                       actionLink("select_short_bonds", "Short-end",
                                                                  style = "font-size: 0.8em; margin-right: 10px;"),
                                                       actionLink("select_belly_bonds", "Belly",
                                                                  style = "font-size: 0.8em; margin-right: 10px;"),
                                                       actionLink("select_long_bonds", "Long-end",
                                                                  style = "font-size: 0.8em; margin-right: 10px;"),
                                                       actionLink("select_curve_spread", "Curve spread",
                                                                  style = "font-size: 0.8em;")
                                                   )
                                               )
                                           ),

                                           column(3,
                                               radioButtons(
                                                   "scenario_y_scale",
                                                   "Y-Axis Scale:",
                                                   choices = c("Unified (easier comparison)" = "fixed",
                                                               "Free (maximize detail)" = "free_y"),
                                                   selected = "fixed",
                                                   inline = FALSE
                                               )
                                           ),

                                           column(2,
                                               checkboxInput(
                                                   "scenario_show_confidence",
                                                   tags$span("Show 95% CI Bands"),
                                                   value = TRUE
                                               ),
                                               tags$div(
                                                   style = "margin-top: 5px;",
                                                   numericInput(
                                                       "scenario_confidence_level",
                                                       "Confidence %:",
                                                       value = 95,
                                                       min = 80,
                                                       max = 99,
                                                       step = 5,
                                                       width = "100%"
                                                   )
                                               )
                                           ),

                                           column(2,
                                               tags$div(
                                                   style = "margin-top: 25px;",
                                                   downloadButton(
                                                       "download_scenario_analysis",
                                                       "Download Chart",
                                                       class = "btn-primary btn-sm",
                                                       style = "width: 100%;"
                                                   )
                                               )
                                           )
                                       )
                                   ),

                                   # Plot output (taller to accommodate confidence bands)
                                   plotOutput("scenario_analysis_plot", height = "550px")
                               )
                           ),

                           # Butterfly Spread Analyzer
                           fluidRow(
                               box(
                                   title = "Butterfly Spread Analyzer",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   collapsible = TRUE,

                                   # Educational header - enhanced with Insele branding
                                   tags$div(
                                       class = "info-box",
                                       style = "background: linear-gradient(135deg, #1B3A6B 0%, #2E5090 100%);
                                                color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px;",

                                       tags$h4(
                                           style = "color: white; margin-top: 0;",
                                           icon("question-circle"),
                                           " What is a Butterfly Spread?"
                                       ),

                                       tags$p(
                                           style = "margin: 10px 0;",
                                           "A butterfly measures yield curve curvature using 3 bonds: ",
                                           tags$code(
                                               style = "background: rgba(255,255,255,0.2); padding: 3px 8px; border-radius: 4px;",
                                               "Spread = Short Wing - 2 \u00d7 Body + Long Wing"
                                           ),
                                           ". When stationary (mean-reverting), extreme Z-scores suggest trading opportunities."
                                       ),

                                       # Trading signals with colored backgrounds
                                       tags$div(
                                           style = "margin-top: 15px;",
                                           fluidRow(
                                               column(6,
                                                   tags$div(
                                                       style = "background: rgba(46, 125, 50, 0.3); padding: 10px; border-radius: 4px; margin-bottom: 10px;",
                                                       tags$strong(style = "color: #A5D6A7;", "Z-Score < -2: "),
                                                       "Spread unusually LOW \u2192 Buy wings, sell body (expect spread to widen)"
                                                   )
                                               ),
                                               column(6,
                                                   tags$div(
                                                       style = "background: rgba(198, 40, 40, 0.3); padding: 10px; border-radius: 4px; margin-bottom: 10px;",
                                                       tags$strong(style = "color: #EF9A9A;", "Z-Score > +2: "),
                                                       "Spread unusually HIGH \u2192 Sell wings, buy body (expect spread to narrow)"
                                                   )
                                               )
                                           ),

                                           # Stationarity explanation
                                           tags$div(
                                               style = "background: rgba(255,255,255,0.1); padding: 10px; border-radius: 4px;",
                                               tags$strong(style = "color: #90CAF9;", "ADF p < 0.05: "),
                                               "Spread is stationary (mean-reverting) - Trade is valid. ",
                                               tags$em("Higher p-values suggest the spread may trend rather than revert.")
                                           )
                                       )
                                   ),

                                   # Controls row
                                   fluidRow(
                                       column(3,
                                              selectInput(
                                                  "butterfly_lookback",
                                                  "Lookback Period:",
                                                  choices = c("6 Months" = 180, "1 Year" = 365, "2 Years" = 730, "All History" = 9999),
                                                  selected = 365
                                              )
                                       ),
                                       column(3,
                                              sliderInput(
                                                  "zscore_threshold",
                                                  "Z-Score Threshold:",
                                                  min = 1.5, max = 3.0, value = 2.0, step = 0.1
                                              )
                                       ),
                                       column(3,
                                              selectInput(
                                                  "stationarity_filter",
                                                  "Stationarity Filter:",
                                                  choices = c("Stationary Only (p<0.05)" = "stationary",
                                                              "All Butterflies" = "all"),
                                                  selected = "stationary"
                                              ),
                                              # Display stationarity counts for debugging/verification
                                              tags$small(class = "text-muted", uiOutput("stationarity_counts"))
                                       ),
                                       column(3,
                                              actionButton(
                                                  "generate_butterflies",
                                                  "Generate Butterflies",
                                                  icon = icon("sync"),
                                                  class = "btn-primary",
                                                  style = "margin-top: 25px; width: 100%;"
                                              )
                                       )
                                   ),

                                   tags$hr(),

                                   # Main content: Table on left, Chart on right
                                   fluidRow(
                                       # Left: Ranked butterfly table
                                       column(5,
                                              tags$h5("Top Butterfly Opportunities", class = "text-primary"),
                                              tags$small(class = "text-muted", "Ranked by absolute Z-Score | Click row to view chart"),
                                              DT::dataTableOutput("butterfly_table"),
                                              tags$div(
                                                  style = "margin-top: 10px;",
                                                  downloadButton("download_butterfly_data", "Export Data", class = "btn-sm")
                                              )
                                       ),

                                       # Right: Selected butterfly chart
                                       column(7,
                                              tags$h5("Butterfly Spread History", class = "text-primary"),
                                              uiOutput("selected_butterfly_info"),
                                              plotOutput("butterfly_chart", height = "400px"),
                                              tags$div(
                                                  style = "margin-top: 10px;",
                                                  downloadButton("download_butterfly_chart", "Download Chart", class = "btn-sm"),
                                                  actionButton("butterfly_trade_details", "Trade Details",
                                                               icon = icon("info-circle"), class = "btn-sm btn-outline-secondary")
                                              )
                                       )
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

                                   # Educational explainer panel - Insele branded gradient
                                   tags$div(
                                       class = "forward-rate-info-box",
                                       style = "margin-bottom: 15px; background: linear-gradient(135deg, #1B3A6B 0%, #2E5090 100%);
                                                border: none; border-radius: 8px; padding: 20px; color: white;",

                                       tags$h5(
                                           style = "color: white; margin-top: 0; font-weight: bold;",
                                           icon("chart-line"), " Understanding Forward Rates"
                                       ),

                                       tags$p(
                                           style = "color: white; margin: 10px 0;",
                                           "Forward rates represent the market's implied expectation for future interest rates, ",
                                           "derived from the current yield curve using no-arbitrage pricing."
                                       ),

                                       # Notation box - white background with dark text
                                       tags$div(
                                           style = "background: rgba(255,255,255,0.95); padding: 12px; border-radius: 4px; margin: 12px 0;",
                                           tags$span(style = "color: #1B3A6B;", tags$strong("Notation: ")),
                                           tags$code(style = "background: rgba(27, 58, 107, 0.1); color: #1B3A6B; padding: 2px 6px; border-radius: 3px;", "XyYy"),
                                           tags$span(style = "color: #333;", " = Y-year rate starting X years from now"),
                                           tags$br(),
                                           tags$span(style = "color: #1B3A6B;", tags$strong("Example: ")),
                                           tags$code(style = "background: rgba(27, 58, 107, 0.1); color: #1B3A6B; padding: 2px 6px; border-radius: 3px;", "3y2y"),
                                           tags$span(style = "color: #333;", " = The 2-year rate, 3 years forward (Year 3 to Year 5)"),
                                           tags$br(),
                                           tags$span(style = "color: #666; font-size: 12px; font-style: italic;",
                                                     "Chart segments span from start year (X) to end year (X+Y)")
                                       ),

                                       tags$h6(style = "color: white; margin-top: 15px; font-weight: bold;", "Spread Interpretation:"),

                                       # Three-column interpretation layout
                                       fluidRow(
                                           column(4,
                                               tags$div(
                                                   style = "background: rgba(46, 125, 50, 0.25); padding: 10px; border-radius: 4px; margin: 5px 0;",
                                                   tags$span(style = "color: #A5D6A7; font-size: 1.2em;", HTML("&#9679; ")),
                                                   tags$strong(style = "color: #A5D6A7;", "Negative spread"),
                                                   tags$br(),
                                                   tags$span(style = "color: white; font-size: 13px;", "Market expects rates to FALL"),
                                                   tags$br(),
                                                   tags$em(style = "font-size: 11px; color: #B0BEC5;", "Bullish for bond prices")
                                               )
                                           ),
                                           column(4,
                                               tags$div(
                                                   style = "background: rgba(158, 158, 158, 0.25); padding: 10px; border-radius: 4px; margin: 5px 0;",
                                                   tags$span(style = "color: #E0E0E0; font-size: 1.2em;", HTML("&#9679; ")),
                                                   tags$strong(style = "color: #E0E0E0;", "Near zero"),
                                                   tags$br(),
                                                   tags$span(style = "color: white; font-size: 13px;", "Rates expected unchanged"),
                                                   tags$br(),
                                                   tags$em(style = "font-size: 11px; color: #B0BEC5;", "Neutral outlook")
                                               )
                                           ),
                                           column(4,
                                               tags$div(
                                                   style = "background: rgba(198, 40, 40, 0.25); padding: 10px; border-radius: 4px; margin: 5px 0;",
                                                   tags$span(style = "color: #EF9A9A; font-size: 1.2em;", HTML("&#9679; ")),
                                                   tags$strong(style = "color: #EF9A9A;", "Positive spread"),
                                                   tags$br(),
                                                   tags$span(style = "color: white; font-size: 13px;", "Market expects rates to RISE"),
                                                   tags$br(),
                                                   tags$em(style = "font-size: 11px; color: #B0BEC5;", "Bearish for bond prices")
                                               )
                                           )
                                       ),

                                       tags$p(
                                           style = "color: rgba(255,255,255,0.75); font-size: 11px; font-style: italic; margin-top: 12px; margin-bottom: 0;",
                                           icon("info-circle"),
                                           " Note: Forward rates are mathematical constructs, not forecasts. ",
                                           "They represent break-even rates for carry trades."
                                       )
                                   ),

                                   # Add visualization (increased height for more forward periods)
                                   plotOutput("forward_curve_plot", height = "420px"),

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
                                   title = tagList(icon("brain"), " ML-Powered Auction Predictions"),
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   collapsible = TRUE,

                                   fluidRow(
                                       style = "display: flex; flex-wrap: wrap;",
                                       # ═══════════════════════════════════════════════════════════════
                                       # LEFT COLUMN: Upcoming Auction Selection (SINGLE ENTRY POINT)
                                       # ═══════════════════════════════════════════════════════════════
                                       column(3,
                                              style = "min-height: 100%;",
                                              tags$div(
                                                  class = "config-panel",
                                                  style = "background: #f8f9fa; padding: 15px; border-radius: 8px;
                                                           border: 2px solid #1B3A6B;",

                                                  tags$h5(
                                                      tagList(icon("calendar-plus"), " Upcoming Auctions"),
                                                      style = "color: #1B3A6B; font-weight: bold; margin-top: 0;"
                                                  ),

                                                  tags$p(
                                                      "Select bonds expected in the next auction(s):",
                                                      class = "text-muted",
                                                      style = "font-size: 0.9em; margin-bottom: 10px;"
                                                  ),

                                                  # SINGLE bond selection - THIS IS THE ONLY PLACE TO SELECT
                                                  selectInput(
                                                      "upcoming_auction_bonds",
                                                      label = NULL,
                                                      choices = NULL,  # Populated dynamically
                                                      selected = NULL,
                                                      multiple = TRUE,
                                                      width = "100%"
                                                  ),

                                                  # Help text
                                                  tags$small(
                                                      tagList(icon("info-circle"), " Select up to 3 bonds. Typically announced on Treasury's website."),
                                                      style = "font-size: 0.8em; color: #6c757d;"
                                                  ),

                                                  tags$hr(style = "margin: 12px 0;"),

                                                  # Auction date input
                                                  dateInput(
                                                      "upcoming_auction_date",
                                                      "Expected Auction Date:",
                                                      value = Sys.Date() + 7,
                                                      min = Sys.Date(),
                                                      max = Sys.Date() + 90,
                                                      width = "100%"
                                                  ),

                                                  # Offer size input
                                                  numericInput(
                                                      "upcoming_offer_amount",
                                                      "Offer Size (R millions):",
                                                      value = 5000,
                                                      min = 500,
                                                      max = 20000,
                                                      step = 500,
                                                      width = "100%"
                                                  ),

                                                  tags$hr(style = "margin: 12px 0;"),

                                                  # Generate predictions button
                                                  actionButton(
                                                      "generate_predictions",
                                                      label = tagList(icon("calculator"), " Generate Forecasts"),
                                                      class = "btn-primary btn-block",
                                                      style = "margin-top: 10px;"
                                                  )
                                              ),

                                              # Quick Stats below
                                              tags$div(
                                                  style = "margin-top: 12px;",
                                                  uiOutput("auction_quick_stats_v2")
                                              )
                                       ),

                                       # ═══════════════════════════════════════════════════════════════
                                       # MIDDLE COLUMN: Forecast Results (width 6)
                                       # ═══════════════════════════════════════════════════════════════
                                       column(6,
                                              style = "min-height: 100%;",
                                              # Market Outlook Card
                                              uiOutput("auction_market_outlook_card"),

                                              # Detailed Forecasts Table
                                              tags$div(
                                                  style = "margin-bottom: 15px;",
                                                  tags$h5(
                                                      tagList(icon("table"), " Forecast Details"),
                                                      style = "color: #1B3A6B; font-weight: bold;"
                                                  ),
                                                  tags$div(
                                                      style = "max-height: 250px; overflow-y: auto;",
                                                      DT::dataTableOutput("auction_forecast_table_v2", height = "auto")
                                                  )
                                              ),

                                              # Market Sentiment - HORIZONTAL LAYOUT
                                              uiOutput("auction_market_sentiment")
                                       ),

                                       # ═══════════════════════════════════════════════════════════════
                                       # RIGHT COLUMN: Historical Context (width 3)
                                       # ═══════════════════════════════════════════════════════════════
                                       column(3,
                                              style = "min-height: 100%;",
                                              # Forecast Chart
                                              tags$div(
                                                  style = "margin-bottom: 15px;",
                                                  tags$h5(
                                                      tagList(icon("chart-line"), " Historical Performance"),
                                                      style = "color: #1B3A6B; font-weight: bold;"
                                                  ),
                                                  tags$div(
                                                      style = "height: 200px;",
                                                      plotOutput("auction_forecast_chart_v2", height = "100%")
                                                  )
                                              ),

                                              # Historical Stats for Selected Bonds
                                              tags$div(
                                                  style = "background-color: #f8f9fa; border-radius: 8px; padding: 15px;",
                                                  uiOutput("selected_bonds_history"),
                                                  tags$hr(style = "margin: 10px 0;"),
                                                  # Download button
                                                  downloadButton(
                                                      "download_forecast_chart",
                                                      "Download Chart",
                                                      class = "btn-sm btn-outline-primary",
                                                      style = "width: 100%;"
                                                  )
                                              )
                                       )
                                   )
                               )
                           ),

                           # ═══════════════════════════════════════════════════════════════════════════
                           # AUCTION QUALITY DASHBOARD (New Section)
                           # ═══════════════════════════════════════════════════════════════════════════
                           fluidRow(
                               box(
                                   title = tagList(icon("chart-bar"), " Auction Quality Dashboard"),
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   collapsible = TRUE,

                                   # Section header with explanation
                                   tags$div(
                                       style = "margin-bottom: 15px; padding: 10px; background: #f8f9fa; border-radius: 6px; border-left: 4px solid #1B3A6B;",
                                       tags$p(
                                           style = "color: #666; font-size: 0.95em; margin: 0;",
                                           "Analyzes recent auction performance to assess market demand and pricing efficiency. ",
                                           tags$strong("Highlighted bonds", style = "color: #1B3A6B;"),
                                           " are selected for the upcoming auction forecast in the ML Predictions section above."
                                       )
                                   ),

                                   # KPI Cards Row with tooltips
                                   fluidRow(
                                       column(3,
                                              tags$div(
                                                  class = "kpi-card",
                                                  style = "background: linear-gradient(135deg, #1B3A6B 0%, #2E5090 100%);
                                                           color: white; padding: 20px; border-radius: 8px; text-align: center;",
                                                  tags$div(
                                                      style = "display: flex; align-items: center; justify-content: center; gap: 5px;",
                                                      tags$p("Avg Auction Quality", style = "margin-bottom: 0; opacity: 0.9;"),
                                                      tags$span(
                                                          `data-toggle` = "tooltip",
                                                          `data-placement` = "top",
                                                          title = "Composite score (0-100) combining bid-to-cover, tail, participation, and concession. A=85+, B=70-84, C=55-69, D=40-54, F=<40",
                                                          style = "cursor: help; opacity: 0.8;",
                                                          icon("info-circle")
                                                      )
                                                  ),
                                                  tags$h2(textOutput("avg_quality_score_text", inline = TRUE),
                                                          style = "margin: 5px 0;"),
                                                  uiOutput("avg_quality_grade_badge")
                                              )
                                       ),
                                       column(3,
                                              tags$div(
                                                  class = "kpi-card",
                                                  style = "background: #f8f9fa; padding: 20px; border-radius: 8px;
                                                           text-align: center; border: 1px solid #dee2e6;",
                                                  tags$div(
                                                      style = "display: flex; align-items: center; justify-content: center; gap: 5px;",
                                                      tags$p("Avg Auction Tail", class = "text-muted", style = "margin-bottom: 0;"),
                                                      tags$span(
                                                          `data-toggle` = "tooltip",
                                                          `data-placement` = "top",
                                                          title = "Spread between highest accepted yield and average yield. Lower tail = tighter pricing and stronger consensus. <3bps=Tight, 3-6bps=Normal, >6bps=Wide",
                                                          style = "cursor: help;",
                                                          icon("info-circle", class = "text-muted")
                                                      )
                                                  ),
                                                  tags$h2(textOutput("avg_tail_text", inline = TRUE),
                                                          style = "margin: 5px 0; color: #1B3A6B;"),
                                                  tags$span(textOutput("tail_interpretation", inline = TRUE),
                                                            class = "text-muted", style = "font-size: 0.9em;")
                                              )
                                       ),
                                       column(3,
                                              tags$div(
                                                  class = "kpi-card",
                                                  style = "background: #f8f9fa; padding: 20px; border-radius: 8px;
                                                           text-align: center; border: 1px solid #dee2e6;",
                                                  tags$div(
                                                      style = "display: flex; align-items: center; justify-content: center; gap: 5px;",
                                                      tags$p("Non-Competitive %", class = "text-muted", style = "margin-bottom: 0;"),
                                                      tags$span(
                                                          `data-toggle` = "tooltip",
                                                          `data-placement` = "top",
                                                          title = "Percentage of allocation to non-competitive (standing order) bids, typically from large institutions with pre-committed demand. Higher = more stable demand base.",
                                                          style = "cursor: help;",
                                                          icon("info-circle", class = "text-muted")
                                                      )
                                                  ),
                                                  tags$h2(textOutput("avg_institutional_text", inline = TRUE),
                                                          style = "margin: 5px 0; color: #1B3A6B;"),
                                                  tags$span(textOutput("institutional_interpretation", inline = TRUE),
                                                            class = "text-muted", style = "font-size: 0.9em;")
                                              )
                                       ),
                                       column(3,
                                              tags$div(
                                                  class = "kpi-card",
                                                  style = "background: #f8f9fa; padding: 20px; border-radius: 8px;
                                                           text-align: center; border: 1px solid #dee2e6;",
                                                  tags$div(
                                                      style = "display: flex; align-items: center; justify-content: center; gap: 5px;",
                                                      tags$p("Avg Concession", class = "text-muted", style = "margin-bottom: 0;"),
                                                      tags$span(
                                                          `data-toggle` = "tooltip",
                                                          `data-placement` = "top",
                                                          title = "Difference between auction clearing yield and pre-auction secondary market yield. Positive = weak demand (discount), Negative = strong demand (premium). Normal range: +/-5 bps.",
                                                          style = "cursor: help;",
                                                          icon("info-circle", class = "text-muted")
                                                      )
                                                  ),
                                                  tags$h2(textOutput("avg_concession_text", inline = TRUE),
                                                          style = "margin: 5px 0; color: #1B3A6B;"),
                                                  tags$span(textOutput("concession_interpretation", inline = TRUE),
                                                            class = "text-muted", style = "font-size: 0.9em;")
                                              )
                                       )
                                   ),

                                   tags$hr(style = "margin: 20px 0;"),

                                   # Charts Row - updated to use DT for heatmap with highlighting
                                   fluidRow(
                                       column(6,
                                              tags$div(
                                                  style = "display: flex; align-items: center; gap: 8px; margin-bottom: 10px;",
                                                  tags$h5("Auction Quality by Bond",
                                                          style = "color: #1B3A6B; font-weight: bold; margin: 0;"),
                                                  tags$span(
                                                      style = "font-size: 0.85em; color: #666; font-style: italic;",
                                                      "(Selected bonds highlighted)"
                                                  )
                                              ),
                                              DT::DTOutput("auction_quality_heatmap_dt", height = "350px")
                                       ),
                                       column(6,
                                              tags$h5("Auction Concession Trend",
                                                      style = "color: #1B3A6B; font-weight: bold;"),
                                              plotOutput("concession_trend_chart", height = "350px")
                                       )
                                   ),

                                   tags$br(),

                                   fluidRow(
                                       column(12,
                                              downloadButton("download_quality_report", "Download Quality Report",
                                                             class = "btn-primary btn-sm")
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
                                   title = "Bid-to-Cover Decomposition",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
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
                                                              checkboxInput("plot_butterfly_spread", HTML("&#10024; Butterfly Spread Analysis"), value = FALSE),
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