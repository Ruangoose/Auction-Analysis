# Enhanced UI Modules for Bond Analytics Dashboard
# Purpose: Reusable, intelligent UI components

library(shiny)
library(shinyWidgets)

# ================== SMART INPUT MODULES ==================

#' Smart Bond Selector with Grouping and Search
#' @param id Module ID
#' @param label Label for the selector
bond_selector_ui <- function(id, label = "Select Bonds:") {
    ns <- NS(id)

    tagList(
        pickerInput(
            ns("bonds"),
            label = label,
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 3",
                `count-selected-text` = "{0} bonds selected",
                `live-search` = TRUE,
                `live-search-placeholder` = "Search bonds...",
                `live-search-style` = "contains",
                title = "Select bonds to analyze",
                header = "Group by maturity bucket",
                `dropdown-align-right` = TRUE,
                size = 10
            ),
            width = "100%"
        ),

        # Quick selection helpers
        fluidRow(
            column(6,
                   actionButton(ns("select_liquid"), "Most Liquid",
                                class = "btn-sm btn-info", width = "100%")
            ),
            column(6,
                   actionButton(ns("select_auction"), "Next Auction",
                                class = "btn-sm btn-warning", width = "100%")
            )
        )
    )
}

#' Server logic for bond selector
bond_selector_server <- function(id, data) {
    moduleServer(id, function(input, output, session) {

        # Update choices based on data
        observe({
            req(data())

            # Group bonds by maturity
            bond_groups <- data() %>%
                group_by(bond) %>%
                summarise(
                    avg_duration = mean(modified_duration, na.rm = TRUE),
                    liquidity_score = mean(bid_to_cover, na.rm = TRUE) *
                        sum(!is.na(bid_to_cover)),
                    .groups = "drop"
                ) %>%
                mutate(
                    group = case_when(
                        avg_duration <= 5 ~ "Short (≤5y)",
                        avg_duration <= 10 ~ "Medium (5-10y)",
                        avg_duration <= 15 ~ "Long (10-15y)",
                        TRUE ~ "Ultra-Long (>15y)"
                    )
                ) %>%
                arrange(group, avg_duration)

            grouped_choices <- split(bond_groups$bond, bond_groups$group)

            updatePickerInput(
                session, "bonds",
                choices = grouped_choices,
                selected = isolate(input$bonds)
            )
        })

        # Select most liquid bonds
        observeEvent(input$select_liquid, {
            req(data())

            liquid_bonds <- data() %>%
                filter(!is.na(bid_to_cover)) %>%
                group_by(bond) %>%
                summarise(
                    liquidity = mean(bid_to_cover) * n(),
                    .groups = "drop"
                ) %>%
                top_n(5, liquidity) %>%
                pull(bond)

            updatePickerInput(session, "bonds", selected = liquid_bonds)
        })

        # Select bonds in next auction (placeholder)
        observeEvent(input$select_auction, {
            # In production, this would check actual auction calendar
            likely_bonds <- c("R2030", "R2035", "R2048")  # Example
            updatePickerInput(session, "bonds", selected = likely_bonds)
        })

        # Return selected bonds
        reactive(input$bonds)
    })
}

# ================== SMART DATE RANGE MODULE ==================

#' Enhanced Date Range Selector with Quick Options
#' @param id Module ID
date_range_ui <- function(id) {
    ns <- NS(id)

    tagList(
        fluidRow(
            column(12,
                   dateRangeInput(
                       ns("dates"),
                       "Analysis Period:",
                       start = Sys.Date() - 90,
                       end = Sys.Date(),
                       width = "100%"
                   )
            )
        ),

        fluidRow(
            column(12,
                   radioButtons(
                       ns("quick_select"),
                       "Quick Select:",
                       choices = c(
                           "YTD" = "ytd",
                           "1M" = "1m",
                           "3M" = "3m",
                           "6M" = "6m",
                           "1Y" = "1y",
                           "Since Auction" = "auction",
                           "Custom" = "custom"
                       ),
                       selected = "3m",
                       inline = TRUE,
                       width = "100%"
                   )
            )
        ),

        # Show important dates
        uiOutput(ns("important_dates"))
    )
}

#' Server logic for date range selector
date_range_server <- function(id, data) {
    moduleServer(id, function(input, output, session) {

        # Update date range based on quick select
        observeEvent(input$quick_select, {
            if(input$quick_select != "custom") {
                end_date <- Sys.Date()

                start_date <- switch(
                    input$quick_select,
                    "ytd" = floor_date(Sys.Date(), "year"),
                    "1m" = end_date - months(1),
                    "3m" = end_date - months(3),
                    "6m" = end_date - months(6),
                    "1y" = end_date - years(1),
                    "auction" = {
                        # Find last major auction date
                        if(!is.null(data())) {
                            max(data()$date[!is.na(data()$bid_to_cover)],
                                end_date - months(1))
                        } else {
                            end_date - months(1)
                        }
                    }
                )

                updateDateRangeInput(
                    session, "dates",
                    start = start_date,
                    end = end_date
                )
            }
        })

        # Show important dates in the range
        output$important_dates <- renderUI({
            req(data())
            req(input$dates)

            # Find important dates in range
            date_data <- data() %>%
                filter(date >= input$dates[1] & date <= input$dates[2])

            important_dates <- list()

            # Find last auction
            last_auction <- date_data %>%
                filter(!is.na(bid_to_cover)) %>%
                summarise(last_date = max(date)) %>%
                pull(last_date)

            if(length(last_auction) > 0) {
                important_dates$auction <- paste("Last auction:",
                                                 format(last_auction, "%b %d"))
            }

            # Find max volatility date
            vol_date <- date_data %>%
                group_by(date) %>%
                summarise(
                    daily_vol = sd(yield_to_maturity, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                filter(daily_vol == max(daily_vol, na.rm = TRUE)) %>%
                pull(date) %>%
                first()

            if(!is.na(vol_date)) {
                important_dates$vol <- paste("High volatility:",
                                             format(vol_date, "%b %d"))
            }

            if(length(important_dates) > 0) {
                tags$small(
                    class = "text-muted",
                    paste(unlist(important_dates), collapse = " | ")
                )
            }
        })

        # Return selected date range
        reactive(input$dates)
    })
}

# ================== ANALYSIS TEMPLATE MODULE ==================

#' Analysis Template Selector
#' @param id Module ID
analysis_template_ui <- function(id) {
    ns <- NS(id)

    tagList(
        h4("Quick Analysis Templates", class = "text-primary"),

        radioButtons(
            ns("template"),
            label = NULL,
            choices = c(
                "Pre-Auction Overview" = "auction",
                "Relative Value Deep Dive" = "rv",
                "Liquidity Analysis" = "liquidity",
                "Risk Assessment" = "risk",
                "Trading Signals" = "signals",
                "Custom Analysis" = "custom"
            ),
            selected = "auction",
            width = "100%"
        ),

        conditionalPanel(
            condition = paste0("input['", ns("template"), "'] != 'custom'"),

            wellPanel(
                style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",

                h5("Template Settings:", class = "font-weight-bold"),

                uiOutput(ns("template_description")),

                hr(),

                actionButton(
                    ns("apply_template"),
                    "Apply Template",
                    icon = icon("check"),
                    class = "btn-success btn-block"
                )
            )
        )
    )
}

#' Server logic for analysis template
analysis_template_server <- function(id) {
    moduleServer(id, function(input, output, session) {

        # Template descriptions
        output$template_description <- renderUI({
            desc <- switch(
                input$template,
                "auction" = tagList(
                    tags$ul(
                        tags$li("Focus on bonds likely in next auction"),
                        tags$li("3-month historical comparison"),
                        tags$li("Emphasis on bid-to-cover trends"),
                        tags$li("Liquidity and demand analysis")
                    )
                ),
                "rv" = tagList(
                    tags$ul(
                        tags$li("Identify rich/cheap bonds"),
                        tags$li("6-month lookback period"),
                        tags$li("Z-score threshold: 1.5"),
                        tags$li("Curve analysis and butterfly trades")
                    )
                ),
                "liquidity" = tagList(
                    tags$ul(
                        tags$li("Focus on most liquid bonds"),
                        tags$li("YTD analysis period"),
                        tags$li("Auction frequency patterns"),
                        tags$li("Bid size and stability metrics")
                    )
                ),
                "risk" = tagList(
                    tags$ul(
                        tags$li("1-month volatility focus"),
                        tags$li("VaR and risk metrics"),
                        tags$li("Sharpe ratio analysis"),
                        tags$li("Stress period identification")
                    )
                ),
                "signals" = tagList(
                    tags$ul(
                        tags$li("Multi-factor signal generation"),
                        tags$li("3-month momentum window"),
                        tags$li("Top 10 opportunities"),
                        tags$li("Pair trade identification")
                    )
                )
            )

            tagList(
                tags$small(desc)
            )
        })

        # Return template settings
        list(
            template = reactive(input$template),
            apply = reactive(input$apply_template)
        )
    })
}

# ================== ALERT CONFIGURATION MODULE ==================

#' Alert Configuration UI
#' @param id Module ID
alert_config_ui <- function(id) {
    ns <- NS(id)

    tagList(
        h4("Alert Configuration", class = "text-warning"),

        sliderInput(
            ns("zscore_threshold"),
            "Z-Score Alert Level:",
            min = 1.0,
            max = 3.0,
            value = 2.0,
            step = 0.1,
            width = "100%"
        ),

        sliderInput(
            ns("btc_threshold"),
            "Bid-to-Cover Alert (Low):",
            min = 1.0,
            max = 3.0,
            value = 2.0,
            step = 0.1,
            width = "100%"
        ),

        numericInput(
            ns("days_stale"),
            "Data Staleness Alert (Days):",
            value = 5,
            min = 1,
            max = 30,
            width = "100%"
        ),

        checkboxGroupInput(
            ns("alert_types"),
            "Enable Alerts For:",
            choices = c(
                "Extreme Valuations" = "valuation",
                "Low Liquidity" = "liquidity",
                "Stale Data" = "stale",
                "Curve Inversions" = "inversion",
                "Auction Anomalies" = "auction"
            ),
            selected = c("valuation", "liquidity", "stale"),
            width = "100%"
        ),

        hr(),

        h5("Alert Delivery:"),

        checkboxInput(
            ns("dashboard_alerts"),
            "Show on Dashboard",
            value = TRUE
        ),

        checkboxInput(
            ns("email_alerts"),
            "Email Notifications",
            value = FALSE
        ),

        conditionalPanel(
            condition = paste0("input['", ns("email_alerts"), "'] == true"),
            textInput(
                ns("email_address"),
                "Email Address:",
                placeholder = "analyst@insele.co.za",
                width = "100%"
            )
        )
    )
}

# ================== METRIC CARD MODULE ==================

#' Create a metric card UI element
#' @param id Module ID
#' @param title Card title
#' @param icon Icon to display
metric_card_ui <- function(id, title = "Metric", icon = "chart-line") {
    ns <- NS(id)

    div(
        class = "metric-card",

        fluidRow(
            column(8,
                   h5(title, class = "font-weight-bold text-primary"),
                   h3(textOutput(ns("value"), inline = TRUE)),
                   tags$small(textOutput(ns("subtitle"), inline = TRUE))
            ),
            column(4,
                   div(
                       class = "text-right",
                       icon(icon, size = "3x", class = "text-muted opacity-50")
                   )
            )
        ),

        # Trend indicator
        div(
            class = "mt-2",
            uiOutput(ns("trend"))
        ),

        # Sparkline
        plotOutput(ns("sparkline"), height = "40px")
    )
}

#' Server logic for metric card
metric_card_server <- function(id, data, metric_func, subtitle_func = NULL,
                               trend_func = NULL) {
    moduleServer(id, function(input, output, session) {

        # Main value
        output$value <- renderText({
            req(data())
            metric_func(data())
        })

        # Subtitle
        output$subtitle <- renderText({
            req(data())
            if(!is.null(subtitle_func)) {
                subtitle_func(data())
            } else {
                ""
            }
        })

        # Trend indicator
        output$trend <- renderUI({
            req(data())
            if(!is.null(trend_func)) {
                trend_val <- trend_func(data())

                if(trend_val > 0) {
                    tags$span(
                        class = "text-success",
                        icon("arrow-up"),
                        paste0("+", round(trend_val, 1), "%")
                    )
                } else if(trend_val < 0) {
                    tags$span(
                        class = "text-danger",
                        icon("arrow-down"),
                        paste0(round(trend_val, 1), "%")
                    )
                } else {
                    tags$span(
                        class = "text-muted",
                        icon("minus"),
                        "0%"
                    )
                }
            }
        })

        # Sparkline
        output$sparkline <- renderPlot({
            req(data())

            # Simple sparkline implementation
            spark_data <- data() %>%
                group_by(date) %>%
                summarise(value = mean(yield_to_maturity, na.rm = TRUE),
                          .groups = "drop") %>%
                tail(20)

            par(mar = c(0, 0, 0, 0))
            plot(spark_data$value, type = "l", axes = FALSE,
                 col = "#6b9aa0", lwd = 2)
        })
    })
}

# ================== COMPARISON SELECTOR MODULE ==================

#' Comparison Period Selector
#' @param id Module ID
comparison_selector_ui <- function(id) {
    ns <- NS(id)

    tagList(
        h5("Comparison Periods:", class = "font-weight-bold"),

        checkboxGroupInput(
            ns("periods"),
            label = NULL,
            choices = c(
                "vs Previous Week" = "week",
                "vs Previous Month" = "month",
                "vs Previous Quarter" = "quarter",
                "vs Previous Year" = "year",
                "vs Last Auction" = "auction"
            ),
            selected = "week",
            width = "100%"
        ),

        conditionalPanel(
            condition = paste0("input['", ns("periods"), "'].length > 0"),

            radioButtons(
                ns("comparison_type"),
                "Show as:",
                choices = c(
                    "Absolute Change" = "absolute",
                    "Percentage Change" = "percentage",
                    "Z-Score Change" = "zscore"
                ),
                selected = "absolute",
                inline = TRUE
            )
        )
    )
}

# ================== CHART OPTIONS MODULE ==================

#' Universal Chart Options Panel
#' @param id Module ID
#' @param chart_type Type of chart for specific options
chart_options_ui <- function(id, chart_type = "general") {
    ns <- NS(id)

    tagList(
        h5("Chart Options:", class = "font-weight-bold"),

        # Common options
        checkboxInput(
            ns("show_grid"),
            "Show Grid",
            value = TRUE
        ),

        checkboxInput(
            ns("show_legend"),
            "Show Legend",
            value = TRUE
        ),

        checkboxInput(
            ns("show_labels"),
            "Show Data Labels",
            value = FALSE
        ),

        # Chart-specific options
        conditionalPanel(
            condition = paste0("'", chart_type, "' == 'scatter'"),

            checkboxInput(
                ns("show_trend"),
                "Show Trend Line",
                value = FALSE
            ),

            checkboxInput(
                ns("show_confidence"),
                "Show Confidence Bands",
                value = FALSE
            )
        ),

        conditionalPanel(
            condition = paste0("'", chart_type, "' == 'time_series'"),

            checkboxInput(
                ns("show_ma"),
                "Show Moving Average",
                value = FALSE
            ),

            sliderInput(
                ns("ma_period"),
                "MA Period:",
                min = 5,
                max = 60,
                value = 20,
                step = 5
            )
        ),

        hr(),

        # Export options
        h5("Export:", class = "font-weight-bold"),

        downloadButton(
            ns("download_chart"),
            "Download Chart",
            class = "btn-sm btn-info btn-block"
        ),

        br(),

        actionButton(
            ns("copy_code"),
            "Copy Code",
            icon = icon("code"),
            class = "btn-sm btn-secondary btn-block"
        )
    )
}

# ================== SMART TABLE MODULE ==================

#' Enhanced Data Table with Smart Features
#' @param id Module ID
smart_table_ui <- function(id) {
    ns <- NS(id)

    tagList(
        fluidRow(
            column(6,
                   textInput(
                       ns("search"),
                       label = NULL,
                       placeholder = "Search table...",
                       width = "100%"
                   )
            ),
            column(3,
                   selectInput(
                       ns("rows_per_page"),
                       label = NULL,
                       choices = c(10, 25, 50, 100),
                       selected = 25,
                       width = "100%"
                   )
            ),
            column(3,
                   downloadButton(
                       ns("export"),
                       "Export",
                       class = "btn-sm btn-success btn-block"
                   )
            )
        ),

        hr(),

        DT::dataTableOutput(ns("table")),

        # Summary stats
        wellPanel(
            style = "background-color: #f8f9fa; margin-top: 10px;",
            uiOutput(ns("summary"))
        )
    )
}

#' Server logic for smart table
smart_table_server <- function(id, data, options = list()) {
    moduleServer(id, function(input, output, session) {

        # Render table with smart formatting
        output$table <- DT::renderDataTable({
            req(data())

            default_options <- list(
                pageLength = as.numeric(input$rows_per_page),
                searchHighlight = TRUE,
                dom = 'tip',
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all')
                )
            )

            # Merge with provided options
            table_options <- modifyList(default_options, options)

            datatable(
                data(),
                options = table_options,
                rownames = FALSE,
                filter = "none"
            )
        })

        # Custom search
        observeEvent(input$search, {
            DT::dataTableProxy("table") %>%
                DT::updateSearch(list(global = input$search))
        })

        # Summary statistics
        output$summary <- renderUI({
            req(data())

            numeric_cols <- names(data())[sapply(data(), is.numeric)]

            if(length(numeric_cols) > 0) {
                summary_stats <- data() %>%
                    summarise(across(all_of(numeric_cols),
                                     list(mean = ~mean(., na.rm = TRUE),
                                          sd = ~sd(., na.rm = TRUE)),
                                     .names = "{.col}_{.fn}"))

                tags$small(
                    class = "text-muted",
                    paste("Summary: Rows =", nrow(data()),
                          "| Columns =", ncol(data()),
                          "| Numeric cols =", length(numeric_cols))
                )
            }
        })

        # Export handler
        output$export <- downloadHandler(
            filename = function() {
                paste0("table_export_", Sys.Date(), ".csv")
            },
            content = function(file) {
                write.csv(data(), file, row.names = FALSE)
            }
        )
    })
}

# report_options_ui() removed — orphaned report config module with mismatched section names
# The live UI uses 9 sections defined in enhanced_bond_ui.R