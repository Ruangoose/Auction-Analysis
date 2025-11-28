# Insele Capital Partners - Enhanced SA Government Bond Analytics Dashboard
# Professional Fixed Income Analysis Platform with Advanced Features



# Install TinyTeX distribution
# tinytex::install_tinytex()

# Define custom theme and colors
insele_colors <- c(
    primary = "#1B3A6B",    # Navy blue
    secondary = "#5B8FA3",   # Teal
    accent = "#F39B3C",      # Orange
    light_teal = "#A8C5D0",
    dark_navy = "#0F2442",
    success = "#28a745",
    danger = "#dc3545",
    warning = "#ffc107"
)

custom_theme <- theme_minimal() +
    theme(
        text = element_text(family = "Arial"),
        plot.title = element_text(size = 14, face = "bold", color = insele_colors["primary"]),
        plot.subtitle = element_text(size = 11, color = "#4A5568"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(size = 9, color = "#718096", hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11, face = "bold")
    )

# =============================================================================
# NEW HELPER FUNCTIONS
# =============================================================================

# Technical Indicators Function
add_technicals <- function(data) {
    tryCatch({
        data <- data %>%
            group_by(bond) %>%
            arrange(date) %>%
            mutate(
                # Moving Averages
                ma_20 = rollapply(yield_to_maturity, 20, mean, fill = NA, align = "right", partial = TRUE),
                ma_50 = rollapply(yield_to_maturity, 50, mean, fill = NA, align = "right", partial = TRUE),

                # RSI (14-day)
                price_change = yield_to_maturity - lag(yield_to_maturity, 1),
                gain = ifelse(price_change > 0, price_change, 0),
                loss = ifelse(price_change < 0, abs(price_change), 0),
                avg_gain = rollapply(gain, 14, mean, fill = NA, align = "right", partial = TRUE),
                avg_loss = rollapply(loss, 14, mean, fill = NA, align = "right", partial = TRUE),
                rs = avg_gain / avg_loss,
                rsi = 100 - (100 / (1 + rs)),

                # MACD
                ema_12 = EMA(yield_to_maturity, n = 12),
                ema_26 = EMA(yield_to_maturity, n = 26),
                macd = ema_12 - ema_26,
                signal = EMA(macd, n = 9),
                macd_histogram = macd - signal,

                # Bollinger Bands
                bb_mean = rollapply(yield_to_maturity, 20, mean, fill = NA, align = "right", partial = TRUE),
                bb_sd = rollapply(yield_to_maturity, 20, sd, fill = NA, align = "right", partial = TRUE),
                bb_upper = bb_mean + 2 * bb_sd,
                bb_lower = bb_mean - 2 * bb_sd,

                # Support and Resistance (using rolling min/max)
                support = rollapply(yield_to_maturity, 50, min, fill = NA, align = "right", partial = TRUE),
                resistance = rollapply(yield_to_maturity, 50, max, fill = NA, align = "right", partial = TRUE)
            ) %>%
            ungroup()

        return(data)
    }, error = function(e) {
        message("Error in add_technicals: ", e$message)
        return(data)
    })
}

# Carry & Roll Calculation Function
calculate_carry_roll <- memoise(function(data, holding_periods = c(30, 90, 180, 360)) {
    tryCatch({
        # Ensure holding periods are sorted
        holding_periods <- sort(holding_periods)
        results <- list()

        for (i in seq_along(holding_periods)) {
            period <- holding_periods[i]
            period_label <- paste0(period, "d")

            carry_roll_data <- data %>%
                group_by(bond) %>%
                filter(date == max(date)) %>%
                mutate(
                    # Annual carry (coupon income)
                    annual_carry = coupon,
                    period_carry = (coupon * period / 365),

                    # Roll-down return (simplified calculation)
                    time_decay = period / 365,
                    new_duration = pmax(0, modified_duration - time_decay),

                    # Simplified roll return calculation
                    roll_return = ifelse(new_duration > 0 & !is.na(yield_to_maturity),
                                         (yield_to_maturity * 0.1 * new_duration / modified_duration),
                                         0),

                    # Total expected return
                    total_return = period_carry + roll_return,

                    # Breakeven yield change (bps)
                    breakeven_bps = ifelse(modified_duration > 0,
                                           (total_return / modified_duration) * 100,
                                           0),

                    # Add holding period label
                    holding_period = period_label,
                    holding_days = period
                ) %>%
                ungroup()

            results[[i]] <- carry_roll_data
        }

        # Combine all periods and ensure proper ordering
        if (length(results) > 0) {
            combined <- bind_rows(results)
            # Convert holding_period to factor with ordered levels
            combined$holding_period <- factor(combined$holding_period,
                                              levels = c("30d", "90d", "180d", "360d"),
                                              ordered = TRUE)
            return(combined)
        } else {
            return(data.frame())
        }
    }, error = function(e) {
        message("Error in calculate_carry_roll: ", e$message)
        return(data.frame(
            bond = character(),
            holding_period = character(),
            total_return = numeric(),
            period_carry = numeric(),
            roll_return = numeric(),
            breakeven_bps = numeric(),
            stringsAsFactors = FALSE
        ))
    })
})

# Bid-to-Cover Prediction Function
predict_bid_cover <- function(historical_data, bond_name, auction_size = NULL) {
    tryCatch({
        # Filter for specific bond
        bond_data <- historical_data %>%
            filter(bond == bond_name, !is.na(bid_to_cover)) %>%
            arrange(date)

        if (nrow(bond_data) < 5) {
            return(list(
                prediction = NA,
                lower_ci = NA,
                upper_ci = NA,
                confidence = "Low"
            ))
        }

        # Create features for prediction
        bond_data <- bond_data %>%
            mutate(
                trend = row_number(),
                ma_btc = rollapply(bid_to_cover, 3, mean, fill = NA, align = "right", partial = TRUE),
                volatility = rollapply(bid_to_cover, 3, sd, fill = NA, align = "right", partial = TRUE)
            )

        # Simple linear model with trend and seasonality
        model <- lm(bid_to_cover ~ trend + sin(trend * 2 * pi / 4) + cos(trend * 2 * pi / 4),
                    data = bond_data)

        # Predict next value
        next_trend <- nrow(bond_data) + 1
        new_data <- data.frame(
            trend = next_trend,
            sin_term = sin(next_trend * 2 * pi / 4),
            cos_term = cos(next_trend * 2 * pi / 4)
        )
        names(new_data) <- c("trend", "sin(trend * 2 * pi/4)", "cos(trend * 2 * pi/4)")

        prediction <- predict(model, new_data, interval = "prediction", level = 0.95)

        # Determine confidence based on R-squared
        r_squared <- summary(model)$r.squared
        confidence <- case_when(
            r_squared > 0.7 ~ "High",
            r_squared > 0.4 ~ "Medium",
            TRUE ~ "Low"
        )

        return(list(
            prediction = prediction[1],
            lower_ci = prediction[2],
            upper_ci = prediction[3],
            confidence = confidence,
            r_squared = r_squared
        ))
    }, error = function(e) {
        message("Error in predict_bid_cover: ", e$message)
        return(list(prediction = NA, lower_ci = NA, upper_ci = NA, confidence = "Error"))
    })
}

# Surprise Index Calculation (vectorized)
calculate_surprise_index <- function(actual, predicted) {
    # Vectorized operation to handle multiple values
    surprise <- ifelse(is.na(actual) | is.na(predicted),
                       NA,
                       (actual - predicted) / predicted * 100)
    return(surprise)
}

# Butterfly Opportunity Detection
detect_butterfly_opportunities <- function(data) {
    tryCatch({
        opportunities <- data.frame()

        # Get latest data
        latest <- data %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            ungroup() %>%
            arrange(modified_duration)

        if (nrow(latest) < 3) return(opportunities)

        # Check all possible butterflies (need at least 3 bonds)
        for (i in 1:(nrow(latest) - 2)) {
            for (j in (i+1):(nrow(latest) - 1)) {
                for (k in (j+1):nrow(latest)) {
                    short1 <- latest[i, ]
                    long <- latest[j, ]
                    short2 <- latest[k, ]

                    # Calculate butterfly value (2*middle - wings)
                    butterfly_value <- 2 * long$yield_to_maturity -
                        short1$yield_to_maturity -
                        short2$yield_to_maturity

                    # Duration-weighted butterfly
                    duration_weight <- long$modified_duration /
                        (short1$modified_duration + short2$modified_duration) * 2

                    # Historical average (if available)
                    hist_avg <- 0  # Placeholder for historical average

                    # Calculate z-score
                    z_score <- butterfly_value / 0.3  # Assuming 30bps standard deviation

                    if (abs(z_score) > 1.5) {
                        opportunities <- rbind(opportunities, data.frame(
                            type = ifelse(z_score > 0, "Bullet", "Barbell"),
                            short1 = short1$bond,
                            long = long$bond,
                            short2 = short2$bond,
                            value_bps = round(butterfly_value * 100, 1),
                            z_score = round(z_score, 2),
                            signal_strength = case_when(
                                abs(z_score) > 2.5 ~ "Strong",
                                abs(z_score) > 2.0 ~ "Medium",
                                TRUE ~ "Weak"
                            ),
                            expected_profit_bps = round(abs(butterfly_value) * 100 * 0.5, 1)
                        ))
                    }
                }
            }
        }

        # Sort by signal strength
        opportunities <- opportunities %>%
            arrange(desc(abs(z_score))) %>%
            head(5)

        return(opportunities)
    }, error = function(e) {
        message("Error in detect_butterfly_opportunities: ", e$message)
        return(data.frame())
    })
}

# Smart Insights Generation Function
generate_smart_insights <- function(data, processed_data, historical_data) {
    tryCatch({
        insights <- list()

        # 1. Relative Value Alerts
        if (!is.null(processed_data) && nrow(processed_data) > 0) {
            cheap_bonds <- processed_data %>%
                filter(!is.na(z_score)) %>%
                filter(z_score < -1.5) %>%
                arrange(z_score) %>%
                head(1)

            if (nrow(cheap_bonds) > 0) {
                insights[[length(insights) + 1]] <- list(
                    icon = "📈",
                    text = sprintf("%s trading %.0fbps cheap to curve (z-score: %.1f)",
                                   cheap_bonds$bond[1],
                                   abs(cheap_bonds$spread_to_curve[1]),
                                   cheap_bonds$z_score[1]),
                    priority = 9
                )
            }

            rich_bonds <- processed_data %>%
                filter(!is.na(z_score)) %>%
                filter(z_score > 1.5) %>%
                arrange(desc(z_score)) %>%
                head(1)

            if (nrow(rich_bonds) > 0) {
                insights[[length(insights) + 1]] <- list(
                    icon = "⚠️",
                    text = sprintf("%s appears rich at %.0fbps above fair value",
                                   rich_bonds$bond[1],
                                   rich_bonds$spread_to_curve[1]),
                    priority = 7
                )
            }
        }

        # 2. Auction Trends
        if (!is.null(data) && nrow(data) > 0) {
            recent_auctions <- data %>%
                filter(!is.na(bid_to_cover),
                       date >= today() - days(30)) %>%
                group_by(bond) %>%
                arrange(date) %>%
                mutate(btc_change = bid_to_cover - lag(bid_to_cover, 1)) %>%
                filter(!is.na(btc_change))

            declining_btc <- recent_auctions %>%
                group_by(bond) %>%
                summarise(
                    consecutive_declines = sum(btc_change < 0),
                    avg_decline = mean(btc_change[btc_change < 0], na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                filter(consecutive_declines >= 2)

            if (nrow(declining_btc) > 0) {
                worst <- declining_btc[1, ]
                insights[[length(insights) + 1]] <- list(
                    icon = "📉",
                    text = sprintf("Bid-to-cover declining for %d consecutive %s auctions",
                                   worst$consecutive_declines, worst$bond),
                    priority = 8
                )
            }
        }

        # 3. Curve Analysis
        if (!is.null(processed_data) && nrow(processed_data) > 0) {
            curve_metrics <- processed_data %>%
                summarise(
                    short_yield = mean(yield_to_maturity[modified_duration < 5], na.rm = TRUE),
                    long_yield = mean(yield_to_maturity[modified_duration > 10], na.rm = TRUE)
                )

            if (!is.na(curve_metrics$short_yield) && !is.na(curve_metrics$long_yield)) {
                steepness <- (curve_metrics$long_yield - curve_metrics$short_yield) * 100

                # Check historical context
                if (!is.null(historical_data)) {
                    hist_steepness <- historical_data %>%
                        group_by(date) %>%
                        summarise(
                            steepness = (mean(yield_to_maturity[modified_duration > 10], na.rm = TRUE) -
                                             mean(yield_to_maturity[modified_duration < 5], na.rm = TRUE)) * 100,
                            .groups = "drop"
                        ) %>%
                        filter(!is.na(steepness))

                    if (nrow(hist_steepness) > 0) {
                        percentile <- ecdf(hist_steepness$steepness)(steepness)

                        if (percentile > 0.9 || percentile < 0.1) {
                            insights[[length(insights) + 1]] <- list(
                                icon = "📊",
                                text = sprintf("Curve steepness at %.0fbps (%dth percentile historically)",
                                               steepness, round(percentile * 100)),
                                priority = 6
                            )
                        }
                    }
                }
            }
        }

        # 4. Butterfly Opportunities
        butterflies <- detect_butterfly_opportunities(data)
        if (nrow(butterflies) > 0) {
            best_butterfly <- butterflies[1, ]
            insights[[length(insights) + 1]] <- list(
                icon = "💡",
                text = sprintf("Butterfly opportunity: Long %s, Short %s/%s (%.1fbps value)",
                               best_butterfly$long,
                               best_butterfly$short1,
                               best_butterfly$short2,
                               best_butterfly$value_bps),
                priority = 8
            )
        }

        # 5. Technical Breakouts
        if ("bb_upper" %in% names(data) && "bb_lower" %in% names(data)) {
            latest_tech <- data %>%
                group_by(bond) %>%
                filter(date == max(date)) %>%
                ungroup()

            breakouts <- latest_tech %>%
                filter(!is.na(bb_upper) & !is.na(bb_lower)) %>%
                mutate(
                    breakout = case_when(
                        yield_to_maturity > bb_upper ~ "above",
                        yield_to_maturity < bb_lower ~ "below",
                        TRUE ~ "none"
                    )
                ) %>%
                filter(breakout != "none")

            if (nrow(breakouts) > 0) {
                insights[[length(insights) + 1]] <- list(
                    icon = "🎯",
                    text = sprintf("%s broke %s Bollinger Band at %.2f%%",
                                   breakouts$bond[1],
                                   breakouts$breakout[1],
                                   breakouts$yield_to_maturity[1]),
                    priority = 7
                )
            }
        }

        # Sort by priority and return top 5
        if (length(insights) > 0) {
            insights <- insights[order(sapply(insights, function(x) -x$priority))]
            insights <- head(insights, 5)
        }

        return(insights)
    }, error = function(e) {
        message("Error in generate_smart_insights: ", e$message)
        return(list())
    })
}

# Helper function to calculate fair value
calculate_fair_value <- function(data, method = "smooth.spline") {
    if(nrow(data) < 4) return(data)

    if(method == "smooth.spline") {
        model <- smooth.spline(x = data$modified_duration,
                               y = data$yield_to_maturity,
                               spar = 0.6)
        data$fitted_yield <- predict(model, data$modified_duration)$y
    } else if(method == "loess") {
        model <- loess(yield_to_maturity ~ modified_duration, data = data, span = 0.75)
        data$fitted_yield <- predict(model, data$modified_duration)
    } else {
        model <- lm(yield_to_maturity ~ ns(modified_duration, 3), data = data)
        data$fitted_yield <- predict(model, data)
    }

    data$spread_to_curve <- (data$yield_to_maturity - data$fitted_yield) * 100
    return(data)
}

calculate_relative_value <- function(data, lookback = 60) {
    result <- data %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            hist_mean_spread = rollapply(spread_to_curve, lookback, mean,
                                         fill = NA, align = "right", partial = TRUE),
            hist_sd_spread = rollapply(spread_to_curve, lookback, sd,
                                       fill = NA, align = "right", partial = TRUE),
            # Enhanced z-score calculation with better NA handling
            z_score = case_when(
                is.na(hist_sd_spread) ~ NA_real_,
                hist_sd_spread == 0 ~ 0,
                hist_sd_spread < 0.01 ~ 0,  # Treat very small std as 0
                TRUE ~ (spread_to_curve - hist_mean_spread) / hist_sd_spread
            ),
            percentile_rank = percent_rank(spread_to_curve)
        ) %>%
        ungroup()

    # Add data validation message
    if (sum(!is.na(result$z_score)) == 0) {
        message("Warning: No valid z-scores calculated. Check data completeness.")
    }

    return(result)
}

validate_data <- function(data) {
    required_cols <- c("date", "bond", "yield_to_maturity", "modified_duration")
    missing_cols <- setdiff(required_cols, names(data))

    if(length(missing_cols) > 0) {
        showNotification(
            paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
            type = "warning"
        )
        return(FALSE)
    }

    if(nrow(data) < 10) {
        showNotification(
            "Insufficient data for analysis. Need at least 10 observations.",
            type = "warning"
        )
        return(FALSE)
    }

    return(TRUE)
}

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
    dashboardHeader(
        title = tags$div(
            style = "display: flex; align-items: center; width: 100%;",
            tags$img(src = "insele_logo.png", height = "35px", style = "margin-right: 10px;"),
            tags$span("INSELE CAPITAL PARTNERS", style = "font-weight: bold; font-size: 18px;"),
            tags$span("The Power of Partnership",
                      style = paste0("color: ", insele_colors["accent"],
                                     "; margin-left: 15px; font-style: italic; font-size: 14px;"))
        ),
        titleWidth = 450,

        # Notification panel for trading signals
        tags$li(
            class = "dropdown",
            tags$div(
                id = "notification_panel",
                style = "padding: 10px; min-width: 300px;",
                uiOutput("trading_signals_notifications")
            )
        )
    ),

    dashboardSidebar(
        width = 280,

        # Date Selection
        tags$div(
            style = "padding: 15px;",
            h4("Analysis Parameters", style = paste0("color: ", insele_colors["primary"], ";")),

            dateRangeInput(
                "date_range",
                "Date Range:",
                start = floor_date(today(), "year"),
                end = today(),
                format = "yyyy-mm-dd",
                width = "100%"
            ),

            fluidRow(
                column(3, actionButton("ytd_btn", "YTD", class = "btn-sm", width = "100%")),
                column(3, actionButton("qtr_btn", "QTR", class = "btn-sm", width = "100%")),
                column(3, actionButton("mth_btn", "MTH", class = "btn-sm", width = "100%")),
                column(3, actionButton("wk_btn", "WK", class = "btn-sm", width = "100%"))
            )
        ),

        # Bond Selection
        tags$div(
            style = "padding: 15px;",
            h5("Bond Selection", style = paste0("color: ", insele_colors["primary"], ";")),

            pickerInput(
                "selected_bonds",
                NULL,
                choices = NULL,  # Will be updated dynamically
                selected = NULL,  # Will be updated dynamically
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    `selected-text-format` = "count > 3",
                    `count-selected-text` = "{0} bonds selected"
                )
            ),

            fluidRow(
                column(6, actionButton("select_short", "Short", class = "btn-sm", width = "100%")),
                column(6, actionButton("select_long", "Long", class = "btn-sm", width = "100%"))
            )
        ),

        # Analysis Options
        tags$div(
            style = "padding: 15px;",
            h5("Analysis Options", style = paste0("color: ", insele_colors["primary"], ";")),

            selectInput(
                "interpolation_method",
                "Interpolation:",
                choices = list(
                    "Smooth Spline" = "smooth.spline",
                    "Cubic Spline" = "cubic",
                    "LOESS" = "loess"
                ),
                selected = "smooth.spline",
                width = "100%"
            ),

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
                "zscore_window",
                "Z-Score Window:",
                min = 10,
                max = 90,
                value = 30,
                step = 5,
                post = " days",
                width = "100%"
            ),

            # New technical indicators toggle
            checkboxInput(
                "show_technicals",
                "Show Technical Indicators",
                value = FALSE
            )
        ),

        # Export Options
        tags$div(
            style = "padding: 15px;",
            h5("Export Options", style = paste0("color: ", insele_colors["primary"], ";")),

            downloadButton("export_current", "Export Current View",
                           class = "btn-primary btn-sm", width = "100%"),
            br(), br(),
            downloadButton("generate_report", "Generate PDF Report",
                           class = "btn-secondary btn-sm", width = "100%")
        )
    ),

    dashboardBody(
        # Custom CSS
        tags$head(
            tags$style(HTML(paste0("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .small-box {
          border-radius: 8px;
          box-shadow: 0 0 10px rgba(0,0,0,0.05);
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: ", insele_colors["primary"], ";
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 0 10px rgba(0,0,0,0.05);
        }
        .box-header {
          background-color: ", insele_colors["primary"], ";
          color: white;
          border-radius: 8px 8px 0 0;
        }
        .box-header .box-title {
          color: white;
          font-weight: bold;
        }
        .smart-insights-box {
          background: linear-gradient(135deg, ", insele_colors["primary"], " 0%, ", insele_colors["secondary"], " 100%);
          color: white;
          border-radius: 8px;
          padding: 15px;
          margin-bottom: 20px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        .insight-item {
          padding: 8px 12px;
          background: rgba(255,255,255,0.1);
          border-radius: 6px;
          margin: 5px 0;
          display: flex;
          align-items: center;
        }
        .insight-icon {
          font-size: 20px;
          margin-right: 10px;
        }
        .sentiment-gauge {
          position: relative;
          width: 200px;
          height: 100px;
          margin: 20px auto;
        }
      ")))
        ),

        # NEW: Smart Summary Box (Top Priority)
        fluidRow(
            column(12,
                   div(class = "smart-insights-box",
                       h3("Market Intelligence",
                          style = "margin-top: 0; margin-bottom: 15px; font-weight: bold;"),
                       uiOutput("smart_insights_content")
                   )
            )
        ),

        # Key Metrics Row
        fluidRow(
            valueBoxOutput("active_bonds_box"),
            valueBoxOutput("avg_bid_cover_box"),
            valueBoxOutput("ytd_issuance_box"),
            valueBoxOutput("curve_steepness_box")
        ),

        # Main Tabbed Interface
        fluidRow(
            column(12,
                   tabBox(
                       id = "main_tabs",
                       width = 12,

                       # Relative Value Tab
                       tabPanel(
                           "Relative Value",
                           icon = icon("chart-line"),

                           fluidRow(
                               box(
                                   title = "Yield Curve & Fair Value Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotOutput("yield_curve_plot", height = "450px"),
                                   downloadButton("download_yield_curve", "Download", class = "btn-sm")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Rich/Cheap Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("rich_cheap_plot", height = "400px"),
                                   downloadButton("download_rich_cheap", "Download", class = "btn-sm")
                               ),
                               box(
                                   title = "Z-Score Distribution",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("zscore_plot", height = "400px"),
                                   downloadButton("download_zscore", "Download", class = "btn-sm")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Relative Value Metrics",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("relative_value_table")
                               )
                           )
                       ),

                       # Auction Analytics Tab (Enhanced)
                       tabPanel(
                           "Auction Analytics",
                           icon = icon("gavel"),

                           # NEW: Prediction Box and Sentiment Gauge
                           fluidRow(
                               box(
                                   title = "Auction Intelligence",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   h4("Next Auction Predictions"),
                                   pickerInput(
                                       "auction_pred_bonds",
                                       "Select bonds for prediction (max 3):",
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = TRUE,
                                       options = list(
                                           `max-options` = 3,
                                           `max-options-text` = "Maximum 3 bonds"
                                       )
                                   ),
                                   uiOutput("auction_prediction_display"),
                                   hr(),
                                   h4("Auction Sentiment"),
                                   plotOutput("sentiment_gauge_plot", height = "150px")
                               ),
                               box(
                                   title = "Surprise Index",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   tags$div(
                                       style = "padding: 8px; background-color: #e8f4f8; border-radius: 5px; margin-bottom: 10px;",
                                       tags$p(
                                           tags$strong("What is the Surprise Index?"),
                                           "This metric compares actual auction results to expected values based on recent history. Large positive surprises indicate unexpectedly strong demand.",
                                           style = "font-size: 11px; margin: 0;"
                                       )
                                   ),
                                   plotOutput("surprise_index_plot", height = "300px")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Supply and Demand Dynamics",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotOutput("supply_demand_plot", height = "450px"),
                                   downloadButton("download_supply_demand", "Download", class = "btn-sm")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Historical Auction Performance",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("auction_hist_plot", height = "400px"),
                                   downloadButton("download_auction_hist", "Download", class = "btn-sm")
                               ),
                               box(
                                   title = "Bid-to-Cover Trends",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("btc_trends_plot", height = "400px"),
                                   downloadButton("download_btc_trends", "Download", class = "btn-sm")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Auction Statistics",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("auction_stats_table")
                               )
                           )
                       ),

                       # Historical Analysis Tab (Enhanced with Technicals)
                       tabPanel(
                           "Historical Analysis",
                           icon = icon("history"),

                           fluidRow(
                               box(
                                   title = "Technical Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   selectInput("tech_analysis_bond", "Select Bond for Analysis:",
                                               choices = NULL, selected = NULL),
                                   plotOutput("technical_indicators_plot", height = "450px"),
                                   downloadButton("download_technicals", "Download", class = "btn-sm")
                               )
                           ),

                           # NEW: Technical Indicators Chart
                           fluidRow(
                               box(
                                   title = "Technical Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotOutput("technical_indicators_plot", height = "450px"),
                                   downloadButton("download_technicals", "Download", class = "btn-sm")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Term Structure Evolution",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotOutput("term_structure_plot", height = "450px"),
                                   downloadButton("download_term_structure", "Download", class = "btn-sm")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Yield Percentile Rankings",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("percentile_plot", height = "400px"),
                                   downloadButton("download_percentile", "Download", class = "btn-sm")
                               ),
                               box(
                                   title = "Correlation Matrix",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("correlation_plot", height = "400px"),
                                   downloadButton("download_correlation", "Download", class = "btn-sm")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Convexity Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotOutput("convexity_plot", height = "400px"),
                                   downloadButton("download_convexity", "Download", class = "btn-sm")
                               )
                           ),

                           # NEW: Technical Indicators Table
                           fluidRow(
                               box(
                                   title = "Current Technical Readings",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("technical_indicators_table")
                               )
                           )
                       ),

                       # NEW: Carry & Roll Tab
                       tabPanel(
                           "Carry & Roll",
                           icon = icon("calculator"),

                           fluidRow(
                               box(
                                   title = "Carry & Roll Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   h4("Expected Returns Heatmap"),
                                   plotOutput("carry_roll_heatmap", height = "400px"),
                                   downloadButton("download_carry_roll", "Download", class = "btn-sm")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Breakeven Analysis",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   plotOutput("breakeven_analysis_plot", height = "400px")
                               ),
                               box(
                                   title = "Optimal Holding Period",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 6,
                                   tags$div(
                                       style = "padding: 10px; background-color: #f0f0f0; border-radius: 5px; margin-bottom: 10px;",
                                       tags$p(
                                           tags$strong("Understanding This Chart:"),
                                           "The solid blue line shows total return for each holding period. The orange dashed line shows annualized return. The optimal period balances total return with time efficiency - typically where annualized return peaks.",
                                           style = "font-size: 12px; margin: 0;"
                                       )
                                   ),
                                   selectInput("carry_roll_bond", "Select Bond:", choices = NULL),
                                   plotOutput("optimal_holding_plot", height = "350px")
                               )
                           ),

                           fluidRow(
                               box(
                                   title = "Total Return Attribution",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("return_attribution_table")
                               )
                           ),

                           # NEW: Butterfly Opportunities
                           fluidRow(
                               box(
                                   title = "Butterfly Trading Opportunities",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   DT::dataTableOutput("butterfly_opportunities_table")
                               )
                           )
                       ),

                       # Report Generation Tab
                       tabPanel(
                           "Report Generation",
                           icon = icon("file-pdf"),

                           fluidRow(
                               box(
                                   title = "Report Configuration",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,

                                   h4("Select Components to Include:"),

                                   fluidRow(
                                       column(6,
                                              checkboxInput("include_summary", "Executive Summary", value = TRUE),
                                              checkboxInput("include_relative", "Relative Value Analysis", value = TRUE),
                                              checkboxInput("include_auction", "Auction Analytics", value = TRUE),
                                              checkboxInput("include_technicals", "Technical Analysis", value = TRUE)
                                       ),
                                       column(6,
                                              checkboxInput("include_historical", "Historical Analysis", value = TRUE),
                                              checkboxInput("include_carry_roll", "Carry & Roll Analysis", value = TRUE),
                                              checkboxInput("include_data_tables", "Data Tables", value = TRUE),
                                              checkboxInput("include_disclaimer", "Compliance Disclaimer", value = TRUE)
                                       )
                                   ),

                                   hr(),

                                   h4("Report Details:"),

                                   fluidRow(
                                       column(6,
                                              textInput("client_name", "Client Name:",
                                                        placeholder = "Enter client name"),
                                              textInput("report_title", "Report Title:",
                                                        value = "SA Government Bond Weekly Analysis")
                                       ),
                                       column(6,
                                              dateInput("report_date", "Report Date:", value = today()),
                                              selectInput("report_format", "Format:",
                                                          choices = c("PDF" = "pdf", "HTML" = "html"),
                                                          selected = "pdf")
                                       )
                                   ),

                                   br(),

                                   fluidRow(
                                       column(6,
                                              actionButton("preview_report", "Preview Report",
                                                           class = "btn-info", icon = icon("eye"))
                                       ),
                                       column(6,
                                              downloadButton("download_report", "Generate Report",
                                                             class = "btn-success")
                                       )
                                   )
                               )
                           )
                       )
                   )
            )
        )
    )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

    # Initialize reactive values for notifications
    notifications <- reactiveValues(
        alerts = list(),
        alert_counter = 0
    )

    # Update bond choices dynamically based on available data
    observe({
        if (exists("full_df")) {
            available_bonds <- unique(full_df$bond)
            updatePickerInput(
                session,
                "selected_bonds",
                choices = available_bonds,
                selected = available_bonds
            )
        }
    })

    # *** ADD THE NEW AUCTION PREDICTION BOND SELECTOR HERE ***
    # Update auction prediction bond selector
    observe({
        req(filtered_data())

        # Get most frequently auctioned bonds
        freq_bonds <- filtered_data() %>%
            filter(!is.na(bid_to_cover)) %>%
            count(bond) %>%
            arrange(desc(n)) %>%
            pull(bond)

        if (length(freq_bonds) > 0) {
            updatePickerInput(
                session,
                "auction_pred_bonds",
                choices = freq_bonds,
                selected = head(freq_bonds, 3)
            )
        }
    })

    # Reactive data processing
    filtered_data <- reactive({
        req(input$selected_bonds, input$date_range)

        if (!exists("full_df")) {
            showNotification("Data not loaded", type = "error")
            return(NULL)
        }

        full_df %>%
            filter(
                bond %in% input$selected_bonds,
                date >= input$date_range[1],
                date <= input$date_range[2]
            )
    })

    # Enhanced processed data with technical indicators
    processed_data <- reactive({
        req(filtered_data())

        data <- filtered_data()

        if(!validate_data(data)) return(NULL)

        # Add technical indicators if requested
        if (input$show_technicals) {
            data <- add_technicals(data)
        }

        # Get latest data for each bond
        latest_data <- data %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            ungroup()

        # Calculate fair value and relative value metrics
        latest_data %>%
            calculate_fair_value(method = input$interpolation_method) %>%
            calculate_relative_value(lookback = input$lookback_days)
    })

    # Smart Insights Reactive
    smart_insights <- reactive({
        req(filtered_data())
        generate_smart_insights(
            filtered_data(),
            processed_data(),
            filtered_data()
        )
    })

    # Render Smart Insights UI
    output$smart_insights_content <- renderUI({
        insights <- smart_insights()

        if (length(insights) == 0) {
            return(div(
                class = "insight-item",
                span(class = "insight-icon", "ℹ️"),
                span("Analyzing market data...")
            ))
        }

        insight_divs <- lapply(insights, function(insight) {
            div(
                class = "insight-item",
                span(class = "insight-icon", insight$icon),
                span(insight$text)
            )
        })

        do.call(tagList, insight_divs)
    })

    # Trading Signals Notifications
    observe({
        req(processed_data())

        # Check for new breakout signals
        if ("bb_upper" %in% names(processed_data())) {
            breakouts <- processed_data() %>%
                filter(!is.na(bb_upper) & !is.na(bb_lower)) %>%
                filter(yield_to_maturity > bb_upper | yield_to_maturity < bb_lower)

            if (nrow(breakouts) > 0) {
                for (i in 1:nrow(breakouts)) {
                    notifications$alert_counter <- notifications$alert_counter + 1
                    alert_id <- paste0("alert_", notifications$alert_counter)

                    notifications$alerts[[alert_id]] <- list(
                        id = alert_id,
                        type = "breakout",
                        bond = breakouts$bond[i],
                        message = sprintf("%s: Bollinger Band breakout at %.2f%%",
                                          breakouts$bond[i], breakouts$yield_to_maturity[i]),
                        timestamp = Sys.time()
                    )

                    showNotification(
                        notifications$alerts[[alert_id]]$message,
                        type = "warning",
                        duration = 10
                    )
                }
            }
        }
    })

    # Carry & Roll Calculations
    carry_roll_data <- reactive({
        req(processed_data())
        calculate_carry_roll(processed_data())
    })

    # Update carry/roll bond selector
    observe({
        bonds <- unique(processed_data()$bond)
        updateSelectInput(session, "carry_roll_bond", choices = bonds, selected = bonds[1])
    })

    # Date range quick selectors
    observeEvent(input$ytd_btn, {
        updateDateRangeInput(session, "date_range",
                             start = floor_date(today(), "year"),
                             end = today())
    })

    observeEvent(input$qtr_btn, {
        updateDateRangeInput(session, "date_range",
                             start = today() - months(3),
                             end = today())
    })

    observeEvent(input$mth_btn, {
        updateDateRangeInput(session, "date_range",
                             start = today() - months(1),
                             end = today())
    })

    observeEvent(input$wk_btn, {
        updateDateRangeInput(session, "date_range",
                             start = today() - weeks(1),
                             end = today())
    })

    # Bond selection helpers
    observeEvent(input$select_short, {
        if (exists("full_df")) {
            short_bonds <- full_df %>%
                group_by(bond) %>%
                summarise(avg_dur = mean(modified_duration, na.rm = TRUE)) %>%
                arrange(avg_dur) %>%
                head(5) %>%
                pull(bond)

            updatePickerInput(session, "selected_bonds", selected = short_bonds)
        }
    })

    observeEvent(input$select_long, {
        if (exists("full_df")) {
            long_bonds <- full_df %>%
                group_by(bond) %>%
                summarise(avg_dur = mean(modified_duration, na.rm = TRUE)) %>%
                arrange(desc(avg_dur)) %>%
                head(5) %>%
                pull(bond)

            updatePickerInput(session, "selected_bonds", selected = long_bonds)
        }
    })

    # Value Boxes
    output$active_bonds_box <- renderValueBox({
        valueBox(
            value = length(input$selected_bonds),
            subtitle = "Active Bonds",
            icon = icon("chart-bar"),
            color = "blue"
        )
    })

    output$avg_bid_cover_box <- renderValueBox({
        avg_btc <- filtered_data() %>%
            filter(!is.na(bid_to_cover)) %>%
            summarise(avg = mean(bid_to_cover, na.rm = TRUE)) %>%
            pull(avg)

        valueBox(
            value = sprintf("%.2fx", avg_btc),
            subtitle = "Avg Bid/Cover",
            icon = icon("balance-scale"),
            color = "green"
        )
    })

    output$ytd_issuance_box <- renderValueBox({
        ytd_total <- filtered_data() %>%
            filter(!is.na(allocation),
                   year(date) == year(today())) %>%
            summarise(total = sum(allocation, na.rm = TRUE) / 1e9) %>%
            pull(total)

        valueBox(
            value = sprintf("R%.0fbn", ytd_total),
            subtitle = "YTD Issuance",
            icon = icon("coins"),
            color = "yellow"
        )
    })

    output$curve_steepness_box <- renderValueBox({
        if(!is.null(processed_data())) {
            steepness <- processed_data() %>%
                summarise(
                    short = mean(yield_to_maturity[modified_duration < 5], na.rm = TRUE),
                    long = mean(yield_to_maturity[modified_duration > 10], na.rm = TRUE)
                ) %>%
                mutate(steepness = (long - short) * 100) %>%
                pull(steepness)

            valueBox(
                value = sprintf("%.0fbps", steepness),
                subtitle = "Curve Steepness",
                icon = icon("chart-area"),
                color = "purple"
            )
        } else {
            valueBox(
                value = "N/A",
                subtitle = "Curve Steepness",
                icon = icon("chart-area"),
                color = "purple"
            )
        }
    })

    # ==========================================================================
    # ENHANCED AND NEW VISUALIZATIONS
    # ==========================================================================

    # 1. Enhanced Yield Curve Plot with Technical Overlays
    output$yield_curve_plot <- renderPlot({
        req(processed_data())

        data <- processed_data()

        # Create spline for smooth curve
        spline_data <- data.frame(
            modified_duration = seq(min(data$modified_duration),
                                    max(data$modified_duration),
                                    length.out = 100)
        )

        if(input$interpolation_method == "smooth.spline") {
            model <- smooth.spline(x = data$modified_duration,
                                   y = data$yield_to_maturity,
                                   spar = 0.6)
            spline_data$yield <- predict(model, spline_data$modified_duration)$y
        } else if(input$interpolation_method == "loess") {
            model <- loess(yield_to_maturity ~ modified_duration, data = data, span = 0.75)
            spline_data$yield <- predict(model, spline_data$modified_duration)
        } else {
            model <- lm(yield_to_maturity ~ ns(modified_duration, 3), data = data)
            spline_data$yield <- predict(model, spline_data)
        }

        p <- ggplot(data, aes(x = modified_duration, y = yield_to_maturity)) +
            geom_line(data = spline_data,
                      aes(x = modified_duration, y = yield),
                      color = insele_colors["primary"],
                      size = 1.5,
                      linetype = "dashed",
                      alpha = 0.7)

        # Add Bollinger Bands if technicals are enabled
        if (input$show_technicals && "bb_upper" %in% names(data)) {
            p <- p +
                geom_ribbon(aes(ymin = bb_lower, ymax = bb_upper),
                            alpha = 0.1, fill = insele_colors["secondary"])
        }

        p <- p +
            geom_point(aes(color = spread_to_curve), size = 4) +
            geom_text(aes(label = bond), vjust = -1, size = 3.5) +
            scale_color_gradient2(
                low = "#28a745",
                mid = "#6c757d",
                high = "#dc3545",
                midpoint = 0,
                name = "Spread to Curve (bps)",
                limits = c(-50, 50)
            ) +
            scale_x_continuous(breaks = pretty_breaks(n = 8)) +
            scale_y_continuous(breaks = pretty_breaks(n = 8),
                               labels = function(x) paste0(x, "%")) +
            labs(
                title = "South African Government Bond Yield Curve",
                subtitle = paste("Relative Value Analysis as of", format(max(data$date), "%B %d, %Y")),
                x = "Modified Duration (years)",
                y = "Yield to Maturity",
                caption = "Source: Insele Capital Partners | Broking Services"
            ) +
            custom_theme +
            theme(legend.position = "bottom")

        print(p)
    })

    # Update technical analysis bond selector
    observe({
        bonds <- input$selected_bonds
        if (!is.null(bonds) && length(bonds) > 0) {
            current_selection <- isolate(input$tech_analysis_bond)
            if (is.null(current_selection) || !(current_selection %in% bonds)) {
                updateSelectInput(session, "tech_analysis_bond",
                                  choices = bonds,
                                  selected = bonds[1])
            } else {
                updateSelectInput(session, "tech_analysis_bond",
                                  choices = bonds,
                                  selected = current_selection)
            }
        }
    })

    # Update technical indicators plot
    output$technical_indicators_plot <- renderPlot({
        req(filtered_data(), input$tech_analysis_bond)

        if (!input$show_technicals) {
            data_with_tech <- add_technicals(filtered_data())
        } else {
            data_with_tech <- filtered_data()
        }

        # Use the selected bond from the dropdown
        bond_data <- data_with_tech %>%
            filter(bond == input$tech_analysis_bond)

        if (nrow(bond_data) == 0) {
            plot.new()
            text(0.5, 0.5, paste("No data available for", input$tech_analysis_bond), cex = 1.5)
            return()
        }

        # Create multi-panel plot
        p1 <- ggplot(bond_data, aes(x = date)) +
            geom_line(aes(y = yield_to_maturity), color = insele_colors["primary"], size = 1) +
            geom_line(aes(y = ma_20), color = insele_colors["secondary"], linetype = "dashed") +
            geom_line(aes(y = ma_50), color = insele_colors["accent"], linetype = "dashed") +
            geom_ribbon(aes(ymin = bb_lower, ymax = bb_upper), alpha = 0.1, fill = insele_colors["light_teal"]) +
            labs(title = paste("Technical Analysis:", input$tech_analysis_bond),
                 y = "Yield (%)", x = "") +
            custom_theme

        # Rest of the plot code remains the same...
        p2 <- ggplot(bond_data, aes(x = date, y = rsi)) +
            geom_line(color = insele_colors["primary"], size = 1) +
            geom_hline(yintercept = c(30, 70), linetype = "dashed", alpha = 0.5) +
            labs(y = "RSI", x = "") +
            custom_theme

        p3 <- ggplot(bond_data, aes(x = date)) +
            geom_line(aes(y = macd), color = insele_colors["primary"], size = 1) +
            geom_line(aes(y = signal), color = insele_colors["accent"], linetype = "dashed") +
            geom_bar(aes(y = macd_histogram), stat = "identity", fill = insele_colors["secondary"], alpha = 0.5) +
            labs(y = "MACD", x = "Date") +
            custom_theme

        gridExtra::grid.arrange(p1, p2, p3, ncol = 1, heights = c(3, 1.5, 1.5))
    })

    # NEW: Carry & Roll Heatmap
    output$carry_roll_heatmap <- renderPlot({
        req(carry_roll_data())

        # Check if data has required columns
        if (!"holding_period" %in% names(carry_roll_data())) {
            plot.new()
            text(0.5, 0.5, "Error: holding_period column missing", cex = 1.5)
            return()
        }

        heatmap_data <- carry_roll_data() %>%
            select(bond, holding_period, total_return) %>%
            filter(!is.na(total_return))

        if (nrow(heatmap_data) == 0) {
            plot.new()
            text(0.5, 0.5, "No carry & roll data available", cex = 1.5)
            return()
        }

        # Create heatmap using ggplot2
        p <- ggplot(heatmap_data, aes(x = holding_period, y = bond, fill = total_return)) +
            geom_tile(color = "white", size = 0.5) +
            geom_text(aes(label = sprintf("%.2f%%", total_return)), color = "white", size = 3) +
            scale_fill_gradient2(
                low = insele_colors["danger"],
                mid = "white",
                high = insele_colors["success"],
                midpoint = 0,
                name = "Expected\nReturn (%)"
            ) +
            labs(
                title = "Expected Total Returns by Holding Period",
                x = "Holding Period",
                y = "Bond",
                caption = "Includes carry and roll-down returns"
            ) +
            custom_theme +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))

        print(p)
    })

    # NEW: Breakeven Analysis Plot
    output$breakeven_analysis_plot <- renderPlot({
        req(carry_roll_data())

        # Check if data has required columns
        if (!"holding_period" %in% names(carry_roll_data())) {
            plot.new()
            text(0.5, 0.5, "Error: holding_period column missing", cex = 1.5)
            return()
        }

        breakeven <- carry_roll_data() %>%
            filter(holding_period == "90d", !is.na(breakeven_bps)) %>%
            arrange(desc(breakeven_bps))

        if (nrow(breakeven) == 0) {
            plot.new()
            text(0.5, 0.5, "No breakeven data available", cex = 1.5)
            return()
        }

        p <- ggplot(breakeven, aes(x = reorder(bond, breakeven_bps), y = breakeven_bps)) +
            geom_col(fill = insele_colors["primary"], alpha = 0.8) +
            geom_text(aes(label = sprintf("%.0f bps", breakeven_bps)),
                      hjust = -0.1, size = 3) +
            coord_flip() +
            labs(
                title = "3-Month Breakeven Analysis",
                subtitle = "Yield rise (bps) before total return turns negative",
                x = "",
                y = "Breakeven (basis points)",
                caption = "Higher values indicate more cushion against rate rises"
            ) +
            custom_theme

        print(p)
    })

    # NEW: Optimal Holding Period Plot
    output$optimal_holding_plot <- renderPlot({
        req(carry_roll_data(), input$carry_roll_bond)

        if (!"holding_period" %in% names(carry_roll_data())) {
            plot.new()
            text(0.5, 0.5, "Error: holding_period column missing", cex = 1.5)
            return()
        }

        bond_returns <- carry_roll_data() %>%
            filter(bond == input$carry_roll_bond, !is.na(total_return)) %>%
            mutate(
                days = as.numeric(gsub("d", "", holding_period)),
                # Corrected annualized return calculation
                annualized_return = (total_return / 100) * (365 / days) * 100
            )

        if (nrow(bond_returns) == 0) {
            plot.new()
            text(0.5, 0.5, paste("No data available for", input$carry_roll_bond), cex = 1.5)
            return()
        }

        p <- ggplot(bond_returns, aes(x = days)) +
            geom_line(aes(y = total_return), color = insele_colors["primary"], size = 1.5) +
            geom_point(aes(y = total_return), color = insele_colors["primary"], size = 3) +
            geom_line(aes(y = annualized_return), color = insele_colors["accent"],
                      size = 1.5, linetype = "dashed") +
            geom_point(aes(y = annualized_return), color = insele_colors["accent"], size = 3) +
            labs(
                title = paste("Return Profile:", input$carry_roll_bond),
                subtitle = "Solid blue: Total Return | Orange dashed: Annualized Return\nOptimal holding period typically occurs where annualized return peaks",
                x = "Holding Period (days)",
                y = "Return (%)",
                caption = "Note: Returns include carry (coupon income) and roll-down components"
            ) +
            custom_theme

        print(p)
    })

    # NEW: Auction Prediction Display
    output$auction_prediction_display <- renderUI({
        req(filtered_data(), input$auction_pred_bonds)

        if (length(input$auction_pred_bonds) == 0) {
            return(p("Please select bonds for prediction"))
        }

        prediction_cards <- lapply(input$auction_pred_bonds, function(bond_name) {
            prediction <- predict_bid_cover(filtered_data(), bond_name)

            card_content <- if (!is.na(prediction$prediction)) {
                tags$div(
                    style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 5px;",
                    h5(bond_name, style = "margin-top: 0; color: #1B3A6B;"),
                    p(sprintf("Expected Bid/Cover: %.2fx", prediction$prediction),
                      style = "margin: 5px 0;"),
                    p(sprintf("95%% CI: [%.2fx - %.2fx]", prediction$lower_ci, prediction$upper_ci),
                      style = "margin: 5px 0; font-size: 12px;"),
                    p(paste("Confidence:", prediction$confidence),
                      style = paste0("margin: 5px 0; font-weight: bold; color: ",
                                     ifelse(prediction$confidence == "High", "#28a745",
                                            ifelse(prediction$confidence == "Medium", "#ffc107", "#dc3545")))),
                    p(sprintf("R²: %.2f", prediction$r_squared),
                      style = "font-size: 11px; color: #666; margin: 0;")
                )
            } else {
                tags$div(
                    style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 5px;",
                    h5(bond_name, style = "margin-top: 0; color: #1B3A6B;"),
                    p("Insufficient data for prediction", style = "color: #666;")
                )
            }

            return(card_content)
        })

        do.call(tagList, prediction_cards)
    })

    # NEW: Sentiment Gauge Plot
    output$sentiment_gauge_plot <- renderPlot({
        req(filtered_data())

        # Calculate average bid-to-cover trend
        recent_btc <- filtered_data() %>%
            filter(!is.na(bid_to_cover),
                   date >= today() - days(90)) %>%
            arrange(date) %>%
            mutate(
                ma_btc = rollapply(bid_to_cover, 5, mean, fill = NA, align = "right", partial = TRUE)
            )

        sentiment_score <- 0  # Default value

        if (nrow(recent_btc) > 10) {
            # Calculate sentiment score (-100 to 100)
            tryCatch({
                trend_model <- lm(ma_btc ~ as.numeric(date), data = recent_btc)
                sentiment_score <- coefficients(trend_model)[2] * 1000  # Scale the slope
                sentiment_score <- max(-100, min(100, sentiment_score))  # Bound between -100 and 100
            }, error = function(e) {
                sentiment_score <- 0
            })
        }

        # Create gauge visualization
        gauge_data <- data.frame(
            category = c("Negative", "Neutral", "Positive"),
            xmin = c(-100, -33, 33),
            xmax = c(-33, 33, 100),
            ymin = rep(0, 3),
            ymax = rep(1, 3),
            fill_color = c(insele_colors["danger"], insele_colors["warning"], insele_colors["success"])
        )

        p <- ggplot(gauge_data) +
            geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                      fill = gauge_data$fill_color) +
            geom_vline(xintercept = sentiment_score, size = 3, color = "black") +
            geom_text(x = sentiment_score, y = 1.1,
                      label = sprintf("%.0f", sentiment_score),
                      size = 6, fontface = "bold") +
            coord_cartesian(xlim = c(-100, 100), ylim = c(0, 1.2)) +
            theme_void() +
            labs(title = "Auction Sentiment Score",
                 subtitle = "Based on bid-to-cover trends")

        print(p)
    })

    # NEW: Surprise Index Plot
    output$surprise_index_plot <- renderPlot({
        req(filtered_data())

        # Get top 5 bonds by auction frequency
        top_bonds <- filtered_data() %>%
            filter(!is.na(bid_to_cover)) %>%
            count(bond) %>%
            arrange(desc(n)) %>%
            head(5) %>%
            pull(bond)

        # Calculate surprise index for top bonds only
        surprise_data <- filtered_data() %>%
            filter(!is.na(bid_to_cover), bond %in% top_bonds) %>%
            group_by(bond) %>%
            arrange(date) %>%
            mutate(
                predicted_btc = lag(rollapply(bid_to_cover, 3, mean,
                                              fill = NA, align = "right", partial = TRUE), 1),
                surprise = calculate_surprise_index(bid_to_cover, predicted_btc)
            ) %>%
            ungroup() %>%
            filter(!is.na(surprise))

        if (nrow(surprise_data) == 0) {
            plot.new()
            text(0.5, 0.5, "Insufficient data for surprise index", cex = 1.5)
            return()
        }

        p <- ggplot(surprise_data, aes(x = date, y = surprise)) +
            geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.8) +
            geom_hline(yintercept = c(-20, 20), linetype = "dashed",
                       color = insele_colors["secondary"], alpha = 0.5) +
            geom_line(color = insele_colors["primary"], size = 1.2, alpha = 0.8) +
            geom_point(color = insele_colors["primary"], size = 2, alpha = 0.8) +
            facet_wrap(~ bond, ncol = 1, scales = "free_x") +
            scale_x_date(date_breaks = "1 month", date_labels = "%b") +
            labs(
                title = "Auction Surprise Index - Top 5 Bonds",
                subtitle = "Measures how actual bid-to-cover differs from expected (3-auction moving average)\nPositive = stronger demand than expected | Negative = weaker demand than expected",
                x = "Date",
                y = "Surprise (%)",
                caption = "Dashed lines at ±20% indicate significant surprises"
            ) +
            custom_theme +
            theme(
                strip.background = element_rect(fill = insele_colors["light_teal"]),
                strip.text = element_text(face = "bold", size = 10)
            )

        print(p)
    })

    # Existing visualization outputs (Rich/Cheap, Z-Score, etc.) remain the same...
    # [Previous plot outputs continue here unchanged]

    # 2. Rich/Cheap Analysis
    output$rich_cheap_plot <- renderPlot({
        req(processed_data())

        data <- processed_data() %>%
            arrange(desc(spread_to_curve))

        p <- ggplot(data, aes(x = reorder(bond, spread_to_curve), y = spread_to_curve)) +
            geom_col(aes(fill = spread_to_curve > 0), width = 0.7) +
            geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
            geom_hline(yintercept = c(-25, 25), linetype = "dashed",
                       color = insele_colors["secondary"], alpha = 0.5) +
            scale_fill_manual(values = c("#28a745", "#dc3545"), guide = "none") +
            scale_y_continuous(breaks = pretty_breaks(n = 8),
                               labels = function(x) paste0(x, " bps")) +
            coord_flip() +
            labs(
                title = "Rich/Cheap Analysis",
                subtitle = "Deviation from Fair Value",
                x = "",
                y = "Spread to Fitted Curve",
                caption = paste("Lookback:", input$lookback_days, "days")
            ) +
            custom_theme

        print(p)
    })

    # 3. Z-Score Distribution
    output$zscore_plot <- renderPlot({
        req(processed_data())

        data <- processed_data() %>%
            filter(!is.na(z_score))

        if (nrow(data) == 0) {
            plot.new()
            text(0.5, 0.5, "Insufficient data for z-score calculation\nNeed at least 60 days of history", cex = 1.2)
            return()
        }

        # Show all bonds, not just outliers
        data <- data %>%
            arrange(desc(z_score))

        # Determine if there are any outliers
        has_outliers <- any(abs(data$z_score) > 2)

        p <- ggplot(data, aes(x = reorder(bond, z_score), y = z_score)) +
            geom_col(aes(fill = cut(abs(z_score),
                                    breaks = c(-Inf, 1, 2, Inf),
                                    labels = c("Normal", "Notable", "Outlier"))),
                     width = 0.7) +
            geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
            geom_hline(yintercept = c(-2, -1, 1, 2), linetype = "dashed",
                       color = c(insele_colors["danger"], insele_colors["warning"],
                                 insele_colors["warning"], insele_colors["danger"]),
                       alpha = 0.5, size = c(0.8, 0.6, 0.6, 0.8)) +
            geom_text(aes(label = sprintf("%.2f", z_score),
                          hjust = ifelse(z_score > 0, -0.2, 1.2)),
                      size = 3) +
            scale_fill_manual(
                values = c("Normal" = insele_colors["secondary"],
                           "Notable" = insele_colors["warning"],
                           "Outlier" = insele_colors["danger"]),
                name = "Category"
            ) +
            scale_y_continuous(breaks = pretty_breaks(n = 8)) +
            coord_flip() +
            labs(
                title = "Statistical Richness/Cheapness",
                subtitle = ifelse(has_outliers,
                                  "Z-Score Analysis: Values beyond ±2 indicate significant outliers",
                                  "Z-Score Analysis: All bonds within normal range (±2 standard deviations)"),
                x = "",
                y = "Z-Score (Standard Deviations)",
                caption = paste("Window:", input$zscore_window, "days | Guidelines: ±1 = Notable, ±2 = Outlier")
            ) +
            custom_theme +
            theme(legend.position = "top")

        print(p)
    })

    # 4. Supply and Demand Plot
    output$supply_demand_plot <- renderPlot({
        req(filtered_data())

        auction_data <- filtered_data() %>%
            filter(!is.na(bid_to_cover), !is.na(offer)) %>%
            group_by(bond) %>%
            summarise(
                avg_bid_cover = mean(bid_to_cover, na.rm = TRUE),
                total_offer = sum(offer, na.rm = TRUE) / 1e9,
                count = n(),
                avg_duration = mean(modified_duration, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            filter(count > 0)

        if (nrow(auction_data) == 0) {
            plot.new()
            text(0.5, 0.5, "No auction data available", cex = 1.5)
            return()
        }

        # Create color palette based on number of bonds
        n_bonds <- length(unique(auction_data$bond))
        if (n_bonds <= 9) {
            bond_colors <- RColorBrewer::brewer.pal(max(3, n_bonds), "Set1")[1:n_bonds]
        } else {
            bond_colors <- c(RColorBrewer::brewer.pal(9, "Set1"),
                             RColorBrewer::brewer.pal(min(8, n_bonds - 9), "Set2"))[1:n_bonds]
        }
        names(bond_colors) <- unique(auction_data$bond)

        p <- ggplot(auction_data, aes(x = avg_duration, y = avg_bid_cover)) +
            geom_point(aes(size = total_offer, color = bond), alpha = 0.7) +
            geom_text(aes(label = bond), vjust = -1, size = 3) +
            geom_smooth(method = "lm", se = TRUE,
                        color = insele_colors["primary"],
                        fill = insele_colors["light_teal"],
                        alpha = 0.2) +
            scale_size_continuous(
                range = c(5, 15),
                name = "Total Offer (R bn)",
                breaks = pretty_breaks(n = 5)
            ) +
            scale_color_manual(values = bond_colors, guide = "none") +
            scale_x_continuous(breaks = pretty_breaks(n = 8)) +
            scale_y_continuous(breaks = pretty_breaks(n = 8)) +
            labs(
                title = "Supply and Demand of Auction Bonds",
                subtitle = "Average bid/cover (demand) vs duration",
                x = "Average Modified Duration",
                y = "Average Bid/Cover Ratio",
                caption = "Source: Insele Capital Partners | Size = YTD Cumulative Offer Amount"
            ) +
            custom_theme +
            theme(legend.position = "bottom")

        print(p)
    })

    # 5. Historical Auction Performance
    output$auction_hist_plot <- renderPlot({
        req(filtered_data())

        auction_stats <- filtered_data() %>%
            filter(!is.na(bid_to_cover)) %>%
            group_by(bond) %>%
            summarise(
                mean_btc = mean(bid_to_cover, na.rm = TRUE),
                min_btc = min(bid_to_cover, na.rm = TRUE),
                max_btc = max(bid_to_cover, na.rm = TRUE),
                last_btc = last(bid_to_cover, order_by = date),
                .groups = "drop"
            ) %>%
            filter(!is.na(mean_btc))

        p <- ggplot(auction_stats, aes(x = reorder(bond, mean_btc))) +
            geom_col(aes(y = mean_btc, fill = mean_btc), width = 0.7, alpha = 0.7) +
            geom_point(aes(y = max_btc), color = insele_colors["secondary"],
                       size = 3, shape = 24, fill = insele_colors["secondary"]) +
            geom_point(aes(y = min_btc), color = insele_colors["danger"],
                       size = 3, shape = 25, fill = insele_colors["danger"]) +
            geom_point(aes(y = last_btc), color = insele_colors["accent"],
                       size = 4, shape = 21, fill = insele_colors["accent"]) +
            geom_hline(yintercept = 2.0, linetype = "dashed",
                       color = "black", alpha = 0.5) +
            scale_fill_gradient2(
                low = insele_colors["danger"],
                mid = "#FFD700",
                high = insele_colors["success"],
                midpoint = 3.5,
                name = "Mean",
                guide = "none"
            ) +
            scale_y_continuous(breaks = pretty_breaks(n = 8)) +
            coord_flip() +
            labs(
                title = "Historical Auction Statistics",
                subtitle = "High, low, last and average bid to cover",
                x = "",
                y = "Bid to Cover Ratio",
                caption = "▲ Highest | ▼ Lowest | ● Last | Bar = Average"
            ) +
            custom_theme

        print(p)
    })

    # 6. Bid-to-Cover Trends
    output$btc_trends_plot <- renderPlot({
        req(filtered_data())

        btc_data <- filtered_data() %>%
            filter(!is.na(bid_to_cover)) %>%
            select(date, bond, bid_to_cover)

        if (nrow(btc_data) == 0) {
            plot.new()
            text(0.5, 0.5, "No bid-to-cover data available", cex = 1.5)
            return()
        }

        # Create color palette based on number of bonds
        n_bonds <- length(unique(btc_data$bond))
        if (n_bonds <= 9) {
            bond_colors <- RColorBrewer::brewer.pal(max(3, n_bonds), "Set1")[1:n_bonds]
        } else {
            bond_colors <- c(RColorBrewer::brewer.pal(9, "Set1"),
                             RColorBrewer::brewer.pal(min(8, n_bonds - 9), "Set2"))[1:n_bonds]
        }
        names(bond_colors) <- unique(btc_data$bond)

        p <- ggplot(btc_data, aes(x = date, y = bid_to_cover, color = bond)) +
            geom_line(size = 1, alpha = 0.7) +
            geom_point(size = 2, alpha = 0.7) +
            geom_hline(yintercept = 2.0, linetype = "dashed",
                       color = "black", alpha = 0.5) +
            geom_smooth(method = "loess", se = FALSE, size = 0.5, alpha = 0.3) +
            facet_wrap(~ bond, scales = "free_y", ncol = 3) +
            scale_color_manual(values = bond_colors, guide = "none") +
            scale_x_date(date_breaks = "1 month", date_labels = "%b") +
            scale_y_continuous(breaks = pretty_breaks(n = 4)) +
            labs(
                title = "Bid-to-Cover Ratio Trends",
                subtitle = "Evolution of auction demand by bond",
                x = "",
                y = "Bid/Cover Ratio",
                caption = "Dashed line = 2.0x coverage threshold"
            ) +
            custom_theme +
            theme(
                strip.background = element_rect(fill = insele_colors["light_teal"]),
                strip.text = element_text(face = "bold")
            )

        print(p)
    })

    # 7. Term Structure Evolution
    output$term_structure_plot <- renderPlot({
        req(filtered_data())

        term_data <- filtered_data() %>%
            select(date, bond, modified_duration, yield_to_maturity) %>%
            filter(!is.na(yield_to_maturity))

        # Create time series of curve shape metrics with clearer labeling
        curve_metrics <- term_data %>%
            group_by(date) %>%
            summarise(
                level = mean(yield_to_maturity, na.rm = TRUE),
                slope = {
                    # SA bonds: Using <5 years as short, >10 years as long
                    short <- mean(yield_to_maturity[modified_duration < 5], na.rm = TRUE)
                    long <- mean(yield_to_maturity[modified_duration > 10], na.rm = TRUE)
                    long - short
                },
                curvature = {
                    # SA bonds curve shape calculation
                    short <- mean(yield_to_maturity[modified_duration < 5], na.rm = TRUE)
                    medium <- mean(yield_to_maturity[modified_duration >= 5 & modified_duration <= 10], na.rm = TRUE)
                    long <- mean(yield_to_maturity[modified_duration > 10], na.rm = TRUE)
                    2 * medium - short - long
                },
                .groups = "drop"
            ) %>%
            pivot_longer(cols = c(level, slope, curvature),
                         names_to = "metric",
                         values_to = "value")

        p <- ggplot(curve_metrics, aes(x = date, y = value, color = metric)) +
            geom_line(size = 1.2) +
            facet_wrap(~ metric, scales = "free_y", ncol = 1,
                       labeller = labeller(metric = c(
                           level = "Level (Average Yield %)",
                           slope = "Slope (Long-Short Spread)",
                           curvature = "Curvature (2×Medium - Short - Long)"
                       ))) +
            scale_color_manual(
                values = c(
                    level = insele_colors["primary"],
                    slope = insele_colors["secondary"],
                    curvature = insele_colors["accent"]
                ),
                guide = "none"
            ) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
            scale_y_continuous(breaks = pretty_breaks(n = 6)) +
            labs(
                title = "Yield Curve Shape Evolution",
                subtitle = "Using SA bonds with duration <5 years as 'short' and >10 years as 'long'",
                x = "",
                y = "",
                caption = "Source: Insele Capital Partners | Level shows average yields, Slope shows steepness, Curvature shows belly of the curve"
            ) +
            custom_theme +
            theme(
                strip.background = element_rect(fill = insele_colors["light_teal"]),
                strip.text = element_text(face = "bold", size = 11)
            )

        print(p)
    })

    # 8. Percentile Rankings
    output$percentile_plot <- renderPlot({
        req(filtered_data())

        # Calculate historical percentiles
        percentile_data <- filtered_data() %>%
            group_by(bond) %>%
            mutate(
                ytm_percentile = percent_rank(yield_to_maturity) * 100,
                current_ytm = last(yield_to_maturity, order_by = date)
            ) %>%
            filter(date == max(date)) %>%
            ungroup()

        p <- ggplot(percentile_data, aes(x = reorder(bond, ytm_percentile),
                                         y = ytm_percentile)) +
            geom_segment(aes(xend = bond, y = 0, yend = ytm_percentile),
                         color = insele_colors["secondary"], size = 1.5) +
            geom_point(aes(color = ytm_percentile), size = 5) +
            geom_hline(yintercept = 50, linetype = "dashed",
                       color = "black", alpha = 0.5) +
            geom_text(aes(label = sprintf("%.1f%%", ytm_percentile)),
                      vjust = -1, size = 3.5) +
            scale_color_gradient2(
                low = insele_colors["success"],
                mid = "#FFD700",
                high = insele_colors["danger"],
                midpoint = 50,
                guide = "none"
            ) +
            scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                               labels = function(x) paste0(x, "%")) +
            coord_flip() +
            labs(
                title = "Current Yield Percentile Rankings",
                subtitle = "Shows where current yields sit relative to their historical range\n0% = lowest yield in period | 100% = highest yield in period | >50% suggests yields are high relative to recent history",
                x = "",
                y = "Historical Percentile",
                caption = paste("Based on", input$date_range[2] - input$date_range[1], "days of history")
            ) +
            custom_theme

        print(p)
    })

    # 9. Correlation Matrix
    output$correlation_plot <- renderPlot({
        req(filtered_data())

        # Calculate correlation matrix
        cor_data <- filtered_data() %>%
            select(date, bond, yield_to_maturity) %>%
            pivot_wider(names_from = bond, values_from = yield_to_maturity) %>%
            select(-date) %>%
            cor(use = "pairwise.complete.obs")

        # Create correlation plot using ggplot2
        cor_melted <- cor_data %>%
            as.data.frame() %>%
            rownames_to_column("bond1") %>%
            pivot_longer(cols = -bond1, names_to = "bond2", values_to = "correlation")

        p <- ggplot(cor_melted, aes(x = bond1, y = bond2, fill = correlation)) +
            geom_tile(color = "white", size = 0.5) +
            geom_text(aes(label = sprintf("%.2f", correlation)),
                      size = 3, color = "white") +
            scale_fill_gradient2(
                low = insele_colors["danger"],
                mid = "white",
                high = insele_colors["primary"],
                midpoint = 0.5,
                limits = c(0, 1),
                name = "Correlation"
            ) +
            labs(
                title = "Bond Yield Correlation Matrix",
                subtitle = "Pairwise correlations of yield movements",
                x = "",
                y = "",
                caption = "1.00 = Perfect correlation | 0.00 = No correlation"
            ) +
            custom_theme +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "right"
            )

        print(p)
    })

    # 10. Convexity Analysis
    output$convexity_plot <- renderPlot({
        req(processed_data())

        data <- processed_data()

        if (nrow(data) == 0) {
            plot.new()
            text(0.5, 0.5, "No convexity data available", cex = 1.5)
            return()
        }

        # Create color palette based on number of bonds
        n_bonds <- length(unique(data$bond))
        if (n_bonds <= 9) {
            bond_colors <- RColorBrewer::brewer.pal(max(3, n_bonds), "Set1")[1:n_bonds]
        } else {
            bond_colors <- c(RColorBrewer::brewer.pal(9, "Set1"),
                             RColorBrewer::brewer.pal(min(8, n_bonds - 9), "Set2"))[1:n_bonds]
        }
        names(bond_colors) <- unique(data$bond)

        p <- ggplot(data, aes(x = modified_duration, y = convexity)) +
            geom_point(aes(size = yield_to_maturity, color = bond), alpha = 0.7) +
            geom_smooth(method = "loess", se = TRUE,
                        color = insele_colors["primary"],
                        fill = insele_colors["light_teal"],
                        alpha = 0.2) +
            geom_text(aes(label = bond), vjust = -1, size = 3) +
            scale_size_continuous(
                range = c(3, 10),
                name = "YTM (%)",
                breaks = pretty_breaks(n = 5)
            ) +
            scale_color_manual(values = bond_colors, guide = "none") +
            scale_x_continuous(breaks = pretty_breaks(n = 8)) +
            scale_y_continuous(breaks = pretty_breaks(n = 8)) +
            labs(
                title = "Convexity Profile Across the Curve",
                subtitle = "Risk/Return characteristics by duration",
                x = "Modified Duration (years)",
                y = "Convexity",
                caption = "Size = Yield to Maturity | Higher convexity = Better risk/return profile"
            ) +
            custom_theme +
            theme(legend.position = "bottom")

        print(p)
    })

    # ==========================================================================
    # DATA TABLES (Enhanced)
    # ==========================================================================

    # NEW: Technical Indicators Table
    output$technical_indicators_table <- DT::renderDataTable({
        req(filtered_data())

        tech_data <- add_technicals(filtered_data())

        latest_tech <- tech_data %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            select(bond, yield_to_maturity, rsi, macd, signal, bb_upper, bb_lower) %>%
            mutate(
                yield_to_maturity = sprintf("%.2f%%", yield_to_maturity),
                rsi = sprintf("%.1f", rsi),
                macd = sprintf("%.3f", macd),
                signal = sprintf("%.3f", signal),
                bb_position = case_when(
                    yield_to_maturity > bb_upper ~ "Above Upper",
                    yield_to_maturity < bb_lower ~ "Below Lower",
                    TRUE ~ "Within Bands"
                ),
                signal_status = case_when(
                    macd > signal ~ "Bullish",
                    macd < signal ~ "Bearish",
                    TRUE ~ "Neutral"
                )
            ) %>%
            select(bond, yield_to_maturity, rsi, macd, signal, bb_position, signal_status)

        datatable(
            latest_tech,
            options = list(
                pageLength = 15,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
            ),
            rownames = FALSE,
            class = 'table-striped table-bordered',
            colnames = c("Bond", "Yield", "RSI", "MACD", "Signal", "BB Position", "Signal")
        )
    })

    # NEW: Return Attribution Table
    output$return_attribution_table <- DT::renderDataTable({
        req(carry_roll_data())

        # Check if data has required columns
        if (!"holding_period" %in% names(carry_roll_data())) {
            return(datatable(
                data.frame(Error = "holding_period column missing from data"),
                options = list(dom = 't'),
                rownames = FALSE
            ))
        }

        attribution <- carry_roll_data() %>%
            filter(holding_period == "90d", !is.na(total_return)) %>%
            select(bond, period_carry, roll_return, total_return, breakeven_bps) %>%
            mutate(
                period_carry = sprintf("%.2f%%", period_carry),
                roll_return = sprintf("%.2f%%", roll_return),
                total_return = sprintf("%.2f%%", total_return),
                breakeven_bps = sprintf("%.0f bps", breakeven_bps)
            )

        if (nrow(attribution) == 0) {
            return(datatable(
                data.frame(Message = "No return attribution data available"),
                options = list(dom = 't'),
                rownames = FALSE
            ))
        }

        datatable(
            attribution,
            options = list(
                pageLength = 15,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
            ),
            rownames = FALSE,
            class = 'table-striped table-bordered',
            colnames = c("Bond", "Carry", "Roll-down", "Total Return", "Breakeven")
        )
    })

    # NEW: Butterfly Opportunities Table
    output$butterfly_opportunities_table <- DT::renderDataTable({
        req(processed_data())

        butterflies <- detect_butterfly_opportunities(processed_data())

        if (nrow(butterflies) > 0) {
            datatable(
                butterflies,
                options = list(
                    pageLength = 10,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel')
                ),
                rownames = FALSE,
                class = 'table-striped table-bordered'
            ) %>%
                formatStyle(
                    'signal_strength',
                    backgroundColor = styleEqual(
                        c("Strong", "Medium", "Weak"),
                        c(insele_colors["success"], insele_colors["warning"], "#f8f9fa")
                    )
                )
        } else {
            datatable(
                data.frame(Message = "No butterfly opportunities detected at current levels"),
                options = list(dom = 't'),
                rownames = FALSE
            )
        }
    })

    # ==========================================================================
    # EXISTING DATA TABLES
    # ==========================================================================

    # Relative Value Table
    output$relative_value_table <- DT::renderDataTable({
        req(processed_data())

        processed_data() %>%
            select(bond, yield_to_maturity, modified_duration, duration,
                   convexity, spread_to_curve, z_score) %>%
            mutate(
                yield_to_maturity = sprintf("%.2f%%", yield_to_maturity),
                modified_duration = sprintf("%.2f", modified_duration),
                duration = sprintf("%.2f", duration),
                convexity = sprintf("%.1f", convexity),
                spread_to_curve = sprintf("%.1f bps", spread_to_curve),
                z_score = sprintf("%.2f", z_score)
            ) %>%
            datatable(
                options = list(
                    pageLength = 15,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel')
                ),
                rownames = FALSE,
                class = 'table-striped table-bordered'
            )
    })

    # Auction Statistics Table
    output$auction_stats_table <- DT::renderDataTable({
        req(filtered_data())

        filtered_data() %>%
            filter(!is.na(bid_to_cover)) %>%
            group_by(bond) %>%
            summarise(
                auctions = n(),
                avg_offer = mean(offer, na.rm = TRUE) / 1e6,
                avg_allocation = mean(allocation, na.rm = TRUE) / 1e6,
                avg_bids = mean(bids, na.rm = TRUE) / 1e6,
                avg_bid_cover = mean(bid_to_cover, na.rm = TRUE),
                last_bid_cover = last(bid_to_cover, order_by = date),
                .groups = "drop"
            ) %>%
            mutate(
                avg_offer = sprintf("%.0f", avg_offer),
                avg_allocation = sprintf("%.0f", avg_allocation),
                avg_bids = sprintf("%.0f", avg_bids),
                avg_bid_cover = sprintf("%.2fx", avg_bid_cover),
                last_bid_cover = sprintf("%.2fx", last_bid_cover)
            ) %>%
            datatable(
                options = list(
                    pageLength = 15,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel')
                ),
                rownames = FALSE,
                class = 'table-striped table-bordered',
                colnames = c("Bond", "# Auctions", "Avg Offer (Rm)", "Avg Alloc (Rm)",
                             "Avg Bids (Rm)", "Avg Bid/Cover", "Last Bid/Cover")
            )
    })

    # ==========================================================================
    # DOWNLOAD HANDLERS
    # ==========================================================================

    # Create generic download handler function
    create_download_handler <- function(plot_name) {
        downloadHandler(
            filename = function() {
                paste0("insele_", plot_name, "_", Sys.Date(), ".png")
            },
            content = function(file) {
                ggsave(file, plot = last_plot(),
                       width = 10, height = 6, dpi = 300,
                       bg = "white")
            }
        )
    }

    # Download handlers for all plots
    output$download_yield_curve <- create_download_handler("yield_curve")
    output$download_rich_cheap <- create_download_handler("rich_cheap")
    output$download_zscore <- create_download_handler("zscore")
    output$download_supply_demand <- create_download_handler("supply_demand")
    output$download_auction_hist <- create_download_handler("auction_hist")
    output$download_btc_trends <- create_download_handler("btc_trends")
    output$download_term_structure <- create_download_handler("term_structure")
    output$download_percentile <- create_download_handler("percentile")
    output$download_correlation <- create_download_handler("correlation")
    output$download_convexity <- create_download_handler("convexity")

    # New plot download handlers
    output$download_technicals <- create_download_handler("technical_indicators")
    output$download_carry_roll <- create_download_handler("carry_roll_heatmap")

    # Export current view (general export)
    output$export_current <- downloadHandler(
        filename = function() {
            paste0("insele_dashboard_export_", Sys.Date(), ".png")
        },
        content = function(file) {
            if(!is.null(last_plot())) {
                ggsave(file, plot = last_plot(),
                       width = 12, height = 8, dpi = 300,
                       bg = "white")
            }
        }
    )

    # ==========================================================================
    # REPORT GENERATION
    # ==========================================================================

    # Define report generation function to be reused
    report_generator <- function(input, output, session) {
        downloadHandler(
            filename = function() {
                paste0("insele_bond_analysis_", Sys.Date(), ".",
                       isolate(input$report_format))
            },
            content = function(file) {
                # Create temporary R Markdown file
                tempReport <- file.path(tempdir(), "report.Rmd")

                # Build report content based on selected components
                report_sections <- character()

                # Header
                report_header <- c(
                    "---",
                    paste0("title: '", isolate(input$report_title), "'"),
                    paste0("author: 'Insele Capital Partners | Broking Services'"),
                    paste0("date: '", format(isolate(input$report_date), "%B %d, %Y"), "'"),
                    if (isolate(input$report_format) == "pdf") {
                        "output:
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true"
                    } else {
                        "output:
  html_document:
    toc: true
    toc_depth: 2
    toc_float: true
    theme: flatly
    highlight: tango"
                    },
                    "---",
                    "",
                    "```{r setup, include=FALSE}",
                    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 10, fig.height = 6)",
                    "```"
                )

                # Executive Summary
                if (isolate(input$include_summary)) {
                    summary_section <- c(
                        "",
                        "# Executive Summary",
                        "",
                        "## Market Overview",
                        "",
                        paste("This report provides comprehensive analysis of the South African Government Bond market."),
                        "",
                        "### Key Metrics",
                        "",
                        paste("- **Analysis Period:**", format(isolate(input$date_range[1]), "%B %d, %Y"), "to",
                              format(isolate(input$date_range[2]), "%B %d, %Y")),
                        paste("- **Bonds Analyzed:**", length(isolate(input$selected_bonds))),
                        "",
                        "### Market Conditions",
                        ""
                    )
                    report_sections <- c(report_sections, summary_section)
                }

                # Relative Value Analysis
                if (isolate(input$include_relative)) {
                    relative_section <- c(
                        "",
                        "# Relative Value Analysis",
                        "",
                        "## Yield Curve Positioning",
                        "",
                        "The yield curve analysis identifies bonds trading rich or cheap relative to the fitted curve.",
                        ""
                    )
                    report_sections <- c(report_sections, relative_section)
                }

                # Auction Analytics
                if (isolate(input$include_auction)) {
                    auction_section <- c(
                        "",
                        "# Auction Analytics",
                        "",
                        "## Supply and Demand Dynamics",
                        "",
                        "Analysis of recent auction performance and bid-to-cover trends.",
                        ""
                    )
                    report_sections <- c(report_sections, auction_section)
                }

                # Technical Analysis
                if (isolate(input$include_technicals)) {
                    technical_section <- c(
                        "",
                        "# Technical Analysis",
                        "",
                        "## Technical Indicators Overview",
                        "",
                        "Current technical signals based on moving averages, RSI, and MACD indicators.",
                        ""
                    )
                    report_sections <- c(report_sections, technical_section)
                }

                # Historical Analysis
                if (isolate(input$include_historical)) {
                    historical_section <- c(
                        "",
                        "# Historical Analysis",
                        "",
                        "## Term Structure Evolution",
                        "",
                        "The yield curve has evolved significantly over the analysis period.",
                        ""
                    )
                    report_sections <- c(report_sections, historical_section)
                }

                # Carry & Roll Analysis
                if (isolate(input$include_carry_roll)) {
                    carry_section <- c(
                        "",
                        "# Carry & Roll Analysis",
                        "",
                        "## Expected Returns",
                        "",
                        "Total return expectations based on carry and roll-down analysis.",
                        ""
                    )
                    report_sections <- c(report_sections, carry_section)
                }

                # Data Tables
                if (isolate(input$include_data_tables)) {
                    tables_section <- c(
                        "",
                        "# Appendix: Data Tables",
                        "",
                        "## Bond Metrics",
                        "",
                        "Detailed metrics for all analyzed bonds.",
                        ""
                    )
                    report_sections <- c(report_sections, tables_section)
                }

                # Disclaimer
                if (isolate(input$include_disclaimer)) {
                    disclaimer_section <- c(
                        "",
                        "# Disclaimer",
                        "",
                        "**For Professional Investors Only**",
                        "",
                        "This report is provided for informational purposes only and does not constitute investment advice.",
                        "",
                        "---",
                        "",
                        "*© 2025 Insele Capital Partners | The Power of Partnership*",
                        ""
                    )
                    report_sections <- c(report_sections, disclaimer_section)
                }

                # Write complete report
                writeLines(c(report_header, report_sections), tempReport)

                # Render the report
                tryCatch({
                    rmarkdown::render(
                        tempReport,
                        output_file = file,
                        envir = new.env(parent = globalenv()),
                        quiet = TRUE
                    )
                }, error = function(e) {
                    showNotification(
                        paste("Error generating report:", e$message),
                        type = "error",
                        session = session
                    )
                })
            }
        )
    }

    # Main report generation handler
    output$generate_report <- report_generator(input, output, session)

    # Separate handler for download button
    output$download_report <- report_generator(input, output, session)

    # Modal download handler
    output$download_report_modal <- report_generator(input, output, session)

    # Preview report functionality
    observeEvent(input$preview_report, {
        showModal(modalDialog(
            title = "Report Preview",
            size = "l",
            p("Report will include the following sections:"),
            tags$ul(
                if(input$include_summary) tags$li("Executive Summary with Market Intelligence"),
                if(input$include_relative) tags$li("Relative Value Analysis"),
                if(input$include_auction) tags$li("Auction Analytics with Predictions"),
                if(input$include_technicals) tags$li("Technical Analysis"),
                if(input$include_historical) tags$li("Historical Analysis"),
                if(input$include_carry_roll) tags$li("Carry & Roll Analysis"),
                if(input$include_data_tables) tags$li("Data Tables Appendix"),
                if(input$include_disclaimer) tags$li("Compliance Disclaimer")
            ),
            p(strong("Client:"), ifelse(nchar(input$client_name) > 0, input$client_name, "Not specified")),
            p(strong("Report Date:"), format(input$report_date, "%B %d, %Y")),
            p(strong("Format:"), toupper(input$report_format)),
            footer = tagList(
                modalButton("Close"),
                downloadButton("download_report_modal", "Generate Report", class = "btn-success")
            )
        ))
    })

}

# Run the application
shinyApp(ui = ui, server = server)