#' @export
# Enhanced Smart Insights Generation
generate_advanced_insights <- function(data, processed_data, var_data, regime_data) {
    safe_execute({
        insights <- list()
        current_date <- max(data$date, na.rm = TRUE)

        # 1. Market Regime Insight
        if (!is.null(regime_data) && nrow(regime_data) > 0) {
            current_regime <- regime_data %>%
                filter(date == max(date)) %>%
                pull(regime)

            regime_icon <- case_when(
                current_regime == "Stressed" ~ "🔴",
                current_regime == "Elevated" ~ "🟡",
                current_regime == "Calm" ~ "🟢",
                TRUE ~ "🔵"
            )

            insights[[length(insights) + 1]] <- list(
                icon = regime_icon,
                text = sprintf("Market Regime: %s - Volatility at %.1f percentile historically",
                               current_regime,
                               regime_data %>% filter(date == max(date)) %>% pull(vol_percentile) * 100),
                priority = 10,
                category = "Market Conditions"
            )
        }

        # 2. Value-at-Risk Alert
        if (!is.null(var_data) && nrow(var_data) > 0) {
            highest_var <- var_data %>%
                arrange(desc(VaR_99_bps)) %>%
                head(1)

            if (highest_var$VaR_99_bps > 50) {
                insights[[length(insights) + 1]] <- list(
                    icon = "⚠️",
                    text = sprintf("%s showing elevated risk: 99%% VaR at %.0f bps",
                                   highest_var$bond,
                                   highest_var$VaR_99_bps),
                    priority = 9,
                    category = "Risk Alert"
                )
            }
        }

        # 3. Technical Signal Convergence
        if (!is.null(processed_data) && nrow(processed_data) > 0) {
            tech_signals <- processed_data %>%
                filter(!is.na(signal_strength)) %>%
                filter(signal_strength %in% c("Strong Buy", "Strong Sell"))

            if (nrow(tech_signals) > 0) {
                top_signal <- tech_signals[1,]
                insights[[length(insights) + 1]] <- list(
                    icon = ifelse(grepl("Buy", top_signal$signal_strength), "🎯", "🚨"),
                    text = sprintf("%s: %s signal (RSI: %.1f, BB Position: %.2f)",
                                   top_signal$bond,
                                   top_signal$signal_strength,
                                   top_signal$rsi_14,
                                   top_signal$bb_position),
                    priority = 8,
                    category = "Trading Signal"
                )
            }
        }

        # 4. Carry & Roll Opportunity
        carry_data <- calculate_advanced_carry_roll(data, holding_periods = c(90))
        if (!is.null(carry_data) && nrow(carry_data) > 0) {
            best_carry <- carry_data %>%
                arrange(desc(return_per_unit_risk)) %>%
                head(1)

            insights[[length(insights) + 1]] <- list(
                icon = "💰",
                text = sprintf("Best risk-adjusted carry: %s with %.2f%% return per unit of risk (90-day)",
                               best_carry$bond,
                               best_carry$return_per_unit_risk),
                priority = 7,
                category = "Opportunity"
            )
        }

        # 5. Auction Momentum
        recent_auctions <- data %>%
            filter(!is.na(bid_to_cover),
                   date >= current_date - days(30)) %>%
            group_by(bond) %>%
            summarise(
                auction_count = n(),
                avg_btc = mean(bid_to_cover),
                trend = ifelse(n() >= 2,
                               coef(lm(bid_to_cover ~ as.numeric(date)))[2],
                               0),
                .groups = "drop"
            ) %>%
            filter(auction_count >= 2)

        if (nrow(recent_auctions) > 0) {
            best_momentum <- recent_auctions %>%
                arrange(desc(trend)) %>%
                head(1)

            if (abs(best_momentum$trend) > 0.01) {
                insights[[length(insights) + 1]] <- list(
                    icon = ifelse(best_momentum$trend > 0, "📈", "📉"),
                    text = sprintf("%s auction demand %s (avg %.2fx coverage)",
                                   best_momentum$bond,
                                   ifelse(best_momentum$trend > 0, "strengthening", "weakening"),
                                   best_momentum$avg_btc),
                    priority = 6,
                    category = "Auction Trend"
                )
            }
        }

        # Sort by priority and add visual categories
        insights <- insights[order(sapply(insights, function(x) -x$priority))]

        return(head(insights, 5))
    }, default = list())
}


#' @export
# Weekly Auction Summary
weekly_auction_summary <- reactive({
    req(filtered_data())

    # Get current date and week ahead
    today_date <- Sys.Date()
    week_ahead <- today_date + days(7)

    # Get bonds with recent auction activity
    recent_auctions <- filtered_data() %>%
        filter(!is.na(bid_to_cover),
               date >= today_date - days(90)) %>%
        group_by(bond) %>%
        summarise(
            last_auction_date = max(date),
            avg_btc = mean(bid_to_cover, na.rm = TRUE),
            last_btc = last(bid_to_cover),
            avg_offer = mean(offer_amount, na.rm = TRUE) / 1e9,
            modified_duration = mean(modified_duration, na.rm = TRUE),
            yield = last(yield_to_maturity),
            .groups = "drop"
        ) %>%
        mutate(
            # Predict next auction (simplified - every 30 days)
            expected_next_auction = last_auction_date + days(30),
            days_to_auction = as.numeric(expected_next_auction - today_date),
            upcoming_this_week = days_to_auction <= 7 & days_to_auction >= 0
        )

    # Get bonds likely on auction this week
    upcoming <- recent_auctions %>%
        filter(upcoming_this_week) %>%
        mutate(
            # ML prediction placeholder (use ARIMA if available)
            predicted_btc = avg_btc * runif(n(), 0.9, 1.1),
            confidence = case_when(
                avg_btc > 3 ~ "High",
                avg_btc > 2.5 ~ "Medium",
                TRUE ~ "Low"
            )
        ) %>%
        arrange(expected_next_auction)

    list(
        upcoming_bonds = upcoming,
        summary_text = paste0(
            "Week of ", format(today_date, "%B %d, %Y"), "\n",
            nrow(upcoming), " bonds expected on auction\n",
            "Average expected bid-to-cover: ",
            sprintf("%.2fx", mean(upcoming$predicted_btc, na.rm = TRUE))
        )
    )
})