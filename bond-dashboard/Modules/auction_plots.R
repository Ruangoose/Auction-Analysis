#' @title Check for Auction Data Availability
#' @description Helper function to check if auction data columns exist
#' @keywords internal
check_auction_data <- function(data, required_cols = c()) {
    # Core auction columns that should exist
    core_cols <- c("bid_to_cover")

    # Combine with any additional required columns
    all_required <- unique(c(core_cols, required_cols))

    # Check which columns are missing
    missing <- setdiff(all_required, names(data))

    if (length(missing) > 0) {
        return(list(
            has_data = FALSE,
            missing_cols = missing,
            message = paste("Auction data columns missing:", paste(missing, collapse = ", "))
        ))
    }

    # Check if there's any actual auction data
    if (!"bid_to_cover" %in% names(data) || all(is.na(data$bid_to_cover))) {
        return(list(
            has_data = FALSE,
            missing_cols = NULL,
            message = "No auction data available (all bid_to_cover values are NA)"
        ))
    }

    return(list(has_data = TRUE, missing_cols = NULL, message = NULL))
}

#' @title Create "No Data" Plot
#' @description Returns an informative plot when auction data is unavailable
#' @keywords internal
create_no_auction_data_plot <- function(message = "Auction data not available") {
    ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = message,
                 size = 6, color = "#666666", hjust = 0.5, vjust = 0.5) +
        annotate("text", x = 0.5, y = 0.4,
                 label = "Please ensure your data includes: offer_amount, bids_received, bid_to_cover",
                 size = 4, color = "#999999", hjust = 0.5, vjust = 0.5) +
        theme_void() +
        theme(plot.background = element_rect(fill = "#f8f9fa", color = NA))
}

#' @export
# 14. Enhanced Auction Analytics Plot Generation
generate_enhanced_auction_analytics <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Check for auction data (silently - user sees message in plot)
    data_check <- check_auction_data(data, required_cols = c("offer_amount"))
    if (!data_check$has_data) {
        return(create_no_auction_data_plot(data_check$message))
    }

    auction_data <- data %>%
        filter(!is.na(bid_to_cover)) %>%
        mutate(
            success = case_when(
                bid_to_cover > 3 ~ "Strong",
                bid_to_cover > 2.5 ~ "Good",
                bid_to_cover > 2 ~ "Normal",
                TRUE ~ "Weak"
            ),
            success = factor(success, levels = c("Strong", "Good", "Normal", "Weak"))
        )

    if(nrow(auction_data) == 0) {
        return(NULL)
    }

    # Calculate per-bond statistics
    bond_stats <- auction_data %>%
        group_by(bond) %>%
        summarise(
            avg_btc = mean(bid_to_cover, na.rm = TRUE),
            n_auctions = n(),
            min_date = min(date),
            max_date = max(date),
            .groups = "drop"
        )

    # Join stats back to main data
    auction_data <- auction_data %>%
        left_join(bond_stats, by = "bond") %>%
        mutate(
            bond_label = paste0(bond, "\n(n=", n_auctions, ", Avg: ", sprintf("%.2fx", avg_btc), ")")
        )

    # Create base plot
    p <- ggplot(auction_data, aes(x = date, y = bid_to_cover)) +

        # Reference lines
        geom_hline(yintercept = 2,
                   linetype = "dotted",
                   color = "#666666",
                   alpha = 0.5,
                   linewidth = 0.5) +

        geom_hline(yintercept = 2.5,
                   linetype = "dashed",
                   color = insele_palette$secondary,
                   alpha = 0.5,
                   linewidth = 0.6) +

        geom_hline(yintercept = 3,
                   linetype = "solid",
                   color = insele_palette$success,
                   alpha = 0.5,
                   linewidth = 0.7) +

        # Lines connecting points (only if multiple points exist)
        geom_line(data = auction_data %>%
                      group_by(bond) %>%
                      filter(n() > 1) %>%
                      ungroup(),
                  color = insele_palette$dark_gray,
                  alpha = 0.3,
                  linewidth = 0.5) +

        # Actual auction points
        geom_point(aes(size = offer_amount/1e9,
                       fill = success,
                       color = success),
                   shape = 21,
                   stroke = 1.2,
                   alpha = 0.9) +

        # Faceting
        facet_wrap(~bond_label,
                   ncol = 4,
                   scales = "free_x") +

        # Color scales
        scale_fill_manual(
            values = c("Strong" = insele_palette$success,
                       "Good" = insele_palette$secondary,
                       "Normal" = insele_palette$warning,
                       "Weak" = insele_palette$danger),
            name = "Performance",
            guide = guide_legend(
                title.position = "top",
                title.hjust = 0.5,
                nrow = 1
            )
        ) +

        scale_color_manual(
            values = c("Strong" = insele_palette$success,
                       "Good" = insele_palette$secondary,
                       "Normal" = insele_palette$warning,
                       "Weak" = insele_palette$danger),
            guide = "none"
        ) +

        scale_size_continuous(
            range = c(2, 8),
            name = "Offer Size (R bn)",
            breaks = c(2, 5, 10, 15),
            guide = guide_legend(
                title.position = "top",
                title.hjust = 0.5,
                nrow = 1
            )
        ) +

        scale_x_date(
            date_breaks = "3 months",
            date_labels = "%b\n%y",
            expand = expansion(mult = c(0.05, 0.05))
        ) +

        scale_y_continuous(
            breaks = c(1, 2, 2.5, 3, 4, 5),
            labels = function(x) paste0(x, "x"),
            expand = expansion(mult = c(0.05, 0.1))
        ) +

        labs(
            title = "Comprehensive Auction Performance Analytics",
            subtitle = paste("Bid-to-cover ratios | Total auctions:",
                             nrow(auction_data),
                             "| Period:",
                             format(min(auction_data$date), "%b %Y"),
                             "to",
                             format(max(auction_data$date), "%b %Y")),
            x = NULL,
            y = "Bid-to-Cover Ratio",
            caption = "Reference lines: 2.0x (minimum) | 2.5x (good) | 3.0x (excellent) | Trend shown for bonds with 5+ auctions"
        ) +

        create_insele_theme() +
        theme(
            panel.spacing.x = unit(1.0, "lines"),
            panel.spacing.y = unit(2.5, "lines"),
            strip.background = element_rect(
                fill = insele_palette$primary,
                color = NA
            ),
            strip.text = element_text(
                color = "white",
                face = "bold",
                size = 10,
                margin = ggplot2::margin(3, 5, 3, 5)
            ),
            legend.position = "bottom",
            legend.box = "horizontal",
            panel.background = element_rect(
                fill = "#FAFBFC",
                color = NA
            ),
            panel.grid.major.y = element_line(
                color = "white",
                linewidth = 0.8
            ),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(
                color = insele_palette$light_gray,
                fill = NA,
                linewidth = 0.5
            ),
            plot.margin = ggplot2::margin(15, 15, 15, 15)
        )

    # Conditionally add smooth trends only for bonds with sufficient data
    bonds_with_trends <- bond_stats %>%
        filter(n_auctions >= 5) %>%
        pull(bond)

    if(length(bonds_with_trends) > 0) {
        for(bond_name in bonds_with_trends) {
            bond_subset <- auction_data %>% filter(bond == bond_name)

            tryCatch({
                p <- p +
                    geom_smooth(
                        data = bond_subset,
                        method = "lm",
                        se = FALSE,
                        color = insele_palette$primary,
                        linewidth = 0.8,
                        linetype = "solid",
                        alpha = 0.6
                    )
            }, error = function(e) {
                NULL
            })
        }
    }

    return(p)
}

#' @export
# 17. Historical Pattern Recognition
generate_auction_pattern_analysis <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Check for auction data (silently - user sees message in plot)
    data_check <- check_auction_data(data, required_cols = c("offer_amount"))
    if (!data_check$has_data) {
        return(create_no_auction_data_plot(data_check$message))
    }

    # Filter to last 2 years for pattern analysis
    two_years_ago <- Sys.Date() - 730

    # Analyze patterns with more relevant dimensions
    pattern_data <- data %>%
        filter(!is.na(bid_to_cover), date >= two_years_ago) %>%
        mutate(
            year = factor(year(date)),
            month_name = factor(months(date, abbreviate = TRUE),
                                levels = month.abb),
            quarter = paste0("Q", quarter(date)),
            year_month = floor_date(date, "month"),
            # Add maturity buckets for pattern analysis
            maturity_bucket = case_when(
                modified_duration <= 3 ~ "Short (≤3y)",
                modified_duration <= 7 ~ "Medium (3-7y)",
                modified_duration <= 12 ~ "Long (7-12y)",
                TRUE ~ "Ultra-Long (>12y)"
            ),
            # Auction size categories
            size_category = case_when(
                offer_amount < 2e9 ~ "Small (<R2bn)",
                offer_amount < 5e9 ~ "Medium (R2-5bn)",
                offer_amount < 10e9 ~ "Large (R5-10bn)",
                TRUE ~ "Jumbo (>R10bn)"
            )
        )

    if(nrow(pattern_data) < 10) {
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = "Insufficient data for pattern analysis\n(need 10+ auctions)",
                         size = 5, color = "grey50") +
                theme_void()
        )
    }

    # Calculate overall statistics for title
    overall_mean <- mean(pattern_data$bid_to_cover, na.rm = TRUE)
    overall_sd <- sd(pattern_data$bid_to_cover, na.rm = TRUE)
    n_auctions <- nrow(pattern_data)

    # 1. Year-over-Year Comparison with trend indicator
    year_stats <- pattern_data %>%
        group_by(year) %>%
        summarise(mean_btc = mean(bid_to_cover, na.rm = TRUE), .groups = "drop") %>%
        arrange(year)

    year_trend <- if (nrow(year_stats) >= 2) {
        diff(tail(year_stats$mean_btc, 2))
    } else {
        0
    }
    trend_arrow <- if (year_trend > 0.1) "↑" else if (year_trend < -0.1) "↓" else "→"

    p1 <- ggplot(pattern_data %>% filter(!is.na(year)),
                 aes(x = year, y = bid_to_cover)) +
        geom_boxplot(fill = "#1B3A6B", alpha = 0.7, outlier.shape = 21) +
        geom_hline(yintercept = overall_mean, linetype = "dashed", color = "#E53935") +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#FDD835") +
        labs(title = sprintf("Year-over-Year %s", trend_arrow),
             subtitle = "◆ = Mean | Dashed = Overall avg",
             x = "", y = "Bid-to-Cover") +
        theme_minimal(base_size = 10) +
        theme(
            plot.title = element_text(face = "bold", size = 11, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 8, color = "grey50"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
        )

    # 2. Seasonal Patterns (Monthly) - identify best/worst months
    month_stats <- pattern_data %>%
        group_by(month_name) %>%
        summarise(mean_btc = mean(bid_to_cover, na.rm = TRUE), .groups = "drop")
    best_month <- month_stats %>% slice_max(mean_btc, n = 1) %>% pull(month_name)
    worst_month <- month_stats %>% slice_min(mean_btc, n = 1) %>% pull(month_name)

    p2 <- ggplot(pattern_data %>% filter(!is.na(month_name)),
                 aes(x = month_name, y = bid_to_cover)) +
        geom_boxplot(fill = "#2E5090", alpha = 0.7, outlier.size = 1) +
        geom_hline(yintercept = overall_mean, linetype = "dashed", color = "#E53935") +
        labs(title = "Seasonal Patterns",
             subtitle = sprintf("Best: %s | Weakest: %s", best_month, worst_month),
             x = "", y = "") +
        theme_minimal(base_size = 10) +
        theme(
            plot.title = element_text(face = "bold", size = 11, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 8, color = "grey50"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
        )

    # 3. Quarterly Patterns
    p3 <- ggplot(pattern_data, aes(x = quarter, y = bid_to_cover)) +
        geom_boxplot(fill = "#4A7C59", alpha = 0.7) +
        geom_hline(yintercept = overall_mean, linetype = "dashed", color = "#E53935") +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#FDD835") +
        labs(title = "Quarterly Patterns",
             subtitle = "◆ = Mean",
             x = "", y = "") +
        theme_minimal(base_size = 10) +
        theme(
            plot.title = element_text(face = "bold", size = 11, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 8, color = "grey50")
        )

    # 4. Enhanced trend with confidence bands
    trend_data <- pattern_data %>%
        arrange(date) %>%
        mutate(
            rolling_mean = zoo::rollapply(bid_to_cover, 5, mean, fill = NA, align = "right"),
            rolling_sd = zoo::rollapply(bid_to_cover, 5, sd, fill = NA, align = "right"),
            upper = rolling_mean + rolling_sd,
            lower = rolling_mean - rolling_sd
        )

    p4 <- ggplot(trend_data, aes(x = date)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1B3A6B", alpha = 0.2,
                    na.rm = TRUE) +
        geom_point(aes(y = bid_to_cover), size = 2, alpha = 0.5, color = "#1B3A6B") +
        geom_line(aes(y = rolling_mean), color = "#E57C00", linewidth = 1.2,
                  na.rm = TRUE) +
        geom_hline(yintercept = overall_mean, linetype = "dashed", color = "#E53935") +
        labs(title = "Trend with Confidence Band",
             subtitle = "Orange = 5-auction MA | Dashed = Overall mean",
             x = "", y = "Bid-to-Cover") +
        scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
        theme_minimal(base_size = 10) +
        theme(
            plot.title = element_text(face = "bold", size = 11, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 8, color = "grey50")
        )

    # Combine with patchwork if available, otherwise use arrangeGrob
    if (requireNamespace("patchwork", quietly = TRUE)) {
        combined <- (p1 | p2 | p3) / p4 +
            patchwork::plot_annotation(
                title = "Historical Auction Patterns Analysis",
                subtitle = sprintf("Based on %d auctions | Overall avg: %.2fx ± %.2fx | Dashed red = overall average",
                                  n_auctions, overall_mean, overall_sd),
                theme = theme(
                    plot.title = element_text(face = "bold", size = 14, color = "#1B3A6B"),
                    plot.subtitle = element_text(size = 10, color = "grey50")
                )
            )
        return(combined)
    } else {
        # Fallback to arrangeGrob
        return(gridExtra::arrangeGrob(
            p1, p2, p3, p4,
            ncol = 2, nrow = 2,
            top = grid::textGrob(
                sprintf("Historical Auction Patterns | %d auctions | Avg: %.2fx",
                       n_auctions, overall_mean),
                gp = grid::gpar(fontsize = 14, fontface = 2, col = "#1B3A6B")
            )
        ))
    }
}

#' @export
# 14b. Auction forecast visualization - focuses on bonds WITH forecasts
generate_auction_forecast_plot <- function(data, selected_bonds) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    if(length(selected_bonds) == 0) {
        return(NULL)
    }

    # First pass: identify bonds with sufficient data for forecasts
    bonds_with_data <- sapply(selected_bonds, function(bond) {
        n <- data %>%
            filter(bond == !!bond, !is.na(bid_to_cover)) %>%
            nrow()
        n > 5  # Need at least 6 data points for forecast
    })

    # Focus on bonds with forecasts only
    forecast_bonds <- selected_bonds[bonds_with_data]

    # Limit to max 6 bonds for readability
    if (length(forecast_bonds) > 6) {
        # Prioritize by number of data points
        bond_counts <- sapply(forecast_bonds, function(bond) {
            data %>%
                filter(bond == !!bond, !is.na(bid_to_cover)) %>%
                nrow()
        })
        forecast_bonds <- forecast_bonds[order(bond_counts, decreasing = TRUE)][1:6]
        message(sprintf("[CHART] Limiting to 6 bonds (from %d) for readability", length(selected_bonds)))
    }

    # If no bonds have forecasts, show message and return NULL
    if (length(forecast_bonds) == 0) {
        message("[CHART] No bonds with sufficient data for forecasts")
        return(NULL)
    }

    forecast_data <- data.frame()

    for(bond in forecast_bonds) {
        hist_data <- data %>%
            filter(bond == !!bond, !is.na(bid_to_cover)) %>%
            arrange(date)

        if(nrow(hist_data) > 5) {
            # Generate forecast
            ts_data <- ts(hist_data$bid_to_cover, frequency = 12)
            model <- tryCatch({
                auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE,
                           approximation = FALSE, trace = FALSE)
            }, error = function(e) {
                message(sprintf("[CHART ERROR] ARIMA failed for %s: %s", bond, e$message))
                NULL
            })

            if (is.null(model)) next

            fc <- forecast(model, h = 3, level = c(80, 95))

            # Combine historical and forecast
            future_dates <- seq(max(hist_data$date) + days(30),
                                by = "month", length.out = 3)

            forecast_df <- data.frame(
                date = c(hist_data$date, future_dates),
                bid_to_cover = c(hist_data$bid_to_cover, as.numeric(fc$mean)),
                lower_80 = c(rep(NA, nrow(hist_data)), as.numeric(fc$lower[,1])),
                upper_80 = c(rep(NA, nrow(hist_data)), as.numeric(fc$upper[,1])),
                lower_95 = c(rep(NA, nrow(hist_data)), as.numeric(fc$lower[,2])),
                upper_95 = c(rep(NA, nrow(hist_data)), as.numeric(fc$upper[,2])),
                bond = bond,
                type = c(rep("Historical", nrow(hist_data)), rep("Forecast", 3))
            )

            forecast_data <- rbind(forecast_data, forecast_df)
        }
    }

    if(nrow(forecast_data) == 0) {
        return(NULL)
    }

    p <- ggplot(forecast_data, aes(x = date)) +

        # 95% CI
        geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = bond),
                    alpha = 0.15, na.rm = TRUE) +

        # 80% CI
        geom_ribbon(aes(ymin = lower_80, ymax = upper_80, fill = bond),
                    alpha = 0.25, na.rm = TRUE) +

        # Historical line
        geom_line(data = filter(forecast_data, type == "Historical"),
                  aes(y = bid_to_cover, color = bond),
                  size = 1.2, na.rm = TRUE) +

        # Historical points
        geom_point(data = filter(forecast_data, type == "Historical"),
                   aes(y = bid_to_cover, color = bond),
                   size = 2, na.rm = TRUE) +

        # Forecast line
        geom_line(data = filter(forecast_data, type == "Forecast"),
                  aes(y = bid_to_cover, color = bond),
                  size = 1.2, linetype = "dashed", na.rm = TRUE) +

        # Forecast points
        geom_point(data = filter(forecast_data, type == "Forecast"),
                   aes(y = bid_to_cover, color = bond),
                   size = 3, shape = 21, fill = "white", na.rm = TRUE) +

        geom_hline(yintercept = 2, linetype = "dotted",
                   color = "#666", alpha = 0.7) +

        scale_color_manual(values = insele_palette$categorical) +
        scale_fill_manual(values = insele_palette$categorical, guide = "none") +

        scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +

        labs(
            title = "Bid-to-Cover Forecast",
            subtitle = "Historical data with 3-month forecast",
            x = "",
            y = "Bid-to-Cover Ratio",
            color = "Bond"
        ) +

        create_insele_theme() +
        theme(
            legend.position = "bottom",
            panel.grid.minor = element_blank()
        )

    return(p)
}

#' @export
# 15. Demand Elasticity Analysis
generate_demand_elasticity_plot <- function(data, params) {
    # Check for auction data (silently - user sees message in plot)
    data_check <- check_auction_data(data, required_cols = c("offer_amount"))
    if (!data_check$has_data) {
        return(create_no_auction_data_plot(data_check$message))
    }

    # ════════════════════════════════════════════════════════════════════════
    # CALCULATE DEMAND ELASTICITY WITH ROBUST VALIDATION
    # ════════════════════════════════════════════════════════════════════════
    # Elasticity = % change in bid-to-cover / % change in offer size
    # Extreme values indicate:
    # - Division by near-zero (small offer changes)
    # - Insufficient data quality
    # - Outlier contamination

    elasticity_data <- data %>%
        filter(!is.na(bid_to_cover), !is.na(offer_amount)) %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            offer_change = (offer_amount - lag(offer_amount)) / lag(offer_amount) * 100,
            btc_change = (bid_to_cover - lag(bid_to_cover)) / lag(bid_to_cover) * 100,

            # ════════════════════════════════════════════════════════════════
            # APPLY MINIMUM CHANGE THRESHOLD
            # ════════════════════════════════════════════════════════════════
            # Only calculate elasticity when offer change is meaningful (>5%)
            # This prevents division by near-zero which causes extreme values

            elasticity_raw = ifelse(
                abs(offer_change) >= 5,  # Minimum 5% change required
                btc_change / offer_change,
                NA_real_
            ),

            # ════════════════════════════════════════════════════════════════
            # DETECT AND FLAG OUTLIERS
            # ════════════════════════════════════════════════════════════════
            # Flag extreme values before capping for diagnostic purposes

            is_outlier = !is.na(elasticity_raw) & abs(elasticity_raw) > 10,

            # ════════════════════════════════════════════════════════════════
            # APPLY BOUNDS
            # ════════════════════════════════════════════════════════════════
            # Cap elasticity at ±10 (typical range is -2 to +2)
            # Values beyond ±10 indicate calculation errors or data quality issues

            elasticity = pmax(pmin(elasticity_raw, 10), -10)
        ) %>%
        ungroup()

    # ════════════════════════════════════════════════════════════════════════
    # DATA QUALITY FILTERING
    # ════════════════════════════════════════════════════════════════════════

    # Count auctions per bond and calculate data quality metrics
    bond_quality <- elasticity_data %>%
        group_by(bond) %>%
        summarise(
            n_auctions = n(),
            n_valid_elasticity = sum(!is.na(elasticity)),
            n_outliers = sum(is_outlier, na.rm = TRUE),
            offer_cv = sd(offer_amount, na.rm = TRUE) / mean(offer_amount, na.rm = TRUE) * 100,
            btc_cv = sd(bid_to_cover, na.rm = TRUE) / mean(bid_to_cover, na.rm = TRUE) * 100,
            .groups = "drop"
        ) %>%
        mutate(
            # Quality checks
            sufficient_data = n_auctions >= 5,
            sufficient_variance = offer_cv > 10,  # At least 10% coefficient of variation
            has_valid_points = n_valid_elasticity >= 3,

            # Overall quality flag
            quality_ok = sufficient_data & sufficient_variance & has_valid_points
        )

    # Join quality flags back to elasticity data
    elasticity_data <- elasticity_data %>%
        left_join(bond_quality, by = "bond")

    # Filter to only include high-quality data
    elasticity_data_filtered <- elasticity_data %>%
        filter(quality_ok, !is.na(elasticity))

    # Log filtering results
    message(sprintf(
        "Demand elasticity: %d bonds total, %d bonds with sufficient data quality",
        n_distinct(elasticity_data$bond),
        n_distinct(elasticity_data_filtered$bond)
    ))

    # Report outliers detected
    outliers_summary <- elasticity_data %>%
        filter(is_outlier) %>%
        select(bond, date, offer_change, btc_change, elasticity_raw) %>%
        arrange(desc(abs(elasticity_raw)))

    if(nrow(outliers_summary) > 0) {
        message(sprintf("  Warning: %d outlier elasticity values detected (capped at ±10):",
                        nrow(outliers_summary)))
        for(i in 1:min(3, nrow(outliers_summary))) {
            message(sprintf("    %s: %.1fx (offer Δ=%.1f%%, btc Δ=%.1f%%)",
                            outliers_summary$bond[i],
                            outliers_summary$elasticity_raw[i],
                            outliers_summary$offer_change[i],
                            outliers_summary$btc_change[i]))
        }
        if(nrow(outliers_summary) > 3) {
            message(sprintf("    ... and %d more", nrow(outliers_summary) - 3))
        }
    }

    if(nrow(elasticity_data_filtered) == 0) {
        return(create_no_auction_data_plot(
            "Insufficient data quality for elasticity analysis.\nRequires: ≥5 auctions per bond with ≥10% variance in offer sizes."
        ))
    }

    # Create elasticity curves using filtered high-quality data
    p <- ggplot(elasticity_data_filtered, aes(x = offer_change, y = btc_change)) +

        # Add quadrants
        geom_hline(yintercept = 0, linetype = "solid", color = "#666", alpha = 0.3) +
        geom_vline(xintercept = 0, linetype = "solid", color = "#666", alpha = 0.3) +

        # Annotate quadrants
        annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
                 fill = insele_palette$success, alpha = 0.05) +
        annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
                 fill = insele_palette$success, alpha = 0.05) +
        annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0,
                 fill = insele_palette$danger, alpha = 0.05) +
        annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
                 fill = insele_palette$warning, alpha = 0.05) +

        # Add smooth trend (only if sufficient data)
        {if(nrow(elasticity_data_filtered) >= 10)
            geom_smooth(method = "loess", se = TRUE,
                        color = insele_palette$primary,
                        fill = insele_palette$primary,
                        alpha = 0.2, size = 1.2)
        } +

        # Points colored by bond
        geom_point(aes(color = bond, size = abs(elasticity)),
                   alpha = 0.7) +

        # Add labels for notable points (elasticity >2 or <-2)
        ggrepel::geom_label_repel(
            data = smart_label(filter(elasticity_data_filtered, abs(elasticity) > 2),
                               "bond", "elasticity", max_labels = 8),
            aes(label = sprintf("%s\n%.1fx", bond, elasticity)),
            size = 3,
            max.overlaps = 15,
            box.padding = 0.3,
            alpha = 0.9
        ) +

        scale_color_manual(values = insele_palette$categorical) +
        scale_size_continuous(range = c(2, 8), guide = "none") +

        scale_x_continuous(labels = function(x) paste0(x, "%")) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +

        labs(
            title = "Demand Elasticity Analysis",
            subtitle = sprintf("Bid-to-cover sensitivity to offer size changes (filtered: %d bonds with quality data)",
                               n_distinct(elasticity_data_filtered$bond)),
            x = "Offer Size Change (%)",
            y = "Bid-to-Cover Change (%)",
            color = "Bond",
            caption = "Green zones: Normal response | Red: Inverse response | Size: Elasticity magnitude\nFiltered to show only bonds with ≥5 auctions and ≥10% offer variance. Elasticity capped at ±10."
        ) +

        create_insele_theme() +
        theme(legend.position = "bottom")

    return(p)
}

#' @export
# 16. Auction Success Probability
generate_success_probability_plot <- function(data, selected_bonds) {
    if(length(selected_bonds) == 0) {
        return(NULL)
    }

    # Calculate success probabilities based on historical data
    success_data <- data %>%
        filter(bond %in% selected_bonds,
               !is.na(bid_to_cover)) %>%
        group_by(bond) %>%
        summarise(
            total_auctions = n(),
            success_rate = sum(bid_to_cover > 2.5) / n() * 100,
            strong_success = sum(bid_to_cover > 3) / n() * 100,
            weak_performance = sum(bid_to_cover < 2) / n() * 100,
            avg_btc = mean(bid_to_cover),
            volatility = sd(bid_to_cover),
            .groups = "drop"
        ) %>%
        mutate(
            confidence = case_when(
                total_auctions > 20 ~ "High",
                total_auctions > 10 ~ "Medium",
                TRUE ~ "Low"
            )
        )

    if(nrow(success_data) == 0) {
        return(NULL)
    }

    # Reshape for stacked bar chart
    success_long <- success_data %>%
        select(bond, strong_success, success_rate, weak_performance) %>%
        mutate(
            moderate = success_rate - strong_success,
            acceptable = 100 - success_rate - weak_performance
        ) %>%
        select(bond, weak_performance, acceptable, moderate, strong_success) %>%
        pivot_longer(cols = -bond, names_to = "category", values_to = "probability")

    success_long$category <- factor(success_long$category,
                                    levels = c("weak_performance", "acceptable",
                                               "moderate", "strong_success"))

    p <- ggplot(success_long, aes(x = bond, y = probability, fill = category)) +

        geom_col(width = 0.7, color = "white", size = 0.5) +

        # Add text labels for key metrics
        geom_text(data = success_data,
                  aes(x = bond, y = 105, label = sprintf("%.1fx", avg_btc)),
                  inherit.aes = FALSE,
                  size = 4, fontface = 2) +

        scale_fill_manual(
            values = c(
                "weak_performance" = insele_palette$danger,
                "acceptable" = insele_palette$warning,
                "moderate" = insele_palette$secondary,
                "strong_success" = insele_palette$success
            ),
            labels = c(
                "weak_performance" = "Weak (<2x)",
                "acceptable" = "Acceptable (2-2.5x)",
                "moderate" = "Good (2.5-3x)",
                "strong_success" = "Strong (>3x)"
            ),
            name = "Performance Level"
        ) +

        scale_y_continuous(
            labels = function(x) paste0(x, "%"),
            expand = c(0, 0),
            limits = c(0, 110)
        ) +

        coord_flip() +

        labs(
            title = "Auction Success Probability Profile",
            subtitle = paste("Based on historical performance of",
                             sum(success_data$total_auctions), "auctions"),
            x = "",
            y = "Probability (%)",
            caption = "Numbers show average bid-to-cover ratio"
        ) +

        create_insele_theme() +
        theme(
            legend.position = "bottom",
            legend.direction = "horizontal"
        )

    return(p)
}

#' @export
# 18. Bid Distribution Analysis
generate_bid_distribution_plot <- function(data, params) {
    # Check for auction data (silently - user sees message in plot)
    data_check <- check_auction_data(data, required_cols = c("bids_received"))
    if (!data_check$has_data) {
        return(create_no_auction_data_plot(data_check$message))
    }

    bid_data <- data %>%
        filter(!is.na(bid_to_cover), !is.na(bids_received)) %>%
        mutate(
            btc_category = case_when(
                bid_to_cover < 2 ~ "Weak (<2x)",
                bid_to_cover < 2.5 ~ "Moderate (2-2.5x)",
                bid_to_cover < 3 ~ "Good (2.5-3x)",
                TRUE ~ "Strong (>3x)"
            ),
            btc_category = factor(btc_category,
                                  levels = c("Weak (<2x)", "Moderate (2-2.5x)",
                                             "Good (2.5-3x)", "Strong (>3x)"))
        )

    if(nrow(bid_data) == 0) {
        return(NULL)
    }

    # Calculate category statistics for labels and insights
    category_stats <- bid_data %>%
        group_by(btc_category) %>%
        summarise(
            n_auctions = n(),
            pct_of_total = n() / nrow(bid_data) * 100,
            total_bids = sum(bids_received, na.rm = TRUE),
            avg_bids = mean(bids_received, na.rm = TRUE),
            .groups = "drop"
        )

    # Get percentages for insight text
    strong_pct <- category_stats %>%
        filter(btc_category == "Strong (>3x)") %>%
        pull(pct_of_total) %>%
        {if(length(.) == 0) 0 else .}
    weak_pct <- category_stats %>%
        filter(btc_category == "Weak (<2x)") %>%
        pull(pct_of_total) %>%
        {if(length(.) == 0) 0 else .}

    # Build insight text
    insight_text <- sprintf("Key Finding: %.0f%% of auctions achieved >3x coverage (Strong), %.0f%% were weak (<2x)",
                           strong_pct, weak_pct)

    # Create violin plot with category counts
    p <- ggplot(bid_data, aes(x = btc_category, y = bids_received/1e9)) +

        geom_violin(aes(fill = btc_category),
                    alpha = 0.6,
                    trim = FALSE,
                    color = NA) +

        geom_boxplot(width = 0.2,
                     fill = "white",
                     alpha = 0.9,
                     outlier.shape = 21) +

        geom_jitter(aes(color = bond),
                    width = 0.1,
                    alpha = 0.6,
                    size = 2) +

        # Add count labels below each category
        geom_text(
            data = category_stats,
            aes(x = btc_category, y = -max(bid_data$bids_received/1e9) * 0.08,
                label = sprintf("n=%d\n(%.0f%%)", n_auctions, pct_of_total)),
            size = 3, color = "grey30"
        ) +

        scale_fill_manual(
            values = c(
                "Weak (<2x)" = "#FFCDD2",
                "Moderate (2-2.5x)" = "#FFF9C4",
                "Good (2.5-3x)" = "#C8E6C9",
                "Strong (>3x)" = "#81C784"
            ),
            guide = "none"
        ) +

        scale_color_manual(values = insele_palette$categorical,
                           name = "Bond") +

        scale_y_continuous(
            labels = function(x) paste0("R", x, "bn"),
            breaks = scales::pretty_breaks(n = 6),
            expand = expansion(mult = c(0.15, 0.05))  # Make room for count labels
        ) +

        labs(
            title = "Bid Distribution by Auction Performance",
            subtitle = "Total bid amounts across bid-to-cover categories",
            x = "Bid-to-Cover Category",
            y = "Total Bids",
            caption = paste(insight_text, "\nPoints colored by bond | Based on", nrow(bid_data), "auctions")
        ) +

        theme_minimal(base_size = 11) +
        theme(
            plot.title = element_text(face = "bold", size = 13, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 10, color = "grey50"),
            plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            legend.title = element_blank()
        )

    return(p)
}

#' @export
# 19. Sophisticated Market Sentiment Gauge using ggplot2
generate_auction_sentiment_gauge <- function(data, params) {
    # Check for auction data (silently - user sees message in plot)
    data_check <- check_auction_data(data, required_cols = c("bids_received"))
    if (!data_check$has_data) {
        return(create_no_auction_data_plot(data_check$message))
    }

    # Calculate comprehensive sentiment indicators
    sentiment_metrics <- data %>%
        filter(!is.na(bid_to_cover),
               date >= today() - days(90)) %>%
        arrange(date) %>%
        mutate(
            period = case_when(
                date >= today() - days(30) ~ "30d",
                date >= today() - days(60) ~ "60d",
                TRUE ~ "90d"
            )
        )

    # Increased minimum threshold for meaningful sentiment analysis
    if(nrow(sentiment_metrics) < 5) {
        return(create_no_auction_data_plot(
            "Insufficient auction data for sentiment analysis (minimum 5 auctions required)"
        ))
    }

    # FIX 1: Helper function to safely calculate metrics with fallback time windows
    safe_mean_with_fallback <- function(values, periods, target_period, fallback_periods) {
        result <- mean(values[periods == target_period], na.rm = TRUE)
        period_count <- sum(periods == target_period)

        # Check if result is NaN or there's no data (this handles the NaN bug!)
        if (is.nan(result) || is.na(result) || period_count == 0) {
            # Try fallback periods in order: 60d, then 90d
            for (fallback in fallback_periods) {
                result <- mean(values[periods == fallback], na.rm = TRUE)
                period_count <- sum(periods == fallback)
                if (!is.nan(result) && !is.na(result) && period_count > 0) {
                    message(sprintf("Auction Sentiment: Using %s data (no %s data available)",
                                    fallback, target_period))
                    return(list(value = result, period_used = fallback, count = period_count))
                }
            }
            # If all fallbacks fail, use all available data
            result <- mean(values, na.rm = TRUE)
            period_count <- length(values[!is.na(values)])
            message("Auction Sentiment: Using all available data (insufficient recent data)")
            return(list(value = result, period_used = "90d", count = period_count))
        }
        return(list(value = result, period_used = target_period, count = period_count))
    }

    # FIX 2: Calculate metrics with robust NaN handling and fallback windows
    # BTC metrics with fallback
    btc_recent <- safe_mean_with_fallback(
        sentiment_metrics$bid_to_cover,
        sentiment_metrics$period,
        "30d",
        c("60d", "90d")
    )
    btc_30d_value <- btc_recent$value
    period_used <- btc_recent$period_used
    count_recent <- btc_recent$count

    btc_90d_value <- mean(sentiment_metrics$bid_to_cover, na.rm = TRUE)

    # Calculate trend safely (guard against division by zero and NaN)
    btc_trend <- if (!is.na(btc_30d_value) && !is.nan(btc_30d_value) &&
                     !is.na(btc_90d_value) && !is.nan(btc_90d_value) &&
                     btc_90d_value != 0) {
        (btc_30d_value - btc_90d_value) / btc_90d_value * 100
    } else {
        0  # Neutral trend if we can't calculate
    }

    # Participation metrics with fallback
    bids_recent <- safe_mean_with_fallback(
        sentiment_metrics$bids_received,
        sentiment_metrics$period,
        "30d",
        c("60d", "90d")
    )
    recent_avg_bids <- if (!is.na(bids_recent$value) && !is.nan(bids_recent$value)) {
        bids_recent$value / 1e9
    } else {
        NA_real_
    }

    hist_avg_bids <- mean(sentiment_metrics$bids_received, na.rm = TRUE) / 1e9

    # Calculate participation change safely
    participation_change <- if (!is.na(recent_avg_bids) && !is.nan(recent_avg_bids) &&
                                !is.na(hist_avg_bids) && !is.nan(hist_avg_bids) &&
                                hist_avg_bids != 0) {
        (recent_avg_bids - hist_avg_bids) / hist_avg_bids * 100
    } else {
        0  # Neutral if we can't calculate
    }

    # FIX 3: Success rates with safe division (prevent division by zero!)
    success_30d <- if (count_recent > 0) {
        sum(sentiment_metrics$bid_to_cover[sentiment_metrics$period == period_used] > 2.5,
            na.rm = TRUE) / count_recent * 100
    } else {
        NA_real_
    }

    success_90d <- sum(sentiment_metrics$bid_to_cover > 2.5, na.rm = TRUE) /
        nrow(sentiment_metrics) * 100

    # Volatility with fallback (use recent period data, or all data if insufficient)
    vol_data <- sentiment_metrics$bid_to_cover[sentiment_metrics$period == period_used]
    volatility <- if (length(vol_data) > 1) {
        sd(vol_data, na.rm = TRUE)
    } else {
        sd(sentiment_metrics$bid_to_cover, na.rm = TRUE)
    }

    # Handle NaN/NA in volatility
    if (is.na(volatility) || is.nan(volatility)) {
        volatility <- 0
    }

    # Store all metrics for later use
    current_metrics <- list(
        btc_30d = btc_30d_value,
        btc_90d = btc_90d_value,
        btc_trend = btc_trend,
        recent_avg_bids = recent_avg_bids,
        hist_avg_bids = hist_avg_bids,
        participation_change = participation_change,
        success_30d = success_30d,
        success_90d = success_90d,
        volatility = volatility,
        auction_count_30d = count_recent,
        auction_count_90d = nrow(sentiment_metrics),
        period_used = period_used
    )

    # FIX 4: Composite sentiment score with comprehensive NaN guards
    sentiment_score <- with(current_metrics, {
        # Start with neutral (0) to avoid NaN propagation
        score <- 0

        # Add trend component (only if valid)
        if (!is.na(btc_trend) && !is.nan(btc_trend) && is.finite(btc_trend)) {
            score <- score + btc_trend * 2
        }

        # Add participation component (only if valid)
        if (!is.na(participation_change) && !is.nan(participation_change) &&
            is.finite(participation_change)) {
            score <- score + min(max(participation_change * 1.5, -30), 30)
        }

        # Add success rate component (only if valid)
        if (!is.na(success_30d) && !is.nan(success_30d) && is.finite(success_30d)) {
            score <- score + (success_30d - 50) * 0.4
        }

        # Subtract volatility penalty (only if valid)
        if (!is.na(volatility) && !is.nan(volatility) && is.finite(volatility) &&
            volatility > 0.5) {
            score <- score - 10
        }

        # Bound the score
        max(-100, min(100, score))
    })

    # FIX 5: CRITICAL VALIDATION - Ensure sentiment aligns with actual performance
    # This prevents showing "Very Weak" when BTC is actually good (2.5-3.5x)
    if (!is.na(btc_90d_value) && !is.nan(btc_90d_value) && is.finite(btc_90d_value)) {

        # If BTC > 2.5 (good performance) but sentiment is negative, correct it
        if (btc_90d_value > 2.5 && sentiment_score < -33) {
            warning(sprintf(
                "Auction Sentiment: Correcting negative sentiment (%.1f) despite good bid-to-cover (%.2fx)",
                sentiment_score, btc_90d_value
            ))
            # Adjust to at least "Balanced" range (minimum 0)
            sentiment_score <- max(0, sentiment_score)
        }

        # If BTC > 3.0 (strong performance), ensure positive sentiment
        if (btc_90d_value > 3.0 && sentiment_score < 0) {
            message(sprintf(
                "Auction Sentiment: Adjusting for strong performance (BTC: %.2fx)",
                btc_90d_value
            ))
            sentiment_score <- max(20, sentiment_score)
        }

        # If BTC < 2.0 (weak performance) but sentiment is very positive, moderate it
        if (btc_90d_value < 2.0 && sentiment_score > 33) {
            message(sprintf(
                "Auction Sentiment: Moderating positive sentiment given weak BTC (%.2fx)",
                btc_90d_value
            ))
            sentiment_score <- min(0, sentiment_score)
        }
    }

    # FIX 6: Diagnostic message when using fallback period
    if (period_used != "30d") {
        message(sprintf(
            "Auction Sentiment: Using %s window (%d auctions) instead of 30-day window",
            period_used, count_recent
        ))
    }

    # Create data for gauge visualization
    gauge_data <- data.frame(
        zone = c("Bearish", "Neutral", "Bullish"),
        xmin = c(-100, -33, 33),
        xmax = c(-33, 33, 100),
        ymin = 0,
        ymax = 1,
        color = c(insele_palette$danger, insele_palette$warning, insele_palette$success)
    )

    # FIX 8: Component data - Handle NaN in component calculations
    # Calculate success differential safely
    success_diff <- if (!is.na(current_metrics$success_30d) &&
                        !is.nan(current_metrics$success_30d)) {
        current_metrics$success_30d - current_metrics$success_90d
    } else {
        0  # Neutral if we can't calculate
    }

    components <- data.frame(
        metric = factor(c("Trend", "Volume", "Success", "Volatility"),
                        levels = c("Trend", "Volume", "Success", "Volatility")),
        value = c(
            ifelse(is.na(current_metrics$btc_trend) || is.nan(current_metrics$btc_trend),
                   0, current_metrics$btc_trend),
            ifelse(is.na(current_metrics$participation_change) || is.nan(current_metrics$participation_change),
                   0, current_metrics$participation_change),
            success_diff,
            -current_metrics$volatility * 20
        ),
        color = c(
            ifelse(!is.na(current_metrics$btc_trend) && !is.nan(current_metrics$btc_trend) &&
                       current_metrics$btc_trend > 0, insele_palette$success, insele_palette$danger),
            ifelse(!is.na(current_metrics$participation_change) && !is.nan(current_metrics$participation_change) &&
                       current_metrics$participation_change > 0, insele_palette$success, insele_palette$danger),
            ifelse(success_diff > 0, insele_palette$success, insele_palette$danger),
            insele_palette$warning
        )
    )

    # Main gauge plot
    p_gauge <- ggplot() +
        # Gradient background
        geom_rect(data = gauge_data,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = gauge_data$color, alpha = 0.3) +

        # Zone dividers
        geom_vline(xintercept = c(-33, 33), linetype = "dotted", alpha = 0.5) +

        # Zone labels
        geom_text(data = gauge_data,
                  aes(x = (xmin + xmax)/2, y = 0.85, label = zone),
                  size = 3, fontface = 2,
                  color = gauge_data$color) +

        # Sentiment needle
        geom_segment(x = sentiment_score, xend = sentiment_score,
                     y = 0.3, yend = 0.7,
                     arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                     size = 2.5, color = "black") +

        # Score box
        annotate("rect",
                 xmin = sentiment_score - 12, xmax = sentiment_score + 12,
                 ymin = 0.15, ymax = 0.3,
                 fill = "white", color = "black", size = 0.8) +
        annotate("text", x = sentiment_score, y = 0.225,
                 label = sprintf("%.0f", sentiment_score),
                 size = 4, fontface = 2) +

        # Scale
        scale_x_continuous(limits = c(-100, 100),
                           breaks = c(-100, -66, -33, 0, 33, 66, 100),
                           labels = c("-100", "", "", "0", "", "", "100")) +
        scale_y_continuous(limits = c(0, 1)) +

        labs(title = "Sentiment Index",
             subtitle = case_when(
                 sentiment_score > 66 ~ "Strong Demand",
                 sentiment_score > 33 ~ "Healthy Market",
                 sentiment_score > -33 ~ "Balanced",
                 sentiment_score > -66 ~ "Weak Demand",
                 TRUE ~ "Very Weak"
             )) +

        theme_minimal() +
        theme(
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 8, hjust = 0.5)
        )

    # Components bar chart
    p_components <- ggplot(components, aes(x = metric, y = value)) +
        geom_col(fill = components$color, alpha = 0.8, width = 0.6) +
        geom_hline(yintercept = 0, color = "#666", size = 0.5) +
        geom_text(aes(label = sprintf("%+.0f", value)),
                  vjust = ifelse(components$value > 0, -0.5, 1.5),
                  size = 2.5, fontface = 2) +
        scale_y_continuous(limits = c(-40, 40)) +
        labs(title = "Components",
             x = "", y = "") +
        theme_minimal() +
        theme(
            axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 9, face = "bold", hjust = 0.5)
        )

    # FIX 7: Metrics text - Show actual period used and handle NaN display
    metrics_text <- sprintf(
        "%s: %.2fx | 90d: %.2fx\nSuccess: %.0f%% | Vol: %.2f",
        current_metrics$period_used,  # Show actual period used (30d/60d/90d)
        ifelse(is.na(current_metrics$btc_30d) || is.nan(current_metrics$btc_30d),
               current_metrics$btc_90d,  # Fallback to 90d if 30d is NaN
               current_metrics$btc_30d),
        current_metrics$btc_90d,
        ifelse(is.na(current_metrics$success_30d) || is.nan(current_metrics$success_30d),
               current_metrics$success_90d,  # Fallback to 90d success if 30d is NaN
               current_metrics$success_30d),
        current_metrics$volatility
    )

    p_metrics <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = metrics_text,
                 size = 2.8, hjust = 0.5, vjust = 0.5,
                 color = "#333", family = if(Sys.info()["sysname"] == "Windows") "sans" else "Helvetica" ) +
        xlim(0, 1) + ylim(0, 1) +
        theme_void()

    # Arrange all components
    return(gridExtra::arrangeGrob(
        p_gauge,
        gridExtra::arrangeGrob(p_components, p_metrics, ncol = 2, widths = c(2, 1)),
        ncol = 1,
        heights = c(2, 1)
    ))
}

# Note: generate_auction_success_factors_plot function removed
# as part of Auction Intelligence tab overhaul

#' @export
# 20. Bid-to-Cover Decomposition Plot
generate_btc_decomposition_plot <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Check for auction data
    data_check <- check_auction_data(data, required_cols = c("offer_amount", "bids_received"))
    if (!data_check$has_data) {
        return(create_no_auction_data_plot(data_check$message))
    }

    # Decompose bid-to-cover into components
    btc_decomp <- data %>%
        filter(!is.na(bid_to_cover), !is.na(offer_amount), !is.na(bids_received)) %>%
        mutate(
            # Calculate decomposition metrics
            demand_ratio = bids_received / offer_amount,
            participation_rate = bid_to_cover,

            # Time-based categorization
            period = case_when(
                date >= today() - days(30) ~ "Last 30d",
                date >= today() - days(90) ~ "Last 90d",
                date >= today() - days(180) ~ "Last 180d",
                TRUE ~ "Older"
            ),
            period = factor(period, levels = c("Last 30d", "Last 90d",
                                               "Last 180d", "Older"))
        ) %>%
        group_by(bond) %>%
        mutate(
            ma_btc_20 = zoo::rollmean(bid_to_cover, k = min(20, n()),
                                      fill = NA, align = "right", partial = TRUE),
            btc_deviation = bid_to_cover - ma_btc_20,

            # Decompose into trend and cycle
            trend = ma_btc_20,
            cycle = btc_deviation,

            # Calculate volatility
            btc_vol = zoo::rollapply(bid_to_cover, width = min(20, n()),
                                     FUN = sd, fill = NA, align = "right", partial = TRUE)
        ) %>%
        ungroup()

    if(nrow(btc_decomp) < 10) {
        return(NULL)
    }

    # 1. Time series decomposition for selected bonds
    top_bonds <- btc_decomp %>%
        count(bond) %>%
        arrange(desc(n)) %>%
        head(4) %>%
        pull(bond)

    decomp_ts <- btc_decomp %>%
        filter(bond %in% top_bonds) %>%
        arrange(date)

    # Calculate per-bond stats for facet labels
    bond_stats <- decomp_ts %>%
        group_by(bond) %>%
        summarise(
            n = n(),
            avg_btc = mean(bid_to_cover, na.rm = TRUE),
            trend_slope = {
                if (n() >= 3) {
                    coef(lm(bid_to_cover ~ as.numeric(date), data = cur_data()))[2] * 365
                } else {
                    NA_real_
                }
            },
            .groups = "drop"
        ) %>%
        mutate(
            trend_arrow = case_when(
                is.na(trend_slope) ~ "",
                trend_slope > 0.1 ~ "↑",
                trend_slope < -0.1 ~ "↓",
                TRUE ~ "→"
            ),
            facet_label = sprintf("%s (n=%d, Avg: %.2fx) %s", bond, n, avg_btc, trend_arrow)
        )

    decomp_ts <- decomp_ts %>%
        left_join(bond_stats %>% select(bond, facet_label), by = "bond")

    p1 <- ggplot(decomp_ts, aes(x = date)) +
        geom_line(aes(y = bid_to_cover), color = "#1B3A6B", linewidth = 0.8) +
        geom_line(aes(y = trend), color = "#E57C00", linewidth = 1.2, linetype = "dashed",
                  na.rm = TRUE) +
        geom_ribbon(aes(ymin = trend - btc_vol, ymax = trend + btc_vol),
                    alpha = 0.15, fill = "#1B3A6B", na.rm = TRUE) +
        geom_hline(yintercept = 2.5, linetype = "dotted", color = "#E53935", alpha = 0.5) +
        facet_wrap(~facet_label, scales = "free_y", ncol = 2) +
        scale_x_date(date_labels = "%b\n'%y") +
        labs(title = "Bid-to-Cover Decomposition",
             subtitle = "Solid = Actual | Dashed orange = 20-auction trend | Shaded = ±1 vol | Dotted red = 2.5x threshold",
             x = "", y = "Bid-to-Cover Ratio") +
        theme_minimal(base_size = 10) +
        theme(
            plot.title = element_text(face = "bold", size = 12, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 9, color = "grey50"),
            strip.background = element_rect(fill = "#1B3A6B", color = NA),
            strip.text = element_text(color = "white", face = "bold", size = 9)
        )

    # 2. Component contribution analysis
    comp_analysis <- btc_decomp %>%
        group_by(period) %>%
        summarise(
            base_demand = mean(bid_to_cover, na.rm = TRUE),
            volatility_impact = mean(abs(cycle), na.rm = TRUE),
            trend_contribution = mean(trend, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        pivot_longer(cols = c(base_demand, volatility_impact, trend_contribution),
                     names_to = "component", values_to = "value")

    p2 <- ggplot(comp_analysis, aes(x = period, y = value, fill = component)) +
        geom_col(position = "dodge", width = 0.7) +
        scale_fill_manual(
            values = c(
                "base_demand" = insele_palette$primary,
                "volatility_impact" = insele_palette$warning,
                "trend_contribution" = insele_palette$secondary
            ),
            labels = c("Base Demand", "Volatility", "Trend"),
            name = "Component"
        ) +
        labs(title = "Component Contributions by Period",
             x = "", y = "Contribution to Bid-to-Cover") +
        create_insele_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # 3. Volatility regime analysis
    vol_regime <- btc_decomp %>%
        filter(!is.na(btc_vol)) %>%
        mutate(
            vol_regime = case_when(
                btc_vol < quantile(btc_vol, 0.33, na.rm = TRUE) ~ "Low Vol",
                btc_vol < quantile(btc_vol, 0.67, na.rm = TRUE) ~ "Medium Vol",
                TRUE ~ "High Vol"
            ),
            vol_regime = factor(vol_regime, levels = c("Low Vol", "Medium Vol", "High Vol"))
        )

    p3 <- ggplot(vol_regime, aes(x = vol_regime, y = bid_to_cover)) +
        geom_violin(aes(fill = vol_regime), alpha = 0.7) +
        geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
        scale_fill_manual(
            values = c(
                "Low Vol" = insele_palette$success,
                "Medium Vol" = insele_palette$warning,
                "High Vol" = insele_palette$danger
            ),
            guide = "none"
        ) +
        labs(title = "Performance by Volatility Regime",
             x = "Volatility Regime", y = "Bid-to-Cover") +
        create_insele_theme()

    # Arrange all plots
    return(gridExtra::arrangeGrob(
        p1,
        gridExtra::arrangeGrob(p2, p3, ncol = 2),
        nrow = 2,
        heights = c(2, 1)
    ))
}

# ═══════════════════════════════════════════════════════════════════════════════
# AUCTION QUALITY DASHBOARD VISUALIZATIONS
# ═══════════════════════════════════════════════════════════════════════════════

#' @title Create Auction Quality Heatmap
#' @description Creates a heatmap showing auction quality metrics by bond
#' @param auction_data Enhanced auction data with quality metrics
#' @return ggplot object
#' @export
create_auction_quality_heatmap <- function(auction_data) {

    # Aggregate by bond - latest 6 months
    six_months_ago <- Sys.Date() - 180

    # Check for required columns
    required_cols <- c("bond", "offer_date", "bid_to_cover")
    if (!all(required_cols %in% names(auction_data))) {
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = "Insufficient auction data for heatmap",
                         size = 5, color = "grey50") +
                theme_void()
        )
    }

    # Filter to recent data and aggregate
    bond_quality <- auction_data %>%
        filter(offer_date >= six_months_ago, !is.na(bid_to_cover)) %>%
        group_by(bond) %>%
        summarise(
            n_auctions = n(),
            avg_btc = mean(bid_to_cover, na.rm = TRUE),
            avg_tail = if ("auction_tail_bps" %in% names(.)) mean(auction_tail_bps, na.rm = TRUE) else NA_real_,
            avg_inst = if ("non_comp_ratio" %in% names(.)) mean(non_comp_ratio, na.rm = TRUE) else NA_real_,
            avg_concession = if ("auction_concession_bps" %in% names(.)) mean(auction_concession_bps, na.rm = TRUE) else NA_real_,
            avg_quality = if ("auction_quality_score" %in% names(.)) mean(auction_quality_score, na.rm = TRUE) else NA_real_,
            .groups = "drop"
        ) %>%
        filter(n_auctions >= 2)  # Minimum 2 auctions for reliability

    if (nrow(bond_quality) == 0) {
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = "Insufficient auction data for heatmap\n(need 2+ auctions per bond)",
                         size = 5, color = "grey50") +
                theme_void()
        )
    }

    # Reshape for heatmap
    heatmap_data <- bond_quality %>%
        pivot_longer(
            cols = c(avg_btc, avg_tail, avg_inst, avg_concession, avg_quality),
            names_to = "metric",
            values_to = "value"
        ) %>%
        mutate(
            metric = case_when(
                metric == "avg_btc" ~ "Bid-to-Cover",
                metric == "avg_tail" ~ "Tail (bps)",
                metric == "avg_inst" ~ "Institutional %",
                metric == "avg_concession" ~ "Concession (bps)",
                metric == "avg_quality" ~ "Quality Score"
            ),
            metric = factor(metric, levels = c("Bid-to-Cover", "Tail (bps)",
                                                "Institutional %", "Concession (bps)",
                                                "Quality Score")),
            # Normalize each metric to 0-100 for consistent color scale
            value_normalized = case_when(
                metric == "Bid-to-Cover" ~ pmin(100, pmax(0, (value - 1) / 4 * 100)),
                metric == "Tail (bps)" ~ pmax(0, 100 - value * 10),  # Inverted (lower is better)
                metric == "Institutional %" ~ pmin(100, value * 2.5),
                metric == "Concession (bps)" ~ pmax(0, 100 - abs(value) * 10),  # Closer to 0 is better
                metric == "Quality Score" ~ ifelse(is.na(value), 50, value),
                TRUE ~ 50
            ),
            # Format display value
            display_value = case_when(
                is.na(value) ~ "—",
                metric == "Bid-to-Cover" ~ sprintf("%.1fx", value),
                metric == "Tail (bps)" ~ sprintf("%.0f", value),
                metric == "Institutional %" ~ sprintf("%.0f%%", value),
                metric == "Concession (bps)" ~ sprintf("%+.0f", value),
                metric == "Quality Score" ~ sprintf("%.0f", value),
                TRUE ~ sprintf("%.1f", value)
            )
        )

    # Order bonds by quality score
    bond_order <- bond_quality %>%
        arrange(desc(avg_quality)) %>%
        pull(bond)

    heatmap_data <- heatmap_data %>%
        mutate(bond = factor(bond, levels = rev(bond_order)))

    # Build heatmap
    ggplot(heatmap_data, aes(x = metric, y = bond, fill = value_normalized)) +
        geom_tile(color = "white", linewidth = 1) +
        geom_text(aes(label = display_value), size = 3.2, fontface = "bold") +
        scale_fill_gradientn(
            colors = c("#FFCDD2", "#FFF9C4", "#C8E6C9", "#81C784", "#388E3C"),
            values = scales::rescale(c(0, 30, 50, 70, 100)),
            limits = c(0, 100),
            name = "Score",
            guide = guide_colorbar(barwidth = 1, barheight = 10),
            na.value = "#F5F5F5"
        ) +
        labs(
            title = "Auction Quality Metrics by Bond",
            subtitle = sprintf("Last 6 months | %d bonds with 2+ auctions", nrow(bond_quality)),
            x = NULL,
            y = NULL
        ) +
        theme_minimal(base_size = 11) +
        theme(
            plot.title = element_text(face = "bold", size = 13, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 9, color = "grey50"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
            axis.text.y = element_text(size = 10),
            legend.position = "right",
            panel.grid = element_blank()
        )
}

#' @title Create Concession Trend Chart
#' @description Creates a chart showing auction concession trends over time
#' @param auction_data Enhanced auction data with concession metrics
#' @return ggplot object
#' @export
create_concession_trend_chart <- function(auction_data) {

    # Check for required columns
    if (!"auction_concession_bps" %in% names(auction_data)) {
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = "Insufficient data for concession analysis\n(requires clearing_yield and secondary market YTM)",
                         size = 4, color = "grey50") +
                theme_void()
        )
    }

    # Filter to auctions with concession data
    concession_data <- auction_data %>%
        filter(!is.na(auction_concession_bps)) %>%
        arrange(offer_date)

    if (nrow(concession_data) < 5) {
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = sprintf("Insufficient data for concession analysis\n(%d auctions, need 5+)",
                                        nrow(concession_data)),
                         size = 4, color = "grey50") +
                theme_void()
        )
    }

    # Calculate rolling average
    concession_data <- concession_data %>%
        arrange(offer_date) %>%
        mutate(
            rolling_avg = zoo::rollapply(auction_concession_bps, 5, mean,
                                          fill = NA, align = "right")
        )

    # Get date range for annotations
    min_date <- min(concession_data$offer_date)
    max_date <- max(concession_data$offer_date)

    ggplot(concession_data, aes(x = offer_date)) +
        # Zero reference line
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +

        # Shaded regions for interpretation
        annotate("rect", xmin = min_date, xmax = max_date,
                 ymin = -Inf, ymax = -5, fill = "#C8E6C9", alpha = 0.3) +
        annotate("rect", xmin = min_date, xmax = max_date,
                 ymin = 5, ymax = Inf, fill = "#FFCDD2", alpha = 0.3) +

        # Individual auction points
        geom_point(aes(y = auction_concession_bps, color = bond),
                   size = 2.5, alpha = 0.7) +

        # Rolling average line
        geom_line(aes(y = rolling_avg), color = "#1B3A6B", linewidth = 1.2,
                  na.rm = TRUE) +

        # Labels
        scale_y_continuous(
            labels = function(x) sprintf("%+d bps", x)
        ) +
        scale_x_date(
            date_breaks = "2 months",
            date_labels = "%b %Y"
        ) +
        scale_color_manual(values = insele_palette$categorical, name = "Bond") +

        labs(
            title = "Auction Concession Trend",
            subtitle = "Clearing yield vs pre-auction secondary market | Line = 5-auction rolling avg",
            x = NULL,
            y = "Concession (bps)",
            caption = "Green zone = Strong (below market) | Red zone = Weak (premium required)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
            plot.title = element_text(face = "bold", size = 13, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 9, color = "grey50"),
            plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
            legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
}