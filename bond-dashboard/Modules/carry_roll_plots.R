#' @export
# 10. Enhanced Carry & Roll Heatmap Generation
generate_enhanced_carry_roll_heatmap <- function(data, return_type = "net", funding_rate = 8.25) {
    # Select return column based on input
    return_col <- switch(return_type,
                         "gross" = "gross_return",
                         "net" = "net_return",
                         "risk_adj" = "return_per_unit_risk",
                         "net_return")  # Default fallback

    # Check if the column exists
    if(!return_col %in% names(data)) {
        return_col <- "net_return"
    }

    # Get return type label for display
    return_type_label <- switch(return_type,
                                "gross" = "Gross",
                                "net" = "Net",
                                "risk_adj" = "Risk-Adjusted",
                                "Net")

    # ═══════════════════════════════════════════════════════════════════════
    # FIX 1: SORT BONDS BY 90-DAY RETURN (Most common trading horizon)
    # ═══════════════════════════════════════════════════════════════════════

    # Get bond order sorted by 90-day return (highest to lowest)
    bond_order <- data %>%
        filter(holding_period == "90d") %>%
        arrange(desc(!!sym(return_col))) %>%
        pull(bond)

    # If no 90d data, fall back to any available period
    if(length(bond_order) == 0) {
        bond_order <- data %>%
            arrange(desc(!!sym(return_col))) %>%
            distinct(bond) %>%
            pull(bond)
    }

    heatmap_data <- data %>%
        filter(!is.na(!!sym(return_col))) %>%
        select(bond, holding_period, all_of(return_col),
               any_of(c("carry_income", "roll_return", "funding_cost", "modified_duration"))) %>%
        rename(return_value = all_of(return_col)) %>%
        mutate(
            # Apply bond ordering (rev for ggplot - highest at top)
            bond = factor(bond, levels = rev(bond_order)),
            # Ensure holding period order is correct
            holding_period = factor(holding_period,
                                    levels = c("30d", "90d", "180d", "360d"))
        )

    if(nrow(heatmap_data) == 0) {
        return(NULL)
    }

    # ═══════════════════════════════════════════════════════════════════════
    # FIX 2: UPDATED COLOR SCALE FOR WIDER RETURN RANGE (removed 4% cap)
    # ═══════════════════════════════════════════════════════════════════════

    # Determine dynamic limits based on actual data
    max_return <- max(heatmap_data$return_value, na.rm = TRUE)
    min_return <- min(heatmap_data$return_value, na.rm = TRUE)

    # Extended color scale for returns up to 8%+ (high coupon bonds)
    return_colors <- c(
        "#FFEBEE",      # 0% - very light red (poor)
        "#FFCDD2",      # 0.5% - light red
        "#FFF9C4",      # 1.0% - yellow (break-even zone)
        "#C8E6C9",      # 2.0% - light green (acceptable)
        "#81C784",      # 3.0% - medium-light green
        "#66BB6A",      # 4.0% - medium green (good)
        "#43A047",      # 5.0% - medium-dark green
        "#2E7D32",      # 6.0% - dark green
        "#1B5E20"       # 8%+ - darkest green (excellent)
    )

    # Dynamic scale limits based on actual data range
    scale_max <- max(8, ceiling(max_return))

    # Use appropriate scale based on return range
    if(min_return < 0) {
        # Diverging scale for negative returns
        color_scale <- scale_fill_gradient2(
            low = "#C62828",           # Dark red for negative
            mid = "#FFF9C4",           # Yellow at zero
            high = "#1B5E20",          # Dark green for positive
            midpoint = 0,
            limits = c(min(min_return, -1), max(max_return, 6)),
            oob = scales::squish,
            name = "Return (%)",
            guide = guide_colorbar(
                title.position = "top",
                barwidth = 10,
                barheight = 0.5
            )
        )
    } else {
        # Sequential scale with dynamic breakpoints based on data
        color_scale <- scale_fill_gradientn(
            colors = return_colors,
            values = scales::rescale(c(0, 0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0)),
            limits = c(0, scale_max),
            oob = scales::squish,
            name = "Return (%)",
            guide = guide_colorbar(
                title.position = "top",
                barwidth = 10,
                barheight = 0.5
            )
        )
    }

    # Dynamic text color based on return value (white on dark backgrounds)
    text_colors <- case_when(
        heatmap_data$return_value > 4 ~ "white",     # White text on dark green
        heatmap_data$return_value < -0.5 ~ "white",  # White text on dark red
        TRUE ~ "black"                               # Black text otherwise
    )

    # Create heatmap with realistic scale
    p <- ggplot(heatmap_data, aes(x = holding_period, y = bond, fill = return_value)) +

        geom_tile(color = "white", linewidth = 1.5) +

        geom_text(aes(label = sprintf("%.2f%%", return_value)),
                  size = 4,
                  fontface = "bold",
                  color = text_colors) +

        color_scale +

        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +

        # ═══════════════════════════════════════════════════════════════════════
        # FIX 3: CLARIFY RETURN TYPE (Period vs Annualized)
        # ═══════════════════════════════════════════════════════════════════════
        labs(
            title = "Carry & Roll Analysis",
            subtitle = sprintf("Sorted by 90-day %s return (highest at top) | Funding: %.2f%%",
                               return_type_label, funding_rate),
            x = "Holding Period",
            y = "",
            caption = "Returns shown are total period returns (not annualized)"
        ) +

        create_insele_theme() +
        theme(
            axis.text = element_text(face = "bold"),
            panel.border = element_rect(fill = NA, color = insele_palette$dark_gray, linewidth = 2),
            legend.position = "bottom"
        )

    return(p)
}

#' @export
# 12. Optimal Holding Period Enhanced Plot Generation
generate_optimal_holding_enhanced_plot <- function(data) {
    # Check if holding_period column exists
    if(!"holding_period" %in% names(data)) {
        return(NULL)
    }

    # Extract numeric holding period and calculate ANNUALIZED metrics
    metrics <- data %>%
        filter(!is.na(holding_period), !is.na(net_return)) %>%
        mutate(
            holding_days = as.numeric(gsub("d$", "", holding_period))
        ) %>%
        group_by(holding_period, holding_days) %>%
        summarise(
            period_return = mean(net_return, na.rm = TRUE),
            return_std = sd(net_return, na.rm = TRUE),
            avg_duration = mean(modified_duration, na.rm = TRUE),
            n_bonds = n(),
            .groups = "drop"
        ) %>%
        arrange(holding_days) %>%
        mutate(
            # Annualize returns (how many times can you roll per year)
            periods_per_year = 365 / holding_days,
            annualized_return = period_return * periods_per_year,

            # Annualize volatility (scales with sqrt of time)
            annualized_vol = return_std * sqrt(periods_per_year),

            # Risk-free rate (SA T-bill ~7-8%)
            risk_free = 7.5,

            # Sharpe Ratio (now should vary meaningfully!)
            sharpe = (annualized_return - risk_free) / pmax(annualized_vol, 0.5),

            # Period label with rolls info
            period_label = paste0(holding_days, "d\n(", round(periods_per_year, 1), "x/yr)"),

            # Determine optimal (highest Sharpe)
            is_optimal = sharpe == max(sharpe, na.rm = TRUE)
        )

    if(nrow(metrics) == 0) {
        return(NULL)
    }

    # Get optimal period info
    optimal <- metrics %>% filter(is_optimal) %>% slice(1)
    risk_free_rate <- 7.5

    p <- ggplot(metrics, aes(x = factor(holding_days), y = annualized_return)) +

        # Bars for annualized return
        geom_col(
            aes(fill = is_optimal),
            width = 0.6,
            alpha = 0.9
        ) +

        # Risk-free line
        geom_hline(
            yintercept = risk_free_rate,
            linetype = "dashed",
            color = "#C62828",
            linewidth = 1
        ) +
        annotate(
            "text",
            x = 0.6, y = risk_free_rate + 1.5,
            label = sprintf("Risk-Free Rate (%.1f%%)", risk_free_rate),
            hjust = 0,
            color = "#C62828",
            size = 3,
            fontface = "italic"
        ) +

        # Value labels on top of bars
        geom_text(
            aes(label = sprintf("%.1f%%", annualized_return)),
            vjust = -0.5,
            fontface = "bold",
            size = 4
        ) +

        # Sharpe labels inside bars
        geom_text(
            aes(label = sprintf("SR: %.2f", sharpe),
                y = pmax(annualized_return / 2, 1)),
            color = "white",
            fontface = "bold",
            size = 3.5
        ) +

        # OPTIMAL label
        geom_label(
            data = metrics %>% filter(is_optimal),
            aes(y = annualized_return, label = "\u2605 OPTIMAL"),
            vjust = -2,
            fill = "#1B5E20",
            color = "white",
            fontface = "bold",
            size = 3,
            label.padding = unit(0.2, "lines")
        ) +

        scale_fill_manual(
            values = c("FALSE" = "#90CAF9", "TRUE" = "#1B5E20"),
            guide = "none"
        ) +

        scale_x_discrete(
            labels = function(x) {
                days <- as.numeric(x)
                rolls <- round(365 / days, 1)
                paste0(days, "d\n(", rolls, "x/yr)")
            }
        ) +

        scale_y_continuous(
            labels = function(x) sprintf("%.0f%%", x),
            expand = expansion(mult = c(0, 0.15))
        ) +

        labs(
            title = "Annualized Return by Holding Period",
            subtitle = "Which holding period generates highest annual return if repeatedly rolled?",
            x = "Holding Period (Rolls per Year)",
            y = "Annualized Net Return (%)",
            caption = sprintf(
                "Green = Optimal (%s, SR: %.2f) | Dashed line = Risk-free rate | SR = Sharpe Ratio",
                optimal$holding_period, optimal$sharpe
            )
        ) +

        create_insele_theme() +
        theme(
            legend.position = "none",
            panel.grid.major.x = element_blank(),
            plot.title = element_text(face = "bold", color = insele_palette$primary),
            plot.subtitle = element_text(color = "#666666", size = 10),
            plot.caption = element_text(size = 9, color = "#666666"),
            axis.text.x = element_text(size = 10)
        )

    # Add metrics as attribute for the decision table
    attr(p, "metrics") <- metrics

    return(p)
}

#' @export
# 13. Forward Curve Plot
generate_forward_curve_plot <- function(data, params) {
    # Calculate forward rates for visualization
    curve_data <- data %>%
        arrange(modified_duration)

    # Create forward rate points
    forward_points <- data.frame()

    for(i in 1:(nrow(curve_data)-1)) {
        for(j in (i+1):nrow(curve_data)) {
            r1 <- curve_data$yield_to_maturity[i] / 100
            r2 <- curve_data$yield_to_maturity[j] / 100
            t1 <- curve_data$modified_duration[i]
            t2 <- curve_data$modified_duration[j]

            if(t2 > t1) {
                fwd_rate <- ((1 + r2)^t2 / (1 + r1)^t1)^(1/(t2 - t1)) - 1

                forward_points <- rbind(forward_points, data.frame(
                    start_tenor = t1,
                    end_tenor = t2,
                    forward_period = t2 - t1,
                    forward_rate = fwd_rate * 100,
                    spot_rate = r2 * 100
                ))
            }
        }
    }

    if(nrow(forward_points) == 0) {
        return(NULL)
    }

    # Create the plot
    p <- ggplot() +

        # Spot curve
        geom_line(data = curve_data,
                  aes(x = modified_duration, y = yield_to_maturity),
                  color = insele_palette$primary,
                  size = 1.5,
                  alpha = 0.8) +
        geom_point(data = curve_data,
                   aes(x = modified_duration, y = yield_to_maturity),
                   color = insele_palette$primary,
                   size = 3) +

        # Forward rates
        geom_point(data = forward_points,
                   aes(x = end_tenor, y = forward_rate,
                       color = forward_period),
                   size = 2,
                   alpha = 0.7) +

        scale_color_viridis_c(
            name = "Forward\nPeriod (yrs)",
            option = "D"
        ) +

        scale_y_continuous(
            labels = function(x) paste0(x, "%"),
            breaks = pretty_breaks(n = 8)
        ) +

        scale_x_continuous(
            breaks = pretty_breaks(n = 10)
        ) +

        labs(
            title = "Spot Curve vs Forward Rates",
            subtitle = "Current yield curve (blue line) and implied forward rates (colored points)",
            x = "Tenor (years)",
            y = "Rate (%)",
            caption = "Forward rates show market expectations for future interest rates"
        ) +

        create_insele_theme() +
        theme(legend.position = "right")

    return(p)
}


# ================================================================================
# BUTTERFLY SPREAD ANALYZER FUNCTIONS
# ================================================================================

#' Calculate all possible butterfly spreads from bond data
#' @param bond_data Data frame with bond yield data (requires FULL historical data, not just latest snapshot)
#' @param lookback_days Number of days to look back for calculations
#' @return List of butterfly spreads with statistics
#' @export
calculate_butterfly_spreads <- function(bond_data, lookback_days = 365) {

    message("=== BUTTERFLY SPREAD CALCULATION START ===")
    message(sprintf("Input data: %d rows, %d cols", nrow(bond_data), ncol(bond_data)))
    message(sprintf("Columns: %s", paste(names(bond_data), collapse = ", ")))
    message(sprintf("Lookback days: %d", lookback_days))

    # Check yield units (are they 9.75 or 0.0975?)
    # If median yield > 1, yields are in percentage form (e.g., 9.75%)
    sample_yield <- median(bond_data$yield_to_maturity, na.rm = TRUE)
    yields_in_pct <- sample_yield > 1
    message(sprintf("Yield unit check: median=%.4f, in_pct=%s", sample_yield, yields_in_pct))

    # Check for required columns
    required_cols <- c("date", "bond", "yield_to_maturity", "modified_duration")
    missing_cols <- setdiff(required_cols, names(bond_data))
    if (length(missing_cols) > 0) {
        message(sprintf("ERROR: Missing required columns: %s", paste(missing_cols, collapse = ", ")))
        return(NULL)
    }

    # Check date range
    date_range <- range(bond_data$date, na.rm = TRUE)
    message(sprintf("Date range: %s to %s", date_range[1], date_range[2]))

    # Get unique bonds sorted by duration
    bonds_by_duration <- bond_data %>%
        group_by(bond) %>%
        summarise(
            avg_duration = mean(modified_duration, na.rm = TRUE),
            n_obs = n(),
            .groups = "drop"
        ) %>%
        filter(!is.na(avg_duration)) %>%
        arrange(avg_duration)

    message(sprintf("Bonds by duration: %d bonds", nrow(bonds_by_duration)))

    # Need at least 3 bonds
    if (nrow(bonds_by_duration) < 3) {
        message("ERROR: Need at least 3 bonds with duration data")
        return(NULL)
    }

    bond_names <- bonds_by_duration$bond

    # Filter to lookback period
    max_date <- max(bond_data$date, na.rm = TRUE)
    cutoff_date <- max_date - lubridate::days(lookback_days)
    message(sprintf("Cutoff date: %s (max date: %s)", cutoff_date, max_date))

    filtered_data <- bond_data %>%
        filter(date >= cutoff_date) %>%
        select(date, bond, yield_to_maturity) %>%
        filter(!is.na(yield_to_maturity))

    message(sprintf("Filtered data: %d rows", nrow(filtered_data)))

    if (nrow(filtered_data) == 0) {
        message("ERROR: No data after filtering to lookback period")
        return(NULL)
    }

    # Check data per bond
    data_per_bond <- filtered_data %>%
        group_by(bond) %>%
        summarise(n = n(), .groups = "drop")
    message(sprintf("Observations per bond (avg): %.1f", mean(data_per_bond$n)))

    # Pivot to wide format
    message("Pivoting to wide format...")
    yields_wide <- filtered_data %>%
        tidyr::pivot_wider(
            names_from = bond,
            values_from = yield_to_maturity,
            values_fn = mean  # Handle duplicates by averaging
        ) %>%
        arrange(date)

    message(sprintf("Wide data: %d rows (unique dates), %d cols", nrow(yields_wide), ncol(yields_wide)))

    if (nrow(yields_wide) < 60) {
        message(sprintf("WARNING: Only %d observations (need 60+ for reliable stats)", nrow(yields_wide)))
    }

    # Generate all valid butterfly combinations
    # Short wing (shortest) - 2×Body (middle) + Long wing (longest)
    n_bonds <- length(bond_names)
    message(sprintf("Generating butterflies from %d bonds...", n_bonds))

    butterflies <- list()
    combos_checked <- 0
    combos_valid <- 0

    for (i in 1:(n_bonds - 2)) {
        for (j in (i + 1):(n_bonds - 1)) {
            for (k in (j + 1):n_bonds) {

                combos_checked <- combos_checked + 1

                short_wing <- bond_names[i]
                body <- bond_names[j]
                long_wing <- bond_names[k]

                # Check if all three bonds exist in wide data
                if (!all(c(short_wing, body, long_wing) %in% names(yields_wide))) {
                    next
                }

                # Calculate butterfly spread time series
                spread_name <- sprintf("%s-%s-%s", short_wing, body, long_wing)

                spread_ts <- tryCatch({
                    yields_wide %>%
                        mutate(
                            butterfly_spread = .data[[short_wing]] - 2 * .data[[body]] + .data[[long_wing]]
                        ) %>%
                        filter(!is.na(butterfly_spread)) %>%
                        select(date, butterfly_spread)
                }, error = function(e) {
                    message(sprintf("Error calculating %s: %s", spread_name, e$message))
                    NULL
                })

                if (is.null(spread_ts) || nrow(spread_ts) < 60) {
                    next
                }

                combos_valid <- combos_valid + 1

                # Calculate statistics
                spread_mean <- mean(spread_ts$butterfly_spread, na.rm = TRUE)
                spread_sd <- sd(spread_ts$butterfly_spread, na.rm = TRUE)

                if (is.na(spread_sd) || spread_sd == 0) {
                    message(sprintf("WARNING: %s has zero/NA std dev, skipping", spread_name))
                    next
                }

                current_spread <- tail(spread_ts$butterfly_spread, 1)
                z_score <- (current_spread - spread_mean) / spread_sd

                # DEBUG: Log spread data details for first 5 combos to verify data varies
                if (combos_valid <= 5) {
                    message(sprintf("[SPREAD DEBUG] %s: n=%d, first=%.6f, last=%.6f, mean=%.6f, sd=%.6f",
                                    spread_name,
                                    nrow(spread_ts),
                                    head(spread_ts$butterfly_spread, 1),
                                    tail(spread_ts$butterfly_spread, 1),
                                    spread_mean,
                                    spread_sd))
                }

                # ADF test for stationarity with comprehensive logging
                adf_result <- tryCatch({
                    spread_vec <- spread_ts$butterfly_spread
                    n_obs <- length(spread_vec)

                    # Pre-test checks
                    if (n_obs < 20) {
                        message(sprintf("[ADF] %s: SKIP - insufficient data (%d obs)", spread_name, n_obs))
                        return(list(p.value = NA, statistic = NA, p_truncated = FALSE))
                    }

                    vec_sd <- sd(spread_vec, na.rm = TRUE)
                    if (is.na(vec_sd) || vec_sd < 1e-10) {
                        message(sprintf("[ADF] %s: SKIP - constant/near-constant series (sd=%.2e)", spread_name, vec_sd))
                        return(list(p.value = NA, statistic = NA, p_truncated = FALSE))
                    }

                    test_result <- tseries::adf.test(spread_vec, alternative = "stationary")

                    # Log detailed ADF results for first 10 butterflies
                    if (combos_valid <= 10) {
                        message(sprintf("[ADF] %s: stat=%.4f, p=%.4f, lag=%d, n=%d",
                                        spread_name,
                                        test_result$statistic,
                                        test_result$p.value,
                                        test_result$parameter,
                                        n_obs))
                    }

                    # tseries::adf.test truncates p-values at 0.01 and 0.99
                    # If p.value == 0.01, it's actually <= 0.01
                    list(
                        p.value = test_result$p.value,
                        statistic = test_result$statistic,
                        lag = test_result$parameter,
                        p_truncated = (test_result$p.value == 0.01)
                    )
                }, error = function(e) {
                    message(sprintf("[ADF] %s: ERROR - %s", spread_name, e$message))
                    list(p.value = NA, statistic = NA, p_truncated = FALSE)
                })

                # Store results - spread is already in correct units (percentage points if yields are in %)
                butterflies[[spread_name]] <- list(
                    name = spread_name,
                    short_wing = short_wing,
                    body = body,
                    long_wing = long_wing,
                    spread_ts = spread_ts,
                    mean = spread_mean,
                    sd = spread_sd,
                    current = current_spread,
                    z_score = z_score,
                    adf_pvalue = adf_result$p.value,
                    adf_statistic = adf_result$statistic,
                    adf_p_truncated = adf_result$p_truncated,
                    is_stationary = !is.na(adf_result$p.value) && adf_result$p.value < 0.05,
                    diff_from_mean = current_spread - spread_mean,
                    n_observations = nrow(spread_ts),
                    yields_in_pct = yields_in_pct  # Track yield units
                )
            }
        }
    }

    message(sprintf("Combinations checked: %d", combos_checked))
    message(sprintf("Valid butterflies (60+ obs): %d", combos_valid))
    message(sprintf("Final butterflies: %d", length(butterflies)))

    # DEBUG: Stationarity distribution summary
    if (length(butterflies) > 0) {
        stationary_count <- sum(sapply(butterflies, function(bf) isTRUE(bf$is_stationary)))
        non_stationary_count <- sum(sapply(butterflies, function(bf) !isTRUE(bf$is_stationary) && !is.na(bf$is_stationary)))
        na_count <- sum(sapply(butterflies, function(bf) is.na(bf$is_stationary)))

        message("=== STATIONARITY DISTRIBUTION ===")
        message(sprintf("Stationary (p<0.05): %d", stationary_count))
        message(sprintf("Non-Stationary (p>=0.05): %d", non_stationary_count))
        message(sprintf("NA/Error: %d", na_count))

        # ADF statistic range
        adf_stats <- sapply(butterflies, function(bf) bf$adf_statistic)
        adf_stats <- adf_stats[!is.na(adf_stats)]
        if (length(adf_stats) > 0) {
            message(sprintf("ADF stat range: %.4f to %.4f (mean=%.4f, sd=%.4f)",
                            min(adf_stats), max(adf_stats), mean(adf_stats), sd(adf_stats)))
        }

        # ADF p-value range
        adf_pvals <- sapply(butterflies, function(bf) bf$adf_pvalue)
        adf_pvals <- adf_pvals[!is.na(adf_pvals)]
        if (length(adf_pvals) > 0) {
            message(sprintf("ADF p-value range: %.4f to %.4f", min(adf_pvals), max(adf_pvals)))
            message(sprintf("P-values at 0.01 (truncated): %d", sum(adf_pvals == 0.01)))
            message(sprintf("P-values > 0.05 (non-stationary): %d", sum(adf_pvals > 0.05)))
        }

        # WARNING if all stationary
        if (stationary_count == length(butterflies)) {
            message("WARNING: ALL butterflies marked as stationary! This may indicate a bug.")
        }
        message("==================================")
    }

    message("=== BUTTERFLY SPREAD CALCULATION END ===")

    return(butterflies)
}


#' Generate butterfly spread chart with mean reversion bands
#' @param bf Butterfly spread object from calculate_butterfly_spreads
#' @param zscore_threshold Threshold for trade signals
#' @return ggplot object
#' @export
generate_butterfly_chart <- function(bf, zscore_threshold = 2.0) {
    if (is.null(bf)) return(NULL)

    # Check if yields were in percentage form (spread already in % points)
    yields_in_pct <- isTRUE(bf$yields_in_pct)

    # If yields are in % form, spread is already in percentage points - don't multiply by 100
    # If yields are in decimal form, multiply by 100 to convert to percentage points
    conversion_factor <- if (yields_in_pct) 1 else 100

    spread_ts <- bf$spread_ts %>%
        mutate(butterfly_pct = butterfly_spread * conversion_factor)

    mean_pct <- bf$mean * conversion_factor
    sd_pct <- bf$sd * conversion_factor
    current_pct <- bf$current * conversion_factor

    # Determine y-axis limits
    y_min <- min(spread_ts$butterfly_pct, mean_pct - 2.5 * sd_pct, na.rm = TRUE)
    y_max <- max(spread_ts$butterfly_pct, mean_pct + 2.5 * sd_pct, na.rm = TRUE)

    p <- ggplot(spread_ts, aes(x = date, y = butterfly_pct)) +

        # ±2 Std Dev band (outer)
        geom_ribbon(
            aes(ymin = mean_pct - 2 * sd_pct, ymax = mean_pct + 2 * sd_pct),
            fill = "#E3F2FD",
            alpha = 0.5
        ) +

        # ±1 Std Dev band (inner)
        geom_ribbon(
            aes(ymin = mean_pct - 1 * sd_pct, ymax = mean_pct + 1 * sd_pct),
            fill = "#BBDEFB",
            alpha = 0.5
        ) +

        # Mean line
        geom_hline(yintercept = mean_pct, linetype = "dashed", color = "#1B3A6B", linewidth = 1) +

        # ±1 Std Dev lines
        geom_hline(yintercept = mean_pct + sd_pct, linetype = "dotted", color = "#90A4AE") +
        geom_hline(yintercept = mean_pct - sd_pct, linetype = "dotted", color = "#90A4AE") +

        # ±2 Std Dev lines
        geom_hline(yintercept = mean_pct + 2 * sd_pct, linetype = "dotted", color = "#78909C") +
        geom_hline(yintercept = mean_pct - 2 * sd_pct, linetype = "dotted", color = "#78909C") +

        # Spread line
        geom_line(color = "#1976D2", linewidth = 0.8) +

        # Current point
        geom_point(
            data = tail(spread_ts, 1),
            aes(x = date, y = butterfly_pct),
            color = ifelse(abs(bf$z_score) > 2, "#C62828", "#FF9800"),
            size = 4
        ) +

        # Current value annotation
        annotate(
            "label",
            x = max(spread_ts$date),
            y = current_pct,
            label = sprintf("%.3f%%\nZ: %.2f", current_pct, bf$z_score),
            hjust = -0.1,
            fill = ifelse(abs(bf$z_score) > 2, "#FFCDD2", "#FFF9C4"),
            size = 3
        ) +

        # Axis formatting
        scale_y_continuous(
            labels = function(x) sprintf("%.2f%%", x),
            limits = c(y_min * 0.95, y_max * 1.05)
        ) +
        scale_x_date(
            date_breaks = "3 months",
            date_labels = "%b-%Y"
        ) +

        labs(
            title = sprintf("Butterfly Spread: %s (Z-Score: %.2f)", bf$name, bf$z_score),
            subtitle = sprintf("Mean: %.3f%% | Current: %.3f%% | Diff: %+.3f%%",
                               mean_pct, current_pct, (bf$diff_from_mean * conversion_factor)),
            x = NULL,
            y = "Butterfly Spread (%)",
            caption = "Dashed = Mean | Dotted = +/-1 and +/-2 sigma | Shaded bands show standard deviation zones"
        ) +

        create_insele_theme() +
        theme(
            plot.title = element_text(face = "bold", color = "#1B3A6B"),
            plot.subtitle = element_text(color = "#666666"),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )

    return(p)
}