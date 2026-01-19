#' @export
# 10. Enhanced Carry & Roll Heatmap Generation
generate_enhanced_carry_roll_heatmap <- function(data, return_type = "net", funding_rate = 8.25) {

    # Error handling wrapper for robustness
    tryCatch({
        # Input validation
        if(is.null(data) || nrow(data) == 0) {
            return(
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5,
                             label = "No data available for Carry & Roll analysis",
                             size = 5, color = "grey50") +
                    theme_void() +
                    labs(title = "Carry & Roll Analysis")
            )
        }

        # Select return column based on input
        return_col <- switch(return_type,
                             "gross" = "gross_return",
                             "net" = "net_return",
                             "risk_adj" = "return_per_unit_risk",
                             "net_return")  # Default fallback

        # Check if the column exists
        if(!return_col %in% names(data)) {
            return_col <- "net_return"
            if(!return_col %in% names(data)) {
                warning("No return column found in data")
                return(
                    ggplot() +
                        annotate("text", x = 0.5, y = 0.5,
                                 label = "Required return data not available",
                                 size = 5, color = "grey50") +
                        theme_void() +
                        labs(title = "Carry & Roll Analysis")
                )
            }
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
               any_of(c("carry_income", "roll_return", "funding_cost", "modified_duration",
                        "projection_truncated", "effective_horizon_days"))) %>%
        rename(return_value = all_of(return_col)) %>%
        mutate(
            # Apply bond ordering (rev for ggplot - highest at top)
            bond = factor(bond, levels = rev(bond_order)),
            # Ensure holding period order is correct
            holding_period = factor(holding_period,
                                    levels = c("30d", "90d", "180d", "360d")),
            # Add asterisk for truncated projections (bond matures before period ends)
            is_truncated = if ("projection_truncated" %in% names(.))
                               ifelse(is.na(projection_truncated), FALSE, projection_truncated)
                           else FALSE
        )

    if(nrow(heatmap_data) == 0) {
        return(NULL)
    }

    # ═══════════════════════════════════════════════════════════════════════
    # FIX 2: PROPER COLOR SCALE - GREEN FOR POSITIVE, RED FOR NEGATIVE
    # ═══════════════════════════════════════════════════════════════════════

    # Determine dynamic limits based on actual data
    max_return <- max(heatmap_data$return_value, na.rm = TRUE)
    min_return <- min(heatmap_data$return_value, na.rm = TRUE)

    # Use appropriate scale based on return range
    if(min_return < 0) {
        # Diverging scale when negative returns exist - centered on zero
        max_abs <- max(abs(min_return), abs(max_return))
        color_scale <- scale_fill_gradient2(
            low = "#D32F2F",           # Red for negative (bad)
            mid = "#FFFDE7",           # Light yellow at zero (neutral)
            high = "#388E3C",          # Green for positive (good)
            midpoint = 0,
            limits = c(-max_abs, max_abs),
            oob = scales::squish,
            name = "Return (%)",
            labels = function(x) sprintf("%.1f%%", x),
            guide = guide_colorbar(
                title.position = "top",
                barwidth = 12,
                barheight = 0.5
            )
        )
    } else {
        # ALL POSITIVE RETURNS: Use sequential GREEN scale (light to dark)
        # This is the critical fix - positive values should ALWAYS be green
        color_scale <- scale_fill_gradient(
            low = "#E8F5E9",           # Very light green for lowest positive
            high = "#1B5E20",          # Dark green for highest positive
            limits = c(max(0, min_return - 0.1), max_return + 0.1),
            oob = scales::squish,
            name = "Return (%)",
            labels = function(x) sprintf("%.1f%%", x),
            guide = guide_colorbar(
                title.position = "top",
                barwidth = 12,
                barheight = 0.5
            )
        )
    }

    # Dynamic text color based on return value and background darkness
    # For all-positive (green scale): higher values = darker green = need white text
    # For diverging (red-yellow-green): extreme values on both ends need white text
    if(min_return < 0) {
        # Diverging scale: white text on dark ends (both high positive and low negative)
        text_colors <- case_when(
            heatmap_data$return_value > (max_return * 0.6) ~ "white",
            heatmap_data$return_value < (min_return * 0.6) ~ "white",
            TRUE ~ "grey20"
        )
    } else {
        # All-positive green scale: white text on darker greens (higher values)
        return_range <- max_return - min_return
        threshold <- min_return + (return_range * 0.55)  # Above 55% of range = dark green
        text_colors <- case_when(
            heatmap_data$return_value > threshold ~ "white",
            TRUE ~ "grey20"
        )
    }

    # Check if any projections are truncated (for caption)
    has_truncated <- any(heatmap_data$is_truncated, na.rm = TRUE)

    # Create labels with asterisk for truncated projections
    heatmap_data <- heatmap_data %>%
        mutate(
            label_text = ifelse(is_truncated,
                               sprintf("%.2f%%*", return_value),
                               sprintf("%.2f%%", return_value))
        )

    # Create heatmap with realistic scale
    p <- ggplot(heatmap_data, aes(x = holding_period, y = bond, fill = return_value)) +

        geom_tile(color = "white", linewidth = 1.5) +

        geom_text(aes(label = label_text),
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
            caption = paste0(
                "Returns shown are total period returns (not annualized)",
                if (has_truncated) " | * = Truncated at maturity" else ""
            )
        ) +

        create_insele_theme() +
        theme(
            axis.text = element_text(face = "bold"),
            panel.border = element_rect(fill = NA, color = insele_palette$dark_gray, linewidth = 2),
            legend.position = "bottom"
        )

        return(p)

    }, error = function(e) {
        # Error handler - return informative plot on failure
        warning(sprintf("Carry & Roll heatmap generation error: %s", e$message))
        ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "Error generating Carry & Roll heatmap.\nPlease check data availability.",
                     size = 4, color = "grey50") +
            theme_void() +
            labs(title = "Carry & Roll Analysis", subtitle = "Data processing error")
    })
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
# 13. Forward Curve Plot - Shows spot curve with key forward rate segments
generate_forward_curve_plot <- function(data, params) {
    # Calculate forward rates for visualization
    curve_data <- data %>%
        arrange(modified_duration)

    if(nrow(curve_data) < 2) {
        return(NULL)
    }

    # Create yield curve interpolation function
    yield_curve_func <- approxfun(
        x = curve_data$modified_duration,
        y = curve_data$yield_to_maturity,
        rule = 2
    )

    # Define key forward periods: XyYy = Y-year rate starting X years from now
    forward_periods <- data.frame(
        start_year = c(1, 2, 3, 5, 7),
        tenor_years = c(1, 1, 2, 2, 3),
        label = c("1y1y", "2y1y", "3y2y", "5y2y", "7y3y"),
        stringsAsFactors = FALSE
    )
    forward_periods$end_year <- forward_periods$start_year + forward_periods$tenor_years

    # Calculate forward rates for each period
    forward_data <- data.frame()
    for(i in 1:nrow(forward_periods)) {
        t1 <- forward_periods$start_year[i]
        t2 <- forward_periods$end_year[i]
        tenor <- forward_periods$tenor_years[i]

        # Get interpolated spot rates (in percentage form)
        r_t1_pct <- yield_curve_func(t1)
        r_t2_pct <- yield_curve_func(t2)
        r_tenor_pct <- yield_curve_func(tenor)  # Current spot for same tenor

        # Convert to decimal for calculation
        r_t1 <- r_t1_pct / 100
        r_t2 <- r_t2_pct / 100

        # Calculate forward rate
        forward_rate <- ((1 + r_t2)^t2 / (1 + r_t1)^t1)^(1/tenor) - 1
        forward_rate_pct <- forward_rate * 100

        # Spread in basis points (forward vs current spot for same tenor)
        spread_bps <- (forward_rate_pct - r_tenor_pct) * 100

        forward_data <- rbind(forward_data, data.frame(
            start_year = t1,
            end_year = t2,
            tenor_years = tenor,
            forward_rate = forward_rate_pct,
            current_spot = r_tenor_pct,
            spread_bps = spread_bps,
            label = forward_periods$label[i],
            mid_point = (t1 + t2) / 2,
            stringsAsFactors = FALSE
        ))
    }

    if(nrow(forward_data) == 0) {
        return(NULL)
    }

    # Create the plot
    p <- ggplot() +

        # Spot curve (main line)
        geom_line(data = curve_data,
                  aes(x = modified_duration, y = yield_to_maturity),
                  color = insele_palette$primary,
                  size = 1.5,
                  alpha = 0.9) +
        geom_point(data = curve_data,
                   aes(x = modified_duration, y = yield_to_maturity),
                   color = insele_palette$primary,
                   size = 3) +

        # Forward rate segments (horizontal lines showing the forward period)
        geom_segment(data = forward_data,
                     aes(x = start_year, xend = end_year,
                         y = forward_rate, yend = forward_rate,
                         color = spread_bps),
                     size = 3,
                     lineend = "round",
                     alpha = 0.85) +

        # Labels for forward rates
        geom_label(data = forward_data,
                   aes(x = mid_point, y = forward_rate,
                       label = sprintf("%s\n%.1f%%", label, forward_rate)),
                   size = 2.8,
                   fontface = "bold",
                   fill = "white",
                   alpha = 0.9,
                   label.padding = unit(0.15, "lines"),
                   label.size = 0.3) +

        # Color scale for spread (diverging: green = bullish, red = bearish)
        scale_color_gradient2(
            low = "#1B5E20",    # Bullish (negative spread - rates falling)
            mid = "#FFC107",    # Neutral
            high = "#C62828",   # Bearish (positive spread - rates rising)
            midpoint = 0,
            name = "Spread\n(bps)",
            limits = c(min(-50, min(forward_data$spread_bps)),
                      max(50, max(forward_data$spread_bps)))
        ) +

        scale_y_continuous(
            labels = function(x) paste0(x, "%"),
            breaks = pretty_breaks(n = 8)
        ) +

        scale_x_continuous(
            breaks = pretty_breaks(n = 10)
        ) +

        # Add legend annotation for spot curve
        annotate("text",
                 x = min(curve_data$modified_duration) + 0.5,
                 y = max(curve_data$yield_to_maturity) + 0.5,
                 label = "Spot Curve",
                 color = insele_palette$primary,
                 fontface = "bold",
                 hjust = 0,
                 size = 3.5) +

        labs(
            title = "Spot Curve vs Implied Forward Rates",
            subtitle = "Segments show forward rate periods | Color indicates spread vs current spot",
            x = "Tenor (years)",
            y = "Rate (%)",
            caption = "Forward notation: XyYy = Y-year rate starting X years from now | Green = bullish, Red = bearish"
        ) +

        create_insele_theme() +
        theme(
            legend.position = "right",
            plot.caption = element_text(size = 9, color = "#666666")
        )

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

    # Validate spread_ts data
    if (is.null(bf$spread_ts) || nrow(bf$spread_ts) == 0) {
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = "No historical data available for this spread",
                         size = 5, color = "grey50") +
                theme_void()
        )
    }

    # Check for minimum data requirements (at least 5 unique dates for meaningful chart)
    unique_dates <- length(unique(as.Date(bf$spread_ts$date)))
    if (unique_dates < 5) {
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = sprintf("Insufficient data points (%d unique dates).\nNeed at least 5 for a meaningful chart.", unique_dates),
                         size = 4.5, color = "grey50", lineheight = 1.2) +
                theme_void()
        )
    }

    # Check if yields were in percentage form (spread already in % points)
    yields_in_pct <- isTRUE(bf$yields_in_pct)

    # If yields are in % form, spread is already in percentage points - don't multiply by 100
    # If yields are in decimal form, multiply by 100 to convert to percentage points
    conversion_factor <- if (yields_in_pct) 1 else 100

    spread_ts <- bf$spread_ts %>%
        mutate(
            date = as.Date(date),
            butterfly_pct = butterfly_spread * conversion_factor
        )

    mean_pct <- bf$mean * conversion_factor
    sd_pct <- bf$sd * conversion_factor
    current_pct <- bf$current * conversion_factor
    current_date <- max(spread_ts$date, na.rm = TRUE)

    # Determine y-axis limits
    y_min <- min(spread_ts$butterfly_pct, mean_pct - 2.5 * sd_pct, na.rm = TRUE)
    y_max <- max(spread_ts$butterfly_pct, mean_pct + 2.5 * sd_pct, na.rm = TRUE)

    # Determine appropriate date breaks based on actual data range
    date_range <- as.numeric(diff(range(spread_ts$date, na.rm = TRUE)))
    n_unique_dates <- length(unique(spread_ts$date))

    # Handle edge case of very narrow date range or few unique dates
    # to ensure proper x-axis label display
    date_breaks <- dplyr::case_when(
        date_range <= 14 | n_unique_dates <= 7 ~ "1 week",
        date_range <= 30 ~ "1 week",
        date_range <= 90 ~ "2 weeks",
        date_range <= 180 ~ "1 month",
        date_range <= 365 ~ "2 months",
        date_range <= 730 ~ "3 months",
        TRUE ~ "6 months"
    )

    date_format <- dplyr::case_when(
        date_range <= 30 ~ "%d %b",
        date_range <= 90 ~ "%d %b",
        date_range <= 365 ~ "%b %Y",
        TRUE ~ "%b %Y"
    )

    p <- ggplot(spread_ts, aes(x = date, y = butterfly_pct)) +

        # ±2 Std Dev band (outer) - slightly more visible
        geom_ribbon(
            aes(ymin = mean_pct - 2 * sd_pct, ymax = mean_pct + 2 * sd_pct),
            fill = "#1B3A6B",
            alpha = 0.08
        ) +

        # ±1 Std Dev band (inner)
        geom_ribbon(
            aes(ymin = mean_pct - 1 * sd_pct, ymax = mean_pct + 1 * sd_pct),
            fill = "#1B3A6B",
            alpha = 0.12
        ) +

        # Mean line
        geom_hline(yintercept = mean_pct, linetype = "dashed", color = "#1B3A6B", linewidth = 0.8) +

        # ±1 Std Dev lines
        geom_hline(yintercept = mean_pct + sd_pct, linetype = "dotted", color = "#1B3A6B", alpha = 0.4, linewidth = 0.5) +
        geom_hline(yintercept = mean_pct - sd_pct, linetype = "dotted", color = "#1B3A6B", alpha = 0.4, linewidth = 0.5) +

        # ±2 Std Dev lines
        geom_hline(yintercept = mean_pct + 2 * sd_pct, linetype = "dotted", color = "#1B3A6B", alpha = 0.6, linewidth = 0.5) +
        geom_hline(yintercept = mean_pct - 2 * sd_pct, linetype = "dotted", color = "#1B3A6B", alpha = 0.6, linewidth = 0.5) +

        # Spread line
        geom_line(color = "#2196F3", linewidth = 0.8) +

        # Current point - double circle marker for visibility
        geom_point(
            data = tail(spread_ts, 1),
            aes(x = date, y = butterfly_pct),
            color = "#E53935",
            size = 4,
            shape = 16
        ) +
        geom_point(
            data = tail(spread_ts, 1),
            aes(x = date, y = butterfly_pct),
            color = "#E53935",
            size = 6,
            shape = 1,
            stroke = 1.5
        ) +

        # Current value annotation (positioned to the right with space)
        annotate(
            "text",
            x = current_date,
            y = current_pct,
            label = sprintf("%.3f%%", current_pct),
            hjust = -0.15,
            vjust = 0.5,
            size = 3.5,
            fontface = "bold",
            color = "#E53935"
        ) +

        # X-axis with proper dynamic date formatting
        # Use explicit limits to ensure full date range is always shown
        scale_x_date(
            limits = c(min(spread_ts$date), max(spread_ts$date)),
            date_breaks = date_breaks,
            date_labels = date_format,
            expand = expansion(mult = c(0.02, 0.12))  # Extra space on right for annotation
        ) +

        # Y-axis formatting
        scale_y_continuous(
            labels = function(x) sprintf("%.2f%%", x),
            expand = expansion(mult = c(0.1, 0.1))
        ) +

        labs(
            title = sprintf("Butterfly Spread: %s (Z-Score: %.2f)", bf$name, bf$z_score),
            subtitle = sprintf("Mean: %.3f%% | Current: %.3f%% | Diff: %+.3f%%",
                               mean_pct, current_pct, (bf$diff_from_mean * conversion_factor)),
            x = NULL,
            y = "Butterfly Spread (%)",
            caption = "Dashed = Mean | Dotted = \u00b11 and \u00b12 sigma | Shaded bands show standard deviation zones"
        ) +

        create_insele_theme() +
        theme(
            plot.title = element_text(face = "bold", size = 13, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 10, color = "grey40"),
            plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
            panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
            axis.text.y = element_text(size = 9),
            axis.title.y = element_text(size = 10)
        )

    return(p)
}