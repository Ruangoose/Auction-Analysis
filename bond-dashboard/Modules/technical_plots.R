
# ════════════════════════════════════════════════════════════════════════
# HELPER FUNCTION: DATE BREAKS
# ════════════════════════════════════════════════════════════════════════

#' @title Get Appropriate Date Breaks
#' @description Determine optimal date break spacing based on date range
get_date_breaks <- function(date_range) {
    days_diff <- as.numeric(diff(range(date_range, na.rm = TRUE)))

    if(days_diff <= 30) return("1 week")
    if(days_diff <= 90) return("2 weeks")
    if(days_diff <= 180) return("1 month")
    if(days_diff <= 365) return("2 months")
    if(days_diff <= 730) return("3 months")
    return("6 months")
}

# ════════════════════════════════════════════════════════════════════════
# MAIN FUNCTION: ADVANCED TECHNICAL PLOT
# ════════════════════════════════════════════════════════════════════════

#' @export
#' @title Generate Advanced Technical Plot
#' @description Multi-panel technical analysis chart with various indicator types
#' @param data Data frame with technical indicators
#' @param bond_select Bond symbol to plot
#' @param indicator_type Type: "volatility", "mean_reversion", "momentum", "all"
#' @return ggplot/grob object with technical analysis panels
generate_advanced_technical_plot <- function(data, bond_select, indicator_type = "all") {

    # ════════════════════════════════════════════════════════════════════
    # VALIDATION
    # ════════════════════════════════════════════════════════════════════

    # ✅ NEW: VALIDATE FIRST - Before doing ANYTHING
    validation <- validate_bond_data_for_plot(
        data,
        required_bonds = 1,
        required_rows_per_bond = 20,  # Need decent history for technicals
        required_cols = c("bond", "date", "yield_to_maturity", "rsi_14", "bb_mean", "macd")
    )

    if(!validation$valid) {
        warning(sprintf(
            "[generate_advanced_technical_plot] Validation failed: %s",
            validation$message
        ))

        # Return informative empty plot
        plot.new()
        text(0.5, 0.5,
             paste("Cannot generate technical plot:\n", validation$message),
             cex = 1.2, col = insele_palette$danger)
        return(NULL)
    }

    # ✅ Validation passed - log success
    message(sprintf(
        "[generate_advanced_technical_plot] ✓ Validated: %d bonds, %d total rows",
        validation$details$bonds,
        validation$details$total_rows
    ))

    # ════════════════════════════════════════════════════════════════════
    # YOUR EXISTING CODE CONTINUES HERE - NO CHANGES NEEDED BELOW
    # ════════════════════════════════════════════════════════════════════

    # Filter for selected bond
    tech_data <- data %>%
        filter(bond == bond_select) %>%
        arrange(date)

    if(nrow(tech_data) == 0) {
        stop(paste(
            "No data found for bond:", bond_select,
            "\nAvailable bonds:", paste(unique(data$bond), collapse = ", ")
        ))
    }

    if(nrow(tech_data) < 10) {
        warning(paste(
            "Limited data for", bond_select,
            "(", nrow(tech_data), "rows). Chart may be incomplete."
        ))
    }

    # ════════════════════════════════════════════════════════════════════
    # ENSURE TECHNICAL INDICATORS ARE PRESENT
    # ════════════════════════════════════════════════════════════════════

    # Check if technical indicators already calculated
    has_technicals <- all(c("rsi_14", "bb_mean", "macd") %in% names(tech_data))

    if(!has_technicals) {
        message(paste(
            "Technical indicators not found for", bond_select,
            "- calculating now..."
        ))

        tryCatch({
            tech_data <- calculate_advanced_technicals(tech_data)
            tech_data <- validate_dataframe_class(
                tech_data,
                "generate_advanced_technical_plot [after calculate_technicals]"
            )
        }, error = function(e) {
            stop(paste(
                "Failed to calculate technical indicators:",
                e$message,
                "\n→ Ensure data has sufficient history (>20 rows recommended)"
            ))
        })
    }

    # ════════════════════════════════════════════════════════════════════
    # DEFINE REQUIRED COLUMNS BY INDICATOR TYPE
    # ════════════════════════════════════════════════════════════════════

    required_cols <- list(
        volatility = c(
            "bb_lower_2", "bb_upper_2", "bb_lower", "bb_upper",
            "bb_mean", "bb_width", "yield_to_maturity"
        ),
        mean_reversion = c(
            "bb_lower", "bb_upper", "yield_to_maturity",
            "mean_20", "sma_50", "sma_200", "z_score_20"
        ),
        momentum = c(
            "rsi_14", "stoch_rsi", "macd", "macd_signal", "macd_histogram"
        ),
        all = c(
            "bb_lower_2", "bb_upper_2", "bb_lower_1", "bb_upper_1",
            "sma_50", "sma_200", "yield_to_maturity", "rsi_14",
            "stoch_rsi", "macd", "macd_signal", "macd_histogram",
            "resistance_3m", "support_3m"
        )
    )

    # Get required columns for selected indicator type
    cols_needed <- required_cols[[tolower(indicator_type)]] %||% required_cols$all

    # ════════════════════════════════════════════════════════════════════
    # ADD MISSING COLUMNS WITH SAFE DEFAULTS
    # ════════════════════════════════════════════════════════════════════

    missing_cols <- setdiff(cols_needed, names(tech_data))

    if(length(missing_cols) > 0) {
        warning(paste(
            "Missing technical columns for", bond_select, ":",
            paste(missing_cols, collapse = ", "),
            "\n→ Using default values. Consider running calculate_advanced_technicals() first."
        ))

        # Add missing columns with appropriate defaults
        for(col in missing_cols) {
            tech_data[[col]] <- switch(col,
                                       bb_lower_2 = tech_data$yield_to_maturity - 0.4,
                                       bb_upper_2 = tech_data$yield_to_maturity + 0.4,
                                       bb_lower_1 = tech_data$yield_to_maturity - 0.2,
                                       bb_upper_1 = tech_data$yield_to_maturity + 0.2,
                                       bb_lower = tech_data$yield_to_maturity - 0.2,
                                       bb_upper = tech_data$yield_to_maturity + 0.2,
                                       bb_mean = tech_data$yield_to_maturity,
                                       bb_width = 0.4,
                                       sma_50 = tech_data$yield_to_maturity,
                                       sma_200 = tech_data$yield_to_maturity,
                                       mean_20 = tech_data$yield_to_maturity,
                                       rsi_14 = 50,
                                       stoch_rsi = 0.5,
                                       macd = 0,
                                       macd_signal = 0,
                                       macd_histogram = 0,
                                       z_score_20 = 0,
                                       resistance_3m = max(tech_data$yield_to_maturity, na.rm = TRUE),
                                       support_3m = min(tech_data$yield_to_maturity, na.rm = TRUE),
                                       0  # fallback default
            )
        }
    }

    # ════════════════════════════════════════════════════════════════════
    # GET DATE BREAKS FOR X-AXIS
    # ════════════════════════════════════════════════════════════════════

    date_breaks <- get_date_breaks(tech_data$date)

    # ════════════════════════════════════════════════════════════════════
    # CREATE PLOTS BASED ON INDICATOR TYPE
    # ════════════════════════════════════════════════════════════════════

    plot_result <- tryCatch({

        if(tolower(indicator_type) == "volatility") {

            # ──────────────────────────────────────────────────────────────
            # VOLATILITY-SPECIFIC PLOTS
            # ──────────────────────────────────────────────────────────────

            p1 <- ggplot(tech_data, aes(x = date)) +
                # Bollinger Bands shading
                geom_ribbon(
                    aes(ymin = bb_lower_2, ymax = bb_upper_2),
                    alpha = 0.1,
                    fill = insele_palette$danger
                ) +
                geom_ribbon(
                    aes(ymin = bb_lower, ymax = bb_upper),
                    alpha = 0.2,
                    fill = insele_palette$secondary
                ) +
                # Actual yield
                geom_line(
                    aes(y = yield_to_maturity),
                    color = insele_palette$primary,
                    linewidth = 1.2
                ) +
                # BB mean
                geom_line(
                    aes(y = bb_mean),
                    color = insele_palette$primary,
                    linetype = "dashed",
                    linewidth = 0.8,
                    alpha = 0.7
                ) +
                scale_x_date(
                    date_breaks = date_breaks,
                    date_labels = "%b\n%Y"
                ) +
                scale_y_continuous(
                    labels = function(x) paste0(x, "%")
                ) +
                labs(
                    title = paste(bond_select, "- Bollinger Bands (Volatility)"),
                    subtitle = "Wider bands = higher volatility | 2σ shaded regions",
                    y = "Yield (%)",
                    x = ""
                ) +
                create_insele_theme()

            p2 <- ggplot(tech_data, aes(x = date)) +
                geom_line(
                    aes(y = bb_width * 100),
                    color = insele_palette$danger,
                    linewidth = 1.2
                ) +
                geom_hline(
                    yintercept = mean(tech_data$bb_width * 100, na.rm = TRUE),
                    linetype = "dashed",
                    color = insele_palette$primary,
                    alpha = 0.7
                ) +
                annotate(
                    "text",
                    x = min(tech_data$date, na.rm = TRUE),
                    y = mean(tech_data$bb_width * 100, na.rm = TRUE),
                    label = sprintf("Avg: %.2f%%", mean(tech_data$bb_width * 100, na.rm = TRUE)),
                    hjust = 0,
                    vjust = -0.5,
                    color = insele_palette$primary,
                    size = 3
                ) +
                scale_x_date(
                    date_breaks = date_breaks,
                    date_labels = "%b"
                ) +
                scale_y_continuous(
                    labels = function(x) paste0(x, "%")
                ) +
                labs(
                    title = "Bollinger Band Width",
                    y = "Width (%)",
                    x = "Date"
                ) +
                create_insele_theme()

            # Combine plots
            p <- gridExtra::arrangeGrob(p1, p2, ncol = 1, heights = c(2, 1))

        } else if(tolower(indicator_type) == "mean_reversion") {

            # ──────────────────────────────────────────────────────────────
            # MEAN REVERSION-SPECIFIC PLOTS
            # ──────────────────────────────────────────────────────────────

            p1 <- ggplot(tech_data, aes(x = date)) +
                # Bollinger Band shading
                geom_ribbon(
                    aes(ymin = bb_lower, ymax = bb_upper),
                    alpha = 0.1,
                    fill = insele_palette$secondary
                ) +
                # Actual yield
                geom_line(
                    aes(y = yield_to_maturity),
                    color = insele_palette$primary,
                    linewidth = 1.2
                ) +
                # Moving averages
                geom_line(
                    aes(y = mean_20, color = "MA 20"),
                    linewidth = 0.8
                ) +
                geom_line(
                    aes(y = sma_50, color = "MA 50"),
                    linewidth = 0.8,
                    na.rm = TRUE
                ) +
                geom_line(
                    aes(y = sma_200, color = "MA 200"),
                    linewidth = 0.8,
                    linetype = "dashed",
                    na.rm = TRUE
                ) +
                scale_color_manual(
                    values = c(
                        "MA 20" = insele_palette$accent,
                        "MA 50" = insele_palette$secondary,
                        "MA 200" = insele_palette$primary_dark
                    ),
                    name = "Moving Averages"
                ) +
                scale_x_date(
                    date_breaks = date_breaks,
                    date_labels = "%b\n%Y"
                ) +
                scale_y_continuous(
                    labels = function(x) paste0(x, "%")
                ) +
                labs(
                    title = paste(bond_select, "- Mean Reversion Analysis"),
                    subtitle = "Yield vs Moving Averages | Bollinger Bands (1σ)",
                    y = "Yield (%)",
                    x = ""
                ) +
                create_insele_theme() +
                theme(legend.position = "top")

            p2 <- ggplot(tech_data, aes(x = date)) +
                # Normal range shading
                geom_ribbon(
                    aes(ymin = -2, ymax = 2),
                    alpha = 0.1,
                    fill = insele_palette$secondary
                ) +
                # Reference lines
                geom_hline(
                    yintercept = c(-2, 0, 2),
                    linetype = c("dashed", "solid", "dashed"),
                    color = c(insele_palette$success, "black", insele_palette$danger),
                    alpha = 0.7
                ) +
                # Z-score line
                geom_line(
                    aes(y = z_score_20),
                    color = insele_palette$primary,
                    linewidth = 1.2
                ) +
                # Annotations
                annotate(
                    "text",
                    x = max(tech_data$date, na.rm = TRUE),
                    y = 2,
                    label = "Overbought",
                    hjust = 1,
                    vjust = -0.5,
                    color = insele_palette$danger,
                    size = 3,
                    fontface = "italic"
                ) +
                annotate(
                    "text",
                    x = max(tech_data$date, na.rm = TRUE),
                    y = -2,
                    label = "Oversold",
                    hjust = 1,
                    vjust = 1.5,
                    color = insele_palette$success,
                    size = 3,
                    fontface = "italic"
                ) +
                scale_x_date(
                    date_breaks = date_breaks,
                    date_labels = "%b"
                ) +
                scale_y_continuous(breaks = seq(-3, 3, 1)) +
                labs(
                    title = "Z-Score from 20-day Mean",
                    subtitle = "Standard deviations from mean | ±2σ = extreme",
                    y = "Z-Score",
                    x = "Date"
                ) +
                create_insele_theme()

            # Combine plots
            p <- gridExtra::arrangeGrob(p1, p2, ncol = 1, heights = c(2, 1))

        } else if(tolower(indicator_type) == "momentum") {

            # ──────────────────────────────────────────────────────────────
            # MOMENTUM-SPECIFIC PLOTS
            # ──────────────────────────────────────────────────────────────

            p2 <- ggplot(tech_data, aes(x = date)) +
                # Neutral zone shading
                geom_ribbon(
                    aes(ymin = 30, ymax = 70),
                    alpha = 0.1,
                    fill = insele_palette$secondary
                ) +
                # RSI line
                geom_line(
                    aes(y = rsi_14),
                    color = insele_palette$primary,
                    linewidth = 1,
                    na.rm = TRUE
                ) +
                # Reference lines
                geom_hline(
                    yintercept = c(30, 50, 70),
                    linetype = c("dashed", "solid", "dashed"),
                    color = c(insele_palette$success, "black", insele_palette$danger),
                    alpha = 0.7
                ) +
                # Stochastic RSI
                geom_line(
                    aes(y = stoch_rsi * 100),
                    color = insele_palette$accent,
                    linewidth = 0.8,
                    alpha = 0.7,
                    na.rm = TRUE
                ) +
                # Annotations
                annotate(
                    "text",
                    x = max(tech_data$date, na.rm = TRUE),
                    y = 70,
                    label = "Overbought",
                    hjust = 1,
                    vjust = -0.5,
                    color = insele_palette$danger,
                    size = 3
                ) +
                annotate(
                    "text",
                    x = max(tech_data$date, na.rm = TRUE),
                    y = 30,
                    label = "Oversold",
                    hjust = 1,
                    vjust = 1.5,
                    color = insele_palette$success,
                    size = 3
                ) +
                scale_x_date(
                    date_breaks = date_breaks,
                    date_labels = "%b"
                ) +
                scale_y_continuous(
                    limits = c(0, 100),
                    breaks = c(0, 30, 50, 70, 100)
                ) +
                labs(
                    title = paste(bond_select, "- Momentum Indicators"),
                    subtitle = "RSI (solid) & Stochastic RSI (faded)",
                    y = "RSI",
                    x = ""
                ) +
                create_insele_theme()

            p3 <- ggplot(tech_data, aes(x = date)) +
                # MACD histogram
                geom_col(
                    aes(y = macd_histogram,
                        fill = macd_histogram > 0),
                    alpha = 0.7
                ) +
                # MACD line
                geom_line(
                    aes(y = macd),
                    color = insele_palette$primary,
                    linewidth = 1,
                    na.rm = TRUE
                ) +
                # Signal line
                geom_line(
                    aes(y = macd_signal),
                    color = insele_palette$accent,
                    linewidth = 0.8,
                    linetype = "dashed",
                    na.rm = TRUE
                ) +
                # Zero line
                geom_hline(
                    yintercept = 0,
                    color = "black",
                    linewidth = 0.5
                ) +
                scale_fill_manual(
                    values = c(
                        "FALSE" = insele_palette$danger,
                        "TRUE" = insele_palette$success
                    ),
                    guide = "none"
                ) +
                scale_x_date(
                    date_breaks = date_breaks,
                    date_labels = "%b"
                ) +
                labs(
                    title = "MACD (Moving Average Convergence Divergence)",
                    subtitle = "Histogram + MACD line (solid) + Signal line (dashed)",
                    y = "MACD",
                    x = "Date"
                ) +
                create_insele_theme()

            # Combine plots
            p <- gridExtra::arrangeGrob(p2, p3, ncol = 1, heights = c(1, 1))

        } else {

            # ──────────────────────────────────────────────────────────────
            # ALL INDICATORS (Comprehensive View)
            # ──────────────────────────────────────────────────────────────

            p1 <- ggplot(tech_data, aes(x = date)) +
                # Bollinger Bands
                geom_ribbon(
                    aes(ymin = bb_lower_2, ymax = bb_upper_2),
                    alpha = 0.1,
                    fill = insele_palette$secondary
                ) +
                geom_ribbon(
                    aes(ymin = bb_lower_1, ymax = bb_upper_1),
                    alpha = 0.2,
                    fill = insele_palette$secondary
                ) +
                # Support/Resistance
                geom_line(
                    aes(y = resistance_3m),
                    color = insele_palette$danger,
                    linetype = "dotted",
                    alpha = 0.7,
                    na.rm = TRUE
                ) +
                geom_line(
                    aes(y = support_3m),
                    color = insele_palette$success,
                    linetype = "dotted",
                    alpha = 0.7,
                    na.rm = TRUE
                ) +
                # Moving averages
                geom_line(
                    aes(y = sma_50, color = "SMA 50"),
                    linewidth = 0.8,
                    alpha = 0.8,
                    na.rm = TRUE
                ) +
                geom_line(
                    aes(y = sma_200, color = "SMA 200"),
                    linewidth = 1,
                    alpha = 0.8,
                    linetype = "dashed",
                    na.rm = TRUE
                ) +
                # Actual yield
                geom_line(
                    aes(y = yield_to_maturity),
                    color = insele_palette$primary,
                    linewidth = 1.2,
                    na.rm = TRUE
                ) +
                scale_color_manual(
                    values = c(
                        "SMA 50" = insele_palette$accent,
                        "SMA 200" = insele_palette$primary_dark
                    ),
                    name = ""
                ) +
                scale_x_date(
                    date_breaks = date_breaks,
                    date_labels = "%b\n%Y"
                ) +
                scale_y_continuous(
                    labels = function(x) paste0(x, "%")
                ) +
                labs(
                    title = paste(bond_select, "- Comprehensive Technical Analysis"),
                    subtitle = "Yield with BBands (1σ & 2σ), MAs, Support/Resistance",
                    y = "Yield (%)",
                    x = ""
                ) +
                create_insele_theme() +
                theme(legend.position = "top")

            # RSI panel (same as momentum)
            p2 <- ggplot(tech_data, aes(x = date)) +
                geom_ribbon(
                    aes(ymin = 30, ymax = 70),
                    alpha = 0.1,
                    fill = insele_palette$secondary
                ) +
                geom_line(
                    aes(y = rsi_14),
                    color = insele_palette$primary,
                    linewidth = 1,
                    na.rm = TRUE
                ) +
                geom_hline(
                    yintercept = c(30, 50, 70),
                    linetype = c("dashed", "solid", "dashed"),
                    color = c(insele_palette$success, "black", insele_palette$danger),
                    alpha = 0.7
                ) +
                geom_line(
                    aes(y = stoch_rsi * 100),
                    color = insele_palette$accent,
                    linewidth = 0.8,
                    alpha = 0.7,
                    na.rm = TRUE
                ) +
                scale_x_date(
                    date_breaks = date_breaks,
                    date_labels = "%b"
                ) +
                scale_y_continuous(
                    limits = c(0, 100),
                    breaks = c(0, 30, 50, 70, 100)
                ) +
                labs(
                    title = "Momentum: RSI & Stochastic RSI",
                    y = "RSI",
                    x = ""
                ) +
                create_insele_theme()

            # MACD panel (same as momentum)
            p3 <- ggplot(tech_data, aes(x = date)) +
                geom_col(
                    aes(y = macd_histogram,
                        fill = macd_histogram > 0),
                    alpha = 0.7
                ) +
                geom_line(
                    aes(y = macd),
                    color = insele_palette$primary,
                    linewidth = 1,
                    na.rm = TRUE
                ) +
                geom_line(
                    aes(y = macd_signal),
                    color = insele_palette$accent,
                    linewidth = 0.8,
                    linetype = "dashed",
                    na.rm = TRUE
                ) +
                geom_hline(
                    yintercept = 0,
                    color = "black",
                    linewidth = 0.5
                ) +
                scale_fill_manual(
                    values = c(
                        "FALSE" = insele_palette$danger,
                        "TRUE" = insele_palette$success
                    ),
                    guide = "none"
                ) +
                scale_x_date(
                    date_breaks = date_breaks,
                    date_labels = "%b"
                ) +
                labs(
                    title = "MACD",
                    y = "MACD",
                    x = "Date"
                ) +
                create_insele_theme()

            # Combine all three plots
            p <- gridExtra::arrangeGrob(
                p1, p2, p3,
                ncol = 1,
                heights = c(3, 1.5, 1.5)
            )
        }

        return(p)

    }, error = function(e) {

        # ════════════════════════════════════════════════════════════════
        # ERROR HANDLING - RETURN INFORMATIVE ERROR PLOT
        # ════════════════════════════════════════════════════════════════

        error_msg <- paste(
            "Error generating technical plot:",
            e$message,
            "\n\nBond:", bond_select,
            "\nIndicator Type:", indicator_type,
            "\nData rows:", nrow(tech_data)
        )

        warning(error_msg)

        # Return error plot
        error_plot <- ggplot() +
            annotate(
                "text",
                x = 0.5,
                y = 0.6,
                label = "Technical Plot Generation Failed",
                color = insele_palette$danger,
                size = 6,
                fontface = "bold"
            ) +
            annotate(
                "text",
                x = 0.5,
                y = 0.4,
                label = paste(
                    "Error:", e$message,
                    "\nBond:", bond_select,
                    "\nTry selecting a different date range or bond"
                ),
                color = insele_palette$dark_gray,
                size = 4,
                lineheight = 1.2
            ) +
            xlim(0, 1) +
            ylim(0, 1) +
            theme_void() +
            theme(
                plot.background = element_rect(
                    fill = insele_palette$gradient_start,
                    color = insele_palette$danger,
                    linewidth = 2
                )
            )

        return(error_plot)
    })

    return(plot_result)
}

#' @export
# 9. Signal Matrix Heatmap Generation
generate_signal_matrix_heatmap <- function(data) {

    # ✅ NEW: VALIDATE FIRST
    validation <- validate_bond_data_for_plot(
        data,
        required_bonds = 2,  # Need multiple bonds for matrix
        required_rows_per_bond = 1,
        required_cols = c("bond", "signal_strength", "rsi_14", "bb_position")
    )

    if(!validation$valid) {
        warning(sprintf(
            "[generate_signal_matrix_heatmap] Validation failed: %s",
            validation$message
        ))
        return(NULL)
    }

    message(sprintf(
        "[generate_signal_matrix_heatmap] ✓ Validated: %d bonds for matrix",
        validation$details$bonds
    ))

    # ════════════════════════════════════════════════════════════════════
    # YOUR EXISTING CODE CONTINUES HERE
    # ════════════════════════════════════════════════════════════════════

    # Calculate technical indicators on full historical data
    signal_matrix_full <- data %>%
        group_by(bond) %>%
        arrange(date) %>%
        ungroup()

    # Calculate technical indicators if not present
    required_cols <- c("rsi_14", "bb_position", "macd", "macd_signal", "roc_20")
    if (!all(required_cols %in% names(signal_matrix_full))) {
        signal_matrix_full <- calculate_advanced_technicals(signal_matrix_full)
    }

    # Get latest values for each bond
    signal_matrix <- signal_matrix_full %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup()

    # ════════════════════════════════════════════════════════════════════════
    # PHASE 1: CROSS-SECTIONAL PERCENTILE RANKING
    # ════════════════════════════════════════════════════════════════════════
    # Calculate where each bond ranks RELATIVE TO OTHER BONDS today
    # This prevents uniform signals in trending markets

    signal_matrix <- signal_matrix %>%
        mutate(
            # Cross-sectional rankings (0-1 scale, where 0 = lowest, 1 = highest)
            rsi_rank_cross = percent_rank(rsi_14),
            macd_rank_cross = percent_rank(macd_normalized),
            bb_rank_cross = percent_rank(bb_position),
            momentum_rank_cross = percent_rank(roc_20)
        )

    # ════════════════════════════════════════════════════════════════════════
    # PHASE 2: GENERATE SIGNALS (Hybrid: Time-Series + Cross-Sectional)
    # ════════════════════════════════════════════════════════════════════════

    signal_scores <- signal_matrix %>%
        mutate(
            # ═══════════════════════════════════════════════════════════════
            # RSI SIGNAL (Hybrid: 60% historical + 40% relative)
            # ═══════════════════════════════════════════════════════════════

            # Time-series signal (vs own history)
            rsi_signal_timeseries = case_when(
                is.na(rsi_14) ~ 0,
                # Use bond-specific thresholds instead of fixed 30/70
                !is.na(rsi_threshold_buy) & !is.na(rsi_threshold_sell) &
                    rsi_14 < rsi_threshold_buy ~ 2,
                !is.na(rsi_median) & rsi_14 < rsi_median * 0.9 ~ 1,
                !is.na(rsi_threshold_sell) & rsi_14 > rsi_threshold_sell ~ -2,
                !is.na(rsi_median) & rsi_14 > rsi_median * 1.1 ~ -1,
                # Fallback to traditional thresholds if dynamic ones unavailable
                rsi_14 < 30 ~ 2,
                rsi_14 < 40 ~ 1,
                rsi_14 > 70 ~ -2,
                rsi_14 > 60 ~ -1,
                TRUE ~ 0
            ),

            # Cross-sectional signal (vs other bonds)
            rsi_signal_cross = case_when(
                is.na(rsi_rank_cross) ~ 0,
                rsi_rank_cross < 0.15 ~ 2,   # Bottom 15% = relatively oversold
                rsi_rank_cross < 0.35 ~ 1,   # Bottom 35%
                rsi_rank_cross > 0.85 ~ -2,  # Top 15% = relatively overbought
                rsi_rank_cross > 0.65 ~ -1,  # Top 35%
                TRUE ~ 0
            ),

            # Blend: 60% time-series + 40% cross-sectional
            rsi_signal = round(rsi_signal_timeseries * 0.6 + rsi_signal_cross * 0.4),

            # ═══════════════════════════════════════════════════════════════
            # BOLLINGER BAND SIGNAL (Already relative within bands)
            # ═══════════════════════════════════════════════════════════════
            bb_signal_timeseries = case_when(
                is.na(bb_position) ~ 0,
                # Strong signals at band extremes
                bb_position < 0 ~ 2,        # Below lower band = oversold
                bb_position < 0.2 ~ 1,      # Near lower band
                bb_position > 1 ~ -2,       # Above upper band = overbought
                bb_position > 0.8 ~ -1,     # Near upper band
                TRUE ~ 0
            ),

            # Cross-sectional Bollinger comparison
            bb_signal_cross = case_when(
                is.na(bb_rank_cross) ~ 0,
                bb_rank_cross < 0.15 ~ 2,    # Relatively oversold vs peers
                bb_rank_cross < 0.35 ~ 1,
                bb_rank_cross > 0.85 ~ -2,   # Relatively overbought vs peers
                bb_rank_cross > 0.65 ~ -1,
                TRUE ~ 0
            ),

            # Blend: 70% time-series + 30% cross-sectional (BB already relative)
            bb_signal = round(bb_signal_timeseries * 0.7 + bb_signal_cross * 0.3),

            # ═══════════════════════════════════════════════════════════════
            # MACD SIGNAL (Hybrid: normalized + relative)
            # ═══════════════════════════════════════════════════════════════
            macd_signal_timeseries = case_when(
                is.na(macd) | is.na(macd_signal) ~ 0,
                # Use normalized MACD if available for better differentiation
                !is.na(macd_normalized) & macd_normalized > 1.5 ~ 2,
                !is.na(macd_normalized) & macd_normalized > 0.5 ~ 1,
                !is.na(macd_normalized) & macd_normalized < -1.5 ~ -2,
                !is.na(macd_normalized) & macd_normalized < -0.5 ~ -1,
                # Fallback to traditional MACD crossover
                !is.na(macd_histogram) & macd > macd_signal & macd_histogram > 0 ~ 2,
                macd > macd_signal ~ 1,
                !is.na(macd_histogram) & macd < macd_signal & macd_histogram < 0 ~ -2,
                macd < macd_signal ~ -1,
                TRUE ~ 0
            ),

            # Cross-sectional MACD
            macd_signal_cross = case_when(
                is.na(macd_rank_cross) ~ 0,
                macd_rank_cross < 0.15 ~ -2,  # Weakest momentum vs peers = sell
                macd_rank_cross < 0.35 ~ -1,
                macd_rank_cross > 0.85 ~ 2,   # Strongest momentum vs peers = buy
                macd_rank_cross > 0.65 ~ 1,
                TRUE ~ 0
            ),

            # Blend: 60% time-series + 40% cross-sectional
            macd_signal_score = round(macd_signal_timeseries * 0.6 + macd_signal_cross * 0.4),

            # ═══════════════════════════════════════════════════════════════
            # MOMENTUM SIGNAL (rate of change with relative comparison)
            # ═══════════════════════════════════════════════════════════════
            momentum_signal_timeseries = case_when(
                is.na(roc_20) ~ 0,
                # Use bond-specific volatility to scale thresholds
                !is.na(bond_volatility) & roc_20 > (5 * bond_volatility / 0.1) ~ 2,
                !is.na(bond_volatility) & roc_20 > (2 * bond_volatility / 0.1) ~ 1,
                !is.na(bond_volatility) & roc_20 < -(5 * bond_volatility / 0.1) ~ -2,
                !is.na(bond_volatility) & roc_20 < -(2 * bond_volatility / 0.1) ~ -1,
                # Fallback to fixed thresholds
                roc_20 > 5 ~ 2,
                roc_20 > 2 ~ 1,
                roc_20 < -5 ~ -2,
                roc_20 < -2 ~ -1,
                TRUE ~ 0
            ),

            # Cross-sectional momentum
            momentum_signal_cross = case_when(
                is.na(momentum_rank_cross) ~ 0,
                momentum_rank_cross < 0.15 ~ -2,  # Weakest momentum vs peers
                momentum_rank_cross < 0.35 ~ -1,
                momentum_rank_cross > 0.85 ~ 2,   # Strongest momentum vs peers
                momentum_rank_cross > 0.65 ~ 1,
                TRUE ~ 0
            ),

            # Blend: 50% time-series + 50% cross-sectional
            momentum_signal = round(momentum_signal_timeseries * 0.5 + momentum_signal_cross * 0.5),

            # ═══════════════════════════════════════════════════════════════
            # TOTAL SIGNAL & CONFIDENCE SCORING
            # ═══════════════════════════════════════════════════════════════
            total_signal = rsi_signal + bb_signal + macd_signal_score + momentum_signal,

            # Signal confidence (0-100 scale)
            signal_confidence = {
                # Factor 1: Indicator agreement (30 points)
                signal_vector <- c(rsi_signal, bb_signal, macd_signal_score, momentum_signal)
                signal_agreement <- if(all(signal_vector == 0)) {
                    0  # All neutral = no confidence
                } else {
                    non_zero <- signal_vector[signal_vector != 0]
                    if(length(non_zero) > 0) {
                        same_direction <- sum(sign(non_zero) == sign(total_signal))
                        (same_direction / length(non_zero)) * 30
                    } else {
                        0
                    }
                }

                # Factor 2: Signal strength (40 points)
                strength_score <- (abs(total_signal) / 8) * 40

                # Factor 3: Data quality (30 points)
                # Penalize if using default values (vectorized operations)
                quality_score <- 30 -
                    ifelse(is.na(rsi_threshold_buy) | rsi_threshold_buy == 30, 10, 0) -
                    ifelse(is.na(macd_normalized) | macd_normalized == 0, 10, 0) -
                    ifelse(is.na(bond_volatility) | bond_volatility == 0.1, 10, 0)

                # Combine and cap
                pmin(100, pmax(0, signal_agreement + strength_score + quality_score))
            }
        ) %>%
        select(bond, rsi_signal, bb_signal, macd_signal_score, momentum_signal,
               total_signal, signal_confidence,
               duration_bucket, indicator_speed,
               rsi_threshold_buy, rsi_threshold_sell, macd_normalized, bond_volatility)

    # Check for any remaining NAs in signal columns and replace with 0
    signal_cols <- c("rsi_signal", "bb_signal", "macd_signal_score", "momentum_signal", "total_signal")
    signal_scores[signal_cols][is.na(signal_scores[signal_cols])] <- 0

    # ════════════════════════════════════════════════════════════════════════
    # PHASE 3: SIGNAL DIVERSITY DIAGNOSTICS
    # ════════════════════════════════════════════════════════════════════════
    # Check that signals are differentiated (not all bonds showing same signal)

    signal_diversity <- signal_scores %>%
        summarise(
            # For each indicator, how many unique signal values?
            rsi_diversity = n_distinct(rsi_signal),
            macd_diversity = n_distinct(macd_signal_score),
            bb_diversity = n_distinct(bb_signal),
            momentum_diversity = n_distinct(momentum_signal),

            # What % of bonds have the modal (most common) signal?
            rsi_mode_pct = if(n() > 0) {
                max(table(rsi_signal)) / n() * 100
            } else 0,
            macd_mode_pct = if(n() > 0) {
                max(table(macd_signal_score)) / n() * 100
            } else 0,
            bb_mode_pct = if(n() > 0) {
                max(table(bb_signal)) / n() * 100
            } else 0,
            momentum_mode_pct = if(n() > 0) {
                max(table(momentum_signal)) / n() * 100
            } else 0,

            # Total bonds
            n_bonds = n()
        )

    # Log diversity metrics
    message("\n══════════════════════════════════════════════════════════════")
    message("SIGNAL DIVERSITY REPORT")
    message("══════════════════════════════════════════════════════════════")
    message(sprintf("Total bonds: %d", signal_diversity$n_bonds))
    message(sprintf("RSI unique signals: %d (expect 3-5 for diverse signals)",
                    signal_diversity$rsi_diversity))
    message(sprintf("MACD unique signals: %d (expect 3-5 for diverse signals)",
                    signal_diversity$macd_diversity))
    message(sprintf("Bollinger unique signals: %d (expect 3-5 for diverse signals)",
                    signal_diversity$bb_diversity))
    message(sprintf("Momentum unique signals: %d (expect 3-5 for diverse signals)",
                    signal_diversity$momentum_diversity))
    message(sprintf("RSI modal %% (should be <60%%): %.1f%%",
                    signal_diversity$rsi_mode_pct))
    message(sprintf("MACD modal %% (should be <60%%): %.1f%%",
                    signal_diversity$macd_mode_pct))
    message("══════════════════════════════════════════════════════════════")

    # Warning if diversity is too low
    if(signal_diversity$rsi_mode_pct > 70 || signal_diversity$macd_mode_pct > 70) {
        warning(
            "⚠ Low signal diversity detected - ",
            ">70% of bonds showing same signal. ",
            "May indicate extreme market conditions or calculation issues."
        )
    } else if(signal_diversity$rsi_mode_pct > 60 || signal_diversity$macd_mode_pct > 60) {
        message(
            "ℹ Moderate signal clustering detected (60-70% same signal). ",
            "May indicate trending market conditions."
        )
    } else {
        message("✓ Good signal diversity - bonds showing differentiated signals")
    }

    # Pivot for heatmap (only signal columns, not metadata)
    signal_scores_long <- signal_scores %>%
        select(bond, rsi_signal, bb_signal, macd_signal_score, momentum_signal, total_signal) %>%
        pivot_longer(cols = -bond, names_to = "indicator", values_to = "score") %>%
        mutate(
            # Ensure score is numeric and not NA
            score = as.numeric(score),
            score = ifelse(is.na(score), 0, score),
            # Create label that shows 0 explicitly
            score_label = as.character(score)
        )

    # Create heatmap with explicit labels for all values
    p <- ggplot(signal_scores_long, aes(x = indicator, y = bond, fill = score)) +

        geom_tile(color = "white", linewidth = 1) +

        # Use score_label to ensure 0s are displayed
        geom_text(aes(label = score_label),
                  size = 4,
                  fontface = "bold",
                  color = case_when(
                      abs(signal_scores_long$score) > 1.5 ~ "white",
                      abs(signal_scores_long$score) > 0.5 ~ "gray20",
                      TRUE ~ "black"
                  )) +

        scale_fill_gradient2(
            low = insele_palette$danger,
            mid = "#F5F5F5",  # Light gray for neutral
            high = insele_palette$success,
            midpoint = 0,
            limits = c(-2, 2),
            name = "Signal",
            breaks = c(-2, -1, 0, 1, 2),
            labels = c("Strong Sell", "Sell", "Neutral", "Buy", "Strong Buy"),
            guide = guide_colorbar(
                barwidth = 20,
                barheight = 0.5,
                title.position = "top",
                title.hjust = 0.5
            )
        ) +

        scale_x_discrete(
            labels = c(
                "rsi_signal" = "RSI",
                "bb_signal" = "Bollinger",
                "macd_signal_score" = "MACD",
                "momentum_signal" = "Momentum",
                "total_signal" = "TOTAL"
            ),
            expand = c(0, 0)
        ) +

        scale_y_discrete(expand = c(0, 0)) +

        labs(
            title = "Trading Signal Matrix",
            subtitle = "Composite technical indicators across all bonds",
            x = "",
            y = "",
            caption = "+2 = Strong Buy | -2 = Strong Sell | 0 = Neutral"
        ) +

        create_insele_theme() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.border = element_rect(fill = NA, color = insele_palette$dark_gray, size = 1.5),
            legend.position = "bottom"
        )

    return(p)
}


get_plottable_bonds <- function(data, min_observations = 30) {
    if(is.null(data) || nrow(data) == 0) {
        return(character(0))
    }

    # Ensure date column is in Date format
    data <- data %>%
        mutate(date = as.Date(date))

    bond_counts <- data %>%
        group_by(bond) %>%
        summarise(
            n_obs = n(),
            date_range = as.numeric(max(date, na.rm = TRUE) - min(date, na.rm = TRUE)),
            has_yield = sum(!is.na(yield_to_maturity)) > 0,
            .groups = "drop"
        ) %>%
        filter(
            n_obs >= min_observations,
            date_range >= 7,  # At least 7 days
            has_yield,
            !is.na(date_range) & is.finite(date_range)  # Ensure valid date range
        )

    return(bond_counts$bond)
}