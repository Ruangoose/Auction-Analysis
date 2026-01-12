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