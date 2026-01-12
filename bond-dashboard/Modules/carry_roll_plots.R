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

    # Extract numeric holding period for proper ordering
    frontier_data <- data %>%
        filter(!is.na(holding_period), !is.na(net_return)) %>%
        mutate(
            # Extract numeric days from holding_period (e.g., "30d" -> 30)
            holding_days = as.numeric(gsub("d$", "", holding_period))
        ) %>%
        group_by(holding_period, holding_days) %>%
        summarise(
            avg_return = mean(net_return, na.rm = TRUE),
            # Risk metric: volatility of returns across bonds (cross-sectional dispersion)
            return_volatility = sd(net_return, na.rm = TRUE),
            avg_duration = mean(modified_duration, na.rm = TRUE),
            sharpe = mean(sharpe_estimate, na.rm = TRUE),
            max_return = max(net_return, na.rm = TRUE),
            min_return = min(net_return, na.rm = TRUE),
            n_bonds = n(),
            .groups = "drop"
        ) %>%
        arrange(holding_days) %>%
        mutate(
            # Ensure reasonable Sharpe display values
            sharpe_display = pmax(pmin(sharpe, 3), -3),
            # Efficiency score: return per unit of volatility
            efficiency = avg_return / pmax(return_volatility, 0.1)
        )

    if(nrow(frontier_data) == 0) {
        return(NULL)
    }

    # Find optimal holding period (highest efficiency)
    optimal_period <- frontier_data %>% slice_max(efficiency, n = 1)

    p <- ggplot(frontier_data, aes(x = holding_days, y = avg_return)) +

        # Confidence band showing return dispersion across bonds
        geom_ribbon(
            aes(ymin = avg_return - return_volatility,
                ymax = avg_return + return_volatility),
            alpha = 0.15,
            fill = insele_palette$primary
        ) +

        # Min/max range as lighter ribbon
        geom_ribbon(
            aes(ymin = min_return, ymax = max_return),
            alpha = 0.08,
            fill = insele_palette$accent
        ) +

        # Connecting line
        geom_line(
            color = insele_palette$primary,
            linewidth = 1.5,
            alpha = 0.8
        ) +

        # Points sized by efficiency (Sharpe-like metric)
        geom_point(
            aes(size = abs(efficiency)),
            shape = 21,
            fill = insele_palette$accent,
            color = "white",
            stroke = 2
        ) +

        # Highlight optimal period
        geom_point(
            data = optimal_period,
            aes(x = holding_days, y = avg_return),
            shape = 21,
            size = 12,
            color = "#1B5E20",
            fill = NA,
            stroke = 2
        ) +

        # Labels for each holding period
        geom_text(
            aes(label = holding_period),
            vjust = -2,
            size = 3.5,
            fontface = "bold",
            color = insele_palette$dark_gray
        ) +

        # Return value labels
        geom_text(
            aes(label = sprintf("%.2f%%", avg_return)),
            vjust = 2.5,
            size = 3,
            color = "#666666"
        ) +

        # Sharpe ratio annotations
        geom_text(
            aes(label = sprintf("SR: %.2f", sharpe_display)),
            vjust = 4,
            size = 2.5,
            color = "#999999"
        ) +

        # Optimal period annotation
        annotate(
            "text",
            x = optimal_period$holding_days,
            y = optimal_period$avg_return + (optimal_period$return_volatility * 1.5),
            label = "OPTIMAL",
            color = "#1B5E20",
            fontface = "bold",
            size = 3.5
        ) +

        # X-axis: Holding period in days
        scale_x_continuous(
            breaks = frontier_data$holding_days,
            labels = frontier_data$holding_period,
            limits = c(min(frontier_data$holding_days) - 20,
                       max(frontier_data$holding_days) + 40)
        ) +

        # Y-axis formatting
        scale_y_continuous(
            labels = function(x) paste0(sprintf("%.1f", x), "%")
        ) +

        scale_size_continuous(
            range = c(6, 14),
            guide = "none"  # Hide size legend for cleaner look
        ) +

        labs(
            title = "Optimal Holding Period Analysis",
            subtitle = "Net return by holding period | Bands show cross-bond variation",
            x = "Holding Period",
            y = "Average Net Return (%)",
            caption = sprintf(
                "Optimal period: %s (efficiency: %.2f) | Dark band: ±1 std dev | Light band: min-max range | SR = Sharpe Ratio",
                optimal_period$holding_period, optimal_period$efficiency
            )
        ) +

        create_insele_theme() +
        theme(
            legend.position = "none",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )

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