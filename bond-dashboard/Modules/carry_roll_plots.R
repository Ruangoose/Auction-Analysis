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
    # FIX 2: IMPROVED COLOR SCALE FOR LOW VALUE DIFFERENTIATION
    # ═══════════════════════════════════════════════════════════════════════

    # Custom color scale optimized for typical carry returns (0-4%)
    # Clusters more colors in the low range for better differentiation
    return_colors <- c(
        "#FFEBEE",      # 0% - very light red (poor)
        "#FFCDD2",      # 0.25% - light red
        "#FFF9C4",      # 0.5% - yellow (break-even zone)
        "#C8E6C9",      # 1.0% - light green (acceptable)
        "#66BB6A",      # 2.0% - medium green (good)
        "#1B5E20"       # 4%+ - dark green (excellent)
    )

    # Determine dynamic limits based on data
    max_return <- max(heatmap_data$return_value, na.rm = TRUE)
    min_return <- min(heatmap_data$return_value, na.rm = TRUE)

    # Use appropriate scale based on return range
    if(min_return < 0) {
        # Diverging scale for negative returns
        color_scale <- scale_fill_gradient2(
            low = "#C62828",           # Dark red for negative
            mid = "#FFF9C4",           # Yellow at zero
            high = "#1B5E20",          # Dark green for positive
            midpoint = 0,
            limits = c(min(min_return, -0.5), max(max_return, 4)),
            oob = scales::squish,
            name = "Return (%)",
            guide = guide_colorbar(
                title.position = "top",
                barwidth = 10,
                barheight = 0.5
            )
        )
    } else {
        # Sequential scale optimized for typical 0-4% returns
        color_scale <- scale_fill_gradientn(
            colors = return_colors,
            values = scales::rescale(c(0, 0.25, 0.5, 1.0, 2.0, 4.0)),
            limits = c(0, max(4, max_return)),
            oob = scales::squish,
            name = "Return (%)",
            guide = guide_colorbar(
                title.position = "top",
                barwidth = 10,
                barheight = 0.5
            )
        )
    }

    # Dynamic text color based on return value (darker text on light backgrounds)
    text_colors <- ifelse(
        heatmap_data$return_value < 0.5 | heatmap_data$return_value > 2.5,
        "black",
        "black"
    )
    # Override for very high/low values
    text_colors <- ifelse(heatmap_data$return_value > 3, "white", text_colors)
    text_colors <- ifelse(heatmap_data$return_value < -0.5, "white", text_colors)

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

    # Calculate efficient frontier
    frontier_data <- data %>%
        filter(!is.na(holding_period), !is.na(net_return)) %>%
        group_by(holding_period) %>%
        summarise(
            avg_return = mean(net_return, na.rm = TRUE),
            avg_risk = mean(modified_duration, na.rm = TRUE),
            sharpe = mean(sharpe_estimate, na.rm = TRUE),
            max_return = max(net_return, na.rm = TRUE),
            min_return = min(net_return, na.rm = TRUE),
            return_dispersion = sd(net_return, na.rm = TRUE),
            .groups = "drop"
        )

    if(nrow(frontier_data) == 0) {
        return(NULL)
    }

    p <- ggplot(frontier_data, aes(x = avg_risk, y = avg_return)) +

        # Add min/max range as ribbon
        geom_ribbon(aes(ymin = min_return, ymax = max_return),
                    alpha = 0.1, fill = insele_palette$primary) +

        geom_line(color = insele_palette$primary,
                  linewidth = 2,
                  alpha = 0.8) +

        geom_point(aes(size = sharpe),
                   shape = 21,
                   fill = insele_palette$accent,
                   color = "white",
                   stroke = 2) +

        ggrepel::geom_label_repel(
            data = smart_label(frontier_data, "holding_period", "sharpe", max_labels = 4),
            aes(label = holding_period),
            size = 3.5,
            max.overlaps = 10,
            box.padding = 0.5,
            fill = "white",
            color = insele_palette$dark_gray
        ) +

        scale_size_continuous(
            range = c(5, 15),
            name = "Sharpe\nRatio"
        ) +

        scale_y_continuous(
            labels = function(x) paste0(x, "%")
        ) +

        labs(
            title = "Optimal Holding Period Analysis",
            subtitle = "Risk-return tradeoff with confidence bands",
            x = "Average Risk (Modified Duration)",
            y = "Average Net Return",
            caption = "Shaded area shows min/max range | Bubble size = Sharpe ratio"
        ) +

        create_insele_theme() +
        theme(legend.position = "right")

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