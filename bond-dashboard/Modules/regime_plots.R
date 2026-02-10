#' @export
# 21. Regime Analysis Plot
generate_regime_analysis_plot <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    regime_df <- data

    # ══════════════════════════════════════════════════════════════════════════
    # Dynamic date axis formatting based on time range
    # ══════════════════════════════════════════════════════════════════════════
    date_range <- range(regime_df$date, na.rm = TRUE)
    date_span_days <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))

    # Choose date breaks and labels based on time span
    if (date_span_days <= 90) {
        date_breaks <- "1 week"
        date_labels <- "%d %b"
    } else if (date_span_days <= 365) {
        date_breaks <- "1 month"
        date_labels <- "%b '%y"
    } else if (date_span_days <= 730) {
        date_breaks <- "2 months"
        date_labels <- "%b '%y"
    } else if (date_span_days <= 1095) {
        date_breaks <- "3 months"
        date_labels <- "%b '%y"
    } else if (date_span_days <= 1825) {
        date_breaks <- "6 months"
        date_labels <- "%b '%y"
    } else {
        date_breaks <- "1 year"
        date_labels <- "%Y"
    }

    # ══════════════════════════════════════════════════════════════════════════
    # FIXED: Proper dual-axis scaling
    # ══════════════════════════════════════════════════════════════════════════

    # Get volatility range (multiply by 100 since stored as decimal)
    vol_values <- regime_df$vol_20d * 100
    vol_min <- 0
    vol_max <- max(vol_values, na.rm = TRUE) * 1.15  # 15% headroom

    # Fixed stress score display range: always -2.5 to +2.5
    stress_display_min <- -2.5
    stress_display_max <- 2.5
    stress_display_span <- stress_display_max - stress_display_min  # = 5

    # Scaling: map stress range [-2.5, 2.5] to volatility range [0, vol_max]
    # Formula: y_vol = (stress - stress_min) * (vol_range / stress_range)
    # Simplified: y_vol = (stress + 2.5) * (vol_max / 5)
    stress_to_vol_scale <- vol_max / stress_display_span
    stress_to_vol_offset <- stress_display_min  # = -2.5

    # Create regime visualization with fixed date handling
    p <- ggplot(regime_df, aes(x = date)) +

        # Background shading for regimes - use dplyr::lead explicitly
        geom_rect(aes(xmin = date,
                      xmax = dplyr::lead(date, default = max(date, na.rm = TRUE)),
                      ymin = -Inf, ymax = Inf,
                      fill = regime),
                  alpha = 0.25) +

        # Volatility line (vol_20d in decimal form, multiply by 100 for %)
        geom_line(aes(y = vol_20d * 100),
                  color = insele_palette$primary,
                  linewidth = 1.2, na.rm = TRUE) +

        # Stress score line: transform from stress space to volatility y-axis space
        geom_line(aes(y = (stress_score - stress_to_vol_offset) * stress_to_vol_scale),
                  color = insele_palette$danger,
                  linewidth = 1.4,
                  linetype = "dashed", na.rm = TRUE) +

        # Zero-line for stress score (stress=0 maps to specific y value)
        geom_hline(yintercept = (0 - stress_to_vol_offset) * stress_to_vol_scale,
                   linetype = "dotted",
                   color = insele_palette$danger,
                   alpha = 0.5) +

        # Current regime annotation box
        {
            current_row <- regime_df %>% filter(date == max(date, na.rm = TRUE)) %>% slice(1)
            if (nrow(current_row) > 0 && "regime" %in% names(current_row)) {
                current_regime <- as.character(current_row$regime)
                regime_color_map <- c("Stressed" = insele_palette$danger,
                                      "Elevated" = insele_palette$warning,
                                      "Normal" = insele_palette$secondary,
                                      "Calm" = insele_palette$success)
                reg_col <- regime_color_map[current_regime] %||% insele_palette$secondary
                annotate("label",
                         x = max(regime_df$date, na.rm = TRUE),
                         y = vol_max * 0.95,
                         label = paste0("Current: ", current_regime),
                         hjust = 1, vjust = 1,
                         fill = reg_col,
                         color = "white",
                         fontface = "bold",
                         size = 4,
                         label.padding = unit(0.4, "lines"),
                         label.r = unit(0.2, "lines"))
            }
        } +

        scale_fill_manual(
            values = c("Stressed" = insele_palette$danger,
                       "Elevated" = insele_palette$warning,
                       "Normal" = "#90CAF9",
                       "Calm" = insele_palette$success),
            name = "Regime"
        ) +

        scale_x_date(
            date_breaks = date_breaks,
            date_labels = date_labels,
            expand = expansion(mult = c(0.01, 0.01))
        ) +

        scale_y_continuous(
            name = "Yield Volatility (% p.a.)",
            labels = function(x) paste0(sprintf("%.1f", x), "%"),
            limits = c(0, vol_max),
            expand = expansion(mult = c(0, 0.02)),
            sec.axis = sec_axis(
                # Inverse transform: y_vol -> stress
                # stress = y_vol / scale + offset = y_vol / (vol_max/5) + (-2.5)
                trans = ~ . / stress_to_vol_scale + stress_to_vol_offset,
                name = "Stress Score",
                breaks = c(-2, -1, 0, 1, 2),
                labels = c("-2", "-1", "0", "+1", "+2")
            )
        ) +

        labs(
            title = "Market Regime Evolution",
            subtitle = "Volatility dynamics and stress indicators",
            x = NULL,
            caption = "Solid: 20-day volatility | Dashed: Stress score | Dotted: Stress = 0"
        ) +

        create_insele_theme() +
        theme(
            # Legend improvements
            legend.position = "top",
            legend.direction = "horizontal",
            legend.justification = "left",
            legend.key.size = unit(0.8, "lines"),
            legend.text = element_text(size = 9),
            legend.title = element_text(size = 10, face = "bold"),
            legend.spacing.x = unit(0.3, "cm"),
            legend.margin = ggplot2::margin(0, 0, 0, 0),
            legend.box.margin = ggplot2::margin(0, 0, -5, 0),

            # Reduce white space
            plot.margin = ggplot2::margin(5, 15, 5, 10),
            plot.title = element_text(margin = ggplot2::margin(0, 0, 5, 0)),
            plot.subtitle = element_text(margin = ggplot2::margin(0, 0, 5, 0)),
            plot.caption = element_text(size = 8, color = "gray50", hjust = 0,
                                        margin = ggplot2::margin(5, 0, 0, 0)),

            # Secondary axis styling
            axis.title.y.left = element_text(size = 10, color = insele_palette$primary),
            axis.title.y.right = element_text(size = 10, color = insele_palette$danger,
                                              margin = ggplot2::margin(0, 0, 0, 10)),
            axis.text.y.right = element_text(color = insele_palette$danger),

            # X-axis date label styling
            axis.text.x = element_text(size = 8, hjust = 0.5, lineheight = 0.9),

            # Panel adjustments
            panel.grid.minor = element_blank()
        )

    return(p)
}


#' @export
# 23. Regime Probability Gauge Generation (Compact Version)
generate_regime_probability_gauge <- function(data, params) {
    # Calculate transition probabilities
    transitions <- data %>%
        arrange(date) %>%
        mutate(
            next_regime = lead(regime),
            transition = paste(regime, "->", next_regime)
        ) %>%
        filter(!is.na(next_regime))

    current_regime <- data %>%
        filter(date == max(date)) %>%
        pull(regime) %>%
        first()

    # Calculate probabilities for next regime
    probs <- transitions %>%
        filter(regime == current_regime) %>%
        count(next_regime) %>%
        mutate(probability = n / sum(n) * 100) %>%
        arrange(desc(probability))

    if(nrow(probs) == 0) {
        # Fallback if no transitions found
        probs <- data.frame(
            next_regime = c("Normal", "Calm", "Elevated", "Stressed"),
            probability = c(50, 25, 20, 5)
        )
    }

    # Ensure all regimes are present
    all_regimes <- data.frame(next_regime = c("Stressed", "Elevated", "Normal", "Calm"))
    probs <- all_regimes %>%
        left_join(probs, by = "next_regime") %>%
        mutate(probability = ifelse(is.na(probability), 0, probability))

    # Order by probability descending for better visual
    probs <- probs %>%
        arrange(desc(probability)) %>%
        mutate(next_regime = factor(next_regime, levels = rev(next_regime)))

    # Compact horizontal bar chart
    p <- ggplot(probs, aes(x = next_regime, y = probability, fill = next_regime)) +
        geom_col(width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.0f%%", probability)),
                  hjust = -0.2, size = 3, fontface = "bold") +
        scale_fill_manual(
            values = c(
                "Stressed" = insele_palette$danger,
                "Elevated" = insele_palette$warning,
                "Normal" = insele_palette$secondary,
                "Calm" = insele_palette$success
            )
        ) +
        scale_y_continuous(
            limits = c(0, max(probs$probability, na.rm = TRUE) * 1.3),
            expand = c(0, 0)
        ) +
        coord_flip() +
        labs(
            title = NULL,
            subtitle = paste("From", current_regime, "regime"),
            x = NULL,
            y = NULL
        ) +
        theme_minimal() +
        theme(
            plot.subtitle = element_text(size = 9, color = "gray50", hjust = 0,
                                         margin = ggplot2::margin(0, 0, 8, 0)),
            axis.text.y = element_text(size = 9, face = "bold"),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.margin = ggplot2::margin(5, 10, 5, 5)
        )

    return(p)
}