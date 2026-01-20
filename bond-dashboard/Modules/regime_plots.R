#' @export
# 21. Regime Analysis Plot
generate_regime_analysis_plot <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    regime_df <- data

    # Calculate dynamic y-axis range based on volatility data
    # vol_20d is in decimal form (e.g., 0.015 = 1.5%), multiply by 100 for display
    vol_range <- range(regime_df$vol_20d * 100, na.rm = TRUE)
    y_min <- max(0, floor(vol_range[1] * 0.9))  # Slightly below min
    y_max <- ceiling(vol_range[2] * 1.2)        # Give some headroom

    # ══════════════════════════════════════════════════════════════════════════
    # FIX: Proper scaling for stress score to fit within volatility range
    # Stress score typically ranges from -2 to +2
    # We map this to the volatility y-axis range properly
    # ══════════════════════════════════════════════════════════════════════════

    # Get the actual stress score range
    stress_range <- range(regime_df$stress_score, na.rm = TRUE)
    stress_min <- min(-2, stress_range[1])
    stress_max <- max(2, stress_range[2])
    stress_span <- stress_max - stress_min  # Total span of stress values

    # Scaling factors: map stress_score range to middle portion of volatility range
    # We want stress=0 at the middle of the volatility range
    stress_scale <- (y_max - y_min) / stress_span  # Scale factor
    stress_offset <- (y_min + y_max) / 2           # Center point (where stress=0 maps)

    # Create regime visualization with fixed date handling
    p <- ggplot(regime_df, aes(x = date)) +

        # Background shading for regimes - use dplyr::lead explicitly
        geom_rect(aes(xmin = date,
                      xmax = dplyr::lead(date, default = max(date, na.rm = TRUE)),
                      ymin = -Inf, ymax = Inf,
                      fill = regime),
                  alpha = 0.2) +

        # Volatility line (vol_20d in decimal form, multiply by 100 for %)
        geom_line(aes(y = vol_20d * 100),
                  color = insele_palette$primary,
                  linewidth = 1.2, na.rm = TRUE) +

        # Stress score line (transformed to fit volatility scale)
        # Transform: y = (stress_score * stress_scale) + stress_offset
        geom_line(aes(y = (stress_score * stress_scale) + stress_offset),
                  color = insele_palette$danger,
                  linewidth = 0.9,
                  linetype = "dashed", na.rm = TRUE) +

        # Add zero-line reference for stress score
        geom_hline(yintercept = stress_offset,
                   linetype = "dotted",
                   color = insele_palette$danger,
                   alpha = 0.5) +

        scale_fill_manual(
            values = c("Stressed" = insele_palette$danger,
                       "Elevated" = insele_palette$warning,
                       "Normal" = insele_palette$secondary,
                       "Calm" = insele_palette$success),
            name = "Regime"
        ) +

        scale_x_date(
            date_breaks = "1 month",
            date_labels = "%b\n%Y",
            expand = expansion(mult = c(0.01, 0.01))
        ) +

        scale_y_continuous(
            name = "Yield Volatility (% p.a.)",
            labels = function(x) paste0(sprintf("%.1f", x), "%"),
            limits = c(y_min, y_max),
            expand = expansion(mult = c(0.02, 0.02)),
            sec.axis = sec_axis(
                # Inverse transform: stress = (y - offset) / scale
                trans = ~ (. - stress_offset) / stress_scale,
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