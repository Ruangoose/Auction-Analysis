#' @export
# 21. Regime Analysis Plot
generate_regime_analysis_plot <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    regime_df <- data

    # Calculate dynamic y-axis range based on volatility data
    # vol_20d is in decimal form (e.g., 0.015 = 1.5%), multiply by 100 for display
    vol_range <- range(regime_df$vol_20d * 100, na.rm = TRUE)
    y_min <- max(0, floor(vol_range[1] - 1))
    y_max <- ceiling(vol_range[2] + 2)

    # Ensure we have reasonable range for stress score overlay
    # Stress score typically ranges -2 to +2, mapped to y-axis via: y = stress * 10 + offset
    # Choose offset to center stress score 0 at mid-volatility range
    vol_mid <- (y_min + y_max) / 2
    stress_offset <- vol_mid  # Stress score 0 maps to middle of volatility range

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

        # Stress score (transformed to align with volatility scale)
        # Maps stress_score [-2, 2] to [offset-20, offset+20] on y-axis
        geom_line(aes(y = stress_score * 10 + stress_offset),
                  color = insele_palette$danger,
                  linewidth = 1,
                  linetype = "dashed", na.rm = TRUE) +

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
            name = "Volatility (%)",
            labels = function(x) paste0(x, "%"),
            limits = c(y_min, y_max),
            expand = expansion(mult = c(0, 0.02)),
            sec.axis = sec_axis(
                ~ (. - stress_offset) / 10,  # Inverse transform to get stress score
                name = "Stress Score",
                breaks = c(-2, -1, 0, 1, 2),
                labels = c("-2", "-1", "0", "1", "2")
            )
        ) +

        labs(
            title = "Market Regime Evolution",
            subtitle = "Volatility dynamics and stress indicators",
            x = NULL,
            caption = "Solid line: 20-day volatility | Dashed line: Composite stress score"
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
            plot.margin = ggplot2::margin(5, 10, 5, 10),
            plot.title = element_text(margin = ggplot2::margin(0, 0, 5, 0)),
            plot.subtitle = element_text(margin = ggplot2::margin(0, 0, 5, 0)),
            plot.caption = element_text(margin = ggplot2::margin(5, 0, 0, 0)),

            # Secondary axis styling
            axis.title.y.right = element_text(margin = ggplot2::margin(0, 0, 0, 10), color = insele_palette$danger),
            axis.text.y.right = element_text(color = insele_palette$danger),

            # Panel adjustments
            panel.grid.minor = element_blank()
        )

    return(p)
}


#' @export
# 23. Regime Probability Gauge Generation
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
        pull(regime)

    # Calculate probabilities for next regime
    probs <- transitions %>%
        filter(regime == current_regime) %>%
        count(next_regime) %>%
        mutate(probability = n / sum(n) * 100) %>%
        arrange(desc(probability))

    if(nrow(probs) == 0) {
        # Fallback if no transitions found
        probs <- data.frame(
            next_regime = c("Stressed", "Elevated", "Normal", "Calm"),
            probability = c(10, 25, 50, 15)
        )
    }

    # Create probability gauge
    p <- ggplot(probs, aes(x = reorder(next_regime, probability), y = probability)) +
        geom_col(aes(fill = next_regime), width = 0.7) +
        geom_text(aes(label = sprintf("%.0f%%", probability)),
                  vjust = -0.5, size = 4, fontface = 2) +
        scale_fill_manual(
            values = c(
                "Stressed" = insele_palette$danger,
                "Elevated" = insele_palette$warning,
                "Normal" = insele_palette$secondary,
                "Calm" = insele_palette$success
            ),
            guide = "none"
        ) +
        scale_y_continuous(
            limits = c(0, max(probs$probability) * 1.2),
            labels = function(x) paste0(x, "%")
        ) +
        coord_flip() +
        labs(
            title = "Next Regime Probability",
            subtitle = paste("Based on historical transitions from", current_regime),
            x = "",
            y = "Probability"
        ) +
        create_insele_theme() +
        theme(
            plot.title = element_text(size = 12),
            plot.subtitle = element_text(size = 10),
            axis.text = element_text(size = 9)
        )

    return(p)
}