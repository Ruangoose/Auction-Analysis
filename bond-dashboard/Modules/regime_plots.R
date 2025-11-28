#' @export
# 21. Regime Analysis Plot
generate_regime_analysis_plot <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    regime_df <- data

    # Create regime visualization with fixed date handling
    p <- ggplot(regime_df, aes(x = date)) +

        # Background shading for regimes - use dplyr::lead explicitly
        geom_rect(aes(xmin = date,
                      xmax = dplyr::lead(date, default = max(date, na.rm = TRUE)),
                      ymin = -Inf, ymax = Inf,
                      fill = regime),
                  alpha = 0.2) +

        # Volatility line
        geom_line(aes(y = vol_20d * 100),
                  color = insele_palette$primary,
                  size = 1.2, na.rm = TRUE) +

        # Stress score
        geom_line(aes(y = stress_score * 10 + 50),
                  color = insele_palette$danger,
                  size = 1,
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
            date_labels = "%b\n%Y"
        ) +

        scale_y_continuous(
            labels = function(x) paste0(x, "%"),
            sec.axis = sec_axis(
                ~(. - 50) / 10,
                name = "Stress Score",
                breaks = pretty_breaks(n = 5)
            )
        ) +

        labs(
            title = "Market Regime Evolution",
            subtitle = "Volatility dynamics and stress indicators",
            x = "",
            y = "Volatility (%)",
            caption = "Solid line: 20-day volatility | Dashed line: Composite stress score"
        ) +

        create_insele_theme() +
        theme(
            legend.position = "top",
            legend.direction = "horizontal"
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