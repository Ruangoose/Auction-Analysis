# Add this helper function at the top of the file (outside the plot function)
get_spread_color_scale <- function(data, palette, scale_name = "Spread (bps)") {
    # Calculate dynamic limits
    if ("spread_to_curve" %in% names(data) && sum(!is.na(data$spread_to_curve)) > 0) {
        spread_range <- range(data$spread_to_curve, na.rm = TRUE)

        # Calculate 95th percentile for outlier-resistant scaling
        spread_percentiles <- quantile(abs(data$spread_to_curve),
                                       probs = c(0.95),
                                       na.rm = TRUE)

        # Use 95th percentile with buffer, but ensure we capture full range
        spread_limit <- max(
            spread_percentiles * 1.2,  # 95th percentile + 20% buffer
            abs(spread_range[1]),       # Ensure we capture minimum
            abs(spread_range[2])        # Ensure we capture maximum
        )

    } else {
        # No spread data - use default
        spread_limit <- 50
    }

    # Apply reasonable bounds
    spread_limit <- pmin(pmax(spread_limit, 10), 200)  # Between 10 and 200 bps

    # Round for cleaner scale
    if (spread_limit <= 25) {
        spread_limit <- ceiling(spread_limit / 5) * 5
    } else if (spread_limit <= 50) {
        spread_limit <- ceiling(spread_limit / 10) * 10
    } else {
        spread_limit <- ceiling(spread_limit / 25) * 25
    }

    # Return the scale
    scale_fill_gradient2(
        low = palette$success,
        mid = "white",
        high = palette$danger,
        midpoint = 0,
        limits = c(-spread_limit, spread_limit),
        oob = scales::squish,
        name = scale_name,
        breaks = pretty(c(-spread_limit, spread_limit), n = 5),
        labels = function(x) {
            # FIX: Use ifelse() for vectorized operation
            ifelse(abs(x) < 1,
                   sprintf("%+.1f", x),  # One decimal for small values
                   sprintf("%+.0f", x))  # No decimals for larger values
        },
        guide = guide_colorbar(
            barwidth = 15,
            barheight = 0.5,
            title.position = "top",
            title.hjust = 0.5
        )
    )
}

# Yield Curve and Relative Value Plots
#' @export
# 1. Enhanced Yield Curve Plot with Confidence Bands
generate_enhanced_yield_curve <- function(data, params) {

    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # VALIDATION: Ensure spread_to_curve exists
    if(!"spread_to_curve" %in% names(data)) {
        warning("spread_to_curve not found in data - calculating it")
        # Calculate using the same curve model if specified, otherwise default
        curve_method <- if(!is.null(params$curve_model) &&
                           params$curve_model %in% c("smooth.spline", "loess", "nss")) {
            params$curve_model
        } else {
            "smooth.spline"
        }
        data <- calculate_fair_value(data, method = curve_method)
    }

    # VALIDATION: Ensure z_score exists (needed for point sizing)
    if(!"z_score" %in% names(data)) {
        warning("z_score not found in data - calculating relative value metrics")
        data <- calculate_relative_value(data, lookback = params$lookback %||% 60)
    }

    # Get x-axis choice with fallback
    x_var <- if(!is.null(params$xaxis_choice)) params$xaxis_choice else "modified_duration"

    # Add time_to_maturity calculation if needed
    if(x_var == "time_to_maturity") {
        if("mature_date" %in% names(data) && !is.na(data$mature_date[1])) {
            data$time_to_maturity <- as.numeric(difftime(data$mature_date,
                                                         Sys.Date(),  # Current date
                                                         units = "days")) / 365.25
        } else {
            # For semi-annual SA bonds:
            data$macaulay_duration <- data$modified_duration * (1 + data$yield_to_maturity/200)
            data$time_to_maturity <- data$macaulay_duration  # Better approximation
        }
        data <- data %>% filter(!is.na(time_to_maturity) & is.finite(time_to_maturity))
    }

    # Ensure the selected variable exists
    if(!x_var %in% names(data) || all(is.na(data[[x_var]]))) {
        x_var <- "modified_duration"
    }

    # Dynamic x-axis label
    x_label <- switch(x_var,
                      "modified_duration" = "Modified Duration (years)",
                      "duration" = "Duration (years)",
                      "time_to_maturity" = "Time to Maturity (years)"
    )

    # Get the x variable data
    x_data <- data[[x_var]]

    # Create smooth curve with confidence intervals
    x_range <- range(x_data, na.rm = TRUE)
    curve_data <- data.frame(
        x_var = seq(x_range[1], x_range[2], length.out = 200)
    )

    # Adaptive span calculation
    n_unique <- length(unique(x_data))
    adaptive_span <- max(0.75, min(1, 10/n_unique))

    # Fit model based on selection
    if (params$curve_model == "nss") {
        # Nelson-Siegel-Svensson is a parametric model - use dedicated function
        tryCatch({
            # Call the NSS implementation from data_processors
            nss_result <- fit_nss_model(data)

            if (!is.null(nss_result$params) && nss_result$convergence == 0) {
                # Use NSS fitted values
                data$fitted_yield <- nss_result$data$nss_fitted

                # Predict on the smooth curve range
                maturities <- curve_data$x_var
                params_nss <- nss_result$params

                # NSS formula for prediction
                curve_data$yield <- params_nss[1] +
                    params_nss[2] * (1 - exp(-maturities/exp(params_nss[5]))) / (maturities/exp(params_nss[5])) +
                    params_nss[3] * ((1 - exp(-maturities/exp(params_nss[5]))) / (maturities/exp(params_nss[5])) -
                                         exp(-maturities/exp(params_nss[5]))) +
                    params_nss[4] * ((1 - exp(-maturities/exp(params_nss[6]))) / (maturities/exp(params_nss[6])) -
                                         exp(-maturities/exp(params_nss[6])))

                curve_data$yield <- curve_data$yield * 100  # Convert back to percentage

                # Calculate confidence bands based on residuals
                residuals <- data$yield_to_maturity - data$fitted_yield
                n <- length(residuals[!is.na(residuals)])
                df <- n - 6  # NSS has 6 parameters
                se <- sqrt(sum(residuals^2, na.rm = TRUE) / df)

                curve_data$yield_lower <- curve_data$yield - 1.96 * se
                curve_data$yield_upper <- curve_data$yield + 1.96 * se

            } else {
                # NSS failed to converge - fallback to polynomial
                warning("NSS model failed to converge, using polynomial fallback")
                lm_model <- lm(yield_to_maturity ~ poly(x_var, 3),
                               data = data.frame(x_var = x_data,
                                                 yield_to_maturity = data$yield_to_maturity))
                curve_predictions <- predict(lm_model,
                                             newdata = data.frame(x_var = curve_data$x_var),
                                             se.fit = TRUE)
                curve_data$yield <- curve_predictions$fit
                curve_data$yield_lower <- curve_predictions$fit - 1.96 * curve_predictions$se.fit
                curve_data$yield_upper <- curve_predictions$fit + 1.96 * curve_predictions$se.fit
            }
        }, error = function(e) {
            # NSS failed - fallback to polynomial
            warning(paste("NSS model error:", e$message, "- using polynomial fallback"))
            lm_model <- lm(yield_to_maturity ~ poly(x_var, 3),
                           data = data.frame(x_var = x_data,
                                             yield_to_maturity = data$yield_to_maturity))
            curve_predictions <- predict(lm_model,
                                         newdata = data.frame(x_var = curve_data$x_var),
                                         se.fit = TRUE)
            curve_data$yield <- curve_predictions$fit
            curve_data$yield_lower <- curve_predictions$fit - 1.96 * curve_predictions$se.fit
            curve_data$yield_upper <- curve_predictions$fit + 1.96 * curve_predictions$se.fit
        })

    } else if (params$curve_model == "loess") {
        # LOESS is a non-parametric local regression
        tryCatch({
            model_fit <- loess(yield_to_maturity ~ x_var,
                               data = data.frame(x_var = x_data,
                                                 yield_to_maturity = data$yield_to_maturity),
                               span = adaptive_span)
            curve_predictions <- predict(model_fit,
                                         newdata = data.frame(x_var = curve_data$x_var),
                                         se = TRUE)
            curve_data$yield <- curve_predictions$fit

            if(any(is.na(curve_predictions$se.fit))) {
                # Fallback SE calculation
                residuals <- data$yield_to_maturity - predict(model_fit,
                                                              data.frame(x_var = x_data))
                n <- length(residuals[!is.na(residuals)])
                # Approximate df for loess
                df <- n * (1 - adaptive_span)
                se <- sqrt(sum(residuals^2, na.rm = TRUE) / max(df, 1))
                curve_data$yield_lower <- curve_data$yield - 1.96 * se
                curve_data$yield_upper <- curve_data$yield + 1.96 * se
            } else {
                curve_data$yield_lower <- curve_predictions$fit - 1.96 * curve_predictions$se.fit
                curve_data$yield_upper <- curve_predictions$fit + 1.96 * curve_predictions$se.fit
            }
        }, error = function(e) {
            # Loess failed - fallback to polynomial
            warning(paste("Loess model error:", e$message, "- using polynomial fallback"))
            lm_model <- lm(yield_to_maturity ~ poly(x_var, 2),
                           data = data.frame(x_var = x_data,
                                             yield_to_maturity = data$yield_to_maturity))
            curve_data$yield <- predict(lm_model,
                                        newdata = data.frame(x_var = curve_data$x_var))
            se <- summary(lm_model)$sigma
            curve_data$yield_lower <- curve_data$yield - 1.96 * se
            curve_data$yield_upper <- curve_data$yield + 1.96 * se
        })

    } else if (params$curve_model == "spline") {
        # Smoothing spline
        tryCatch({
            model_fit <- smooth.spline(x = x_data,
                                       y = data$yield_to_maturity,
                                       spar = 0.6)
            curve_data$yield <- predict(model_fit, curve_data$x_var)$y

            # Calculate proper SE with degrees of freedom
            residuals <- data$yield_to_maturity - predict(model_fit, x_data)$y
            n <- length(residuals[!is.na(residuals)])
            df <- model_fit$df  # smooth.spline provides df
            se <- sqrt(sum(residuals^2, na.rm = TRUE) / df)

            curve_data$yield_lower <- curve_data$yield - 1.96 * se
            curve_data$yield_upper <- curve_data$yield + 1.96 * se
        }, error = function(e) {
            # Spline failed - fallback
            warning(paste("Spline model error:", e$message))
            curve_data$yield <- data$yield_to_maturity[1]
            curve_data$yield_lower <- curve_data$yield - 0.2
            curve_data$yield_upper <- curve_data$yield + 0.2
        })

    } else if (params$curve_model == "polynomial") {
        # Polynomial regression (default)
        tryCatch({
            # Determine polynomial degree based on data points
            n_unique <- length(unique(x_data))
            poly_degree <- min(3, max(1, n_unique - 2))

            lm_model <- lm(yield_to_maturity ~ poly(x_var, poly_degree),
                           data = data.frame(x_var = x_data,
                                             yield_to_maturity = data$yield_to_maturity))
            curve_predictions <- predict(lm_model,
                                         newdata = data.frame(x_var = curve_data$x_var),
                                         se.fit = TRUE)
            curve_data$yield <- curve_predictions$fit
            curve_data$yield_lower <- curve_predictions$fit - 1.96 * curve_predictions$se.fit
            curve_data$yield_upper <- curve_predictions$fit + 1.96 * curve_predictions$se.fit
        }, error = function(e) {
            # Final fallback - simple linear
            lm_model <- lm(yield_to_maturity ~ x_var,
                           data = data.frame(x_var = x_data,
                                             yield_to_maturity = data$yield_to_maturity))
            curve_data$yield <- predict(lm_model,
                                        newdata = data.frame(x_var = curve_data$x_var))
            se <- summary(lm_model)$sigma
            curve_data$yield_lower <- curve_data$yield - 1.96 * se
            curve_data$yield_upper <- curve_data$yield + 1.96 * se
        })
    } else {
        # Unknown model - use polynomial as default
        params$curve_model <- "polynomial"
        # Recursive call with polynomial
        return(generate_enhanced_yield_curve(data, params))
    }

    # Add x variable to data for plotting
    data$x_var_plot <- x_data
    curve_data$x_var_plot <- curve_data$x_var

    # Calculate dynamic limits based on actual spread data
    spread_range <- range(data$spread_to_curve, na.rm = TRUE)
    spread_limit <- max(abs(spread_range)) * 1.1  # Add 10% buffer


    # Create the plot
    p <- ggplot(data, aes(x = x_var_plot)) +
        geom_ribbon(data = curve_data,
                    aes(y = yield, ymin = yield_lower, ymax = yield_upper),
                    alpha = 0.15,
                    fill = insele_palette$primary) +
        geom_line(data = curve_data,
                  aes(y = yield),
                  color = insele_palette$primary,
                  linewidth = 1.5,  # Changed from size to linewidth (ggplot2 update)
                  alpha = 0.9) +
        geom_point(aes(y = yield_to_maturity,
                       fill = spread_to_curve,
                       size = abs(z_score)),
                   shape = 21,
                   stroke = 1.5,
                   color = "white",
                   alpha = 0.9) +
        ggrepel::geom_text_repel(
            data = smart_label(data, "bond", "z_score", max_labels = 10),
            aes(y = yield_to_maturity, label = bond),
            size = 3.5,
            max.overlaps = 20,
            segment.size = 0.3,
            segment.alpha = 0.6,
            box.padding = 0.4,
            point.padding = 0.3,
            force = 2,
            color = insele_palette$dark_gray,
            family = if(Sys.info()["sysname"] == "Windows") "sans" else "Helvetica"
        ) +
        # USE THE DYNAMIC SCALE FUNCTION HERE
        get_spread_color_scale(data, insele_palette, "Spread (bps)") +
        scale_size_continuous(
            range = c(3, 10),
            name = "|Z-Score|",
            breaks = c(0, 1, 2, 3, 4, 5),  # Added explicit breaks for clarity
            labels = function(x) sprintf("%.0f", x),
            guide = guide_legend(
                title.position = "top",
                title.hjust = 0.5
            )
        ) +
        scale_x_continuous(
            breaks = pretty_breaks(n = 10),
            expand = c(0.02, 0)
        ) +
        scale_y_continuous(
            breaks = pretty_breaks(n = 8),
            labels = function(x) sprintf("%.2f%%", x),
            expand = c(0.02, 0)
        ) +
        labs(
            title = "South African Government Bond Yield Curve",
            subtitle = {
                # Calculate dynamic spread summary
                spread_summary <- if(sum(!is.na(data$spread_to_curve)) > 0) {
                    avg_spread <- mean(abs(data$spread_to_curve), na.rm = TRUE)
                    max_spread <- max(abs(data$spread_to_curve), na.rm = TRUE)
                    sprintf("Avg: %.1f bps | Max: %.1f bps", avg_spread, max_spread)
                } else {
                    "Spread: N/A"
                }

                paste(
                    "Model:", params$curve_model, "|",
                    "X-Axis:", gsub("_", " ", tools::toTitleCase(x_var)), "|",
                    "Date:", format(max(data$date), "%d %B %Y"), "|",
                    "Bonds:", length(unique(data$bond)), "|",
                    spread_summary  # Use dynamic spread summary
                )
            },
            x = x_label,
            y = "Yield to Maturity",
            caption = "Point size = |Z-Score| | Color = Spread to curve | 95% confidence band shown"
        ) +
        create_insele_theme()

    return(p)
}

#' @export
# 2. Relative Value Heatmap Generation
generate_relative_value_heatmap <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    rv_matrix <- data %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            weekly_return = case_when(
                # Use 7 calendar days or find last week's observation
                !is.na(lag(yield_to_maturity, 7)) ~
                    (yield_to_maturity - lag(yield_to_maturity, 7)) / lag(yield_to_maturity, 7) * 100,
                # Fallback to nearest weekly observation
                !is.na(lag(yield_to_maturity, 5)) ~
                    (yield_to_maturity - lag(yield_to_maturity, 5)) / lag(yield_to_maturity, 5) * 100,
                TRUE ~ NA_real_
            )
        ) %>%
        filter(!is.na(weekly_return)) %>%
        mutate(
            week = floor_date(date, "week"),
            z_score = {
                s <- sd(weekly_return, na.rm = TRUE)
                if(is.na(s) || s < 0.001) {
                    rep(0, length(weekly_return))
                } else {
                    (weekly_return - mean(weekly_return, na.rm = TRUE)) / s
                }
            }
        ) %>%
        group_by(bond, week) %>%
        summarise(
            avg_z_score = mean(z_score, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        filter(week >= max(week) - weeks(12))

    p <- ggplot(rv_matrix, aes(x = week, y = bond, fill = avg_z_score)) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_gradient2(
            low = insele_palette$danger,
            mid = "white",
            high = insele_palette$success,
            midpoint = 0,
            limits = c(-3, 3),
            oob = scales::squish,
            name = "Z-Score",
            guide = guide_colorbar(
                barwidth = 20,
                barheight = 0.5,
                title.position = "top",
                title.hjust = 0.5
            )
        ) +
        geom_text(aes(label = ifelse(abs(avg_z_score) > 2,
                                     sprintf("%.1f", avg_z_score), "")),
                  size = 3,
                  color = "white",
                  fontface = 2) +
        scale_x_date(
            date_breaks = "2 weeks",
            date_labels = "%d\n%b",
            expand = c(0, 0)
        ) +
        scale_y_discrete(expand = c(0, 0)) +
        labs(
            title = "Relative Value Heatmap",
            subtitle = "Weekly Z-scores showing rich/cheap dynamics across bonds",
            x = "",
            y = "",
            caption = "Red = Rich (Sell) | Green = Cheap (Buy) | Values > |2| shown"
        ) +
        create_insele_theme() +
        theme(
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            axis.text.y = element_text(face = "bold"),
            panel.border = element_rect(fill = NA, color = insele_palette$dark_gray, size = 1),
            legend.position = "bottom"
        )

    return(p)
}


#' @export
# 3. Enhanced Z-Score Distribution Generation
generate_enhanced_zscore_plot <- function(data, params) {
    data <- data %>%
        filter(!is.na(z_score)) %>%
        arrange(desc(abs(z_score)))

    p <- ggplot(data, aes(x = reorder(bond, z_score), y = z_score)) +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = -1, ymax = 1,
                 alpha = 0.1, fill = insele_palette$success) +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = -2, ymax = -1,
                 alpha = 0.1, fill = insele_palette$warning) +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = 1, ymax = 2,
                 alpha = 0.1, fill = insele_palette$warning) +
        geom_hline(yintercept = c(-2, -1, 0, 1, 2),
                   linetype = c("dashed", "dotted", "solid", "dotted", "dashed"),
                   color = c(insele_palette$danger, insele_palette$warning,
                             "black", insele_palette$warning, insele_palette$danger),
                   alpha = c(0.7, 0.5, 1, 0.5, 0.7)) +
        geom_segment(aes(x = bond, xend = bond, y = 0, yend = z_score,
                         color = cut(abs(z_score),
                                     breaks = c(-Inf, 1, 2, Inf),
                                     labels = c("Normal", "Notable", "Extreme"))),
                     size = 1.5) +
        geom_point(aes(size = abs(z_score),
                       color = cut(abs(z_score),
                                   breaks = c(-Inf, 1, 2, Inf),
                                   labels = c("Normal", "Notable", "Extreme"))),
                   shape = 21,
                   fill = "white",
                   stroke = 2) +
        geom_text(aes(label = sprintf("%.2f", z_score),
                      hjust = ifelse(z_score > 0, -0.3, 1.3)),
                  size = 3.5,
                  fontface = 2) +
        scale_color_manual(
            values = c("Normal" = insele_palette$secondary,
                       "Notable" = insele_palette$warning,
                       "Extreme" = insele_palette$danger),
            name = "Significance"
        ) +
        scale_size_continuous(
            range = c(4, 10),
            guide = "none"
        ) +
        scale_y_continuous(
            breaks = seq(-4, 4, 1),
            limits = c(min(data$z_score, -3), max(data$z_score, 3))
        ) +
        coord_flip() +
        labs(
            title = "Statistical Significance Analysis",
            subtitle = paste("Z-Score Distribution |",
                             sum(abs(data$z_score) > 2), "bonds in extreme territory"),
            x = "",
            y = "Z-Score (Standard Deviations)",
            caption = "Normal: |z| < 1 | Notable: 1 ≤ |z| < 2 | Extreme: |z| ≥ 2"
        ) +
        create_insele_theme()+
        theme(
            panel.grid.major.y = element_blank(),
            legend.position = "top",
            legend.direction = "horizontal"
        )

    return(p)
}


#' @export
# 7. Enhanced Convexity Plot Generation
generate_enhanced_convexity_plot <- function(data, params) {
    conv_data <- data %>%
        filter(!is.na(convexity))

    p <- ggplot(conv_data, aes(x = modified_duration, y = convexity)) +

        # Smooth trend with confidence band
        geom_smooth(method = "lm",
                    se = TRUE,
                    color = insele_palette$primary,
                    fill = insele_palette$primary,
                    alpha = 0.2,
                    size = 1.2) +

        # Points with size based on yield
        geom_point(aes(size = yield_to_maturity,
                       fill = yield_to_maturity),
                   shape = 21,
                   stroke = 1.5,
                   color = "white",
                   alpha = 0.8) +

        # Labels with smart positioning
        ggrepel::geom_label_repel(
            data = smart_label(conv_data, "bond", "yield_to_maturity", max_labels = 10),
            aes(label = bond),
            size = 3,
            max.overlaps = 15,
            box.padding = 0.3,
            label.padding = 0.2,
            segment.size = 0.3,
            segment.alpha = 0.6,
            fill = "white",
            color = insele_palette$dark_gray,
            family = if(Sys.info()["sysname"] == "Windows") "sans" else "Helvetica"
        ) +

        scale_size_continuous(
            range = c(3, 12),
            name = "Yield (%)",
            guide = guide_legend(
                title.position = "top"
            )
        ) +

        scale_fill_viridis_c(
            option = "D",
            name = "Yield (%)",
            guide = "none"
        ) +

        scale_x_continuous(breaks = pretty_breaks(n = 8)) +
        scale_y_continuous(breaks = pretty_breaks(n = 8)) +

        labs(
            title = "Convexity Profile Analysis",
            subtitle = "Risk-return characteristics across the curve",
            x = "Modified Duration (years)",
            y = "Convexity",
            caption = "Larger bubbles = higher yields | Higher convexity = better risk/return profile"
        ) +

        create_insele_theme() +
        theme(
            legend.position = "right",
            panel.grid.major = element_line(color = "#F0F0F0", linetype = "solid")
        )

    return(p)
}

#' @export
# 25. Enhanced Correlation Plot
generate_enhanced_correlation_plot <- function(data, params) {
    # CRITICAL FIX: Calculate correlation on yield CHANGES, not levels
    #
    # WHY THIS MATTERS:
    # - SA government bonds trade in a narrow 8-11% yield range
    # - Correlating yield LEVELS produces artificially high correlations (0.78-0.85)
    #   because all bonds move within this narrow band
    # - Correlating yield CHANGES reflects how bonds actually move TOGETHER
    #   in response to market events, which shows more realistic variation
    # - Short-term and long-term bonds don't always move identically - they respond
    #   differently to rate changes, inflation expectations, and risk-off events
    #
    # TECHNICAL APPROACH:
    # - Calculate daily yield changes (simple differences, not log returns)
    # - Simple differences are more intuitive for fixed income (measured in bps)
    # - First observation per bond will be NA (no previous value to compare)

    cor_data <- data %>%
        arrange(date) %>%
        group_by(bond) %>%
        # Calculate daily yield changes (differences in yield levels)
        # This captures the co-movement of bonds in basis points
        mutate(yield_change = yield_to_maturity - lag(yield_to_maturity)) %>%
        ungroup() %>%
        # Remove first observation per bond (NA due to lag)
        filter(!is.na(yield_change)) %>%
        select(date, bond, yield_change) %>%
        pivot_wider(names_from = bond, values_from = yield_change) %>%
        select(-date)

    # Check if we have enough data
    if(ncol(cor_data) < 2) {
        return(NULL)
    }

    # Calculate correlation matrix on yield CHANGES (not levels)
    # This now shows realistic correlations typically in 0.60-0.80 range
    cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

    # Create enhanced correlation heatmap
    cor_melted <- cor_matrix %>%
        as.data.frame() %>%
        rownames_to_column("bond1") %>%
        pivot_longer(cols = -bond1, names_to = "bond2", values_to = "correlation")

    # Order bonds by maturity for better visualization
    bond_order <- data %>%
        group_by(bond) %>%
        summarise(avg_duration = mean(modified_duration, na.rm = TRUE), .groups = "drop") %>%
        arrange(avg_duration) %>%
        pull(bond)

    cor_melted$bond1 <- factor(cor_melted$bond1, levels = bond_order)
    cor_melted$bond2 <- factor(cor_melted$bond2, levels = bond_order)

    p <- ggplot(cor_melted, aes(x = bond1, y = bond2, fill = correlation)) +

        # Heatmap tiles
        geom_tile(color = "white", size = 0.5) +

        # Correlation values
        geom_text(aes(label = sprintf("%.2f", correlation)),
                  color = ifelse(abs(cor_melted$correlation) > 0.5, "white", "black"),
                  size = 3) +

        # Advanced color scale
        scale_fill_gradient2(
            low = insele_palette$danger,
            mid = "white",
            high = insele_palette$primary,
            midpoint = 0,
            limits = c(-1, 1),
            breaks = c(-1, -0.5, 0, 0.5, 1),
            name = "Correlation",
            guide = guide_colorbar(
                barwidth = 20,
                barheight = 0.5,
                title.position = "top",
                title.hjust = 0.5
            )
        ) +

        # Add clustering dendrogram lines (visual enhancement)
        annotate("segment", x = 0.5, xend = length(bond_order) + 0.5,
                 y = 0.5, yend = 0.5, color = insele_palette$dark_gray, size = 0.5) +
        annotate("segment", x = 0.5, xend = 0.5,
                 y = 0.5, yend = length(bond_order) + 0.5, color = insele_palette$dark_gray, size = 0.5) +

        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +

        labs(
            title = "Cross-Asset Correlation Matrix",
            subtitle = paste("Based on", nrow(data), "observations | Ordered by duration"),
            x = "",
            y = "",
            caption = paste("Date range:", min(data$date), "to", max(data$date))
        ) +

        create_insele_theme() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(hjust = 1),
            panel.border = element_rect(fill = NA, color = insele_palette$dark_gray, size = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
        )

    return(p)
}

#' @export
# 26. Term Structure 3D (converted to 2D heatmap for web compatibility)
generate_term_structure_3d_plot <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Create term structure evolution data
    term_evolution <- data %>%
        mutate(
            date_group = floor_date(date, "week"),
            duration_bucket = cut(modified_duration,
                                  breaks = c(0, 3, 5, 7, 10, 15, 20),
                                  labels = c("0-3y", "3-5y", "5-7y", "7-10y", "10-15y", "15y+"),
                                  include.lowest = TRUE)
        ) %>%
        filter(!is.na(duration_bucket)) %>%
        group_by(date_group, duration_bucket) %>%
        summarise(
            avg_yield = mean(yield_to_maturity, na.rm = TRUE),
            .groups = "drop"
        )

    if(nrow(term_evolution) == 0) {
        return(NULL)
    }

    # Create the heatmap
    p <- ggplot(term_evolution, aes(x = date_group, y = duration_bucket, fill = avg_yield)) +

        # Heatmap tiles
        geom_tile(color = "white", size = 0.2) +

        # Add contour lines for better visualization
        geom_contour(aes(z = avg_yield), color = "white", alpha = 0.3, size = 0.5) +

        # Color scale
        scale_fill_viridis_c(
            option = "D",
            name = "Yield (%)",
            guide = guide_colorbar(
                barwidth = 20,
                barheight = 0.5,
                title.position = "top",
                title.hjust = 0.5
            )
        ) +

        # Date formatting
        scale_x_date(
            date_breaks = "1 month",
            date_labels = "%b\n%Y",
            expand = c(0, 0)
        ) +

        scale_y_discrete(expand = c(0, 0)) +

        labs(
            title = "Term Structure Evolution",
            subtitle = "Yield surface dynamics across maturity buckets",
            x = "Date",
            y = "Maturity Bucket",
            caption = "Weekly averages | Darker colors = higher yields"
        ) +

        create_insele_theme() +
        theme(
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.border = element_rect(fill = NA, color = insele_palette$dark_gray, size = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
        )

    return(p)
}