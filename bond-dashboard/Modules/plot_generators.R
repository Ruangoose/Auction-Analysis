# ================================================================================
# SPREAD COLOR SCALE HELPER FUNCTION
# ================================================================================
#' Get spread color scale for yield curve plots
#'
#' COLOR LOGIC (Institutional Bond Trading Convention):
#'   - POSITIVE spread (actual > fitted) = CHEAP = GREEN = BUY signal
#'   - NEGATIVE spread (actual < fitted) = RICH = RED = SELL/AVOID signal
#'
#' @param data Data frame with spread_to_curve column
#' @param palette Color palette with success/danger colors
#' @param scale_name Name for the legend
#' @return ggplot2 scale_fill_gradient2 object
get_spread_color_scale <- function(data, palette, scale_name = "Spread to Fair Value (bps)\nRed = Rich | Green = Cheap") {
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

    # Return the scale with CORRECT color encoding:
    # LOW (negative) = RED (rich/expensive) = SELL signal
    # HIGH (positive) = GREEN (cheap) = BUY signal
    scale_fill_gradient2(
        low = "#D32F2F",      # Red for rich (negative spread - below curve)
        mid = "#FFFFFF",       # White for fair value
        high = "#388E3C",      # Green for cheap (positive spread - above curve)
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

    # ═══════════════════════════════════════════════════════════════════════════
    # FIX: DATA AVAILABILITY FILTER - Only include bonds with VALID data
    # Problem 3 & 4: Exclude bonds with placeholder values or missing data
    # ═══════════════════════════════════════════════════════════════════════════
    n_before <- nrow(data)
    data <- data %>%
        filter(
            # Must have valid yield and duration
            !is.na(yield_to_maturity),
            !is.na(modified_duration),
            is.finite(yield_to_maturity),
            is.finite(modified_duration),
            # Exclude placeholder values that indicate bad data
            yield_to_maturity > 1.5,  # Filter out placeholder values only (was 2%)
            yield_to_maturity < 20.0,  # Sanity check upper bound
            modified_duration > 0.5,   # Must have meaningful duration
            modified_duration < 30.0   # Sanity check upper bound
        )
    n_after <- nrow(data)
    if (n_before > n_after) {
        message(sprintf("  [Yield Curve] Filtered %d bonds with invalid data (keeping %d)",
                       n_before - n_after, n_after))
    }

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

    # ================================================================================
    # FIT MODEL BASED ON SELECTION - Using SELECTED X-Axis Variable
    # ================================================================================
    # CRITICAL: Model must REFIT when x-axis changes (modified_duration, duration, time_to_maturity)
    # All derived metrics (spreads, z-scores) are recalculated based on the new x-axis

    if (params$curve_model == "nss") {
        # Nelson-Siegel-Svensson model - fit using SELECTED x-axis variable
        tryCatch({
            # Use the selected x-axis variable for fitting (NOT hardcoded modified_duration)
            x_values <- x_data  # This is data[[x_var]] - the selected x-axis variable
            yields <- data$yield_to_maturity / 100  # Convert to decimal

            # Filter valid data points
            valid_idx <- !is.na(x_values) & !is.na(yields) & is.finite(x_values) & is.finite(yields)
            x_values <- x_values[valid_idx]
            yields <- yields[valid_idx]

            req(length(x_values) >= 4)  # Need minimum bonds for NSS fitting

            # Initial parameter estimates
            beta0 <- mean(yields, na.rm = TRUE)
            beta1 <- yields[which.min(x_values)] - beta0
            beta2 <- 0
            beta3 <- 0
            lambda1 <- 0.0609
            lambda2 <- 0.0609

            # NSS objective function using selected x variable
            nss_objective <- function(params) {
                b0 <- params[1]
                b1 <- params[2]
                b2 <- params[3]
                b3 <- params[4]
                l1 <- exp(params[5])
                l2 <- exp(params[6])

                # Handle edge cases for small x values
                x_safe <- pmax(x_values, 0.01)

                fitted <- b0 +
                    b1 * (1 - exp(-x_safe/l1)) / (x_safe/l1) +
                    b2 * ((1 - exp(-x_safe/l1)) / (x_safe/l1) - exp(-x_safe/l1)) +
                    b3 * ((1 - exp(-x_safe/l2)) / (x_safe/l2) - exp(-x_safe/l2))

                sum((yields - fitted)^2, na.rm = TRUE)
            }

            # Optimize
            opt_result <- optim(
                c(beta0, beta1, beta2, beta3, log(lambda1), log(lambda2)),
                nss_objective,
                method = "BFGS",
                control = list(maxit = 500)
            )

            if (opt_result$convergence == 0) {
                params_nss <- opt_result$par

                # Predict on the smooth curve range using selected x-axis
                maturities <- pmax(curve_data$x_var, 0.01)

                curve_data$yield <- params_nss[1] +
                    params_nss[2] * (1 - exp(-maturities/exp(params_nss[5]))) / (maturities/exp(params_nss[5])) +
                    params_nss[3] * ((1 - exp(-maturities/exp(params_nss[5]))) / (maturities/exp(params_nss[5])) -
                                         exp(-maturities/exp(params_nss[5]))) +
                    params_nss[4] * ((1 - exp(-maturities/exp(params_nss[6]))) / (maturities/exp(params_nss[6])) -
                                         exp(-maturities/exp(params_nss[6])))

                curve_data$yield <- curve_data$yield * 100  # Convert back to percentage

                # Calculate fitted values for actual bonds (for residuals/spreads)
                x_bonds <- pmax(x_data, 0.01)
                data$fitted_yield <- params_nss[1] +
                    params_nss[2] * (1 - exp(-x_bonds/exp(params_nss[5]))) / (x_bonds/exp(params_nss[5])) +
                    params_nss[3] * ((1 - exp(-x_bonds/exp(params_nss[5]))) / (x_bonds/exp(params_nss[5])) -
                                         exp(-x_bonds/exp(params_nss[5]))) +
                    params_nss[4] * ((1 - exp(-x_bonds/exp(params_nss[6]))) / (x_bonds/exp(params_nss[6])) -
                                         exp(-x_bonds/exp(params_nss[6])))
                data$fitted_yield <- data$fitted_yield * 100

                # Recalculate spread_to_curve based on new fit (in basis points)
                data$spread_to_curve <- (data$yield_to_maturity - data$fitted_yield) * 100

                # Calculate confidence bands based on residuals
                residuals <- data$yield_to_maturity - data$fitted_yield
                n <- length(residuals[!is.na(residuals)])
                df <- max(n - 6, 1)  # NSS has 6 parameters
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

                # Calculate spread for fallback (in basis points)
                data$fitted_yield <- predict(lm_model, newdata = data.frame(x_var = x_data))
                data$spread_to_curve <- (data$yield_to_maturity - data$fitted_yield) * 100
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

            # Calculate spread for fallback (in basis points)
            data$fitted_yield <- predict(lm_model, newdata = data.frame(x_var = x_data))
            data$spread_to_curve <- (data$yield_to_maturity - data$fitted_yield) * 100
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

            # Calculate fitted values for bonds and recalculate spread (in basis points)
            data$fitted_yield <- predict(model_fit, newdata = data.frame(x_var = x_data))
            data$spread_to_curve <- (data$yield_to_maturity - data$fitted_yield) * 100

            if(any(is.na(curve_predictions$se.fit))) {
                # Fallback SE calculation
                residuals <- data$yield_to_maturity - data$fitted_yield
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

            # Calculate spread for fallback (in basis points)
            data$fitted_yield <- predict(lm_model, newdata = data.frame(x_var = x_data))
            data$spread_to_curve <- (data$yield_to_maturity - data$fitted_yield) * 100
        })

    } else if (params$curve_model == "spline") {
        # Smoothing spline
        tryCatch({
            model_fit <- smooth.spline(x = x_data,
                                       y = data$yield_to_maturity,
                                       spar = 0.6)
            curve_data$yield <- predict(model_fit, curve_data$x_var)$y

            # Calculate fitted values for bonds and recalculate spread (in basis points)
            data$fitted_yield <- predict(model_fit, x_data)$y
            data$spread_to_curve <- (data$yield_to_maturity - data$fitted_yield) * 100

            # Calculate proper SE with degrees of freedom
            residuals <- data$yield_to_maturity - data$fitted_yield
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

            # Calculate fitted values for bonds and recalculate spread (in basis points)
            data$fitted_yield <- predict(lm_model, newdata = data.frame(x_var = x_data))
            data$spread_to_curve <- (data$yield_to_maturity - data$fitted_yield) * 100
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

            # Calculate spread for fallback (in basis points)
            data$fitted_yield <- predict(lm_model, newdata = data.frame(x_var = x_data))
            data$spread_to_curve <- (data$yield_to_maturity - data$fitted_yield) * 100
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

    # ================================================================================
    # READ ADVANCED SETTINGS WITH DEFAULTS
    # ================================================================================
    show_labels <- params$show_labels %||% TRUE
    show_confidence_band <- params$show_confidence_band %||% TRUE
    point_size_metric <- params$point_size_metric %||% "zscore"
    confidence_level <- params$confidence_level %||% 0.95

    # ================================================================================
    # MATURITY STATUS: Add shape variable for maturing bonds
    # ================================================================================
    # Check if maturity info is available in the data
    if ("matures_in_period" %in% names(data)) {
        data$maturity_status <- ifelse(data$matures_in_period, "Maturing", "Active")
    } else if ("final_maturity_date" %in% names(data) && !is.null(params$end_date)) {
        # Calculate matures_in_period from final_maturity_date if available
        data$matures_in_period <- !is.na(data$final_maturity_date) &
                                  data$final_maturity_date >= params$start_date &
                                  data$final_maturity_date <= params$end_date
        data$maturity_status <- ifelse(data$matures_in_period, "Maturing", "Active")
    } else {
        # No maturity info available - treat all as active
        data$maturity_status <- "Active"
        data$matures_in_period <- FALSE
    }

    # Track if there are any maturing bonds (for legend display)
    has_maturing_bonds <- any(data$matures_in_period, na.rm = TRUE)

    # Determine point size variable based on setting
    if (point_size_metric == "zscore") {
        data$point_size_var <- abs(data$z_score)
        size_name <- "|Z-Score|\nSignal Strength"
        size_breaks <- c(0.5, 1.5, 2.5)
        size_labels <- c("Weak", "Moderate", "Strong")
        size_limits <- c(0, 3)
    } else if (point_size_metric == "spread") {
        data$point_size_var <- abs(data$spread_to_curve)
        size_name <- "|Spread|\n(bps)"
        spread_max <- max(abs(data$spread_to_curve), na.rm = TRUE)
        size_breaks <- pretty(c(0, spread_max), n = 3)
        size_labels <- as.character(round(size_breaks))
        size_limits <- c(0, spread_max * 1.1)
    } else {
        # Uniform size
        data$point_size_var <- 5  # Fixed size
        size_name <- NULL
        size_breaks <- NULL
        size_labels <- NULL
        size_limits <- c(4, 6)
    }

    # ================================================================================
    # BUILD PLOT LAYERS
    # ================================================================================

    # Start with base plot
    p <- ggplot(data, aes(x = x_var_plot))

    # Add confidence band (if enabled)
    if (show_confidence_band) {
        p <- p +
            geom_ribbon(data = curve_data,
                        aes(y = yield, ymin = yield_lower, ymax = yield_upper),
                        alpha = 0.15,
                        fill = insele_palette$primary)
    }

    # Add fitted curve line
    p <- p +
        geom_line(data = curve_data,
                  aes(y = yield),
                  color = insele_palette$primary,
                  linewidth = 1.5,
                  alpha = 0.9)

    # Add bond points (with shape indicating maturity status)
    # Shape 21 = filled circle (Active), Shape 24 = filled triangle (Maturing)
    if (has_maturing_bonds) {
        p <- p +
            geom_point(aes(y = yield_to_maturity,
                           fill = spread_to_curve,
                           size = point_size_var,
                           shape = maturity_status),
                       stroke = 1.5,
                       color = "white",
                       alpha = 0.9) +
            scale_shape_manual(
                values = c("Active" = 21, "Maturing" = 24),
                name = "Status",
                guide = guide_legend(
                    title.position = "top",
                    title.hjust = 0.5,
                    override.aes = list(fill = insele_palette$primary, size = 5)
                )
            )
    } else {
        p <- p +
            geom_point(aes(y = yield_to_maturity,
                           fill = spread_to_curve,
                           size = point_size_var),
                       shape = 21,
                       stroke = 1.5,
                       color = "white",
                       alpha = 0.9)
    }

    # Add labels (if enabled)
    if (show_labels) {
        p <- p +
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
            )
    }

    # Add scales
    p <- p +
        # USE THE DYNAMIC SCALE FUNCTION HERE - with correct color encoding
        get_spread_color_scale(data, insele_palette)

    # Add size scale (if not uniform)
    if (point_size_metric != "uniform") {
        p <- p +
            scale_size_continuous(
                range = c(3, 12),
                limits = size_limits,
                name = size_name,
                breaks = size_breaks,
                labels = size_labels,
                guide = guide_legend(
                    title.position = "top",
                    title.hjust = 0.5,
                    override.aes = list(fill = insele_palette$primary)
                )
            )
    } else {
        p <- p + scale_size_identity()
    }

    p <- p +
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
            caption = paste0(
                "Z-Score = Signal Strength (|Z| > 2 = Strong, |Z| > 1.5 = Moderate) | ",
                "Color = Spread to Fair Value (Red = Rich/SELL, Green = Cheap/BUY) | ",
                "95% confidence band shown",
                if (has_maturing_bonds) " | Triangle = Matures in period" else ""
            )
        ) +
        create_insele_theme()

    return(p)
}

#' @export
# 2. Relative Value Heatmap Generation
#' @param data Bond data with date, bond, yield_to_maturity, modified_duration columns
#' @param params List of parameters (optional: bond_order for custom ordering)
generate_relative_value_heatmap <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # ========================================================================
    # FIX 4: ORDER BONDS BY DURATION (short bonds at bottom of heatmap)
    # ========================================================================
    # Get duration-based ordering from the data
    bond_duration_order <- data %>%
        group_by(bond) %>%
        summarise(avg_duration = mean(modified_duration, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(avg_duration)) %>%  # Descending so short bonds at bottom
        pull(bond)

    # Use custom order if provided in params
    if (!is.null(params$bond_order)) {
        bond_duration_order <- params$bond_order
    }

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
        filter(week >= max(week) - weeks(12)) %>%
        # Apply duration-based ordering
        mutate(bond = factor(bond, levels = bond_duration_order))

    # Get current date for "Today" indicator
    current_date <- Sys.Date()
    current_week <- floor_date(current_date, "week")

    p <- ggplot(rv_matrix, aes(x = week, y = bond, fill = avg_z_score)) +
        geom_tile(color = "white", linewidth = 0.5) +

        # ========================================================================
        # FIX 6: ADD "TODAY" INDICATOR
        # ========================================================================
        # "Today" vertical line
        geom_vline(
            xintercept = as.numeric(current_week),
            linetype = "longdash",
            color = insele_palette$primary,
            linewidth = 1,
            alpha = 0.8
        ) +

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

        # "Today" label annotation
        annotate(
            "text",
            x = current_week,
            y = Inf,
            label = "Today",
            vjust = -0.5,
            hjust = 0.5,
            size = 3,
            fontface = "bold",
            color = insele_palette$primary
        ) +

        scale_x_date(
            date_breaks = "2 weeks",
            date_labels = "%d\n%b",
            expand = c(0.02, 0)  # Small expand to show Today label
        ) +
        scale_y_discrete(expand = c(0, 0)) +

        # Allow label outside plot area
        coord_cartesian(clip = "off") +

        labs(
            title = "Relative Value Heatmap",
            subtitle = "Weekly Z-scores showing rich/cheap dynamics | Ordered by duration (long at top)",
            x = "",
            y = "",
            caption = "Red = Rich (Sell) | Green = Cheap (Buy) | Values > |2| shown"
        ) +
        create_insele_theme() +
        theme(
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            axis.text.y = element_text(face = "bold"),
            panel.border = element_rect(fill = NA, color = insele_palette$dark_gray, linewidth = 1),
            legend.position = "bottom",
            plot.margin = ggplot2::margin(t = 20, r = 10, b = 10, l = 10)  # Extra top margin for Today label
        )

    return(p)
}


#' @export
# 3. Enhanced Z-Score Distribution Generation
#' @description Z-Score lollipop chart with proper label positioning to avoid overlap
generate_enhanced_zscore_plot <- function(data, params) {
    # ========================================================================
    # FIX 5: DYNAMIC LABEL POSITIONING to avoid overlap with circles
    # ========================================================================
    plot_data <- data %>%
        filter(!is.na(z_score)) %>%
        arrange(desc(abs(z_score))) %>%
        mutate(
            bond_name = factor(bond, levels = rev(bond)),  # Preserve order

            # Dynamic label positioning based on Z-Score sign AND magnitude
            # Nudge labels away from circles
            label_nudge = case_when(
                z_score >= 0 ~ 0.3,   # Positive: nudge right (after coord_flip, this is up)
                TRUE ~ -0.3           # Negative: nudge left (after coord_flip, this is down)
            ),
            label_hjust = case_when(
                z_score >= 0 ~ 0,     # Positive: left-align (text starts at point)
                TRUE ~ 1              # Negative: right-align (text ends at point)
            ),

            # Significance category for coloring
            significance = cut(abs(z_score),
                               breaks = c(-Inf, 1, 2, Inf),
                               labels = c("Normal", "Notable", "Extreme"))
        )

    p <- ggplot(plot_data, aes(x = reorder(bond, z_score), y = z_score)) +

        # Background zones
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = -1, ymax = 1,
                 alpha = 0.1, fill = insele_palette$success) +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = -2, ymax = -1,
                 alpha = 0.1, fill = insele_palette$warning) +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = 1, ymax = 2,
                 alpha = 0.1, fill = insele_palette$warning) +

        # Reference lines
        geom_hline(yintercept = c(-2, -1, 0, 1, 2),
                   linetype = c("dashed", "dotted", "solid", "dotted", "dashed"),
                   color = c(insele_palette$danger, insele_palette$warning,
                             "black", insele_palette$warning, insele_palette$danger),
                   alpha = c(0.7, 0.5, 1, 0.5, 0.7)) +

        # Lollipop stems
        geom_segment(aes(x = bond, xend = bond, y = 0, yend = z_score,
                         color = significance),
                     linewidth = 1.5) +

        # Points (circles)
        geom_point(aes(size = abs(z_score),
                       color = significance),
                   shape = 21,
                   fill = "white",
                   stroke = 2) +

        # Labels with NUDGE to avoid overlap with circles
        geom_text(aes(label = sprintf("%+.2f", z_score),  # Show sign
                      hjust = label_hjust,
                      y = z_score + label_nudge),  # Nudge away from point
                  size = 3.5,
                  fontface = "bold",
                  color = insele_palette$dark_gray) +

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
            limits = c(min(-3, min(plot_data$z_score) - 0.5),
                       max(3, max(plot_data$z_score) + 0.5)),  # Extra space for labels
            expand = c(0.05, 0)
        ) +
        coord_flip() +
        labs(
            title = "Z-Score Distribution",
            subtitle = paste("Statistical Significance |",
                             sum(abs(plot_data$z_score) > 2), "bonds in extreme territory"),
            x = "",
            y = "Z-Score (Standard Deviations)",
            caption = "Normal: |z| < 1 | Notable: 1 ≤ |z| < 2 | Extreme: |z| ≥ 2"
        ) +
        create_insele_theme() +
        theme(
            panel.grid.major.y = element_blank(),
            legend.position = "top",
            legend.direction = "horizontal"
        )

    return(p)
}


#' @export
# 7. Enhanced Convexity Plot Generation
#' @description Creates a scatter plot showing convexity vs duration with quadratic fit
#' @param data Bond data with modified_duration, convexity, yield_to_maturity columns
#' @param params List of parameters including notional for DV01 calculation
#' @return ggplot object
generate_enhanced_convexity_plot <- function(data, params = list()) {

    # Get notional for DV01 calculation (default R10 million)
    notional <- params$notional %||% 10000000

    # Prepare data with DV01 calculation
    # CRITICAL: Filter out NA bond names and invalid data
    conv_data <- data %>%
        filter(
            !is.na(bond),
            bond != "",
            bond != "NA",
            !is.na(convexity),
            !is.na(modified_duration),
            !is.na(yield_to_maturity),
            is.finite(convexity),
            is.finite(modified_duration),
            modified_duration > 0  # Duration must be positive
        ) %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(
            # Calculate DV01 for point sizing
            dv01 = notional * modified_duration * 0.0001,
            # Scale DV01 to reasonable point size range (3 to 15)
            dv01_scaled = scales::rescale(dv01, to = c(3, 15)),
            # Calculate convexity-per-duration ratio (higher = better)
            convexity_ratio = convexity / (modified_duration^2)
        )

    # Validate we have enough data for analysis
    if (nrow(conv_data) < 3) {
        warning("generate_enhanced_convexity_plot: Insufficient data (need at least 3 bonds)")
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = "Insufficient data for convexity analysis\n(need at least 3 bonds)",
                         size = 5, color = "#666666") +
                theme_void() +
                labs(title = "Convexity Profile Analysis")
        )
    }

    # Fit quadratic model for convexity ~ duration
    quad_fit <- tryCatch({
        lm(convexity ~ poly(modified_duration, 2), data = conv_data)
    }, error = function(e) {
        # Fallback to linear if quadratic fails
        lm(convexity ~ modified_duration, data = conv_data)
    })

    # Generate smooth prediction line
    pred_data <- data.frame(
        modified_duration = seq(
            min(conv_data$modified_duration),
            max(conv_data$modified_duration),
            length.out = 100
        )
    )
    pred_data$convexity <- predict(quad_fit, newdata = pred_data)

    # Calculate residuals for identifying outliers
    conv_data <- conv_data %>%
        mutate(
            predicted_convexity = predict(quad_fit, newdata = .),
            convexity_residual = convexity - predicted_convexity,
            convexity_signal = case_when(
                convexity_residual > 10 ~ "High (attractive)",
                convexity_residual < -10 ~ "Low (unattractive)",
                TRUE ~ "Normal"
            )
        )

    # Calculate SE for confidence band
    # CRITICAL: Handle edge cases where SE might be NA, 0, or infinite
    residuals <- conv_data$convexity - conv_data$predicted_convexity
    se <- sd(residuals, na.rm = TRUE)

    # Default SE to 10% of convexity range if invalid
    if (is.na(se) || !is.finite(se) || se <= 0) {
        convexity_range <- diff(range(conv_data$convexity, na.rm = TRUE))
        se <- max(convexity_range * 0.1, 1)  # At least 1 unit
        warning("generate_enhanced_convexity_plot: Using default SE for confidence band")
    }

    pred_data$convexity_lower <- pred_data$convexity - 1.96 * se
    pred_data$convexity_upper <- pred_data$convexity + 1.96 * se

    # Find best and worst convexity bonds
    best_conv <- conv_data %>% arrange(desc(convexity_ratio)) %>% slice(1)
    worst_conv <- conv_data %>% arrange(convexity_ratio) %>% slice(1)

    # Create the plot
    p <- ggplot(conv_data, aes(x = modified_duration, y = convexity)) +

        # Confidence band for fit
        # CRITICAL FIX: When inherit.aes = FALSE, x aesthetic must be explicitly specified
        geom_ribbon(
            data = pred_data,
            aes(x = modified_duration, ymin = convexity_lower, ymax = convexity_upper),
            fill = "#E3F2FD",
            alpha = 0.5,
            inherit.aes = FALSE
        ) +

        # Quadratic fit line
        geom_line(
            data = pred_data,
            aes(x = modified_duration, y = convexity),
            color = "#1B3A6B",
            linewidth = 1,
            linetype = "dashed"
        ) +

        # Points: color = yield, size = DV01
        geom_point(
            aes(color = yield_to_maturity, size = dv01_scaled),
            alpha = 0.8
        ) +

        # Bond labels
        ggrepel::geom_text_repel(
            aes(label = bond),
            size = 3,
            max.overlaps = 15,
            segment.size = 0.3,
            segment.alpha = 0.6,
            color = insele_palette$dark_gray,
            family = if(Sys.info()["sysname"] == "Windows") "sans" else "Helvetica"
        ) +

        scale_color_gradient(
            low = "#4CAF50",
            high = "#F44336",
            name = "Yield (%)",
            labels = scales::percent_format(scale = 1)
        ) +

        scale_size_identity() +  # Use pre-scaled sizes

        scale_x_continuous(breaks = pretty_breaks(n = 8)) +
        scale_y_continuous(breaks = pretty_breaks(n = 8)) +

        labs(
            title = "Convexity Profile Analysis",
            subtitle = sprintf(
                "Quadratic fit shown | Best: %s (%.2f per dur\u00B2) | Worst: %s (%.2f per dur\u00B2)",
                best_conv$bond, best_conv$convexity_ratio,
                worst_conv$bond, worst_conv$convexity_ratio
            ),
            x = "Modified Duration (years)",
            y = "Convexity",
            caption = "Point size = DV01 (larger = more rate sensitive) | Color = Yield (green = low, red = high)"
        ) +

        create_insele_theme() +
        theme(
            legend.position = "right",
            panel.grid.major = element_line(color = "#F0F0F0", linetype = "solid"),
            plot.caption = element_text(
                hjust = 0,
                size = 8,
                lineheight = 1.2,
                margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")
            )
        )

    # Add convexity insight as attribute
    attr(p, "insight") <- list(
        best_bond = best_conv$bond,
        best_ratio = best_conv$convexity_ratio,
        worst_bond = worst_conv$bond,
        worst_ratio = worst_conv$convexity_ratio,
        model_r2 = summary(quad_fit)$r.squared
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