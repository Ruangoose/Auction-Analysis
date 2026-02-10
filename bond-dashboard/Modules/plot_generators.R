# ================================================================================
# ENSURE REQUIRED PACKAGES ARE LOADED
# ================================================================================
# ggrepel is required for non-overlapping label positioning
if (!requireNamespace("ggrepel", quietly = TRUE)) {
    message("Installing ggrepel for label positioning...")
    install.packages("ggrepel")
}

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

    # ═══════════════════════════════════════════════════════════════════════════
    # FIX 2: YIELD CURVE DATE SELECTION
    # Use the LATEST date available in the data for a coherent curve snapshot
    # This ensures all bonds shown on the curve are from the same date
    # ═══════════════════════════════════════════════════════════════════════════
    if ("date" %in% names(data) && !all(is.na(data$date))) {
        # Find the latest date in the data
        latest_curve_date <- max(data$date, na.rm = TRUE)

        # Filter to only the latest date
        data <- data %>%
            filter(date == latest_curve_date)

        message(sprintf("  [Yield Curve] Using data from: %s (%d bonds)",
                       format(latest_curve_date, "%Y-%m-%d"), nrow(data)))

        # DEBUG: Log bond list for comparison with Trading Signals
        bonds_in_curve <- sort(unique(data$bond))
        message(sprintf("  [YIELD CURVE] Bond list: %s", paste(bonds_in_curve, collapse = ", ")))
    } else {
        # No date column - use all data (shouldn't happen normally)
        latest_curve_date <- Sys.Date()
        warning("[Yield Curve] No date column found - using all available data")
    }

    # Store the curve date in params for subtitle use
    params$curve_date <- latest_curve_date

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
    # LOGIC (per user requirements):
    # - Bonds maturing AFTER end_date = "Active" (circle marker)
    # - Bonds maturing BETWEEN start_date and end_date = "Maturing" (triangle marker)
    # - Bonds maturing BEFORE start_date should already be excluded by get_active_bonds()
    #
    # Check if maturity info is already calculated in the data (preferred - from filtered_data)
    if ("matures_in_period" %in% names(data)) {
        data$maturity_status <- ifelse(data$matures_in_period, "Maturing", "Active")
    } else if ("final_maturity_date" %in% names(data) && !is.null(params$end_date)) {
        # Fallback: Calculate matures_in_period from final_maturity_date if available
        # CORRECTED: Only mark as "Maturing" if bond matures WITHIN the analysis period
        data <- data %>%
            dplyr::mutate(
                days_to_maturity_calc = as.numeric(difftime(final_maturity_date, params$end_date, units = "days")),
                matures_in_period = !is.na(final_maturity_date) &
                                    final_maturity_date >= params$start_date &
                                    final_maturity_date <= params$end_date
            )
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

    # ════════════════════════════════════════════════════════════════════════════
    # FIX: Improved label handling to reduce overlap in crowded areas
    # Uses enhanced smart_label function with spatial distribution awareness
    # ════════════════════════════════════════════════════════════════════════════
    if (show_labels) {
        # Use improved smart_label with spatial awareness
        label_data <- smart_label(
            data,
            label_col = "bond",
            priority_col = "z_score",
            max_labels = 15,  # Increased from 10 to show more labels
            x_col = "x_var_plot",
            y_col = "yield_to_maturity",
            min_spacing = 0.3
        )

        p <- p +
            ggrepel::geom_text_repel(
                data = label_data,
                aes(y = yield_to_maturity, label = bond),
                size = 3.2,
                max.overlaps = Inf,          # Never drop labels due to overlap
                min.segment.length = 0.2,    # Draw connectors for closer labels
                segment.size = 0.3,
                segment.alpha = 0.5,
                segment.color = "grey60",
                box.padding = 0.5,           # Padding around labels
                point.padding = 0.3,         # Distance from points
                force = 2,                   # Repulsion force
                force_pull = 0.5,            # Pull toward original position
                direction = "both",          # Allow movement in x and y
                seed = 42,                   # Reproducible positioning
                color = insele_palette$dark_gray,
                fontface = "bold",
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
                range = c(1.5, 8),
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
                # Calculate dynamic spread summary with rich/cheap count
                n_rich <- sum(data$spread_to_curve < 0, na.rm = TRUE)
                n_cheap <- sum(data$spread_to_curve > 0, na.rm = TRUE)
                spread_summary <- if(sum(!is.na(data$spread_to_curve)) > 0) {
                    avg_spread <- mean(abs(data$spread_to_curve), na.rm = TRUE)
                    max_spread <- max(abs(data$spread_to_curve), na.rm = TRUE)
                    sprintf("Avg: %.1f bps | Max: %.1f bps (%d rich, %d cheap vs fitted curve)",
                            avg_spread, max_spread, n_rich, n_cheap)
                } else {
                    "Spread: N/A"
                }

                paste0(
                    "Model: ", params$curve_model, " | ",
                    "Date: ", format(params$curve_date, "%d %B %Y"), " | ",
                    "Bonds: ", length(unique(data$bond)),
                    "\n", spread_summary
                )
            },
            x = x_label,
            y = "Yield to Maturity",
            # FIX: Use SIGNAL_THRESHOLDS constants for consistency across dashboard
            caption = paste0(
                sprintf("Z-Score = Signal Strength (|Z| > %.1f = Strong, |Z| > %.1f = Moderate) | ",
                        SIGNAL_THRESHOLDS$strong, SIGNAL_THRESHOLDS$moderate),
                "Color = Spread to Fair Value (Red = Rich/SELL, Green = Cheap/BUY) | ",
                "95% confidence band shown",
                if (has_maturing_bonds) " | Triangle = Matures in period" else "",
                if (x_var == "modified_duration") "\nX-axis: Modified Duration (not time to maturity) \u2014 compresses long-end bonds by design" else ""
            )
        ) +
        create_insele_theme() +
        # ════════════════════════════════════════════════════════════════════════════
        # FIX: Improved legend readability - larger text, better spacing
        # ════════════════════════════════════════════════════════════════════════════
        theme(
            # Legend improvements for better readability
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.box.spacing = unit(0.5, "cm"),
            legend.spacing.x = unit(0.5, "cm"),
            legend.text = element_text(size = 10),  # Increased from default
            legend.title = element_text(size = 11, face = "bold"),
            legend.key.size = unit(1.2, "lines"),
            legend.margin = ggplot2::margin(t = 10, b = 5, l = 0, r = 0),

            # Caption improvements - make it more readable
            plot.caption = element_text(
                size = 7,
                color = insele_palette$medium_gray,
                hjust = 0,
                lineheight = 1.3,
                margin = ggplot2::margin(t = 10, b = 0)
            ),

            # Add some padding around the plot for labels that extend beyond
            plot.margin = ggplot2::margin(t = 15, r = 15, b = 10, l = 10)
        )

    return(p)
}

#' @export
# 2. Relative Value Heatmap Generation
#' @param data Bond data with date, bond, yield_to_maturity, modified_duration columns
#' @param params List of parameters (optional: bond_order for custom ordering, active_bonds for filtering,
#'               label_threshold for z-score label visibility)
generate_relative_value_heatmap <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # ========================================================================
    # CRITICAL FIX: Filter to active bonds ONLY (exclude matured bonds like R186)
    # This ensures consistency with the Z-Score Distribution chart
    # ========================================================================
    active_bonds <- params$active_bonds
    if (!is.null(active_bonds) && length(active_bonds) > 0) {
        bonds_before <- unique(data$bond)
        data <- data[data$bond %in% active_bonds, ]
        bonds_after <- unique(data$bond)
        excluded <- setdiff(bonds_before, bonds_after)
        if (length(excluded) > 0) {
            message(sprintf("[Heatmap] Filtered to %d active bonds (excluded: %s)",
                           length(bonds_after), paste(excluded, collapse = ", ")))
        } else {
            message(sprintf("[Heatmap] Using %d active bonds", length(bonds_after)))
        }
    }

    # Double-check: Remove any known matured bonds as safety net
    known_matured_bonds <- c("R157", "R186", "R197", "R203", "R204", "R207", "R208", "R212", "R2023")
    found_matured <- intersect(unique(data$bond), known_matured_bonds)
    if (length(found_matured) > 0) {
        warning(sprintf("[Heatmap] REMOVING matured bonds: %s", paste(found_matured, collapse = ", ")))
        data <- data[!data$bond %in% found_matured, ]
    }

    # Get label threshold (default 1.5 for better visibility)
    label_threshold <- params$label_threshold %||% 1.5

    # ========================================================================
    # FIX: ORDER BONDS BY DURATION - Long duration at TOP of heatmap
    # ========================================================================
    # In ggplot y-axis: first factor level = BOTTOM, last level = TOP
    # So for "long at top", we need SHORT duration bonds FIRST in factor levels
    # (i.e., arrange by duration ASCENDING)
    # ========================================================================
    bond_duration_order <- data %>%
        group_by(bond) %>%
        summarise(avg_duration = mean(modified_duration, na.rm = TRUE), .groups = "drop") %>%
        arrange(avg_duration) %>%  # ASCENDING: short first in levels = long at TOP
        pull(bond)

    # Log the ordering for verification
    duration_info <- data %>%
        group_by(bond) %>%
        summarise(avg_duration = mean(modified_duration, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(avg_duration))  # Show longest first for logging
    message("[Heatmap] Bond ordering (long at top):")
    for (i in seq_len(min(5, nrow(duration_info)))) {
        message(sprintf("  TOP-%d: %s (%.1fy)", i, duration_info$bond[i], duration_info$avg_duration[i]))
    }

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

    # Count active bonds for subtitle
    n_bonds <- length(unique(rv_matrix$bond))

    # Get current date for "Today" indicator
    current_date <- Sys.Date()
    current_week <- floor_date(current_date, "week")

    # Create z-score labels with threshold
    rv_matrix <- rv_matrix %>%
        mutate(
            z_label = ifelse(
                abs(avg_z_score) >= label_threshold,
                sprintf("%+.1f", avg_z_score),  # Format with sign: +1.5, -2.3
                ""
            )
        )

    p <- ggplot(rv_matrix, aes(x = week, y = bond, fill = avg_z_score)) +
        geom_tile(color = "white", linewidth = 0.2) +  # Clean white borders between cells

        # "Today" vertical line
        geom_vline(
            xintercept = as.numeric(current_week),
            linetype = "longdash",
            color = insele_palette$primary,
            linewidth = 1,
            alpha = 0.8
        ) +

        scale_fill_gradient2(
            low = "#B71C1C",
            mid = "white",
            high = "#1B5E20",
            midpoint = 0,
            limits = c(-3, 3),
            oob = scales::squish,
            name = "Z-Score",
            guide = guide_colorbar(
                barwidth = 12,
                barheight = 0.5,
                title.position = "top",
                title.hjust = 0.5
            )
        ) +

        # Highlight strongest emerging signal in most recent week
        geom_tile(
            data = rv_matrix %>%
                filter(week == max(week)) %>%
                filter(abs(avg_z_score) == max(abs(avg_z_score))),
            aes(x = week, y = bond),
            color = insele_palette$primary, linewidth = 1.2, fill = NA
        ) +

        # Z-score labels with configurable threshold
        geom_text(aes(label = z_label),
                  size = 2.8,
                  color = "black",
                  fontface = "bold") +

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
            date_labels = "%d %b",
            expand = c(0.02, 0)  # Small expand to show Today label
        ) +
        scale_y_discrete(expand = c(0, 0)) +

        # Allow label outside plot area
        coord_cartesian(clip = "off") +

        labs(
            title = "Relative Value Heatmap",
            subtitle = sprintf("Weekly Z-scores | %d active bonds | Ordered by duration (long at top) | Labels: |Z| > %.1f",
                              n_bonds, label_threshold),
            x = "",
            y = "",
            caption = "Red = Rich (Sell) | Green = Cheap (Buy)"
        ) +
        create_insele_theme() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.text.y = element_text(face = "bold", size = 9),
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
            # Sort by absolute z-score (strongest signals first)
            bond_name = forcats::fct_reorder(bond, abs(z_score), .desc = TRUE),

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

    # Portfolio-weighted average z-score (equal weight if no weights available)
    weighted_avg_zscore <- mean(plot_data$z_score, na.rm = TRUE)

    p <- ggplot(plot_data, aes(x = forcats::fct_reorder(bond, abs(z_score)), y = z_score)) +

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

        # Portfolio average z-score reference line
        geom_hline(yintercept = weighted_avg_zscore, linetype = "dashed",
                   color = "#1B3A6B", linewidth = 0.8) +
        annotate("text", x = Inf, y = weighted_avg_zscore,
                 label = sprintf("Wtd Avg: %+.2f", weighted_avg_zscore),
                 vjust = -0.5, hjust = 1.1, size = 3, color = "#1B3A6B") +

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
        # In-plot threshold legend (more visible than a small footnote)
        annotate("label",
                 x = nrow(plot_data), y = max(3, max(plot_data$z_score) + 0.2),
                 label = "Normal: |z|<1\nNotable: 1\u2264|z|<2\nExtreme: |z|\u22652",
                 hjust = 1, vjust = 1,
                 size = 2.8,
                 fill = "white",
                 color = "grey40",
                 label.padding = unit(0.3, "lines"),
                 fontface = "italic") +

        labs(
            title = "Z-Score Distribution",
            subtitle = sprintf("Statistical Significance | %d bonds | %d in extreme territory",
                              nrow(plot_data), sum(abs(plot_data$z_score) > 2)),
            x = "",
            y = "Z-Score (Standard Deviations)",
            caption = "Positive Z = cheap (yields above fair value) | Negative Z = rich (yields below fair value)"
        ) +
        create_insele_theme() +
        theme(
            panel.grid.major.y = element_blank(),
            panel.spacing.y = unit(0.1, "lines"),
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
            # Scale DV01 to reasonable point size range (4 to 12) — min 4 ensures short-end bonds are visible
            dv01_scaled = scales::rescale(dv01, to = c(4, 12)),
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

        # Bond labels - aggressive ggrepel settings to prevent overlapping
        ggrepel::geom_text_repel(
            aes(label = bond),
            size = 3.2,
            max.overlaps = Inf,             # Never drop labels
            min.segment.length = 0.2,       # Only show segments when needed
            segment.size = 0.3,
            segment.alpha = 0.6,
            segment.color = "grey60",
            box.padding = 0.5,              # Space between label box and point
            point.padding = 0.3,            # Minimum distance from point
            force = 2,                      # Repulsion force between labels
            force_pull = 0.5,               # Force pulling labels toward points
            seed = 42,                      # Reproducible layout
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
                "Quadratic fit shown | Best convexity/duration: %s (%s, %.2f per dur\u00B2) | Lowest: %s (%s, %.2f per dur\u00B2)",
                best_conv$bond,
                if(best_conv$modified_duration > 15) "ultra-long" else if(best_conv$modified_duration > 10) "long" else if(best_conv$modified_duration > 5) "medium" else "short-end",
                best_conv$convexity_ratio,
                worst_conv$bond,
                if(worst_conv$modified_duration > 15) "ultra-long" else if(worst_conv$modified_duration > 10) "long" else if(worst_conv$modified_duration > 5) "medium" else "short-end, expected",
                worst_conv$convexity_ratio
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

    # ENHANCED: Add flags for low correlations and format labels
    cor_melted <- cor_melted %>%
        mutate(
            bond1 = factor(bond1, levels = bond_order),
            bond2 = factor(bond2, levels = bond_order),
            # Flag low correlations for highlighting (useful for diversification analysis)
            low_corr = abs(correlation) < 0.7,
            # Format correlation for display
            corr_label = sprintf("%.2f", correlation)
        )

    # ENHANCED: Dynamic text sizing based on number of bonds
    # With 17 bonds, fixed size=3 is too small; adapt based on matrix size
    n_bonds <- length(bond_order)
    text_size <- if(n_bonds > 15) {
        2.2   # Many bonds: smaller text to fit
    } else if(n_bonds > 10) {
        2.8   # Medium number: moderate text
    } else {
        3.2   # Few bonds: larger text
    }

    # Dynamic axis text size
    axis_text_size <- if(n_bonds > 15) {
        8
    } else if(n_bonds > 10) {
        9
    } else {
        10
    }

    p <- ggplot(cor_melted, aes(x = bond1, y = bond2, fill = correlation)) +

        # Heatmap tiles with thin white borders
        geom_tile(color = "white", linewidth = 0.3) +

        # ENHANCED: Correlation values with dynamic styling
        # - Low correlations (<0.7) are bold to highlight diversification opportunities
        # - Text color adapts to background for readability
        geom_text(
            aes(
                label = corr_label,
                fontface = ifelse(low_corr, "bold", "plain")
            ),
            color = ifelse(
                abs(cor_melted$correlation) > 0.5,
                "white",
                "black"
            ),
            size = text_size
        ) +

        # Advanced color scale using Insele branding
        scale_fill_gradient2(
            low = insele_palette$danger,
            mid = "white",
            high = insele_palette$primary,
            midpoint = 0,
            limits = c(-1, 1),
            breaks = c(-1, -0.5, 0, 0.5, 1),
            name = "Correlation",
            guide = guide_colorbar(
                barwidth = unit(2, "cm"),
                barheight = unit(0.3, "cm"),
                title.position = "top",
                title.hjust = 0.5
            )
        ) +

        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +

        labs(
            title = "Cross-Asset Correlation Matrix",
            subtitle = paste0("Based on ", format(nrow(data), big.mark = ","),
                              " observations | Ordered by duration | Bold = low correlation (<0.7)"),
            x = NULL,
            y = NULL,
            caption = paste0("Date range: ", format(min(data$date), "%Y-%m-%d"),
                             " to ", format(max(data$date), "%Y-%m-%d"),
                             " | Based on yield changes (not levels)",
                             "\nNote: Near-maturity bonds may show low correlations due to reduced market sensitivity rather than true portfolio diversification.")
        ) +

        create_insele_theme() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = axis_text_size),
            axis.text.y = element_text(hjust = 1, size = axis_text_size),
            panel.border = element_rect(fill = NA, color = insele_palette$dark_gray, linewidth = 1),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            legend.key.height = unit(0.3, "cm"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 9),
            plot.margin = ggplot2::margin(10, 10, 10, 10),
            plot.caption = element_text(hjust = 0, size = 8, lineheight = 1.2,
                                        margin = ggplot2::margin(t = 10)),
            panel.grid = element_blank()
        ) +

        # ENHANCED: Square tiles for proper matrix appearance
        coord_fixed()

    return(p)
}

#' @export
# 26. Term Structure 3D (converted to 2D heatmap for web compatibility)
generate_term_structure_3d_plot <- function(data, params) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Create term structure evolution data
    # FIXED: Extended upper bound to Inf to capture ultra-long bonds (R2048, R2053 with 20+ years)
    term_evolution <- data %>%
        mutate(
            date_group = floor_date(date, "week"),
            duration_bucket = cut(modified_duration,
                                  breaks = c(0, 3, 5, 7, 10, 15, 20, Inf),
                                  labels = c("0-3y", "3-5y", "5-7y", "7-10y", "10-15y", "15-20y", "20y+"),
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

    # CRITICAL FIX: Calculate adaptive date breaks to prevent label corruption
    # When date range is large (e.g., 5+ years), too many monthly labels causes
    # garbled overlapping text like "JgPMMAtAtyiuA.SeONDebrPM..."
    date_range_days <- as.numeric(difftime(
        max(term_evolution$date_group, na.rm = TRUE),
        min(term_evolution$date_group, na.rm = TRUE),
        units = "days"
    ))

    # Adaptive breaks based on date range
    date_breaks <- if(date_range_days > 1825) {
        "6 months"   # > 5 years: semi-annual labels
    } else if(date_range_days > 730) {
        "3 months"   # > 2 years: quarterly labels
    } else if(date_range_days > 365) {
        "2 months"   # > 1 year: bi-monthly labels
    } else if(date_range_days > 180) {
        "1 month"    # > 6 months: monthly labels
    } else {
        "2 weeks"    # < 6 months: bi-weekly labels
    }

    # Create the heatmap
    p <- ggplot(term_evolution, aes(x = date_group, y = duration_bucket, fill = avg_yield)) +

        # Heatmap tiles
        geom_tile(color = "white", linewidth = 0.2) +

        # Add contour lines for better visualization
        geom_contour(aes(z = avg_yield), color = "white", alpha = 0.3, linewidth = 0.5) +

        # Diverging colour scale centred on mean yield to highlight dynamics
        scale_fill_gradient2(
            low = "#1B3A6B",
            mid = "#F5F5F5",
            high = "#D32F2F",
            midpoint = mean(term_evolution$avg_yield, na.rm = TRUE),
            name = "Yield (%)",
            na.value = "#E0E0E0",
            guide = guide_colorbar(
                barwidth = 20,
                barheight = 0.5,
                title.position = "top",
                title.hjust = 0.5
            )
        ) +

        # CRITICAL FIX: Adaptive date breaks to prevent label corruption
        scale_x_date(
            date_breaks = date_breaks,
            date_labels = "%b\n%Y",
            expand = expansion(mult = c(0.01, 0.01))
        ) +

        scale_y_discrete(expand = c(0, 0)) +

        labs(
            title = "Term Structure Evolution",
            subtitle = sprintf("Yield surface dynamics across maturity buckets | Centred on mean yield (%.2f%%)",
                               mean(term_evolution$avg_yield, na.rm = TRUE)),
            x = "Date",
            y = "Maturity Bucket",
            caption = "Weekly averages | Blue = below mean yield | Red = above mean yield | Grey = no active bonds in bucket"
        ) +

        create_insele_theme() +
        theme(
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
            panel.border = element_rect(fill = NA, color = insele_palette$dark_gray, size = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
        )

    return(p)
}

# ================================================================================
# NEW FIXED INCOME ANALYTICS: CURVE DYNAMICS & RELATIVE VALUE
# ================================================================================

#' @title Generate Curve Spreads Plot
#' @description Time series of key yield curve spreads (2s10s, 5s10s, 10s20s, butterfly)
#' @param data Bond data with date, yield_to_maturity, modified_duration
#' @param params List of optional parameters
#' @return ggplot object
#' @export
generate_curve_spreads_plot <- function(data, params) {

    # Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Calculate key spreads daily using modified duration as maturity proxy
    spread_data <- data %>%
        filter(!is.na(yield_to_maturity), !is.na(modified_duration)) %>%
        group_by(date) %>%
        summarise(
            # Get yields by approximate maturity buckets
            yield_2y = {
                short_bonds <- yield_to_maturity[modified_duration >= 1.5 & modified_duration <= 3]
                if(length(short_bonds) > 0) mean(short_bonds, na.rm = TRUE) else NA_real_
            },
            yield_5y = {
                mid_bonds <- yield_to_maturity[modified_duration >= 4 & modified_duration <= 6]
                if(length(mid_bonds) > 0) mean(mid_bonds, na.rm = TRUE) else NA_real_
            },
            yield_10y = {
                long_bonds <- yield_to_maturity[modified_duration >= 8 & modified_duration <= 12]
                if(length(long_bonds) > 0) mean(long_bonds, na.rm = TRUE) else NA_real_
            },
            yield_20y = {
                ultra_bonds <- yield_to_maturity[modified_duration >= 15]
                if(length(ultra_bonds) > 0) mean(ultra_bonds, na.rm = TRUE) else NA_real_
            },
            .groups = "drop"
        ) %>%
        mutate(
            # Key spreads in basis points
            spread_2s10s = (yield_10y - yield_2y) * 100,
            spread_5s10s = (yield_10y - yield_5y) * 100,
            spread_10s20s = (yield_20y - yield_10y) * 100,
            # Butterfly: 2 * 5y - 2y - 10y (measures curve curvature)
            butterfly_2s5s10s = (2 * yield_5y - yield_2y - yield_10y) * 100
        ) %>%
        filter(!is.na(spread_2s10s) | !is.na(butterfly_2s5s10s))

    if(nrow(spread_data) == 0) {
        return(NULL)
    }

    # Reshape for plotting
    spread_long <- spread_data %>%
        select(date, spread_2s10s, spread_5s10s, spread_10s20s, butterfly_2s5s10s) %>%
        pivot_longer(-date, names_to = "spread_type", values_to = "spread_bps") %>%
        filter(!is.na(spread_bps)) %>%
        mutate(
            spread_label = case_when(
                spread_type == "spread_2s10s" ~ "2s10s Spread",
                spread_type == "spread_5s10s" ~ "5s10s Spread",
                spread_type == "spread_10s20s" ~ "10s20s Spread",
                spread_type == "butterfly_2s5s10s" ~ "2s5s10s Butterfly",
                TRUE ~ spread_type
            ),
            spread_label = factor(spread_label, levels = c(
                "2s10s Spread", "5s10s Spread", "10s20s Spread", "2s5s10s Butterfly"
            ))
        )

    # Current values for annotation
    latest <- spread_data %>% filter(date == max(date))

    # Get values with fallback
    latest_2s10s <- if(!is.na(latest$spread_2s10s)) latest$spread_2s10s else NA
    latest_butterfly <- if(!is.na(latest$butterfly_2s5s10s)) latest$butterfly_2s5s10s else NA

    # Build subtitle
    subtitle_parts <- c()
    if(!is.na(latest_2s10s)) subtitle_parts <- c(subtitle_parts, sprintf("2s10s: %.0f bps", latest_2s10s))
    if(!is.na(latest_butterfly)) subtitle_parts <- c(subtitle_parts, sprintf("Butterfly: %.0f bps", latest_butterfly))
    subtitle_text <- paste0(paste(subtitle_parts, collapse = " | "),
                            " (as of ", format(latest$date, "%d %b %Y"), ")")

    # Adaptive date breaks
    date_range_days <- as.numeric(difftime(max(spread_data$date), min(spread_data$date), units = "days"))
    date_breaks <- if(date_range_days > 1095) {
        "6 months"
    } else if(date_range_days > 365) {
        "3 months"
    } else if(date_range_days > 180) {
        "2 months"
    } else {
        "1 month"
    }

    p <- ggplot(spread_long, aes(x = date, y = spread_bps, color = spread_label)) +
        geom_line(linewidth = 1, na.rm = TRUE) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +

        # Highlight current level
        geom_point(data = spread_long %>% filter(date == max(date)),
                   size = 3, show.legend = FALSE) +

        scale_color_manual(
            values = c(
                "2s10s Spread" = insele_palette$primary,
                "5s10s Spread" = insele_palette$secondary,
                "10s20s Spread" = insele_palette$success,
                "2s5s10s Butterfly" = insele_palette$warning
            ),
            name = NULL,
            drop = FALSE
        ) +

        scale_x_date(date_breaks = date_breaks, date_labels = "%b\n%Y") +
        scale_y_continuous(labels = function(x) paste0(x, " bps")) +

        labs(
            title = "Curve Spread Dynamics",
            subtitle = subtitle_text,
            x = NULL,
            y = "Spread (basis points)",
            caption = "Positive 2s10s = steep curve | Negative butterfly = humped curve"
        ) +

        create_insele_theme() +
        theme(
            legend.position = "top",
            legend.direction = "horizontal",
            legend.text = element_text(size = 9),
            plot.title = element_text(hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            plot.caption = element_text(hjust = 0.5, size = 8, color = "gray50")
        )

    return(p)
}

#' @title Generate Curve Shape Indicator
#' @description Timeline showing yield curve shape classification over time
#' @param data Bond data with date, yield_to_maturity, modified_duration
#' @param params List of optional parameters
#' @return ggplot object
#' @export
generate_curve_shape_indicator <- function(data, params) {

    # Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Calculate curve shape metrics
    shape_data <- data %>%
        filter(!is.na(yield_to_maturity), !is.na(modified_duration)) %>%
        group_by(date) %>%
        summarise(
            yield_short = mean(yield_to_maturity[modified_duration <= 3], na.rm = TRUE),
            yield_mid = mean(yield_to_maturity[modified_duration > 3 & modified_duration <= 7], na.rm = TRUE),
            yield_long = mean(yield_to_maturity[modified_duration > 7], na.rm = TRUE),
            n_bonds = n(),
            .groups = "drop"
        ) %>%
        filter(!is.na(yield_short) & !is.na(yield_mid) & !is.na(yield_long)) %>%
        mutate(
            # Classify curve shape
            curve_shape = case_when(
                yield_long > yield_mid & yield_mid > yield_short &
                    (yield_long - yield_short) > 1.5 ~ "Steep",
                yield_long > yield_mid & yield_mid > yield_short ~ "Normal",
                yield_short > yield_long ~ "Inverted",
                yield_mid > yield_long & yield_mid > yield_short ~ "Humped",
                abs(yield_long - yield_short) < 0.5 ~ "Flat",
                TRUE ~ "Normal"
            ),
            curve_shape = factor(curve_shape,
                                 levels = c("Steep", "Normal", "Flat", "Humped", "Inverted"))
        )

    if(nrow(shape_data) == 0) {
        return(NULL)
    }

    # Adaptive date breaks
    date_range_days <- as.numeric(difftime(max(shape_data$date), min(shape_data$date), units = "days"))
    date_breaks <- if(date_range_days > 1095) {
        "1 year"
    } else if(date_range_days > 365) {
        "6 months"
    } else {
        "3 months"
    }

    # Current shape for subtitle
    current_shape <- shape_data %>% filter(date == max(date)) %>% pull(curve_shape) %>% as.character()

    # Create timeline showing curve shape
    p <- ggplot(shape_data, aes(x = date, y = 1, fill = curve_shape)) +
        geom_tile(height = 1, color = NA) +

        scale_fill_manual(
            values = c(
                "Steep" = insele_palette$success,
                "Normal" = insele_palette$secondary,
                "Flat" = insele_palette$warning,
                "Humped" = "#9B59B6",
                "Inverted" = insele_palette$danger
            ),
            name = "Curve Shape",
            drop = FALSE
        ) +

        scale_x_date(date_breaks = date_breaks, date_labels = "%b %Y") +

        labs(
            title = "Yield Curve Shape Evolution",
            subtitle = paste0("Current: ", current_shape, " | Historical curve regime classification"),
            x = NULL,
            y = NULL
        ) +

        create_insele_theme() +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size = unit(0.4, "cm"),
            legend.text = element_text(size = 8),
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 11),
            plot.subtitle = element_text(hjust = 0.5, size = 9),
            axis.text.x = element_text(size = 8)
        )

    return(p)
}

#' @title Generate Relative Value Scanner
#' @description Scatter plot showing bonds' deviation from fitted curve (rich/cheap)
#' @param data Bond data with date, yield_to_maturity, modified_duration, bond
#' @param params List of optional parameters
#' @return ggplot object
#' @export
generate_relative_value_scanner <- function(data, params) {

    # Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Get latest date data
    latest_data <- data %>%
        filter(date == max(date)) %>%
        filter(!is.na(yield_to_maturity), !is.na(modified_duration)) %>%
        # Get one row per bond (in case of duplicates)
        group_by(bond) %>%
        slice(1) %>%
        ungroup()

    if(nrow(latest_data) < 3) {
        return(NULL)
    }

    # Fit a curve (quadratic polynomial)
    fit <- tryCatch(
        lm(yield_to_maturity ~ poly(modified_duration, 2), data = latest_data),
        error = function(e) NULL
    )

    if(is.null(fit)) {
        return(NULL)
    }

    latest_data <- latest_data %>%
        mutate(
            fitted_yield = predict(fit, newdata = .),
            residual_bps = (yield_to_maturity - fitted_yield) * 100,
            rich_cheap = case_when(
                residual_bps < -10 ~ "Rich (Expensive)",
                residual_bps > 10 ~ "Cheap (Value)",
                TRUE ~ "Fair Value"
            ),
            rich_cheap = factor(rich_cheap, levels = c("Rich (Expensive)", "Fair Value", "Cheap (Value)"))
        )

    # Calculate R-squared for subtitle
    r_squared <- summary(fit)$r.squared

    # Create scatter with residuals
    p <- ggplot(latest_data, aes(x = modified_duration, y = residual_bps)) +

        # Zero line (fair value)
        geom_hline(yintercept = 0, linetype = "solid", color = "gray30", linewidth = 1) +

        # Threshold bands
        geom_hline(yintercept = c(-10, 10), linetype = "dashed", color = "gray60") +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = 10, ymax = Inf,
                 fill = insele_palette$success, alpha = 0.1) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -10,
                 fill = insele_palette$danger, alpha = 0.1) +

        # Points
        geom_point(aes(color = rich_cheap), size = 4) +

        # Labels for bonds
        ggrepel::geom_text_repel(
            aes(label = bond),
            size = 3,
            fontface = "bold",
            max.overlaps = 20,
            box.padding = 0.3,
            point.padding = 0.2,
            segment.color = "gray70",
            segment.size = 0.3
        ) +

        scale_color_manual(
            values = c(
                "Rich (Expensive)" = insele_palette$danger,
                "Fair Value" = insele_palette$secondary,
                "Cheap (Value)" = insele_palette$success
            ),
            name = NULL,
            drop = FALSE
        ) +

        scale_y_continuous(
            labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x), " bps")
        ) +

        scale_x_continuous(
            labels = function(x) paste0(round(x, 1), "y")
        ) +

        labs(
            title = "Relative Value Scanner",
            subtitle = sprintf("Deviation from fitted curve (R² = %.2f) | %s",
                               r_squared, format(max(data$date), "%d %b %Y")),
            x = "Modified Duration (years)",
            y = "Rich ← → Cheap (bps vs fitted curve)",
            caption = "Above line = cheap (undervalued) | Below line = rich (overvalued)"
        ) +

        create_insele_theme() +
        theme(
            legend.position = "top",
            plot.title = element_text(hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            plot.caption = element_text(hjust = 0.5, size = 8, color = "gray50")
        )

    return(p)
}

#' @title Generate RV Summary Table
#' @description Table showing top relative value opportunities with recommendations
#' @param data Bond data with date, yield_to_maturity, modified_duration, bond
#' @param params List of optional parameters
#' @return Data frame with relative value analysis
#' @export
generate_rv_summary_table <- function(data, params) {

    # Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Get latest data
    latest <- data %>%
        filter(date == max(date)) %>%
        filter(!is.na(yield_to_maturity), !is.na(modified_duration)) %>%
        group_by(bond) %>%
        slice(1) %>%
        ungroup()

    if(nrow(latest) < 3) {
        return(data.frame(
            Bond = character(),
            `YTM (%)` = numeric(),
            `Duration` = numeric(),
            `vs Curve (bps)` = numeric(),
            `RV Score` = numeric(),
            Recommendation = character()
        ))
    }

    # Fit curve
    fit <- tryCatch(
        lm(yield_to_maturity ~ poly(modified_duration, 2), data = latest),
        error = function(e) NULL
    )

    if(is.null(fit)) {
        return(data.frame(
            Bond = character(),
            `YTM (%)` = numeric(),
            `Duration` = numeric(),
            `vs Curve (bps)` = numeric(),
            `RV Score` = numeric(),
            Recommendation = character()
        ))
    }

    rv_table <- latest %>%
        mutate(
            fitted = predict(fit, newdata = .),
            residual_bps = (yield_to_maturity - fitted) * 100,
            # Simple carry proxy: yield / duration (higher = better carry per unit risk)
            carry_score = yield_to_maturity / modified_duration,
            # Composite score: combine residual (value) with carry
            rv_score = residual_bps + (carry_score * 10)
        ) %>%
        arrange(desc(rv_score)) %>%
        mutate(
            recommendation = case_when(
                rv_score > 15 ~ "Strong Buy",
                rv_score > 5 ~ "Buy",
                rv_score < -15 ~ "Avoid",
                rv_score < -5 ~ "Underweight",
                TRUE ~ "Neutral"
            )
        ) %>%
        select(
            Bond = bond,
            `YTM (%)` = yield_to_maturity,
            Duration = modified_duration,
            `vs Curve (bps)` = residual_bps,
            `RV Score` = rv_score,
            Recommendation = recommendation
        ) %>%
        mutate(
            `YTM (%)` = round(`YTM (%)`, 2),
            Duration = round(Duration, 1),
            `vs Curve (bps)` = round(`vs Curve (bps)`, 0),
            `RV Score` = round(`RV Score`, 1)
        )

    return(rv_table)
}


# ================================================================================
# MARKET INTELLIGENCE - YIELD ENVIRONMENT MONITOR
# ================================================================================

#' Generate Yield Percentile Heatmap
#'
#' Shows where current yields sit vs their historical distribution by tenor bucket.
#' Answers: "Are we at extremes? Is there room to move?"
#'
#' @param data Bond data with date, bond, yield_to_maturity, modified_duration
#' @param params Additional parameters (unused)
#' @return ggplot object
#' @export
generate_yield_percentile_heatmap <- function(data, params) {

    # Validate data
    if (is.null(data) || nrow(data) < 10) {
        return(NULL)
    }

    # Ensure required columns exist
    required_cols <- c("date", "yield_to_maturity", "modified_duration")
    if (!all(required_cols %in% names(data))) {
        return(NULL)
    }

    # Filter valid data
    data <- data %>%
        filter(
            !is.na(yield_to_maturity),
            !is.na(modified_duration),
            is.finite(yield_to_maturity),
            is.finite(modified_duration),
            yield_to_maturity > 1.5,
            yield_to_maturity < 25,
            modified_duration > 0.5
        )

    if (nrow(data) < 10) return(NULL)

    # Check data history length for subtitle note
    min_date <- min(data$date, na.rm = TRUE)
    max_date <- max(data$date, na.rm = TRUE)
    date_range_years <- as.numeric(difftime(max_date, min_date, units = "days")) / 365.25

    # Create detailed history note
    history_note <- if(date_range_years >= 3) {
        sprintf("Since %s (%.1f years)", format(min_date, "%b %Y"), date_range_years)
    } else if(date_range_years >= 1) {
        sprintf("Since %s (%.1f years - building history)", format(min_date, "%b %Y"), date_range_years)
    } else {
        sprintf("Since %s (%.1f years - limited history)", format(min_date, "%b %Y"), date_range_years)
    }

    # Debug output with more detail
    message("=== YIELD PERCENTILE DEBUG ===")
    message(sprintf("Using FULL historical data: %s to %s (%.1f years)",
                    format(min_date, "%Y-%m-%d"),
                    format(max_date, "%Y-%m-%d"),
                    date_range_years))
    message(sprintf("Total observations: %d | Unique bonds: %d",
                    nrow(data), n_distinct(data$bond)))

    # Define tenor buckets
    tenor_data <- data %>%
        mutate(
            tenor = case_when(
                modified_duration <= 2 ~ "1-2y",
                modified_duration <= 4 ~ "2-4y",
                modified_duration <= 6 ~ "4-6y",
                modified_duration <= 8 ~ "6-8y",
                modified_duration <= 10 ~ "8-10y",
                TRUE ~ "10y+"
            ),
            tenor = factor(tenor, levels = c("1-2y", "2-4y", "4-6y", "6-8y", "8-10y", "10y+"))
        ) %>%
        filter(!is.na(tenor))

    # Calculate percentile for each tenor
    # Group by tenor and date to get daily average yield per bucket
    daily_tenor <- tenor_data %>%
        group_by(date, tenor) %>%
        summarise(avg_yield = mean(yield_to_maturity, na.rm = TRUE), .groups = "drop")

    # For each tenor, calculate where current yield sits vs history
    max_date <- max(daily_tenor$date)

    percentile_data <- daily_tenor %>%
        group_by(tenor) %>%
        filter(n() >= 5) %>%  # Need sufficient history
        summarise(
            current_yield = avg_yield[date == max_date][1],
            # Calculate percentile: % of historical observations <= current
            percentile = mean(avg_yield <= current_yield, na.rm = TRUE) * 100,
            min_yield = min(avg_yield, na.rm = TRUE),
            max_yield = max(avg_yield, na.rm = TRUE),
            median_yield = median(avg_yield, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        filter(!is.na(percentile), !is.na(current_yield)) %>%
        mutate(
            # Classify based on percentile
            level_class = case_when(
                percentile >= 90 ~ "Very High (90%+)",
                percentile >= 75 ~ "High (75-90%)",
                percentile >= 25 ~ "Normal (25-75%)",
                percentile >= 10 ~ "Low (10-25%)",
                TRUE ~ "Very Low (<10%)"
            ),
            level_class = factor(level_class,
                                 levels = c("Very Low (<10%)", "Low (10-25%)", "Normal (25-75%)",
                                            "High (75-90%)", "Very High (90%+)"))
        )

    if (nrow(percentile_data) < 2) return(NULL)

    # Create the visualization with horizontal bars
    p <- ggplot(percentile_data, aes(x = percentile, y = tenor)) +

        # Background track (full range)
        geom_segment(aes(x = 0, xend = 100, yend = tenor),
                     color = "gray85", linewidth = 12, lineend = "round") +

        # Percentile bar with color based on classification
        geom_segment(aes(x = 0, xend = percentile, yend = tenor, color = level_class),
                     linewidth = 12, lineend = "round") +

        # Current yield label - positioned OUTSIDE the bar
        geom_label(aes(x = pmin(percentile + 8, 95),
                       label = sprintf("%.2f%%", current_yield)),
                   hjust = 0, size = 3.2, fontface = "bold",
                   fill = "white", label.size = 0, label.padding = unit(0.15, "lines")) +

        # Percentile value - INSIDE the bar, left-aligned (P = percentile)
        geom_text(aes(x = 5,
                      label = sprintf("P%.0f", percentile)),
                  hjust = 0, size = 3, color = "white", fontface = "bold") +

        # Reference lines at quartiles
        geom_vline(xintercept = c(25, 50, 75), linetype = "dashed",
                   color = "gray50", alpha = 0.4, linewidth = 0.5) +

        scale_color_manual(
            values = c(
                "Very Low (<10%)" = "#27AE60",
                "Low (10-25%)" = "#82E0AA",
                "Normal (25-75%)" = "#5DADE2",
                "High (75-90%)" = "#F5B041",
                "Very High (90%+)" = "#E74C3C"
            ),
            name = NULL,
            drop = TRUE,  # Only show categories present in data
            breaks = unique(percentile_data$level_class)  # Only legend items actually used
        ) +

        scale_x_continuous(
            limits = c(0, 115),
            breaks = c(0, 25, 50, 75, 100),
            labels = c("0%", "25%", "50%", "75%", "100%"),
            expand = c(0, 0)
        ) +

        labs(
            title = "Yield Levels vs History",
            subtitle = sprintf("Current yields vs historical distribution | %s", history_note),
            x = "Historical Percentile",
            y = NULL,
            caption = "Higher percentile = yields HIGH vs history"
        ) +

        create_insele_theme() +
        theme(
            legend.position = "right",
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.6, "lines"),
            legend.margin = ggplot2::margin(0, 0, 0, 5),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 10, face = "bold"),
            plot.caption = element_text(size = 8, color = "gray50", hjust = 0)
        )

    return(p)
}


#' Generate Rate of Change Monitor
#'
#' Shows yield changes at different horizons by tenor bucket.
#' Answers: "What's the momentum? Are rates rising or falling?"
#'
#' @param data Bond data with date, bond, yield_to_maturity, modified_duration
#' @param params Additional parameters (unused)
#' @return ggplot object
#' @export
generate_rate_of_change_monitor <- function(data, params) {

    # Validate data
    if (is.null(data) || nrow(data) < 30) {
        return(NULL)
    }

    # Ensure required columns exist
    required_cols <- c("date", "bond", "yield_to_maturity", "modified_duration")
    if (!all(required_cols %in% names(data))) {
        return(NULL)
    }

    # Filter valid data
    data <- data %>%
        filter(
            !is.na(yield_to_maturity),
            !is.na(modified_duration),
            is.finite(yield_to_maturity),
            is.finite(modified_duration),
            yield_to_maturity > 1.5,
            modified_duration > 0.5
        )

    if (nrow(data) < 30) return(NULL)

    max_date <- max(data$date, na.rm = TRUE)

    # Calculate changes at different horizons
    roc_data <- data %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            change_1w = yield_to_maturity - lag(yield_to_maturity, 5),
            change_1m = yield_to_maturity - lag(yield_to_maturity, 21),
            change_3m = yield_to_maturity - lag(yield_to_maturity, 63)
        ) %>%
        ungroup() %>%
        filter(date == max_date) %>%
        filter(!is.na(change_1m))

    if (nrow(roc_data) < 2) return(NULL)

    # CRITICAL: Ensure all 3 tenor buckets are created
    # Buckets adjusted to match actual SA bond universe (max duration ~9.5y)
    roc_data <- roc_data %>%
        mutate(
            tenor = case_when(
                modified_duration < 4 ~ "Short (0-4y)",
                modified_duration < 7 ~ "Medium (4-7y)",
                modified_duration >= 7 ~ "Long (7y+)"
            ),
            tenor = factor(tenor, levels = c("Short (0-4y)", "Medium (4-7y)", "Long (7y+)"))
        ) %>%
        filter(!is.na(tenor))

    # Debug: Print tenor distribution
    message("=== RATE MOMENTUM DEBUG ===")
    message(sprintf("Bonds with duration >= 10: %d",
                    sum(roc_data$modified_duration >= 10, na.rm = TRUE)))
    message("Tenor counts:")
    print(table(roc_data$tenor, useNA = "always"))

    # Calculate average change by tenor and horizon
    avg_changes <- roc_data %>%
        select(tenor, modified_duration, change_1w, change_1m, change_3m) %>%
        pivot_longer(cols = starts_with("change_"),
                     names_to = "horizon", values_to = "change") %>%
        mutate(change_bps = change * 100) %>%
        group_by(tenor, horizon) %>%
        summarise(
            avg_change = mean(change_bps, na.rm = TRUE),
            n_bonds = n(),
            .groups = "drop"
        ) %>%
        mutate(
            horizon = case_when(
                horizon == "change_1w" ~ "1 Week",
                horizon == "change_1m" ~ "1 Month",
                horizon == "change_3m" ~ "3 Month"
            ),
            horizon = factor(horizon, levels = c("1 Week", "1 Month", "3 Month")),
            # Color for labels based on direction
            label_color = ifelse(avg_change >= 0, "#E74C3C", "#27AE60")
        )

    # Ensure all tenor-horizon combinations exist
    all_combos <- expand.grid(
        tenor = levels(avg_changes$tenor),
        horizon = levels(avg_changes$horizon),
        stringsAsFactors = FALSE
    )

    avg_changes <- all_combos %>%
        left_join(avg_changes, by = c("tenor", "horizon")) %>%
        mutate(
            avg_change = ifelse(is.na(avg_change), 0, avg_change),
            label_color = ifelse(is.na(label_color), "gray50", label_color),
            tenor = factor(tenor, levels = c("Short (0-4y)", "Medium (4-7y)", "Long (7y+)")),
            horizon = factor(horizon, levels = c("1 Week", "1 Month", "3 Month"))
        )

    # Calculate y-axis limits with padding
    y_max <- max(abs(avg_changes$avg_change), na.rm = TRUE) * 1.3
    y_max <- max(y_max, 50)  # Minimum range of +/- 50 bps

    p <- ggplot(avg_changes, aes(x = horizon, y = avg_change, fill = tenor)) +

        geom_col(position = position_dodge(width = 0.75), width = 0.65) +

        # Zero reference line
        geom_hline(yintercept = 0, linewidth = 0.8, color = "gray30") +

        # Value labels with conditional coloring
        geom_text(aes(label = sprintf("%+.0f", avg_change),
                      y = avg_change + sign(avg_change) * (y_max * 0.08)),
                  position = position_dodge(width = 0.75),
                  size = 3, fontface = "bold",
                  color = avg_changes$label_color) +

        scale_fill_manual(
            values = c(
                "Short (0-4y)" = insele_palette$primary,
                "Medium (4-7y)" = "#5DADE2",
                "Long (7y+)" = "#85929E"
            ),
            name = NULL,
            drop = FALSE  # Show all legend items even if missing data
        ) +

        scale_y_continuous(
            limits = c(-y_max, y_max),
            labels = function(x) paste0(ifelse(x > 0, "+", ""), x, " bps"),
            breaks = scales::pretty_breaks(n = 5)
        ) +

        labs(
            title = "Rate Momentum",
            subtitle = "Average yield change by tenor bucket",
            x = NULL,
            y = "Δ (bps)",
            caption = "Green = yields falling (bullish) | Red = yields rising (bearish)"
        ) +

        create_insele_theme() +
        theme(
            legend.position = "top",
            legend.direction = "horizontal",
            legend.margin = ggplot2::margin(0, 0, 5, 0),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(size = 10, face = "bold"),
            plot.caption = element_text(size = 8, color = "gray50")
        )

    return(p)
}


# ================================================================================
# MARKET INTELLIGENCE - CURVE SHAPE & MOMENTUM
# ================================================================================

#' Generate Curve Comparison Plot
#'
#' Visual comparison of current yield curve vs historical snapshots.
#' Answers: "How has the curve evolved? Where are we vs history?"
#'
#' @param data Bond data with date, bond, yield_to_maturity, modified_duration
#' @param params Additional parameters (unused)
#' @return ggplot object
#' @export
generate_curve_comparison_plot <- function(data, params) {

    # Validate data
    if (is.null(data) || nrow(data) < 30) {
        return(NULL)
    }

    # Ensure required columns exist
    required_cols <- c("date", "bond", "yield_to_maturity", "modified_duration")
    if (!all(required_cols %in% names(data))) {
        return(NULL)
    }

    # Filter valid data (broader range to preserve historical bonds)
    data <- data %>%
        filter(
            !is.na(yield_to_maturity),
            !is.na(modified_duration),
            is.finite(yield_to_maturity),
            is.finite(modified_duration),
            yield_to_maturity > 0,
            yield_to_maturity < 50,
            modified_duration > 0
        )

    if (nrow(data) < 30) return(NULL)

    # Get key dates
    max_date <- max(data$date, na.rm = TRUE)
    date_1m <- max_date - 30
    date_3m <- max_date - 90

    # ════════════════════════════════════════════════════════════════════════
    # HELPER FUNCTION: Get curve at a specific date
    # Returns ALL bonds that had data on that date (including now-matured bonds)
    # ════════════════════════════════════════════════════════════════════════
    get_curve_at_date <- function(data, target_date, curve_name) {
        # Find the actual date closest to target (handles weekends/holidays)
        available_dates <- unique(data$date)
        actual_date <- available_dates[which.min(abs(available_dates - target_date))]

        curve <- data %>%
            filter(date == actual_date,
                   !is.na(yield_to_maturity),
                   !is.na(modified_duration),
                   yield_to_maturity > 1.5,
                   yield_to_maturity < 25,
                   modified_duration > 0.5) %>%
            select(bond, modified_duration, yield_to_maturity) %>%
            arrange(modified_duration) %>%
            mutate(curve_type = curve_name,
                   curve_date = actual_date)

        return(curve)
    }

    # ════════════════════════════════════════════════════════════════════════
    # GET CURVES AT DIFFERENT POINTS IN TIME
    # Each curve includes ALL bonds that were active at that specific date
    # ════════════════════════════════════════════════════════════════════════
    current_curve <- get_curve_at_date(data, max_date, "Current")
    curve_1m <- get_curve_at_date(data, date_1m, "1M Ago")
    curve_3m <- get_curve_at_date(data, date_3m, "3M Ago")

    if (nrow(current_curve) < 3) return(NULL)

    # Debug output: Log what bonds are in each curve
    message("=== CURVE COMPARISON DEBUG ===")
    message(sprintf("Current curve (%s): %d bonds - %s",
                    max_date, nrow(current_curve),
                    paste(sort(current_curve$bond), collapse = ", ")))
    if(nrow(curve_1m) > 0) {
        message(sprintf("1M Ago curve (%s): %d bonds - %s",
                        curve_1m$curve_date[1], nrow(curve_1m),
                        paste(sort(curve_1m$bond), collapse = ", ")))
    }
    if(nrow(curve_3m) > 0) {
        message(sprintf("3M Ago curve (%s): %d bonds - %s",
                        curve_3m$curve_date[1], nrow(curve_3m),
                        paste(sort(curve_3m$bond), collapse = ", ")))
    }

    # Identify bonds that have matured (were in historical curves but not current)
    all_historical_bonds <- union(curve_1m$bond, curve_3m$bond)
    matured_bonds <- setdiff(all_historical_bonds, current_curve$bond)
    if(length(matured_bonds) > 0) {
        message(sprintf("Bonds in historical curves but not current (matured?): %s",
                        paste(sort(matured_bonds), collapse = ", ")))
    }

    # ════════════════════════════════════════════════════════════════════════
    # HISTORICAL AVERAGE - Smooth fitted curve across all historical data
    # Uses loess regression to create a smooth reference curve instead of
    # per-bond averages (which create scattered points)
    # ════════════════════════════════════════════════════════════════════════

    # Step 1: Collect all historical data points for current bonds
    hist_data <- data %>%
        filter(bond %in% current_curve$bond,
               !is.na(yield_to_maturity),
               yield_to_maturity > 1.5,
               yield_to_maturity < 25,
               modified_duration > 0.5) %>%
        select(date, bond, modified_duration, yield_to_maturity)

    # Step 2: Fit loess curve to all historical points
    avg_curve <- data.frame()  # Default empty

    if (nrow(hist_data) >= 10) {
        tryCatch({
            # Fit loess model to historical data
            hist_loess <- loess(
                yield_to_maturity ~ modified_duration,
                data = hist_data,
                span = 0.6,  # Smoothness parameter
                degree = 2   # Quadratic local regression
            )

            # Generate prediction grid spanning current curve's duration range
            duration_range <- range(current_curve$modified_duration, na.rm = TRUE)
            duration_grid <- seq(
                duration_range[1],
                duration_range[2],
                length.out = 100
            )

            # Predict smooth curve values
            predicted_ytm <- predict(hist_loess, newdata = data.frame(modified_duration = duration_grid))

            # Create smooth curve data frame
            avg_curve <- data.frame(
                modified_duration = duration_grid,
                yield_to_maturity = predicted_ytm,
                bond = NA_character_  # Placeholder for bind_rows compatibility
            ) %>%
                filter(!is.na(yield_to_maturity)) %>%
                mutate(curve_type = "Hist. Avg",
                       curve_date = as.Date(NA))

            message(sprintf("Hist. Avg: Fitted loess curve from %d historical points, generating %d smooth points",
                            nrow(hist_data), nrow(avg_curve)))
        }, error = function(e) {
            message("Warning: Could not fit loess for historical average: ", e$message)
            avg_curve <<- data.frame()
        })
    }

    # Combine all curves
    curves_list <- list(current_curve)
    if (nrow(curve_1m) >= 3) curves_list <- c(curves_list, list(curve_1m))
    if (nrow(curve_3m) >= 3) curves_list <- c(curves_list, list(curve_3m))
    if (nrow(avg_curve) >= 3) curves_list <- c(curves_list, list(avg_curve))

    all_curves <- bind_rows(curves_list) %>%
        mutate(
            curve_type = factor(curve_type,
                                levels = c("Current", "1M Ago", "3M Ago", "Hist. Avg")),
            # Flag if bond is currently active
            is_active = bond %in% current_curve$bond
        )

    # X-axis range based on CURRENT curve only (focus area)
    x_min <- max(0, min(current_curve$modified_duration) - 0.5)
    x_max <- max(current_curve$modified_duration) + 0.5

    # Separate smooth curves and historical average line
    smooth_curves <- all_curves %>% filter(curve_type != "Hist. Avg")

    p <- ggplot(mapping = aes(x = modified_duration, y = yield_to_maturity)) +

        # Historical average as simple dotted line (no points to avoid visual clutter)
        geom_line(data = avg_curve,
                  aes(color = "Hist. Avg"),
                  linewidth = 1.2, linetype = "dotted") +

        # 3M Ago curve - smoothed but only through its own points
        {if(nrow(curve_3m) >= 3) geom_smooth(data = curve_3m,
                    aes(color = curve_type),
                    method = "loess", formula = y ~ x, se = FALSE,
                    span = 1.2, linewidth = 0.9, linetype = "longdash",
                    fullrange = FALSE, na.rm = TRUE)} +

        # 1M Ago curve
        {if(nrow(curve_1m) >= 3) geom_smooth(data = curve_1m,
                    aes(color = curve_type),
                    method = "loess", formula = y ~ x, se = FALSE,
                    span = 1.2, linewidth = 1.1, linetype = "solid",
                    fullrange = FALSE, na.rm = TRUE)} +

        # Current curve - most prominent
        geom_smooth(data = current_curve,
                    aes(color = curve_type),
                    method = "loess", formula = y ~ x, se = FALSE,
                    span = 1.0, linewidth = 1.8, linetype = "solid",
                    fullrange = FALSE, na.rm = TRUE) +

        # Points for current curve only (emphasized)
        geom_point(data = current_curve, aes(color = curve_type),
                   size = 3, show.legend = FALSE) +

        # Bond labels for current curve using ggrepel
        # Optimized for crowded duration clusters (especially 7-8y area)
        ggrepel::geom_text_repel(
            data = current_curve,
            aes(label = bond),
            size = 2.5,
            color = "gray25",
            fontface = "bold",
            box.padding = 0.5,
            point.padding = 0.4,
            segment.color = "gray50",
            segment.size = 0.3,
            min.segment.length = 0.2,
            max.overlaps = 25,
            force = 4,
            force_pull = 0.3,
            direction = "both",
            nudge_y = 0.1,
            seed = 42,
            show.legend = FALSE
        ) +

        scale_color_manual(
            values = c(
                "Current" = insele_palette$primary,
                "1M Ago" = "#F39C12",
                "3M Ago" = "#95A5A6",
                "Hist. Avg" = "#E74C3C"
            ),
            name = NULL,
            breaks = c("Current", "1M Ago", "3M Ago", "Hist. Avg")
        ) +

        scale_x_continuous(
            limits = c(x_min, x_max),
            breaks = seq(0, ceiling(x_max), by = 2),
            expand = c(0.01, 0.01)
        ) +

        scale_y_continuous(
            labels = function(x) paste0(x, "%"),
            expand = expansion(mult = c(0.02, 0.05))
        ) +

        labs(
            title = "Yield Curve Evolution",
            subtitle = sprintf("Current vs historical snapshots | As of %s",
                               format(max_date, "%d %b %Y")),
            x = "Modified Duration (years)",
            y = "YTM (%)",
            caption = "Solid = recent | Dashed = 3M ago | Dotted = historical average (current bonds only)"
        ) +

        create_insele_theme() +
        theme(
            legend.position = c(0.02, 0.98),
            legend.justification = c(0, 1),
            legend.background = element_rect(fill = alpha("white", 0.9), color = NA),
            legend.key.width = unit(1.5, "cm"),
            legend.key.height = unit(0.4, "cm"),
            legend.text = element_text(size = 9),
            legend.spacing.y = unit(0.2, "cm"),
            plot.caption = element_text(size = 7, color = "gray50", hjust = 0)
        )

    return(p)
}


#' Generate Curve Steepness Gauge
#'
#' Bullet gauge showing 2s10s spread position within historical range.
#' Answers: "How steep is the curve? Is this extreme or normal?"
#'
#' @param data Bond data with date, yield_to_maturity, modified_duration
#' @param params Additional parameters (unused)
#' @return ggplot object
#' @export
generate_curve_steepness_gauge <- function(data, params) {

    # Validate data
    if (is.null(data) || nrow(data) < 30) {
        return(NULL)
    }

    # Ensure required columns exist
    required_cols <- c("date", "yield_to_maturity", "modified_duration")
    if (!all(required_cols %in% names(data))) {
        return(NULL)
    }

    # Filter valid data
    data <- data %>%
        filter(
            !is.na(yield_to_maturity),
            !is.na(modified_duration),
            is.finite(yield_to_maturity),
            is.finite(modified_duration),
            yield_to_maturity > 1.5,
            modified_duration > 0.5
        )

    if (nrow(data) < 30) return(NULL)

    # Calculate 2s10s spread over time
    spread_history <- data %>%
        group_by(date) %>%
        summarise(
            yield_2y = mean(yield_to_maturity[modified_duration >= 1.5 & modified_duration <= 3], na.rm = TRUE),
            yield_10y = mean(yield_to_maturity[modified_duration >= 8 & modified_duration <= 12], na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(spread_2s10s = (yield_10y - yield_2y) * 100) %>%
        filter(!is.na(spread_2s10s), is.finite(spread_2s10s))

    if (nrow(spread_history) < 10) return(NULL)

    # Current value and statistics
    current_spread <- spread_history %>%
        filter(date == max(date)) %>%
        pull(spread_2s10s)

    if (length(current_spread) == 0 || is.na(current_spread)) return(NULL)

    spread_min <- min(spread_history$spread_2s10s, na.rm = TRUE)
    spread_max <- max(spread_history$spread_2s10s, na.rm = TRUE)
    spread_avg <- mean(spread_history$spread_2s10s, na.rm = TRUE)
    spread_q25 <- quantile(spread_history$spread_2s10s, 0.25, na.rm = TRUE)
    spread_q75 <- quantile(spread_history$spread_2s10s, 0.75, na.rm = TRUE)
    spread_percentile <- sum(spread_history$spread_2s10s <= current_spread) / nrow(spread_history) * 100

    # Determine classification
    steepness_class <- case_when(
        current_spread < 0 ~ "Inverted",
        current_spread < 50 ~ "Flat",
        current_spread < 100 ~ "Moderately Flat",
        current_spread < 200 ~ "Normal",
        current_spread < 300 ~ "Steep",
        TRUE ~ "Very Steep"
    )

    # Create gauge visualization
    # Add padding to range
    range_padding <- (spread_max - spread_min) * 0.1
    x_min <- spread_min - range_padding
    x_max <- spread_max + range_padding

    p <- ggplot() +

        # Background range bar
        geom_rect(aes(xmin = x_min, xmax = x_max, ymin = 0.7, ymax = 1.3),
                  fill = "gray95") +

        # Quartile zones
        geom_rect(aes(xmin = spread_min, xmax = spread_q25,
                      ymin = 0.72, ymax = 1.28),
                  fill = "#E8F8F5", alpha = 0.9) +
        geom_rect(aes(xmin = spread_q25, xmax = spread_q75,
                      ymin = 0.72, ymax = 1.28),
                  fill = "#D5F4E6", alpha = 0.9) +
        geom_rect(aes(xmin = spread_q75, xmax = spread_max,
                      ymin = 0.72, ymax = 1.28),
                  fill = "#FCF3CF", alpha = 0.9) +

        # Average line (vertical)
        geom_segment(aes(x = spread_avg, xend = spread_avg, y = 0.6, yend = 1.4),
                     linetype = "dashed", color = "gray40", linewidth = 0.8) +

        # Average label - position ABOVE the gauge, centered on the line
        annotate("text",
                 x = spread_avg,
                 y = 1.55,
                 label = sprintf("Avg: %.0f", spread_avg),
                 size = 3,
                 color = "gray40",
                 fontface = "italic",
                 hjust = 0.5) +

        # Current value marker (thick bar)
        geom_segment(aes(x = current_spread, xend = current_spread, y = 0.82, yend = 1.18),
                     color = "#1B3A6B", linewidth = 5) +

        # Current value label - moved below the gauge for cleaner look
        annotate("text", x = current_spread, y = -0.1,
                 label = sprintf("%.0f bps (%s)", current_spread, steepness_class),
                 size = 4.5, fontface = "bold", color = "#1B3A6B") +

        # Scale markers
        scale_x_continuous(
            breaks = c(spread_min, spread_q25, spread_avg, spread_q75, spread_max),
            labels = c(sprintf("%.0f\n(Min)", spread_min),
                       sprintf("%.0f\n(25%%)", spread_q25),
                       sprintf("%.0f\n(Avg)", spread_avg),
                       sprintf("%.0f\n(75%%)", spread_q75),
                       sprintf("%.0f\n(Max)", spread_max))
        ) +

        coord_cartesian(ylim = c(-0.3, 1.7), xlim = c(x_min, x_max)) +

        labs(
            title = "Curve Steepness: 2s10s Spread",
            subtitle = sprintf("Position within historical range | Currently at %.0f%% percentile",
                               spread_percentile),
            x = "Spread (basis points)",
            y = NULL
        ) +

        theme_minimal() +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 11, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 9, color = "gray50"),
            plot.margin = ggplot2::margin(10, 15, 5, 15)
        )

    return(p)
}


#' Generate Market Summary Metrics
#'
#' Calculate key summary metrics for the market intelligence panel.
#' Returns a list of metrics for use in value boxes.
#'
#' @param data Bond data with date, yield_to_maturity, modified_duration
#' @param params Additional parameters (unused)
#' @return List of metrics
#' @export
generate_market_summary_metrics <- function(data, params) {

    # Default metrics in case of insufficient data
    default_metrics <- list(
        avg_yield = NA_real_,
        yield_change_1m = NA_real_,
        curve_slope = NA_real_,
        yield_range = NA_real_,
        valid = FALSE
    )

    # Validate data
    if (is.null(data) || nrow(data) < 30) {
        return(default_metrics)
    }

    # Ensure required columns exist
    required_cols <- c("date", "yield_to_maturity", "modified_duration")
    if (!all(required_cols %in% names(data))) {
        return(default_metrics)
    }

    # Filter valid data
    data <- data %>%
        filter(
            !is.na(yield_to_maturity),
            !is.na(modified_duration),
            is.finite(yield_to_maturity),
            is.finite(modified_duration),
            yield_to_maturity > 1.5,
            modified_duration > 0.5
        )

    if (nrow(data) < 30) return(default_metrics)

    max_date <- max(data$date, na.rm = TRUE)

    # Current curve metrics
    current <- data %>% filter(date == max_date)

    if (nrow(current) < 3) return(default_metrics)

    # Calculate key metrics
    tryCatch({
        # Average yield level
        avg_yield <- mean(current$yield_to_maturity, na.rm = TRUE)

        # Yield range (dispersion)
        yield_range <- max(current$yield_to_maturity, na.rm = TRUE) -
                       min(current$yield_to_maturity, na.rm = TRUE)

        # 1-month change in average yield
        date_1m_ago <- max_date - 30
        data_1m_ago <- data %>%
            filter(date <= date_1m_ago) %>%
            filter(date == max(date))

        avg_yield_1m_ago <- if (nrow(data_1m_ago) >= 3) {
            mean(data_1m_ago$yield_to_maturity, na.rm = TRUE)
        } else {
            NA_real_
        }

        yield_change_1m <- if (!is.na(avg_yield_1m_ago)) {
            (avg_yield - avg_yield_1m_ago) * 100  # Convert to bps
        } else {
            NA_real_
        }

        # Curve slope (10y - 2y approximation using modified duration)
        short_yield <- mean(current$yield_to_maturity[current$modified_duration <= 3], na.rm = TRUE)
        long_yield <- mean(current$yield_to_maturity[current$modified_duration >= 8], na.rm = TRUE)

        curve_slope <- if (!is.na(short_yield) && !is.na(long_yield)) {
            (long_yield - short_yield) * 100  # Convert to bps
        } else {
            NA_real_
        }

        list(
            avg_yield = avg_yield,
            yield_change_1m = yield_change_1m,
            curve_slope = curve_slope,
            yield_range = yield_range * 100,  # Convert to bps for display
            valid = TRUE
        )
    }, error = function(e) {
        default_metrics
    })
}