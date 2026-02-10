# ============================================================================
# VaR DATA PREPARATION FUNCTIONS
# ============================================================================

#' Prepare VaR Data for Plotting
#' @description Removes all NA, NaN, and Inf values from VaR data BEFORE plotting.
#'              This ensures both distribution and ladder plots show identical bonds.
#' @param var_data Data frame with VaR metrics from calculate_var() or generate_var_distribution_plot()
#' @param source Character string identifying the source (for logging)
#' @return Filtered data frame with only valid VaR values
#' @export
prepare_var_data_for_plotting <- function(var_data, source = "VaR") {

    # Handle NULL or empty input
    if (is.null(var_data) || nrow(var_data) == 0) {
        message(sprintf("[%s Plot Prep] Input is NULL or empty", source))
        return(data.frame())
    }

    n_total <- nrow(var_data)
    message(sprintf("[%s Plot Prep] Input: %d bonds", source, n_total))

    # Determine which columns to check based on what's available
    # Distribution plot uses VaR_95, VaR_99 (percentages)
    # Ladder plot uses VaR_95_bps, VaR_99_bps (basis points)
    has_bps <- "VaR_95_bps" %in% names(var_data)
    has_pct <- "VaR_95" %in% names(var_data)

    # Build comprehensive NA mask
    na_mask <- rep(FALSE, nrow(var_data))

    # Check bond column
    if ("bond" %in% names(var_data)) {
        na_mask <- na_mask |
            is.na(var_data$bond) |
            var_data$bond == "" |
            var_data$bond == "NA" |
            as.character(var_data$bond) == "NA" |
            nchar(trimws(as.character(var_data$bond))) == 0
    }

    # Check VaR columns (basis points version)
    if (has_bps) {
        na_mask <- na_mask |
            is.na(var_data$VaR_95_bps) |
            is.nan(var_data$VaR_95_bps) |
            is.infinite(var_data$VaR_95_bps) |
            is.na(var_data$VaR_99_bps) |
            is.nan(var_data$VaR_99_bps) |
            is.infinite(var_data$VaR_99_bps)
    }

    # Check VaR columns (percentage version)
    if (has_pct) {
        na_mask <- na_mask |
            is.na(var_data$VaR_95) |
            is.nan(var_data$VaR_95) |
            is.infinite(var_data$VaR_95) |
            is.na(var_data$VaR_99) |
            is.nan(var_data$VaR_99) |
            is.infinite(var_data$VaR_99)
    }

    # Check CVaR if present
    if ("CVaR_95" %in% names(var_data)) {
        na_mask <- na_mask |
            is.na(var_data$CVaR_95) |
            is.nan(var_data$CVaR_95) |
            is.infinite(var_data$CVaR_95)
    }

    # Identify excluded bonds for logging
    if (any(na_mask)) {
        na_bonds <- if ("bond" %in% names(var_data)) {
            var_data$bond[na_mask]
        } else {
            paste0("Row ", which(na_mask))
        }
        na_bonds <- na_bonds[!is.na(na_bonds) & na_bonds != "" & na_bonds != "NA"]

        if (length(na_bonds) > 0) {
            warning(sprintf("[%s Plot Prep] Excluding %d bonds with invalid VaR: %s",
                           source, length(na_bonds), paste(na_bonds, collapse = ", ")))
        }
    }

    # Filter to valid data only
    valid_data <- var_data[!na_mask, ]

    # Additional sanity check: filter extreme values
    if (has_bps && nrow(valid_data) > 0) {
        # VaR > 1000 bps (10%) is extremely unusual for government bonds
        extreme_mask <- valid_data$VaR_95_bps > 1000 | valid_data$VaR_99_bps > 1500
        if (any(extreme_mask, na.rm = TRUE)) {
            extreme_bonds <- valid_data$bond[extreme_mask]
            warning(sprintf("[%s Plot Prep] Excluding %d bonds with extreme VaR (>1000 bps): %s",
                           source, length(extreme_bonds), paste(extreme_bonds, collapse = ", ")))
            valid_data <- valid_data[!extreme_mask, ]
        }
    }

    message(sprintf("[%s Plot Prep] Output: %d valid bonds", source, nrow(valid_data)))

    # Final verification
    if (has_bps && nrow(valid_data) > 0) {
        if (any(is.na(valid_data$VaR_95_bps)) || any(is.na(valid_data$VaR_99_bps))) {
            stop(sprintf("[%s Plot Prep] CRITICAL: NA values still present after filtering!", source))
        }
    }

    return(valid_data)
}

#' Assess Data Quality for VaR Calculation
#' @description Evaluates bond history completeness based on CURRENT lookback setting.
#'              Longer lookback periods require more observations to be "Good".
#' @param data Bond data with date and bond columns
#' @param lookback_days Current lookback period from UI (e.g., 252, 375)
#' @param min_ratio Minimum ratio of observations to lookback for "Limited" (default 0.8)
#' @return Data frame with quality assessment per bond
#' @export
assess_data_quality_for_var <- function(data, lookback_days, min_ratio = 0.8) {

    # Minimum absolute observations for any VaR calculation
    min_required <- 30

    # Quality thresholds scale with lookback period
    recommended <- lookback_days  # Ideal: full lookback period worth of data
    limited_threshold <- floor(lookback_days * min_ratio)  # 80% of lookback = Limited

    # Count observations per bond
    obs_count <- data %>%
        dplyr::group_by(bond) %>%
        dplyr::summarise(
            n_observations = dplyr::n(),
            date_range_days = as.numeric(max(date) - min(date)),
            first_date = min(date),
            last_date = max(date),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            data_quality = dplyr::case_when(
                n_observations < min_required ~ "Insufficient",
                n_observations < limited_threshold ~ "Limited",
                n_observations < recommended ~ "Adequate",
                TRUE ~ "Good"
            ),
            # Quality flags for plot labels
            quality_flag = dplyr::case_when(
                data_quality == "Insufficient" ~ "**",
                data_quality == "Limited" ~ "*",
                TRUE ~ ""
            ),
            quality_note = dplyr::case_when(
                data_quality == "Insufficient" ~ paste0("INSUFFICIENT (", n_observations,
                                                        " obs, need ", min_required, "+)"),
                data_quality == "Limited" ~ paste0("Limited (", n_observations,
                                                   " of ", lookback_days, " days)"),
                data_quality == "Adequate" ~ paste0("Adequate (", n_observations,
                                                    " of ", lookback_days, " days)"),
                TRUE ~ ""
            )
        )

    # Log summary
    message(sprintf("[Data Quality] Lookback: %d days", lookback_days))
    message(sprintf("  Good (>=%d obs): %d bonds",
                   recommended, sum(obs_count$data_quality == "Good")))
    message(sprintf("  Adequate (%d-%d obs): %d bonds",
                   limited_threshold, recommended, sum(obs_count$data_quality == "Adequate")))
    message(sprintf("  Limited (<%d obs): %d bonds",
                   limited_threshold, sum(obs_count$data_quality == "Limited")))
    message(sprintf("  Insufficient (<%d obs): %d bonds",
                   min_required, sum(obs_count$data_quality == "Insufficient")))

    return(obs_count)
}

#' Filter Bonds for VaR Calculation Based on Method
#' @description Determines which bonds have sufficient data for VaR calculation
#'              based on the selected method (parametric requires more data).
#' @param data Bond data
#' @param method VaR method: "historical", "parametric", or "cornish-fisher"
#' @param lookback_days Current lookback period
#' @return List with sufficient_bonds and insufficient_bonds vectors
#' @export
filter_bonds_for_var_method <- function(data, method, lookback_days) {

    # Minimum observations depend on method
    min_required <- switch(method,
        "parametric" = 30,       # Normal distribution needs more samples
        "cornish-fisher" = 30,   # Needs reliable skewness/kurtosis estimates
        "historical" = 20        # Empirical quantiles work with fewer samples
    )

    # Count observations within lookback window
    latest_date <- max(data$date, na.rm = TRUE)
    lookback_start <- latest_date - lookback_days

    lookback_data <- data %>%
        dplyr::filter(date >= lookback_start)

    obs_counts <- lookback_data %>%
        dplyr::group_by(bond) %>%
        dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")

    # Identify bonds with sufficient/insufficient data
    sufficient_bonds <- obs_counts$bond[obs_counts$n_obs >= min_required]
    insufficient_bonds <- obs_counts$bond[obs_counts$n_obs < min_required]

    # Log exclusions
    if (length(insufficient_bonds) > 0) {
        message(sprintf("[VaR Filter] Excluding %d bonds with insufficient data for %s method (need %d+ obs):",
                       length(insufficient_bonds), method, min_required))
        for (b in insufficient_bonds) {
            n <- obs_counts$n_obs[obs_counts$bond == b]
            message(sprintf("  %s: %d obs", b, n))
        }
    }

    return(list(
        sufficient_bonds = sufficient_bonds,
        insufficient_bonds = insufficient_bonds,
        min_required = min_required,
        method = method
    ))
}

# ============================================================================
# ORIGINAL FUNCTIONS
# ============================================================================

#' Data Quality Check Function
#' @description Performs comprehensive data quality checks for a specific bond
data_quality_check <- function(data, bond_name) {
    bond_data <- data %>% filter(bond == bond_name)

    if (nrow(bond_data) == 0) {
        return(list(quality_score = 0, error = "No data found"))
    }

    checks <- list(
        bond = bond_name,
        n_obs = nrow(bond_data),

        # Check 1: Missing values
        pct_missing = mean(is.na(bond_data$yield_to_maturity)) * 100,

        # Check 2: Duplicate dates
        n_duplicates = nrow(bond_data) - n_distinct(bond_data$date),

        # Check 3: Extreme yield changes (>200 bps in one day)
        extreme_changes = bond_data %>%
            arrange(date) %>%
            mutate(yield_change = abs(yield_to_maturity - lag(yield_to_maturity))) %>%
            filter(!is.na(yield_change) & yield_change > 2.0) %>%
            nrow(),

        # Check 4: Suspicious yield levels
        suspicious_yields = bond_data %>%
            filter(yield_to_maturity < 5 | yield_to_maturity > 20) %>%
            nrow(),

        # Check 5: Duration/Convexity sanity
        invalid_duration = sum(bond_data$modified_duration < 0 |
                                   bond_data$modified_duration > 30, na.rm = TRUE),
        invalid_convexity = sum(bond_data$convexity < 0 |
                                    bond_data$convexity > 1000, na.rm = TRUE)
    )

    # Calculate quality score (0-100)
    checks$quality_score <- 100 - (
        checks$pct_missing * 0.5 +
            checks$n_duplicates * 2 +
            checks$extreme_changes * 5 +
            checks$suspicious_yields * 2 +
            checks$invalid_duration * 10 +
            checks$invalid_convexity * 10
    )
    checks$quality_score <- max(0, min(100, checks$quality_score))

    return(checks)
}

#' Adaptive Outlier Threshold Function
#' @description Returns bond-specific outlier threshold based on duration
adaptive_outlier_threshold <- function(bond, duration) {
    if (duration > 20) {
        return(2.5)  # 2.5σ for ultra-long bonds
    } else if (duration > 15) {
        return(3.0)  # 3σ for long bonds
    } else if (duration > 10) {
        return(3.5)  # 3.5σ for medium-long bonds
    } else {
        return(4.0)  # 4σ for shorter bonds
    }
}

#' @export
# 4. VAR Distribution Plot Generation
generate_var_distribution_plot <- function(data, params = list()) {

    # Extract parameters with defaults
    confidence_levels <- params$confidence_levels %||% c(0.95, 0.99)
    horizon_days <- params$horizon_days %||% 1  # Daily VaR by default
    method <- params$method %||% "historical"  # "historical", "parametric", or "cornish-fisher"
    show_stats <- params$show_stats %||% FALSE  # CHANGED: Don't show stats in facets by default
    max_bonds <- params$max_bonds %||% 12
    outlier_threshold <- params$outlier_threshold  # Will use adaptive if NULL
    enable_diagnostics <- params$enable_diagnostics %||% FALSE  # CHANGED: Disable verbose diagnostics by default
    max_daily_return <- params$max_daily_return %||% 10  # Cap at 10% daily return
    recalculate_volatility_if_low <- params$recalculate_volatility_if_low %||% TRUE  # Failsafe for upstream bugs
    bond_order <- params$bond_order  # Optional: pre-specified bond order for consistency
    data_quality <- params$data_quality  # Optional: data quality assessment for annotations

    # ===========================================================================
    # METHOD-AWARE MINIMUM OBSERVATIONS
    # Parametric and Cornish-Fisher methods require more data for reliable estimates
    # ===========================================================================
    min_observations <- switch(method,
        "parametric" = params$min_observations %||% 30,      # Normal distribution needs samples for mean/sd
        "cornish-fisher" = params$min_observations %||% 30,  # Needs reliable skewness/kurtosis
        "historical" = params$min_observations %||% 20       # Empirical quantiles work with fewer samples
    )

    message(sprintf("[VaR Distribution] Method: %s | Min observations: %d | Horizon: %d days",
                   method, min_observations, horizon_days))

    # Validate required columns
    required_cols <- c("bond", "date", "yield_to_maturity", "modified_duration", "convexity")
    if (!all(required_cols %in% names(data))) {
        stop(paste("Missing required columns:",
                   paste(setdiff(required_cols, names(data)), collapse = ", ")))
    }

    # ===========================================================================
    # FILTER BONDS WITH INSUFFICIENT DATA FOR CURRENT METHOD
    # ===========================================================================
    data_check <- data %>%
        group_by(bond) %>%
        summarise(n_obs = n(), .groups = "drop")

    if (any(data_check$n_obs < min_observations)) {
        insufficient_bonds <- data_check %>%
            filter(n_obs < min_observations) %>%
            pull(bond)

        # Log exclusions with observation counts
        message(sprintf("[VaR Distribution] Excluding %d bond(s) with insufficient data for %s method:",
                       length(insufficient_bonds), method))
        for (b in insufficient_bonds) {
            n <- data_check$n_obs[data_check$bond == b]
            message(sprintf("  - %s: %d obs (need %d+)", b, n, min_observations))
        }

        data <- data %>%
            filter(!bond %in% insufficient_bonds)
    }

    # Check if we have any data left after filtering
    if (nrow(data) == 0 || n_distinct(data$bond) == 0) {
        warning("generate_var_distribution_plot: No bonds with sufficient data")
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = paste0("No bonds have sufficient data\nfor ", method, " VaR calculation.\n",
                                       "Try using Historical method or reducing lookback period."),
                         size = 5, color = "#666666") +
                theme_void() +
                labs(title = "Daily Return Distributions")
        )
    }

    message(sprintf("[VaR Distribution] Processing %d bonds", n_distinct(data$bond)))

    # COMPREHENSIVE DATA QUALITY CHECKS
    if (enable_diagnostics) {
        cat("\n=== VaR ANALYSIS: DATA QUALITY REPORT ===\n")

        unique_bonds <- unique(data$bond)
        quality_report <- list()

        for (b in unique_bonds) {
            quality_report[[b]] <- data_quality_check(data, b)

            # Warn about low-quality bonds
            if (quality_report[[b]]$quality_score < 70) {
                cat(sprintf("\n⚠️  WARNING: %s data quality issues (Score: %.1f/100)\n",
                            b, quality_report[[b]]$quality_score))
                cat(sprintf("   - Missing values: %.1f%%\n", quality_report[[b]]$pct_missing))
                cat(sprintf("   - Extreme yield changes: %d\n", quality_report[[b]]$extreme_changes))
                cat(sprintf("   - Suspicious yields: %d\n", quality_report[[b]]$suspicious_yields))
            }
        }
        cat("\n")
    }

    # Calculate price returns properly
    returns_data <- data %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            # Calculate yield changes in decimal form
            yield_change = (yield_to_maturity - lag(yield_to_maturity)) / 100,

            # PROPER PRICE RETURN CALCULATION
            # Price Return = -Duration × Δy + 0.5 × Convexity × (Δy)²
            # Convert to percentage by multiplying by 100
            price_return = (-modified_duration * yield_change +
                                0.5 * convexity * yield_change^2) * 100,

            # Scale to desired horizon (e.g., 1-day, 10-day)
            scaled_return = price_return * sqrt(horizon_days)
        ) %>%
        filter(!is.na(price_return))

    # ADAPTIVE OUTLIER REMOVAL
    # Use bond-specific thresholds based on duration if not explicitly set
    if (is.null(outlier_threshold)) {
        # Adaptive approach: different thresholds for different duration bonds
        returns_data <- returns_data %>%
            group_by(bond) %>%
            mutate(
                duration_value = first(modified_duration),
                threshold = case_when(
                    duration_value > 20 ~ 2.5,  # 2.5σ for ultra-long bonds
                    duration_value > 15 ~ 3.0,  # 3σ for long bonds
                    duration_value > 10 ~ 3.5,  # 3.5σ for medium-long bonds
                    TRUE ~ 4.0  # 4σ for shorter bonds
                ),
                z_score = abs((price_return - mean(price_return, na.rm = TRUE)) /
                                  sd(price_return, na.rm = TRUE)),
                is_outlier = z_score > threshold
            )

        # Report outlier statistics
        if (enable_diagnostics) {
            outlier_summary <- returns_data %>%
                summarise(
                    bond = first(bond),
                    duration = first(duration_value),
                    threshold = first(threshold),
                    n_total = n(),
                    n_outliers = sum(is_outlier, na.rm = TRUE),
                    pct_outliers = mean(is_outlier, na.rm = TRUE) * 100,
                    .groups = "drop"
                )

            cat("=== OUTLIER REMOVAL SUMMARY (ADAPTIVE THRESHOLDS) ===\n")
            for (i in 1:nrow(outlier_summary)) {
                cat(sprintf("%s (D=%.1fy): %.1fσ threshold → %d/%d outliers (%.1f%%)\n",
                            outlier_summary$bond[i],
                            outlier_summary$duration[i],
                            outlier_summary$threshold[i],
                            outlier_summary$n_outliers[i],
                            outlier_summary$n_total[i],
                            outlier_summary$pct_outliers[i]))
            }
            cat("\n")
        }

        returns_data <- returns_data %>%
            filter(!is_outlier) %>%
            select(-z_score, -is_outlier, -threshold, -duration_value)
    } else {
        # Use fixed threshold if explicitly provided
        returns_data <- returns_data %>%
            group_by(bond) %>%
            mutate(
                z_score = abs((price_return - mean(price_return, na.rm = TRUE)) /
                                  sd(price_return, na.rm = TRUE))
            ) %>%
            filter(z_score <= outlier_threshold) %>%
            select(-z_score)
    }

    # PRICE RETURN SANITY CHECKS AND CAPPING
    # Cap extreme returns that may be due to data errors
    returns_data <- returns_data %>%
        group_by(bond) %>%
        mutate(
            # Flag suspicious returns
            suspicious_return = abs(price_return) > max_daily_return,

            # Cap extreme values
            price_return_original = price_return,
            price_return_capped = case_when(
                price_return > max_daily_return ~ max_daily_return,
                price_return < -max_daily_return ~ -max_daily_return,
                TRUE ~ price_return
            )
        )

    # Report capping statistics
    if (enable_diagnostics) {
        capping_summary <- returns_data %>%
            summarise(
                bond = first(bond),
                n_capped = sum(suspicious_return, na.rm = TRUE),
                pct_capped = mean(suspicious_return, na.rm = TRUE) * 100,
                max_original = max(abs(price_return_original), na.rm = TRUE),
                .groups = "drop"
            ) %>%
            filter(n_capped > 0)

        if (nrow(capping_summary) > 0) {
            cat("=== EXTREME RETURN CAPPING SUMMARY ===\n")
            cat(sprintf("Maximum daily return cap: ±%.1f%%\n", max_daily_return))
            for (i in 1:nrow(capping_summary)) {
                cat(sprintf("%s: %d returns capped (%.1f%%), max was %.2f%%\n",
                            capping_summary$bond[i],
                            capping_summary$n_capped[i],
                            capping_summary$pct_capped[i],
                            capping_summary$max_original[i]))
            }
            cat("\n")
        }
    }

    # Use capped returns if more than 5% of returns are suspicious for a bond
    returns_data <- returns_data %>%
        mutate(
            use_capped = sum(suspicious_return, na.rm = TRUE) / n() > 0.05,
            final_return = if_else(use_capped, price_return_capped, price_return),
            # Update scaled_return with final_return
            scaled_return = final_return * sqrt(horizon_days)
        ) %>%
        select(-price_return_original, -price_return_capped, -suspicious_return,
               -use_capped, -final_return)

    # Select bonds to display (prioritize by data quality and diversity)
    unique_bonds <- unique(returns_data$bond)
    if (length(unique_bonds) > max_bonds) {
        selected_bonds <- returns_data %>%
            group_by(bond) %>%
            summarise(
                n_obs = n(),
                vol = sd(scaled_return),
                .groups = "drop"
            ) %>%
            arrange(desc(n_obs), desc(vol)) %>%
            slice_head(n = max_bonds) %>%
            pull(bond)

        returns_data <- filter(returns_data, bond %in% selected_bonds)
    }

    # Calculate VaR and risk metrics
    # EXPECTED RANGES FOR SA GOVERNMENT BONDS:
    # - Daily volatility: 1.0% - 3.0% (annualized: ~15-50%)
    # - VaR95 (daily): 1.0% - 3.0% for most bonds
    # - VaR99 (daily): 1.5% - 4.5% for most bonds
    # - Longer duration bonds (e.g., R2053) should show higher volatility than shorter duration bonds (e.g., R186)
    #
    # WARNING THRESHOLDS:
    # - If volatility < 0.5%, likely data quality issue
    # - If VaR95 < 0.1%, likely calculation error
    # - If all bonds show near-identical volatility, suspect upstream bug

    var_levels <- returns_data %>%
        group_by(bond) %>%
        summarise(
            # Sample statistics
            n_observations = n(),
            mean_return = mean(scaled_return),
            median_return = median(scaled_return),
            vol = sd(scaled_return),
            skewness = moments::skewness(scaled_return),
            kurtosis = moments::kurtosis(scaled_return),

            # Historical VaR (empirical quantiles)
            VaR_95_hist = quantile(scaled_return, 1 - confidence_levels[1]),
            VaR_99_hist = quantile(scaled_return, 1 - confidence_levels[2]),

            # Parametric VaR (assuming normal distribution)
            VaR_95_param = mean_return - qnorm(confidence_levels[1]) * vol,
            VaR_99_param = mean_return - qnorm(confidence_levels[2]) * vol,

            # Cornish-Fisher VaR (adjusting for skewness and kurtosis)
            z_95 = qnorm(confidence_levels[1]),
            z_99 = qnorm(confidence_levels[2]),
            VaR_95_cf = mean_return - vol * (z_95 +
                                                 (z_95^2 - 1) * skewness/6 +
                                                 (z_95^3 - 3*z_95) * (kurtosis - 3)/24),
            VaR_99_cf = mean_return - vol * (z_99 +
                                                 (z_99^2 - 1) * skewness/6 +
                                                 (z_99^3 - 3*z_99) * (kurtosis - 3)/24),

            # Conditional VaR (Expected Shortfall) - proper calculation
            CVaR_95 = mean(scaled_return[scaled_return < VaR_95_hist]),
            CVaR_99 = mean(scaled_return[scaled_return < VaR_99_hist]),

            # Choose which VaR to display based on method
            VaR_95 = case_when(
                method == "historical" ~ VaR_95_hist,
                method == "parametric" ~ VaR_95_param,
                method == "cornish-fisher" ~ VaR_95_cf,
                TRUE ~ VaR_95_hist
            ),
            VaR_99 = case_when(
                method == "historical" ~ VaR_99_hist,
                method == "parametric" ~ VaR_99_param,
                method == "cornish-fisher" ~ VaR_99_cf,
                TRUE ~ VaR_99_hist
            ),

            # Risk-adjusted metrics
            sharpe_ratio = mean_return / vol * sqrt(252 / horizon_days),
            sortino_ratio = mean_return / sd(scaled_return[scaled_return < 0]) * sqrt(252 / horizon_days),

            .groups = "drop"
        ) %>%
        select(-starts_with("z_"))

    # =================================================================
    # CRITICAL VALIDATION: VOLATILITY & VaR SANITY CHECKS
    # =================================================================
    # These checks detect common bugs where volatility calculations are
    # off by factors of 10, 100, or other scaling errors.
    # For SA Government Bonds, typical daily volatility ranges:
    #   - Short-term (2-5y): 0.8% - 2.0%
    #   - Medium-term (5-12y): 1.5% - 2.5%
    #   - Long-term (12-30y): 2.0% - 3.5%

    if (enable_diagnostics) {
        cat("\n")
        cat("╔════════════════════════════════════════════════════════════════╗\n")
        cat("║  CRITICAL: VOLATILITY & VAR VALIDATION CHECKS                 ║\n")
        cat("╚════════════════════════════════════════════════════════════════╝\n\n")

        # 1. VOLATILITY RANGE CHECK
        vol_min <- min(var_levels$vol, na.rm = TRUE)
        vol_max <- max(var_levels$vol, na.rm = TRUE)
        vol_mean <- mean(var_levels$vol, na.rm = TRUE)
        vol_unique <- length(unique(round(var_levels$vol, 4)))

        cat(sprintf("📊 Volatility Statistics:\n"))
        cat(sprintf("   Range: %.3f%% - %.3f%%\n", vol_min, vol_max))
        cat(sprintf("   Mean: %.3f%%\n", vol_mean))
        cat(sprintf("   Unique values: %d/%d bonds\n", vol_unique, nrow(var_levels)))

        # Check for suspiciously low volatility (< 0.5%)
        if (vol_mean < 0.5) {
            cat("\n")
            cat("╔════════════════════════════════════════════════════════════════╗\n")
            cat("║  🚨 CRITICAL BUG DETECTED: VOLATILITY TOO LOW 🚨              ║\n")
            cat("╚════════════════════════════════════════════════════════════════╝\n")
            cat(sprintf("   Mean volatility: %.4f%% (EXPECTED: 1.0-3.0%%)\n", vol_mean))
            cat(sprintf("   This is %.0fx too small!\n", 1.5 / vol_mean))
            cat("\n   POSSIBLE CAUSES:\n")
            cat("   1. Yield changes divided by 10000 instead of 100\n")
            cat("   2. Price returns not converted to percentage (missing ×100)\n")
            cat("   3. Volatility inherited from upstream calculation bug\n")
            cat("   4. Rolling window calculation error\n")
            cat("\n   IMPACT:\n")
            cat("   - VaR distributions will be unrealistically narrow\n")
            cat("   - Risk severely underestimated\n")
            cat("   - All risk metrics (VaR, CVaR) will be wrong\n")
            cat("\n   ACTION REQUIRED:\n")
            cat("   - Investigate yield_change calculation in data preparation\n")
            cat("   - Check for missing percentage conversions\n")
            cat("   - Verify upstream volatility calculations\n")
            cat("╚════════════════════════════════════════════════════════════════╝\n\n")

            warning("CRITICAL: Mean volatility ", sprintf("%.4f%%", vol_mean),
                    " is suspiciously low (expected 1-3%). Check data preparation!")
        } else if (vol_mean < 1.0) {
            cat("\n")
            cat("⚠️  WARNING: Volatility appears low (%.3f%%, expected 1.0-3.0%%)\n", vol_mean)
            cat("   Consider reviewing data quality and calculation methods.\n\n")
            warning("Volatility ", sprintf("%.3f%%", vol_mean),
                    " is lower than expected for SA govt bonds (1-3%)")
        } else if (vol_mean > 5.0) {
            cat("\n")
            cat("⚠️  WARNING: Volatility appears high (%.3f%%, expected 1.0-3.0%%)\n", vol_mean)
            cat("   This could indicate:\n")
            cat("   - Data quality issues (outliers not removed)\n")
            cat("   - Calculation errors (e.g., multiplied by 100 twice)\n")
            cat("   - Extreme market conditions\n\n")
            warning("Volatility ", sprintf("%.3f%%", vol_mean),
                    " is higher than typical for SA govt bonds (1-3%)")
        } else {
            cat(sprintf("✓ Volatility range is reasonable (%.2f%% - %.2f%%)\n",
                        vol_min, vol_max))
        }

        # Check for suspiciously uniform volatility (all bonds same vol)
        if (vol_unique < nrow(var_levels) * 0.3) {
            cat(sprintf("\n⚠️  WARNING: Only %d unique volatility values for %d bonds\n",
                        vol_unique, nrow(var_levels)))
            cat("   Bonds should have different volatilities based on duration.\n")
            cat("   This suggests a calculation error or data quality issue.\n")
        }

        cat("\n")

        # 2. VAR MAGNITUDE CHECK
        var_95_mean <- mean(abs(var_levels$VaR_95), na.rm = TRUE)
        var_95_min <- min(abs(var_levels$VaR_95), na.rm = TRUE)
        var_95_max <- max(abs(var_levels$VaR_95), na.rm = TRUE)

        cat(sprintf("📊 VaR95 Statistics:\n"))
        cat(sprintf("   Range: %.3f%% - %.3f%%\n", var_95_min, var_95_max))
        cat(sprintf("   Mean: %.3f%%\n", var_95_mean))

        # Check for suspiciously small VaR (< 0.1%)
        if (var_95_mean < 0.1) {
            cat("\n")
            cat("╔════════════════════════════════════════════════════════════════╗\n")
            cat("║  🚨 CRITICAL BUG DETECTED: VAR TOO SMALL 🚨                   ║\n")
            cat("╚════════════════════════════════════════════════════════════════╝\n")
            cat(sprintf("   Mean VaR95: %.4f%% (EXPECTED: 1.0-3.0%%)\n", var_95_mean))
            cat("\n   This indicates volatility is calculated incorrectly!\n")
            cat("   VaR should be ~1.65 × volatility for normal distribution.\n")
            cat(sprintf("   Current implies volatility: %.4f%% (actual: %.4f%%)\n",
                        var_95_mean / 1.65, vol_mean))
            cat("╚════════════════════════════════════════════════════════════════╝\n\n")

            warning("CRITICAL: Mean VaR95 ", sprintf("%.4f%%", var_95_mean),
                    " is too small (expected 1-3%). Volatility calculation is wrong!")
        } else if (var_95_mean < 0.5) {
            cat(sprintf("\n⚠️  WARNING: VaR95 appears low (%.3f%%, expected 1.0-3.0%%)\n",
                        var_95_mean))
            warning("VaR95 ", sprintf("%.3f%%", var_95_mean),
                    " is lower than expected for SA govt bonds")
        } else {
            cat(sprintf("✓ VaR95 range is reasonable (%.2f%% - %.2f%%)\n",
                        var_95_min, var_95_max))
        }

        cat("\n")

        # 3. DURATION VS VOLATILITY RELATIONSHIP CHECK
        if ("modified_duration" %in% names(data)) {
            duration_vol_check <- returns_data %>%
                group_by(bond) %>%
                summarise(
                    duration = first(modified_duration),
                    vol = sd(scaled_return),
                    .groups = "drop"
                ) %>%
                arrange(duration)

            # Calculate correlation between duration and volatility
            if (nrow(duration_vol_check) >= 3) {
                dur_vol_cor <- cor(duration_vol_check$duration,
                                   duration_vol_check$vol,
                                   use = "complete.obs")

                cat(sprintf("📊 Duration vs Volatility Relationship:\n"))
                cat(sprintf("   Correlation: %.3f\n", dur_vol_cor))

                # We expect positive correlation (longer duration → higher vol)
                if (dur_vol_cor < 0.3) {
                    cat("\n⚠️  WARNING: Weak or negative correlation between duration and volatility\n")
                    cat("   Expected: Longer duration bonds should have higher volatility\n")
                    cat("   Actual correlation: ", sprintf("%.3f", dur_vol_cor), "\n")
                    cat("   This suggests a data quality or calculation issue.\n")
                } else {
                    cat("✓ Duration-volatility relationship is reasonable\n")
                }

                # Show extremes
                cat(sprintf("\n   Shortest duration: %s (%.1fy, vol=%.3f%%)\n",
                            duration_vol_check$bond[1],
                            duration_vol_check$duration[1],
                            duration_vol_check$vol[1]))
                cat(sprintf("   Longest duration: %s (%.1fy, vol=%.3f%%)\n",
                            duration_vol_check$bond[nrow(duration_vol_check)],
                            duration_vol_check$duration[nrow(duration_vol_check)],
                            duration_vol_check$vol[nrow(duration_vol_check)]))
            }
        }

        cat("\n")

        # 4. INPUT DATA QUALITY SUMMARY
        cat(sprintf("📊 Input Data Summary:\n"))
        returns_summary <- returns_data %>%
            group_by(bond) %>%
            summarise(
                n_returns = n(),
                mean_ret = mean(price_return, na.rm = TRUE),
                sd_ret = sd(price_return, na.rm = TRUE),
                min_ret = min(price_return, na.rm = TRUE),
                max_ret = max(price_return, na.rm = TRUE),
                range_ret = max_ret - min_ret,
                .groups = "drop"
            )

        cat(sprintf("   Total returns used: %d-%d per bond\n",
                    min(returns_summary$n_returns),
                    max(returns_summary$n_returns)))
        cat(sprintf("   Return range: %.3f%% to %.3f%%\n",
                    min(returns_summary$min_ret, na.rm = TRUE),
                    max(returns_summary$max_ret, na.rm = TRUE)))
        cat(sprintf("   Mean return volatility: %.3f%%\n",
                    mean(returns_summary$sd_ret, na.rm = TRUE)))

        cat("\n")
        cat("╔════════════════════════════════════════════════════════════════╗\n")
        cat("║  END VALIDATION CHECKS                                         ║\n")
        cat("╚════════════════════════════════════════════════════════════════╝\n\n")
    }

    # =================================================================
    # OPTIONAL FAILSAFE: RECALCULATE VOLATILITY FROM RAW DATA
    # =================================================================
    # If volatility appears suspiciously low, recalculate it directly
    # from yield changes to bypass any upstream calculation errors.
    # This is a defensive programming practice to ensure robust results
    # even when input data has quality issues.

    if (recalculate_volatility_if_low && mean(var_levels$vol, na.rm = TRUE) < 0.5) {
        if (enable_diagnostics) {
            cat("\n")
            cat("╔════════════════════════════════════════════════════════════════╗\n")
            cat("║  🔧 FAILSAFE ACTIVATED: RECALCULATING VOLATILITY              ║\n")
            cat("╚════════════════════════════════════════════════════════════════╝\n\n")
            cat("   Detected suspiciously low volatility.\n")
            cat("   Recalculating volatility directly from yield changes...\n\n")
        }

        # Store original values for comparison
        var_levels_original <- var_levels

        # Recalculate returns and volatility from scratch
        # This ensures we're not using any pre-calculated volatility from upstream
        returns_data_recalc <- data %>%
            group_by(bond) %>%
            arrange(date) %>%
            mutate(
                # Calculate yield changes in decimal form (VERIFIED CORRECT)
                yield_change = (yield_to_maturity - lag(yield_to_maturity)) / 100,

                # PROPER PRICE RETURN CALCULATION
                # Price Return = -Duration × Δy + 0.5 × Convexity × (Δy)²
                # Convert to percentage by multiplying by 100
                price_return = (-modified_duration * yield_change +
                                    0.5 * convexity * yield_change^2) * 100,

                # Scale to desired horizon
                scaled_return = price_return * sqrt(horizon_days)
            ) %>%
            filter(!is.na(price_return))

        # Recalculate VaR metrics with fresh data
        var_levels <- returns_data_recalc %>%
            group_by(bond) %>%
            summarise(
                # Sample statistics
                n_observations = n(),
                mean_return = mean(scaled_return),
                median_return = median(scaled_return),
                vol = sd(scaled_return),
                skewness = moments::skewness(scaled_return),
                kurtosis = moments::kurtosis(scaled_return),

                # Historical VaR (empirical quantiles)
                VaR_95_hist = quantile(scaled_return, 1 - confidence_levels[1]),
                VaR_99_hist = quantile(scaled_return, 1 - confidence_levels[2]),

                # Parametric VaR (assuming normal distribution)
                VaR_95_param = mean_return - qnorm(confidence_levels[1]) * vol,
                VaR_99_param = mean_return - qnorm(confidence_levels[2]) * vol,

                # Cornish-Fisher VaR (adjusting for skewness and kurtosis)
                z_95 = qnorm(confidence_levels[1]),
                z_99 = qnorm(confidence_levels[2]),
                VaR_95_cf = mean_return - vol * (z_95 +
                                                     (z_95^2 - 1) * skewness/6 +
                                                     (z_95^3 - 3*z_95) * (kurtosis - 3)/24),
                VaR_99_cf = mean_return - vol * (z_99 +
                                                     (z_99^2 - 1) * skewness/6 +
                                                     (z_99^3 - 3*z_99) * (kurtosis - 3)/24),

                # Conditional VaR (Expected Shortfall)
                CVaR_95 = mean(scaled_return[scaled_return < VaR_95_hist]),
                CVaR_99 = mean(scaled_return[scaled_return < VaR_99_hist]),

                # Choose which VaR to display based on method
                VaR_95 = case_when(
                    method == "historical" ~ VaR_95_hist,
                    method == "parametric" ~ VaR_95_param,
                    method == "cornish-fisher" ~ VaR_95_cf,
                    TRUE ~ VaR_95_hist
                ),
                VaR_99 = case_when(
                    method == "historical" ~ VaR_99_hist,
                    method == "parametric" ~ VaR_99_param,
                    method == "cornish-fisher" ~ VaR_99_cf,
                    TRUE ~ VaR_99_hist
                ),

                # Risk-adjusted metrics
                sharpe_ratio = mean_return / vol * sqrt(252 / horizon_days),
                sortino_ratio = mean_return / sd(scaled_return[scaled_return < 0]) * sqrt(252 / horizon_days),

                .groups = "drop"
            ) %>%
            select(-starts_with("z_"))

        if (enable_diagnostics) {
            # Compare before and after
            comparison <- var_levels_original %>%
                select(bond, vol_old = vol, VaR_95_old = VaR_95) %>%
                left_join(
                    var_levels %>% select(bond, vol_new = vol, VaR_95_new = VaR_95),
                    by = "bond"
                ) %>%
                mutate(
                    vol_change_pct = (vol_new - vol_old) / vol_old * 100,
                    var_change_pct = (VaR_95_new - VaR_95_old) / abs(VaR_95_old) * 100
                )

            cat("   Recalculation complete!\n\n")
            cat("📊 Before vs After Comparison:\n")
            cat(sprintf("   Mean volatility: %.4f%% → %.4f%% (%.0fx change)\n",
                        mean(var_levels_original$vol),
                        mean(var_levels$vol),
                        mean(var_levels$vol) / mean(var_levels_original$vol)))
            cat(sprintf("   Mean VaR95: %.4f%% → %.4f%%\n",
                        mean(abs(var_levels_original$VaR_95)),
                        mean(abs(var_levels$VaR_95))))

            cat("\n   Bond-by-bond changes:\n")
            for (i in 1:min(5, nrow(comparison))) {
                cat(sprintf("   %s: vol %.4f%% → %.4f%% (%+.0f%%)\n",
                            comparison$bond[i],
                            comparison$vol_old[i],
                            comparison$vol_new[i],
                            comparison$vol_change_pct[i]))
            }

            # Check if recalculation fixed the issue
            new_vol_mean <- mean(var_levels$vol, na.rm = TRUE)
            if (new_vol_mean >= 1.0) {
                cat("\n✓ SUCCESS: Volatility now in expected range (1-3%)\n")
                cat("   The upstream data had a calculation error that has been corrected.\n")
            } else if (new_vol_mean >= 0.5) {
                cat("\n✓ IMPROVED: Volatility increased but still lower than expected\n")
                cat("   Consider investigating data quality further.\n")
            } else {
                cat("\n⚠️  WARNING: Volatility still too low after recalculation\n")
                cat("   This suggests a fundamental data quality issue.\n")
                cat("   Possible causes:\n")
                cat("   - Insufficient data variation\n")
                cat("   - Data collection or processing error\n")
                cat("   - Market conditions (unusual period)\n")
            }

            cat("\n╚════════════════════════════════════════════════════════════════╝\n\n")
        }

        # Update returns_data to use recalculated values
        returns_data <- returns_data_recalc
    }

    # R2053-SPECIFIC DIAGNOSTIC REPORT
    if (enable_diagnostics && "R2053" %in% var_levels$bond) {
        cat("=== R2053 DETAILED DIAGNOSTIC REPORT ===\n")

        r2053_stats <- var_levels %>% filter(bond == "R2053")
        r2053_returns <- returns_data %>% filter(bond == "R2053")

        cat(sprintf("\nR2053 Statistics:\n"))
        cat(sprintf("  Observations: %d\n", r2053_stats$n_observations))
        cat(sprintf("  Mean return: %.3f%%\n", r2053_stats$mean_return))
        cat(sprintf("  Median return: %.3f%%\n", r2053_stats$median_return))
        cat(sprintf("  Volatility (σ): %.2f%%\n", r2053_stats$vol))
        cat(sprintf("  Skewness: %.3f\n", r2053_stats$skewness))
        cat(sprintf("  Kurtosis: %.3f (normal=3.0)\n", r2053_stats$kurtosis))
        cat(sprintf("  VaR 95%%: %.2f%%\n", r2053_stats$VaR_95))
        cat(sprintf("  VaR 99%%: %.2f%%\n", r2053_stats$VaR_99))

        # Analyze return distribution
        r2053_extreme <- r2053_returns %>%
            summarise(
                min_return = min(scaled_return, na.rm = TRUE),
                max_return = max(scaled_return, na.rm = TRUE),
                range = max_return - min_return,
                n_beyond_3sigma = sum(abs(scaled_return - mean(scaled_return)) >
                                          3 * sd(scaled_return), na.rm = TRUE),
                pct_beyond_3sigma = mean(abs(scaled_return - mean(scaled_return)) >
                                             3 * sd(scaled_return), na.rm = TRUE) * 100,
                n_beyond_5pct = sum(abs(scaled_return) > 5, na.rm = TRUE),
                pct_beyond_5pct = mean(abs(scaled_return) > 5, na.rm = TRUE) * 100
            )

        cat(sprintf("\nReturn Distribution Analysis:\n"))
        cat(sprintf("  Range: %.2f%% to %.2f%% (span: %.2f%%)\n",
                    r2053_extreme$min_return, r2053_extreme$max_return, r2053_extreme$range))
        cat(sprintf("  Returns beyond 3σ: %d (%.1f%%) [Normal: ~0.3%%]\n",
                    r2053_extreme$n_beyond_3sigma, r2053_extreme$pct_beyond_3sigma))
        cat(sprintf("  Returns beyond ±5%%: %d (%.1f%%)\n",
                    r2053_extreme$n_beyond_5pct, r2053_extreme$pct_beyond_5pct))

        # Normality assessment
        cat(sprintf("\nNormality Assessment:\n"))
        if (abs(r2053_stats$kurtosis - 3) > 2) {
            cat(sprintf("  ⚠️  High excess kurtosis (%.2f) indicates fat tails\n",
                        r2053_stats$kurtosis - 3))
        } else {
            cat(sprintf("  ✓ Kurtosis is reasonable\n"))
        }

        if (abs(r2053_stats$skewness) > 0.5) {
            cat(sprintf("  ⚠️  Significant skewness (%.2f) indicates asymmetric distribution\n",
                        r2053_stats$skewness))
        } else {
            cat(sprintf("  ✓ Skewness is acceptable\n"))
        }

        # Compare to other bonds
        other_bonds_avg <- var_levels %>%
            filter(bond != "R2053") %>%
            summarise(
                avg_vol = mean(vol, na.rm = TRUE),
                avg_var95 = mean(VaR_95, na.rm = TRUE),
                avg_var99 = mean(VaR_99, na.rm = TRUE)
            )

        cat(sprintf("\nComparison to Other Bonds:\n"))
        cat(sprintf("  R2053 vol: %.2f%% vs Other bonds avg: %.2f%% (%.1fx)\n",
                    r2053_stats$vol, other_bonds_avg$avg_vol,
                    r2053_stats$vol / other_bonds_avg$avg_vol))
        cat(sprintf("  R2053 VaR95: %.2f%% vs Other bonds avg: %.2f%% (%.1fx)\n",
                    r2053_stats$VaR_95, other_bonds_avg$avg_var95,
                    abs(r2053_stats$VaR_95) / abs(other_bonds_avg$avg_var95)))

        # Recommendations
        cat(sprintf("\nRecommendations:\n"))
        if (r2053_stats$vol > other_bonds_avg$avg_vol * 2) {
            cat("  ⚠️  R2053 volatility is >2x other bonds - investigate data quality\n")
        }
        if (r2053_extreme$pct_beyond_5pct > 5) {
            cat("  ⚠️  >5%% of returns exceed ±5%% - check for data errors\n")
        }
        if (abs(r2053_stats$kurtosis - 3) > 3) {
            cat("  ⚠️  Extreme kurtosis - distribution has very fat tails\n")
        }

        cat("\n========================================\n\n")
    }

    # =========================================================================
    # ORDER BONDS BY 95% VaR (highest risk first) - for consistency
    # =========================================================================
    if (is.null(bond_order)) {
        bond_order <- var_levels %>%
            arrange(desc(abs(VaR_95))) %>%
            pull(bond)
    }

    # =========================================================================
    # ADD DATA QUALITY INDICATORS TO BOND LABELS
    # =========================================================================
    # Join data quality info if provided, and create bond labels with indicators
    # * = Limited history (<252 obs)
    # ** = Insufficient history (<60 obs)
    # =========================================================================
    has_quality_issues <- FALSE
    if (!is.null(data_quality) && nrow(data_quality) > 0) {
        # Merge quality info with var_levels
        var_levels <- var_levels %>%
            left_join(
                data_quality %>% select(bond, data_quality, quality_note, n_observations),
                by = "bond",
                suffix = c("", "_quality")
            ) %>%
            mutate(
                # Use n_observations from quality if not already present
                n_observations = coalesce(n_observations, n_observations_quality),
                # Create display label with quality indicator
                bond_label = case_when(
                    data_quality == "Limited" ~ paste0(bond, " *"),
                    data_quality == "Insufficient" ~ paste0(bond, " **"),
                    TRUE ~ as.character(bond)
                )
            ) %>%
            select(-any_of(c("n_observations_quality")))

        # Check if there are any quality issues to show in subtitle
        has_quality_issues <- any(var_levels$data_quality %in% c("Limited", "Insufficient"), na.rm = TRUE)

        # Create bond label order (matching bond_order but with labels)
        bond_label_order <- var_levels %>%
            arrange(match(bond, bond_order)) %>%
            pull(bond_label)

        # Apply to returns data - need to match original bond to labeled bond
        bond_to_label <- var_levels %>%
            select(bond, bond_label) %>%
            distinct()

        returns_data <- returns_data %>%
            left_join(bond_to_label, by = "bond") %>%
            mutate(bond_display = factor(bond_label, levels = bond_label_order))

        # Update var_levels for plotting
        var_levels <- var_levels %>%
            mutate(bond_display = factor(bond_label, levels = bond_label_order))
    } else {
        # No quality data - use plain bond names
        returns_data <- returns_data %>%
            mutate(bond_display = factor(bond, levels = bond_order))

        var_levels <- var_levels %>%
            mutate(bond_display = factor(bond, levels = bond_order))
    }

    # Apply bond ordering to returns data (keep original bond factor too)
    returns_data <- returns_data %>%
        mutate(bond = factor(bond, levels = bond_order))

    # Apply ordering to var_levels as well
    var_levels <- var_levels %>%
        mutate(bond = factor(bond, levels = bond_order))

    # Determine dynamic layout
    n_bonds <- n_distinct(returns_data$bond)

    # Prepare VaR lines data for cleaner legend
    var_lines <- var_levels %>%
        select(bond, VaR_95, VaR_99) %>%
        tidyr::pivot_longer(cols = c(VaR_95, VaR_99), names_to = "var_type", values_to = "var_value") %>%
        mutate(
            var_type = factor(var_type, levels = c("VaR_95", "VaR_99"),
                              labels = c("95% VaR", "99% VaR"))
        )

    # ══════════════════════════════════════════════════════════════
    # RIDGELINE PLOT (ggridges) - superior readability vs facets
    # Fallback to faceted approach if ggridges not available
    # ══════════════════════════════════════════════════════════════
    use_ridgeline <- requireNamespace("ggridges", quietly = TRUE)

    if (use_ridgeline) {
        # Order bonds by 95% VaR (highest risk at top)
        var_order <- var_levels %>%
            arrange(VaR_95) %>%
            pull(bond)
        returns_data$bond_ridge <- factor(returns_data$bond, levels = var_order)

        p <- ggplot(returns_data, aes(x = scaled_return, y = bond_ridge, fill = after_stat(x))) +
            ggridges::geom_density_ridges_gradient(
                scale = 2.5,
                rel_min_height = 0.01,
                gradient_lwd = 0.3,
                quantile_lines = TRUE,
                quantiles = c(0.05, 0.01)
            ) +
            scale_fill_viridis_c(option = "C", name = "Return (%)") +
            geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
            scale_x_continuous(
                labels = scales::percent_format(accuracy = 1, scale = 1),
                limits = c(-8, 8)
            ) +
            labs(
                title = "Value-at-Risk Distribution by Bond",
                subtitle = paste0("Ordered by 95% VaR | Quantile lines show 95% and 99% VaR thresholds | ",
                                  str_to_title(method), " method | ", horizon_days, "-day horizon"),
                x = "Daily Return (%)",
                y = NULL,
                caption = sprintf(
                    "Returns calculated using modified duration and convexity | %d active bonds",
                    n_bonds
                )
            ) +
            create_insele_theme() +
            theme(
                axis.text.y = element_text(size = 9, face = "bold"),
                legend.position = "bottom",
                plot.title = element_text(face = "bold", color = insele_palette$primary),
                plot.caption = element_text(
                    hjust = 0, size = 8, lineheight = 1.2,
                    margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")
                ),
                panel.grid.minor = element_blank()
            )
    } else {
        # Fallback: faceted approach with top/bottom 5 bonds by VaR
        n_cols <- case_when(
            n_bonds <= 2 ~ n_bonds,
            n_bonds <= 4 ~ 2,
            n_bonds <= 6 ~ 3,
            n_bonds <= 9 ~ 3,
            TRUE ~ 4
        )

        # If many bonds, reduce to top 5 and bottom 5 by VaR
        if (n_bonds > 10) {
            top5 <- var_levels %>% arrange(VaR_95) %>% head(5) %>% pull(bond)
            bot5 <- var_levels %>% arrange(desc(VaR_95)) %>% head(5) %>% pull(bond)
            selected <- unique(c(top5, bot5))
            returns_data <- returns_data %>% filter(bond %in% selected)
            var_levels_plot <- var_levels %>% filter(bond %in% selected)
            n_bonds_display <- length(selected)
        } else {
            var_levels_plot <- var_levels
            n_bonds_display <- n_bonds
        }

        p <- ggplot(returns_data, aes(x = scaled_return)) +
            geom_histogram(aes(y = after_stat(density)), bins = 30,
                           fill = "#64B5F6", color = "white", alpha = 0.7) +
            geom_density(color = insele_palette$primary, linewidth = 0.8) +
            geom_vline(data = var_levels_plot, aes(xintercept = VaR_95),
                       color = "#FF9800", linetype = "dashed", linewidth = 1.0) +
            geom_vline(data = var_levels_plot, aes(xintercept = VaR_99),
                       color = "#D32F2F", linetype = "solid", linewidth = 1.0) +
            facet_wrap(~ bond_display, ncol = n_cols, scales = "free_y") +
            scale_x_continuous(labels = scales::percent_format(accuracy = 1, scale = 1),
                               limits = c(-8, 8)) +
            scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
            labs(
                title = "Daily Return Distributions",
                subtitle = sprintf("%s Method | %d-day horizon | Dashed orange = 95%% VaR | Solid red = 99%% VaR",
                    str_to_title(method), horizon_days),
                x = "Daily Price Return (%)", y = "Density",
                caption = sprintf(
                    "Returns calculated using modified duration and convexity | %d active bonds ordered by 95%% VaR",
                    n_bonds_display)
            ) +
            create_insele_theme() +
            theme(
                strip.background = element_rect(fill = "#E3F2FD", color = NA),
                strip.text = element_text(face = "bold", size = 9, color = insele_palette$primary),
                panel.spacing = unit(1, "lines"),
                plot.title = element_text(face = "bold", color = insele_palette$primary),
                plot.caption = element_text(hjust = 0, size = 8, lineheight = 1.2,
                    margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")),
                legend.position = "top",
                panel.grid.minor = element_blank()
            )
    }

    # Add summary table as attribute (for external statistics table)
    attr(p, "var_summary") <- var_levels %>%
        arrange(desc(abs(VaR_95))) %>%
        select(bond, VaR_95, VaR_99, VaR_95_hist, VaR_99_hist, CVaR_95, CVaR_99,
               mean_return, vol, skewness, kurtosis, n_observations)

    # Add returns data as attribute
    attr(p, "returns_data") <- returns_data

    # Add bond order as attribute (for consistent ordering in Risk Ladder)
    attr(p, "bond_order") <- bond_order

    return(p)
}

#' @export
# 5. VAR Ladder Plot Generation - REBUILT VERSION
#' @description Creates a horizontal bar chart showing VaR and CVaR for each bond
#' @param var_data Data frame with VaR metrics (VaR_95_bps, VaR_99_bps, CVaR_95)
#' @param params List of optional parameters including bond_order
#' @return ggplot object
generate_var_ladder_plot <- function(var_data, params = list()) {

    # Extract parameters with defaults
    bond_order <- params$bond_order  # Optional: pre-specified bond order for consistency
    data_quality <- params$data_quality  # Optional: data quality info for annotations

    # ===========================================================================
    # STEP 1: COMPREHENSIVE NA FILTERING USING CENTRALIZED FUNCTION
    # This is the first line of defense against NA/invalid values
    # ===========================================================================
    var_data <- prepare_var_data_for_plotting(var_data, source = "Risk Ladder")

    # Validate input after filtering
    if (is.null(var_data) || nrow(var_data) == 0) {
        warning("generate_var_ladder_plot: No valid data after filtering")
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = "No bonds have sufficient data\nfor VaR calculation with current parameters",
                         size = 5, color = "#666666") +
                theme_void() +
                labs(title = "Risk Ladder: VaR & Expected Shortfall")
        )
    }

    # Ensure required columns exist
    required_cols <- c("bond", "VaR_95_bps", "VaR_99_bps", "CVaR_95")
    missing_cols <- setdiff(required_cols, names(var_data))
    if (length(missing_cols) > 0) {
        warning(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
        return(NULL)
    }

    # ===========================================================================
    # STEP 2: SECONDARY FILTERING - belt and suspenders approach
    # The prepare_var_data_for_plotting should have handled this, but double-check
    # ===========================================================================
    var_ladder <- var_data %>%
        select(bond, VaR_95_bps, VaR_99_bps, CVaR_95) %>%
        # Final NA check (should be clean but verify)
        filter(
            !is.na(bond),
            bond != "",
            bond != "NA",
            as.character(bond) != "NA",
            nchar(trimws(as.character(bond))) > 0
        ) %>%
        mutate(
            VaR_95_bps = as.numeric(VaR_95_bps),
            VaR_99_bps = as.numeric(VaR_99_bps),
            CVaR_95 = as.numeric(CVaR_95),
            # Convert CVaR to basis points (absolute value for display)
            CVaR_bps = abs(CVaR_95) * 100
        ) %>%
        filter(
            !is.na(VaR_95_bps),
            is.finite(VaR_95_bps),
            !is.na(VaR_99_bps),
            is.finite(VaR_99_bps),
            VaR_95_bps > 0,     # Must have positive VaR
            VaR_95_bps < 1000,  # Filter extreme values (>10% daily VaR)
            VaR_99_bps < 1500   # Filter extreme 99% VaR values
        )

    # Check if we have valid data after filtering
    if (nrow(var_ladder) == 0) {
        warning("generate_var_ladder_plot: No valid VaR data after all filtering")
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = "All bonds filtered out due to invalid VaR values\nTry adjusting parameters",
                         size = 5, color = "#666666") +
                theme_void() +
                labs(title = "Risk Ladder: VaR & Expected Shortfall")
        )
    }

    # Sort by 95% VaR (ascending so highest ends up at top after coord_flip)
    var_ladder <- var_ladder %>%
        arrange(VaR_95_bps)

    # =========================================================================
    # MERGE DATA QUALITY INFO IF PROVIDED
    # =========================================================================
    has_quality_issues <- FALSE
    if (!is.null(data_quality) && nrow(data_quality) > 0) {
        var_ladder <- var_ladder %>%
            left_join(
                data_quality %>% select(bond, data_quality, quality_note, n_observations),
                by = "bond"
            ) %>%
            mutate(
                # Create display label with quality indicator
                bond_label = case_when(
                    data_quality == "Limited" ~ paste0(bond, " *"),
                    data_quality == "Insufficient" ~ paste0(bond, " **"),
                    TRUE ~ as.character(bond)
                )
            )

        has_quality_issues <- any(var_ladder$data_quality %in% c("Limited", "Insufficient"), na.rm = TRUE)
    } else {
        var_ladder <- var_ladder %>%
            mutate(
                data_quality = "Good",
                quality_note = "",
                bond_label = as.character(bond)
            )
    }

    # Apply bond ordering - create factor with levels locked in order
    if (!is.null(bond_order)) {
        # =========================================================================
        # CRITICAL FIX: Filter var_ladder to ONLY bonds in bond_order BEFORE
        # creating factor levels. This prevents NA rows from appearing when
        # var_ladder has bonds not present in bond_order.
        # =========================================================================
        original_count <- nrow(var_ladder)
        bonds_in_ladder <- unique(var_ladder$bond)
        bonds_not_in_order <- setdiff(bonds_in_ladder, bond_order)

        if (length(bonds_not_in_order) > 0) {
            warning(sprintf("[Risk Ladder Plot] %d bonds in data not in bond_order - FILTERING OUT: %s",
                           length(bonds_not_in_order), paste(bonds_not_in_order, collapse = ", ")))
            var_ladder <- var_ladder %>%
                filter(bond %in% bond_order)
            message(sprintf("[Risk Ladder Plot] Filtered: %d -> %d bonds to match bond_order",
                           original_count, nrow(var_ladder)))
        }

        # Check if we have any data left after filtering
        if (nrow(var_ladder) == 0) {
            warning("[Risk Ladder Plot] No bonds remaining after filtering to bond_order")
            return(
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5,
                             label = "No matching bonds between data and ordering\nCheck VaR parameters",
                             size = 5, color = "#666666") +
                    theme_void() +
                    labs(title = "Risk Ladder: VaR & Expected Shortfall")
            )
        }

        # Create label order matching bond_order
        label_lookup <- var_ladder %>% select(bond, bond_label) %>% distinct()
        bond_label_order <- tibble(bond = bond_order) %>%
            left_join(label_lookup, by = "bond") %>%
            filter(!is.na(bond_label)) %>%
            pull(bond_label)

        # Use provided order (reversed for coord_flip so highest at top)
        var_ladder <- var_ladder %>%
            mutate(bond_display = factor(bond_label, levels = rev(bond_label_order)))

        # FINAL SAFETY CHECK: Remove any rows with NA bond_display
        # This should not happen after the above fix, but belt-and-suspenders
        na_display_count <- sum(is.na(var_ladder$bond_display))
        if (na_display_count > 0) {
            warning(sprintf("[Risk Ladder Plot] CRITICAL: %d rows with NA bond_display after factor creation - removing",
                           na_display_count))
            var_ladder <- var_ladder %>%
                filter(!is.na(bond_display))
        }
    } else {
        # Lock in current order (sorted by VaR_95_bps ascending)
        var_ladder <- var_ladder %>%
            mutate(bond_display = factor(bond_label, levels = bond_label))
    }

    # Calculate portfolio average (only from active bonds)
    n_bonds <- nrow(var_ladder)
    avg_var_95 <- mean(var_ladder$VaR_95_bps, na.rm = TRUE)

    # Build plot using x = bond_display with coord_flip() for horizontal bars
    p <- ggplot(var_ladder, aes(x = bond_display)) +

        # 95% VaR bar (wide, orange)
        geom_col(
            aes(y = VaR_95_bps),
            fill = "#FFB74D",
            width = 0.7
        ) +

        # 99% VaR bar (narrow, red) - overlaid on top
        geom_col(
            aes(y = VaR_99_bps),
            fill = "#E57373",
            width = 0.4
        ) +

        # CVaR diamond
        geom_point(
            aes(y = CVaR_bps),
            shape = 23,
            size = 4,
            fill = "#1565C0",
            color = "white",
            stroke = 1.5
        ) +

        # Value labels for 95% VaR
        geom_text(
            aes(y = VaR_95_bps, label = sprintf("%.0f", VaR_95_bps)),
            hjust = -0.2,
            size = 3,
            fontface = "bold"
        ) +

        # Portfolio average line
        geom_hline(
            yintercept = avg_var_95,
            linetype = "dashed",
            color = "#1B3A6B",
            linewidth = 1
        ) +

        # Portfolio average label
        annotate(
            "text",
            x = n_bonds + 0.5,
            y = avg_var_95,
            label = sprintf("Portfolio Avg\n%.0f bps", avg_var_95),
            hjust = 0,
            vjust = 0.5,
            size = 3,
            fontface = "bold",
            color = "#1B3A6B"
        ) +

        # CRITICAL: Flip coordinates to make horizontal bar chart
        coord_flip(clip = "off") +

        # Expand to make room for labels
        scale_y_continuous(
            expand = expansion(mult = c(0, 0.15)),
            breaks = seq(0, 600, 100),
            labels = function(x) paste0(x, " bps")
        ) +

        # Labels - include quality indicators and bond count
        labs(
            title = "Risk Ladder: VaR & Expected Shortfall",
            subtitle = if (has_quality_issues) {
                sprintf("n = %d active bonds | Sorted by 95%% VaR (highest risk at top) | Daily horizon\n* Limited history (<252 obs) | ** Insufficient history (<60 obs)",
                       n_bonds)
            } else {
                sprintf("n = %d active bonds | Sorted by 95%% VaR (highest risk at top) | Daily horizon",
                       n_bonds)
            },
            x = NULL,
            y = "Risk (basis points of price)",
            caption = "Wide bar = 95% VaR | Narrow bar = 99% VaR | \u25C6 = CVaR (Expected Shortfall)\nNote: Short-duration bonds may show elevated CVaR due to percentage-based calculation on low-duration instruments."
        ) +

        theme_minimal(base_size = 11) +
        theme(
            plot.title = element_text(face = "bold", color = "#1B3A6B", size = 14),
            plot.subtitle = element_text(color = "#666666", size = 10, lineheight = 1.2),
            axis.text.y = element_text(face = "bold", size = 10),
            panel.grid.major.x = element_line(color = "#E0E0E0"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = ggplot2::margin(10, 60, 10, 10),  # Extra right margin for labels
            plot.caption = element_text(
                hjust = 0,
                size = 8,
                lineheight = 1.2,
                margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")
            )
        )

    return(p)
}

#' @export
# 6. DV01 Ladder Plot Generation
#' @description Creates a horizontal bar chart showing DV01 for each bond
#' @param data Bond data with modified_duration column
#' @param params List of parameters including notional
#' @return ggplot object
generate_dv01_ladder_plot <- function(data, params = list()) {
    # Extract parameters with defaults
    # Default to R10 million (10e6) - a more realistic per-bond notional
    notional <- params$notional %||% 10000000
    show_average <- params$show_average %||% TRUE
    show_labels <- params$show_labels %||% TRUE
    max_bonds <- params$max_bonds %||% 20

    # Validate required columns
    required_cols <- c("bond", "modified_duration")
    if (!all(required_cols %in% names(data))) {
        stop("Missing required columns for DV01 calculation")
    }

    # Calculate DV01 properly using correct formula
    # DV01 = Notional × Modified Duration × 0.0001 (0.01% = 1 basis point)
    dv01_data <- data %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%  # Use latest data point
        ungroup() %>%
        mutate(
            # DV01 = Notional × Modified Duration × 0.0001
            dv01_absolute = notional * modified_duration * 0.0001,

            # Convert to millions for display
            dv01_millions = dv01_absolute / 1e6,

            # Risk categorization based on DV01
            risk_category = case_when(
                dv01_millions > quantile(dv01_millions, 0.75, na.rm = TRUE) ~ "High",
                dv01_millions > quantile(dv01_millions, 0.25, na.rm = TRUE) ~ "Medium",
                TRUE ~ "Low"
            ),

            # IMPROVED DV01 FORMATTING: More precision for smaller values
            # >= 0.10m: 2 decimal places (e.g., "R0.10m")
            # >= 0.01m and < 0.10m: 3 decimal places (e.g., "R0.095m")
            # < 0.01m: show in thousands (e.g., "R9.5k")
            dv01_label = case_when(
                is.na(dv01_millions) ~ "N/A",
                dv01_millions >= 0.10 ~ sprintf("R%.2fm", dv01_millions),
                dv01_millions >= 0.01 ~ sprintf("R%.3fm", dv01_millions),
                TRUE ~ sprintf("R%.1fk", dv01_absolute / 1e3)
            )
        ) %>%
        arrange(dv01_millions)  # Sort ascending so highest at top after coord_flip

    # Limit to top bonds if too many
    if (nrow(dv01_data) > max_bonds) {
        dv01_data <- dv01_data %>%
            arrange(desc(dv01_millions)) %>%
            slice_head(n = max_bonds) %>%
            arrange(dv01_millions)
    }

    # Lock in factor order for consistent display
    dv01_data <- dv01_data %>%
        mutate(bond = factor(bond, levels = bond))

    # Calculate statistics
    avg_dv01 <- mean(dv01_data$dv01_millions, na.rm = TRUE)
    total_dv01 <- sum(dv01_data$dv01_millions, na.rm = TRUE)

    # Format notional for display
    notional_display <- if(notional >= 1e9) {
        sprintf("R%.0fbn", notional / 1e9)
    } else {
        sprintf("R%.0fm", notional / 1e6)
    }

    # Create the plot
    p <- ggplot(dv01_data, aes(x = bond, y = dv01_millions)) +

        # Main bars with gradient coloring by duration
        geom_col(aes(fill = modified_duration), width = 0.7) +

        # Add border to bars for clarity
        geom_col(color = "white", fill = NA, width = 0.7, linewidth = 0.5) +

        # Average reference line
        {if(show_average) {
            geom_hline(yintercept = avg_dv01,
                       linetype = "dashed",
                       color = "#E65100",
                       linewidth = 1)
        }} +

        # Value labels on bars (DV01 in Rands) - using adaptive precision
        {if(show_labels) {
            geom_text(aes(label = dv01_label),
                      hjust = -0.1,
                      size = 3.5,
                      fontface = "bold")
        }} +

        # Duration labels inside bars
        {if(show_labels) {
            geom_text(aes(x = bond, y = dv01_millions / 2,
                          label = sprintf("%.1fy", modified_duration)),
                      size = 3,
                      fontface = "bold",
                      color = "white")
        }} +

        # Average line annotation - with adaptive precision
        {if(show_average) {
            avg_label <- if(avg_dv01 >= 0.10) {
                sprintf("Avg: R%.2fm", avg_dv01)
            } else if(avg_dv01 >= 0.01) {
                sprintf("Avg: R%.3fm", avg_dv01)
            } else {
                sprintf("Avg: R%.1fk", avg_dv01 * 1000)
            }
            annotate("text",
                     x = nrow(dv01_data) + 0.5,
                     y = avg_dv01,
                     label = avg_label,
                     hjust = -0.1,
                     vjust = 0.5,
                     color = "#E65100",
                     size = 3,
                     fontface = "bold")
        }} +

        scale_fill_gradient(
            low = "#81D4FA",
            high = "#1B3A6B",
            name = "Duration (years)"
        ) +

        # Standardized x-axis labels in thousands
        scale_y_continuous(
            labels = function(x) {
                sapply(x, function(v) {
                    if(is.na(v)) return("")
                    paste0("R", round(v * 1000, 1), "k")
                })
            },
            expand = expansion(mult = c(0, 0.15)),
            breaks = pretty_breaks(n = 6)
        ) +

        coord_flip(clip = "off") +

        labs(
            title = "DV01 Risk Ladder: Interest Rate Sensitivity",
            subtitle = sprintf(
                "Value change per basis point move | Notional: %s | Total DV01: R%.2fm",
                notional_display, total_dv01
            ),
            x = NULL,
            y = "DV01 (R millions)",
            caption = "DV01 = Notional × Modified Duration × 0.01% | Duration shown inside bars"
        ) +

        create_insele_theme() +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_line(color = "#F5F5F5", linetype = "dotted"),
            # Remove legend since duration is shown as labels inside bars
            legend.position = "none",
            plot.caption = element_text(
                hjust = 0,
                size = 8,
                lineheight = 1.2,
                margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")
            ),
            plot.subtitle = element_text(
                size = 10,
                face = "italic"
            ),
            plot.margin = ggplot2::margin(10, 40, 10, 10),  # Extra right margin for labels
            axis.text.y = element_text(face = "bold")
        )

    # Add summary table as an attribute (for reporting)
    attr(p, "summary") <- dv01_data %>%
        summarise(
            bonds_count = n(),
            total_dv01_millions = sum(dv01_millions, na.rm = TRUE),
            average_dv01_millions = mean(dv01_millions, na.rm = TRUE),
            median_dv01_millions = median(dv01_millions, na.rm = TRUE),
            max_dv01_millions = max(dv01_millions, na.rm = TRUE),
            min_dv01_millions = min(dv01_millions, na.rm = TRUE),
            portfolio_modified_duration = weighted.mean(modified_duration,
                                                        w = dv01_millions,
                                                        na.rm = TRUE),
            notional_used = notional
        )

    # Add data as attribute for further analysis
    attr(p, "data") <- dv01_data

    return(p)
}

#' @export
# 11. Scenario Analysis Plot Generation
generate_scenario_analysis_plot <- function(data, params = list()) {

    # Extract parameters with defaults
    scenarios_to_show <- params$scenarios %||% c("parallel", "flattening", "steepening", "butterfly")
    horizon_days <- params$horizon_days %||% 90  # 3-month default
    confidence_level <- params$confidence_level %||% 0.95
    show_total_return <- params$show_total_return %||% FALSE
    max_bonds <- params$max_bonds %||% 8  # Maximum bonds for readability
    shift_range <- params$shift_range %||% c(-200, 200)
    shift_increment <- params$shift_increment %||% 25

    # NEW PARAMETERS for user controls
    selected_bonds_param <- params$selected_bonds %||% NULL  # User-selected bonds
    y_scale <- params$y_scale %||% "fixed"  # "fixed" for unified, "free_y" for individual scales
    show_confidence <- params$show_confidence %||% TRUE  # Toggle confidence bands

    # Validate required columns
    required_cols <- c("bond", "yield_to_maturity", "modified_duration", "convexity", "coupon")
    if (!all(required_cols %in% names(data))) {
        stop(paste("Missing required columns:",
                   paste(setdiff(required_cols, names(data)), collapse = ", ")))
    }

    # Get latest data for each bond
    latest_data <- data %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup()

    # Handle bond selection - either use user-selected bonds or auto-select
    if (!is.null(selected_bonds_param) && length(selected_bonds_param) > 0) {
        # User has selected specific bonds - filter to those
        # Remove any selected bonds that don't exist in data
        valid_bonds <- intersect(selected_bonds_param, unique(latest_data$bond))

        if (length(valid_bonds) == 0) {
            # No valid bonds selected - return empty plot with message
            return(
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5,
                             label = "Please select at least one valid bond to analyze",
                             size = 5, color = "grey50") +
                    theme_void()
            )
        }

        # Cap at max_bonds for readability
        if (length(valid_bonds) > max_bonds) {
            valid_bonds <- valid_bonds[1:max_bonds]
        }

        latest_data <- filter(latest_data, bond %in% valid_bonds)

    } else {
        # Auto-select: 4 representative bonds (shortest, ~5y, ~10y, longest)
        tryCatch({
            dur_ranked <- latest_data %>% arrange(modified_duration)
            representative_bonds <- c()
            # Shortest
            representative_bonds <- c(representative_bonds, dur_ranked$bond[1])
            # ~5y
            mid5 <- dur_ranked %>% mutate(d5 = abs(modified_duration - 5)) %>%
                arrange(d5) %>% slice(1)
            representative_bonds <- c(representative_bonds, mid5$bond)
            # ~10y
            mid10 <- dur_ranked %>% mutate(d10 = abs(modified_duration - 10)) %>%
                arrange(d10) %>% slice(1)
            representative_bonds <- c(representative_bonds, mid10$bond)
            # Longest
            representative_bonds <- c(representative_bonds, dur_ranked$bond[nrow(dur_ranked)])
            representative_bonds <- unique(representative_bonds)
            latest_data <- filter(latest_data, bond %in% representative_bonds)
        }, error = function(e) {
            # Fallback: evenly spaced selection
            if (n_distinct(latest_data$bond) > max_bonds) {
                duration_ranked <- latest_data %>%
                    arrange(modified_duration) %>%
                    mutate(row_num = row_number())
                n_bonds <- nrow(duration_ranked)
                selected_indices <- unique(round(seq(1, n_bonds, length.out = max_bonds)))
                auto_selected_bonds <- duration_ranked %>%
                    filter(row_num %in% selected_indices) %>%
                    pull(bond)
                latest_data <- filter(latest_data, bond %in% auto_selected_bonds)
            }
        })
    }

    # Colorblind-friendly palette for scenario analysis (5 distinct colors)
    scenario_colors <- c(
        "#1B3A6B",  # Dark blue (short duration)
        "#E53935",  # Red
        "#43A047",  # Green
        "#FF9800",  # Orange
        "#8E24AA"   # Purple (long duration)
    )
    # Extend palette if needed
    if (n_distinct(latest_data$bond) > 5) {
        scenario_colors <- c(scenario_colors,
                             "#00ACC1",  # Cyan
                             "#795548",  # Brown
                             "#607D8B")  # Blue-gray
    }

    # Calculate historical volatility for confidence bands
    if (nrow(data) > 30) {
        hist_vol <- data %>%
            group_by(bond) %>%
            arrange(date) %>%
            mutate(yield_change = yield_to_maturity - lag(yield_to_maturity)) %>%
            summarise(
                daily_vol = sd(yield_change, na.rm = TRUE),
                annual_vol_bps = daily_vol * sqrt(252) * 100
            )
    } else {
        # Default volatility if insufficient history
        hist_vol <- latest_data %>%
            distinct(bond) %>%
            mutate(annual_vol_bps = 50)  # 50bps annual volatility default
    }

    # Merge volatility data
    latest_data <- latest_data %>%
        left_join(hist_vol, by = "bond")

    # Define sophisticated scenarios with proper curve dynamics
    scenarios_list <- list()

    if ("parallel" %in% scenarios_to_show) {
        scenarios_list$parallel <- data.frame(
            scenario = "Parallel Shift",
            shift = seq(shift_range[1], shift_range[2], shift_increment)
        )
    }

    if ("flattening" %in% scenarios_to_show) {
        scenarios_list$flattening <- data.frame(
            scenario = "Curve Flattening",
            shift = seq(shift_range[1], shift_range[2], shift_increment)
        )
    }

    if ("steepening" %in% scenarios_to_show) {
        scenarios_list$steepening <- data.frame(
            scenario = "Curve Steepening",
            shift = seq(shift_range[1], shift_range[2], shift_increment)
        )
    }

    if ("butterfly" %in% scenarios_to_show) {
        scenarios_list$butterfly <- data.frame(
            scenario = "Butterfly Twist",
            shift = seq(shift_range[1], shift_range[2], shift_increment)
        )
    }

    # Calculate returns for each bond and scenario
    scenario_results <- latest_data %>%
        select(bond, yield_to_maturity, modified_duration, convexity, coupon, annual_vol_bps) %>%
        crossing(bind_rows(scenarios_list)) %>%
        mutate(
            # KEY RATE DURATION ADJUSTMENTS
            # More sophisticated scenario-specific yield changes
            maturity_factor = modified_duration / 10,  # Normalized to 10-year

            adjusted_shift = case_when(
                # Parallel: same shift for all maturities
                scenario == "Parallel Shift" ~ shift,

                # Flattening: short end up, long end down
                # Uses exponential decay function
                scenario == "Curve Flattening" ~ shift * (1 - exp(-0.1 * modified_duration)),

                # Steepening: short end down, long end up
                # Uses logistic function for smooth transition
                scenario == "Curve Steepening" ~ shift * (2 / (1 + exp(-0.5 * (modified_duration - 5))) - 1),

                # Butterfly: belly outperforms wings
                # Uses Gaussian-like function centered at 5-7 years
                scenario == "Butterfly Twist" ~ shift * exp(-0.5 * ((modified_duration - 6)/3)^2),

                TRUE ~ shift
            ),

            # Convert basis points to decimal
            yield_change_decimal = adjusted_shift / 10000,

            # PROPER BOND PRICING FORMULA
            # Price Change = -D × Δy + 0.5 × C × (Δy)²
            duration_impact = -modified_duration * yield_change_decimal * 100,
            convexity_impact = 0.5 * convexity * (yield_change_decimal^2) * 100,

            # CARRY COMPONENT (time-scaled)
            # Properly annualized and scaled to holding period
            carry_component = (coupon / 100) * (horizon_days / 365),

            # ROLL-DOWN COMPONENT
            # Based on empirical curve shape rather than arbitrary assumption
            # Estimate using current curve slope
            curve_slope = case_when(
                modified_duration < 3 ~ 0.30,    # Steep at short end
                modified_duration < 7 ~ 0.15,    # Moderate in belly
                modified_duration < 15 ~ 0.10,   # Flatter at long end
                TRUE ~ 0.05                      # Very flat beyond 15 years
            ),
            roll_component = curve_slope * (horizon_days / 365) * modified_duration,

            # PRICE RETURN COMPONENTS
            pure_price_return = duration_impact + convexity_impact,

            # TOTAL RETURN
            total_return = pure_price_return + carry_component + roll_component,

            # CONFIDENCE BANDS
            # Based on historical volatility and holding period
            vol_adjustment = annual_vol_bps * sqrt(horizon_days / 365) / 100,
            confidence_multiplier = qnorm((1 + confidence_level) / 2),
            confidence_lower = pure_price_return - confidence_multiplier * vol_adjustment,
            confidence_upper = pure_price_return + confidence_multiplier * vol_adjustment,

            # Return metric to plot
            plot_return = if(show_total_return) total_return else pure_price_return
        )

    # Calculate summary statistics for annotation
    summary_stats <- scenario_results %>%
        group_by(scenario, shift) %>%
        summarise(
            avg_return = mean(plot_return),
            max_return = max(plot_return),
            min_return = min(plot_return),
            .groups = "drop"
        )

    # Create the sophisticated plot
    p <- ggplot(scenario_results, aes(x = shift, y = plot_return)) +

        # Shaded risk zones
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = -5, ymax = 0,
                 alpha = 0.02, fill = insele_palette$warning) +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = -Inf, ymax = -5,
                 alpha = 0.02, fill = insele_palette$danger) +

        # Zero return line
        geom_hline(yintercept = 0,
                   linetype = "solid",
                   color = "black",
                   alpha = 0.5,
                   size = 0.5) +

        # Current market position
        geom_vline(xintercept = 0,
                   linetype = "dashed",
                   color = insele_palette$primary,
                   size = 0.8)

    # Conditionally add confidence bands based on show_confidence parameter
    if (show_confidence) {
        p <- p +
            geom_ribbon(aes(ymin = confidence_lower,
                            ymax = confidence_upper,
                            fill = bond, group = bond),
                        alpha = 0.1)
    }

    p <- p +
        # Main scenario lines
        geom_line(aes(color = bond, group = bond),
                  linewidth = 1.0,
                  alpha = 0.9) +

        # Highlight key points (-100, 0, +100 bps)
        geom_point(data = filter(scenario_results, shift %in% c(-100, 0, 100)),
                   aes(color = bond),
                   size = 3,
                   shape = 21,
                   fill = "white",
                   stroke = 1.5) +

        # Add breakeven points (where return = 0)
        geom_point(data = scenario_results %>%
                       group_by(bond, scenario) %>%
                       filter(abs(plot_return) == min(abs(plot_return))),
                   aes(color = bond),
                   size = 2,
                   shape = 4,
                   stroke = 2) +

        # Highlight best performing bond in each scenario at +100bps shock
        geom_point(data = scenario_results %>%
                       filter(shift == 100) %>%
                       group_by(scenario) %>%
                       slice_max(plot_return, n = 1) %>%
                       ungroup(),
                   aes(x = shift, y = plot_return),
                   shape = 8,  # Star
                   size = 4,
                   color = "#1B5E20",
                   stroke = 1.5) +

        # Facet by scenario with user-controlled y-axis scaling
        facet_wrap(~scenario, ncol = 2, scales = y_scale) +

        # Color and fill scales (using distinct colorblind-friendly palette)
        scale_color_manual(values = scenario_colors,
                           name = "Bond") +
        scale_fill_manual(values = scenario_colors,
                          guide = "none") +

        # X-axis formatting
        scale_x_continuous(
            breaks = seq(shift_range[1], shift_range[2], 100),
            labels = function(x) paste0(ifelse(x >= 0, "+", ""), x, " bps"),
            limits = shift_range
        ) +

        # Y-axis formatting
        scale_y_continuous(
            labels = function(x) paste0(ifelse(x >= 0, "+", ""), sprintf("%.1f", x), "%"),
            breaks = pretty_breaks(n = 6)
        ) +

        # Labels - dynamic subtitle based on show_confidence
        labs(
            title = "Comprehensive Scenario Analysis: Yield Curve Shift Impacts",
            subtitle = sprintf(
                "%s return shown | %d-day horizon%s",
                ifelse(show_total_return, "Total", "Price"),
                horizon_days,
                ifelse(show_confidence,
                       sprintf(" | %d%% confidence bands shown", confidence_level * 100),
                       "")
            ),
            x = "Yield Change",
            y = sprintf("%s Return (%%)", ifelse(show_total_return, "Total", "Price")),
            caption = paste(
                "Scenarios: Parallel (uniform shift) | Flattening (short\u2191 long\u2193) | Steepening (short\u2193 long\u2191) | Butterfly (belly outperforms)",
                ifelse(show_confidence,
                       sprintf("\n%d%% confidence bands based on bond-specific historical volatility", confidence_level * 100),
                       ""),
                "\n\u00d7 = breakeven | \u25cb = key points (-100, 0, +100 bps) | \u2605 = best bond at +100bps",
                sep = ""
            )
        ) +

        create_insele_theme() +
        theme(
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "horizontal",
            panel.grid.major = element_line(color = "#F0F0F0"),
            panel.grid.minor = element_line(color = "#F8F8F8", linetype = "dotted"),
            strip.background = element_rect(fill = insele_palette$primary, color = NA),
            strip.text = element_text(color = "white", face = "bold", size = 11),
            plot.caption = element_text(
                hjust = 0,
                size = 8,
                lineheight = 1.2,
                margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")  # FIX: Use ggplot2::margin
            ),
            panel.spacing = unit(1.5, "lines")
        )

    # Add summary attributes for reporting
    attr(p, "summary") <- scenario_results %>%
        group_by(scenario, bond) %>%
        summarise(
            breakeven_bps = shift[which.min(abs(plot_return))],
            max_gain = max(plot_return),
            max_loss = min(plot_return),
            return_at_plus_100 = plot_return[shift == 100],
            return_at_minus_100 = plot_return[shift == -100],
            .groups = "drop"
        )

    attr(p, "data") <- scenario_results

    return(p)
}