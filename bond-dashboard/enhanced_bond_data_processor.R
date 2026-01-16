# Enhanced SA Government Bond Data Processing Pipeline
# Institutional-Grade Analytics with Advanced Statistical Methods

# Load comprehensive package suite
pacman::p_load(
    readxl, dplyr, tidyverse, lubridate, TTR, forecast, memoise,
    RColorBrewer, scales, splines, zoo,
    ggrepel, ggiraph, ggtext, shadowtext,  # Enhanced visualization
    quantmod, PerformanceAnalytics,         # Financial analytics
    tseries, rugarch, rmgarch,              # Time series & GARCH
    isotree, dbscan,                        # Anomaly detection
    boot, parallel,
    doParallel,                         # Bootstrap & parallel processing
    YieldCurve,trend, #termstrc,                   # Yield curve modeling
    htmlwidgets, plotly                     # Interactive elements
)

# Configure parallel processing
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# ============================================================================
# ENHANCED CONFIGURATION SYSTEM
# ============================================================================

# Centralized visual configuration
visual_config <- list(
    # Sophisticated color palette - Bloomberg-inspired with perceptual uniformity
    colors = list(
        primary = "#003366",      # Deep navy
        secondary = "#0066CC",    # Bright blue
        accent = "#FF6B35",       # Orange accent
        positive = "#00C853",     # Green for positive
        negative = "#D32F2F",     # Red for negative
        warning = "#FFA000",      # Amber warning
        neutral = "#546E7A",      # Blue-grey neutral
        gradient_start = "#E3F2FD",
        gradient_end = "#0D47A1",
        background = "#FAFAFA",
        grid = "#E0E0E0",
        text_primary = "#212121",
        text_secondary = "#757575"
    ),

    # Typography hierarchy
    fonts = list(
        family_primary = "Helvetica Neue",
        family_mono = "Consolas",
        size_title = 14,
        size_subtitle = 11,
        size_axis = 10,
        size_annotation = 9,
        size_caption = 8,
        weight_title = "bold",
        weight_subtitle = "normal"
    ),

    # Grid and layout parameters
    grid = list(
        major_alpha = 0.3,
        minor_alpha = 0.1,
        major_size = 0.5,
        minor_size = 0.25
    ),

    # Animation parameters
    animation = list(
        transition_duration = 500,
        easing = "cubic-in-out"
    )
)

# Market regime detection configuration
regime_config <- list(
    vol_threshold_high = 0.75,    # 75th percentile for high volatility
    vol_threshold_low = 0.25,     # 25th percentile for low volatility
    trend_window = 20,             # Days for trend detection
    regime_window = 60,            # Days for regime classification
    min_regime_duration = 5        # Minimum days to confirm regime change
)

# ============================================================================
# SOPHISTICATED THEME SYSTEM
# ============================================================================

create_bloomberg_theme <- function(config = visual_config) {
    theme_minimal() +
        theme(
            # Typography
            text = element_text(
                family = config$fonts$family_primary,
                color = config$colors$text_primary
            ),
            plot.title = element_text(
                size = config$fonts$size_title,
                face = config$fonts$weight_title,
                color = config$colors$primary,
                margin = ggplot2::margin(b = 5)
            ),
            plot.subtitle = element_text(
                size = config$fonts$size_subtitle,
                color = config$colors$text_secondary,
                margin = ggplot2::margin(b = 10)
            ),
            plot.caption = element_text(
                size = config$fonts$size_caption,
                color = config$colors$text_secondary,
                hjust = 1,
                margin = ggplot2::margin(t = 10)
            ),

            # Grid system with integrated alpha in color
            panel.grid.major = element_line(
                colour = alpha(config$colors$grid, config$grid$major_alpha),
                linewidth = config$grid$major_size
            ),
            panel.grid.minor = element_line(
                colour = alpha(config$colors$grid, config$grid$minor_alpha),
                linewidth = config$grid$minor_size
            ),

            # Axis styling
            axis.text = element_text(
                size = config$fonts$size_axis,
                color = config$colors$text_secondary
            ),
            axis.title = element_text(
                size = config$fonts$size_subtitle,
                face = "bold",
                color = config$colors$text_primary
            ),
            axis.ticks = element_line(
                colour = config$colors$grid,
                linewidth = 0.5
            ),

            # Legend optimization
            legend.position = "bottom",
            legend.background = element_rect(
                fill = alpha(config$colors$background, 0.95),
                color = NA
            ),
            legend.key = element_blank(),
            legend.title = element_text(
                size = config$fonts$size_axis,
                face = "bold"
            ),
            legend.text = element_text(
                size = config$fonts$size_annotation
            ),

            # Panel styling
            panel.background = element_rect(
                fill = config$colors$background,
                color = NA
            ),
            plot.background = element_rect(
                fill = config$colors$background,
                color = NA
            ),

            # Facet styling for multi-panel plots
            strip.background = element_rect(
                fill = alpha(config$colors$primary, 0.1),
                color = config$colors$primary
            ),
            strip.text = element_text(
                face = "bold",
                size = config$fonts$size_axis,
                color = config$colors$primary
            )
        )
}

# Set global theme
bloomberg_theme <- create_bloomberg_theme()

# ============================================================================
# ADVANCED YIELD CURVE MODELING
# ============================================================================

# Nelson-Siegel-Svensson model implementation
fit_nss_model <- function(data, method = "nss") {
    tryCatch({
        # Prepare data for NSS fitting
        maturity_vec <- data$mod_dur
        yield_vec <- data$ytm / 100  # Convert to decimal

        if (method == "nss") {
            # Nelson-Siegel-Svensson with 6 parameters
            # y(t) = β0 + β1*f1(t,λ1) + β2*f2(t,λ1) + β3*f3(t,λ2)

            # Initial parameter guess
            beta0 <- mean(yield_vec)
            beta1 <- -2
            beta2 <- 0
            beta3 <- 0
            lambda1 <- 2
            lambda2 <- 5

            # Objective function
            nss_objective <- function(params) {
                b0 <- params[1]
                b1 <- params[2]
                b2 <- params[3]
                b3 <- params[4]
                l1 <- exp(params[5])  # Ensure positive
                l2 <- exp(params[6])  # Ensure positive

                fitted <- b0 +
                    b1 * (1 - exp(-maturity_vec/l1)) / (maturity_vec/l1) +
                    b2 * ((1 - exp(-maturity_vec/l1)) / (maturity_vec/l1) - exp(-maturity_vec/l1)) +
                    b3 * ((1 - exp(-maturity_vec/l2)) / (maturity_vec/l2) - exp(-maturity_vec/l2))

                sum((yield_vec - fitted)^2)
            }

            # Optimize
            opt_result <- optim(
                par = c(beta0, beta1, beta2, beta3, log(lambda1), log(lambda2)),
                fn = nss_objective,
                method = "L-BFGS-B"
            )

            # Extract parameters
            params <- opt_result$par
            params[5:6] <- exp(params[5:6])  # Convert back lambdas

            # Generate fitted curve
            fitted_yields <- params[1] +
                params[2] * (1 - exp(-maturity_vec/params[5])) / (maturity_vec/params[5]) +
                params[3] * ((1 - exp(-maturity_vec/params[5])) / (maturity_vec/params[5]) - exp(-maturity_vec/params[5])) +
                params[4] * ((1 - exp(-maturity_vec/params[6])) / (maturity_vec/params[6]) - exp(-maturity_vec/params[6]))

            data$nss_fitted <- fitted_yields * 100  # Convert back to percentage
            data$nss_spread <- data$ytm - data$nss_fitted

            # Add model parameters as attributes
            attr(data, "nss_params") <- list(
                beta0 = params[1],
                beta1 = params[2],
                beta2 = params[3],
                beta3 = params[4],
                lambda1 = params[5],
                lambda2 = params[6],
                rmse = sqrt(opt_result$value / length(yield_vec))
            )
        }

        return(data)
    }, error = function(e) {
        message("NSS fitting failed: ", e$message)
        return(data)
    })
}

# Principal Component Analysis for curve movements
perform_curve_pca <- function(historical_data, n_components = 3) {
    tryCatch({
        # Prepare data matrix
        yield_matrix <- historical_data %>%
            select(date, bond, ytm) %>%
            pivot_wider(names_from = bond, values_from = ytm) %>%
            select(-date) %>%
            as.matrix()

        # Remove NA rows
        complete_rows <- complete.cases(yield_matrix)
        yield_matrix <- yield_matrix[complete_rows, ]

        # Calculate returns
        yield_changes <- diff(yield_matrix)

        # Perform PCA
        pca_result <- prcomp(yield_changes, center = TRUE, scale = TRUE)

        # Extract components
        components <- list(
            loadings = pca_result$rotation[, 1:n_components],
            scores = pca_result$x[, 1:n_components],
            variance_explained = summary(pca_result)$importance[2, 1:n_components],
            interpretation = c(
                "Level" = "Parallel shift in yields",
                "Slope" = "Steepening/Flattening",
                "Curvature" = "Butterfly/Barbell"
            )
        )

        return(components)
    }, error = function(e) {
        message("PCA failed: ", e$message)
        return(NULL)
    })
}

# ============================================================================
# SOPHISTICATED CARRY & ROLL CALCULATIONS
# ============================================================================

calculate_advanced_carry_roll <- memoise(function(data,
                                                  holding_periods = c(30, 90, 180, 360),
                                                  n_simulations = 1000,
                                                  confidence_level = 0.95) {
    tryCatch({
        results <- list()

        for (period in holding_periods) {
            period_label <- paste0(period, "d")

            # Bootstrap forward curve projections
            bootstrap_results <- foreach(i = 1:n_simulations, .combine = rbind) %dopar% {
                # Simulate yield curve evolution
                simulated_data <- data %>%
                    group_by(bond) %>%
                    filter(date == max(date)) %>%
                    mutate(
                        # Stochastic yield evolution using Vasicek model
                        drift = 0.01,  # Mean reversion level
                        vol = 0.02,    # Volatility
                        dt = period / 365,
                        dW = rnorm(n(), 0, sqrt(dt)),

                        # Project yield forward
                        forward_yield = ytm +
                            drift * (8.5 - ytm) * dt +
                            vol * sqrt(ytm) * dW,

                        # Calculate carry component (cpn accrual)
                        carry_component = cpn * period / 365,

                        # Calculate roll-down with term structure
                        time_decay = period / 365,
                        new_duration = pmax(0, mod_dur - time_decay),

                        # Forward curve roll-down
                        roll_component = ifelse(
                            new_duration > 0,
                            (ytm - forward_yield) * new_duration,
                            0
                        ),

                        # Pull-to-par effect for bonds approaching maturity
                        pull_to_par = ifelse(
                            time_to_maturity < 1,
                            (100 - clean_price) * (period / 365) / time_to_maturity,
                            0
                        ),

                        # Total return calculation
                        total_return = carry_component + roll_component +
                            ifelse(!is.na(pull_to_par), pull_to_par, 0),

                        # Risk metrics
                        return_volatility = abs(dW) * vol * sqrt(mod_dur),
                        sharpe_ratio = total_return / (return_volatility + 0.0001),

                        simulation = i
                    )

                return(simulated_data)
            }

            # Aggregate simulation results
            aggregated <- bootstrap_results %>%
                group_by(bond) %>%
                summarise(
                    mean_carry = mean(carry_component, na.rm = TRUE),
                    mean_roll = mean(roll_component, na.rm = TRUE),
                    mean_total_return = mean(total_return, na.rm = TRUE),

                    # Confidence intervals
                    ci_lower = quantile(total_return, (1 - confidence_level) / 2, na.rm = TRUE),
                    ci_upper = quantile(total_return, (1 + confidence_level) / 2, na.rm = TRUE),

                    # Risk metrics
                    return_std = sd(total_return, na.rm = TRUE),
                    var_95 = quantile(total_return, 0.05, na.rm = TRUE),
                    cvar_95 = mean(total_return[total_return <= var_95], na.rm = TRUE),

                    # Sharpe ratio
                    mean_sharpe = mean(sharpe_ratio, na.rm = TRUE),

                    # Breakeven calculation with confidence
                    breakeven_bps = mean_total_return / mean(mod_dur) * 100,
                    breakeven_ci_lower = ci_lower / mean(mod_dur) * 100,
                    breakeven_ci_upper = ci_upper / mean(mod_dur) * 100,

                    holding_period = period_label,
                    holding_days = period,

                    .groups = "drop"
                )

            results[[length(results) + 1]] <- aggregated
        }

        # Combine results
        combined <- bind_rows(results)

        # Add regime-conditional adjustments
        combined <- combined %>%
            mutate(
                regime = detect_market_regime(data),
                regime_adjustment = case_when(
                    regime == "high_volatility" ~ mean_total_return * 0.8,
                    regime == "trending_up" ~ mean_total_return * 1.1,
                    regime == "trending_down" ~ mean_total_return * 0.9,
                    TRUE ~ mean_total_return
                )
            )

        return(combined)
    }, error = function(e) {
        message("Advanced carry & roll calculation failed: ", e$message)
        return(data.frame())
    })
})

# ============================================================================
# MARKET REGIME DETECTION
# ============================================================================

detect_market_regime <- function(data, config = regime_config) {
    tryCatch({
        recent_data <- data %>%
            arrange(date) %>%
            tail(config$regime_window)

        if (nrow(recent_data) < config$regime_window) {
            return("insufficient_data")
        }

        # Calculate regime indicators
        returns <- diff(log(recent_data$ytm + 1))
        volatility <- sd(returns, na.rm = TRUE)

        # Historical volatility percentiles
        hist_vol <- rollapply(returns, width = 20, FUN = sd, fill = NA, align = "right")
        vol_percentile <- ecdf(hist_vol)(volatility)

        # Trend detection using multiple methods
        trend_lm <- lm(ytm ~ as.numeric(date), data = recent_data)
        trend_slope <- coef(trend_lm)[2]

        # Mann-Kendall trend test
        mk_test <- trend::mk.test(recent_data$ytm)

        # Regime classification
        regime <- case_when(
            vol_percentile > config$vol_threshold_high ~ "high_volatility",
            vol_percentile < config$vol_threshold_low ~ "low_volatility",
            trend_slope > 0.01 & mk_test$p.value < 0.05 ~ "trending_up",
            trend_slope < -0.01 & mk_test$p.value < 0.05 ~ "trending_down",
            TRUE ~ "range_bound"
        )

        return(regime)
    }, error = function(e) {
        message("Regime detection failed: ", e$message)
        return("unknown")
    })
}

# ============================================================================
# ANOMALY DETECTION SYSTEM
# ============================================================================

detect_anomalies <- function(data, contamination = 0.05) {
    tryCatch({
        # Prepare features for anomaly detection
        features <- data %>%
            select_if(is.numeric) %>%
            as.matrix()

        # Isolation Forest (can handle NAs)
        iso_model <- isotree::isolation.forest(
            features,
            ntrees = 500,
            sample_size = 256,
            seed = 123
        )

        # Get anomaly scores from Isolation Forest
        anomaly_scores <- predict(iso_model, features)

        # Track which rows have complete cases for LOF
        complete_rows <- complete.cases(features)
        features_clean <- features[complete_rows, ]

        # Initialize LOF scores with NA for all rows
        lof_scores_full <- rep(NA, nrow(data))

        # Compute LOF only on clean data
        if(sum(complete_rows) >= 5) {  # Need at least minPts observations
            lof_scores <- lof(features_clean, minPts = 5)
            # Assign LOF scores to their correct positions
            lof_scores_full[complete_rows] <- lof_scores
        }

        # Add scores to data
        data$anomaly_score <- anomaly_scores
        data$lof_score <- lof_scores_full

        # Determine anomalies based on isolation forest scores
        anomaly_threshold <- quantile(anomaly_scores, 1 - contamination, na.rm = TRUE)
        data$is_anomaly <- anomaly_scores > anomaly_threshold

        # Optional: Combined anomaly flag using both methods
        lof_threshold <- quantile(lof_scores_full, 0.95, na.rm = TRUE)
        data$lof_anomaly <- !is.na(lof_scores_full) & lof_scores_full > lof_threshold

        # Combined anomaly (either method flags it)
        data$combined_anomaly <- data$is_anomaly | (!is.na(data$lof_anomaly) & data$lof_anomaly)

        # Calculate statistics for anomaly classification
        # Using group_by to avoid issues with global mean/sd
        stats <- data %>%
            summarise(
                mean_ytm = mean(ytm, na.rm = TRUE),
                sd_ytm = sd(ytm, na.rm = TRUE),
                mean_btc = mean(bid_to_cover, na.rm = TRUE),
                sd_btc = sd(bid_to_cover, na.rm = TRUE)
            )

        # Classify anomaly types
        data <- data %>%
            mutate(
                anomaly_type = case_when(
                    is_anomaly & ytm > stats$mean_ytm + 2*stats$sd_ytm ~ "yield_spike",
                    is_anomaly & ytm < stats$mean_ytm - 2*stats$sd_ytm ~ "yield_drop",
                    is_anomaly & !is.na(bid_to_cover) &
                        bid_to_cover > stats$mean_btc + 2*stats$sd_btc ~ "demand_surge",
                    is_anomaly & !is.na(bid_to_cover) &
                        bid_to_cover < stats$mean_btc - 2*stats$sd_btc ~ "demand_collapse",
                    is_anomaly ~ "multi_factor",
                    TRUE ~ "normal"
                ),
                anomaly_severity = case_when(
                    anomaly_score > quantile(anomaly_scores, 0.99, na.rm = TRUE) ~ "critical",
                    anomaly_score > quantile(anomaly_scores, 0.97, na.rm = TRUE) ~ "high",
                    anomaly_score > quantile(anomaly_scores, 0.95, na.rm = TRUE) ~ "medium",
                    is_anomaly ~ "low",
                    TRUE ~ "none"
                ),
                # Add a flag for data completeness
                has_lof_score = !is.na(lof_score)
            )

        # Add summary message
        message(sprintf("Anomaly detection complete: %d anomalies detected (%.1f%%)",
                        sum(data$is_anomaly, na.rm = TRUE),
                        mean(data$is_anomaly, na.rm = TRUE) * 100))
        message(sprintf("LOF computed for %d/%d rows (%.1f%% complete cases)",
                        sum(!is.na(lof_scores_full)),
                        nrow(data),
                        sum(complete_rows) / nrow(data) * 100))

        return(data)

    }, error = function(e) {
        message("Anomaly detection failed: ", e$message)
        # Return data with NA columns to maintain structure
        data$anomaly_score <- NA
        data$lof_score <- NA
        data$is_anomaly <- FALSE
        data$anomaly_type <- "unknown"
        data$anomaly_severity <- "none"
        return(data)
    })
}

# ============================================================================
# DATA QUALITY SCORING SYSTEM
# ============================================================================

calculate_data_quality_score <- function(data) {
    # Comprehensive data quality metrics
    quality_metrics <- list(
        completeness = sum(!is.na(data$ytm)) / nrow(data),

        consistency = {
            # Check for logical consistency
            consistent_duration <- sum(data$mod_dur >= 0, na.rm = TRUE) / sum(!is.na(data$mod_dur))
            consistent_conv <- sum(data$conv >= 0, na.rm = TRUE) / sum(!is.na(data$conv))
            consistent_yields <- sum(data$ytm > 0 & data$ytm < 50, na.rm = TRUE) / sum(!is.na(data$ytm))
            mean(c(consistent_duration, consistent_conv, consistent_yields))
        },

        timeliness = {
            days_since_update <- as.numeric(Sys.Date() - max(data$date))
            case_when(
                days_since_update <= 1 ~ 1.0,
                days_since_update <= 7 ~ 0.8,
                days_since_update <= 30 ~ 0.6,
                TRUE ~ 0.4
            )
        },

        accuracy = {
            # Check for outliers using MAD
            mad_score <- function(x) {
                med <- median(x, na.rm = TRUE)
                mad <- median(abs(x - med), na.rm = TRUE)
                sum(abs(x - med) <= 3 * mad, na.rm = TRUE) / sum(!is.na(x))
            }
            mad_score(data$ytm)
        }
    )

    # Calculate overall score
    overall_score <- weighted.mean(
        unlist(quality_metrics),
        weights = c(0.3, 0.3, 0.2, 0.2)
    )

    # Add quality grade
    quality_grade <- case_when(
        overall_score >= 0.95 ~ "A+",
        overall_score >= 0.90 ~ "A",
        overall_score >= 0.85 ~ "B+",
        overall_score >= 0.80 ~ "B",
        overall_score >= 0.75 ~ "C+",
        overall_score >= 0.70 ~ "C",
        TRUE ~ "D"
    )

    return(list(
        score = overall_score,
        grade = quality_grade,
        metrics = quality_metrics,
        recommendations = generate_quality_recommendations(quality_metrics)
    ))
}

generate_quality_recommendations <- function(metrics) {
    recommendations <- c()

    if (metrics$completeness < 0.9) {
        recommendations <- c(recommendations,
                             "Data completeness below 90% - investigate missing yield data sources")
    }

    if (metrics$consistency < 0.95) {
        recommendations <- c(recommendations,
                             "Logical inconsistencies detected - review data validation rules")
    }

    if (metrics$timeliness < 0.8) {
        recommendations <- c(recommendations,
                             "Data staleness detected - update data feeds more frequently")
    }

    if (metrics$accuracy < 0.95) {
        recommendations <- c(recommendations,
                             "Potential outliers detected - implement outlier review process")
    }

    return(recommendations)
}

# ============================================================================
# ENHANCED DATA PROCESSING PIPELINE
# ============================================================================

# Main processing function with all enhancements
process_bond_data_advanced <- function(file_path = "data/Siyanda Bonds.xlsx") {

    # Start processing timer
    start_time <- Sys.time()

    message("\n╔════════════════════════════════════════════════════════════════╗")
    message("║  INITIATING ADVANCED BOND ANALYTICS PROCESSING PIPELINE       ║")
    message("╚════════════════════════════════════════════════════════════════╝\n")

    # Set data parameters
    last_date <- today()

    # Check if running in Shiny context
    in_shiny <- shiny::isRunning() || (!is.null(shiny::getDefaultReactiveDomain()))

    # Progress tracking function
    update_progress <- if(in_shiny) {
        function(amount, detail = NULL) {
            incProgress(amount, detail = detail)
        }
    } else {
        function(amount, detail = NULL) {
            if(!is.null(detail)) {
                message("► ", detail)
            }
        }
    }

    # Wrap main processing in appropriate progress handler
    process_wrapper <- if(in_shiny) {
        function(expr) {
            withProgress(message = 'Processing bond data', value = 0, expr)
        }
    } else {
        function(expr) expr
    }

    # Main processing
    process_wrapper({

        # Step 1: Load and validate data
        update_progress(0.1, detail = "Loading data from Excel...")

        # Function to dynamically detect available bonds
        detect_available_bonds <- function(df) {
            cols <- names(df)
            cols[!cols %in% c("date")]
        }

        # Load all data sheets with error handling
        sheets_to_load <- c("mod_dur", "ytm", "dur", "conv", "cpn", "accrued",
                            "clean_price", "full_price", "bpv", "auctions")

        loaded_data <- list()

        for (sheet in sheets_to_load) {
            tryCatch({
                update_progress(0.05, detail = paste("Loading", sheet, "sheet..."))

                # CRITICAL: guess_max scans all rows to correctly type columns with late-starting data
                df <- read_excel(file_path, sheet = sheet, guess_max = 21474836)

                if (sheet == "mod_dur") {
                    available_bonds <- detect_available_bonds(df)
                    message("✓ Detected ", length(available_bonds), " bonds: ",
                            paste(available_bonds, collapse = ", "))
                }

                # Process based on sheet type
                if (sheet == "auctions") {
                    df <- df %>%
                        mutate(
                            announcement_date = lubridate::ymd(announce_date),
                            offer_date = lubridate::ymd(offer_date),
                            settle_date = lubridate::ymd(sett_date),
                            mature_date = lubridate::ymd(mat_date)
                        ) %>%
                        select(offer_date, bond, offer, allocation, bids, bid_to_cover,
                               announcement_date, settle_date, mature_date) %>%
                        mutate(date = offer_date)
                } else if (sheet == "cpn") {
                    df <- df  %>%
                        select(date, all_of(available_bonds)) %>%
                        mutate(date = lubridate::ymd(date)) %>%
                        pivot_longer(-date, names_to = "bond", values_to = sheet) %>%
                        filter(!is.na(!!sym(sheet))) %>%
                        group_by(bond) %>%
                        distinct(!!sym(sheet)) %>%
                        ungroup()
                } else {
                    df <- df %>%
                        select(date, all_of(available_bonds)) %>%
                        mutate(date = lubridate::ymd(date)) %>%
                        pivot_longer(-date, names_to = "bond", values_to = sheet)
                }

                loaded_data[[sheet]] <- df

            }, error = function(e) {
                message("⚠ Optional sheet '", sheet, "' not found - skipping")
            })
        }

        # Step 2: Combine all data
        update_progress(0.1, detail = "Merging datasets...")

        full_df <- loaded_data[["mod_dur"]]
        for (sheet_name in names(loaded_data)[-1]) {
            if (!is.null(loaded_data[[sheet_name]])) {
                if (sheet_name == "cpn") {
                    full_df <- left_join(full_df, loaded_data[[sheet_name]], by = "bond")
                } else {
                    full_df <- left_join(full_df, loaded_data[[sheet_name]], by = c("date", "bond"))
                }
            }
        }

        # Step 3: Add calculated fields
        update_progress(0.1, detail = "Calculating derived metrics...")

        full_df <- full_df %>%
            mutate(
                # DV01 calculation
                dv01 = if("bpv" %in% names(.)) bpv else mod_dur * 0.01,

                # Time to maturity
                time_to_maturity = as.numeric(difftime(mature_date, date, units = "days")) / 365.25,

                # Maturity buckets with finer granularity
                maturity_bucket = case_when(
                    time_to_maturity <= 1 ~ "Ultra-Short",
                    time_to_maturity <= 3 ~ "Short",
                    time_to_maturity <= 5 ~ "Short-Medium",
                    time_to_maturity <= 7 ~ "Medium",
                    time_to_maturity <= 10 ~ "Medium-Long",
                    time_to_maturity <= 15 ~ "Long",
                    time_to_maturity <= 20 ~ "Ultra-Long",
                    time_to_maturity > 20 ~ "Super-Long",
                    TRUE ~ "Unknown"
                ),

                # Enhanced auction metrics
                auction_success = case_when(
                    bid_to_cover >= 3.5 ~ "Exceptional",
                    bid_to_cover >= 2.5 ~ "Strong",
                    bid_to_cover >= 2.0 ~ "Success",
                    bid_to_cover >= 1.5 ~ "Moderate",
                    bid_to_cover >= 1.0 ~ "Weak",
                    bid_to_cover < 1.0 ~ "Failed",
                    TRUE ~ NA_character_
                ),

                # Z-spread approximation (simplified)
                z_spread = (ytm - 7.5) * 100,  # Basis points above risk-free

                # OAS approximation (simplified - would need option model)
                oas = z_spread - (0.1 * conv),  # Rough approximation

                # Yield percentile within historical range
                yield_percentile = percent_rank(ytm)
            )

        # Step 4: Advanced analytics
        update_progress(0.1, detail = "Performing Nelson-Siegel-Svensson fitting...")

        # Get latest data for NSS fitting
        latest_data <- full_df %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            ungroup()

        latest_data <- fit_nss_model(latest_data)

        # Step 5: PCA analysis
        update_progress(0.1, detail = "Running PCA on yield curves...")

        pca_results <- perform_curve_pca(full_df)

        # Step 6: Anomaly detection
        update_progress(0.1, detail = "Detecting anomalies with Isolation Forest...")

        full_df <- detect_anomalies(full_df)

        # Step 7: Market regime detection
        update_progress(0.1, detail = "Classifying market regime...")

        current_regime <- detect_market_regime(full_df)

        # Step 8: Data quality assessment
        update_progress(0.1, detail = "Calculating data quality scores...")

        quality_report <- calculate_data_quality_score(full_df)

        # Step 9: Create enhanced metadata
        update_progress(0.05, detail = "Generating comprehensive metadata...")

        bond_metadata <- full_df %>%
            group_by(bond) %>%
            summarise(
                cpn = dplyr::first(cpn[!is.na(cpn)]),
                avg_duration = mean(mod_dur, na.rm = TRUE),
                duration_volatility = sd(mod_dur, na.rm = TRUE),
                avg_conv = mean(conv, na.rm = TRUE),
                latest_ytm = dplyr::last(ytm[!is.na(ytm)]),
                ytm_1m_change = latest_ytm - nth(ytm, -20, default = latest_ytm),
                ytm_3m_change = latest_ytm - nth(ytm, -60, default = latest_ytm),
                first_date = min(date),
                last_date = max(date),
                maturity = dplyr::first(mature_date[!is.na(mature_date)]),
                total_auctions = sum(!is.na(bid_to_cover)),
                avg_bid_cover = mean(bid_to_cover, na.rm = TRUE),
                bid_cover_trend = {
                    if(sum(!is.na(bid_to_cover)) > 2) {
                        # Option 1: Remove na.rm (lm handles NAs automatically)
                        coef(lm(bid_to_cover ~ as.numeric(date)))[2]

                        # Option 2: Explicitly filter NAs if you want to be clear
                        # valid_data <- !is.na(bid_to_cover)
                        # coef(lm(bid_to_cover[valid_data] ~ as.numeric(date[valid_data])))[2]
                    } else {
                        NA
                    }
                },
                data_completeness = sum(!is.na(ytm)) / n() * 100,
                anomaly_count = sum(is_anomaly, na.rm = TRUE),
                avg_z_spread = mean(z_spread, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            arrange(avg_duration) %>%
            mutate(
                duration_bucket = cut(avg_duration,
                                      breaks = c(0, 3, 5, 7, 10, 15, Inf),
                                      labels = c("Ultra-Short", "Short", "Short-Medium",
                                                 "Medium", "Long", "Ultra-Long")),
                trend_signal = case_when(
                    ytm_1m_change > 20 ~ "Sharp Rise",
                    ytm_1m_change > 10 ~ "Rising",
                    ytm_1m_change < -20 ~ "Sharp Fall",
                    ytm_1m_change < -10 ~ "Falling",
                    TRUE ~ "Stable"
                )
            )

        # Step 10: Save all processed data
        update_progress(0.05, detail = "Saving enhanced datasets...")

        saveRDS(full_df, "data/processed_bond_data_advanced.rds")
        saveRDS(bond_metadata, "data/bond_metadata_advanced.rds")
        saveRDS(latest_data, "data/latest_data_nss.rds")
        saveRDS(pca_results, "data/pca_results.rds")
        saveRDS(list(regime = current_regime, timestamp = Sys.time()), "data/market_regime.rds")
        saveRDS(quality_report, "data/data_quality_report.rds")

        # Create timestamped backup
        backup_dir <- "data/backups"
        if (!dir.exists(backup_dir)) dir.create(backup_dir)
        saveRDS(full_df, file.path(backup_dir,
                                   paste0("bond_data_advanced_", format(Sys.Date(), "%Y%m%d_%H%M%S"), ".rds")))

    })  # End process_wrapper

    # Calculate processing time
    processing_time <- difftime(Sys.time(), start_time, units = "secs")

    # Print comprehensive report
    cat("\n╔════════════════════════════════════════════════════════════════╗\n")
    cat("║           ADVANCED PROCESSING PIPELINE COMPLETE               ║\n")
    cat("╚════════════════════════════════════════════════════════════════╝\n\n")

    cat("═══ PROCESSING METRICS ═══════════════════════════════════════════\n")
    cat("• Processing Time: ", sprintf("%.2f", processing_time), " seconds\n")
    cat("• Total Observations: ", format(nrow(full_df), big.mark = ","), "\n")
    cat("• Unique Bonds: ", n_distinct(full_df$bond), "\n")
    cat("• Date Range: ", format(min(full_df$date)), " to ", format(max(full_df$date)), "\n")
    cat("• Current Market Regime: ", current_regime, "\n")

    cat("\n═══ DATA QUALITY REPORT ══════════════════════════════════════════\n")
    cat("• Overall Quality Score: ", sprintf("%.1f%%", quality_report$score * 100),
        " (Grade: ", quality_report$grade, ")\n")
    cat("• Completeness: ", sprintf("%.1f%%", quality_report$metrics$completeness * 100), "\n")
    cat("• Consistency: ", sprintf("%.1f%%", quality_report$metrics$consistency * 100), "\n")
    cat("• Timeliness: ", sprintf("%.1f%%", quality_report$metrics$timeliness * 100), "\n")
    cat("• Accuracy: ", sprintf("%.1f%%", quality_report$metrics$accuracy * 100), "\n")

    if (length(quality_report$recommendations) > 0) {
        cat("\n⚠ Quality Recommendations:\n")
        for (rec in quality_report$recommendations) {
            cat("  • ", rec, "\n")
        }
    }

    cat("\n═══ ANOMALY DETECTION SUMMARY ════════════════════════════════════\n")
    anomaly_summary <- full_df %>%
        filter(is_anomaly) %>%
        count(anomaly_type, anomaly_severity)

    if (nrow(anomaly_summary) > 0) {
        print(anomaly_summary)
    } else {
        cat("• No significant anomalies detected\n")
    }

    cat("\n═══ NSS MODEL FIT ═════════════════════════════════════════════════\n")
    if (!is.null(attr(latest_data, "nss_params"))) {
        nss_params <- attr(latest_data, "nss_params")
        cat("• β₀ (Level): ", sprintf("%.4f", nss_params$beta0), "\n")
        cat("• β₁ (Slope): ", sprintf("%.4f", nss_params$beta1), "\n")
        cat("• β₂ (Curvature): ", sprintf("%.4f", nss_params$beta2), "\n")
        cat("• β₃ (2nd Curvature): ", sprintf("%.4f", nss_params$beta3), "\n")
        cat("• λ₁: ", sprintf("%.4f", nss_params$lambda1), "\n")
        cat("• λ₂: ", sprintf("%.4f", nss_params$lambda2), "\n")
        cat("• RMSE: ", sprintf("%.2f bps", nss_params$rmse * 10000), "\n")
    }

    if (!is.null(pca_results)) {
        cat("\n═══ PCA RESULTS ═══════════════════════════════════════════════════\n")
        cat("• PC1 (Level) explains: ", sprintf("%.1f%%", pca_results$variance_explained[1] * 100), "\n")
        cat("• PC2 (Slope) explains: ", sprintf("%.1f%%", pca_results$variance_explained[2] * 100), "\n")
        cat("• PC3 (Curvature) explains: ", sprintf("%.1f%%", pca_results$variance_explained[3] * 100), "\n")
        cat("• Total variance explained: ", sprintf("%.1f%%", sum(pca_results$variance_explained) * 100), "\n")
    }

    cat("\n═══ FILES SAVED ═══════════════════════════════════════════════════\n")
    cat("✓ data/processed_bond_data_advanced.rds\n")
    cat("✓ data/bond_metadata_advanced.rds\n")
    cat("✓ data/latest_data_nss.rds\n")
    cat("✓ data/pca_results.rds\n")
    cat("✓ data/market_regime.rds\n")
    cat("✓ data/data_quality_report.rds\n")
    cat("✓ Backup created with timestamp\n")

    cat("\n╔════════════════════════════════════════════════════════════════╗\n")
    cat("║      🚀 SYSTEM READY FOR ADVANCED DASHBOARD DEPLOYMENT        ║\n")
    cat("╚════════════════════════════════════════════════════════════════╝\n")

    # Return comprehensive results
    return(list(
        data = full_df,
        metadata = bond_metadata,
        nss_data = latest_data,
        pca = pca_results,
        regime = current_regime,
        quality = quality_report,
        processing_time = processing_time
    ))
}

# Run the enhanced processing pipeline
results <- process_bond_data_advanced()

# Stop parallel cluster
stopCluster(cl)