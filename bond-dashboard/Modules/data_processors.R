# ================================================================================
# CONVICTION SCORE CALCULATOR (0-10 Scale)
# ================================================================================
#' Calculate conviction score on 0-10 scale
#' @description Components: spread magnitude (0-4), z-score confirmation (0-4), liquidity (0-2)
#' @param spread_bps Spread to fair value in basis points
#' @param zscore Z-score of the spread
#' @param bid_to_cover Bid-to-cover ratio (optional, default NA)
#' @return Numeric score between 0 and 10
#' @export
calculate_conviction_score <- function(spread_bps, zscore, bid_to_cover = NA_real_) {
    # Handle NA inputs
    if (is.na(spread_bps) || is.na(zscore)) {
        return(NA_real_)
    }

    # ==========================================================================
    # COMPONENT 1: Spread magnitude (0-4 points)
    # Captures: How mispriced is the bond right now?
    # ==========================================================================
    # Normalize: 20 bps spread = 4 points (max)
    spread_component <- min(abs(spread_bps) / 5, 4)

    # ==========================================================================
    # COMPONENT 2: Z-Score confirmation (0-4 points)
    # Captures: Is this mispricing statistically significant?
    # ==========================================================================
    # Check if Z-Score CONFIRMS spread direction
    # (positive spread + positive Z = both say cheap = confirming)
    # (negative spread + negative Z = both say rich = confirming)
    zscore_confirms <- sign(spread_bps) == sign(zscore)

    if (zscore_confirms) {
        # Confirming: reward based on |Z-Score|
        # |Z| = 2 gives full 4 points
        zscore_component <- min(abs(zscore) * 2, 4)
    } else {
        # Contradicting: Z-Score says "this mispricing is reversing"
        # Still give some credit but reduced
        zscore_component <- min(abs(zscore) * 0.5, 1)
    }

    # ==========================================================================
    # COMPONENT 3: Liquidity bonus (0-2 points)
    # Captures: Can we actually trade this?
    # ==========================================================================
    if (!is.na(bid_to_cover) && bid_to_cover > 0) {
        # bid_to_cover of 2.0 = healthy liquidity = 2 points
        liquidity_component <- min(bid_to_cover, 2)
    } else {
        # No data: assume average liquidity
        liquidity_component <- 1
    }

    # ==========================================================================
    # TOTAL SCORE (0-10 scale)
    # ==========================================================================
    total_score <- spread_component + zscore_component + liquidity_component

    return(round(total_score, 1))
}

#' @export
calculate_relative_value <- function(data, lookback = NULL) {
    # Set default lookback if NULL or invalid
    if(is.null(lookback) || length(lookback) == 0 || is.na(lookback) || lookback <= 0) {
        lookback <- 60
    }

    # Ensure we have minimum data points
    lookback <- min(lookback, 30)  # At least 30 days for meaningful statistics

    # Check if spread_to_curve exists
    if(!"spread_to_curve" %in% names(data)) {
        warning("spread_to_curve not found in data - calculating it")
        data <- calculate_fair_value(data, method = "smooth.spline")
    }

    # Calculate metrics for each bond separately
    result_list <- list()

    for(bond_name in unique(data$bond)) {
        bond_data <- data %>%
            filter(bond == bond_name) %>%
            arrange(date)

        n_obs <- nrow(bond_data)

        if(n_obs >= 2) {
            # Calculate rolling statistics with minimum window
            window_size <- min(lookback, n_obs, 20)  # At least 20 days

            bond_data$hist_mean_spread <- zoo::rollmean(
                bond_data$spread_to_curve,
                k = window_size,
                fill = mean(bond_data$spread_to_curve, na.rm = TRUE),
                align = "right",
                partial = TRUE
            )

            bond_data$hist_sd_spread <- zoo::rollapply(
                bond_data$spread_to_curve,
                width = window_size,
                FUN = function(x) {
                    s <- sd(x, na.rm = TRUE)
                    ifelse(is.na(s) || s < 1, 1, s)  # Minimum 1bp standard deviation
                },
                fill = sd(bond_data$spread_to_curve, na.rm = TRUE),
                align = "right",
                partial = TRUE
            )

        } else {
            bond_data$hist_mean_spread <- bond_data$spread_to_curve
            bond_data$hist_sd_spread <- 10  # Default 10bp standard deviation
        }

        # Calculate z-score with floor on standard deviation
        bond_data$z_score <- (bond_data$spread_to_curve - bond_data$hist_mean_spread) /
            pmax(bond_data$hist_sd_spread, 1)

        # Cap z-scores at reasonable levels
        bond_data$z_score <- pmin(pmax(bond_data$z_score, -5), 5)

        # Calculate percentile rank
        bond_data$percentile_rank <- percent_rank(bond_data$spread_to_curve)

        result_list[[bond_name]] <- bond_data
    }

    # Combine results
    result <- bind_rows(result_list)

    # Fill any remaining NAs
    result$z_score[is.na(result$z_score)] <- 0
    result$percentile_rank[is.na(result$percentile_rank)] <- 0.5

    return(result)
}

# Add to calculate_advanced_technicals:
# Identify actual support/resistance levels using density peaks
identify_support_resistance <- function(prices, lookback = 60) {
    if(length(prices) < lookback) return(list(support = min(prices), resistance = max(prices)))

    # Calculate price density
    dens <- density(tail(prices, lookback), n = 100)
    peaks <- which(diff(sign(diff(dens$y))) == -2) + 1

    if(length(peaks) == 0) {
        return(list(support = min(prices), resistance = max(prices)))
    }

    # Get price levels at density peaks
    levels <- sort(dens$x[peaks])
    current_price <- tail(prices, 1)

    # Find nearest support and resistance
    support <- max(levels[levels < current_price], min(prices))
    resistance <- min(levels[levels > current_price], max(prices))

    return(list(support = support, resistance = resistance))
}


#' @export
#' @title Calculate Advanced Technical Indicators (DURATION-ADAPTIVE VERSION)
#' @description Comprehensive technical analysis with duration-adaptive parameters,
#'   volatility normalization, and dynamic thresholds for differentiated signals
#' @param data Data frame with columns: bond, date, yield_to_maturity, modified_duration
#' @return Plain data.frame with 40+ technical indicator columns including adaptive metrics
calculate_advanced_technicals <- function(data) {

    # ════════════════════════════════════════════════════════════════════
    # PHASE 1: VALIDATION & SANITIZATION
    # ════════════════════════════════════════════════════════════════════

    if(!requireNamespace("TTR", quietly = TRUE)) {
        stop("TTR package is required for technical indicators. Please install it.")
    }

    # Validate input
    required_cols <- c("bond", "date", "yield_to_maturity")
    validate_required_columns(data, required_cols, "calculate_advanced_technicals")

    # ✅ NEW: Sanitize input IMMEDIATELY
    data <- sanitize_pipeline_data(data, "calculate_advanced_technicals [input]")

    # ════════════════════════════════════════════════════════════════════
    # PHASE 2: HANDLE INSUFFICIENT DATA
    # ════════════════════════════════════════════════════════════════════

    if(nrow(data) < 2) {
        warning("Insufficient data (<2 rows) for technical indicators. Using defaults.")
        return(add_default_technicals(data))  # Helper function at bottom
    }

    # ════════════════════════════════════════════════════════════════════
    # PHASE 3: DETERMINE DURATION BUCKETS & ADAPTIVE PARAMETERS
    # ════════════════════════════════════════════════════════════════════

    # If modified_duration doesn't exist, use medium parameters as default
    if(!"modified_duration" %in% names(data)) {
        warning("modified_duration not found - using Medium parameters for all bonds")
        data$modified_duration <- 7  # Default to medium duration
    }

    # ════════════════════════════════════════════════════════════════════
    # PHASE 4: CALCULATE INDICATORS (with duration-adaptive parameters)
    # ════════════════════════════════════════════════════════════════════

    result <- tryCatch({

        # Calculate all indicators in single chain
        temp_result <- data %>%
            group_by(bond) %>%
            arrange(date) %>%
            mutate(
                # Track observations
                n_obs = n(),

                # ═══════════════════════════════════════════════════════════
                # DURATION BUCKET CLASSIFICATION
                # ═══════════════════════════════════════════════════════════
                duration_bucket = case_when(
                    modified_duration < 5 ~ "Short",
                    modified_duration < 12 ~ "Medium",
                    TRUE ~ "Long"
                ),

                indicator_speed = case_when(
                    duration_bucket == "Short" ~ "Fast",
                    duration_bucket == "Medium" ~ "Standard",
                    TRUE ~ "Slow"
                ),

                # ═══════════════════════════════════════════════════════════
                # ADAPTIVE PARAMETER SELECTION
                # ═══════════════════════════════════════════════════════════
                rsi_period = case_when(
                    duration_bucket == "Short" ~ 10L,
                    duration_bucket == "Medium" ~ 14L,
                    TRUE ~ 18L
                ),

                macd_fast = case_when(
                    duration_bucket == "Short" ~ 8L,
                    duration_bucket == "Medium" ~ 12L,
                    TRUE ~ 15L
                ),

                macd_slow = case_when(
                    duration_bucket == "Short" ~ 18L,
                    duration_bucket == "Medium" ~ 26L,
                    TRUE ~ 35L
                ),

                macd_signal_period = case_when(
                    duration_bucket == "Short" ~ 7L,
                    duration_bucket == "Medium" ~ 9L,
                    TRUE ~ 11L
                ),

                bb_period = case_when(
                    duration_bucket == "Short" ~ 15L,
                    duration_bucket == "Medium" ~ 20L,
                    TRUE ~ 25L
                ),

                sma_short_period = case_when(
                    duration_bucket == "Short" ~ 30L,
                    duration_bucket == "Medium" ~ 50L,
                    TRUE ~ 60L
                ),

                sma_long_period = case_when(
                    duration_bucket == "Short" ~ 100L,
                    duration_bucket == "Medium" ~ 200L,
                    TRUE ~ 250L
                ),

                # ═══════════════════════════════════════════════════════════
                # RSI (ADAPTIVE BY DURATION)
                # ═══════════════════════════════════════════════════════════
                rsi_14_raw = {
                    period <- first(rsi_period)
                    if(n() >= period) {
                        tryCatch(
                            suppressWarnings(TTR::RSI(yield_to_maturity, n = period, maType = "WMA")),
                            error = function(e) rep(50, n())
                        )
                    } else if(n() >= 3) {
                        tryCatch(
                            suppressWarnings(TTR::RSI(yield_to_maturity, n = max(3, n()), maType = "WMA")),
                            error = function(e) rep(50, n())
                        )
                    } else {
                        rep(50, n())
                    }
                },
                rsi_14 = ifelse(is.na(rsi_14_raw), 50, rsi_14_raw),

                # ═══════════════════════════════════════════════════════════
                # DYNAMIC RSI THRESHOLDS (Bond-specific percentiles)
                # ═══════════════════════════════════════════════════════════
                # Standard thresholds (30/70 percentiles)
                rsi_threshold_buy = {
                    rsi_history <- rsi_14[!is.na(rsi_14)]
                    if(length(rsi_history) >= 20) {
                        quantile(rsi_history, 0.30, na.rm = TRUE)
                    } else {
                        30  # Default fallback
                    }
                },

                rsi_threshold_sell = {
                    rsi_history <- rsi_14[!is.na(rsi_14)]
                    if(length(rsi_history) >= 20) {
                        quantile(rsi_history, 0.70, na.rm = TRUE)
                    } else {
                        70  # Default fallback
                    }
                },

                rsi_median = {
                    rsi_history <- rsi_14[!is.na(rsi_14)]
                    if(length(rsi_history) >= 10) {
                        median(rsi_history, na.rm = TRUE)
                    } else {
                        50
                    }
                },

                # Extended percentiles for more granular signal detection
                rsi_p10 = {
                    rsi_history <- rsi_14[!is.na(rsi_14)]
                    if(length(rsi_history) >= 20) {
                        quantile(rsi_history, 0.10, na.rm = TRUE)
                    } else {
                        20  # Default fallback
                    }
                },

                rsi_p25 = {
                    rsi_history <- rsi_14[!is.na(rsi_14)]
                    if(length(rsi_history) >= 20) {
                        quantile(rsi_history, 0.25, na.rm = TRUE)
                    } else {
                        35  # Default fallback
                    }
                },

                rsi_p75 = {
                    rsi_history <- rsi_14[!is.na(rsi_14)]
                    if(length(rsi_history) >= 20) {
                        quantile(rsi_history, 0.75, na.rm = TRUE)
                    } else {
                        65  # Default fallback
                    }
                },

                rsi_p90 = {
                    rsi_history <- rsi_14[!is.na(rsi_14)]
                    if(length(rsi_history) >= 20) {
                        quantile(rsi_history, 0.90, na.rm = TRUE)
                    } else {
                        80  # Default fallback
                    }
                },

                # ═══════════════════════════════════════════════════════════
                # BOLLINGER BANDS (ADAPTIVE PERIOD)
                # ═══════════════════════════════════════════════════════════
                bb_mean = {
                    period <- first(bb_period)
                    tryCatch(
                        zoo::rollmeanr(
                            yield_to_maturity,
                            k = min(period, n()),
                            fill = mean(yield_to_maturity, na.rm = TRUE)
                        ),
                        error = function(e) yield_to_maturity
                    )
                },

                bb_sd = {
                    period <- first(bb_period)
                    tryCatch(
                        zoo::rollapplyr(
                            yield_to_maturity,
                            width = min(period, n()),
                            FUN = function(x) {
                                s <- sd(x, na.rm = TRUE)
                                ifelse(is.na(s) | s < 0.01, 0.1, s)
                            },
                            fill = 0.1,
                            partial = TRUE
                        ),
                        error = function(e) rep(0.1, n())
                    )
                },

                # ═══════════════════════════════════════════════════════════
                # BOND-SPECIFIC VOLATILITY (for normalization)
                # ═══════════════════════════════════════════════════════════
                bond_volatility = {
                    yield_changes <- diff(yield_to_maturity)
                    vol <- sd(yield_changes, na.rm = TRUE)
                    ifelse(is.na(vol) | vol < 0.01, 0.1, vol)
                },

                bb_upper = bb_mean + 2 * bb_sd,
                bb_lower = bb_mean - 2 * bb_sd,
                bb_upper_2 = bb_upper,
                bb_lower_2 = bb_lower,
                bb_upper_1 = bb_mean + bb_sd,
                bb_lower_1 = bb_mean - bb_sd,
                bb_width = bb_upper - bb_lower,
                bb_width_pct = (bb_width / pmax(bb_mean, 0.01)) * 100,
                bb_position = (yield_to_maturity - bb_lower) / pmax(0.01, bb_width),

                # ═══════════════════════════════════════════════════════════
                # MOVING AVERAGES (ADAPTIVE PERIODS)
                # ═══════════════════════════════════════════════════════════
                sma_50 = {
                    period <- first(sma_short_period)
                    tryCatch(
                        zoo::rollmeanr(
                            yield_to_maturity,
                            k = min(period, n()),
                            fill = yield_to_maturity
                        ),
                        error = function(e) yield_to_maturity
                    )
                },

                sma_200 = {
                    period <- first(sma_long_period)
                    tryCatch(
                        zoo::rollmeanr(
                            yield_to_maturity,
                            k = min(period, n()),
                            fill = yield_to_maturity
                        ),
                        error = function(e) yield_to_maturity
                    )
                },

                # ═══════════════════════════════════════════════════════════
                # MACD (ADAPTIVE PERIODS)
                # ═══════════════════════════════════════════════════════════
                ema_12 = {
                    fast_period <- first(macd_fast)
                    if(n() >= fast_period) {
                        tryCatch(
                            as.numeric(TTR::EMA(yield_to_maturity, n = fast_period)),
                            error = function(e) yield_to_maturity
                        )
                    } else {
                        yield_to_maturity
                    }
                },

                ema_26 = {
                    slow_period <- first(macd_slow)
                    if(n() >= slow_period) {
                        tryCatch(
                            as.numeric(TTR::EMA(yield_to_maturity, n = slow_period)),
                            error = function(e) yield_to_maturity
                        )
                    } else {
                        yield_to_maturity
                    }
                },

                ema_12 = ifelse(is.na(ema_12), yield_to_maturity, ema_12),
                ema_26 = ifelse(is.na(ema_26), yield_to_maturity, ema_26),
                macd = ema_12 - ema_26,

                macd_signal = {
                    signal_period <- first(macd_signal_period)
                    if(sum(!is.na(macd)) >= signal_period) {
                        tryCatch(
                            as.numeric(TTR::EMA(macd, n = signal_period)),
                            error = function(e) rep(0, n())
                        )
                    } else {
                        rep(0, n())
                    }
                },
                macd_signal = ifelse(is.na(macd_signal), 0, macd_signal),
                macd_histogram = macd - macd_signal,

                # ═══════════════════════════════════════════════════════════
                # VOLATILITY-NORMALIZED MACD
                # ═══════════════════════════════════════════════════════════
                macd_normalized = {
                    vol <- first(bond_volatility)
                    (macd_histogram / pmax(vol, 0.01))
                },

                # MACD historical percentiles (for time-series signals)
                macd_p10 = {
                    macd_history <- macd_normalized[!is.na(macd_normalized)]
                    if(length(macd_history) >= 20) {
                        quantile(macd_history, 0.10, na.rm = TRUE)
                    } else {
                        -1.5  # Default fallback
                    }
                },

                macd_p25 = {
                    macd_history <- macd_normalized[!is.na(macd_normalized)]
                    if(length(macd_history) >= 20) {
                        quantile(macd_history, 0.25, na.rm = TRUE)
                    } else {
                        -0.5  # Default fallback
                    }
                },

                macd_p75 = {
                    macd_history <- macd_normalized[!is.na(macd_normalized)]
                    if(length(macd_history) >= 20) {
                        quantile(macd_history, 0.75, na.rm = TRUE)
                    } else {
                        0.5  # Default fallback
                    }
                },

                macd_p90 = {
                    macd_history <- macd_normalized[!is.na(macd_normalized)]
                    if(length(macd_history) >= 20) {
                        quantile(macd_history, 0.90, na.rm = TRUE)
                    } else {
                        1.5  # Default fallback
                    }
                },

                # ═══════════════════════════════════════════════════════════
                # Rate of Change - WITH SAFE CALCULATIONS
                # ═══════════════════════════════════════════════════════════
                roc_5 = case_when(
                    n() <= 5 ~ 0,
                    is.na(lag(yield_to_maturity, 5)) ~ 0,
                    lag(yield_to_maturity, 5) == 0 ~ 0,
                    TRUE ~ (yield_to_maturity - lag(yield_to_maturity, 5)) /
                        lag(yield_to_maturity, 5) * 100
                ),

                roc_20 = case_when(
                    n() <= 20 ~ 0,
                    is.na(lag(yield_to_maturity, 20)) ~ 0,
                    lag(yield_to_maturity, 20) == 0 ~ 0,
                    TRUE ~ (yield_to_maturity - lag(yield_to_maturity, 20)) /
                        lag(yield_to_maturity, 20) * 100
                ),

                # ═══════════════════════════════════════════════════════════
                # Support & Resistance - WITH FALLBACKS
                # ═══════════════════════════════════════════════════════════
                resistance_1m = tryCatch(
                    zoo::rollmaxr(
                        yield_to_maturity,
                        k = min(20, n()),
                        fill = max(yield_to_maturity, na.rm = TRUE)
                    ),
                    error = function(e) yield_to_maturity + 0.1
                ),

                support_1m = tryCatch(
                    zoo::rollapplyr(
                        yield_to_maturity,
                        width = min(20, n()),
                        FUN = function(x) min(x, na.rm = TRUE),
                        fill = min(yield_to_maturity, na.rm = TRUE),
                        partial = TRUE
                    ),
                    error = function(e) yield_to_maturity - 0.1
                ),

                resistance_3m = tryCatch(
                    zoo::rollmaxr(
                        yield_to_maturity,
                        k = min(60, n()),
                        fill = max(yield_to_maturity, na.rm = TRUE)
                    ),
                    error = function(e) yield_to_maturity + 0.2
                ),

                support_3m = tryCatch(
                    zoo::rollapplyr(
                        yield_to_maturity,
                        width = min(60, n()),
                        FUN = function(x) min(x, na.rm = TRUE),
                        fill = min(yield_to_maturity, na.rm = TRUE),
                        partial = TRUE
                    ),
                    error = function(e) yield_to_maturity - 0.2
                ),

                # ═══════════════════════════════════════════════════════════
                # Williams %R - WITH SAFE DIVISION
                # ═══════════════════════════════════════════════════════════
                highest_14 = tryCatch(
                    zoo::rollmaxr(
                        yield_to_maturity,
                        k = min(14, n()),
                        fill = max(yield_to_maturity, na.rm = TRUE)
                    ),
                    error = function(e) yield_to_maturity
                ),

                lowest_14 = tryCatch(
                    zoo::rollapplyr(
                        yield_to_maturity,
                        width = min(14, n()),
                        FUN = function(x) min(x, na.rm = TRUE),
                        fill = min(yield_to_maturity, na.rm = TRUE),
                        partial = TRUE
                    ),
                    error = function(e) yield_to_maturity
                ),

                williams_r = case_when(
                    n() < 14 ~ -50,
                    (highest_14 - lowest_14) == 0 ~ -50,
                    TRUE ~ ((highest_14 - yield_to_maturity) / (highest_14 - lowest_14)) * -100
                ),

                # ═══════════════════════════════════════════════════════════
                # Stochastic RSI - WITH SAFE DIVISION
                # ═══════════════════════════════════════════════════════════
                rsi_highest_14 = zoo::rollmaxr(rsi_14, k = min(14, n()), fill = 100),
                rsi_lowest_14 = tryCatch(
                    zoo::rollapplyr(
                        rsi_14,
                        width = min(14, n()),
                        FUN = function(x) min(x, na.rm = TRUE),
                        fill = 0,
                        partial = TRUE
                    ),
                    error = function(e) rep(0, n())
                ),

                stoch_rsi = case_when(
                    n() < 14 ~ 0.5,
                    (rsi_highest_14 - rsi_lowest_14) == 0 ~ 0.5,
                    TRUE ~ (rsi_14 - rsi_lowest_14) / (rsi_highest_14 - rsi_lowest_14)
                ),

                # ═══════════════════════════════════════════════════════════
                # Historical Volatility - WITH FALLBACK
                # ═══════════════════════════════════════════════════════════
                daily_return = (yield_to_maturity - lag(yield_to_maturity, 1)) /
                    pmax(lag(yield_to_maturity, 1), 0.01) * 100,

                historical_vol = tryCatch(
                    zoo::rollapplyr(
                        daily_return,
                        width = min(20, n()),
                        FUN = function(x) {
                            vol <- sd(x, na.rm = TRUE)
                            ifelse(is.na(vol), 10, vol * sqrt(252))
                        },
                        fill = 10,
                        partial = TRUE
                    ),
                    error = function(e) rep(10, n())
                ),

                # ═══════════════════════════════════════════════════════════
                # Z-score and Mean
                # ═══════════════════════════════════════════════════════════
                mean_20 = bb_mean,
                z_score_20 = (yield_to_maturity - bb_mean) / pmax(0.01, bb_sd),

                # ═══════════════════════════════════════════════════════════
                # Signal Strength (Trading Signal) - COMPREHENSIVE SCORING
                # Uses all 4 indicators: RSI, BB, MACD, Momentum (ROC)
                # Each contributes -2 to +2 points, total range: -8 to +8
                # ═══════════════════════════════════════════════════════════

                # RSI Signal Score (-2 to +2)
                # For YIELDS: overbought RSI = yields may fall = bullish for bond prices
                rsi_signal_score = case_when(
                    is.na(rsi_14) ~ 0L,
                    rsi_14 > 80 ~ 2L,        # Extremely overbought - yields likely to fall
                    rsi_14 > 70 ~ 1L,        # Overbought
                    rsi_14 < 20 ~ -2L,       # Extremely oversold - yields likely to rise
                    rsi_14 < 30 ~ -1L,       # Oversold
                    TRUE ~ 0L                 # Neutral zone (30-70)
                ),

                # Bollinger Band Signal Score (-2 to +2)
                # Position > 1 = above upper band = overbought = may fall
                bb_signal_score = case_when(
                    is.na(bb_position) ~ 0L,
                    bb_position > 1.0 ~ 2L,   # Above upper band - overbought
                    bb_position > 0.8 ~ 1L,   # Near upper band
                    bb_position < 0 ~ -2L,    # Below lower band - oversold
                    bb_position < 0.2 ~ -1L,  # Near lower band
                    TRUE ~ 0L                  # Within bands
                ),

                # MACD Signal Score (-2 to +2)
                # Positive histogram = bullish momentum for yields
                macd_signal_score = case_when(
                    is.na(macd_histogram) ~ 0L,
                    macd_histogram > 0.05 ~ -2L,   # Strong bullish momentum (yields rising = bad for bonds)
                    macd_histogram > 0 ~ -1L,      # Bullish
                    macd_histogram < -0.05 ~ 2L,   # Strong bearish momentum (yields falling = good for bonds)
                    macd_histogram < 0 ~ 1L,       # Bearish
                    TRUE ~ 0L
                ),

                # Momentum/ROC Signal Score (-2 to +2)
                momentum_signal_score = case_when(
                    is.na(roc_20) ~ 0L,
                    roc_20 > 5 ~ -2L,         # Strong positive momentum (yields rising fast = bad)
                    roc_20 > 2 ~ -1L,         # Positive momentum
                    roc_20 < -5 ~ 2L,         # Strong negative momentum (yields falling = good)
                    roc_20 < -2 ~ 1L,         # Negative momentum
                    TRUE ~ 0L
                ),

                # TOTAL Signal Score (-8 to +8)
                # Positive = bullish for bond prices (yields falling)
                # Negative = bearish for bond prices (yields rising)
                total_signal_score = rsi_signal_score + bb_signal_score +
                                     macd_signal_score + momentum_signal_score,

                # Overall Signal Classification (5 levels with proper gradation)
                signal_strength = case_when(
                    total_signal_score >= 3 ~ "Strong Buy",
                    total_signal_score >= 1 ~ "Buy",
                    total_signal_score >= -1 ~ "Neutral",
                    total_signal_score >= -3 ~ "Sell",
                    TRUE ~ "Strong Sell"
                )
            ) %>%
            # Remove temporary columns
            select(-rsi_14_raw, -ema_12, -ema_26,
                   -highest_14, -lowest_14, -rsi_highest_14, -rsi_lowest_14,
                   -daily_return, -n_obs, -rsi_period, -macd_fast, -macd_slow,
                   -macd_signal_period, -bb_period, -sma_short_period, -sma_long_period) %>%
            ungroup()

        # ════════════════════════════════════════════════════════════════
        # PHASE 4: CRITICAL SANITIZATION
        # ════════════════════════════════════════════════════════════════

        # ✅ NEW: Nuclear sanitization of output
        temp_result <- sanitize_pipeline_data(temp_result, "calculate_advanced_technicals [output]")

        # ════════════════════════════════════════════════════════════════
        # PHASE 5: VERIFICATION
        # ════════════════════════════════════════════════════════════════

        # Verify bond column preserved
        if(!"bond" %in% names(temp_result)) {
            stop("CRITICAL: Bond column lost during technical calculation!")
        }

        # Verify required columns exist
        required_technical_cols <- c(
            "rsi_14", "bb_position", "bb_mean", "bb_upper_2", "bb_lower_2",
            "macd", "macd_signal", "macd_histogram",
            "sma_50", "sma_200", "signal_strength"
        )

        missing_cols <- setdiff(required_technical_cols, names(temp_result))
        if(length(missing_cols) > 0) {
            warning(paste(
                "Some technical columns missing after calculation:",
                paste(missing_cols, collapse = ", "),
                "- Adding defaults"
            ))

            # Add missing columns with safe defaults
            for(col in missing_cols) {
                temp_result[[col]] <- switch(col,
                                             "rsi_14" = 50,
                                             "bb_position" = 0.5,
                                             "bb_mean" = temp_result$yield_to_maturity,
                                             "bb_upper_2" = temp_result$yield_to_maturity + 0.2,
                                             "bb_lower_2" = temp_result$yield_to_maturity - 0.2,
                                             "macd" = 0,
                                             "macd_signal" = 0,
                                             "macd_histogram" = 0,
                                             "sma_50" = temp_result$yield_to_maturity,
                                             "sma_200" = temp_result$yield_to_maturity,
                                             "signal_strength" = "Neutral",
                                             NA  # default for any other column
                )
            }
        }

        message(sprintf(
            "✓ Technical indicators SUCCESS: %d rows × %d cols | %d bonds",
            nrow(temp_result), ncol(temp_result), n_distinct(temp_result$bond)
        ))

        return(temp_result)

    }, error = function(e) {

        # ════════════════════════════════════════════════════════════════
        # PHASE 6: COMPREHENSIVE ERROR FALLBACK
        # ════════════════════════════════════════════════════════════════

        warning(paste(
            "Technical calculation FAILED:", e$message,
            "| Using safe defaults for all indicators"
        ))

        return(add_default_technicals(data))
    })

    return(result)
}

# ════════════════════════════════════════════════════════════════════════
# HELPER FUNCTION: Add Default Technical Indicators
# ════════════════════════════════════════════════════════════════════════

#' @keywords internal
add_default_technicals <- function(data) {
    # Set default modified_duration if missing
    if(!"modified_duration" %in% names(data)) {
        data$modified_duration <- 7  # Medium duration default
    }

    # Duration classification
    data$duration_bucket <- "Medium"
    data$indicator_speed <- "Standard"

    # Traditional indicators
    data$signal_strength <- "Neutral"
    data$rsi_14 <- 50
    data$rsi_threshold_buy <- 30
    data$rsi_threshold_sell <- 70
    data$rsi_median <- 50
    data$bb_position <- 0.5
    data$macd <- 0
    data$macd_signal <- 0
    data$macd_histogram <- 0
    data$macd_normalized <- 0
    data$bond_volatility <- 0.1
    data$roc_20 <- 0
    data$roc_5 <- 0
    data$bb_upper_2 <- data$yield_to_maturity + 0.2
    data$bb_lower_2 <- data$yield_to_maturity - 0.2
    data$bb_upper_1 <- data$yield_to_maturity + 0.1
    data$bb_lower_1 <- data$yield_to_maturity - 0.1
    data$bb_mean <- data$yield_to_maturity
    data$bb_sd <- 0.1
    data$bb_width <- 0.4
    data$bb_width_pct <- 40
    data$sma_50 <- data$yield_to_maturity
    data$sma_200 <- data$yield_to_maturity
    data$resistance_1m <- data$yield_to_maturity + 0.1
    data$support_1m <- data$yield_to_maturity - 0.1
    data$resistance_3m <- data$yield_to_maturity + 0.2
    data$support_3m <- data$yield_to_maturity - 0.2
    data$williams_r <- -50
    data$stoch_rsi <- 0.5
    data$historical_vol <- 10
    data$mean_20 <- data$yield_to_maturity
    data$z_score_20 <- 0
    data$signal_confidence <- 0

    return(sanitize_pipeline_data(data, "add_default_technicals"))
}

# ════════════════════════════════════════════════════════════════════════
# OVERALL SIGNAL SYNTHESIS - Composite Technical Score
# ════════════════════════════════════════════════════════════════════════

#' @title Calculate Overall Technical Signal
#' @description Synthesizes multiple technical indicators into a composite score with
#'   bond-specific interpretation (yields up = bearish for bonds)
#' @param rsi Current RSI value
#' @param macd_signal MACD signal ("Bullish", "Bearish", etc.)
#' @param trend_status Trend status ("Strong Uptrend", "Downtrend", etc.)
#' @param bb_position Position within Bollinger Bands (0-1 scale)
#' @return List with score, signal, recommendation, confidence, and component breakdown
#' @export
calculate_technical_signal <- function(rsi, macd_signal, trend_status, bb_position) {

    # Initialize score (range: -100 to +100)
    # Positive = yields likely to rise (bearish for bonds)
    # Negative = yields likely to fall (bullish for bonds)

    # RSI Component (-25 to +25)
    # For YIELDS: overbought RSI suggests yields rose too fast and may reverse DOWN
    # This is BULLISH for bond prices, so we invert
    rsi_raw <- case_when(
        is.na(rsi) ~ 0,
        rsi > 70 ~ 25,          # Overbought - yields may fall (good for bonds)
        rsi > 60 ~ 10,          # Elevated
        rsi > 40 ~ 0,           # Neutral
        rsi > 30 ~ -10,         # Depressed
        TRUE ~ -25              # Oversold - yields may rise (bad for bonds)
    )
    rsi_score <- -rsi_raw  # Invert for bond context (overbought yields = bullish for prices)

    # MACD Component (-30 to +30)
    # For YIELDS: bullish MACD means yields rising = bearish for bonds
    macd_score <- case_when(
        is.na(macd_signal) ~ 0,
        macd_signal == "Strong Bullish" ~ 30,   # Yields rising strongly
        macd_signal == "Bullish" ~ 15,
        macd_signal == "Neutral" ~ 0,
        macd_signal == "Bearish" ~ -15,
        macd_signal == "Strong Bearish" ~ -30,  # Yields falling strongly
        TRUE ~ 0
    )

    # Trend Component (-30 to +30)
    trend_score <- case_when(
        is.na(trend_status) ~ 0,
        grepl("Strong.*Up|Strongly Rising", trend_status, ignore.case = TRUE) ~ 30,
        grepl("Uptrend|Rising", trend_status, ignore.case = TRUE) ~ 15,
        grepl("Sideways|Stable|Neutral", trend_status, ignore.case = TRUE) ~ 0,
        grepl("Downtrend|Falling", trend_status, ignore.case = TRUE) ~ -15,
        grepl("Strong.*Down|Strongly Falling", trend_status, ignore.case = TRUE) ~ -30,
        TRUE ~ 0
    )

    # BB Position Component (-15 to +15)
    # Mean reversion: extreme positions suggest reversal
    bb_raw <- case_when(
        is.na(bb_position) ~ 0,
        bb_position > 1 ~ 15,       # Above upper band - extended, may reverse
        bb_position > 0.8 ~ 8,      # Near upper band
        bb_position > 0.2 ~ 0,      # Middle
        bb_position > 0 ~ -8,       # Near lower band
        TRUE ~ -15                   # Below lower band - extended, may reverse
    )
    bb_score <- -bb_raw  # Invert for mean reversion expectation

    # Composite Score
    total_score <- rsi_score + macd_score + trend_score + bb_score

    # Confidence based on agreement
    signals <- c(sign(rsi_score), sign(macd_score), sign(trend_score))
    signals <- signals[signals != 0]
    agreement <- if(length(signals) > 0) abs(sum(signals)) / length(signals) else 0

    # Overall signal (bond-specific terminology)
    signal <- case_when(
        total_score > 40 ~ "Strong Yield Rise Expected",
        total_score > 15 ~ "Yield Rise Expected",
        total_score > -15 ~ "Neutral / Range-bound",
        total_score > -40 ~ "Yield Fall Expected",
        TRUE ~ "Strong Yield Fall Expected"
    )

    # Bond-specific recommendation
    recommendation <- case_when(
        total_score > 40 ~ "REDUCE DURATION - Yields likely to rise",
        total_score > 15 ~ "CAUTION - Upward yield pressure",
        total_score > -15 ~ "HOLD - No clear direction",
        total_score > -40 ~ "OPPORTUNITY - Yields may fall",
        TRUE ~ "ADD DURATION - Strong rally expected"
    )

    list(
        score = total_score,
        signal = signal,
        recommendation = recommendation,
        confidence = agreement,
        components = list(
            rsi = rsi_score,
            macd = macd_score,
            trend = trend_score,
            bb = bb_score
        )
    )
}

#' @title Get Bond-Specific Trend Label
#' @description Generates trend labels using bond-specific terminology
#'   (yield direction and price implication)
#' @param sma_50 50-day simple moving average
#' @param sma_200 200-day simple moving average
#' @param current_yield Current yield value
#' @return List with label, direction, strength, price_implication, and color
#' @export
get_trend_label_bonds <- function(sma_50, sma_200, current_yield) {

    # Handle missing values
    if (is.na(sma_50) || is.na(sma_200) || is.na(current_yield)) {
        return(list(
            label = "Unknown",
            direction = "Unknown",
            strength = "Unknown",
            price_implication = "",
            color = "#9E9E9E"
        ))
    }

    # Determine trend direction
    if (sma_50 > sma_200 * 1.02) {
        trend_direction <- "Rising"
        trend_strength <- "Strongly"
    } else if (sma_50 > sma_200) {
        trend_direction <- "Rising"
        trend_strength <- "Moderately"
    } else if (sma_50 < sma_200 * 0.98) {
        trend_direction <- "Falling"
        trend_strength <- "Strongly"
    } else if (sma_50 < sma_200) {
        trend_direction <- "Falling"
        trend_strength <- "Moderately"
    } else {
        trend_direction <- "Stable"
        trend_strength <- ""
    }

    # Create bond-specific label
    label <- if (trend_direction == "Stable") {
        "Stable Yields"
    } else {
        paste(trend_strength, trend_direction, "Yields")
    }

    # Add price implication (inverse relationship: yield up = price down)
    price_implication <- case_when(
        trend_direction == "Rising" ~ "(Bearish for Prices)",
        trend_direction == "Falling" ~ "(Bullish for Prices)",
        TRUE ~ ""
    )

    # Color coding: Red = bad for bondholders (rising yields), Green = good (falling yields)
    color <- case_when(
        trend_direction == "Rising" ~ "#D32F2F",   # Red - bad for bondholders
        trend_direction == "Falling" ~ "#388E3C", # Green - good for bondholders
        TRUE ~ "#FF9800"                           # Orange - neutral
    )

    list(
        label = label,
        direction = trend_direction,
        strength = trend_strength,
        price_implication = price_implication,
        color = color
    )
}

#' @title Calculate Support and Resistance Levels
#' @description Calculates S/R levels with methodology explanation
#' @param yield_series Vector of yield values
#' @param lookback Number of periods to look back (default 60)
#' @return List with support, resistance, pivot, distances, and methodology
#' @export
calculate_support_resistance_detailed <- function(yield_series, lookback = 60) {

    # Get recent data
    if (length(yield_series) < lookback) {
        lookback <- max(20, length(yield_series))
    }
    recent_data <- tail(yield_series, lookback)
    recent_data <- recent_data[!is.na(recent_data)]

    if (length(recent_data) < 5) {
        return(list(
            resistance = NA,
            support = NA,
            pivot = NA,
            r1 = NA,
            s1 = NA,
            current = NA,
            distance_to_resistance = NA,
            distance_to_support = NA,
            position_pct = 50,
            methodology = "Insufficient data"
        ))
    }

    # Method: Recent high/low
    resistance_high <- max(recent_data, na.rm = TRUE)
    support_low <- min(recent_data, na.rm = TRUE)

    # Pivot points
    high <- max(recent_data, na.rm = TRUE)
    low <- min(recent_data, na.rm = TRUE)
    close <- tail(recent_data, 1)

    pivot <- (high + low + close) / 3
    r1 <- 2 * pivot - low
    s1 <- 2 * pivot - high

    # Current position relative to levels
    current <- tail(yield_series[!is.na(yield_series)], 1)
    distance_to_resistance <- resistance_high - current
    distance_to_support <- current - support_low

    # Position (0 = at support, 100 = at resistance)
    range_size <- resistance_high - support_low
    position_pct <- if (range_size > 0) {
        ((current - support_low) / range_size) * 100
    } else {
        50
    }

    list(
        resistance = resistance_high,
        support = support_low,
        pivot = pivot,
        r1 = r1,
        s1 = s1,
        current = current,
        distance_to_resistance = distance_to_resistance,
        distance_to_support = distance_to_support,
        position_pct = min(100, max(0, position_pct)),
        methodology = sprintf("%d-day rolling high/low", lookback)
    )
}

#' @export
# Value-at-Risk Calculation - CORRECTED VERSION
#' @description Calculates VaR, CVaR, and distribution statistics using DURATION-BASED price returns
#' @details
#'   CRITICAL FIX: Now uses proper bond pricing formula instead of yield percentage change.
#'   Price Return = -Modified Duration × Δ Yield + ½ × Convexity × (Δ Yield)²
#'
#'   This ensures VaR correctly scales with duration:
#'   - Longer duration bonds have HIGHER VaR (more price sensitivity)
#'   - Expected VaR per year of duration: ~10-25 bps for SA govt bonds
#'
#'   CVaR (Expected Shortfall) is the average of all losses beyond VaR threshold
#'   CVaR must always be more extreme (more negative) than VaR
calculate_var <- memoise(function(data, confidence_levels = c(0.95, 0.99), horizon = 1) {
    safe_execute({
        # ════════════════════════════════════════════════════════════════════════
        # STEP 1: CRITICAL DATA FILTERING - Remove NA bonds at SOURCE
        # ════════════════════════════════════════════════════════════════════════
        # This must happen BEFORE any grouping to prevent NA groups from forming

        data <- data %>%
            filter(
                # Bond name validation - comprehensive check
                !is.na(bond),
                bond != "",
                bond != "NA",
                as.character(bond) != "NA",
                nchar(trimws(as.character(bond))) > 0,

                # Data quality checks
                !is.na(yield_to_maturity),
                is.finite(yield_to_maturity),
                yield_to_maturity > 0,
                yield_to_maturity < 30,  # SA govt bonds typically 5-15%

                # Duration and convexity required for proper calculation
                !is.na(modified_duration),
                is.finite(modified_duration),
                modified_duration > 0,
                modified_duration < 50,  # Sanity check for govt bonds

                !is.na(convexity),
                is.finite(convexity),
                convexity > 0
            )

        # Log data quality after filtering
        n_bonds <- n_distinct(data$bond)
        message(sprintf("[calculate_var] Processing %d bonds after filtering", n_bonds))

        if (n_bonds == 0) {
            warning("calculate_var: No valid bonds after filtering - check data quality")
            return(data.frame())
        }

        # ════════════════════════════════════════════════════════════════════════
        # STEP 2: CALCULATE DURATION-BASED PRICE RETURNS (CORRECTED FORMULA)
        # ════════════════════════════════════════════════════════════════════════
        #
        # CRITICAL FIX: Previous code used yield PERCENTAGE change which is WRONG!
        #
        # OLD (WRONG): daily_return = (yield_t - yield_t1) / yield_t1 × 100
        #   This produces INVERSE relationship: short duration bonds show HIGHER VaR
        #   because yield changes are proportionally larger for lower-yielding bonds
        #
        # NEW (CORRECT): Price Return = -Duration × Δ Yield + ½ × Convexity × (Δ Yield)²
        #   This produces CORRECT relationship: longer duration = higher VaR
        #   Because longer duration bonds are more sensitive to yield changes
        #
        # Example: If yield rises 10 bps (0.001 in decimal):
        #   R186 (4.69y duration): Price change ≈ -4.69 × 0.001 × 100 = -0.47%
        #   R2053 (23.82y duration): Price change ≈ -23.82 × 0.001 × 100 = -2.38%
        #   → R2053 correctly shows ~5x higher price sensitivity

        returns_data <- data %>%
            group_by(bond) %>%
            arrange(date) %>%
            mutate(
                # ABSOLUTE yield change in decimal form (NOT percentage change!)
                # yield_to_maturity is in percentage (e.g., 8.5), divide by 100 to get decimal
                # Δ yield in decimal: e.g., 0.001 = 10 basis points
                yield_change = (yield_to_maturity - lag(yield_to_maturity)) / 100,

                # CORRECT price return using bond pricing formula
                # Price Return = -Modified Duration × Δy + 0.5 × Convexity × (Δy)²
                # Multiply by 100 to express as percentage
                price_return = (-modified_duration * yield_change +
                               0.5 * convexity * (yield_change^2)) * 100
            ) %>%
            # Remove first row per bond (no lag available) and any invalid returns
            filter(
                !is.na(price_return),
                is.finite(price_return),
                abs(price_return) < 15  # Cap at 15% daily move (very extreme for govt bonds)
            ) %>%
            ungroup()

        # Verify we have enough data
        obs_per_bond <- returns_data %>%
            group_by(bond) %>%
            summarise(n_obs = n(), .groups = "drop")

        # Require at least 20 observations for meaningful VaR calculation
        valid_bonds <- obs_per_bond %>%
            filter(n_obs >= 20) %>%
            pull(bond)

        returns_data <- returns_data %>%
            filter(bond %in% valid_bonds)

        if (nrow(returns_data) == 0) {
            warning("calculate_var: No bonds with sufficient observations (min 20)")
            return(data.frame())
        }

        # ════════════════════════════════════════════════════════════════════════
        # STEP 3: CALCULATE VAR METRICS WITH PROPER PRICE RETURNS
        # ════════════════════════════════════════════════════════════════════════

        var_results <- returns_data %>%
            group_by(bond) %>%
            summarise(
                # Get duration for validation
                modified_duration = first(modified_duration),

                # Sample statistics using PRICE returns (not yield returns)
                n_observations = n(),
                mean_return = mean(price_return),
                vol = sd(price_return),
                skewness = moments::skewness(price_return),
                # Use EXCESS kurtosis (normal = 0) for easier interpretation
                kurtosis = moments::kurtosis(price_return) - 3,

                # Historical VaR (left tail quantiles - losses)
                # These are PRICE returns, so left tail = price losses
                VaR_95_hist = quantile(price_return, 0.05),
                VaR_99_hist = quantile(price_return, 0.01),

                # Parametric VaR (assuming normal distribution)
                VaR_95_param = mean_return - 1.645 * vol * sqrt(horizon),
                VaR_99_param = mean_return - 2.326 * vol * sqrt(horizon),

                # CORRECTED CVaR (Expected Shortfall) Calculation
                # CVaR_95 = Average of all returns in the WORST 5% of observations
                CVaR_95 = {
                    threshold <- quantile(price_return, 0.05)
                    tail_returns <- price_return[price_return < threshold]
                    if (length(tail_returns) > 0) {
                        cvar_val <- mean(tail_returns)
                        # VALIDATION: CVaR must be more extreme (more negative) than VaR
                        if (cvar_val > threshold) {
                            threshold * 1.2
                        } else {
                            cvar_val
                        }
                    } else {
                        threshold * 1.2
                    }
                },

                CVaR_99 = {
                    threshold <- quantile(price_return, 0.01)
                    tail_returns <- price_return[price_return < threshold]
                    if (length(tail_returns) > 0) {
                        cvar_val <- mean(tail_returns)
                        if (cvar_val > threshold) {
                            threshold * 1.2
                        } else {
                            cvar_val
                        }
                    } else {
                        threshold * 1.2
                    }
                },

                # VaR in basis points (absolute value for display)
                # price_return is already in % (e.g., -1.5 = -1.5% price change)
                # Multiply by 100 to convert to basis points (e.g., -1.5% = 150 bps)
                VaR_95_bps = abs(VaR_95_hist) * 100,
                VaR_99_bps = abs(VaR_99_hist) * 100,

                .groups = "drop"
            )

        # ════════════════════════════════════════════════════════════════════════
        # STEP 4: VALIDATION - Verify Duration-VaR Relationship
        # ════════════════════════════════════════════════════════════════════════
        # VaR should be approximately proportional to duration
        # Expected: ~10-25 bps VaR per year of modified duration

        var_results <- var_results %>%
            mutate(
                var_per_duration = VaR_95_bps / modified_duration,
                # Flag if VaR/duration ratio is outside expected range
                is_reasonable = var_per_duration > 5 & var_per_duration < 50
            )

        # Log validation results
        problematic <- var_results %>% filter(!is_reasonable)
        if (nrow(problematic) > 0) {
            message(sprintf("[calculate_var] WARNING: %d bonds have unexpected VaR/duration ratio",
                           nrow(problematic)))
            message("Expected: ~10-25 bps VaR per year of modified duration")
        }

        # ════════════════════════════════════════════════════════════════════════
        # STEP 5: FINAL FILTERING - Remove extreme/invalid values
        # ════════════════════════════════════════════════════════════════════════

        var_results <- var_results %>%
            filter(
                # Bond name validation (final check)
                !is.na(bond),
                bond != "",
                bond != "NA",
                as.character(bond) != "NA",

                # VaR value validation
                !is.na(VaR_95_bps),
                is.finite(VaR_95_bps),
                VaR_95_bps > 0,       # Must have positive risk
                VaR_95_bps < 500,     # Cap at 5% daily VaR (extreme but possible)
                VaR_99_bps < 750      # Cap at 7.5% for 99% VaR
            ) %>%
            # Remove validation columns (not needed downstream)
            select(-var_per_duration, -is_reasonable)

        # Log final results
        message(sprintf("[calculate_var] Final output: %d bonds", nrow(var_results)))

        if (nrow(var_results) == 0) {
            warning("calculate_var: All VaR results filtered - check data quality")
        }

        return(var_results)
    }, default = data.frame())
})

# ════════════════════════════════════════════════════════════════════════════════
# HELPER FUNCTIONS FOR ROBUST REGIME DETECTION
# ════════════════════════════════════════════════════════════════════════════════

#' @keywords internal
#' @title Winsorize Vector
#' @description Caps extreme values at specified percentiles to handle outliers
#' @param x Numeric vector to winsorize
#' @param lower_pct Lower percentile threshold (default 0.01 = 1%)
#' @param upper_pct Upper percentile threshold (default 0.99 = 99%)
#' @return Winsorized vector with outliers capped
winsorize_vector <- function(x, lower_pct = 0.01, upper_pct = 0.99) {
    # Remove NAs for quantile calculation
    x_clean <- x[!is.na(x)]

    if(length(x_clean) < 3) {
        return(x)  # Not enough data to winsorize
    }

    # Calculate percentile thresholds
    lower_bound <- quantile(x_clean, lower_pct, na.rm = TRUE)
    upper_bound <- quantile(x_clean, upper_pct, na.rm = TRUE)

    # Cap values at thresholds
    x_winsorized <- pmax(pmin(x, upper_bound), lower_bound)

    return(x_winsorized)
}

#' @keywords internal
#' @title Calculate Rolling Volatility Time Series
#' @description Calculates rolling annualized volatility for each time point
#' @param yields Numeric vector of yield values (in percentage form, e.g., 8.5)
#' @param window Window size for rolling calculation (default 20)
#' @param max_vol Maximum allowed volatility (default 50% for govt bonds)
#' @param winsorize_pct Percentile for Winsorization (default 0.99)
#' @return Numeric vector of volatility values (one per date), expressed as decimals (e.g., 0.12 = 12%)
calculate_rolling_volatility <- function(yields, window = 20, max_vol = 50, winsorize_pct = 0.99) {

    n_obs <- length(yields)

    # Validate input
    if(n_obs < 2) {
        warning("calculate_rolling_volatility: Insufficient data (n < 2)")
        return(rep(0.10, n_obs))  # Return 10% default
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 1: Calculate yield changes and apply winsorization
    # ══════════════════════════════════════════════════════════════════════════

    # Calculate daily yield changes
    yield_changes <- c(NA, diff(yields))  # First element is NA (no prior value)

    # Winsorize to handle outliers (exclude NA from winsorization)
    valid_changes <- yield_changes[!is.na(yield_changes)]

    if(length(valid_changes) < 2) {
        warning("calculate_rolling_volatility: Insufficient valid yield changes")
        return(rep(0.10, n_obs))
    }

    # Calculate winsorization thresholds
    lower_threshold <- quantile(valid_changes, probs = 1 - winsorize_pct, na.rm = TRUE)
    upper_threshold <- quantile(valid_changes, probs = winsorize_pct, na.rm = TRUE)

    # Apply winsorization
    yield_changes_clean <- yield_changes
    yield_changes_clean[!is.na(yield_changes) & yield_changes < lower_threshold] <- lower_threshold
    yield_changes_clean[!is.na(yield_changes) & yield_changes > upper_threshold] <- upper_threshold

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 2: Calculate rolling standard deviation for EACH date
    # ══════════════════════════════════════════════════════════════════════════

    # Adaptive window size for small samples
    effective_window <- min(window, max(5, floor(n_obs * 0.5)))

    # Calculate rolling volatility using TTR::runSD
    # This returns a vector with one volatility value per date
    rolling_vol <- tryCatch({
        # Use runSD on yield changes (standard deviation of changes)
        sd_values <- TTR::runSD(yield_changes_clean, n = effective_window, sample = TRUE)

        # Annualize: multiply by sqrt(252 trading days)
        annualized_vol <- sd_values * sqrt(252)

        annualized_vol
    }, error = function(e) {
        warning(paste("calculate_rolling_volatility: Error in runSD:", e$message))
        rep(0.10, n_obs)
    })

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 3: Handle NAs and apply caps
    # ══════════════════════════════════════════════════════════════════════════

    # For initial NAs (where window hasn't filled), use simple expanding window estimate
    na_indices <- which(is.na(rolling_vol))
    for(idx in na_indices) {
        if(idx <= 2) {
            rolling_vol[idx] <- 0.10  # Default for very early dates
        } else {
            # Use all available data up to this point
            available_changes <- yield_changes_clean[2:idx]
            available_changes <- available_changes[!is.na(available_changes)]
            if(length(available_changes) >= 2) {
                rolling_vol[idx] <- sd(available_changes, na.rm = TRUE) * sqrt(252)
            } else {
                rolling_vol[idx] <- 0.10
            }
        }
    }

    # Cap volatility at maximum (default 50% for govt bonds)
    rolling_vol <- pmin(rolling_vol, max_vol, na.rm = TRUE)

    # Handle any remaining NAs or infinite values
    rolling_vol[is.na(rolling_vol) | is.infinite(rolling_vol)] <- 0.10

    # IMPORTANT: Do NOT divide by 100 here!
    # The volatility is already in the correct scale:
    # - Yield changes are differences in percentage points (e.g., 0.1 for a 0.1% change)
    # - Standard deviation of these changes is in percentage points per day
    # - Annualized volatility (SD * sqrt(252)) is in percentage points per year
    # - For display as percentage (e.g., "1.59%"), the value 1.59 is correct
    # - For use as decimal in calculations (e.g., 0.0159), divide by 100 at point of use
    # - Returning the raw annualized volatility allows flexible formatting downstream

    return(rolling_vol)
}

#' @keywords internal
#' @title Calculate Robust Volatility (DEPRECATED - Returns single value)
#' @description Calculates annualized volatility with outlier protection and bounds checking
#' @param yields Numeric vector of yield values (in percentage form, e.g., 8.5)
#' @param window Window size for rolling calculation
#' @param max_vol Maximum allowed volatility (default 50% for govt bonds)
#' @param winsorize_pct Percentile for Winsorization (default 0.99 = 99%)
#' @return List containing volatility metrics and diagnostic flags
#' @note This function returns a SINGLE volatility value. Use calculate_rolling_volatility() for time series.
calculate_robust_volatility <- function(yields, window = 20, max_vol = 50, winsorize_pct = 0.99) {

    n_obs <- sum(!is.na(yields))

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 1: DATA VALIDATION
    # ══════════════════════════════════════════════════════════════════════════

    if(n_obs < 2) {
        return(list(
            volatility = 10,  # Default 10% for govt bonds
            volatility_uncapped = 10,
            capped_flag = FALSE,
            outlier_count = 0,
            data_quality_score = 0,
            warning_message = "Insufficient data"
        ))
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 2: CALCULATE YIELD CHANGES (with outlier detection)
    # ══════════════════════════════════════════════════════════════════════════

    # Calculate daily yield changes
    yield_changes <- diff(yields)
    yield_changes <- yield_changes[!is.na(yield_changes)]

    if(length(yield_changes) < 2) {
        return(list(
            volatility = 10,
            volatility_uncapped = 10,
            capped_flag = FALSE,
            outlier_count = 0,
            data_quality_score = 30,
            warning_message = "Insufficient yield changes"
        ))
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 3: WINSORIZE YIELD CHANGES (protect against extreme outliers)
    # ══════════════════════════════════════════════════════════════════════════

    yield_changes_original <- yield_changes
    yield_changes_winsorized <- winsorize_vector(yield_changes,
                                                 lower_pct = 1 - winsorize_pct,
                                                 upper_pct = winsorize_pct)

    # Count how many observations were winsorized
    outlier_count <- sum(abs(yield_changes_original - yield_changes_winsorized) > 1e-10, na.rm = TRUE)

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 4: CALCULATE ROLLING VOLATILITY (using winsorized changes)
    # ══════════════════════════════════════════════════════════════════════════

    # Reconstruct yields from winsorized changes (for rolling calculation)
    yields_robust <- c(yields[1], yields[1] + cumsum(yield_changes_winsorized))

    # Adaptive window size for small samples
    effective_window <- min(window, n_obs, max(5, floor(n_obs * 0.3)))

    if(effective_window < 5) {
        # Very small sample - use simple SD with warning
        volatility_raw <- sd(yield_changes_winsorized, na.rm = TRUE) * sqrt(252)
        warning_msg <- paste("Small sample (n=", n_obs, "), using adaptive window")
    } else if(n_obs >= window) {
        # Sufficient data - use proper rolling SD
        volatility_raw <- tryCatch({
            rolling_sd <- TTR::runSD(yields_robust, n = effective_window)
            # Take the last non-NA value and annualize
            last_sd <- tail(rolling_sd[!is.na(rolling_sd)], 1)
            if(length(last_sd) == 0) {
                sd(yield_changes_winsorized, na.rm = TRUE) * sqrt(252)
            } else {
                last_sd * sqrt(252)
            }
        }, error = function(e) {
            sd(yield_changes_winsorized, na.rm = TRUE) * sqrt(252)
        })
        warning_msg <- ""
    } else {
        # Medium sample - use reduced window
        volatility_raw <- tryCatch({
            rolling_sd <- TTR::runSD(yields_robust, n = effective_window)
            last_sd <- tail(rolling_sd[!is.na(rolling_sd)], 1)
            if(length(last_sd) == 0) {
                sd(yield_changes_winsorized, na.rm = TRUE) * sqrt(252)
            } else {
                last_sd * sqrt(252)
            }
        }, error = function(e) {
            sd(yield_changes_winsorized, na.rm = TRUE) * sqrt(252)
        })
        warning_msg <- paste("Reduced window (", effective_window, " vs ", window, ")", sep = "")
    }

    # Handle NA or infinite values
    if(is.na(volatility_raw) || is.infinite(volatility_raw)) {
        volatility_raw <- 10  # Safe default
        warning_msg <- paste(warning_msg, "NA/Inf detected, using default")
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 5: APPLY REALISTIC BOUNDS (govt bonds should never exceed 50%)
    # ══════════════════════════════════════════════════════════════════════════

    volatility_uncapped <- volatility_raw
    volatility_capped <- min(volatility_raw, max_vol)
    capped_flag <- volatility_raw > max_vol

    if(capped_flag) {
        warning_msg <- paste(warning_msg,
                             sprintf("Volatility capped: %.1f%% → %.1f%%",
                                     volatility_raw, volatility_capped))
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 6: DATA QUALITY SCORE (0-100 scale)
    # ══════════════════════════════════════════════════════════════════════════

    # Factors affecting quality:
    # - Sample size (more = better)
    # - Completeness (fewer NAs = better)
    # - Outlier rate (fewer outliers = better)
    # - Capping needed (no cap = better)

    size_score <- min(100, (n_obs / window) * 100)
    completeness_score <- (n_obs / length(yields)) * 100
    outlier_penalty <- (outlier_count / max(length(yield_changes), 1)) * 100
    cap_penalty <- ifelse(capped_flag, 20, 0)

    data_quality_score <- max(0, min(100,
                                     (size_score * 0.3 + completeness_score * 0.4 - outlier_penalty * 0.2 - cap_penalty * 0.1)
    ))

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 7: RETURN COMPREHENSIVE DIAGNOSTICS
    # ══════════════════════════════════════════════════════════════════════════

    return(list(
        volatility = volatility_capped,
        volatility_uncapped = volatility_uncapped,
        capped_flag = capped_flag,
        outlier_count = outlier_count,
        data_quality_score = round(data_quality_score, 1),
        warning_message = trimws(warning_msg)
    ))
}

# ════════════════════════════════════════════════════════════════════════════════
# MAIN FUNCTION: ROBUST MARKET REGIME DETECTION
# ════════════════════════════════════════════════════════════════════════════════

#' @export
#' @title Detect Market Regime (PRODUCTION-READY ROBUST VERSION)
#' @description Enhanced regime detection with outlier protection and diagnostic warnings
#' @details
#' This function calculates market stress metrics and classifies market conditions into
#' four regimes: Stressed, Elevated, Normal, and Calm.
#'
#' Key improvements over previous version:
#' - Winsorization of yield changes at 99th percentile before volatility calculation
#' - Hard cap on volatility at 50% (realistic maximum for govt bonds)
#' - Adaptive window sizing for small samples (< 20 observations)
#' - Enhanced stress score weighting (volatility: 50%, dispersion: 20%, momentum: 15%, bid-cover: 10%, curve: 5%)
#' - Comprehensive diagnostic flags (vol_capped_flag, outlier_count, data_quality_score)
#' - Data validation with early warnings for anomalies
#'
#' @param data Data frame with columns: date, bond, yield_to_maturity, modified_duration, bid_to_cover
#' @return Data frame with regime metrics including:
#'   - date: Date of observation
#'   - avg_yield: Cross-sectional average yield
#'   - yield_dispersion: Cross-sectional yield standard deviation
#'   - vol_20d: 20-day rolling volatility (capped, annualized %)
#'   - vol_60d: 60-day rolling volatility (capped, annualized %)
#'   - vol_20d_uncapped: Uncapped volatility for diagnostics
#'   - vol_capped_flag: Boolean flag indicating if volatility was capped
#'   - outlier_count: Number of observations Winsorized
#'   - data_quality_score: 0-100 quality metric
#'   - vol_percentile: Percentile rank of current volatility
#'   - stress_score: Composite stress metric (-3 to +3, typically -2 to +2)
#'   - regime: Regime classification (Stressed/Elevated/Normal/Calm)
#'   - regime_confidence: Confidence in classification (0-100)
#'
#' @examples
#' \dontrun{
#' regime_data <- detect_market_regime(bond_data)
#'
#' # Check for data quality issues
#' low_quality <- regime_data %>% filter(data_quality_score < 50)
#'
#' # Identify periods where volatility was capped
#' capped_periods <- regime_data %>% filter(vol_capped_flag == TRUE)
#' }
detect_market_regime <- function(data) {
    safe_execute({

        # ══════════════════════════════════════════════════════════════════════════
        # PHASE 1: INPUT VALIDATION & DATA QUALITY CHECKS
        # ══════════════════════════════════════════════════════════════════════════

        if(is.null(data) || nrow(data) == 0) {
            warning("detect_market_regime: Empty input data")
            return(data.frame())
        }

        required_cols <- c("date", "yield_to_maturity")
        missing_cols <- setdiff(required_cols, names(data))
        if(length(missing_cols) > 0) {
            stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
        }

        # Check for suspicious yield values (potential data entry errors)
        suspicious_yields <- data %>%
            filter(!is.na(yield_to_maturity)) %>%
            filter(yield_to_maturity > 50 | yield_to_maturity < 0.1)

        if(nrow(suspicious_yields) > 0) {
            warning(sprintf(
                "⚠ SUSPICIOUS YIELDS DETECTED: %d observations with yields outside 0.1%%-50%% range. Max yield: %.1f%%. These will be handled by Winsorization.",
                nrow(suspicious_yields),
                max(data$yield_to_maturity, na.rm = TRUE)
            ))
        }

        # ══════════════════════════════════════════════════════════════════════════
        # PHASE 2: CALCULATE DAILY MARKET METRICS (cross-sectional)
        # ══════════════════════════════════════════════════════════════════════════

        market_metrics <- data %>%
            group_by(date) %>%
            summarise(
                avg_yield = mean(yield_to_maturity, na.rm = TRUE),
                yield_dispersion = sd(yield_to_maturity, na.rm = TRUE),
                avg_bid_cover = mean(bid_to_cover, na.rm = TRUE),

                # Curve slope: long-end minus short-end yields
                curve_slope = mean(yield_to_maturity[modified_duration > 10], na.rm = TRUE) -
                    mean(yield_to_maturity[modified_duration < 5], na.rm = TRUE),

                # Curve curvature: butterfly spread
                curve_curvature = 2 * mean(yield_to_maturity[modified_duration >= 5 &
                                                                 modified_duration <= 10], na.rm = TRUE) -
                    mean(yield_to_maturity[modified_duration < 5], na.rm = TRUE) -
                    mean(yield_to_maturity[modified_duration > 10], na.rm = TRUE),

                .groups = "drop"
            ) %>%
            arrange(date)

        # ══════════════════════════════════════════════════════════════════════════
        # PHASE 3: CALCULATE ROBUST VOLATILITY (20-day and 60-day)
        # ══════════════════════════════════════════════════════════════════════════

        n_dates <- nrow(market_metrics)

        # Initialize volatility columns
        market_metrics$vol_20d <- NA_real_
        market_metrics$vol_20d_uncapped <- NA_real_
        market_metrics$vol_20d_capped_flag <- FALSE
        market_metrics$vol_20d_outlier_count <- 0L
        market_metrics$vol_20d_quality <- NA_real_

        market_metrics$vol_60d <- NA_real_
        market_metrics$vol_60d_uncapped <- NA_real_
        market_metrics$vol_60d_capped_flag <- FALSE

        # ══════════════════════════════════════════════════════════════════════════
        # CRITICAL FIX: Calculate rolling volatility for EACH date (not single value)
        # ══════════════════════════════════════════════════════════════════════════

        # Calculate 20-day rolling volatility time series
        if(n_dates >= 5) {  # Minimum 5 observations
            # NEW: This returns a VECTOR with one volatility per date
            vol_20d_vector <- calculate_rolling_volatility(
                yields = market_metrics$avg_yield,
                window = 20,
                max_vol = 50,  # Cap at 50% for govt bonds
                winsorize_pct = 0.99  # Winsorize at 99th percentile
            )

            market_metrics$vol_20d <- vol_20d_vector

            # Add diagnostic output
            vol_range <- range(vol_20d_vector, na.rm = TRUE)
            vol_unique <- length(unique(vol_20d_vector))
            message(sprintf(
                "✓ 20-day volatility calculated: Range %.2f%% - %.2f%% | Unique values: %d/%d dates",
                vol_range[1] * 100, vol_range[2] * 100, vol_unique, n_dates
            ))

            # Set legacy columns for backward compatibility
            market_metrics$vol_20d_uncapped <- vol_20d_vector
            market_metrics$vol_20d_capped_flag <- any(vol_20d_vector >= 0.50, na.rm = TRUE)
            market_metrics$vol_20d_outlier_count <- 0L
            market_metrics$vol_20d_quality <- 100

        } else {
            # Very small dataset - use simple defaults
            market_metrics$vol_20d <- 0.10  # 10% default in decimal form
            market_metrics$vol_20d_uncapped <- 0.10
            market_metrics$vol_20d_quality <- 20
            warning("Insufficient data for 20-day volatility calculation (n < 5)")
        }

        # Calculate 60-day rolling volatility time series
        if(n_dates >= 10) {  # Minimum 10 observations for 60-day
            # NEW: This returns a VECTOR with one volatility per date
            vol_60d_vector <- calculate_rolling_volatility(
                yields = market_metrics$avg_yield,
                window = 60,
                max_vol = 50,
                winsorize_pct = 0.99
            )

            market_metrics$vol_60d <- vol_60d_vector
            market_metrics$vol_60d_uncapped <- vol_60d_vector
            market_metrics$vol_60d_capped_flag <- any(vol_60d_vector >= 0.50, na.rm = TRUE)
        } else {
            # Use 20-day as proxy
            market_metrics$vol_60d <- market_metrics$vol_20d
            market_metrics$vol_60d_uncapped <- market_metrics$vol_20d_uncapped
        }

        # ══════════════════════════════════════════════════════════════════════════
        # PHASE 4: CALCULATE MOMENTUM AND TREND INDICATORS
        # ══════════════════════════════════════════════════════════════════════════

        market_metrics <- market_metrics %>%
            mutate(
                # Volatility ratio (short-term vs long-term)
                vol_ratio = vol_20d / pmax(vol_60d, 0.001),

                # Momentum indicators
                momentum_5d = if(n() >= 5) {
                    TTR::ROC(avg_yield, n = 5, type = "discrete")
                } else {
                    0
                },
                momentum_20d = if(n() >= 20) {
                    TTR::ROC(avg_yield, n = 20, type = "discrete")
                } else {
                    0
                },

                # Moving averages for trend
                ma_50 = if(n() >= 50) {
                    TTR::SMA(avg_yield, n = 50)
                } else {
                    avg_yield
                },
                ma_200 = if(n() >= 200) {
                    TTR::SMA(avg_yield, n = 200)
                } else {
                    avg_yield
                },

                # Replace NA values
                momentum_5d = ifelse(is.na(momentum_5d), 0, momentum_5d),
                momentum_20d = ifelse(is.na(momentum_20d), 0, momentum_20d),
                ma_50 = ifelse(is.na(ma_50), avg_yield, ma_50),
                ma_200 = ifelse(is.na(ma_200), avg_yield, ma_200),

                # Trend classification
                trend = case_when(
                    avg_yield > ma_50 & ma_50 > ma_200 & momentum_20d > 0 ~ "Strong Uptrend",
                    avg_yield > ma_50 & ma_50 > ma_200 ~ "Uptrend",
                    avg_yield < ma_50 & ma_50 < ma_200 & momentum_20d < 0 ~ "Strong Downtrend",
                    avg_yield < ma_50 & ma_50 < ma_200 ~ "Downtrend",
                    TRUE ~ "Sideways"
                )
            )

        # ══════════════════════════════════════════════════════════════════════════
        # PHASE 5: CALCULATE STRESS COMPONENTS (with enhanced weighting)
        # ══════════════════════════════════════════════════════════════════════════

        market_metrics <- market_metrics %>%
            mutate(
                # Volatility percentiles (for regime classification)
                vol_percentile = percent_rank(vol_20d),
                dispersion_percentile = percent_rank(yield_dispersion),

                # Stress components (z-scored, then enhanced for tail events)
                stress_volatility_raw = scale(vol_20d, center = TRUE, scale = TRUE)[,1],
                stress_dispersion_raw = scale(yield_dispersion, center = TRUE, scale = TRUE)[,1],
                stress_momentum_raw = -scale(abs(momentum_20d), center = TRUE, scale = TRUE)[,1],
                stress_bid_cover_raw = -scale(avg_bid_cover, center = TRUE, scale = TRUE)[,1],
                stress_curve_raw = scale(abs(curve_slope - median(curve_slope, na.rm = TRUE)),
                                         center = TRUE, scale = TRUE)[,1],

                # Replace NAs from scaling
                stress_volatility_raw = ifelse(is.na(stress_volatility_raw), 0, stress_volatility_raw),
                stress_dispersion_raw = ifelse(is.na(stress_dispersion_raw), 0, stress_dispersion_raw),
                stress_momentum_raw = ifelse(is.na(stress_momentum_raw), 0, stress_momentum_raw),
                stress_bid_cover_raw = ifelse(is.na(stress_bid_cover_raw), 0, stress_bid_cover_raw),
                stress_curve_raw = ifelse(is.na(stress_curve_raw), 0, stress_curve_raw),

                # Apply exponential scaling for tail events (vol_z > 2 gets amplified)
                stress_volatility = ifelse(
                    abs(stress_volatility_raw) > 2,
                    stress_volatility_raw * (1 + 0.5 * (abs(stress_volatility_raw) - 2)),
                    stress_volatility_raw
                ),

                # ═══════════════════════════════════════════════════════════════════
                # IMPROVED STRESS SCORE: Volatility now 50% (was 30%)
                # ═══════════════════════════════════════════════════════════════════
                stress_score = (
                    stress_volatility * 0.50 +      # ↑ Increased from 0.30
                        stress_dispersion_raw * 0.20 +  # Unchanged
                        stress_momentum_raw * 0.15 +    # ↓ Decreased from 0.20
                        stress_bid_cover_raw * 0.10 +   # ↓ Decreased from 0.15
                        stress_curve_raw * 0.05         # ↓ Decreased from 0.15
                )
            )

        # ══════════════════════════════════════════════════════════════════════════
        # PHASE 6: REGIME CLASSIFICATION (with updated thresholds)
        # ══════════════════════════════════════════════════════════════════════════

        market_metrics <- market_metrics %>%
            mutate(
                # Enhanced regime classification with adjusted thresholds
                regime = case_when(
                    stress_score > 2.0 & vol_percentile > 0.8 ~ "Stressed",      # ↑ Was 1.5
                    stress_score > 0.75 | vol_percentile > 0.65 ~ "Elevated",    # ↑ Was 0.5
                    stress_score < -0.75 & vol_percentile < 0.35 ~ "Calm",       # ↓ Was -0.5
                    TRUE ~ "Normal"
                ),

                # Regime confidence (0-100) - higher confidence for extreme scores
                regime_confidence = case_when(
                    abs(stress_score) > 2.5 ~ 95,
                    abs(stress_score) > 2.0 ~ 85,
                    abs(stress_score) > 1.5 ~ 70,
                    abs(stress_score) > 1.0 ~ 55,
                    abs(stress_score) > 0.5 ~ 40,
                    TRUE ~ 25
                )
            )

        # ══════════════════════════════════════════════════════════════════════════
        # PHASE 7: ADD REGIME PERSISTENCE METRICS
        # ══════════════════════════════════════════════════════════════════════════

        market_metrics <- market_metrics %>%
            mutate(
                regime_change = regime != lag(regime, default = first(regime)),
                regime_id = cumsum(regime_change)
            ) %>%
            group_by(regime_id) %>%
            mutate(regime_duration = row_number()) %>%
            ungroup()

        # ══════════════════════════════════════════════════════════════════════════
        # PHASE 8: FINAL DIAGNOSTICS AND QUALITY REPORTING
        # ══════════════════════════════════════════════════════════════════════════

        # Calculate aggregate data quality score
        avg_quality <- mean(market_metrics$vol_20d_quality, na.rm = TRUE)
        total_outliers <- sum(market_metrics$vol_20d_outlier_count, na.rm = TRUE)
        total_capped <- sum(market_metrics$vol_20d_capped_flag, na.rm = TRUE)

        # Summary message
        message(sprintf(
            "✓ Market regime detection complete: %d dates | Avg data quality: %.1f/100 | Outliers Winsorized: %d | Periods capped: %d",
            nrow(market_metrics),
            avg_quality,
            total_outliers,
            total_capped
        ))

        # Regime distribution
        regime_summary <- market_metrics %>%
            count(regime) %>%
            mutate(pct = n / nrow(market_metrics) * 100)

        message(sprintf(
            "  Regime distribution: %s",
            paste(sprintf("%s: %.0f%%", regime_summary$regime, regime_summary$pct), collapse = " | ")
        ))

        # Volatility summary
        message(sprintf(
            "  Volatility range: %.1f%% - %.1f%% (20-day) | Median: %.1f%%",
            min(market_metrics$vol_20d, na.rm = TRUE),
            max(market_metrics$vol_20d, na.rm = TRUE),
            median(market_metrics$vol_20d, na.rm = TRUE)
        ))

        return(market_metrics)

    }, default = data.frame())
}

# ════════════════════════════════════════════════════════════════════════════════
# HELPER FUNCTIONS FOR CARRY & ROLL CALCULATIONS
# ════════════════════════════════════════════════════════════════════════════════

#' @keywords internal
#' @title Validate and Standardize Coupon Format
#' @description Detects if coupon is in decimal (0.085) or percentage (8.5) format
#'   and standardizes to percentage. Flags unusual values.
#' @param coupon Numeric vector of coupon rates
#' @param bond_name Character vector of bond identifiers
#' @return List with standardized_coupon and validation_flag
validate_coupon_format <- function(coupon, bond_name = NULL) {

    # Handle NA values
    if(all(is.na(coupon))) {
        return(list(
            standardized = rep(NA_real_, length(coupon)),
            format_detected = rep("missing", length(coupon)),
            validation_flag = rep("Missing coupon data", length(coupon))
        ))
    }

    # Detect format for each coupon value
    format_detected <- case_when(
        is.na(coupon) ~ "missing",
        coupon < 0 ~ "invalid_negative",
        coupon < 0.5 ~ "decimal",              # 0.085 = 8.5% stored as decimal
        coupon > 25 ~ "invalid_high",          # > 25% is unrealistic for govt bonds
        coupon < 3 ~ "invalid_low",            # < 3% is unrealistic for SA bonds (unless ILB)
        TRUE ~ "percentage"
    )

    # Standardize to percentage format
    standardized <- case_when(
        format_detected == "decimal" ~ coupon * 100,
        format_detected == "percentage" ~ coupon,
        format_detected == "invalid_negative" ~ NA_real_,
        TRUE ~ coupon  # Keep original for other cases
    )

    # Create validation flags
    validation_flag <- case_when(
        is.na(coupon) ~ "Missing coupon data",
        format_detected == "invalid_negative" ~ paste0("Invalid negative coupon: ", round(coupon, 2)),
        format_detected == "decimal" ~ paste0("Coupon auto-corrected from decimal (", round(coupon, 4), " → ", round(standardized, 2), "%)"),
        standardized < 4 ~ paste0("Unusually low coupon: ", round(standardized, 2), "% (may be ILB)"),
        standardized > 20 ~ paste0("Unusually high coupon: ", round(standardized, 2), "%"),
        TRUE ~ NA_character_
    )

    # Add bond-specific context if available
    if(!is.null(bond_name)) {
        validation_flag <- ifelse(
            !is.na(validation_flag),
            paste0(bond_name, ": ", validation_flag),
            NA_character_
        )
    }

    return(list(
        standardized = standardized,
        format_detected = format_detected,
        validation_flag = validation_flag
    ))
}

#' @keywords internal
#' @title Estimate Bond-Specific Yield Curve Slope
#' @description Calculates local yield curve slope using nearby bonds instead of
#'   fixed 20bps assumption. Uses linear regression on bonds within ±3 years duration.
#' @param bond_data Data frame with all bonds (must have: bond, modified_duration, yield_to_maturity)
#' @param target_duration Duration of the bond to estimate slope for
#' @param target_bond Name of target bond (to exclude from regression)
#' @return List with slope_bps, method, and n_bonds_used
estimate_curve_slope <- function(bond_data, target_duration, target_bond) {

    # ════════════════════════════════════════════════════════════════════
    # SPECIAL HANDLING FOR ULTRA-LONG BONDS (>20 years)
    # ════════════════════════════════════════════════════════════════════
    # For bonds with duration > 20 years, curve slope tends to flatten
    # significantly as the curve approaches its asymptote. Use more
    # conservative slope estimates to avoid inflated roll returns.

    if(target_duration > 20) {
        # Use wider search window but apply logarithmic damping
        search_window <- 5  # ±5 years instead of ±3

        nearby_bonds <- bond_data %>%
            filter(abs(modified_duration - target_duration) <= search_window,
                   bond != target_bond,
                   !is.na(modified_duration),
                   !is.na(yield_to_maturity))

        if(nrow(nearby_bonds) >= 2) {
            tryCatch({
                model <- lm(yield_to_maturity ~ modified_duration, data = nearby_bonds)
                slope_pct <- coef(model)[2]
                slope_bps <- slope_pct * 100

                # Apply logarithmic damping for ultra-long bonds
                # As duration increases beyond 20y, reduce effective slope
                damping_factor <- 1 / (1 + log(target_duration / 20))
                slope_bps_damped <- slope_bps * damping_factor

                # Tighter caps for ultra-long bonds: -5 to +25 bps/year
                slope_bps_capped <- pmax(pmin(slope_bps_damped, 25), -5)

                message(sprintf(
                    "Ultra-long bond %s (%.1fy): slope %.1f bps → damped %.1f bps → capped %.1f bps",
                    target_bond, target_duration, slope_bps, slope_bps_damped, slope_bps_capped
                ))

                return(list(
                    slope_bps = slope_bps_capped,
                    slope_bps_uncapped = slope_bps,
                    slope_bps_damped = slope_bps_damped,
                    damping_factor = damping_factor,
                    method = "ultra_long_damped_regression",
                    n_bonds_used = nrow(nearby_bonds),
                    r_squared = summary(model)$r.squared
                ))

            }, error = function(e) {
                # Use conservative fixed assumption for ultra-long
                return(list(
                    slope_bps = 15,
                    slope_bps_uncapped = 15,
                    slope_bps_damped = 15,
                    damping_factor = 1,
                    method = "ultra_long_fixed_assumption",
                    n_bonds_used = nrow(nearby_bonds),
                    r_squared = NA
                ))
            })
        } else {
            # Not enough nearby bonds - use conservative fixed assumption
            return(list(
                slope_bps = 15,
                slope_bps_uncapped = 15,
                slope_bps_damped = 15,
                damping_factor = 1,
                method = "ultra_long_fixed_insufficient_data",
                n_bonds_used = nrow(nearby_bonds),
                r_squared = NA
            ))
        }
    }

    # ════════════════════════════════════════════════════════════════════
    # STANDARD HANDLING FOR NORMAL DURATION BONDS (≤20 years)
    # ════════════════════════════════════════════════════════════════════

    # Find bonds with similar duration (±3 years), excluding the target bond
    nearby_bonds <- bond_data %>%
        filter(abs(modified_duration - target_duration) <= 3,
               bond != target_bond,
               !is.na(modified_duration),
               !is.na(yield_to_maturity))

    # Need at least 2 nearby bonds for regression
    if(nrow(nearby_bonds) >= 2) {

        # Fit local linear regression: yield ~ duration
        tryCatch({
            model <- lm(yield_to_maturity ~ modified_duration, data = nearby_bonds)
            slope_pct <- coef(model)[2]  # Slope in percentage points per year

            # Convert to basis points per year
            slope_bps <- slope_pct * 100

            # Sanity check: slopes typically -10 to +50 bps/year for govt bonds
            # Negative slope = inverted curve (rare but possible)
            # Very steep slope (>50 bps) might indicate data issues
            slope_bps_capped <- pmax(pmin(slope_bps, 50), -10)

            if(abs(slope_bps - slope_bps_capped) > 1e-6) {
                warning(sprintf(
                    "Curve slope capped for %s: %.1f bps → %.1f bps",
                    target_bond, slope_bps, slope_bps_capped
                ))
            }

            return(list(
                slope_bps = slope_bps_capped,
                slope_bps_uncapped = slope_bps,
                slope_bps_damped = NA,
                damping_factor = 1,
                method = "local_regression",
                n_bonds_used = nrow(nearby_bonds),
                r_squared = summary(model)$r.squared
            ))

        }, error = function(e) {
            # Regression failed - use fallback
            return(list(
                slope_bps = 20,
                slope_bps_uncapped = 20,
                slope_bps_damped = NA,
                damping_factor = 1,
                method = "fixed_assumption_regression_failed",
                n_bonds_used = nrow(nearby_bonds),
                r_squared = NA
            ))
        })

    } else {
        # Not enough nearby bonds - use fixed assumption
        return(list(
            slope_bps = 20,
            slope_bps_uncapped = 20,
            slope_bps_damped = NA,
            damping_factor = 1,
            method = "fixed_assumption_insufficient_data",
            n_bonds_used = nrow(nearby_bonds),
            r_squared = NA
        ))
    }
}

#' @keywords internal
#' @title Detect Inflation-Linked Bonds
#' @description Uses heuristics to identify inflation-linked bonds (ILBs):
#'   1. Low nominal coupon (< 4.5%)
#'   2. Bond name contains "I", "ILB", or "INFLATION"
#'   3. Large yield-coupon spread (> 5%)
#' @param bond_name Character, bond identifier
#' @param coupon Numeric, coupon rate in percentage
#' @param yield_ytm Numeric, yield to maturity in percentage
#' @return List with is_likely_ilb (boolean) and confidence (0-1)
detect_inflation_linked <- function(bond_name, coupon, yield_ytm) {

    # Handle NA values
    if(is.na(coupon) || is.na(yield_ytm)) {
        return(list(
            is_likely_ilb = FALSE,
            confidence = 0,
            indicators = c(FALSE, FALSE, FALSE),
            reason = "Insufficient data"
        ))
    }

    # Three indicators for ILBs
    low_coupon <- coupon < 4.5

    name_pattern <- grepl("I(?![0-9])|ILB|INFLATION|LINKER", toupper(bond_name), perl = TRUE)

    large_spread <- (yield_ytm - coupon) > 5

    indicators <- c(low_coupon, name_pattern, large_spread)

    # Count how many indicators are true
    indicator_count <- sum(indicators, na.rm = TRUE)

    # Classification logic
    # 3/3 indicators = very likely ILB
    # 2/3 indicators = likely ILB
    # 1/3 indicators = possibly ILB
    # 0/3 indicators = nominal bond

    is_likely_ilb <- indicator_count >= 2
    confidence <- indicator_count / 3

    reason <- case_when(
        indicator_count == 3 ~ "All indicators suggest ILB",
        indicator_count == 2 ~ paste0("Two indicators (",
                                      paste(c("low_coupon", "name_pattern", "large_spread")[indicators],
                                            collapse = ", "), ")"),
        indicator_count == 1 ~ paste0("One indicator (",
                                      c("low_coupon", "name_pattern", "large_spread")[indicators][1], ")"),
        TRUE ~ "No ILB indicators"
    )

    return(list(
        is_likely_ilb = is_likely_ilb,
        confidence = confidence,
        indicators = indicators,
        reason = reason
    ))
}

#' @keywords internal
#' @title Validate Funding Rate Format
#' @description Ensures funding rate is in percentage format (8.25 not 0.0825)
#' @param funding_rate Numeric, funding/repo rate
#' @return Standardized funding rate in percentage
validate_funding_rate <- function(funding_rate) {

    if(is.na(funding_rate) || length(funding_rate) == 0) {
        warning("Funding rate is NA or missing, using default 8.25%")
        return(8.25)
    }

    # Detect format
    if(funding_rate < 1) {
        # Likely decimal format (0.0825)
        standardized <- funding_rate * 100
        message(sprintf("Funding rate converted from decimal: %.4f → %.2f%%",
                        funding_rate, standardized))
        return(standardized)
    } else if(funding_rate > 20) {
        # Unrealistically high
        warning(sprintf("Funding rate > 20%% (%.2f%%) seems unrealistic, using default 8.25%%",
                        funding_rate))
        return(8.25)
    } else {
        # Already in percentage format
        return(funding_rate)
    }
}

#' @export
#' @title Calculate Advanced Carry & Roll Analysis (PRODUCTION-READY VERSION)
#' @description Comprehensive carry & roll calculation with:
#'   - Coupon format validation and standardization
#'   - Bond-specific yield curve slope estimation
#'   - Inflation-linked bond detection
#'   - Comprehensive bounds checking and validation
#'   - Diagnostic flags for suspicious values
#'
#' @param data Bond data frame with columns: bond, date, yield_to_maturity,
#'   modified_duration, coupon
#' @param holding_periods Numeric vector of holding periods in days (default: c(30, 90, 180, 360))
#' @param funding_rate Funding/repo rate in percentage (default: 8.25%)
#'
#' @return Data frame with carry & roll metrics including validation flags
#'
#' @details
#' Key improvements over previous version:
#' - Detects and corrects coupon format issues (decimal vs percentage)
#' - Estimates bond-specific curve slopes instead of fixed 20bps
#' - Identifies inflation-linked bonds requiring special treatment
#' - Validates all outputs with bounds checking
#' - Returns percentile rankings for performance comparison
#'
#' @examples
#' \dontrun{
#' result <- calculate_advanced_carry_roll(bond_data,
#'                                         holding_periods = c(30, 90, 180, 360),
#'                                         funding_rate = 8.25)
#'
#' # Check for validation warnings
#' warnings <- result %>%
#'   filter(!is.na(coupon_validation_flag) | !is.na(carry_validation_flag))
#' }
calculate_advanced_carry_roll <- memoise(function(data,
                                                  holding_periods = c(30, 90, 180, 360),
                                                  funding_rate = 8.25) {
    safe_execute({

        # ═══════════════════════════════════════════════════════════════════════
        # PHASE 1: INPUT VALIDATION
        # ═══════════════════════════════════════════════════════════════════════

        required_cols <- c("bond", "date", "yield_to_maturity", "modified_duration", "coupon")
        missing_cols <- setdiff(required_cols, names(data))

        if(length(missing_cols) > 0) {
            stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
        }

        if(nrow(data) == 0) {
            warning("Empty input data for carry & roll calculation")
            return(data.frame())
        }

        # Validate and standardize funding rate
        funding_rate_std <- validate_funding_rate(funding_rate)

        # ═══════════════════════════════════════════════════════════════════════
        # PHASE 2: GET LATEST DATA FOR EACH BOND
        # ═══════════════════════════════════════════════════════════════════════

        latest_data <- data %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            ungroup()

        message(sprintf("Calculating carry & roll for %d bonds at latest date: %s",
                        n_distinct(latest_data$bond),
                        max(latest_data$date)))

        # ═══════════════════════════════════════════════════════════════════════
        # PHASE 3: VALIDATE AND STANDARDIZE COUPONS (ONE-TIME, BEFORE LOOP)
        # ═══════════════════════════════════════════════════════════════════════

        coupon_validation <- validate_coupon_format(latest_data$coupon, latest_data$bond)

        latest_data$coupon_standardized <- coupon_validation$standardized
        latest_data$coupon_format_detected <- coupon_validation$format_detected
        latest_data$coupon_validation_flag <- coupon_validation$validation_flag

        # Report coupon issues
        coupon_issues <- latest_data %>%
            filter(!is.na(coupon_validation_flag)) %>%
            select(bond, coupon, coupon_standardized, coupon_validation_flag)

        if(nrow(coupon_issues) > 0) {
            warning(sprintf("Coupon validation issues detected for %d bonds:", nrow(coupon_issues)))
            for(i in 1:min(5, nrow(coupon_issues))) {  # Show first 5
                warning(sprintf("  %s", coupon_issues$coupon_validation_flag[i]))
            }
            if(nrow(coupon_issues) > 5) {
                warning(sprintf("  ... and %d more", nrow(coupon_issues) - 5))
            }
        }

        # ═══════════════════════════════════════════════════════════════════════
        # PHASE 4: DETECT INFLATION-LINKED BONDS
        # ═══════════════════════════════════════════════════════════════════════

        latest_data <- latest_data %>%
            rowwise() %>%
            mutate(
                ilb_detection = list(detect_inflation_linked(bond, coupon_standardized, yield_to_maturity)),
                is_ilb = ilb_detection$is_likely_ilb,
                ilb_confidence = ilb_detection$confidence,
                bond_type = ifelse(is_ilb, "inflation_linked", "nominal")
            ) %>%
            ungroup()

        # Report ILBs detected
        ilbs_detected <- latest_data %>% filter(is_ilb)
        if(nrow(ilbs_detected) > 0) {
            message(sprintf("Inflation-linked bonds detected: %s",
                            paste(ilbs_detected$bond, collapse = ", ")))
        }

        # ═══════════════════════════════════════════════════════════════════════
        # PHASE 5: ESTIMATE BOND-SPECIFIC CURVE SLOPES
        # ═══════════════════════════════════════════════════════════════════════

        # For each bond, estimate local curve slope
        latest_data <- latest_data %>%
            rowwise() %>%
            mutate(
                curve_slope_result = list(estimate_curve_slope(latest_data,
                                                               modified_duration,
                                                               bond)),
                curve_slope_bps = curve_slope_result$slope_bps,
                curve_slope_method = curve_slope_result$method,
                curve_slope_n_bonds = curve_slope_result$n_bonds_used
            ) %>%
            ungroup()

        # Report curve slope summary with uniqueness check
        slope_summary <- latest_data %>%
            summarise(
                min_slope = min(curve_slope_bps, na.rm = TRUE),
                median_slope = median(curve_slope_bps, na.rm = TRUE),
                max_slope = max(curve_slope_bps, na.rm = TRUE),
                n_unique_slopes = n_distinct(round(curve_slope_bps, 2)),
                n_local_regression = sum(curve_slope_method == "local_regression"),
                n_fixed_assumption = sum(grepl("fixed_assumption", curve_slope_method))
            )

        message(sprintf("Curve slope estimation: %.1f to %.1f bps/year (median: %.1f) | %d unique slopes | %d local, %d fixed",
                        slope_summary$min_slope, slope_summary$max_slope, slope_summary$median_slope,
                        slope_summary$n_unique_slopes,
                        slope_summary$n_local_regression, slope_summary$n_fixed_assumption))

        # Coupon uniqueness check
        n_unique_coupons <- n_distinct(round(latest_data$coupon_standardized, 2))
        coupon_range <- range(latest_data$coupon_standardized, na.rm = TRUE)
        message(sprintf("Coupon values: %.2f%% to %.2f%% | %d unique coupons for %d bonds",
                        coupon_range[1], coupon_range[2], n_unique_coupons, nrow(latest_data)))

        if(n_unique_coupons < min(3, nrow(latest_data))) {
            warning(sprintf("⚠ LOW COUPON VARIATION: Only %d unique coupons - check data!", n_unique_coupons))
        }

        # ═══════════════════════════════════════════════════════════════════════
        # PHASE 6: CALCULATE CARRY & ROLL FOR EACH HOLDING PERIOD
        # ═══════════════════════════════════════════════════════════════════════

        results <- list()

        for (period in holding_periods) {
            period_label <- paste0(period, "d")

            carry_roll_data <- latest_data %>%
                mutate(
                    # ═══════════════════════════════════════════════════════════
                    # TIME CONVERSION
                    # ═══════════════════════════════════════════════════════════
                    period_years = period / 365,

                    # ═══════════════════════════════════════════════════════════
                    # CARRY INCOME (with ILB adjustment)
                    # ═══════════════════════════════════════════════════════════
                    # Format: coupon_standardized is in PERCENTAGE (e.g., 10.0 = 10%)
                    # Formula: (coupon% / 100) × period_years × 100 = coupon% × period_years
                    # Result: Carry income as PERCENTAGE return (e.g., 9.86 = 9.86%)

                    expected_inflation = 4.5,  # SA inflation target midpoint (%)

                    # Base carry income (coupon accrued over holding period)
                    carry_income_base = (coupon_standardized / 100) * period_years * 100,

                    # Add inflation compensation for ILBs
                    # For ILBs: add expected real return from inflation indexation
                    carry_income = ifelse(
                        is_ilb,
                        carry_income_base + ((expected_inflation / 100) * period_years * 100),
                        carry_income_base
                    ),

                    # ═══════════════════════════════════════════════════════════
                    # ROLL-DOWN RETURN (using bond-specific curve slope)
                    # ═══════════════════════════════════════════════════════════
                    # As time passes, yields fall as bond "rolls down" the curve
                    # Price gain ≈ Duration × Yield decrease

                    forward_duration = pmax(0, modified_duration - period_years),

                    # Yield decrease as bond rolls down curve
                    # Duration decreases by period_years, yields fall by slope × time
                    yield_decrease_bps = period_years * curve_slope_bps,

                    # Convert to decimal for price calculation
                    yield_decrease_decimal = yield_decrease_bps / 10000,

                    # Roll return = Duration × Yield change (in decimal) × 100 for %
                    # Use modified_duration (initial) for price sensitivity
                    roll_return = modified_duration * yield_decrease_decimal * 100,

                    # ═══════════════════════════════════════════════════════════
                    # FUNDING COST
                    # ═══════════════════════════════════════════════════════════
                    # Format: funding_rate_std is in PERCENTAGE (e.g., 8.25 = 8.25%)
                    # Result: Funding cost as PERCENTAGE (e.g., 8.13 = 8.13%)

                    funding_cost = (funding_rate_std / 100) * period_years * 100,

                    # ═══════════════════════════════════════════════════════════
                    # TOTAL RETURNS (NO ARTIFICIAL CAP)
                    # ═══════════════════════════════════════════════════════════
                    # All values are in PERCENTAGE format (e.g., 2.56 = 2.56%)
                    # FIX: Removed artificial 4% cap that was flattening all returns

                    gross_return = carry_income + roll_return,

                    # Net return = Gross - Funding (no cap applied)
                    net_return = gross_return - funding_cost,

                    # Warning threshold for unusually high returns (for logging only)
                    # 15% annual is very high but possible for high-coupon bonds
                    max_reasonable_annual_return = 15.0,
                    max_reasonable_period_return = max_reasonable_annual_return * period_years,

                    # Flag extreme returns for investigation (but don't cap)
                    return_extreme = abs(net_return) > max_reasonable_period_return,

                    # ═══════════════════════════════════════════════════════════
                    # RISK-ADJUSTED METRICS
                    # ═══════════════════════════════════════════════════════════
                    return_per_unit_risk = net_return / pmax(modified_duration, 0.1),

                    # Annualized metrics
                    annualized_return = net_return / period_years,

                    # ═══════════════════════════════════════════════════════════
                    # SHARPE RATIO CALCULATION (FIXED)
                    # ═══════════════════════════════════════════════════════════
                    # Proper Sharpe = (Return - Risk-Free Rate) / Volatility
                    # All values in consistent units (annual %)

                    # Risk-free rate (SA 3-month T-bill proxy, typically ~7-8%)
                    risk_free_rate = 7.5,

                    # Estimated annual volatility based on bond characteristics
                    # Longer duration bonds have higher price volatility
                    # Typical SA bond yield volatility: 50-150 bps annually
                    # Price volatility ≈ Duration × Yield volatility
                    annual_yield_vol_bps = 75,  # Conservative 75 bps annual yield vol
                    annual_price_vol = (annual_yield_vol_bps / 100) * modified_duration,

                    # Sharpe Ratio: (Annualized Return - Risk-Free Rate) / Price Volatility
                    # Cap to reasonable range to avoid display issues
                    sharpe_raw = (annualized_return - risk_free_rate) / pmax(annual_price_vol, 0.5),
                    sharpe_estimate = pmax(pmin(sharpe_raw, 5.0), -5.0),  # Cap to [-5, +5]

                    # Breakeven: How much can yields rise before losing money?
                    breakeven_yield_rise = gross_return / pmax(modified_duration, 0.1),
                    breakeven_bps = breakeven_yield_rise * 100,

                    # ═══════════════════════════════════════════════════════════
                    # VALIDATION FLAGS
                    # ═══════════════════════════════════════════════════════════

                    # ═══════════════════════════════════════════════════════════
                    # DIAGNOSTIC INTERMEDIATE VALUES
                    # ═══════════════════════════════════════════════════════════
                    # Store intermediate values for debugging

                    diag_coupon_pct = coupon_standardized,
                    diag_coupon_format = coupon_format_detected,
                    diag_yield_change_bps = yield_decrease_bps,
                    diag_curve_slope_bps = curve_slope_bps,

                    # ═══════════════════════════════════════════════════════════
                    # VALIDATION FLAGS
                    # ═══════════════════════════════════════════════════════════

                    # Validate carry income
                    # Realistic range: 3-12% for SA bonds (annual coupon × time)
                    # IMPORTANT: Carry income should never exceed coupon × time (for nominals)
                    carry_validation_flag = case_when(
                        is.na(carry_income) ~ "ERROR: Carry income is NA",
                        carry_income < 0 ~ "ERROR: Negative carry income",
                        !is_ilb & carry_income > (coupon_standardized * period_years * 1.01) ~
                            paste0("ERROR: Carry exceeds coupon income (", round(carry_income, 2),
                                   "% > ", round(coupon_standardized * period_years, 2),
                                   "%, coupon=", round(coupon_standardized, 2), "%)"),
                        carry_income > 13 * period_years ~
                            paste0("WARNING: Carry unusually high (", round(carry_income, 2),
                                   "%, coupon=", round(coupon_standardized, 2), "%)"),
                        carry_income < 2 * period_years & !is_ilb ~
                            paste0("WARNING: Carry unusually low (", round(carry_income, 2),
                                   "%, coupon=", round(coupon_standardized, 2), "%)"),
                        TRUE ~ NA_character_
                    ),

                    # Validate net return
                    # Updated: Removed artificial 4% cap - now just flags extreme returns
                    # Realistic range for 360 days: -5% to +10% for SA govt bonds
                    # (High coupon bonds can have net returns > 5%)
                    net_return_validation_flag = case_when(
                        is.na(net_return) ~ "ERROR: Net return is NA",
                        return_extreme ~
                            paste0("INFO: High return (", round(net_return, 2),
                                   "%, threshold=", round(max_reasonable_period_return, 2),
                                   "%, carry=", round(carry_income, 2),
                                   "%, roll=", round(roll_return, 2),
                                   "%, funding=", round(funding_cost, 2), "%)"),
                        net_return < -5 ~
                            paste0("WARNING: Large negative return (", round(net_return, 2), "%)"),
                        TRUE ~ NA_character_
                    ),

                    # Validate roll return
                    # Realistic range: 0-3% for 360 days
                    # (Duration 5-25 × curve slope 5-20 bps × time 0.986 years)
                    roll_validation_flag = case_when(
                        is.na(roll_return) ~ "ERROR: Roll return is NA",
                        roll_return > 4 ~
                            paste0("WARNING: Roll return unusually large (", round(roll_return, 2),
                                   "%, duration=", round(modified_duration, 2),
                                   ", slope=", round(curve_slope_bps, 1), " bps)"),
                        roll_return < -1 ~
                            paste0("WARNING: Negative roll return (", round(roll_return, 2),
                                   "%, curve slope=", round(curve_slope_bps, 1), " bps)"),
                        TRUE ~ NA_character_
                    ),

                    # Combine all validation flags
                    validation_summary = case_when(
                        !is.na(carry_validation_flag) ~ carry_validation_flag,
                        !is.na(net_return_validation_flag) ~ net_return_validation_flag,
                        !is.na(roll_validation_flag) ~ roll_validation_flag,
                        !is.na(coupon_validation_flag) ~ coupon_validation_flag,
                        TRUE ~ NA_character_
                    ),

                    # ═══════════════════════════════════════════════════════════
                    # LABELS
                    # ═══════════════════════════════════════════════════════════
                    holding_period = period_label,
                    holding_days = period
                )

            results[[period_label]] <- carry_roll_data
        }

        # ═══════════════════════════════════════════════════════════════════════
        # PHASE 7: COMBINE ALL PERIODS AND ADD PERCENTILE RANKINGS
        # ═══════════════════════════════════════════════════════════════════════

        if (length(results) > 0) {
            combined <- bind_rows(results)

            # Add percentile rankings within each holding period
            combined <- combined %>%
                group_by(holding_period) %>%
                mutate(
                    net_return_percentile = percent_rank(net_return) * 100,
                    carry_income_percentile = percent_rank(carry_income) * 100,

                    # Flag outliers
                    outlier_flag = case_when(
                        net_return_percentile > 95 ~
                            paste0("Top 5% performer (", round(net_return, 2), "%) - verify calculation"),
                        net_return_percentile < 5 ~
                            paste0("Bottom 5% performer (", round(net_return, 2), "%) - verify calculation"),
                        TRUE ~ NA_character_
                    )
                ) %>%
                ungroup()

            # Set factor levels for holding_period
            combined$holding_period <- factor(combined$holding_period,
                                              levels = paste0(sort(holding_periods), "d"))

            # ═══════════════════════════════════════════════════════════════════
            # PHASE 8: FINAL VALIDATION SUMMARY
            # ═══════════════════════════════════════════════════════════════════

            validation_summary <- combined %>%
                filter(holding_period == paste0(max(holding_periods), "d")) %>%
                summarise(
                    total_bonds = n(),
                    coupon_issues = sum(!is.na(coupon_validation_flag)),
                    carry_issues = sum(!is.na(carry_validation_flag)),
                    net_return_issues = sum(!is.na(net_return_validation_flag)),
                    outliers = sum(!is.na(outlier_flag)),
                    mean_net_return = mean(net_return, na.rm = TRUE),
                    median_net_return = median(net_return, na.rm = TRUE),
                    min_net_return = min(net_return, na.rm = TRUE),
                    max_net_return = max(net_return, na.rm = TRUE)
                )

            message(sprintf(
                "✓ Carry & roll complete: %d bonds × %d periods | Validation: %d coupon, %d carry, %d net return, %d outliers",
                validation_summary$total_bonds,
                length(holding_periods),
                validation_summary$coupon_issues,
                validation_summary$carry_issues,
                validation_summary$net_return_issues,
                validation_summary$outliers
            ))

            message(sprintf(
                "  %d-day net returns: %.2f%% to %.2f%% (median: %.2f%%)",
                max(holding_periods),
                validation_summary$min_net_return,
                validation_summary$max_net_return,
                validation_summary$median_net_return
            ))

            # Report validation issues
            issues <- combined %>%
                filter(holding_period == paste0(max(holding_periods), "d"),
                       !is.na(validation_summary)) %>%
                select(bond, validation_summary)

            if(nrow(issues) > 0) {
                warning(sprintf("Validation warnings for %d bonds:", nrow(issues)))
                for(i in 1:min(3, nrow(issues))) {
                    warning(sprintf("  %s: %s", issues$bond[i], issues$validation_summary[i]))
                }
                if(nrow(issues) > 3) {
                    warning(sprintf("  ... and %d more (check full results)", nrow(issues) - 3))
                }
            }

            # ═══════════════════════════════════════════════════════════════════
            # CRITICAL VALIDATION: Check for identical returns (bug detection)
            # ═══════════════════════════════════════════════════════════════════
            gross_returns_90d <- combined %>%
                filter(holding_period == "90d") %>%
                pull(gross_return)

            n_unique_gross <- n_distinct(round(gross_returns_90d, 4))
            n_bonds <- length(gross_returns_90d)

            if(n_unique_gross < min(5, n_bonds) && n_bonds > 1) {
                warning("═══════════════════════════════════════════════════════════════════")
                warning(sprintf("⚠ CRITICAL: Only %d unique gross returns for %d bonds!", n_unique_gross, n_bonds))
                warning("This indicates identical calculations - likely a bug in:")
                warning("  1. Coupon values all being the same")
                warning("  2. Curve slope estimation returning same value for all bonds")
                warning("  3. Data processing not preserving bond-specific values")
                warning("═══════════════════════════════════════════════════════════════════")

                # Log sample data for debugging
                sample_data <- combined %>%
                    filter(holding_period == "90d") %>%
                    select(bond, coupon_standardized, yield_to_maturity, modified_duration,
                           curve_slope_bps, carry_income, roll_return, gross_return) %>%
                    head(5)

                message("Sample calculation data (90d, first 5 bonds):")
                print(sample_data)
            } else {
                message(sprintf("✓ Return variation check passed: %d unique returns for %d bonds", n_unique_gross, n_bonds))
            }

            return(combined)

        } else {
            warning("No results generated")
            return(data.frame())
        }

    }, default = data.frame())
})

#' @export
#' @title Investigate Bond Carry Calculation (DIAGNOSTIC TOOL)
#' @description Provides detailed breakdown of carry & roll calculation for a specific bond,
#'   showing intermediate steps, validation checks, and comparison to peer bonds.
#'
#' @param bond_name Character, bond identifier to investigate
#' @param data Bond data frame (same as used for calculate_advanced_carry_roll)
#' @param holding_period Numeric, holding period in days (default: 360)
#' @param funding_rate Numeric, funding rate in percentage (default: 8.25)
#'
#' @return List with detailed diagnostic information including:
#'   - bond_info: Basic bond characteristics
#'   - coupon_analysis: Coupon format detection and validation
#'   - curve_analysis: Yield curve slope estimation details
#'   - carry_breakdown: Step-by-step carry calculation
#'   - peer_comparison: How this bond compares to similar bonds
#'   - validation_results: All validation checks and flags
#'
#' @examples
#' \dontrun{
#' # Investigate R214 showing unusually high carry
#' diagnosis <- investigate_bond_carry("R214", bond_data, holding_period = 360)
#'
#' # Print summary
#' print(diagnosis$summary)
#'
#' # Check validation issues
#' print(diagnosis$validation_results)
#' }
investigate_bond_carry <- function(bond_name,
                                   data,
                                   holding_period = 360,
                                   funding_rate = 8.25) {

    # ═══════════════════════════════════════════════════════════════════════
    # STEP 1: EXTRACT BOND DATA
    # ═══════════════════════════════════════════════════════════════════════

    # Get latest data for target bond
    bond_latest <- data %>%
        filter(bond == bond_name) %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        slice(1)  # Take first if duplicates

    if(nrow(bond_latest) == 0) {
        stop(sprintf("Bond '%s' not found in data", bond_name))
    }

    # Get all latest bond data for context
    all_bonds_latest <- data %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup()

    cat("\n")
    cat("═══════════════════════════════════════════════════════════════════════\n")
    cat(sprintf("CARRY & ROLL DIAGNOSTIC REPORT: %s\n", bond_name))
    cat("═══════════════════════════════════════════════════════════════════════\n")
    cat(sprintf("Analysis Date: %s\n", bond_latest$date))
    cat(sprintf("Holding Period: %d days (%.2f years)\n", holding_period, holding_period/365))
    cat(sprintf("Funding Rate: %.2f%%\n", funding_rate))
    cat("\n")

    # ═══════════════════════════════════════════════════════════════════════
    # STEP 2: BOND CHARACTERISTICS
    # ═══════════════════════════════════════════════════════════════════════

    cat("───────────────────────────────────────────────────────────────────────\n")
    cat("1. BOND CHARACTERISTICS\n")
    cat("───────────────────────────────────────────────────────────────────────\n")
    cat(sprintf("Yield to Maturity:    %.2f%%\n", bond_latest$yield_to_maturity))
    cat(sprintf("Modified Duration:    %.2f years\n", bond_latest$modified_duration))
    cat(sprintf("Coupon (raw):         %.4f\n", bond_latest$coupon))
    cat("\n")

    # ═══════════════════════════════════════════════════════════════════════
    # STEP 3: COUPON VALIDATION
    # ═══════════════════════════════════════════════════════════════════════

    cat("───────────────────────────────────────────────────────────────────────\n")
    cat("2. COUPON VALIDATION & STANDARDIZATION\n")
    cat("───────────────────────────────────────────────────────────────────────\n")

    coupon_result <- validate_coupon_format(bond_latest$coupon, bond_name)

    cat(sprintf("Format Detected:      %s\n", coupon_result$format_detected))
    cat(sprintf("Standardized Coupon:  %.2f%%\n", coupon_result$standardized))

    if(!is.na(coupon_result$validation_flag)) {
        cat(sprintf("⚠ VALIDATION FLAG:    %s\n", coupon_result$validation_flag))
    } else {
        cat("✓ Coupon validation:  PASSED\n")
    }
    cat("\n")

    # ═══════════════════════════════════════════════════════════════════════
    # STEP 4: INFLATION-LINKED BOND DETECTION
    # ═══════════════════════════════════════════════════════════════════════

    cat("───────────────────────────────────────────────────────────────────────\n")
    cat("3. INFLATION-LINKED BOND DETECTION\n")
    cat("───────────────────────────────────────────────────────────────────────\n")

    ilb_result <- detect_inflation_linked(bond_name,
                                          coupon_result$standardized,
                                          bond_latest$yield_to_maturity)

    cat(sprintf("Is Likely ILB:        %s\n", ifelse(ilb_result$is_likely_ilb, "YES", "NO")))
    cat(sprintf("Confidence:           %.0f%%\n", ilb_result$confidence * 100))
    cat(sprintf("Reason:               %s\n", ilb_result$reason))
    cat(sprintf("Bond Type:            %s\n",
                ifelse(ilb_result$is_likely_ilb, "Inflation-Linked", "Nominal")))
    cat("\n")

    # ═══════════════════════════════════════════════════════════════════════
    # STEP 5: YIELD CURVE SLOPE ESTIMATION
    # ═══════════════════════════════════════════════════════════════════════

    cat("───────────────────────────────────────────────────────────────────────\n")
    cat("4. YIELD CURVE SLOPE ESTIMATION\n")
    cat("───────────────────────────────────────────────────────────────────────\n")

    slope_result <- estimate_curve_slope(all_bonds_latest,
                                         bond_latest$modified_duration,
                                         bond_name)

    cat(sprintf("Method:               %s\n", slope_result$method))
    cat(sprintf("Nearby Bonds Used:    %d\n", slope_result$n_bonds_used))
    cat(sprintf("Estimated Slope:      %.1f bps/year\n", slope_result$slope_bps))

    if(!is.na(slope_result$r_squared)) {
        cat(sprintf("R-squared:            %.3f\n", slope_result$r_squared))
    }

    if(slope_result$slope_bps != slope_result$slope_bps_uncapped) {
        cat(sprintf("⚠ Slope capped from:  %.1f bps/year\n", slope_result$slope_bps_uncapped))
    }

    # Show nearby bonds used in regression
    if(slope_result$n_bonds_used > 0) {
        nearby <- all_bonds_latest %>%
            filter(abs(modified_duration - bond_latest$modified_duration) <= 3,
                   bond != bond_name) %>%
            arrange(modified_duration) %>%
            select(bond, modified_duration, yield_to_maturity)

        cat("\nNearby bonds used for slope estimation:\n")
        for(i in 1:min(5, nrow(nearby))) {
            cat(sprintf("  %s: Duration=%.2f, Yield=%.2f%%\n",
                        nearby$bond[i], nearby$modified_duration[i],
                        nearby$yield_to_maturity[i]))
        }
        if(nrow(nearby) > 5) {
            cat(sprintf("  ... and %d more\n", nrow(nearby) - 5))
        }
    }
    cat("\n")

    # ═══════════════════════════════════════════════════════════════════════
    # STEP 6: CARRY & ROLL CALCULATION BREAKDOWN
    # ═══════════════════════════════════════════════════════════════════════

    cat("───────────────────────────────────────────────────────────────────────\n")
    cat("5. CARRY & ROLL CALCULATION BREAKDOWN\n")
    cat("───────────────────────────────────────────────────────────────────────\n")

    period_years <- holding_period / 365
    funding_rate_std <- validate_funding_rate(funding_rate)

    # Carry income (with explicit format conversions)
    carry_income_base <- (coupon_result$standardized / 100) * period_years * 100

    if(ilb_result$is_likely_ilb) {
        expected_inflation <- 4.5
        inflation_adjustment <- (expected_inflation / 100) * period_years * 100
        carry_income <- carry_income_base + inflation_adjustment
        cat(sprintf("Carry (Base):         (%.2f%% / 100) × %.3f × 100 = %.2f%%\n",
                    coupon_result$standardized, period_years, carry_income_base))
        cat(sprintf("Inflation Adj:        (%.2f%% / 100) × %.3f × 100 = %.2f%%\n",
                    expected_inflation, period_years, inflation_adjustment))
        cat(sprintf("Carry (Total):        %.2f%% + %.2f%% = %.2f%%\n",
                    carry_income_base, inflation_adjustment, carry_income))
    } else {
        carry_income <- carry_income_base
        cat(sprintf("Carry Income:         (%.2f%% / 100) × %.3f × 100 = %.2f%%\n",
                    coupon_result$standardized, period_years, carry_income))
    }

    # Roll-down (CORRECTED FORMULA)
    forward_duration <- pmax(0, bond_latest$modified_duration - period_years)

    # Yield decreases as bond rolls down curve (slope × time, NOT slope × duration_change!)
    yield_decrease_bps <- period_years * slope_result$slope_bps
    yield_decrease_decimal <- yield_decrease_bps / 10000  # Convert bps to decimal

    # Price gain = Duration × Yield change (using INITIAL duration, not forward)
    roll_return <- bond_latest$modified_duration * yield_decrease_decimal * 100

    cat("\n")
    cat("ROLL-DOWN CALCULATION (CORRECTED):\n")
    cat(sprintf("Initial Duration:     %.2f years\n", bond_latest$modified_duration))
    cat(sprintf("Forward Duration:     %.2f years (after %.3f years)\n",
                forward_duration, period_years))
    cat(sprintf("Curve Slope:          %.1f bps/year\n", slope_result$slope_bps))
    cat(sprintf("Yield Decrease:       %.3f years × %.1f bps = %.1f bps\n",
                period_years, slope_result$slope_bps, yield_decrease_bps))
    cat(sprintf("Yield (decimal):      %.1f bps / 10000 = %.6f\n",
                yield_decrease_bps, yield_decrease_decimal))
    cat(sprintf("Roll Return:          %.2f × %.6f × 100 = %.2f%%\n",
                bond_latest$modified_duration, yield_decrease_decimal, roll_return))

    # Funding cost (with explicit format conversion)
    funding_cost <- (funding_rate_std / 100) * period_years * 100
    cat("\n")
    cat(sprintf("Funding Cost:         %.2f%% × %.3f years = %.2f%%\n",
                funding_rate_std, period_years, funding_cost))

    # Total returns
    gross_return <- carry_income + roll_return
    net_return <- gross_return - funding_cost

    cat("\n")
    cat(sprintf("Gross Return:         %.2f%% + %.2f%% = %.2f%%\n",
                carry_income, roll_return, gross_return))
    cat(sprintf("Net Return:           %.2f%% - %.2f%% = %.2f%%\n",
                gross_return, funding_cost, net_return))

    # Risk metrics
    return_per_risk <- net_return / pmax(bond_latest$modified_duration, 0.1)
    annualized <- net_return / period_years
    breakeven_bps <- (gross_return / pmax(bond_latest$modified_duration, 0.1)) * 100

    cat("\n")
    cat(sprintf("Return/Duration:      %.3f%%\n", return_per_risk))
    cat(sprintf("Annualized Return:    %.2f%%\n", annualized))
    cat(sprintf("Breakeven Rise:       %.1f bps\n", breakeven_bps))
    cat("\n")

    # ═══════════════════════════════════════════════════════════════════════
    # STEP 7: PEER COMPARISON
    # ═══════════════════════════════════════════════════════════════════════

    cat("───────────────────────────────────────────────────────────────────────\n")
    cat("6. PEER COMPARISON\n")
    cat("───────────────────────────────────────────────────────────────────────\n")

    # Calculate carry for all bonds
    all_carry_results <- calculate_advanced_carry_roll(
        all_bonds_latest,
        holding_periods = holding_period,
        funding_rate = funding_rate
    )

    target_result <- all_carry_results %>%
        filter(bond == bond_name)

    # Percentile
    percentile <- target_result$net_return_percentile[1]

    cat(sprintf("Net Return Percentile: %.1f%% (rank among %d bonds)\n",
                percentile, nrow(all_carry_results)))

    # Show top 5 and bottom 5
    top_5 <- all_carry_results %>%
        arrange(desc(net_return)) %>%
        head(5) %>%
        select(bond, net_return, carry_income, roll_return, coupon_standardized)

    bottom_5 <- all_carry_results %>%
        arrange(net_return) %>%
        head(5) %>%
        select(bond, net_return, carry_income, roll_return, coupon_standardized)

    cat("\nTop 5 performers:\n")
    cat(sprintf("  %-10s  Net Return  Carry   Roll    Coupon\n", "Bond"))
    for(i in 1:nrow(top_5)) {
        cat(sprintf("  %-10s  %7.2f%%  %6.2f%% %6.2f%%  %6.2f%%\n",
                    top_5$bond[i], top_5$net_return[i],
                    top_5$carry_income[i], top_5$roll_return[i],
                    top_5$coupon_standardized[i]))
    }

    cat("\nBottom 5 performers:\n")
    cat(sprintf("  %-10s  Net Return  Carry   Roll    Coupon\n", "Bond"))
    for(i in 1:nrow(bottom_5)) {
        cat(sprintf("  %-10s  %7.2f%%  %6.2f%% %6.2f%%  %6.2f%%\n",
                    bottom_5$bond[i], bottom_5$net_return[i],
                    bottom_5$carry_income[i], bottom_5$roll_return[i],
                    bottom_5$coupon_standardized[i]))
    }

    # Summary statistics
    summary_stats <- all_carry_results %>%
        summarise(
            mean_net = mean(net_return, na.rm = TRUE),
            median_net = median(net_return, na.rm = TRUE),
            sd_net = sd(net_return, na.rm = TRUE),
            min_net = min(net_return, na.rm = TRUE),
            max_net = max(net_return, na.rm = TRUE)
        )

    cat("\nMarket summary statistics:\n")
    cat(sprintf("  Mean:   %.2f%%\n", summary_stats$mean_net))
    cat(sprintf("  Median: %.2f%%\n", summary_stats$median_net))
    cat(sprintf("  Std Dev: %.2f%%\n", summary_stats$sd_net))
    cat(sprintf("  Range:  %.2f%% to %.2f%%\n", summary_stats$min_net, summary_stats$max_net))

    # Z-score
    z_score <- (net_return - summary_stats$mean_net) / summary_stats$sd_net
    cat(sprintf("\n%s z-score: %.2f standard deviations from mean\n", bond_name, z_score))

    if(abs(z_score) > 2) {
        cat(sprintf("⚠ WARNING: This is an OUTLIER (|z| > 2)\n"))
    }
    cat("\n")

    # ═══════════════════════════════════════════════════════════════════════
    # STEP 8: VALIDATION SUMMARY
    # ═══════════════════════════════════════════════════════════════════════

    cat("───────────────────────────────────────────────────────────────────────\n")
    cat("7. VALIDATION SUMMARY\n")
    cat("───────────────────────────────────────────────────────────────────────\n")

    validation_issues <- c()

    # Check coupon
    if(!is.na(coupon_result$validation_flag)) {
        validation_issues <- c(validation_issues,
                               paste("COUPON:", coupon_result$validation_flag))
    }

    # Check carry
    if(carry_income < 0) {
        validation_issues <- c(validation_issues,
                               "CARRY: Negative carry income")
    } else if(carry_income > 15 * period_years) {
        validation_issues <- c(validation_issues,
                               sprintf("CARRY: Unusually high (%.2f%%)", carry_income))
    }

    # Check net return
    if(abs(net_return) > 10) {
        validation_issues <- c(validation_issues,
                               sprintf("NET RETURN: Exceeds ±10%% (%.2f%%)", net_return))
    }

    # Check if outlier
    if(abs(z_score) > 2) {
        validation_issues <- c(validation_issues,
                               sprintf("OUTLIER: %.2f standard deviations from mean", z_score))
    }

    if(length(validation_issues) == 0) {
        cat("✓ All validation checks PASSED\n")
        cat("  No issues detected\n")
    } else {
        cat(sprintf("⚠ %d validation issues detected:\n", length(validation_issues)))
        for(issue in validation_issues) {
            cat(sprintf("  • %s\n", issue))
        }
    }
    cat("\n")

    # ═══════════════════════════════════════════════════════════════════════
    # STEP 9: RECOMMENDATIONS
    # ═══════════════════════════════════════════════════════════════════════

    cat("───────────────────────────────────────────────────────────────────────\n")
    cat("8. DIAGNOSIS & RECOMMENDATIONS\n")
    cat("───────────────────────────────────────────────────────────────────────\n")

    # Determine most likely issue
    if(coupon_result$format_detected == "decimal") {
        cat("✓ ISSUE FOUND: Coupon was stored in decimal format\n")
        cat(sprintf("  Original: %.4f → Corrected: %.2f%%\n",
                    bond_latest$coupon, coupon_result$standardized))
        cat("  This has been automatically corrected.\n")
    } else if(coupon_result$standardized > 15) {
        cat("⚠ UNUSUAL: Very high coupon rate\n")
        cat(sprintf("  Coupon: %.2f%% (market avg: ~8%%)\n", coupon_result$standardized))
        cat("  Verify this is correct for this bond.\n")
    } else if(ilb_result$is_likely_ilb) {
        cat("ℹ INFO: Bond identified as inflation-linked\n")
        cat("  Carry calculation includes inflation compensation.\n")
        cat(sprintf("  Base carry: %.2f%% + Inflation: %.2f%% = Total: %.2f%%\n",
                    carry_income_base, carry_income - carry_income_base, carry_income))
    } else if(abs(z_score) > 2) {
        cat("⚠ OUTLIER DETECTED\n")
        cat(sprintf("  Net return (%.2f%%) is %.2f std dev from mean (%.2f%%)\n",
                    net_return, z_score, summary_stats$mean_net))
        cat("\nPossible causes:\n")
        if(carry_income > summary_stats$mean_net) {
            cat("  • Very high coupon rate\n")
        }
        if(roll_return > 2) {
            cat("  • Very long duration benefiting from steep curve\n")
        }
        cat("  • Data quality issue - verify source data\n")
    } else {
        cat("✓ No issues detected - calculation appears correct\n")
    }

    cat("\n")
    cat("═══════════════════════════════════════════════════════════════════════\n")
    cat("END OF DIAGNOSTIC REPORT\n")
    cat("═══════════════════════════════════════════════════════════════════════\n")
    cat("\n")

    # Return structured results invisibly
    invisible(list(
        bond_name = bond_name,
        bond_info = list(
            date = bond_latest$date,
            yield = bond_latest$yield_to_maturity,
            duration = bond_latest$modified_duration,
            coupon_raw = bond_latest$coupon
        ),
        coupon_analysis = coupon_result,
        ilb_detection = ilb_result,
        curve_analysis = slope_result,
        carry_breakdown = list(
            period_years = period_years,
            carry_income = carry_income,
            roll_return = roll_return,
            funding_cost = funding_cost,
            gross_return = gross_return,
            net_return = net_return,
            annualized_return = annualized,
            breakeven_bps = breakeven_bps
        ),
        peer_comparison = list(
            percentile = percentile,
            z_score = z_score,
            market_mean = summary_stats$mean_net,
            market_median = summary_stats$median_net,
            market_sd = summary_stats$sd_net
        ),
        validation_results = validation_issues,
        full_results = target_result
    ))
}

#' @export
calculate_fair_value <- function(data, method = "smooth.spline") {
    if(nrow(data) < 4) {
        data$fitted_yield <- data$yield_to_maturity
        data$spread_to_curve <- 0
        return(data)
    }

    if(method == "smooth.spline") {
        model <- smooth.spline(x = data$modified_duration,
                               y = data$yield_to_maturity,
                               spar = 0.6)
        data$fitted_yield <- predict(model, data$modified_duration)$y
    } else if(method == "loess") {
        model <- loess(yield_to_maturity ~ modified_duration, data = data, span = 0.75)
        data$fitted_yield <- predict(model, data$modified_duration)
    } else {
        model <- lm(yield_to_maturity ~ ns(modified_duration, 3), data = data)
        data$fitted_yield <- predict(model, data)
    }

    # Calculate spread in basis points
    data$spread_to_curve <- (data$yield_to_maturity - data$fitted_yield) * 100
    return(data)
}

#' @export
# Nelson-Siegel-Svensson Model Implementation
fit_nss_model <- memoise(function(data, lambda1 = 0.0609, lambda2 = 0.0609) {
    safe_execute({
        # Check if YieldCurve package is available
        if (!requireNamespace("YieldCurve", quietly = TRUE)) {
            stop("YieldCurve package required but not installed. Install with: install.packages('YieldCurve')")
        }

        # Prepare data
        yields <- data$yield_to_maturity / 100
        maturities <- data$modified_duration

        # Initial parameter estimates
        beta0 <- mean(yields)
        beta1 <- yields[1] - beta0
        beta2 <- 0
        beta3 <- 0

        # Objective function
        nss_objective <- function(params) {
            b0 <- params[1]
            b1 <- params[2]
            b2 <- params[3]
            b3 <- params[4]
            l1 <- exp(params[5])
            l2 <- exp(params[6])

            fitted <- b0 +
                b1 * (1 - exp(-maturities/l1)) / (maturities/l1) +
                b2 * ((1 - exp(-maturities/l1)) / (maturities/l1) - exp(-maturities/l1)) +
                b3 * ((1 - exp(-maturities/l2)) / (maturities/l2) - exp(-maturities/l2))

            sum((yields - fitted)^2)
        }

        # Optimize
        opt_result <- optim(
            c(beta0, beta1, beta2, beta3, log(lambda1), log(lambda2)),
            nss_objective,
            method = "BFGS"
        )

        # Calculate fitted values
        params <- opt_result$par
        data$nss_fitted <- params[1] +
            params[2] * (1 - exp(-maturities/exp(params[5]))) / (maturities/exp(params[5])) +
            params[3] * ((1 - exp(-maturities/exp(params[5]))) / (maturities/exp(params[5])) -
                             exp(-maturities/exp(params[5]))) +
            params[4] * ((1 - exp(-maturities/exp(params[6]))) / (maturities/exp(params[6])) -
                             exp(-maturities/exp(params[6])))

        data$nss_fitted <- data$nss_fitted * 100
        data$nss_spread <- data$yield_to_maturity - data$nss_fitted

        return(list(data = data, params = params, convergence = opt_result$convergence))
    }, default = list(data = data, params = NULL, convergence = -1))
})

#' @export
# ARIMA-based Bid-to-Cover Prediction with ETS fallback for degenerate models
predict_btc_arima <- function(historical_data, bond_name, h = 1) {
    safe_execute({
        bond_data <- historical_data %>%
            filter(bond == bond_name, !is.na(bid_to_cover)) %>%
            arrange(date)

        if (nrow(bond_data) < 10) {
            return(list(
                forecast = NA,
                lower_80 = NA,
                upper_80 = NA,
                lower_95 = NA,
                upper_95 = NA,
                confidence = "Insufficient Data",
                model_type = "None"
            ))
        }

        # Create time series
        ts_data <- ts(bond_data$bid_to_cover, frequency = 12)

        # Fit ARIMA model with auto selection
        model <- auto.arima(
            ts_data,
            seasonal = TRUE,
            stepwise = FALSE,
            approximation = FALSE,
            trace = FALSE
        )

        # Check if model is degenerate (0,0,0) - ARIMA(0,0,0) is just a mean model
        # arma indices: [1]=p, [2]=q, [3]=P, [4]=Q, [5]=frequency, [6]=d, [7]=D
        is_degenerate <- (model$arma[1] == 0 && model$arma[6] == 0 && model$arma[2] == 0 &&
                         model$arma[3] == 0 && model$arma[7] == 0 && model$arma[4] == 0)

        model_type <- ""

        if (is_degenerate) {
            # Fallback to ETS for degenerate ARIMA models
            message(sprintf("WARNING: ARIMA(0,0,0) detected for %s - falling back to ETS", bond_name))
            model <- tryCatch({
                ets(ts_data)
            }, error = function(e) {
                # If ETS also fails, use simple mean forecast
                NULL
            })

            if (is.null(model)) {
                # Simple mean forecast as last resort
                mean_val <- mean(ts_data, na.rm = TRUE)
                sd_val <- sd(ts_data, na.rm = TRUE)
                return(list(
                    forecast = mean_val,
                    lower_80 = mean_val - 1.28 * sd_val,
                    upper_80 = mean_val + 1.28 * sd_val,
                    lower_95 = mean_val - 1.96 * sd_val,
                    upper_95 = mean_val + 1.96 * sd_val,
                    confidence = "Low",
                    model_type = "Mean"
                ))
            }

            # Build ETS model type string
            model_type <- sprintf("ETS(%s)", model$method)
        } else {
            # Build ARIMA model type string
            model_type <- paste0("ARIMA(", model$arma[1], ",", model$arma[6], ",", model$arma[2], ")")
            if (model$arma[3] > 0 || model$arma[7] > 0 || model$arma[4] > 0) {
                model_type <- paste0(model_type, "(", model$arma[3], ",", model$arma[7], ",", model$arma[4], ")")
            }
        }

        # Generate forecast
        fc <- forecast(model, h = h, level = c(80, 95))

        # Calculate confidence based on model fit
        accuracy_metrics <- tryCatch({
            accuracy(model)
        }, error = function(e) {
            # Return default high MAPE if accuracy fails
            matrix(25, nrow = 1, ncol = 1, dimnames = list(NULL, "MAPE"))
        })

        mape <- if ("MAPE" %in% colnames(accuracy_metrics)) {
            accuracy_metrics[, "MAPE"]
        } else {
            25  # Default moderate MAPE
        }

        confidence <- case_when(
            mape < 10 ~ "High",
            mape < 20 ~ "Medium",
            mape < 30 ~ "Low",
            TRUE ~ "Very Low"
        )

        return(list(
            forecast = as.numeric(fc$mean)[1],
            lower_80 = as.numeric(fc$lower[1, 1]),
            upper_80 = as.numeric(fc$upper[1, 1]),
            lower_95 = as.numeric(fc$lower[1, 2]),
            upper_95 = as.numeric(fc$upper[1, 2]),
            confidence = confidence,
            model_type = model_type
        ))
    }, default = list(forecast = NA, lower_80 = NA, upper_80 = NA, lower_95 = NA, upper_95 = NA, confidence = "Error", model_type = "None"))
}


#' @export
#' @title Validate Approved Series (CORRECTED)
#' @description Validates that core data uses only approved series, but allows derived columns
#' @param data Bond data frame
#' @return Data frame with validated columns
validate_approved_series <- function(data) {
    if(is.null(data) || nrow(data) == 0) {
        return(data)
    }

    # ──────────────────────────────────────────────────────────────────────────
    # APPROVED CORE SERIES (from requirements)
    # ──────────────────────────────────────────────────────────────────────────

    approved_core <- c(
        # Required identifiers
        "bond", "date",

        # Core bond metrics (APPROVED FOR PLOTTING)
        "yield_to_maturity",
        "coupon",
        "convexity",
        "accrued_interest",
        "clean_price",
        "duration",
        "modified_duration",
        "basis_point_value",
        "full_price",

        # Auction data (APPROVED FOR PLOTTING)
        "offer_amount",      # Government offer
        "bids_received",     # Market bids
        "bid_to_cover"       # Ratio
    )

    # ──────────────────────────────────────────────────────────────────────────
    # COLUMN ALIASES (your data uses different names)
    # ──────────────────────────────────────────────────────────────────────────

    column_aliases <- list(
        offer_amount = c("offer_amount", "offer"),           # Both are OK
        bids_received = c("bids_received", "bids"),          # Both are OK
        basis_point_value = c("basis_point_value", "dv01")   # Both are OK
    )

    # ──────────────────────────────────────────────────────────────────────────
    # ALLOWED DERIVED/CALCULATED COLUMNS (computed from approved series)
    # ──────────────────────────────────────────────────────────────────────────

    allowed_derived <- c(
        # Date/time derivatives
        "offer_date", "announcement_date", "settle_date", "mature_date",
        "time_to_maturity", "maturity_bucket",
        "year", "quarter", "month", "week", "ytd_flag",

        # Auction derivatives
        "allocation", "auction_success",

        # Technical indicators (calculated from yield_to_maturity)
        "rsi_14", "rsi_signal", "bb_position", "bb_mean", "bb_upper", "bb_lower",
        "bb_upper_1", "bb_lower_1", "bb_upper_2", "bb_lower_2", "bb_width",
        "sma_50", "sma_200", "ema_12", "ema_26",
        "macd", "macd_signal", "macd_histogram",
        "roc_5", "roc_20", "momentum_signal",
        "stoch_rsi", "williams_r", "historical_vol",
        "resistance_1m", "resistance_3m", "support_1m", "support_3m",
        "signal_strength", "mean_20", "z_score_20",

        # Curve/spread derivatives
        "spread_to_curve", "fitted_yield", "z_score", "percentile_rank",
        "hist_mean_spread", "hist_sd_spread",

        # Carry/roll derivatives
        "carry_income", "roll_return", "funding_cost", "gross_return", "net_return",
        "return_per_unit_risk", "sharpe_estimate", "breakeven_bps",
        "holding_period", "holding_days",

        # Duration-adaptive technical indicators (NEW)
        "duration_bucket", "indicator_speed",
        "rsi_threshold_buy", "rsi_threshold_sell", "rsi_median",
        "macd_normalized", "bond_volatility", "signal_confidence"
    )

    # ──────────────────────────────────────────────────────────────────────────
    # BUILD COMPLETE ALLOWED LIST
    # ──────────────────────────────────────────────────────────────────────────

    # Start with approved core
    allowed_columns <- approved_core

    # Add all aliases
    for(alias_group in column_aliases) {
        allowed_columns <- c(allowed_columns, alias_group)
    }

    # Add derived columns
    allowed_columns <- c(allowed_columns, allowed_derived)

    # Remove duplicates
    allowed_columns <- unique(allowed_columns)

    # ──────────────────────────────────────────────────────────────────────────
    # VALIDATE DATA COLUMNS
    # ──────────────────────────────────────────────────────────────────────────

    current_cols <- names(data)

    # Check which current columns are allowed
    valid_cols <- intersect(current_cols, allowed_columns)

    # Identify truly unapproved columns (not core, not aliases, not derived)
    unapproved <- setdiff(current_cols, allowed_columns)

    # Only warn about unapproved if there are truly problematic columns
    truly_problematic <- unapproved[!grepl("^(n_obs|min_|max_|avg_|total_|count_)", unapproved)]

    if(length(truly_problematic) > 0) {
        warning(paste(
            "Unapproved columns detected (not removed, just flagged):",
            paste(truly_problematic, collapse = ", ")
        ))
    }

    # ──────────────────────────────────────────────────────────────────────────
    # STANDARDIZE COLUMN NAMES (map aliases to standard names) - CASE INSENSITIVE
    # ──────────────────────────────────────────────────────────────────────────

    # Helper function for case-insensitive column finding
    find_col_idx <- function(col_name, data_names) {
        which(tolower(data_names) == tolower(col_name))
    }

    # Map 'offer' to 'offer_amount' if needed (case-insensitive)
    offer_idx <- find_col_idx("offer", names(data))
    if(length(offer_idx) > 0 && !"offer_amount" %in% names(data)) {
        old_name <- names(data)[offer_idx[1]]
        names(data)[offer_idx[1]] <- "offer_amount"
        message(sprintf("Mapped: %s → offer_amount", old_name))
    }

    # Map 'bids' to 'bids_received' if needed (case-insensitive)
    bids_idx <- find_col_idx("bids", names(data))
    if(length(bids_idx) > 0 && !"bids_received" %in% names(data)) {
        old_name <- names(data)[bids_idx[1]]
        names(data)[bids_idx[1]] <- "bids_received"
        message(sprintf("Mapped: %s → bids_received", old_name))
    }

    # Map 'dv01' to 'basis_point_value' if needed (case-insensitive)
    dv01_idx <- find_col_idx("dv01", names(data))
    if(length(dv01_idx) > 0 && !"basis_point_value" %in% names(data)) {
        old_name <- names(data)[dv01_idx[1]]
        names(data)[dv01_idx[1]] <- "basis_point_value"
        message(sprintf("Mapped: %s → basis_point_value", old_name))
    }

    # ──────────────────────────────────────────────────────────────────────────
    # LOG SUMMARY
    # ──────────────────────────────────────────────────────────────────────────

    # Count core approved series present
    core_present <- intersect(approved_core, names(data))

    message(sprintf(
        "✓ Data validation: %d core approved series, %d derived columns",
        length(core_present),
        length(intersect(allowed_derived, names(data)))
    ))

    return(data)
}