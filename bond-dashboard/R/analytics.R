# Enhanced Analytics Module for SA Government Bonds
# Purpose: Provide actionable insights for weekly auction decisions

library(tidyverse)
library(zoo)
library(mgcv)

# ================== RELATIVE VALUE ANALYTICS ==================

#' Calculate Rolling Z-Scores with Multiple Windows
#' @param df Bond dataframe with yield and duration data
#' @param windows Vector of lookback periods (default: 20, 60, 252 days)
#' @return Dataframe with z-scores for each window
calculate_rolling_zscores <- function(df, windows = c(20, 60, 252)) {
    # Ensure data is sorted by date
    df <- df %>% arrange(date)

    # Initialize results with the original dataframe
    results <- df

    for(window in windows) {
        # For each bond, calculate rolling statistics
        temp <- results %>%  # Use results instead of df
            group_by(bond) %>%
            arrange(date) %>%
            mutate(
                # Rolling mean and sd of yield
                roll_mean = zoo::rollmean(yield_to_maturity, window, fill = NA, align = "right"),
                roll_sd = zoo::rollapply(yield_to_maturity, window, sd, fill = NA, align = "right"),
                # Z-score calculation
                !!paste0("zscore_", window, "d") := (yield_to_maturity - roll_mean) / roll_sd,
                # Mean reversion speed (days to revert to mean historically)
                !!paste0("reversion_speed_", window, "d") := zoo::rollapply(
                    yield_to_maturity,
                    window,
                    function(x) {
                        if(length(x) < 2) return(NA)
                        acf_result <- acf(x, lag.max = 1, plot = FALSE)$acf[2]
                        return(-1 / log(abs(acf_result)))
                    },
                    fill = NA,
                    align = "right"
                )
            ) %>%
            ungroup()

        results <- temp
    }

    # Add conviction score (weighted average of z-scores)
    results <- results %>%
        mutate(
            conviction_score = (
                coalesce(zscore_20d, 0) * 0.5 +
                    coalesce(zscore_60d, 0) * 0.3 +
                    coalesce(zscore_252d, 0) * 0.2
            ),
            conviction_strength = case_when(
                abs(conviction_score) > 2 ~ "High",
                abs(conviction_score) > 1 ~ "Medium",
                TRUE ~ "Low"
            )
        )

    return(results)
}

#' Calculate Curve Decomposition (Level, Slope, Curvature)
#' @param df Bond dataframe
#' @return List with curve components
decompose_curve <- function(df) {
    # Get latest data point for each bond
    latest <- df %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        arrange(modified_duration)

    if(nrow(latest) < 3) {
        return(list(level = NA, slope = NA, curvature = NA))
    }

    # Define maturity buckets
    short_term <- latest %>% filter(modified_duration <= 5)
    medium_term <- latest %>% filter(modified_duration > 5 & modified_duration <= 10)
    long_term <- latest %>% filter(modified_duration > 10)

    # Calculate components
    level <- mean(latest$yield_to_maturity, na.rm = TRUE)

    # Slope: long minus short
    slope <- ifelse(
        nrow(long_term) > 0 & nrow(short_term) > 0,
        mean(long_term$yield_to_maturity, na.rm = TRUE) - mean(short_term$yield_to_maturity, na.rm = TRUE),
        NA
    )

    # Curvature: 2*medium - short - long
    curvature <- ifelse(
        nrow(medium_term) > 0 & nrow(short_term) > 0 & nrow(long_term) > 0,
        2 * mean(medium_term$yield_to_maturity, na.rm = TRUE) -
            mean(short_term$yield_to_maturity, na.rm = TRUE) -
            mean(long_term$yield_to_maturity, na.rm = TRUE),
        NA
    )

    # Historical context (percentile ranks)
    hist_data <- df %>%
        group_by(date) %>%
        summarise(
            hist_level = mean(yield_to_maturity, na.rm = TRUE),
            .groups = "drop"
        )

    level_percentile <- ecdf(hist_data$hist_level)(level) * 100

    return(list(
        level = level,
        slope = slope,
        curvature = curvature,
        level_percentile = level_percentile,
        slope_bps = slope * 100,
        curve_shape = case_when(
            slope > 100 ~ "Steep",
            slope > 50 ~ "Normal",
            slope > 0 ~ "Flat",
            TRUE ~ "Inverted"
        )
    ))
}

#' Calculate Butterfly Trade Opportunities
#' @param df Bond dataframe
#' @param min_zscore Minimum z-score for trade signal
#' @return Dataframe of butterfly trades
identify_butterfly_trades <- function(df, min_zscore = 1.5) {
    # Get latest data
    latest <- df %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        arrange(modified_duration) %>%
        filter(!is.na(yield_to_maturity))

    butterflies <- data.frame()

    # Generate all possible butterfly combinations
    if(nrow(latest) >= 3) {
        for(i in 2:(nrow(latest)-1)) {
            wing1 <- latest[i-1,]
            body <- latest[i,]
            wing2 <- latest[i+1,]

            # Calculate expected body yield (linear interpolation)
            weight <- (body$modified_duration - wing1$modified_duration) /
                (wing2$modified_duration - wing1$modified_duration)
            expected_yield <- wing1$yield_to_maturity * (1 - weight) + wing2$yield_to_maturity * weight

            # Richness/cheapness in basis points
            richness_bps <- (body$yield_to_maturity - expected_yield) * 100

            # Historical comparison
            hist_richness <- df %>%
                filter(bond %in% c(wing1$bond, body$bond, wing2$bond)) %>%
                group_by(date) %>%
                summarise(
                    w1_yield = mean(yield_to_maturity[bond == wing1$bond], na.rm = TRUE),
                    b_yield = mean(yield_to_maturity[bond == body$bond], na.rm = TRUE),
                    w2_yield = mean(yield_to_maturity[bond == wing2$bond], na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                mutate(
                    hist_richness = (b_yield - (w1_yield * (1-weight) + w2_yield * weight)) * 100
                )

            z_score <- (richness_bps - mean(hist_richness$hist_richness, na.rm = TRUE)) /
                sd(hist_richness$hist_richness, na.rm = TRUE)

            butterflies <- bind_rows(butterflies, data.frame(
                wing1 = wing1$bond,
                body = body$bond,
                wing2 = wing2$bond,
                richness_bps = richness_bps,
                z_score = z_score,
                percentile = ecdf(hist_richness$hist_richness)(richness_bps) * 100,
                signal = case_when(
                    z_score > min_zscore ~ "CHEAP (Buy Body)",
                    z_score < -min_zscore ~ "RICH (Sell Body)",
                    TRUE ~ "NEUTRAL"
                ),
                duration_weighted_ratio = paste0(
                    round(body$modified_duration / wing1$modified_duration, 2), ":",
                    "1:",
                    round(body$modified_duration / wing2$modified_duration, 2)
                )
            ))
        }
    }

    return(butterflies %>% arrange(desc(abs(z_score))))
}

#' Calculate Roll-Down Analysis
#' @param df Bond dataframe
#' @param horizons Vector of horizons in months
#' @return Dataframe with roll-down returns
calculate_rolldown <- function(df, horizons = c(3, 6, 12)) {
    latest <- df %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup()

    # Fit the curve
    curve_fit <- smooth.spline(latest$modified_duration, latest$yield_to_maturity, spar = 0.7)

    results <- data.frame()

    for(bond_row in 1:nrow(latest)) {
        bond_data <- latest[bond_row,]

        for(horizon_months in horizons) {
            # Adjust duration for time passage
            new_duration <- bond_data$modified_duration - (horizon_months / 12)

            if(new_duration > 0) {
                # Predict yield at new duration
                new_yield <- predict(curve_fit, new_duration)$y

                # Calculate return components
                carry_return <- bond_data$coupon * (horizon_months / 12)
                rolldown_return <- -(new_yield - bond_data$yield_to_maturity) * new_duration * 100
                total_return <- carry_return + rolldown_return

                # Calculate breakeven (how much yields can rise before negative return)
                breakeven_bps <- total_return / new_duration

                results <- bind_rows(results, data.frame(
                    bond = bond_data$bond,
                    horizon_months = horizon_months,
                    carry_return = carry_return,
                    rolldown_return = rolldown_return,
                    total_return = total_return,
                    breakeven_bps = breakeven_bps,
                    annualized_return = total_return * (12 / horizon_months)
                ))
            }
        }
    }

    return(results)
}

# ================== AUCTION ANALYTICS ==================

#' Calculate Auction Pressure Score
#' @param df Bond dataframe with auction data
#' @return Dataframe with pressure scores
calculate_auction_pressure <- function(df) {
    # Calculate metrics for each bond
    auction_metrics <- df %>%
        filter(!is.na(bid_to_cover)) %>%
        group_by(bond) %>%
        summarise(
            last_auction_date = max(date[!is.na(bid_to_cover)]),
            days_since_auction = as.numeric(Sys.Date() - last_auction_date),
            avg_days_between = mean(diff(sort(unique(date[!is.na(bid_to_cover)]))), na.rm = TRUE),
            auction_frequency = n(),
            recent_btc_trend = {
                recent <- tail(bid_to_cover[!is.na(bid_to_cover)], 5)
                if(length(recent) >= 2) {
                    coef(lm(recent ~ seq_along(recent)))[2]
                } else NA
            },
            btc_volatility = sd(bid_to_cover, na.rm = TRUE),
            .groups = "drop"
        )

    # Calculate pressure score
    auction_metrics <- auction_metrics %>%
        mutate(
            # Squeeze risk: high if long time since auction relative to normal frequency
            squeeze_risk = ifelse(
                !is.na(avg_days_between) & avg_days_between > 0,
                pmin(days_since_auction / avg_days_between, 3),
                ifelse(days_since_auction > 90, 3, 1)  # Default logic if no frequency data
            ),

            # Demand momentum: positive if btc trending up
            demand_momentum = case_when(
                recent_btc_trend > 0.1 ~ 1,
                recent_btc_trend < -0.1 ~ -1,
                TRUE ~ 0
            ),

            # Composite pressure score
            pressure_score = squeeze_risk * 0.4 +
                (1 - btc_volatility/max(btc_volatility, na.rm = TRUE)) * 0.3 +
                (demand_momentum + 1) * 0.3,

            pressure_signal = case_when(
                pressure_score > 2 ~ "High Demand Expected",
                pressure_score > 1 ~ "Normal Demand",
                TRUE ~ "Low Demand Risk"
            )
        )

    return(auction_metrics)
}

#' Identify Seasonal Patterns in Auctions
#' @param df Bond dataframe
#' @return Dataframe with seasonal factors
calculate_seasonality <- function(df) {
    auction_data <- df %>%
        filter(!is.na(bid_to_cover)) %>%
        mutate(
            month = lubridate::month(date, label = TRUE),
            quarter = lubridate::quarter(date),
            year = lubridate::year(date)
        )

    # Monthly seasonality
    monthly_seasonal <- auction_data %>%
        group_by(month, bond) %>%
        summarise(
            avg_btc = mean(bid_to_cover, na.rm = TRUE),
            avg_allocation = mean(allocation, na.rm = TRUE),
            n_auctions = n(),
            .groups = "drop"
        )

    # Calculate seasonal factors
    overall_avg <- mean(auction_data$bid_to_cover, na.rm = TRUE)

    seasonal_factors <- monthly_seasonal %>%
        group_by(month) %>%
        summarise(
            seasonal_factor = mean(avg_btc) / overall_avg,
            typical_demand = case_when(
                seasonal_factor > 1.1 ~ "Above Average",
                seasonal_factor < 0.9 ~ "Below Average",
                TRUE ~ "Average"
            ),
            .groups = "drop"
        )

    return(seasonal_factors)
}

# ================== RISK METRICS ==================

#' Calculate Comprehensive Risk Metrics
#' @param df Bond dataframe
#' @param vol_window Window for volatility calculation
#' @return Dataframe with risk metrics
calculate_risk_metrics <- function(df, vol_window = 20) {
    risk_data <- df %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            # Daily yield changes
            yield_change = yield_to_maturity - lag(yield_to_maturity),

            # Volatility metrics
            yield_vol = zoo::rollapply(yield_change, vol_window, sd, fill = NA, align = "right") * sqrt(252),

            # DV01 approximation (price change for 1bp move)
            dv01_pct = modified_duration / 100,

            # Risk units (ModDur × Volatility)
            risk_units = modified_duration * yield_vol,

            # Sharpe-like ratio (yield premium over short rate / volatility)
            sharpe_ratio = {
                short_bonds <- yield_to_maturity[modified_duration <= 2]
                if(length(short_bonds) > 0 && !all(is.na(short_bonds))) {
                    short_rate <- min(short_bonds, na.rm = TRUE)
                } else {
                    short_rate <- min(yield_to_maturity, na.rm = TRUE)
                }
                ifelse(yield_vol > 0, (yield_to_maturity - short_rate) / yield_vol, NA_real_)
            },

            # Maximum drawdown over rolling window
            roll_max = zoo::rollapply(yield_to_maturity, vol_window, max, fill = NA, align = "right"),
            drawdown = (yield_to_maturity - roll_max) * modified_duration,

            # 95% VaR (1.65 std devs for 1-day, scale for different periods)
            var_1d = 1.65 * yield_vol * modified_duration / sqrt(252),
            var_5d = var_1d * sqrt(5),
            var_20d = var_1d * sqrt(20)
        ) %>%
        ungroup()

    # Latest risk metrics summary
    latest_risk <- risk_data %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        select(bond, yield_vol, risk_units, sharpe_ratio, var_1d, var_5d, var_20d) %>%
        ungroup()

    return(latest_risk)
}

#' Calculate Composite Liquidity Score
#' @param df Bond dataframe
#' @return Dataframe with liquidity scores
calculate_liquidity_score <- function(df) {
        liquidity_data <- df %>%
            group_by(bond) %>%
            summarise(
                # Component calculations...
                avg_btc = mean(bid_to_cover, na.rm = TRUE),
                btc_stability = 1 / (sd(bid_to_cover, na.rm = TRUE) + 0.1),
                issue_frequency = sum(!is.na(bid_to_cover)),
                avg_bid_size = mean(bids / 1e9, na.rm = TRUE),
                # Fix days_since calculation
                days_since = {
                    auction_dates <- date[!is.na(bid_to_cover)]
                    if(length(auction_dates) > 0) {
                        as.numeric(Sys.Date() - max(auction_dates))
                    } else {
                        999  # Default value for bonds with no auctions
                    }
                },
                .groups = "drop"
            )

    # Normalize components to 0-1 scale
    liquidity_data <- liquidity_data %>%
        mutate(
            norm_btc = avg_btc / max(avg_btc, na.rm = TRUE),
            norm_stability = btc_stability / max(btc_stability, na.rm = TRUE),
            norm_frequency = issue_frequency / max(issue_frequency, na.rm = TRUE),
            norm_bid_size = avg_bid_size / max(avg_bid_size, na.rm = TRUE),

            # Composite score
            liquidity_score = norm_btc * 0.4 + norm_stability * 0.2 +
                norm_frequency * 0.2 + norm_bid_size * 0.2,

            liquidity_rank = rank(-liquidity_score),

            liquidity_category = case_when(
                liquidity_score > 0.7 ~ "Highly Liquid",
                liquidity_score > 0.4 ~ "Moderately Liquid",
                TRUE ~ "Less Liquid"
            )
        )

    return(liquidity_data)
}

# ================== TRADING SIGNALS ==================

#' Generate Multi-Factor Trading Signals
#' @param df Bond dataframe
#' @param z_score_data Z-score calculations
#' @param liquidity_data Liquidity scores
#' @param risk_data Risk metrics
#' @return Dataframe with composite trading signals
generate_trading_signals <- function(df, z_score_data = NULL, liquidity_data = NULL, risk_data = NULL) {
    # Get latest data
    latest <- df %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup()

    # Calculate relative value if not provided
    if(is.null(z_score_data)) {
        z_score_data <- calculate_rolling_zscores(df)
    }

    # Calculate liquidity if not provided
    if(is.null(liquidity_data)) {
        liquidity_data <- calculate_liquidity_score(df)
    }

    # Calculate risk metrics if not provided
    if(is.null(risk_data)) {
        risk_data <- calculate_risk_metrics(df)
    }

    # Calculate momentum
    momentum_data <- df %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            yield_change_5d = yield_to_maturity - lag(yield_to_maturity, 5),
            yield_change_20d = yield_to_maturity - lag(yield_to_maturity, 20),
            momentum_score = case_when(
                yield_change_5d < -0.1 & yield_change_20d < -0.2 ~ 1,
                yield_change_5d > 0.1 & yield_change_20d > 0.2 ~ -1,
                TRUE ~ 0
            )
        ) %>%
        filter(date == max(date)) %>%
        select(bond, momentum_score)

    # Curve positioning
    curve_data <- decompose_curve(df)
    curve_score <- case_when(
        curve_data$slope > 100 ~ 0.5,  # Prefer duration in steep curve
        curve_data$slope < 50 ~ -0.5,   # Avoid duration in flat curve
        TRUE ~ 0
    )

    # Combine all factors
    signals <- latest %>%
        select(bond, yield_to_maturity, modified_duration, coupon) %>%
        left_join(z_score_data %>%
                      group_by(bond) %>%
                      filter(date == max(date)) %>%
                      select(bond, conviction_score), by = "bond") %>%
        left_join(momentum_data, by = "bond") %>%
        left_join(liquidity_data %>% select(bond, liquidity_score), by = "bond") %>%
        left_join(risk_data %>% select(bond, sharpe_ratio), by = "bond")

    # Calculate composite score
    signals <- signals %>%
        mutate(
            # Normalize components
            value_score = pmin(pmax(conviction_score / 2, -1), 1),  # Cap at ±1
            momentum_adj = momentum_score * 0.2,
            curve_adj = curve_score,
            liquidity_adj = (liquidity_score - 0.5) * 0.3,
            risk_adj = pmin(pmax(sharpe_ratio / 3, -1), 1) * 0.2,

            # Composite signal
            composite_score = value_score * 0.4 +
                momentum_adj * 0.2 +
                curve_adj * 0.15 +
                liquidity_adj * 0.15 +
                risk_adj * 0.1,

            # Signal strength
            signal = case_when(
                composite_score > 0.6 ~ "STRONG BUY",
                composite_score > 0.3 ~ "BUY",
                composite_score < -0.6 ~ "STRONG SELL",
                composite_score < -0.3 ~ "SELL",
                TRUE ~ "NEUTRAL"
            ),

            # Confidence level based on data quality and agreement
            confidence = case_when(
                abs(value_score) > 0.5 & sign(momentum_adj) == sign(value_score) ~ "High",
                abs(value_score) > 0.3 | abs(momentum_adj) > 0.3 ~ "Medium",
                TRUE ~ "Low"
            ),

            # Rank by attractiveness
            rank = rank(-composite_score)
        )

    return(signals %>% arrange(rank))
}

#' Identify Pair Trade Opportunities
#' @param df Bond dataframe
#' @param min_spread_zscore Minimum z-score for pair trade
#' @return Dataframe with pair trade recommendations
identify_pair_trades <- function(df, min_spread_zscore = 1.5) {
    latest <- df %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup()

    pairs <- data.frame()

    # Generate all pairs with similar duration (within 2 years)
    for(i in 1:(nrow(latest)-1)) {
        for(j in (i+1):nrow(latest)) {
            bond1 <- latest[i,]
            bond2 <- latest[j,]

            dur_diff <- abs(bond1$modified_duration - bond2$modified_duration)

            if(dur_diff < 2) {
                # Calculate current spread
                current_spread <- bond2$yield_to_maturity - bond1$yield_to_maturity

                # Historical spread analysis
                hist_spread <- df %>%
                    filter(bond %in% c(bond1$bond, bond2$bond)) %>%
                    select(date, bond, yield_to_maturity) %>%
                    pivot_wider(names_from = bond, values_from = yield_to_maturity) %>%
                    mutate(spread = .[[bond2$bond]] - .[[bond1$bond]]) %>%
                    filter(!is.na(spread))

                if(nrow(hist_spread) > 20) {
                    mean_spread <- mean(hist_spread$spread, na.rm = TRUE)
                    sd_spread <- sd(hist_spread$spread, na.rm = TRUE)
                    z_score <- (current_spread - mean_spread) / sd_spread

                    # Duration-neutral hedge ratio
                    hedge_ratio <- bond2$modified_duration / bond1$modified_duration

                    pairs <- bind_rows(pairs, data.frame(
                        long_bond = ifelse(z_score > 0, bond2$bond, bond1$bond),
                        short_bond = ifelse(z_score > 0, bond1$bond, bond2$bond),
                        current_spread_bps = current_spread * 100,
                        mean_spread_bps = mean_spread * 100,
                        z_score = abs(z_score),
                        hedge_ratio = hedge_ratio,
                        expected_profit_bps = abs(current_spread - mean_spread) * 100,
                        signal = case_when(
                            abs(z_score) > min_spread_zscore ~ "EXECUTE",
                            abs(z_score) > min_spread_zscore * 0.7 ~ "MONITOR",
                            TRUE ~ "IGNORE"
                        )
                    ))
                }
            }
        }
    }

    return(pairs %>% arrange(desc(z_score)))
}