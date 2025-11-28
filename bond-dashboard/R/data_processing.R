# Enhanced Data Processing Module with Validation and Derived Metrics
# Purpose: Ensure data quality and calculate actionable metrics

library(tidyverse)
library(lubridate)
library(zoo)

# ================== DATA VALIDATION ==================

#' Comprehensive Data Validation and Cleaning
#' @param df Raw bond dataframe
#' @return List with cleaned data, quality report, and warnings
validate_bond_data <- function(df) {
    warnings_list <- list()
    fixes_made <- list()

    # Store original row count
    original_rows <- nrow(df)

    # 1. Check for yield inversions
    yield_check <- df %>%
        group_by(date) %>%
        arrange(modified_duration) %>%
        mutate(
            yield_diff = yield_to_maturity - lag(yield_to_maturity),
            potential_inversion = yield_diff < -0.5 & modified_duration > lag(modified_duration)
        ) %>%
        filter(potential_inversion == TRUE)

    if(nrow(yield_check) > 0) {
        warnings_list$inversions <- paste(nrow(yield_check),
                                          "potential yield inversions detected")
    }

    # 2. Validate bid-to-cover ratios
    btc_outliers <- df %>%
        filter(!is.na(bid_to_cover)) %>%
        filter(bid_to_cover < 0.5 | bid_to_cover > 10)

    if(nrow(btc_outliers) > 0) {
        warnings_list$btc_outliers <- paste(nrow(btc_outliers),
                                            "bid-to-cover ratios outside normal range (0.5-10)")
        # Fix extreme outliers
        df <- df %>%
            mutate(bid_to_cover = case_when(
                bid_to_cover < 0.5 ~ NA_real_,
                bid_to_cover > 10 ~ 10,
                TRUE ~ bid_to_cover
            ))
        fixes_made$btc <- "Extreme bid-to-cover values capped or removed"
    }

    # 3. Check for data staleness
    bonds_stale <- df %>%
        group_by(bond) %>%
        summarise(
            last_update = max(date),
            days_stale = as.numeric(Sys.Date() - last_update),
            .groups = "drop"
        ) %>%
        filter(days_stale > 5)

    if(nrow(bonds_stale) > 0) {
        warnings_list$stale_data <- paste("Bonds with stale data (>5 days):",
                                          paste(bonds_stale$bond, collapse = ", "))
    }

    # 4. Detect and handle sudden jumps
    jump_check <- df %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            yield_jump = abs(yield_to_maturity - lag(yield_to_maturity)),
            large_jump = yield_jump > 1  # More than 100bps in one day
        ) %>%
        filter(large_jump == TRUE)

    if(nrow(jump_check) > 0) {
        warnings_list$jumps <- paste(nrow(jump_check),
                                     "large yield jumps (>100bps) detected")
    }

    # 5. Check for missing critical data patterns
    missing_patterns <- df %>%
        group_by(bond) %>%
        summarise(
            pct_missing_yield = sum(is.na(yield_to_maturity)) / n() * 100,
            pct_missing_duration = sum(is.na(modified_duration)) / n() * 100,
            consecutive_missing = max(rle(is.na(yield_to_maturity))$lengths[
                rle(is.na(yield_to_maturity))$values == TRUE], default = 0),
            .groups = "drop"
        ) %>%
        filter(pct_missing_yield > 20 | consecutive_missing > 5)

    if(nrow(missing_patterns) > 0) {
        warnings_list$missing <- paste("Bonds with significant missing data:",
                                       paste(missing_patterns$bond, collapse = ", "))
    }

    # 6. Validate coupon rates
    coupon_check <- df %>%
        filter(!is.na(coupon)) %>%
        filter(coupon < 0 | coupon > 20)

    if(nrow(coupon_check) > 0) {
        warnings_list$coupon <- "Unusual coupon rates detected"
        df <- df %>%
            mutate(coupon = ifelse(coupon < 0 | coupon > 20, NA, coupon))
        fixes_made$coupon <- "Invalid coupon rates set to NA"
    }

    # 7. Check date consistency
    date_issues <- df %>%
        filter(date > Sys.Date() | date < as.Date("2020-01-01"))

    if(nrow(date_issues) > 0) {
        warnings_list$dates <- paste(nrow(date_issues), "records with invalid dates")
        df <- df %>%
            filter(date <= Sys.Date() & date >= as.Date("2020-01-01"))
        fixes_made$dates <- "Records with invalid dates removed"
    }

    # Create quality report
    quality_report <- list(
        total_records = nrow(df),
        records_removed = original_rows - nrow(df),
        completeness = (1 - sum(is.na(df)) / (nrow(df) * ncol(df))) * 100,
        bonds_count = n_distinct(df$bond),
        date_range = paste(min(df$date), "to", max(df$date)),
        warnings = warnings_list,
        fixes = fixes_made,
        summary = data.frame(
            metric = c("Data Completeness", "Records Validated", "Issues Fixed"),
            value = c(
                paste0(round((1 - sum(is.na(df)) / (nrow(df) * ncol(df))) * 100, 1), "%"),
                format(nrow(df), big.mark = ","),
                length(fixes_made)
            )
        )
    )

    return(list(
        data = df,
        report = quality_report,
        needs_attention = length(warnings_list) > 0
    ))
}

#' Smart Missing Data Handling with Curve Interpolation
#' @param df Validated bond dataframe
#' @return Dataframe with intelligently filled missing values
handle_missing_data <- function(df) {
    # For each date, fit a curve and use it to fill missing yields
    df_filled <- df %>%
        group_by(date) %>%
        mutate(
            # Only fill if we have enough data points
            n_valid = sum(!is.na(yield_to_maturity)),
            should_interpolate = n_valid >= 5
        ) %>%
        ungroup()

    # Interpolate yields using curve fitting
    dates_to_process <- unique(df_filled$date[df_filled$should_interpolate])

    for(process_date in dates_to_process) {
        date_data <- df_filled %>% filter(date == process_date)

        if(any(is.na(date_data$yield_to_maturity))) {
            # Fit curve with available data
            valid_data <- date_data %>% filter(!is.na(yield_to_maturity))

            if(nrow(valid_data) >= 3) {
                # Use spline interpolation
                curve_fit <- smooth.spline(valid_data$modified_duration,
                                           valid_data$yield_to_maturity,
                                           spar = 0.7)

                # Fill missing values
                missing_indices <- which(is.na(date_data$yield_to_maturity))
                for(idx in missing_indices) {
                    interpolated_yield <- predict(curve_fit,
                                                  date_data$modified_duration[idx])$y
                    df_filled$yield_to_maturity[df_filled$date == process_date &
                                                    df_filled$bond == date_data$bond[idx]] <- interpolated_yield

                    # Mark as interpolated
                    df_filled$data_quality[df_filled$date == process_date &
                                               df_filled$bond == date_data$bond[idx]] <- "interpolated"
                }
            }
        }
    }

    # Forward-fill auction data with decay
    df_filled <- df_filled %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            # Fill bid_to_cover with decay
            days_since_auction = cumsum(is.na(bid_to_cover)) * ifelse(is.na(bid_to_cover), 1, 0),
            btc_filled = zoo::na.locf(bid_to_cover, na.rm = FALSE),
            btc_decay_factor = exp(-days_since_auction / 30),  # 30-day half-life
            bid_to_cover_adj = btc_filled * btc_decay_factor,

            # Mark filled values
            data_quality = case_when(
                !is.na(data_quality) ~ data_quality,
                !is.na(bid_to_cover) ~ "actual",
                !is.na(bid_to_cover_adj) ~ "forward_filled",
                TRUE ~ "missing"
            )
        ) %>%
        ungroup() %>%
        select(-days_since_auction, -btc_filled, -btc_decay_factor, -n_valid, -should_interpolate)

    return(df_filled)
}

# ================== CALCULATED FIELDS ==================

#' Add Comprehensive Derived Metrics
#' @param df Clean bond dataframe
#' @return Enhanced dataframe with calculated fields
add_derived_metrics <- function(df) {

    # Define benchmark bonds
    benchmark_short <- "R2030"  # Short-term benchmark
    benchmark_long <- "R2048"   # Long-term benchmark

    # Calculate spreads to benchmark
    benchmark_yields <- df %>%
        filter(bond %in% c(benchmark_short, benchmark_long)) %>%
        select(date, bond, yield_to_maturity) %>%
        pivot_wider(names_from = bond, values_from = yield_to_maturity,
                    names_prefix = "benchmark_")

    df_enhanced <- df %>%
        left_join(benchmark_yields, by = "date") %>%
        mutate(
            # Spread to benchmark (basis points)
            spread_to_benchmark = case_when(
                modified_duration <= 7 ~ (yield_to_maturity - benchmark_R2030) * 100,
                TRUE ~ (yield_to_maturity - benchmark_R2048) * 100
            ),

            # Asset swap spread approximation (simplified)
            asset_swap_spread = yield_to_maturity - (coupon + (100 - 100) / modified_duration),

            # DV01 in Rands (assuming R1mm nominal)
            dv01_rands = modified_duration * 100,  # Simplified: 1bp move on R1mm

            # Convexity-adjusted duration
            convexity_adj_duration = modified_duration - 0.5 * convexity * 0.01^2,

            # Carry metrics (annualized)
            carry = coupon,

            # Roll-down (1-year, simplified)
            expected_duration_1y = pmax(modified_duration - 1, 0),

            # Yield volatility (20-day realized, annualized)
            # This will be calculated in a grouped operation below

            # Time-based calculations
            days_to_coupon = case_when(
                # Assuming semi-annual coupons
                month(date) <= 2 ~ as.numeric(as.Date(paste0(year(date), "-02-28")) - date),
                month(date) <= 8 ~ as.numeric(as.Date(paste0(year(date), "-08-31")) - date),
                TRUE ~ as.numeric(as.Date(paste0(year(date) + 1, "-02-28")) - date)
            ),

            # Maturity bucket for analysis
            maturity_bucket = case_when(
                modified_duration <= 5 ~ "Short",
                modified_duration <= 10 ~ "Medium",
                modified_duration <= 15 ~ "Long",
                TRUE ~ "Ultra-Long"
            ),
            maturity_bucket = factor(maturity_bucket,
                                     levels = c("Short", "Medium", "Long", "Ultra-Long")),

            # Add seasonality indicators
            month = month(date, label = TRUE),
            quarter = quarter(date),
            year = year(date),
            week_of_month = ceiling(day(date) / 7),
            is_month_end = day(date) >= day(ceiling_date(date, "month") - days(1)) - 2,
            is_quarter_end = month(date) %in% c(3, 6, 9, 12) & is_month_end
        )

    # Calculate rolling volatility and other grouped metrics
    df_enhanced <- df_enhanced %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            # Daily yield changes
            yield_change_1d = yield_to_maturity - lag(yield_to_maturity),

            # Rolling volatility (20-day, annualized)
            yield_vol_20d = zoo::rollapply(
                yield_change_1d, 20, sd, fill = NA, align = "right"
            ) * sqrt(252),

            # Rolling volatility (60-day, annualized)
            yield_vol_60d = zoo::rollapply(
                yield_change_1d, 60, sd, fill = NA, align = "right"
            ) * sqrt(252),

            # Momentum indicators
            yield_ma_5d = zoo::rollmean(yield_to_maturity, 5, fill = NA, align = "right"),
            yield_ma_20d = zoo::rollmean(yield_to_maturity, 20, fill = NA, align = "right"),
            yield_ma_60d = zoo::rollmean(yield_to_maturity, 60, fill = NA, align = "right"),

            momentum_5d = yield_to_maturity - yield_ma_5d,
            momentum_20d = yield_to_maturity - yield_ma_20d,
            momentum_signal = case_when(
                yield_ma_5d < yield_ma_20d & lag(yield_ma_5d) >= lag(yield_ma_20d) ~ "Bullish Cross",
                yield_ma_5d > yield_ma_20d & lag(yield_ma_5d) <= lag(yield_ma_20d) ~ "Bearish Cross",
                yield_ma_5d < yield_ma_20d ~ "Bullish",
                yield_ma_5d > yield_ma_20d ~ "Bearish",
                TRUE ~ "Neutral"
            ),

            # Auction metrics
            days_since_auction = ifelse(
                !is.na(bid_to_cover),
                0,
                cumsum(!is.na(bid_to_cover)) * as.numeric(date - lag(date, default = first(date)))
            ),

            # Calculate average days between auctions for this bond
            auction_frequency = {
                auction_dates <- date[!is.na(bid_to_cover)]
                if(length(auction_dates) > 1) {
                    mean(diff(auction_dates))
                } else {
                    NA_real_
                }
            },

            # Flag if overdue for auction
            auction_overdue = days_since_auction > (auction_frequency * 1.5)
        ) %>%
        ungroup()

    # Add cross-sectional percentile ranks
    df_enhanced <- df_enhanced %>%
        group_by(date) %>%
        mutate(
            yield_percentile = percent_rank(yield_to_maturity) * 100,
            duration_percentile = percent_rank(modified_duration) * 100,
            spread_percentile = percent_rank(spread_to_benchmark) * 100,

            # Relative richness/cheapness vs peers
            yield_vs_curve = {
                if(sum(!is.na(yield_to_maturity)) >= 3) {
                    fit <- lm(yield_to_maturity ~ poly(modified_duration, 2))
                    residuals(fit)
                } else {
                    NA_real_
                }
            }
        ) %>%
        ungroup()

    return(df_enhanced)
}

#' Calculate Seasonal Adjustment Factors
#' @param df Enhanced dataframe
#' @return Dataframe with seasonal adjustments
calculate_seasonal_factors <- function(df) {
    # Calculate historical seasonal patterns
    seasonal_patterns <- df %>%
        filter(!is.na(bid_to_cover)) %>%
        group_by(month, bond) %>%
        summarise(
            avg_btc = mean(bid_to_cover, na.rm = TRUE),
            avg_spread = mean(spread_to_benchmark, na.rm = TRUE),
            n_obs = n(),
            .groups = "drop"
        )

    # Calculate overall averages
    overall_avg <- df %>%
        filter(!is.na(bid_to_cover)) %>%
        summarise(
            overall_btc = mean(bid_to_cover, na.rm = TRUE),
            overall_spread = mean(spread_to_benchmark, na.rm = TRUE)
        )

    # Calculate seasonal factors
    seasonal_factors <- seasonal_patterns %>%
        mutate(
            btc_seasonal_factor = avg_btc / overall_avg$overall_btc,
            spread_seasonal_factor = avg_spread / overall_avg$overall_spread
        ) %>%
        select(month, bond, btc_seasonal_factor, spread_seasonal_factor)

    # Merge back to main dataframe
    df_seasonal <- df %>%
        left_join(seasonal_factors, by = c("month", "bond")) %>%
        mutate(
            # Seasonally adjusted metrics
            btc_seasonally_adj = bid_to_cover / btc_seasonal_factor,
            spread_seasonally_adj = spread_to_benchmark / spread_seasonal_factor
        )

    return(df_seasonal)
}

# ================== PERFORMANCE OPTIMIZATION ==================

#' Create Optimized Cache for Expensive Calculations
#' @param df Dataframe to cache calculations for
#' @return List with cached results
create_calculation_cache <- function(df) {
    cache <- list()

    # Cache spline fits by date
    cache$spline_fits <- df %>%
        group_by(date) %>%
        filter(sum(!is.na(yield_to_maturity)) >= 3) %>%
        group_map(~ {
            list(
                date = .y$date,
                fit = smooth.spline(.x$modified_duration, .x$yield_to_maturity, spar = 0.7)
            )
        })

    # Cache historical percentiles
    cache$historical_percentiles <- df %>%
        group_by(bond) %>%
        summarise(
            yield_percentiles = list(quantile(yield_to_maturity,
                                              probs = seq(0, 1, 0.01),
                                              na.rm = TRUE)),
            spread_percentiles = list(quantile(spread_to_benchmark,
                                               probs = seq(0, 1, 0.01),
                                               na.rm = TRUE)),
            .groups = "drop"
        )

    # Cache rolling statistics
    cache$rolling_stats <- df %>%
        group_by(bond) %>%
        arrange(date) %>%
        summarise(
            dates = list(date),
            roll_mean_20d = list(zoo::rollmean(yield_to_maturity, 20,
                                               fill = NA, align = "right")),
            roll_sd_20d = list(zoo::rollapply(yield_to_maturity, 20, sd,
                                              fill = NA, align = "right")),
            .groups = "drop"
        )

    # Add cache timestamp
    cache$created_at <- Sys.time()
    cache$valid_until <- Sys.time() + hours(24)

    return(cache)
}

#' Process Large Dataset Efficiently
#' @param df Large dataframe
#' @param chunk_size Size of chunks for processing
#' @return Processed dataframe
process_large_dataset <- function(df, chunk_size = 10000) {
    # If dataset is small, process normally
    if(nrow(df) <= chunk_size) {
        return(df)
    }

    # Split into chunks
    n_chunks <- ceiling(nrow(df) / chunk_size)
    chunks <- split(df, rep(1:n_chunks, each = chunk_size, length.out = nrow(df)))

    # Process each chunk
    processed_chunks <- lapply(chunks, function(chunk) {
        # Apply heavy calculations to chunk
        chunk %>%
            add_derived_metrics() %>%
            calculate_seasonal_factors()
    })

    # Combine results
    result <- bind_rows(processed_chunks)

    return(result)
}

# ================== UTILITY FUNCTIONS ==================

#' Generate Data Quality Report
#' @param df Processed dataframe
#' @return Formatted quality report
generate_quality_report <- function(df) {
    report <- list()

    # Overall statistics
    report$summary <- data.frame(
        Metric = c("Total Records", "Date Range", "Bonds", "Completeness"),
        Value = c(
            format(nrow(df), big.mark = ","),
            paste(min(df$date), "to", max(df$date)),
            n_distinct(df$bond),
            paste0(round((1 - sum(is.na(df)) / (nrow(df) * ncol(df))) * 100, 1), "%")
        )
    )

    # Per-bond statistics
    report$bond_quality <- df %>%
        group_by(bond) %>%
        summarise(
            records = n(),
            completeness = round((1 - sum(is.na(yield_to_maturity)) / n()) * 100, 1),
            last_update = max(date),
            days_stale = as.numeric(Sys.Date() - last_update),
            avg_btc = round(mean(bid_to_cover, na.rm = TRUE), 2),
            data_quality = case_when(
                completeness > 95 & days_stale <= 2 ~ "Excellent",
                completeness > 85 & days_stale <= 5 ~ "Good",
                completeness > 70 & days_stale <= 10 ~ "Fair",
                TRUE ~ "Poor"
            ),
            .groups = "drop"
        ) %>%
        arrange(desc(completeness))

    # Missing data patterns
    report$missing_patterns <- df %>%
        select(date, bond, yield_to_maturity, bid_to_cover, modified_duration) %>%
        pivot_longer(cols = c(yield_to_maturity, bid_to_cover, modified_duration),
                     names_to = "variable",
                     values_to = "value") %>%
        group_by(variable) %>%
        summarise(
            missing_count = sum(is.na(value)),
            missing_pct = round(sum(is.na(value)) / n() * 100, 1),
            .groups = "drop"
        ) %>%
        arrange(desc(missing_pct))

    return(report)
}

#' Export Processed Data with Metadata
#' @param df Processed dataframe
#' @param filename Output filename
#' @param include_metadata Whether to include metadata sheet
export_processed_data <- function(df, filename, include_metadata = TRUE) {

    if(include_metadata) {
        # Create metadata
        metadata <- data.frame(
            Field = names(df),
            Type = sapply(df, class),
            Description = c(
                "Date of observation",
                "Bond identifier",
                "Modified duration in years",
                "Yield to maturity (%)",
                # ... add descriptions for all fields
                rep("Calculated field", ncol(df) - 4)
            ),
            stringsAsFactors = FALSE
        )

        # Write to Excel with multiple sheets
        library(openxlsx)
        wb <- createWorkbook()
        addWorksheet(wb, "Data")
        addWorksheet(wb, "Metadata")
        addWorksheet(wb, "Quality_Report")

        writeData(wb, "Data", df)
        writeData(wb, "Metadata", metadata)

        quality_report <- generate_quality_report(df)
        writeData(wb, "Quality_Report", quality_report$summary)

        saveWorkbook(wb, filename, overwrite = TRUE)

    } else {
        # Simple CSV export
        write.csv(df, filename, row.names = FALSE)
    }

    message(paste("Data exported to:", filename))
}