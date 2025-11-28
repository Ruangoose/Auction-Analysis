# Enhanced Data Loading Script for SA Government Bond Dashboard
# Load this before running the enhanced dashboard

# Load required packages
pacman::p_load(readxl, dplyr, tidyverse, lubridate, TTR, forecast, memoise)

# Load required libraries
pacman::p_load(shiny)
pacman::p_load(shinydashboard)
pacman::p_load(shinyWidgets)
pacman::p_load(tidyverse)
pacman::p_load(ggplot2)
pacman::p_load(lubridate)
pacman::p_load(scales)
pacman::p_load(readxl)
pacman::p_load(splines)
pacman::p_load(zoo)
pacman::p_load(DT)
pacman::p_load(rmarkdown)
pacman::p_load(knitr)
pacman::p_load(TTR)  # For technical indicators
pacman::p_load(forecast)  # For predictions
pacman::p_load(memoise)  # For caching
pacman::p_load(tinytex)  # For caching

# Set data parameters
last_date <- today()

# Function to dynamically detect available bonds
detect_available_bonds <- function(df) {
    # Get all column names except date
    cols <- names(df)
    cols[!cols %in% c("date")]
}

# Load Modified Duration data
mod_dur_df <- read_excel("data/Siyanda Bonds.xlsx", sheet = "mod_dur") %>%
    filter(date < last_date)

# Dynamically get bond names
available_bonds <- detect_available_bonds(mod_dur_df)
message("Detected bonds: ", paste(available_bonds, collapse = ", "))

# Process Modified Duration
mod_dur_df <- mod_dur_df %>%
    select(date, all_of(available_bonds)) %>%
    mutate(date = lubridate::ymd(date)) %>%
    pivot_longer(-date,
                 names_to = "bond",
                 values_to = "modified_duration")

# Load Yield to Maturity data
ytm_df <- read_excel("data/Siyanda Bonds.xlsx", sheet = "ytm") %>%
    filter(date < last_date) %>%
    select(date, all_of(available_bonds)) %>%
    mutate(date = lubridate::ymd(date)) %>%
    pivot_longer(-date,
                 names_to = "bond",
                 values_to = "yield_to_maturity")

# Load Duration data
dur_df <- read_excel("data/Siyanda Bonds.xlsx", sheet = "dur") %>%
    filter(date < last_date) %>%
    select(date, all_of(available_bonds)) %>%
    mutate(date = lubridate::ymd(date)) %>%
    pivot_longer(-date,
                 names_to = "bond",
                 values_to = "duration")

# Load Auction data with enhanced processing
auction_df <- read_excel("data/Siyanda Bonds.xlsx", sheet = "auctions") %>%
    mutate(
        announcement_date = lubridate::ymd(announce_date),
        offer_date = lubridate::ymd(offer_date),
        settle_date = lubridate::ymd(sett_date),
        mature_date = lubridate::ymd(mat_date)
    ) %>%
    select(offer_date, bond, offer, allocation, bids, bid_to_cover,
           announcement_date, settle_date, mature_date) %>%
    mutate(date = offer_date)

# Load Convexity data
conv_df <- read_excel("data/Siyanda Bonds.xlsx", sheet = "conv") %>%
    filter(date < last_date) %>%
    select(date, all_of(available_bonds)) %>%
    mutate(date = lubridate::ymd(date)) %>%
    pivot_longer(-date,
                 names_to = "bond",
                 values_to = "convexity")

# Load Coupon data
cpn_df <- read_excel("data/Siyanda Bonds.xlsx", sheet = "cpn") %>%
    select(date, all_of(available_bonds)) %>%
    mutate(date = lubridate::ymd(date)) %>%
    pivot_longer(-date,
                 names_to = "bond",
                 values_to = "coupon") %>%
    filter(!is.na(coupon)) %>%
    group_by(bond) %>%
    distinct(coupon) %>%
    ungroup()

# Optional: Load additional sheets if available
load_optional_sheet <- function(file, sheet_name, value_name) {
    tryCatch({
        df <- read_excel(file, sheet = sheet_name) %>%
            filter(date < last_date) %>%
            select(date, all_of(available_bonds)) %>%
            mutate(date = lubridate::ymd(date)) %>%
            pivot_longer(-date,
                         names_to = "bond",
                         values_to = value_name)
        message("Loaded sheet: ", sheet_name)
        return(df)
    }, error = function(e) {
        message("Optional sheet '", sheet_name, "' not found - skipping")
        return(NULL)
    })
}

# Try to load additional data if available
accrued_df <- load_optional_sheet("data/Siyanda Bonds.xlsx", "accrued", "accrued_interest")
clean_price_df <- load_optional_sheet("data/Siyanda Bonds.xlsx", "clean_price", "clean_price")
full_price_df <- load_optional_sheet("data/Siyanda Bonds.xlsx", "full_price", "full_price")
bpv_df <- load_optional_sheet("data/Siyanda Bonds.xlsx", "bpv", "basis_point_value")

# Combine all data into master dataframe
full_df <- mod_dur_df %>%
    left_join(ytm_df, by = c("date", "bond")) %>%
    left_join(dur_df, by = c("date", "bond")) %>%
    left_join(conv_df, by = c("date", "bond")) %>%
    left_join(cpn_df, by = "bond") %>%
    left_join(auction_df, by = c("date", "bond"))

# Add optional data if available
if (!is.null(accrued_df)) full_df <- left_join(full_df, accrued_df, by = c("date", "bond"))
if (!is.null(clean_price_df)) full_df <- left_join(full_df, clean_price_df, by = c("date", "bond"))
if (!is.null(full_price_df)) full_df <- left_join(full_df, full_price_df, by = c("date", "bond"))
if (!is.null(bpv_df)) full_df <- left_join(full_df, bpv_df, by = c("date", "bond"))

# Add calculated fields for enhanced features
full_df <- full_df %>%
    mutate(
        # Calculate DV01 if not provided
        dv01 = if("basis_point_value" %in% names(.)) basis_point_value else modified_duration * 0.01,

        # Calculate time to maturity
        time_to_maturity = as.numeric(difftime(mature_date, date, units = "days")) / 365.25,

        # Add maturity buckets
        maturity_bucket = case_when(
            time_to_maturity <= 3 ~ "Short",
            time_to_maturity <= 7 ~ "Medium",
            time_to_maturity <= 12 ~ "Long",
            time_to_maturity > 12 ~ "Ultra-Long",
            TRUE ~ "Unknown"
        ),

        # Time-based groupings
        year = year(date),
        quarter = quarter(date),
        month = month(date),
        week = week(date),

        # Flags for filtering
        ytd_flag = year == year(today()),

        # Auction metrics
        auction_success = ifelse(!is.na(bid_to_cover),
                                 ifelse(bid_to_cover > 2, "Success", "Weak"),
                                 NA)
    ) %>%
    # Remove any completely empty rows
    filter(rowSums(is.na(select(., -c(date, bond)))) != ncol(select(., -c(date, bond))))

# Create enhanced metadata
bond_metadata <- full_df %>%
    group_by(bond) %>%
    summarise(
        coupon = first(na.omit(coupon)),
        avg_duration = mean(modified_duration, na.rm = TRUE),
        avg_convexity = mean(convexity, na.rm = TRUE),
        latest_ytm = last(na.omit(yield_to_maturity)),
        first_date = min(date),
        last_date = max(date),
        maturity = first(na.omit(mature_date)),
        total_auctions = sum(!is.na(bid_to_cover)),
        avg_bid_cover = mean(bid_to_cover, na.rm = TRUE),
        data_completeness = sum(!is.na(yield_to_maturity)) / n() * 100,
        .groups = "drop"
    ) %>%
    arrange(avg_duration)

# Enhanced auction summary
auction_summary <- auction_df %>%
    filter(!is.na(bid_to_cover)) %>%
    group_by(bond) %>%
    summarise(
        total_auctions = n(),
        total_offered = sum(offer, na.rm = TRUE) / 1e9,
        total_allocated = sum(allocation, na.rm = TRUE) / 1e9,
        avg_bid_cover = mean(bid_to_cover, na.rm = TRUE),
        min_bid_cover = min(bid_to_cover, na.rm = TRUE),
        max_bid_cover = max(bid_to_cover, na.rm = TRUE),
        std_bid_cover = sd(bid_to_cover, na.rm = TRUE),
        last_auction = max(offer_date),
        trend_coefficient = {
            if(n() > 2) {
                coef(lm(bid_to_cover ~ as.numeric(offer_date)))[2]
            } else {
                NA
            }
        },
        .groups = "drop"
    ) %>%
    mutate(
        trend_direction = case_when(
            trend_coefficient > 0.01 ~ "Improving",
            trend_coefficient < -0.01 ~ "Deteriorating",
            TRUE ~ "Stable"
        )
    )

# Create technical indicators preview (for latest date)
add_latest_technicals <- function(data) {
    data %>%
        group_by(bond) %>%
        arrange(date) %>%
        mutate(
            ma_20 = rollapply(yield_to_maturity, 20, mean, fill = NA, align = "right", partial = TRUE),
            ma_50 = rollapply(yield_to_maturity, 50, mean, fill = NA, align = "right", partial = TRUE),

            # Simple RSI
            price_change = yield_to_maturity - lag(yield_to_maturity, 1),
            gain = ifelse(price_change > 0, price_change, 0),
            loss = ifelse(price_change < 0, abs(price_change), 0),
            avg_gain = rollapply(gain, 14, mean, fill = NA, align = "right", partial = TRUE),
            avg_loss = rollapply(loss, 14, mean, fill = NA, align = "right", partial = TRUE),
            rsi = 100 - (100 / (1 + avg_gain / avg_loss)),

            # Technical signal
            signal = case_when(
                rsi > 70 ~ "Overbought",
                rsi < 30 ~ "Oversold",
                yield_to_maturity > ma_20 & ma_20 > ma_50 ~ "Bullish",
                yield_to_maturity < ma_20 & ma_20 < ma_50 ~ "Bearish",
                TRUE ~ "Neutral"
            )
        ) %>%
        filter(date == max(date)) %>%
        select(bond, date, yield_to_maturity, ma_20, ma_50, rsi, signal) %>%
        ungroup()
}

latest_technicals <- add_latest_technicals(full_df)

# Print comprehensive data quality report
cat("=====================================\n")
cat("ENHANCED DATA QUALITY REPORT\n")
cat("=====================================\n")
cat("Date Range:", format(min(full_df$date)), "to", format(max(full_df$date)), "\n")
cat("Total Observations:", format(nrow(full_df), big.mark = ","), "\n")
cat("Unique Bonds:", n_distinct(full_df$bond), "\n")
cat("Available Bonds:", paste(sort(unique(full_df$bond)), collapse = ", "), "\n")
cat("Bonds with auction data:", n_distinct(auction_df$bond[!is.na(auction_df$bid_to_cover)]), "\n")

# Data completeness by field
completeness <- full_df %>%
    summarise(across(everything(), ~sum(!is.na(.)) / n() * 100)) %>%
    pivot_longer(everything(), names_to = "Field", values_to = "Completeness") %>%
    arrange(desc(Completeness))

cat("\n=== Data Completeness ===\n")
print(completeness %>% filter(Completeness < 100), n = 20)

cat("\n=== Bond Coverage Summary ===\n")
print(bond_metadata %>%
          select(bond, coupon, avg_duration, latest_ytm, total_auctions, data_completeness) %>%
          mutate(
              coupon = sprintf("%.2f%%", coupon),
              avg_duration = sprintf("%.2f", avg_duration),
              latest_ytm = sprintf("%.2f%%", latest_ytm),
              data_completeness = sprintf("%.1f%%", data_completeness)
          ))

cat("\n=== Latest Technical Signals ===\n")
print(latest_technicals %>%
          mutate(
              yield = sprintf("%.2f%%", yield_to_maturity),
              ma_20 = sprintf("%.2f%%", ma_20),
              ma_50 = sprintf("%.2f%%", ma_50),
              rsi = sprintf("%.1f", rsi)
          ) %>%
          select(bond, yield, ma_20, ma_50, rsi, signal))

cat("\n=== Recent Auction Trends ===\n")
print(auction_summary %>%
          filter(last_auction >= today() - days(30)) %>%
          select(bond, total_auctions, avg_bid_cover, trend_direction, last_auction) %>%
          mutate(
              avg_bid_cover = sprintf("%.2fx", avg_bid_cover),
              last_auction = format(last_auction, "%Y-%m-%d")
          ))

# Save all processed data
saveRDS(full_df, "data/processed_bond_data.rds")
saveRDS(bond_metadata, "data/bond_metadata.rds")
saveRDS(auction_summary, "data/auction_summary.rds")
saveRDS(latest_technicals, "data/latest_technicals.rds")

# Create a backup with timestamp
backup_dir <- "data/backups"
if (!dir.exists(backup_dir)) dir.create(backup_dir)
saveRDS(full_df, file.path(backup_dir, paste0("bond_data_", format(Sys.Date(), "%Y%m%d"), ".rds")))

cat("\n✅ Enhanced data processing complete!\n")
cat("📁 Files saved to data/ directory\n")
cat("🚀 Ready to run enhanced dashboard\n")