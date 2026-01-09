# Enhanced Plotting Module with Professional ggplot2 Visualizations
# Purpose: Create actionable visualizations for bond auction decisions

library(ggplot2)
library(viridis)
library(scales)
library(gridExtra)
library(ggrepel)

# Define Insele color palette
insele_colors <- list(
    navy = "#1e3a5f",
    teal = "#6b9aa0",
    orange = "#f47920",
    light_teal = "#a3c4c9",
    dark_navy = "#0f1f3b",
    green = "#4CAF50",
    red = "#F44336",
    yellow = "#FFC107",
    gray = "#9E9E9E"
)

# Enhanced theme function
theme_insele <- function(base_size = 12) {
    theme_minimal(base_size = base_size) %+replace%
        theme(
            text = element_text(family = "Arial", color = insele_colors$dark_navy),
            plot.title = element_text(size = base_size * 1.5, face = "bold",
                                      color = insele_colors$navy, margin = margin(b = 10)),
            plot.subtitle = element_text(size = base_size * 0.9,
                                         color = insele_colors$teal, margin = margin(b = 10)),
            plot.caption = element_text(size = base_size * 0.7,
                                        color = insele_colors$light_teal,
                                        hjust = 1, margin = margin(t = 10)),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey92", linewidth = 0.5),
            axis.title = element_text(size = base_size * 0.9, face = "bold"),
            axis.text = element_text(size = base_size * 0.8),
            legend.title = element_text(face = "bold", size = base_size * 0.9),
            legend.text = element_text(size = base_size * 0.8),
            legend.position = "right",
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.margin = ggplot2::margin(20, 20, 20, 20)
        )
}

# ================== SUPPLY/DEMAND VISUALIZATIONS ==================

#' Enhanced Supply/Demand Scatter Plot with Advanced Features
create_enhanced_supply_demand_plot <- function(df, x_var = "modified_duration",
                                               y_var = "bid_to_cover",
                                               size_var = "total_offered") {
    # Prepare data
    plot_data <- df %>%
        group_by(bond) %>%
        summarise(
            avg_modified_duration = mean(modified_duration, na.rm = TRUE),
            avg_duration = mean(duration, na.rm = TRUE),
            avg_yield_to_maturity = mean(yield_to_maturity, na.rm = TRUE),  # Add this line
            avg_bid_to_cover = mean(bid_to_cover, na.rm = TRUE),
            avg_allocation = mean(allocation / offer * 100, na.rm = TRUE),  # Add avg_ prefix
            total_bids = sum(bids, na.rm = TRUE) / 1e9,
            avg_total_bids = sum(bids, na.rm = TRUE) / 1e9,  # Add this duplicate with avg_ prefix
            total_offered = sum(offer, na.rm = TRUE) / 1e9,
            frequency = sum(!is.na(bid_to_cover)),
            liquidity = mean(bid_to_cover, na.rm = TRUE) * sum(!is.na(bid_to_cover)),  # Add liquidity calculation
            last_btc = last(bid_to_cover[!is.na(bid_to_cover)]),
            prev_btc = nth(bid_to_cover[!is.na(bid_to_cover)], -2, default = NA),
            .groups = "drop"
        ) %>%
        filter(!is.na(avg_bid_to_cover)) %>%
        mutate(
            # Calculate with NA handling
            btc_change = coalesce(last_btc - prev_btc, 0),
            improving = !is.na(btc_change) & btc_change > 0
        )

    # Determine zones for background
    sweet_spot_x <- quantile(plot_data[[paste0("avg_", x_var)]], c(0.3, 0.7), na.rm = TRUE)
    sweet_spot_y <- quantile(plot_data[[paste0("avg_", y_var)]], c(0.4, 0.8), na.rm = TRUE)

    # Create base plot with zones
    p <- ggplot(plot_data, aes_string(x = paste0("avg_", x_var),
                                      y = paste0("avg_", y_var))) +
        # Add background zones
        annotate("rect",
                 xmin = sweet_spot_x[1], xmax = sweet_spot_x[2],
                 ymin = sweet_spot_y[1], ymax = sweet_spot_y[2],
                 alpha = 0.1, fill = insele_colors$green) +
        annotate("text",
                 x = mean(sweet_spot_x), y = max(plot_data[[paste0("avg_", y_var)]]) * 0.95,
                 label = "Sweet Spot", color = insele_colors$green,
                 size = 4, fontface = "italic", alpha = 0.7) +

        # Main scatter plot
        geom_point(aes_string(size = size_var,
                              fill = "bond",
                              alpha = "frequency"),
                   shape = 21, color = "white", stroke = 1.5) +

        # Add arrows showing movement
        geom_segment(
            data = plot_data %>% filter(!is.na(btc_change)),
            aes(xend = avg_modified_duration + btc_change * 0.2,
                yend = avg_bid_to_cover,
                color = improving),
            arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
            alpha = 0.6, size = 0.8
        ) +

        # Add bond labels with smart positioning
        geom_text_repel(aes(label = bond),
                        size = 3.5,
                        box.padding = 0.5,
                        point.padding = 0.3,
                        segment.color = insele_colors$gray,
                        segment.size = 0.3,
                        max.overlaps = 20) +

        # Historical average points (ghost points)
        geom_point(aes_string(x = paste0("avg_", x_var),
                              y = paste0("avg_", y_var)),
                   shape = 1, size = 8, alpha = 0.3, color = insele_colors$dark_navy) +

        # Scales and theme
        scale_size_continuous(range = c(4, 16),
                              name = gsub("_", " ", str_to_title(size_var)),
                              guide = guide_legend(override.aes = list(fill = insele_colors$teal))) +
        scale_fill_viridis_d(option = "turbo", name = "Bond") +
        scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
        scale_color_manual(values = c("TRUE" = insele_colors$green, "FALSE" = insele_colors$red),
                           name = "Trend", labels = c("Weakening", "Improving"),
                           guide = guide_legend(override.aes = list(size = 4))) +

        labs(
            title = "Supply & Demand Analysis with Momentum Indicators",
            subtitle = paste("Sized by", gsub("_", " ", size_var), "| Arrows show recent trend"),
            x = gsub("_", " ", str_to_title(x_var)),
            y = gsub("_", " ", str_to_title(y_var)),
            caption = paste("Data as of", format(Sys.Date(), "%B %d, %Y"), "| Insele Capital Partners")
        ) +
        theme_insele() +
        theme(legend.position = "right")

    # Add marginal density plots indicators
    p <- p +
        annotate("text", x = Inf, y = Inf,
                 label = paste("Avg", y_var, ":", round(mean(plot_data[[paste0("avg_", y_var)]]), 2)),
                 hjust = 1.1, vjust = 1.5, size = 3, color = insele_colors$teal)

    return(p)
}

#' Auction Demand Heatmap Calendar
create_auction_demand_heatmap <- function(df) {
    # Prepare data with weekly aggregation
    heatmap_data <- df %>%
        filter(!is.na(bid_to_cover)) %>%
        mutate(
            week = lubridate::week(date),
            year = lubridate::year(date),
            year_week = paste0(year, "-W", sprintf("%02d", week))
        ) %>%
        group_by(bond, year_week) %>%
        summarise(
            avg_btc = mean(bid_to_cover, na.rm = TRUE),
            .groups = "drop"
        )

    # Order bonds by maturity
    bond_order <- df %>%
        group_by(bond) %>%
        summarise(avg_duration = mean(modified_duration, na.rm = TRUE)) %>%
        arrange(avg_duration) %>%
        pull(bond)

    heatmap_data$bond <- factor(heatmap_data$bond, levels = bond_order)

    # Create heatmap
    p <- ggplot(heatmap_data, aes(x = year_week, y = bond, fill = avg_btc)) +
        geom_tile(color = "white", size = 0.5) +
        scale_fill_gradient2(
            low = insele_colors$red,
            mid = insele_colors$yellow,
            high = insele_colors$green,
            midpoint = median(heatmap_data$avg_btc, na.rm = TRUE),
            name = "Bid-to-Cover",
            limits = c(1, 5),
            oob = scales::squish
        ) +

        # Mark upcoming auctions
        geom_tile(data = heatmap_data %>%
                      filter(year_week == paste0(lubridate::year(Sys.Date()),
                                                 "-W",
                                                 sprintf("%02d", lubridate::week(Sys.Date() + 7)))),
                  color = insele_colors$orange, fill = NA, size = 2) +

        labs(
            title = "Weekly Auction Demand Heatmap",
            subtitle = "Darker green indicates stronger demand | Orange border = next week",
            x = "Week",
            y = "Bond (ordered by maturity)",
            caption = "Insele Capital Partners"
        ) +
        theme_insele() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm")
        )

    return(p)
}

# ================== RELATIVE VALUE VISUALIZATIONS ==================

#' Enhanced Yield Curve with Advanced Features
create_enhanced_yield_curve <- function(df, show_history = TRUE, confidence_bands = TRUE) {
    # Get current and historical data
    latest_data <- df %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        arrange(modified_duration)

    prev_week_data <- df %>%
        filter(date >= max(date) - 7 & date < max(date)) %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        arrange(modified_duration)

    # Fit curves
    current_fit <- smooth.spline(latest_data$modified_duration,
                                 latest_data$yield_to_maturity, spar = 0.7)

    # Create smooth curve data
    x_smooth <- seq(min(latest_data$modified_duration),
                    max(latest_data$modified_duration),
                    length.out = 100)
    y_smooth_current <- predict(current_fit, x_smooth)$y

    # Calculate fair value corridor
    residuals <- latest_data$yield_to_maturity -
        predict(current_fit, latest_data$modified_duration)$y
    corridor_width <- sd(residuals) * 100  # Convert to bps

    # Calculate liquidity scores for sizing
    liquidity_scores <- df %>%
        group_by(bond) %>%
        summarise(
            # Count non-NA bid_to_cover values
            n_auctions = sum(!is.na(bid_to_cover)),
            # Calculate liquidity only if there are auctions
            liquidity = ifelse(
                n_auctions > 0,
                n_auctions * mean(bid_to_cover, na.rm = TRUE),
                0.1  # Small default value
            ),
            # Calculate days since auction with proper handling
            days_since_auction = {
                auction_dates <- date[!is.na(bid_to_cover)]
                if(length(auction_dates) > 0) {
                    as.numeric(Sys.Date() - max(auction_dates))
                } else {
                    999  # Default for bonds with no auction history
                }
            },
            .groups = "drop"
        ) %>%
        mutate(
            # Handle cases with no auction history
            liquidity = ifelse(is.na(liquidity), 0.1, liquidity),
            days_since_auction = ifelse(is.infinite(days_since_auction) | is.na(days_since_auction), 999, days_since_auction)
        )

    latest_data <- latest_data %>%
        left_join(liquidity_scores, by = "bond") %>%
        mutate(
            # Use coalesce or check column existence
            liquidity = coalesce(liquidity, 1),
            days_since_auction = coalesce(days_since_auction, 999)
        )

    # Determine curve shape
    curve_shape <- case_when(
        y_smooth_current[100] - y_smooth_current[1] > 1.5 ~ "Steep",
        y_smooth_current[100] - y_smooth_current[1] > 0.5 ~ "Normal",
        y_smooth_current[100] - y_smooth_current[1] > 0 ~ "Flat",
        TRUE ~ "Inverted"
    )

    # Base plot
    p <- ggplot() +
        # Add confidence bands if requested
        {if(confidence_bands) {
            list(
                geom_ribbon(aes(x = x_smooth,
                                ymin = y_smooth_current - corridor_width/100,
                                ymax = y_smooth_current + corridor_width/100),
                            alpha = 0.2, fill = insele_colors$teal)
            )
        }}

    # Add previous week's curve if requested
    if(show_history & nrow(prev_week_data) > 3) {
        prev_fit <- smooth.spline(prev_week_data$modified_duration,
                                  prev_week_data$yield_to_maturity, spar = 0.7)
        y_smooth_prev <- predict(prev_fit, x_smooth)$y

        p <- p +
            geom_line(aes(x = x_smooth, y = y_smooth_prev),
                      linetype = "dashed", color = insele_colors$gray, size = 1, alpha = 0.7) +
            annotate("text", x = max(x_smooth) * 0.9, y = y_smooth_prev[90],
                     label = "Previous Week", color = insele_colors$gray,
                     size = 3, fontface = "italic")
    }

    # Add main curve and points
    p <- p +
        # Current curve
        geom_line(aes(x = x_smooth, y = y_smooth_current),
                  color = insele_colors$orange, size = 2) +

        # Bond points with liquidity-based sizing and coloring
        geom_point(data = latest_data,
                   aes(x = modified_duration, y = yield_to_maturity,
                       size = liquidity,
                       fill = days_since_auction),
                   shape = 21, color = "white", stroke = 1.5, alpha = 0.9) +

        # Bond labels with days since auction
        geom_text_repel(data = latest_data,
                        aes(x = modified_duration, y = yield_to_maturity,
                            label = paste0(bond, "\n(", days_since_auction, "d)")),
                        size = 3, box.padding = 0.3, segment.size = 0.3) +

        # Scales
        scale_size_continuous(range = c(4, 12), name = "Liquidity Score",
                              guide = guide_legend(order = 1)) +
        scale_fill_gradient2(low = insele_colors$green,
                             mid = insele_colors$yellow,
                             high = insele_colors$red,
                             midpoint = 30,
                             name = "Days Since\nAuction",
                             guide = guide_colorbar(order = 2)) +

        # Labels and theme
        labs(
            title = paste("Yield Curve Analysis -", curve_shape, "Shape"),
            subtitle = paste("Fair value corridor: ±", round(corridor_width, 0), "bps | ",
                             "Point size = liquidity | Color = auction recency"),
            x = "Modified Duration (years)",
            y = "Yield to Maturity (%)",
            caption = paste("As of", format(Sys.Date(), "%B %d, %Y"), "| Insele Capital Partners")
        ) +
        theme_insele() +

        # Add curve shape indicator
        annotate("text", x = min(x_smooth) + 1, y = max(latest_data$yield_to_maturity) - 0.2,
                 label = paste("Shape:", curve_shape),
                 color = insele_colors$navy, size = 5, fontface = "bold")

    return(p)
}

#' Rich/Cheap Waterfall Chart
create_rich_cheap_waterfall <- function(df, curve_fit = NULL) {
    # Get latest data and fit curve if not provided
    latest_data <- df %>%
        group_by(bond) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        arrange(modified_duration)

    if(is.null(curve_fit)) {
        curve_fit <- smooth.spline(latest_data$modified_duration,
                                   latest_data$yield_to_maturity, spar = 0.7)
    }

    # Calculate deviations
    latest_data$fitted_yield <- predict(curve_fit, latest_data$modified_duration)$y
    latest_data$deviation_bps <- (latest_data$yield_to_maturity - latest_data$fitted_yield) * 100

    # Calculate z-scores and percentiles
    latest_data$z_score <- (latest_data$deviation_bps - mean(latest_data$deviation_bps)) /
        sd(latest_data$deviation_bps)

    # Historical percentiles
    hist_deviations <- df %>%
        group_by(bond) %>%
        mutate(
            fitted = predict(smooth.spline(modified_duration, yield_to_maturity, spar = 0.7),
                             modified_duration)$y,
            hist_dev = (yield_to_maturity - fitted) * 100
        ) %>%
        ungroup()

    # Calculate percentiles by joining with latest_data to get current deviation
    percentile_data <- hist_deviations %>%
        group_by(bond) %>%
        summarise(
            all_hist_devs = list(hist_dev),  # Store all historical deviations as a list
            .groups = "drop"
        ) %>%
        inner_join(
            latest_data %>% select(bond, deviation_bps),
            by = "bond"
        ) %>%
        mutate(
            percentile = mapply(
                function(hist_vals, current_val) {
                    if(!is.na(current_val) && length(hist_vals) > 0) {
                        ecdf(hist_vals)(current_val) * 100
                    } else {
                        50
                    }
                },
                all_hist_devs,
                deviation_bps
            )
        ) %>%
        select(bond, percentile)

    latest_data <- latest_data %>%
        left_join(percentile_data %>% select(bond, percentile), by = "bond")

    # Order by deviation for waterfall effect
    latest_data <- latest_data %>%
        arrange(deviation_bps) %>%
        mutate(bond = factor(bond, levels = bond))

    # Create waterfall chart
    p <- ggplot(latest_data, aes(x = bond)) +
        # Add horizontal reference lines
        geom_hline(yintercept = 0, color = insele_colors$navy, size = 1) +
        geom_hline(yintercept = c(-20, 20), color = insele_colors$orange,
                   linetype = "dashed", alpha = 0.5) +

        # Main bars
        geom_col(aes(y = deviation_bps, fill = deviation_bps), width = 0.7) +

        # Add percentile rank as text
        geom_text(aes(y = deviation_bps,
                      label = paste0(round(percentile, 0), "%")),
                  vjust = ifelse(latest_data$deviation_bps > 0, -0.5, 1.5),
                  size = 3, color = insele_colors$dark_navy) +

        # Color scale
        scale_fill_gradient2(
            low = insele_colors$green,
            mid = insele_colors$gray,
            high = insele_colors$red,
            midpoint = 0,
            name = "Deviation\n(bps)",
            limits = c(-50, 50),
            oob = scales::squish
        ) +

        # Labels and theme
        labs(
            title = "Rich/Cheap Analysis - Deviation from Fair Value",
            subtitle = "Percentile ranks shown above bars | Reference lines at ±20bps",
            x = "Bond",
            y = "Deviation from Fitted Curve (bps)",
            caption = paste("Negative = Rich, Positive = Cheap | As of",
                            format(Sys.Date(), "%B %d, %Y"))
        ) +
        theme_insele() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.x = element_blank()
        ) +

        # Add annotations for extreme values
        {if(any(abs(latest_data$deviation_bps) > 30)) {
            geom_text(data = latest_data %>% filter(abs(deviation_bps) > 30),
                      aes(y = deviation_bps, label = "!"),
                      color = insele_colors$orange, size = 6, fontface = "bold")
        }}

    return(p)
}

#' Z-Score Evolution Chart
create_zscore_evolution <- function(df, lookback_periods = c(20, 60, 252)) {
    # Calculate z-scores for different periods
    zscore_data <- data.frame()

    for(period in lookback_periods) {
        period_data <- df %>%
            group_by(bond) %>%
            arrange(date) %>%
            mutate(
                roll_mean = zoo::rollmean(yield_to_maturity, period, fill = NA, align = "right"),
                roll_sd = zoo::rollapply(yield_to_maturity, period, sd, fill = NA, align = "right"),
                z_score = (yield_to_maturity - roll_mean) / roll_sd,
                period = paste0(period, "d")
            ) %>%
            filter(!is.na(z_score)) %>%
            ungroup()

        zscore_data <- bind_rows(zscore_data, period_data)
    }

    # Get latest data for each period
    latest_zscores <- zscore_data %>%
        group_by(bond, period) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        arrange(modified_duration)

    # Create line plot
    p <- ggplot(latest_zscores, aes(x = modified_duration, y = z_score,
                                    color = period, group = period)) +
        # Add reference zones
        geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -1.5, ymax = 1.5),
                  fill = insele_colors$light_teal, alpha = 0.1) +
        geom_hline(yintercept = c(-2, -1, 0, 1, 2),
                   color = c(insele_colors$red, insele_colors$orange,
                             insele_colors$navy,
                             insele_colors$orange, insele_colors$red),
                   linetype = c("dashed", "dotted", "solid", "dotted", "dashed"),
                   alpha = c(0.7, 0.5, 1, 0.5, 0.7)) +

        # Lines for each period
        geom_line(size = 1.5, alpha = 0.8) +
        geom_point(size = 3, alpha = 0.9) +

        # Highlight divergences
        geom_point(data = latest_zscores %>%
                       group_by(bond, modified_duration) %>%
                       summarise(divergence = sd(z_score) > 0.5, .groups = "drop") %>%
                       filter(divergence),
                   aes(x = modified_duration, y = 0),
                   shape = 24, size = 4, fill = insele_colors$yellow,
                   color = insele_colors$orange, inherit.aes = FALSE) +

        # Bond labels for extreme values
        geom_text_repel(data = latest_zscores %>%
                            filter(abs(z_score) > 1.5),
                        aes(label = bond),
                        size = 3, box.padding = 0.3) +

        # Scales and theme
        scale_color_manual(values = c(
            "20d" = insele_colors$orange,
            "60d" = insele_colors$teal,
            "252d" = insele_colors$navy
        ), name = "Lookback\nPeriod") +

        labs(
            title = "Term Structure of Z-Scores - Multiple Time Horizons",
            subtitle = "Triangles indicate signal divergence | Shaded area = fair value zone",
            x = "Modified Duration (years)",
            y = "Z-Score (Standard Deviations)",
            caption = "Rich < -1.5 | Cheap > 1.5 | Insele Capital Partners"
        ) +
        theme_insele() +
        theme(legend.position = "top")

    return(p)
}

# ================== AUCTION PERFORMANCE VISUALIZATIONS ==================

#' Bid-to-Cover Evolution with Event Markers
create_btc_evolution_plot <- function(df, add_events = TRUE) {
    # Prepare data by maturity buckets
    btc_data <- df %>%
        filter(!is.na(bid_to_cover)) %>%
        mutate(
            maturity_bucket = case_when(
                modified_duration <= 5 ~ "Short (≤5y)",
                modified_duration <= 10 ~ "Medium (5-10y)",
                modified_duration <= 15 ~ "Long (10-15y)",
                TRUE ~ "Ultra-Long (>15y)"
            ),
            maturity_bucket = factor(maturity_bucket,
                                     levels = c("Short (≤5y)", "Medium (5-10y)",
                                                "Long (10-15y)", "Ultra-Long (>15y)"))
        ) %>%
        group_by(date, maturity_bucket) %>%
        summarise(
            avg_btc = mean(bid_to_cover, na.rm = TRUE),
            .groups = "drop"
        )

    # Calculate moving averages
    btc_data <- btc_data %>%
        group_by(maturity_bucket) %>%
        arrange(date) %>%
        mutate(
            ma_20d = zoo::rollmean(avg_btc, 5, fill = NA, align = "right")
        ) %>%
        ungroup()

    # Create plot
    p <- ggplot(btc_data, aes(x = date)) +
        # Add stress periods (btc < 2)
        geom_rect(data = btc_data %>%
                      filter(avg_btc < 2) %>%
                      group_by(maturity_bucket) %>%
                      summarise(xmin = min(date), xmax = max(date), .groups = "drop"),
                  aes(xmin = xmin - 5, xmax = xmax + 5, ymin = -Inf, ymax = 2),
                  fill = insele_colors$red, alpha = 0.1) +

        # Reference lines
        geom_hline(yintercept = 2, color = insele_colors$red,
                   linetype = "dashed", alpha = 0.5) +
        geom_hline(yintercept = 3, color = insele_colors$navy,
                   linetype = "solid", alpha = 0.3) +

        # Main lines
        geom_line(aes(y = avg_btc, color = maturity_bucket),
                  size = 0.5, alpha = 0.5) +
        geom_line(aes(y = ma_20d, color = maturity_bucket),
                  size = 1.5) +

        # Points for actual auctions
        geom_point(aes(y = avg_btc, color = maturity_bucket),
                   size = 2, alpha = 0.7) +

        # Event markers
        {if(add_events) {
            # Add major rate decision dates (example)
            list(
                annotate("segment", x = as.Date("2024-11-01"), xend = as.Date("2024-11-01"),
                         y = 0, yend = max(btc_data$avg_btc, na.rm = TRUE) * 0.9,
                         color = insele_colors$orange, linetype = "dotted"),
                annotate("text", x = as.Date("2024-11-01"),
                         y = max(btc_data$avg_btc, na.rm = TRUE) * 0.95,
                         label = "Rate Decision", angle = 90, size = 3,
                         hjust = 0, color = insele_colors$orange)
            )
        }} +

        # Scales and theme
        scale_color_manual(values = c(
            "Short (≤5y)" = insele_colors$green,
            "Medium (5-10y)" = insele_colors$teal,
            "Long (10-15y)" = insele_colors$orange,
            "Ultra-Long (>15y)" = insele_colors$red
        ), name = "Maturity\nBucket") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +

        labs(
            title = "Bid-to-Cover Evolution by Maturity Bucket",
            subtitle = "Solid lines = 20-day moving average | Shaded = stress periods (BTC < 2)",
            x = "Date",
            y = "Average Bid-to-Cover Ratio",
            caption = "Insele Capital Partners"
        ) +
        theme_insele() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right"
        )

    return(p)
}

#' Liquidity Spider/Radar Chart
create_liquidity_radar <- function(df) {
    # Calculate liquidity metrics for each bond
    liquidity_metrics <- df %>%
        group_by(bond) %>%
        summarise(
            btc_score = mean(bid_to_cover, na.rm = TRUE) /
                max(mean(df$bid_to_cover, na.rm = TRUE), na.rm = TRUE),
            stability_score = 1 - (sd(bid_to_cover, na.rm = TRUE) /
                                       mean(bid_to_cover, na.rm = TRUE)),
            frequency_score = sum(!is.na(bid_to_cover)) /
                max(sum(!is.na(df$bid_to_cover)), na.rm = TRUE),
            size_score = mean(bids / 1e9, na.rm = TRUE) /
                max(mean(df$bids / 1e9, na.rm = TRUE), na.rm = TRUE),
            recency_score = 1 - (as.numeric(Sys.Date() - max(date[!is.na(bid_to_cover)])) / 90),
            .groups = "drop"
        ) %>%
        mutate(across(c(btc_score, stability_score, frequency_score,
                        size_score, recency_score),
                      ~pmin(pmax(., 0), 1)))  # Ensure 0-1 range

    # Reshape for radar plot (convert to long format)
    radar_data <- liquidity_metrics %>%
        pivot_longer(cols = ends_with("_score"),
                     names_to = "metric",
                     values_to = "score") %>%
        mutate(
            metric = str_replace(metric, "_score", ""),
            metric = str_to_title(metric)
        )

    # Create faceted radar-style plot
    p <- ggplot(radar_data, aes(x = metric, y = score, group = bond)) +
        geom_polygon(aes(fill = bond), alpha = 0.3) +
        geom_line(aes(color = bond), size = 1) +
        geom_point(aes(color = bond), size = 2) +
        coord_polar() +
        facet_wrap(~bond, ncol = 4) +
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
        scale_fill_viridis_d(option = "turbo", guide = "none") +
        scale_color_viridis_d(option = "turbo", guide = "none") +
        labs(
            title = "Multi-Dimensional Liquidity Analysis",
            subtitle = "Larger area indicates better overall liquidity",
            caption = "Metrics: BTC, Stability, Frequency, Size, Recency | Insele Capital Partners"
        ) +
        theme_insele() +
        theme(
            axis.text.x = element_text(size = 8),
            strip.text = element_text(face = "bold"),
            panel.grid = element_line(color = "gray90")
        )

    return(p)
}

# ================== COMPOSITE DASHBOARD ==================

#' Create One-Page Executive Summary Dashboard
create_executive_dashboard <- function(df, signals_df = NULL) {
    # Calculate key metrics
    latest_date <- max(df$date)

    # 1. Mini yield curve
    p_yield <- df %>%
        filter(date == latest_date) %>%
        ggplot(aes(x = modified_duration, y = yield_to_maturity)) +
        geom_smooth(method = "loess", se = FALSE, color = insele_colors$orange, size = 1.5) +
        geom_point(size = 3, color = insele_colors$navy, alpha = 0.8) +
        labs(title = "Current Yield Curve", x = "Duration", y = "Yield (%)") +
        theme_insele(base_size = 10) +
        theme(plot.margin = ggplot2::margin(5, 5, 5, 5))

    # 2. Top opportunities table
    if(!is.null(signals_df)) {
        top_opps <- signals_df %>%
            filter(signal %in% c("BUY", "STRONG BUY")) %>%
            head(3) %>%
            select(bond, yield_to_maturity, composite_score, signal)

        p_table <- tableGrob(top_opps, rows = NULL,
                             theme = ttheme_minimal(
                                 base_size = 10,
                                 core = list(fg_params = list(col = insele_colors$dark_navy)),
                                 colhead = list(fg_params = list(col = insele_colors$navy,
                                                                 fontface = "bold"))
                             ))
    }

    # 3. Risk radar for top 6 bonds
    p_risk <- df %>%
        filter(date == latest_date) %>%
        head(6) %>%
        mutate(
            yield_rank = rank(yield_to_maturity) / n(),
            duration_rank = rank(modified_duration) / n(),
            liquidity_rank = rank(runif(n())) / n()  # Placeholder
        ) %>%
        select(bond, yield_rank, duration_rank, liquidity_rank) %>%
        pivot_longer(cols = -bond, names_to = "metric", values_to = "value") %>%
        ggplot(aes(x = metric, y = value, group = bond, color = bond)) +
        geom_line(alpha = 0.5) +
        geom_point(size = 2) +
        coord_polar() +
        labs(title = "Risk Profile") +
        theme_insele(base_size = 10) +
        theme(legend.position = "none")

    # 4. Auction calendar preview
    p_calendar <- df %>%
        filter(!is.na(bid_to_cover)) %>%
        mutate(week = lubridate::week(date)) %>%
        filter(week >= lubridate::week(Sys.Date()) - 4) %>%
        ggplot(aes(x = date, y = bond, fill = bid_to_cover)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = insele_colors$red, mid = insele_colors$yellow,
                             high = insele_colors$green, midpoint = 3) +
        labs(title = "Recent Auctions", x = "", y = "") +
        theme_insele(base_size = 10) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")

    # Combine all plots
    if(!is.null(signals_df)) {
        grid.arrange(
            p_yield, p_table,
            p_risk, p_calendar,
            ncol = 2, nrow = 2,
            top = textGrob("Executive Dashboard - Bond Auction Analysis",
                           gp = gpar(fontsize = 16, col = insele_colors$navy,
                                     fontface = "bold")),
            bottom = textGrob(paste("Insele Capital Partners |", format(Sys.Date(), "%B %d, %Y")),
                              gp = gpar(fontsize = 10, col = insele_colors$light_teal))
        )
    }
}

# ================== EXPORT FUNCTIONS ==================

#' Save plot with Insele branding
save_insele_plot <- function(plot, filename, width = 12, height = 8, dpi = 300) {
    ggsave(
        filename = filename,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        bg = "white"
    )

    message(paste("Plot saved to:", filename))
}