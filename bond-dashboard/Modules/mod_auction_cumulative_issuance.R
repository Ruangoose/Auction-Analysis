#' @title Cumulative Government Bond Issuance Module
#' @description Visualization module for cumulative bond issuance based on selected date range from analysis parameters
#' @author Insele Capital Partners

#' @title Check for Issuance Data Availability
#' @description Helper function to check if offer amount data exists
#' @keywords internal
check_issuance_data <- function(data, required_cols = c()) {
    # Core issuance columns that should exist
    core_cols <- c("offer_amount", "bond")

    # Combine with any additional required columns
    all_required <- unique(c(core_cols, required_cols))

    # Check which columns are missing
    missing <- setdiff(all_required, names(data))

    if (length(missing) > 0) {
        return(list(
            has_data = FALSE,
            missing_cols = missing,
            message = paste("Issuance data columns missing:", paste(missing, collapse = ", "))
        ))
    }

    # Check if there's any actual issuance data
    if (!"offer_amount" %in% names(data) || all(is.na(data$offer_amount))) {
        return(list(
            has_data = FALSE,
            missing_cols = NULL,
            message = "No issuance data available (all offer_amount values are NA)"
        ))
    }

    # Check for at least one valid offer amount > 0
    if (all(data$offer_amount <= 0, na.rm = TRUE)) {
        return(list(
            has_data = FALSE,
            missing_cols = NULL,
            message = "No valid issuance data available (all offer amounts are zero or negative)"
        ))
    }

    return(list(has_data = TRUE, missing_cols = NULL, message = NULL))
}

#' @title Create "No Data" Plot for Issuance
#' @description Returns an informative plot when issuance data is unavailable
#' @keywords internal
create_no_issuance_data_plot <- function(message = "Issuance data not available") {
    ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = message,
                 size = 6, color = "#666666", hjust = 0.5, vjust = 0.5) +
        annotate("text", x = 0.5, y = 0.4,
                 label = "Please ensure your data includes: bond, date, offer_amount",
                 size = 4, color = "#999999", hjust = 0.5, vjust = 0.5) +
        annotate("text", x = 0.5, y = 0.3,
                 label = "And that the selected date range contains auction data",
                 size = 4, color = "#999999", hjust = 0.5, vjust = 0.5) +
        theme_void() +
        theme(plot.background = element_rect(fill = "#f8f9fa", color = NA))
}

#' @title Generate Cumulative Government Bond Issuance Chart
#' @description Creates a bar chart showing cumulative bond issuance by bond code for the selected date range
#' @param data Filtered bond auction data (respects date range from analysis parameters)
#' @param params Optional parameters list
#' @return ggplot object
#' @export
generate_ytd_bond_issuance_chart <- function(data, params = list()) {
    # CRITICAL FIX: Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Check for issuance data (silently - user sees message in plot)
    data_check <- check_issuance_data(data, required_cols = c("date"))
    if (!data_check$has_data) {
        return(create_no_issuance_data_plot(data_check$message))
    }

    # Calculate cumulative issuance per bond
    issuance_summary <- data %>%
        filter(!is.na(offer_amount), offer_amount > 0) %>%
        group_by(bond) %>%
        summarise(
            total_issuance = sum(offer_amount, na.rm = TRUE),
            n_auctions = n(),
            min_date = min(date, na.rm = TRUE),
            max_date = max(date, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        arrange(desc(total_issuance))

    # Check if we have any data after filtering
    if (nrow(issuance_summary) == 0) {
        return(create_no_issuance_data_plot("No bonds with valid offer amounts in selected date range"))
    }

    # Convert to millions for display
    issuance_summary <- issuance_summary %>%
        mutate(
            total_issuance_mil = total_issuance / 1e6,
            # Add bond label with auction count
            bond_label = paste0(bond, " (n=", n_auctions, ")")
        )

    # Determine date range for subtitle
    date_range_start <- min(issuance_summary$min_date, na.rm = TRUE)
    date_range_end <- max(issuance_summary$max_date, na.rm = TRUE)

    # Calculate total issuance for subtitle
    total_issuance_all <- sum(issuance_summary$total_issuance, na.rm = TRUE) / 1e9  # in billions

    # Calculate percentage of total for each bond
    grand_total <- sum(issuance_summary$total_issuance, na.rm = TRUE)
    issuance_summary <- issuance_summary %>%
        mutate(pct_of_total = total_issuance / grand_total * 100)

    # Create the bar chart with flat Insele navy fill
    p <- ggplot(issuance_summary, aes(x = reorder(bond_label, total_issuance_mil),
                                      y = total_issuance_mil)) +

        # Bars with flat Insele navy fill
        geom_col(fill = "#1B3A6B",
                 alpha = 0.9,
                 width = 0.7) +

        # Add value labels on bars with percentage
        geom_text(aes(label = paste0("R", format(round(total_issuance_mil, 0), big.mark = ","),
                                     "m (", round(pct_of_total, 1), "%)")),
                  hjust = -0.05,
                  size = 3.2,
                  color = insele_palette$text_primary,
                  fontface = "bold") +

        # Y-axis: format in millions
        scale_y_continuous(
            labels = function(x) paste0(format(x, big.mark = ","), "m"),
            expand = expansion(mult = c(0, 0.15))  # Add space for labels
        ) +

        # Flip coordinates for horizontal bars (better readability)
        coord_flip() +

        # Labels
        labs(
            title = "Cumulative Government Bond Issuance",
            subtitle = sprintf(
                "Cumulative offer amounts by bond | Period: %s to %s | Total: R%.2f bn",
                format(date_range_start, "%b %d, %Y"),
                format(date_range_end, "%b %d, %Y"),
                total_issuance_all
            ),
            x = NULL,  # Remove x-axis label (bond names are self-explanatory)
            y = "Total Issuance (R millions)",
            caption = paste("Source: Insele Capital Partners Bond Analytics | Generated:",
                            format(Sys.Date(), "%b %d, %Y"))
        ) +

        # Apply Insele theme
        create_insele_theme() +

        # Additional theme customizations
        theme(
            panel.grid.major.y = element_blank(),  # Remove vertical gridlines
            panel.grid.major.x = element_line(color = insele_palette$light_gray,
                                              linewidth = 0.5,
                                              linetype = "dashed"),
            axis.text.y = element_text(size = 10, face = "bold",
                                       color = insele_palette$text_primary),
            axis.text.x = element_text(size = 9, color = insele_palette$text_secondary),
            plot.title = element_text(size = 16, face = "bold",
                                      color = insele_palette$primary,
                                      margin = ggplot2::margin(b = 5)),
            plot.subtitle = element_text(size = 11,
                                         color = insele_palette$text_secondary,
                                         margin = ggplot2::margin(b = 15)),
            plot.caption = element_text(size = 8,
                                        color = insele_palette$text_secondary,
                                        hjust = 1,
                                        margin = ggplot2::margin(t = 10)),
            legend.position = "none",
            plot.margin = ggplot2::margin(15, 15, 15, 15)
        )

    return(p)
}

#' @export
#' @title YTD vs Previous Year Issuance Comparison Chart
#' @description Horizontal grouped bar chart comparing cumulative bond issuance
#'   for current YTD period vs the equivalent period in the prior year
#' @param data Full unfiltered bond auction data
#' @param params Optional parameters
#' @return ggplot object
generate_ytd_vs_prev_year_issuance_chart <- function(data, params = list()) {
    tryCatch({
        # Ensure date columns are Date objects
        data <- ensure_date_columns(data)

        # Validate issuance data
        data_check <- check_issuance_data(data, required_cols = c("date"))
        if (!data_check$has_data) {
            return(create_no_issuance_data_plot(data_check$message))
        }

        # Determine the latest data date
        max_date <- max(data$date[!is.na(data$offer_amount) & data$offer_amount > 0], na.rm = TRUE)

        # Define periods
        current_year <- lubridate::year(max_date)
        current_ytd_start <- as.Date(paste0(current_year, "-01-01"))
        prev_year_start <- as.Date(paste0(current_year - 1, "-01-01"))
        prev_year_end <- max_date - lubridate::years(1)

        # Current YTD data
        current_data <- data %>%
            dplyr::filter(!is.na(offer_amount), offer_amount > 0,
                          date >= current_ytd_start, date <= max_date) %>%
            dplyr::group_by(bond) %>%
            dplyr::summarise(
                total_issuance = sum(offer_amount, na.rm = TRUE),
                n_auctions = dplyr::n(),
                .groups = "drop"
            ) %>%
            dplyr::mutate(period = paste0("Current (", current_year, ")"))

        # Previous year equivalent period data
        prev_data <- data %>%
            dplyr::filter(!is.na(offer_amount), offer_amount > 0,
                          date >= prev_year_start, date <= prev_year_end) %>%
            dplyr::group_by(bond) %>%
            dplyr::summarise(
                total_issuance = sum(offer_amount, na.rm = TRUE),
                n_auctions = dplyr::n(),
                .groups = "drop"
            ) %>%
            dplyr::mutate(period = paste0("Previous (", current_year - 1, ")"))

        # Combine and handle bonds in either period
        combined <- dplyr::bind_rows(current_data, prev_data)

        if (nrow(combined) == 0) {
            return(create_no_issuance_data_plot("No issuance data available for comparison periods"))
        }

        # Convert to millions
        combined <- combined %>%
            dplyr::mutate(total_issuance_mil = total_issuance / 1e6)

        # Order bonds by current YTD total issuance (descending)
        bond_order <- current_data %>%
            dplyr::arrange(dplyr::desc(total_issuance)) %>%
            dplyr::pull(bond)
        # Add any bonds only in previous period
        prev_only <- setdiff(prev_data$bond, current_data$bond)
        bond_order <- c(bond_order, prev_only)

        combined <- combined %>%
            dplyr::mutate(bond = factor(bond, levels = rev(bond_order)))

        # Define period labels and colors
        current_label <- paste0("Current (", current_year, ")")
        prev_label <- paste0("Previous (", current_year - 1, ")")
        period_colors <- c("#1B3A6B", "#90A4AE")
        names(period_colors) <- c(current_label, prev_label)

        combined$period <- factor(combined$period, levels = c(current_label, prev_label))

        # Build subtitle
        subtitle_text <- sprintf(
            "Current: Jan 1, %d to %s  |  Previous: Jan 1, %d to %s",
            current_year, format(max_date, "%b %d, %Y"),
            current_year - 1, format(prev_year_end, "%b %d, %Y")
        )

        # Create the grouped bar chart
        p <- ggplot(combined, aes(x = bond, y = total_issuance_mil, fill = period)) +
            geom_col(position = position_dodge(width = 0.8),
                     width = 0.7, alpha = 0.9) +

            # Value labels on bars
            geom_text(aes(label = paste0("R", format(round(total_issuance_mil, 0), big.mark = ","), "m")),
                      position = position_dodge(width = 0.8),
                      hjust = -0.05, size = 3, fontface = "bold",
                      color = insele_palette$text_primary) +

            scale_fill_manual(values = period_colors,
                              name = NULL) +

            scale_y_continuous(
                labels = function(x) paste0("R", format(x, big.mark = ","), "m"),
                expand = expansion(mult = c(0, 0.20))
            ) +

            coord_flip() +

            labs(
                title = "Cumulative Government Bond Issuance \u2014 YTD Comparison",
                subtitle = subtitle_text,
                x = NULL,
                y = "Total Issuance (R millions)",
                caption = paste("Source: Insele Capital Partners Bond Analytics | Generated:",
                                format(Sys.Date(), "%b %d, %Y"))
            ) +

            theme_minimal() +

            theme(
                panel.grid.major.y = element_blank(),
                panel.grid.major.x = element_line(color = "#E0E0E0",
                                                  linewidth = 0.5,
                                                  linetype = "dashed"),
                axis.text.y = element_text(size = 10, face = "bold",
                                           color = insele_palette$text_primary),
                axis.text.x = element_text(size = 9, color = insele_palette$text_secondary),
                plot.title = element_text(size = 16, face = "bold",
                                          color = "#1B3A6B",
                                          margin = ggplot2::margin(b = 5)),
                plot.subtitle = element_text(size = 11,
                                             color = insele_palette$text_secondary,
                                             margin = ggplot2::margin(b = 15)),
                plot.caption = element_text(size = 8,
                                            color = insele_palette$text_secondary,
                                            hjust = 1,
                                            margin = ggplot2::margin(t = 10)),
                legend.position = "bottom",
                legend.text = element_text(size = 10),
                plot.margin = ggplot2::margin(15, 15, 15, 15)
            )

        return(p)

    }, error = function(e) {
        message(sprintf("[YTD vs Prev Year Issuance] Error: %s", e$message))
        return(create_no_issuance_data_plot(
            paste("Issuance comparison chart error:", e$message)
        ))
    })
}

#' @title Generate Cumulative Issuance Data Table
#' @description Creates a formatted data table with cumulative issuance statistics for the selected date range.
#'              First/Last Auction columns show HISTORICAL dates (from full_data), independent of the date filter.
#' @param data Filtered bond auction data (respects date range from analysis parameters)
#' @param full_data Optional: Full unfiltered bond auction data for calculating historical first/last auction dates.
#'                  If NULL, falls back to using 'data' for these dates.
#' @return Data frame for display
#' @export
generate_ytd_issuance_table <- function(data, full_data = NULL) {
    # Ensure date columns are Date objects
    data <- ensure_date_columns(data)

    # Check for required data
    if (is.null(data) || nrow(data) == 0) {
        return(data.frame(
            Message = "No data available for the selected date range"
        ))
    }

    # Check for required columns
    if (!all(c("bond", "offer_amount") %in% names(data))) {
        return(data.frame(
            Message = "Required columns (bond, offer_amount) not found in data"
        ))
    }

    # Define numeric columns that need conversion
    numeric_cols <- c("offer_amount", "allocation", "bids", "bids_received",
                      "bid_to_cover", "yield_to_maturity")

    # Ensure numeric columns are actually numeric BEFORE summarise
    data <- data %>%
        mutate(across(any_of(numeric_cols),
                      ~ suppressWarnings(as.numeric(as.character(.x)))))

    # Handle bids column (might be named 'bids' or 'bids_received')
    if (!"bids" %in% names(data) && "bids_received" %in% names(data)) {
        data <- data %>% mutate(bids = bids_received)
    }

    # ============================================================
    # STEP 1: Calculate HISTORICAL first/last auction from FULL data
    # These are independent of the date filter
    # ============================================================
    if (!is.null(full_data) && nrow(full_data) > 0) {
        full_data <- ensure_date_columns(full_data)
        full_data <- full_data %>%
            mutate(across(any_of(numeric_cols),
                          ~ suppressWarnings(as.numeric(as.character(.x)))))

        historical_auction_dates <- full_data %>%
            filter(
                !is.na(date),
                !is.na(offer_amount),
                offer_amount > 0
            ) %>%
            group_by(bond) %>%
            summarise(
                first_auction_historical = min(date, na.rm = TRUE),
                last_auction_historical = max(date, na.rm = TRUE),
                total_historical_auctions = n(),
                .groups = "drop"
            )
    } else {
        # Fallback: use filtered data if full_data not provided (backward compatibility)
        historical_auction_dates <- data %>%
            filter(
                !is.na(date),
                !is.na(offer_amount),
                offer_amount > 0
            ) %>%
            group_by(bond) %>%
            summarise(
                first_auction_historical = min(date, na.rm = TRUE),
                last_auction_historical = max(date, na.rm = TRUE),
                total_historical_auctions = n(),
                .groups = "drop"
            )
    }

    # ============================================================
    # STEP 2: Calculate PERFORMANCE METRICS from FILTERED data
    # These respect the date range selection
    # ============================================================

    # Calculate yield trend WITHIN THE SELECTED PERIOD ONLY
    yield_trends <- data %>%
        filter(!is.na(offer_amount), offer_amount > 0, !is.na(yield_to_maturity)) %>%
        group_by(bond) %>%
        arrange(date) %>%
        filter(n() >= 2) %>%  # Need at least 2 auctions to calculate meaningful trend
        summarise(
            first_yield = first(yield_to_maturity),
            last_yield = last(yield_to_maturity),
            yield_change_bps = (last_yield - first_yield) * 100,  # Convert to bps
            n_auctions_for_trend = n(),
            .groups = "drop"
        )

    # Calculate issuance statistics from FILTERED data
    issuance_table <- data %>%
        filter(!is.na(offer_amount), offer_amount > 0) %>%
        group_by(bond) %>%
        summarise(
            n_auctions = n(),
            total_offered = sum(offer_amount, na.rm = TRUE) / 1e6,  # Convert to millions
            total_bids = sum(bids, na.rm = TRUE) / 1e6,
            avg_b2c = mean(bid_to_cover[!is.na(bid_to_cover) & bid_to_cover > 0], na.rm = TRUE),
            min_b2c = {
                valid_b2c <- bid_to_cover[!is.na(bid_to_cover) & bid_to_cover > 0]
                if (length(valid_b2c) > 0) min(valid_b2c) else NA_real_
            },
            max_b2c = {
                valid_b2c <- bid_to_cover[!is.na(bid_to_cover) & bid_to_cover > 0]
                if (length(valid_b2c) > 0) max(valid_b2c) else NA_real_
            },
            avg_oversubscription = mean((bids - offer_amount) / offer_amount * 100, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        left_join(yield_trends, by = "bond") %>%
        # JOIN THE HISTORICAL DATES (from full data, not filtered)
        left_join(historical_auction_dates, by = "bond") %>%
        arrange(desc(total_offered))

    # ============================================================
    # STEP 3: Create TOTAL row with proper aggregations
    # ============================================================
    if (nrow(issuance_table) > 0) {
        # Calculate overall market yield trend for total row
        overall_yield_trend <- if (any(!is.na(issuance_table$yield_change_bps))) {
            weighted.mean(
                issuance_table$yield_change_bps,
                issuance_table$n_auctions,
                na.rm = TRUE
            )
        } else {
            NA_real_
        }

        # Get valid B2C values across all bonds for total row
        valid_min_b2c <- issuance_table$min_b2c[!is.na(issuance_table$min_b2c) & !is.infinite(issuance_table$min_b2c)]
        valid_max_b2c <- issuance_table$max_b2c[!is.na(issuance_table$max_b2c) & !is.infinite(issuance_table$max_b2c)]

        totals_row <- data.frame(
            bond = "TOTAL",
            n_auctions = sum(issuance_table$n_auctions, na.rm = TRUE),
            total_offered = sum(issuance_table$total_offered, na.rm = TRUE),
            total_bids = sum(issuance_table$total_bids, na.rm = TRUE),
            avg_b2c = weighted.mean(issuance_table$avg_b2c, issuance_table$n_auctions, na.rm = TRUE),
            min_b2c = if (length(valid_min_b2c) > 0) min(valid_min_b2c) else NA_real_,
            max_b2c = if (length(valid_max_b2c) > 0) max(valid_max_b2c) else NA_real_,
            avg_oversubscription = weighted.mean(issuance_table$avg_oversubscription,
                                                  issuance_table$n_auctions, na.rm = TRUE),
            first_yield = NA_real_,
            last_yield = NA_real_,
            yield_change_bps = overall_yield_trend,
            n_auctions_for_trend = NA_integer_,
            # TOTAL row: show overall earliest and latest auction across ALL bonds (historical)
            first_auction_historical = min(issuance_table$first_auction_historical, na.rm = TRUE),
            last_auction_historical = max(issuance_table$last_auction_historical, na.rm = TRUE),
            total_historical_auctions = sum(historical_auction_dates$total_historical_auctions, na.rm = TRUE),
            stringsAsFactors = FALSE
        )

        # Prepend TOTAL row at the top
        issuance_table <- bind_rows(totals_row, issuance_table)
    }

    # Handle bonds with only 1 auction (can't calculate meaningful trend)
    issuance_table <- issuance_table %>%
        mutate(
            yield_change_bps = ifelse(is.na(yield_change_bps) | is.infinite(yield_change_bps),
                                       NA_real_,
                                       yield_change_bps)
        )

    # ============================================================
    # STEP 4: Format for display
    # ============================================================
    display_table <- issuance_table %>%
        transmute(
            Bond = bond,
            `# Auctions` = n_auctions,
            `Total (R mil)` = round(total_offered, 0),
            `Avg B2C` = round(avg_b2c, 2),
            `B2C Range` = case_when(
                is.na(min_b2c) | is.na(max_b2c) ~ "—",
                is.infinite(min_b2c) | is.infinite(max_b2c) ~ "—",
                TRUE ~ paste0(sprintf("%.2f", min_b2c), "x - ", sprintf("%.2f", max_b2c), "x")
            ),
            `Oversub %` = round(avg_oversubscription, 1),
            `Yield Trend` = round(yield_change_bps, 1),
            `Yield Trend Display` = case_when(
                is.na(yield_change_bps) | is.nan(yield_change_bps) ~ "—",
                yield_change_bps >= 0 ~ paste0("+", sprintf("%.1f", yield_change_bps), " bps"),
                TRUE ~ paste0(sprintf("%.1f", yield_change_bps), " bps")
            ),
            # USE HISTORICAL DATES - not affected by date filter
            `First Auction` = ifelse(
                is.na(first_auction_historical),
                "—",
                format(first_auction_historical, "%Y-%m-%d")
            ),
            `Last Auction` = ifelse(
                is.na(last_auction_historical),
                "—",
                format(last_auction_historical, "%Y-%m-%d")
            )
        )

    return(display_table)
}

#' @export
#' @title Dual Cumulative Issuance Charts (YTD + Last 12 Months)
#' @description Creates two stacked horizontal bar charts for the PDF report:
#'   Top: Year-to-date issuance. Bottom: Last 12 months issuance.
#'   Date reference is always Sys.Date() (today), NOT data max date.
#' @param data Full unfiltered bond auction data
#' @return A grob (gridExtra::arrangeGrob) combining both charts
generate_dual_issuance_charts <- function(data) {
    tryCatch({
        require(gridExtra)

        data <- ensure_date_columns(data)

        # Date logic — CRITICAL: Use Sys.Date(), NOT max(data$date)
        today <- Sys.Date()
        current_year <- lubridate::year(today)

        # YTD period
        ytd_start <- as.Date(paste0(current_year, "-01-01"))
        ytd_end <- today

        # Last 12 months period
        l12m_start <- today - lubridate::years(1)
        l12m_end <- today

        # Helper to build a horizontal bar chart for a given period
        build_issuance_chart <- function(period_data, title, subtitle, no_data_msg) {
            if (is.null(period_data) || nrow(period_data) == 0) {
                # Return a clean placeholder plot
                return(
                    ggplot() +
                        annotate("text", x = 0.5, y = 0.5,
                                 label = no_data_msg,
                                 size = 5, color = "#666666", hjust = 0.5, vjust = 0.5) +
                        labs(title = title, subtitle = subtitle) +
                        theme_void() +
                        theme(
                            plot.title = element_text(size = 13, face = "bold",
                                                      color = "#1B3A6B",
                                                      margin = ggplot2::margin(b = 5)),
                            plot.subtitle = element_text(size = 10,
                                                         color = "grey50",
                                                         margin = ggplot2::margin(b = 10)),
                            plot.background = element_rect(fill = "white", color = NA),
                            plot.margin = ggplot2::margin(10, 10, 10, 10)
                        )
                )
            }

            # Summarize by bond
            summary_data <- period_data %>%
                dplyr::filter(!is.na(offer_amount), offer_amount > 0) %>%
                dplyr::group_by(bond) %>%
                dplyr::summarise(
                    total_issuance = sum(offer_amount, na.rm = TRUE),
                    n_auctions = dplyr::n(),
                    .groups = "drop"
                ) %>%
                dplyr::arrange(dplyr::desc(total_issuance)) %>%
                dplyr::mutate(
                    total_issuance_mil = total_issuance / 1e6,
                    bond_label = paste0(bond, " (n=", n_auctions, ")")
                )

            if (nrow(summary_data) == 0) {
                return(
                    ggplot() +
                        annotate("text", x = 0.5, y = 0.5,
                                 label = no_data_msg,
                                 size = 5, color = "#666666", hjust = 0.5, vjust = 0.5) +
                        labs(title = title, subtitle = subtitle) +
                        theme_void() +
                        theme(
                            plot.title = element_text(size = 13, face = "bold",
                                                      color = "#1B3A6B",
                                                      margin = ggplot2::margin(b = 5)),
                            plot.subtitle = element_text(size = 10,
                                                         color = "grey50",
                                                         margin = ggplot2::margin(b = 10)),
                            plot.background = element_rect(fill = "white", color = NA),
                            plot.margin = ggplot2::margin(10, 10, 10, 10)
                        )
                )
            }

            # Total for caption
            total_bn <- sum(summary_data$total_issuance, na.rm = TRUE) / 1e9

            ggplot(summary_data, aes(x = reorder(bond_label, total_issuance_mil),
                                     y = total_issuance_mil)) +
                geom_col(fill = "#1B3A6B", alpha = 0.9, width = 0.7) +
                geom_text(aes(label = paste0("R", format(round(total_issuance_mil, 0),
                                                          big.mark = ","), "m")),
                          hjust = -0.05, size = 3, fontface = "bold",
                          color = "#333333") +
                scale_y_continuous(
                    labels = function(x) paste0("R", format(x, big.mark = ","), "m"),
                    expand = expansion(mult = c(0, 0.18))
                ) +
                coord_flip() +
                labs(
                    title = title,
                    subtitle = paste0(subtitle, "  |  Total: R", sprintf("%.2f", total_bn), " bn"),
                    x = NULL,
                    y = "Total Issuance (R millions)"
                ) +
                theme_minimal(base_size = 9) +
                theme(
                    panel.grid.major.y = element_blank(),
                    panel.grid.major.x = element_line(color = "#E0E0E0",
                                                      linewidth = 0.5,
                                                      linetype = "dashed"),
                    panel.grid.minor = element_blank(),
                    axis.text.y = element_text(size = 9, face = "bold", color = "#333333"),
                    axis.text.x = element_text(size = 8, color = "#666666"),
                    plot.title = element_text(size = 13, face = "bold",
                                              color = "#1B3A6B",
                                              margin = ggplot2::margin(b = 3)),
                    plot.subtitle = element_text(size = 10,
                                                 color = "grey50",
                                                 margin = ggplot2::margin(b = 10)),
                    legend.position = "none",
                    plot.background = element_rect(fill = "white", color = NA),
                    plot.margin = ggplot2::margin(10, 15, 5, 10)
                )
        }

        # --- YTD chart (top) ---
        ytd_data <- data %>%
            dplyr::filter(date >= ytd_start, date <= ytd_end)

        ytd_chart <- build_issuance_chart(
            ytd_data,
            title = "Year-to-Date Government Bond Issuance",
            subtitle = paste0(format(ytd_start, "%b %d, %Y"), " to ", format(ytd_end, "%b %d, %Y")),
            no_data_msg = paste0("No auction data for YTD ", current_year, " period")
        )

        # --- Last 12 months chart (bottom) ---
        l12m_data <- data %>%
            dplyr::filter(date >= l12m_start, date <= l12m_end)

        l12m_chart <- build_issuance_chart(
            l12m_data,
            title = "Last 12 Months Government Bond Issuance",
            subtitle = paste0(format(l12m_start, "%b %d, %Y"), " to ", format(l12m_end, "%b %d, %Y")),
            no_data_msg = "No auction data for last 12 months period"
        )

        # Combine both charts
        combined <- gridExtra::arrangeGrob(
            ytd_chart,   # Top
            l12m_chart,  # Bottom
            ncol = 1,
            heights = c(1, 1)
        )

        return(combined)

    }, error = function(e) {
        message(sprintf("[Dual Issuance Charts] Error: %s", e$message))
        return(NULL)
    })
}