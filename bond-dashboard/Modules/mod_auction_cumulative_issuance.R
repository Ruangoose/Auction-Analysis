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

    # Create gradient colors: light green to dark green based on issuance amount
    # Using a green gradient for financial/positive growth visualization
    color_low <- "#90EE90"      # Light green
    color_mid <- "#28a745"      # Medium green (insele_palette$success)
    color_high <- "#006400"     # Dark green

    # Create the bar chart
    p <- ggplot(issuance_summary, aes(x = reorder(bond_label, total_issuance_mil),
                                      y = total_issuance_mil)) +

        # Bars with fill based on issuance amount (gradient)
        geom_col(aes(fill = total_issuance_mil),
                 alpha = 0.9,
                 width = 0.7) +

        # Add value labels on bars
        geom_text(aes(label = paste0("R", format(round(total_issuance_mil, 0), big.mark = ","), "m")),
                  hjust = -0.1,
                  size = 3.5,
                  color = insele_palette$text_primary,
                  fontface = "bold") +

        # Color scale: green gradient based on issuance amount
        scale_fill_gradient(
            low = color_low,
            high = color_high,
            name = "Issuance Scale\n(R millions)",
            labels = function(x) paste0("R", format(round(x, 0), big.mark = ",")),
            guide = guide_colorbar(
                title.position = "top",
                title.hjust = 0.5,
                barwidth = 15,
                barheight = 1.5,
                frame.colour = insele_palette$border,
                ticks.colour = insele_palette$border
            )
        ) +

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
            legend.position = "bottom",
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 9),
            plot.margin = ggplot2::margin(15, 15, 15, 15)
        )

    return(p)
}

#' @title Generate Cumulative Issuance Data Table
#' @description Creates a formatted data table with cumulative issuance statistics for the selected date range
#' @param data Filtered bond auction data (respects date range from analysis parameters)
#' @return Data frame for display
#' @export
generate_ytd_issuance_table <- function(data) {
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
    # Using available columns: offer_amount, allocation, bids/bids_received, bid_to_cover, yield_to_maturity
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

    # Calculate yield trend WITHIN THE SELECTED PERIOD ONLY
    # Compare first vs last auction yield for each bond within the filtered date range
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

    # Calculate issuance statistics with meaningful metrics from available data
    # CRITICAL FIX: Filter out invalid bid_to_cover values (NA, 0, negative) before min/max
    issuance_table <- data %>%
        filter(!is.na(offer_amount), offer_amount > 0) %>%
        group_by(bond) %>%
        summarise(
            n_auctions = n(),
            total_offered = sum(offer_amount, na.rm = TRUE) / 1e6,  # Convert to millions
            total_bids = sum(bids, na.rm = TRUE) / 1e6,
            avg_b2c = mean(bid_to_cover[!is.na(bid_to_cover) & bid_to_cover > 0], na.rm = TRUE),
            # FIX: Only use valid B2C values (> 0) to avoid 0.00x in range
            min_b2c = {
                valid_b2c <- bid_to_cover[!is.na(bid_to_cover) & bid_to_cover > 0]
                if (length(valid_b2c) > 0) min(valid_b2c) else NA_real_
            },
            max_b2c = {
                valid_b2c <- bid_to_cover[!is.na(bid_to_cover) & bid_to_cover > 0]
                if (length(valid_b2c) > 0) max(valid_b2c) else NA_real_
            },
            # Oversubscription: average demand vs supply
            avg_oversubscription = mean((bids - offer_amount) / offer_amount * 100, na.rm = TRUE),
            first_auction = min(date, na.rm = TRUE),
            last_auction = max(date, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        left_join(yield_trends, by = "bond") %>%
        arrange(desc(total_offered))

    # Create TOTAL row with proper aggregations
    if (nrow(issuance_table) > 0) {
        # Calculate overall market yield trend for total row
        # Weighted average of individual bond trends
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
            first_auction = as.Date(NA),
            last_auction = as.Date(NA),
            first_yield = NA_real_,
            last_yield = NA_real_,
            yield_change_bps = overall_yield_trend,
            n_auctions_for_trend = NA_integer_,
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

    # Format for display with proper column names
    # REMOVED: Alloc Rate % column (always ~100%, provides no analytical value)
    display_table <- issuance_table %>%
        transmute(
            Bond = bond,
            `# Auctions` = n_auctions,
            # Keep Total (R mil) as numeric for DT formatting (will add thousand separators in DT)
            `Total (R mil)` = round(total_offered, 0),
            # Keep Avg B2C as numeric for conditional formatting
            `Avg B2C` = round(avg_b2c, 2),
            # B2C Range: shows min-max spread - FIXED: now properly handles NA/0 values
            `B2C Range` = case_when(
                is.na(min_b2c) | is.na(max_b2c) ~ "—",
                is.infinite(min_b2c) | is.infinite(max_b2c) ~ "—",
                TRUE ~ paste0(sprintf("%.2f", min_b2c), "x - ", sprintf("%.2f", max_b2c), "x")
            ),
            # Oversubscription %
            `Oversub %` = round(avg_oversubscription, 1),
            # Yield Trend in bps - keep numeric for conditional formatting
            # The numeric value will be formatted in the DT rendering
            `Yield Trend` = round(yield_change_bps, 1),
            # Also store a formatted version for display with +/- prefix
            `Yield Trend Display` = case_when(
                is.na(yield_change_bps) | is.nan(yield_change_bps) ~ "—",
                yield_change_bps >= 0 ~ paste0("+", sprintf("%.1f", yield_change_bps), " bps"),
                TRUE ~ paste0(sprintf("%.1f", yield_change_bps), " bps")
            ),
            `First Auction` = ifelse(
                is.na(first_auction),
                "—",
                format(first_auction, "%Y-%m-%d")
            ),
            `Last Auction` = ifelse(
                is.na(last_auction),
                "—",
                format(last_auction, "%Y-%m-%d")
            )
        )

    return(display_table)
}