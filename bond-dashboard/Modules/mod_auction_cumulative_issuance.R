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
    numeric_cols <- c("offer_amount", "bid_to_cover", "auction_quality_score",
                      "auction_tail_bps", "non_comp_ratio", "auction_concession_bps",
                      "number_bids_received")

    # Ensure numeric columns are actually numeric BEFORE summarise
    data <- data %>%
        mutate(across(any_of(numeric_cols),
                      ~ suppressWarnings(as.numeric(as.character(.x)))))

    # Calculate issuance statistics with new quality metrics
    issuance_table <- data %>%
        filter(!is.na(offer_amount), offer_amount > 0) %>%
        group_by(bond) %>%
        summarise(
            `# Auctions` = n(),
            `Total (R mil)` = round(sum(offer_amount, na.rm = TRUE) / 1e6, 2),
            `Avg B2C` = if ("bid_to_cover" %in% names(.))
                round(mean(bid_to_cover, na.rm = TRUE), 2) else NA_real_,
            # NEW: Quality metrics
            `Avg Quality` = if ("auction_quality_score" %in% names(.))
                round(mean(auction_quality_score, na.rm = TRUE), 0) else NA_real_,
            `Avg Tail` = if ("auction_tail_bps" %in% names(.))
                round(mean(auction_tail_bps, na.rm = TRUE), 1) else NA_real_,
            `Inst %` = if ("non_comp_ratio" %in% names(.))
                round(mean(non_comp_ratio, na.rm = TRUE), 0) else NA_real_,
            `Concession` = if ("auction_concession_bps" %in% names(.))
                round(mean(auction_concession_bps, na.rm = TRUE), 1) else NA_real_,
            `# Bidders` = if ("number_bids_received" %in% names(.))
                round(mean(number_bids_received, na.rm = TRUE), 0) else NA_real_,
            `First Auction` = format(min(date, na.rm = TRUE), "%Y-%m-%d"),
            `Last Auction` = format(max(date, na.rm = TRUE), "%Y-%m-%d"),
            .groups = "drop"
        ) %>%
        arrange(desc(`Total (R mil)`))

    # Add row for totals
    if (nrow(issuance_table) > 0) {
        # Calculate weighted averages for totals
        total_auctions <- sum(issuance_table$`# Auctions`, na.rm = TRUE)

        totals_row <- data.frame(
            bond = "TOTAL",
            `# Auctions` = total_auctions,
            `Total (R mil)` = sum(issuance_table$`Total (R mil)`, na.rm = TRUE),
            `Avg B2C` = round(mean(issuance_table$`Avg B2C`, na.rm = TRUE), 2),
            `Avg Quality` = round(mean(issuance_table$`Avg Quality`, na.rm = TRUE), 0),
            `Avg Tail` = round(mean(issuance_table$`Avg Tail`, na.rm = TRUE), 1),
            `Inst %` = round(mean(issuance_table$`Inst %`, na.rm = TRUE), 0),
            `Concession` = round(mean(issuance_table$`Concession`, na.rm = TRUE), 1),
            `# Bidders` = round(mean(issuance_table$`# Bidders`, na.rm = TRUE), 0),
            `First Auction` = "",
            `Last Auction` = "",
            check.names = FALSE
        )

        issuance_table <- rbind(issuance_table, totals_row)
    }

    # Format columns for display
    issuance_table <- issuance_table %>%
        mutate(
            `Avg B2C` = ifelse(is.na(`Avg B2C`), "—", sprintf("%.2fx", `Avg B2C`)),
            `Avg Quality` = ifelse(is.na(`Avg Quality`), "—", as.character(`Avg Quality`)),
            `Avg Tail` = ifelse(is.na(`Avg Tail`), "—", as.character(`Avg Tail`)),
            `Inst %` = ifelse(is.na(`Inst %`), "—", paste0(`Inst %`, "%")),
            `Concession` = ifelse(is.na(`Concession`), "—", sprintf("%+.1f", `Concession`)),
            `# Bidders` = ifelse(is.na(`# Bidders`), "—", as.character(`# Bidders`))
        )

    # Rename bond column for display
    names(issuance_table)[1] <- "Bond"

    return(issuance_table)
}