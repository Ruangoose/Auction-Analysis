# ================================================================================
# INSELE CAPITAL PARTNERS - TREASURY HOLDINGS PLOT GENERATORS
# Professional visualizations for SA Government Bond institutional ownership
# ================================================================================

# Required packages (loaded quietly - main app should already have them)
suppressPackageStartupMessages({
    require(ggplot2, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(tidyr, quietly = TRUE)
    require(lubridate, quietly = TRUE)  # For floor_date, ceiling_date
    require(scales, quietly = TRUE)
    require(zoo, quietly = TRUE)        # For na.locf() robust NA handling
})

# ================================================================================
# INSELE CAPITAL PARTNERS - BRAND COLOR PALETTE
# ================================================================================

# Official Insele palette (extracted from logo)
# NOTE: Named treasury_brand_colors to avoid conflict with global insele_palette
# from theme_config.R which has a different structure
treasury_brand_colors <- list(
    # Primary colors from logo
    navy_dark = "#1B3A6B",      # Primary dark navy (main brand color)
    navy_medium = "#2B4F7F",    # Medium navy blue
    navy_light = "#3D6494",     # Lighter navy

    # Secondary colors from logo
    slate_dark = "#5B7B8A",     # Dark slate/teal gray
    slate_medium = "#7A9BA8",   # Medium slate
    slate_light = "#9AB5C0",    # Light slate

    # Accent color
    orange = "#E8913A",         # Orange accent (from logo highlight)
    orange_light = "#F5A54D",   # Lighter orange

    # Neutral tones
    gray_dark = "#4A5568",      # Dark gray
    gray_medium = "#718096",    # Medium gray
    gray_light = "#A0AEC0",     # Light gray

    # Background/light tones
    cream = "#F7FAFC",          # Off-white background
    white = "#FFFFFF"
)

# ================================================================================
# COLOR PALETTES - INSELE BRAND COLORS
# ================================================================================

# Aggregate holdings (navy/slate/orange) - for historical time series
treasury_aggregate_colors <- c(
    "Banks" = "#1B3A6B",                      # Navy dark (top of stack)
    "Insurers" = "#2B4F7F",                   # Navy medium
    "Local pension funds" = "#5B7B8A",        # Slate dark
    "Non-residents" = "#7A9BA8",              # Slate medium
    "Other financial institutions" = "#9AB5C0", # Slate light
    "Other" = "#E8913A"                       # Orange accent (bottom)
)

# Bond-level holdings - for bond-specific views (more colors needed)
# NOTE: Do NOT include "NA" as a valid sector - filter it out instead
treasury_sector_colors <- c(
    # === STANDARD NAMES (with hyphens, lowercase) ===
    "Foreign sector" = "#E8913A",             # Orange (highlight foreign)
    "Monetary institutions" = "#1B3A6B",      # Navy dark
    "Official pension funds" = "#2B4F7F",     # Navy medium
    "Other financial institutions" = "#5B7B8A", # Slate dark
    "Long-term insurers" = "#7A9BA8",         # Slate medium
    "Private self-administered funds" = "#9AB5C0", # Slate light
    "Other sector" = "#4A5568",               # Gray dark
    "Short-term insurers" = "#718096",        # Gray medium
    "CSDP reporting error" = "#A0AEC0",       # Gray light (filtered but kept for edge cases)

    # === ALIASES FOR AGGREGATE SECTORS ===
    "Non-residents" = "#E8913A",              # Orange (alias)
    "Banks" = "#1B3A6B",                      # Navy dark (alias)
    "Insurers" = "#2B4F7F",                   # Navy medium (alias)
    "Local pension funds" = "#5B7B8A",        # Slate dark (alias)
    "Other" = "#4A5568",                      # Gray dark (alias)

    # === ALTERNATIVE NAMES (without hyphens) - SAME COLORS ===
    # These handle data sources that use inconsistent naming conventions
    "Long term insurers" = "#7A9BA8",         # Same as Long-term insurers
    "Short term insurers" = "#718096",        # Same as Short-term insurers
    "Private self administered funds" = "#9AB5C0", # Same as Private self-administered funds

    # === SINGULAR VS PLURAL VARIATIONS ===
    "Other financial institution" = "#5B7B8A", # Same as Other financial institutions

    # === CASE VARIATIONS ===
    "Official Pension Funds" = "#2B4F7F",     # Same as Official pension funds
    "Monetary Institutions" = "#1B3A6B",      # Same as Monetary institutions
    "Foreign Sector" = "#E8913A",             # Same as Foreign sector
    "Other Sector" = "#4A5568",               # Same as Other sector
    "Other Financial Institutions" = "#5B7B8A", # Same as Other financial institutions
    "Long-Term Insurers" = "#7A9BA8",         # Same as Long-term insurers
    "Short-Term Insurers" = "#718096",        # Same as Short-term insurers
    "Private Self-Administered Funds" = "#9AB5C0" # Same as Private self-administered funds
)

# Change periods (navy gradient - lightest to darkest)
treasury_change_period_colors <- c(
    "1-month" = "#9AB5C0",     # Lightest (slate light)
    "3-month" = "#5B7B8A",     # Medium (slate dark)
    "6-month" = "#2B4F7F",     # Medium-dark (navy medium)
    "12-month" = "#1B3A6B"     # Darkest (navy)
)

# ================================================================================
# 1. STACKED AREA CHART - AGGREGATE HOLDINGS OVER TIME
# ================================================================================

#' Generate Holdings Area Chart
#'
#' Creates a stacked area chart showing institutional ownership percentages over time
#'
#' @param holdings_long Data frame with columns: date, sector, percentage
#' @param date_range Vector of length 2 with start and end dates (optional)
#' @param sectors_selected Character vector of sectors to include (optional)
#' @param palette Insele palette object (optional)
#' @return ggplot2 object
#' @export
generate_holdings_area_chart <- function(holdings_long,
                                         date_range = NULL,
                                         sectors_selected = NULL,
                                         palette = NULL) {

    # Input validation
    if (is.null(holdings_long) || nrow(holdings_long) == 0) {
        return(create_empty_plot("No holdings data available"))
    }

    required_cols <- c("date", "sector", "percentage")
    missing_cols <- setdiff(required_cols, names(holdings_long))
    if (length(missing_cols) > 0) {
        return(create_empty_plot(paste("Missing columns:", paste(missing_cols, collapse = ", "))))
    }

    # Filter by date range if provided
    plot_data <- holdings_long
    if (!is.null(date_range) && length(date_range) == 2) {
        plot_data <- plot_data %>%
            filter(date >= date_range[1], date <= date_range[2])
    }

    # Filter by sectors if provided
    if (!is.null(sectors_selected) && length(sectors_selected) > 0) {
        plot_data <- plot_data %>%
            filter(sector %in% sectors_selected)
    }

    # Order sectors for proper stacking
    # NOTE: With position_stack(reverse = TRUE), the LAST sector in this list
    # will appear at the BOTTOM of the stack. We put "Other" last so its small
    # values (0.12% - 3.46%) render at the bottom where gaps are less noticeable.
    sector_order <- c("Banks", "Insurers", "Local pension funds",
                      "Non-residents", "Other financial institutions", "Other")

    # First, clean the data by removing NA values and invalid sectors
    # BUG FIX: Filter out NA sectors and TOTAL rows that may have slipped through
    clean_data <- plot_data %>%
        # Trim whitespace from sector to catch edge cases
        mutate(sector = trimws(as.character(sector))) %>%
        filter(
            !is.na(percentage),
            !is.na(date),
            !is.na(sector),
            sector != "",
            nchar(sector) > 0,
            # Remove string "NA" and TOTAL rows
            !grepl("^\\s*NA\\s*$", sector, ignore.case = TRUE),
            !grepl("TOTAL", sector, ignore.case = TRUE)
        )

    if (nrow(clean_data) == 0) {
        return(create_empty_plot("No data available for selected filters"))
    }

    # ===========================================
    # SIMPLIFIED DATA PROCESSING (FIX FOR "Other" SEGMENT GAPS)
    # The key insight: Working test configs do NOT use complex interpolation.
    # They simply use tidyr::complete() with fill = 0.
    # ===========================================

    # Simple, clean data preparation - NO complex interpolation with approx() or na.locf()
    plot_data <- clean_data %>%
        filter(sector %in% sector_order) %>%
        mutate(
            sector = factor(sector, levels = sector_order),
            # Convert decimal percentages (0.25) to display percentages (25)
            percentage_display = percentage * 100
        ) %>%
        arrange(date, sector)

    # Fill any gaps with zeros - THIS IS THE KEY FIX
    # tidyr::complete() ensures every date-sector combination exists
    plot_data <- plot_data %>%
        tidyr::complete(
            date,
            sector,
            fill = list(percentage_display = 0)
        ) %>%
        # Ensure factor ordering is maintained after complete()
        mutate(sector = factor(sector, levels = sector_order)) %>%
        arrange(date, sector)

    # Validate that each date sums to ~100% (data integrity check)
    date_totals <- plot_data %>%
        group_by(date) %>%
        summarise(total = sum(percentage_display, na.rm = TRUE), .groups = "drop")

    outlier_dates <- date_totals %>% filter(total < 95 | total > 105)
    if (nrow(outlier_dates) > 0 && nrow(outlier_dates) <= 10) {
        warning("Holdings data integrity issue - these dates don't sum to ~100%: ",
                paste(format(outlier_dates$date, "%Y-%m"), sprintf("(%.1f%%)", outlier_dates$total), collapse = ", "))
    } else if (nrow(outlier_dates) > 10) {
        warning(sprintf("Holdings data integrity issue - %d dates don't sum to ~100%% (range: %.1f%% to %.1f%%)",
                        nrow(outlier_dates), min(outlier_dates$total), max(outlier_dates$total)))
    }

    # Get date range for title
    date_min <- min(plot_data$date, na.rm = TRUE)
    date_max <- max(plot_data$date, na.rm = TRUE)

    # Create the stacked area chart
    # CRITICAL: Use group = sector to ensure geom_area connects points within each sector
    # This prevents the "Other" segment from appearing as disconnected rectangles
    # FIX: Use position_stack(reverse = TRUE) so sectors are stacked in reverse order
    # (last factor level at bottom). This places "Other" at the bottom where small
    # values render without gaps - gaps at the chart top are more visually problematic.
    p <- ggplot(plot_data, aes(x = date, y = percentage_display, fill = sector, group = sector)) +
        geom_area(alpha = 0.9, position = position_stack(reverse = TRUE)) +
        scale_fill_manual(
            values = treasury_aggregate_colors,
            name = "Sector",
            guide = guide_legend(reverse = TRUE)
        ) +
        scale_x_date(
            date_breaks = "1 year",
            date_labels = "%Y",
            expand = c(0.01, 0)
        ) +
        scale_y_continuous(
            labels = scales::percent_format(scale = 1),
            expand = c(0, 0)
            # NO limits = c(0, 100) - let ggplot auto-scale to prevent clipping
        ) +
        labs(
            title = "SA Government Bond Holdings",
            subtitle = sprintf("Institutional Ownership Over Time (%s - %s)",
                               format(date_min, "%B %Y"),
                               format(date_max, "%B %Y")),
            x = NULL,
            y = "% Holdings",
            caption = "Source: Insele Capital Partners & National Treasury"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", size = 16, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 11, color = "#666666"),
            plot.caption = element_text(size = 9, color = "#999999", hjust = 1),
            legend.position = "bottom",
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 9),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 11)
        ) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    return(p)
}

# ================================================================================
# 2. GROUPED BAR CHART - CHANGE IN OWNERSHIP
# ================================================================================

#' Generate Ownership Change Chart
#'
#' Creates a grouped bar chart showing changes in institutional ownership
#' over different time periods
#'
#' @param holdings_long Data frame with columns: date, sector, percentage
#' @param periods Vector of period lengths in months (default: c(1, 3, 12))
#' @param reference_date Date to use as reference (default: most recent)
#' @return ggplot2 object
#' @export
generate_ownership_change_chart <- function(holdings_long,
                                            periods = c(1, 3, 12),
                                            reference_date = NULL) {

    # Input validation
    if (is.null(holdings_long) || nrow(holdings_long) == 0) {
        return(create_empty_plot("No holdings data available"))
    }

    # Set reference date to most recent if not provided
    if (is.null(reference_date)) {
        reference_date <- max(holdings_long$date, na.rm = TRUE)
    }

    # Calculate changes for each period
    change_data <- calculate_period_changes(holdings_long, reference_date, periods)

    if (is.null(change_data) || nrow(change_data) == 0) {
        return(create_empty_plot("Unable to calculate period changes"))
    }

    # Pivot to long format for plotting
    change_long <- change_data %>%
        select(sector, starts_with("change_")) %>%
        pivot_longer(
            cols = starts_with("change_"),
            names_to = "period",
            values_to = "change"
        ) %>%
        mutate(
            period_label = case_when(
                period == "change_1m" ~ "1-month",
                period == "change_3m" ~ "3-month",
                period == "change_6m" ~ "6-month",
                period == "change_12m" ~ "12-month",
                TRUE ~ period
            ),
            period_label = factor(period_label, levels = c("1-month", "3-month", "6-month", "12-month")),
            # Convert decimal changes to percentage changes (0.01 -> 1%)
            change_display = change * 100
        ) %>%
        filter(!is.na(change))

    # Create the grouped bar chart
    p <- ggplot(change_long, aes(x = sector, y = change_display, fill = period_label)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        geom_hline(yintercept = 0, color = "#333333", linewidth = 0.5) +
        scale_fill_manual(
            values = treasury_change_period_colors,
            name = "Period"
        ) +
        scale_y_continuous(
            labels = function(x) paste0(sprintf("%+.1f", x), "%"),
            expand = expansion(mult = c(0.1, 0.1))
        ) +
        coord_flip() +
        labs(
            title = "SA Bonds: Change in Institutional Ownership",
            subtitle = sprintf("As of %s", format(reference_date, "%B %Y")),
            x = NULL,
            y = "% Change",
            caption = "Source: Insele Capital Partners & National Treasury"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", size = 16, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 11, color = "#666666"),
            plot.caption = element_text(size = 9, color = "#999999", hjust = 1),
            legend.position = "bottom",
            legend.title = element_text(face = "bold", size = 10),
            panel.grid.major.y = element_blank(),
            axis.text = element_text(size = 10)
        )

    return(p)
}

# ================================================================================
# 3. HORIZONTAL STACKED BAR CHART - BOND HOLDINGS BY SECTOR
# ================================================================================

#' Generate Bond Holdings Bar Chart
#'
#' Creates a horizontal stacked bar chart showing institutional holdings
#' by bond at a specific date
#'
#' @param bond_pct_long Data frame with columns: file_date, sector, bond, value, bond_type
#' @param selected_bond_type Filter for bond type (e.g., "Fixed Rate", "ILB", "FRN", "Sukuk")
#' @param target_date Date to display (default: most recent)
#' @param show_labels Whether to show percentage labels inside bars
#' @param min_pct_label Minimum percentage to show label (default: 5)
#' @return ggplot2 object
#' @export
generate_bond_holdings_bar_chart <- function(bond_pct_long,
                                             selected_bond_type = "Fixed Rate",
                                             target_date = NULL,
                                             show_labels = TRUE,
                                             min_pct_label = 5) {

    # Input validation
    if (is.null(bond_pct_long) || nrow(bond_pct_long) == 0) {
        return(create_empty_plot("No bond holdings data available"))
    }

    # Set target date to most recent if not provided
    if (is.null(target_date)) {
        target_date <- max(bond_pct_long$file_date, na.rm = TRUE)
    }

    # Filter data - CRITICAL: Filter out NA sectors and summary rows BEFORE any processing
    # First filter for date and bond type, keeping all valid sectors (including 0% holdings)
    # NOTE: Using selected_bond_type parameter to avoid name collision with bond_type column
    #
    # BUG FIX: The Excel data contains TOTAL rows where sector appears as the STRING "NA"
    # (not R's NA value) and value = 1.0 (100%). Without proper filtering, each bond sums
    # to 200% instead of 100%, causing "Foreign sector" (~41% for R2039) to be pushed
    # past the 100% y-axis limit and become invisible.
    #
    # The fix uses MULTIPLE filtering approaches to catch string "NA" reliably:
    # 1. toupper(trimws(sector)) != "NA" - Simple string comparison (most reliable)
    # 2. Regex pattern as backup
    # 3. Character encoding check using nchar() for empty/whitespace strings
    filtered_data <- bond_pct_long %>%
        # CRITICAL: Convert to character and trim whitespace FIRST
        mutate(
            sector = trimws(as.character(sector)),
            bond = trimws(as.character(bond))
        ) %>%
        filter(
            # Date and bond type filters
            file_date == target_date,
            bond_type == selected_bond_type,
            # Remove R NA values (null/missing)
            !is.na(sector),
            !is.na(bond),
            !is.na(value),
            # Remove empty strings and whitespace-only sectors
            sector != "",
            nchar(sector) > 0,
            # CRITICAL FIX: Remove literal string "NA" using simple comparison
            # This is more reliable than regex for catching the TOTAL row issue
            toupper(sector) != "NA",
            # Remove TOTAL rows (partial match, case-insensitive)
            !grepl("TOTAL", sector, ignore.case = TRUE),
            # Remove CSDP reporting error rows (negligible, clutters legend)
            !grepl("CSDP", sector, ignore.case = TRUE),
            # Also filter bonds that are NA or TOTAL (summary rows)
            toupper(bond) != "NA",
            !grepl("TOTAL", bond, ignore.case = TRUE)
        )

    # Additional safety check: remove any rows where sector matches NA patterns
    # This handles edge cases with special characters or encoding issues
    filtered_data <- filtered_data %>%
        filter(!grepl("^\\s*NA\\s*$", sector, ignore.case = TRUE)) %>%
        filter(!grepl("^\\s*NA\\s*$", bond, ignore.case = TRUE))

    # Validate that each bond sums to ~100% (data integrity check)
    bond_totals <- filtered_data %>%
        group_by(bond) %>%
        summarise(total = sum(value, na.rm = TRUE) * 100, .groups = "drop")

    incomplete_bonds <- bond_totals %>% filter(total < 95 | total > 105)
    if (nrow(incomplete_bonds) > 0) {
        warning("Bond holdings data integrity issue - these bonds don't sum to ~100%: ",
                paste(incomplete_bonds$bond, sprintf("(%.1f%%)", incomplete_bonds$total), collapse = ", "))
    }

    # Now filter for positive values only (for display purposes)
    plot_data <- filtered_data %>%
        filter(value > 0)

    if (nrow(plot_data) == 0) {
        return(create_empty_plot(sprintf("No data available for %s bonds", selected_bond_type)))
    }

    # Create bond label with maturity year and convert decimal percentages to display percentages
    plot_data <- plot_data %>%
        mutate(
            bond_label = if_else(
                !is.na(maturity_year),
                sprintf("%s (%d)", gsub(" \\(\\d{4}\\)$", "", bond), maturity_year),
                bond
            ),
            # Convert decimal percentages (0.25) to display percentages (25)
            value_display = value * 100
        )

    # Order bonds by maturity year
    bond_order <- plot_data %>%
        distinct(bond_label, maturity_year) %>%
        arrange(maturity_year) %>%
        pull(bond_label)

    plot_data <- plot_data %>%
        mutate(bond_label = factor(bond_label, levels = bond_order))

    # FIX: Use full treasury_sector_colors directly - ggplot will match by name
    # The previous subsetting approach was causing "Foreign sector" to get wrong colors
    # because the name matching wasn't working reliably.
    #
    # Check for any sectors in data that don't have color mappings (for debugging)
    sectors_in_data <- unique(plot_data$sector)
    unmapped_sectors <- setdiff(sectors_in_data, names(treasury_sector_colors))
    if (length(unmapped_sectors) > 0) {
        warning("Bar chart: Unmapped sectors (will get default colors): ",
                paste(unmapped_sectors, collapse = ", "),
                "\nSectors in data: ", paste(sort(sectors_in_data), collapse = ", "))
    }

    # Create the horizontal stacked bar chart
    # CRITICAL FIX: Use full treasury_sector_colors mapping - ggplot matches by name
    # This ensures "Foreign sector" gets orange (#E8913A) as defined in the palette
    p <- ggplot(plot_data, aes(x = bond_label, y = value_display, fill = sector)) +
        geom_col(position = "stack", width = 0.8) +
        scale_fill_manual(
            values = treasury_sector_colors,  # Use FULL color mapping, not subset
            name = "Sector",
            drop = TRUE  # Only show sectors present in data in legend
        ) +
        scale_y_continuous(
            labels = scales::percent_format(scale = 1),
            expand = c(0, 0)
            # NO limits = c(0, 100) - let ggplot auto-scale to prevent clipping
        ) +
        coord_flip() +
        labs(
            title = sprintf("%s Bonds (%s)", selected_bond_type, format(target_date, "%B %Y")),
            subtitle = "Institutional Holdings by Bond",
            x = NULL,
            y = "% Holdings",
            caption = "Source: Insele Capital Partners & National Treasury"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", size = 16, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 11, color = "#666666"),
            plot.caption = element_text(size = 9, color = "#999999", hjust = 1),
            legend.position = "bottom",
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 8),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 9),
            axis.text.x = element_text(size = 10)
        ) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    # Add labels if requested
    if (show_labels) {
        # Calculate label positions (using value_display which is in 0-100 scale)
        label_data <- plot_data %>%
            arrange(bond_label, desc(sector)) %>%
            group_by(bond_label) %>%
            mutate(
                cumsum_value = cumsum(value_display),
                label_pos = cumsum_value - value_display / 2
            ) %>%
            ungroup() %>%
            filter(value_display >= min_pct_label)

        p <- p +
            geom_text(
                data = label_data,
                aes(x = bond_label, y = label_pos, label = sprintf("%.0f%%", value_display)),
                color = "white",
                fontface = "bold",
                size = 2.5
            )
    }

    return(p)
}

# ================================================================================
# 4. DIVERGING BAR CHART - CHANGE IN HOLDINGS PER BOND
# ================================================================================

#' Generate Holdings Change Diverging Chart
#'
#' Creates a diverging bar chart showing changes in holdings by bond and sector.
#' Supports bond exclusion (manual and automatic outlier detection) and maturity ordering.
#'
#' @param bond_pct_long Data frame with columns: file_date, sector, bond, value, bond_type
#' @param period_months Number of months for change calculation (default: 3)
#' @param selected_bond_type Filter for bond type
#' @param top_n Number of bonds to show (default: 15)
#' @param exclude_bonds Character vector of bond names to exclude from the chart (default: NULL)
#' @param auto_exclude_outliers Logical, whether to auto-exclude bonds with extreme changes (default: FALSE)
#' @param outlier_threshold Numeric, threshold for outlier detection in percentage points (default: 80)
#' @param order_by_maturity Logical, whether to order bonds by maturity date (default: TRUE)
#' @return List with ggplot2 object and vector of excluded bonds, or just ggplot2 object for backward compatibility
#' @export
generate_holdings_change_diverging <- function(bond_pct_long,
                                               period_months = 3,
                                               selected_bond_type = "Fixed Rate",
                                               top_n = 15,
                                               exclude_bonds = NULL,
                                               auto_exclude_outliers = FALSE,
                                               outlier_threshold = 80,
                                               order_by_maturity = TRUE) {

    # Input validation
    if (is.null(bond_pct_long) || nrow(bond_pct_long) == 0) {
        return(create_empty_plot("No bond holdings data available"))
    }

    # Get current and historical dates
    current_date <- max(bond_pct_long$file_date, na.rm = TRUE)

    # Find historical date closest to period_months ago
    all_dates <- sort(unique(bond_pct_long$file_date))
    target_historical <- current_date %m-% months(period_months)
    historical_date <- all_dates[which.min(abs(all_dates - target_historical))]

    # CRITICAL: Filter out NA sectors and summary rows BEFORE any calculations
    # This prevents NA from appearing in the legend
    # NOTE: Using selected_bond_type parameter to avoid name collision with bond_type column
    #
    # BUG FIX: The Excel data contains TOTAL rows where sector appears as the STRING "NA"
    # (not R's NA value) and value = 1.0 (100%). These must be filtered out using
    # multiple approaches to ensure accurate change calculations.
    clean_bond_data <- bond_pct_long %>%
        # CRITICAL: Convert to character and trim whitespace FIRST
        mutate(
            sector = trimws(as.character(sector)),
            bond = trimws(as.character(bond))
        ) %>%
        filter(
            bond_type == selected_bond_type,
            # Remove R NA values (null/missing)
            !is.na(sector),
            !is.na(bond),
            !is.na(value),
            # Remove empty strings and whitespace-only sectors
            sector != "",
            nchar(sector) > 0,
            # CRITICAL FIX: Remove literal string "NA" using simple comparison
            toupper(sector) != "NA",
            # Remove TOTAL rows (partial match, case-insensitive)
            !grepl("TOTAL", sector, ignore.case = TRUE),
            # Remove CSDP reporting error rows (negligible, clutters legend)
            !grepl("CSDP", sector, ignore.case = TRUE),
            # Also filter bonds that are NA or TOTAL (summary rows)
            toupper(bond) != "NA",
            !grepl("TOTAL", bond, ignore.case = TRUE)
        ) %>%
        # Additional safety check for edge cases
        filter(!grepl("^\\s*NA\\s*$", sector, ignore.case = TRUE)) %>%
        filter(!grepl("^\\s*NA\\s*$", bond, ignore.case = TRUE))

    # Calculate changes using cleaned data
    current_data <- clean_bond_data %>%
        filter(file_date == current_date) %>%
        select(sector, bond, current_value = value)

    historical_data <- clean_bond_data %>%
        filter(file_date == historical_date) %>%
        select(sector, bond, historical_value = value)

    change_data <- current_data %>%
        inner_join(historical_data, by = c("sector", "bond")) %>%
        mutate(
            change = current_value - historical_value,
            # Convert decimal changes to percentage changes (0.01 -> 1%)
            change_display = (current_value - historical_value) * 100
        ) %>%
        filter(!is.na(change))

    if (nrow(change_data) == 0) {
        return(create_empty_plot("Unable to calculate changes"))
    }

    # ==================================================================
    # BOND EXCLUSION LOGIC
    # ==================================================================

    # Calculate total change per bond for outlier detection
    bond_total_changes <- change_data %>%
        group_by(bond) %>%
        summarize(
            total_change = sum(change_display, na.rm = TRUE),
            total_abs_change = sum(abs(change_display), na.rm = TRUE),
            .groups = "drop"
        )

    # Initialize excluded bonds tracking
    all_excluded_bonds <- character(0)
    auto_excluded_bonds <- character(0)

    # Auto-exclude outlier bonds (newly issued or matured bonds with extreme changes)
    if (auto_exclude_outliers) {
        auto_excluded_bonds <- bond_total_changes %>%
            filter(abs(total_change) > outlier_threshold) %>%
            pull(bond) %>%
            unique()

        all_excluded_bonds <- auto_excluded_bonds
    }

    # Add manually excluded bonds
    if (!is.null(exclude_bonds) && length(exclude_bonds) > 0) {
        all_excluded_bonds <- unique(c(all_excluded_bonds, exclude_bonds))
    }

    # Remove excluded bonds from change_data
    if (length(all_excluded_bonds) > 0) {
        change_data <- change_data %>%
            filter(!bond %in% all_excluded_bonds)

        bond_total_changes <- bond_total_changes %>%
            filter(!bond %in% all_excluded_bonds)
    }

    # Check if we have any data left after exclusions
    if (nrow(change_data) == 0) {
        return(create_empty_plot("All bonds excluded - adjust exclusion settings"))
    }

    # ==================================================================
    # BOND SELECTION AND ORDERING
    # ==================================================================

    # FIX: Select top bonds by absolute change - ONLY from bonds remaining after exclusion
    # Get unique bonds that actually exist in change_data after exclusion
    available_bonds <- unique(change_data$bond)

    top_bonds <- bond_total_changes %>%
        filter(bond %in% available_bonds) %>%  # FIX: Only consider bonds that weren't excluded
        arrange(desc(total_abs_change)) %>%
        slice_head(n = top_n) %>%
        pull(bond)

    # Order bonds by maturity date if requested
    if (order_by_maturity) {
        # Calculate maturity year for each bond
        bond_maturity <- tibble(
            bond = top_bonds,
            maturity_year = sapply(top_bonds, extract_maturity_year)
        ) %>%
            arrange(maturity_year)  # Nearest maturity first

        # Order: nearest maturity at TOP of chart (which means last in factor levels due to coord_flip)
        # With coord_flip, the first factor level appears at the bottom of the y-axis
        # So we reverse the order: furthest maturity first in levels = appears at bottom
        top_bonds_ordered <- rev(bond_maturity$bond)  # Furthest maturity first in levels
    } else {
        # Keep original order by absolute change (largest change at top)
        top_bonds_ordered <- rev(top_bonds)
    }

    # FIX: Filter plot_data first, THEN set factor levels based on what's actually in the data
    plot_data <- change_data %>%
        filter(bond %in% top_bonds)

    # FIX: Only use bonds that are actually in plot_data for factor levels
    # This prevents empty rows from appearing for excluded bonds
    actual_bonds_in_plot <- intersect(top_bonds_ordered, unique(plot_data$bond))

    plot_data <- plot_data %>%
        mutate(
            bond = factor(bond, levels = actual_bonds_in_plot),  # FIX: Only include actual bonds
            direction = if_else(change_display >= 0, "Increase", "Decrease")
        ) %>%
        filter(!is.na(bond))  # FIX: Remove any rows where bond became NA due to factor mismatch

    # Validate we still have data
    if (nrow(plot_data) == 0) {
        p <- create_empty_plot("No data available after filtering")
        attr(p, "excluded_bonds") <- all_excluded_bonds
        attr(p, "auto_excluded_bonds") <- auto_excluded_bonds
        return(p)
    }

    # Drop unused factor levels to ensure clean y-axis
    plot_data <- plot_data %>%
        mutate(bond = droplevels(bond))

    # FIX: Use full treasury_sector_colors directly - ggplot will match by name
    # Check for any sectors in data that don't have color mappings (for debugging)
    sectors_in_data <- unique(plot_data$sector)
    unmapped_sectors <- setdiff(sectors_in_data, names(treasury_sector_colors))
    if (length(unmapped_sectors) > 0) {
        warning("Diverging chart: Unmapped sectors (will get default colors): ",
                paste(unmapped_sectors, collapse = ", "))
    }

    # Build subtitle with ordering and exclusion info
    subtitle_parts <- sprintf("%d-month change per bond (as of %s)",
                              period_months,
                              format(current_date, "%B %Y"))
    if (order_by_maturity) {
        subtitle_parts <- paste0(subtitle_parts, " • Ordered by maturity")
    }

    # Create the diverging bar chart
    # CRITICAL FIX: Use full treasury_sector_colors mapping - ggplot matches by name
    p <- ggplot(plot_data, aes(x = bond, y = change_display, fill = sector)) +
        geom_col(position = "stack", width = 0.7) +
        geom_hline(yintercept = 0, color = "#333333", linewidth = 0.5) +
        scale_fill_manual(
            values = treasury_sector_colors,  # Use FULL color mapping, not subset
            name = "Sector",
            drop = TRUE  # Only show sectors present in data in legend
        ) +
        scale_y_continuous(
            labels = function(x) paste0(sprintf("%+.1f", x), "%"),
            expand = expansion(mult = c(0.15, 0.15))
        ) +
        coord_flip() +
        labs(
            title = sprintf("SA %s Bonds: Change in Institutional Ownership", selected_bond_type),
            subtitle = subtitle_parts,
            x = NULL,
            y = "% Change",
            caption = "Source: Insele Capital Partners & National Treasury"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", size = 16, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 11, color = "#666666"),
            plot.caption = element_text(size = 9, color = "#999999", hjust = 1),
            legend.position = "bottom",
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 8),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 9),
            axis.text.x = element_text(size = 10)
        ) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    # Return plot with metadata about excluded bonds
    # This allows the calling code to display exclusion information
    # Store excluded bonds info as an attribute to maintain backward compatibility
    attr(p, "excluded_bonds") <- all_excluded_bonds
    attr(p, "auto_excluded_bonds") <- auto_excluded_bonds

    return(p)
}

# ================================================================================
# 5. SECTOR TREEMAP - HOLDINGS DISTRIBUTION
# ================================================================================

#' Generate Sector Treemap
#'
#' Creates a treemap showing the distribution of holdings by sector
#'
#' @param holdings_long Data frame with columns: date, sector, percentage
#' @param target_date Date to display (default: most recent)
#' @return ggplot2 object
#' @export
generate_sector_treemap <- function(holdings_long, target_date = NULL) {

    # Check if treemapify package is available
    if (!requireNamespace("treemapify", quietly = TRUE)) {
        return(create_empty_plot("treemapify package required for treemap"))
    }

    # Input validation
    if (is.null(holdings_long) || nrow(holdings_long) == 0) {
        return(create_empty_plot("No holdings data available"))
    }

    # Set target date to most recent if not provided
    if (is.null(target_date)) {
        target_date <- max(holdings_long$date, na.rm = TRUE)
    }

    # Filter to target date and convert percentages
    plot_data <- holdings_long %>%
        filter(date == target_date) %>%
        filter(!is.na(percentage), percentage > 0) %>%
        mutate(percentage_display = percentage * 100)

    if (nrow(plot_data) == 0) {
        return(create_empty_plot("No data available for selected date"))
    }

    # Create the treemap (using percentage_display for labels, percentage for area)
    p <- ggplot(plot_data, aes(area = percentage_display, fill = sector, label = sprintf("%s\n%.1f%%", sector, percentage_display))) +
        treemapify::geom_treemap() +
        treemapify::geom_treemap_text(color = "white", place = "centre", grow = FALSE, fontface = "bold") +
        scale_fill_manual(
            values = treasury_aggregate_colors,
            name = "Sector"
        ) +
        labs(
            title = "SA Government Bond Holdings Distribution",
            subtitle = format(target_date, "%B %Y"),
            caption = "Source: Insele Capital Partners & National Treasury"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", size = 16, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 11, color = "#666666"),
            plot.caption = element_text(size = 9, color = "#999999", hjust = 1),
            legend.position = "none"
        )

    return(p)
}

# ================================================================================
# 6. LINE CHART - SINGLE SECTOR TREND
# ================================================================================

#' Generate Single Sector Trend Chart
#'
#' Creates a line chart showing the trend for a single sector over time
#'
#' @param holdings_long Data frame with columns: date, sector, percentage
#' @param sector_name Name of the sector to plot
#' @param date_range Vector of length 2 with start and end dates (optional)
#' @return ggplot2 object
#' @export
generate_sector_trend_chart <- function(holdings_long,
                                        sector_name = "Non-residents",
                                        date_range = NULL) {

    # Input validation
    if (is.null(holdings_long) || nrow(holdings_long) == 0) {
        return(create_empty_plot("No holdings data available"))
    }

    # Filter to sector
    plot_data <- holdings_long %>%
        filter(sector == sector_name)

    if (nrow(plot_data) == 0) {
        return(create_empty_plot(sprintf("No data available for sector: %s", sector_name)))
    }

    # Filter by date range if provided
    if (!is.null(date_range) && length(date_range) == 2) {
        plot_data <- plot_data %>%
            filter(date >= date_range[1], date <= date_range[2])
    }

    # Use Insele navy color for single sector trend (professional, consistent look)
    sector_color <- treasury_brand_colors$navy_dark

    # Create the line chart - convert decimal percentages to display percentages
    p <- ggplot(plot_data, aes(x = date, y = percentage * 100)) +
        geom_area(fill = sector_color, alpha = 0.3) +
        geom_line(color = sector_color, linewidth = 1.2) +
        geom_point(color = sector_color, size = 2) +
        scale_x_date(
            date_breaks = "1 year",
            date_labels = "%Y",
            expand = c(0.01, 0)
        ) +
        scale_y_continuous(
            labels = scales::percent_format(scale = 1),
            expand = expansion(mult = c(0.05, 0.1))
        ) +
        labs(
            title = sprintf("%s Holdings of SA Government Bonds", sector_name),
            subtitle = "Percentage of total outstanding",
            x = NULL,
            y = "Holdings (%)",
            caption = "Source: Insele Capital Partners & National Treasury"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", size = 16, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 11, color = "#666666"),
            plot.caption = element_text(size = 9, color = "#999999", hjust = 1),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 10)
        )

    return(p)
}

# ================================================================================
# HELPER FUNCTIONS
# ================================================================================

#' Extract Maturity Year from Bond Name
#'
#' Parses SA Government bond names to extract the maturity year.
#' Handles various naming conventions:
#' - R2033, I2043 -> direct year extraction (2033, 2043)
#' - R186, R209, R213, R214 -> legacy naming with known maturity mapping
#' - Other formats -> regex extraction of 4-digit year
#'
#' @param bond_name Character string with bond name (e.g., "R2033", "I2043", "R186")
#' @return Integer year (e.g., 2033) or 9999 for unknown bonds (placed at end)
#' @export
extract_maturity_year <- function(bond_name) {
    # Handle NULL or NA
    if (is.null(bond_name) || is.na(bond_name) || bond_name == "") {
        return(9999)
    }

    # Clean the bond name (remove any maturity year suffix like " (2033)")
    clean_name <- trimws(gsub("\\s*\\(\\d{4}\\)\\s*$", "", as.character(bond_name)))

    # Handle R20XX, I20XX format (e.g., R2033, I2043, R2048)
    if (grepl("^[RI]20[0-9]{2}$", clean_name)) {
        return(as.integer(substr(clean_name, 2, 5)))
    }

    # Handle legacy SA bond naming conventions with known maturity dates
    # These are well-known SA government bonds with specific maturity dates
    legacy_map <- c(
        "R186" = 2026,   # 21 December 2026
        "R197" = 2023,   # Matured (included for historical data)
        "R202" = 2033,   # 7 December 2033
        "R203" = 2025,   # 15 September 2025
        "R204" = 2018,   # Matured
        "R207" = 2020,   # Matured
        "R208" = 2021,   # Matured
        "R209" = 2036,   # 31 March 2036
        "R210" = 2028,   # 31 March 2028
        "R211" = 2024,   # Matured
        "R212" = 2024,   # Matured
        "R213" = 2031,   # 28 February 2031
        "R214" = 2041,   # 31 March 2041
        "R2023" = 2023,  # Already handled by regex above, but included for safety
        "R2025" = 2025,
        "R2030" = 2030,
        "R2032" = 2032,
        "R2035" = 2035,
        "R2037" = 2037,
        "R2040" = 2040,
        "R2044" = 2044,
        "R2048" = 2048,
        "R2053" = 2053
    )

    if (clean_name %in% names(legacy_map)) {
        return(legacy_map[[clean_name]])
    }

    # Try to extract any 4-digit year starting with 20 from the bond name
    year_match <- regmatches(clean_name, regexpr("20[0-9]{2}", clean_name))
    if (length(year_match) > 0 && nchar(year_match) == 4) {
        return(as.integer(year_match))
    }

    # For ILBs (Inflation-Linked Bonds) with I prefix
    # e.g., I2025, I2033, I2038, I2046, I2050
    if (grepl("^I[0-9]{4}$", clean_name)) {
        return(as.integer(substr(clean_name, 2, 5)))
    }

    # For any remaining patterns, return 9999 to place unknown bonds at the end
    return(9999)
}

#' Calculate Period Changes
#'
#' Calculates ownership changes over different time periods
#'
#' @param holdings_long Data frame with columns: date, sector, percentage
#' @param reference_date Reference date for calculations
#' @param periods Vector of period lengths in months
#' @return Data frame with change calculations
#' @export
calculate_period_changes <- function(holdings_long,
                                     reference_date = NULL,
                                     periods = c(1, 3, 6, 12)) {

    if (is.null(reference_date)) {
        reference_date <- max(holdings_long$date, na.rm = TRUE)
    }

    # Get all available dates
    all_dates <- sort(unique(holdings_long$date))

    # Get current data
    current_data <- holdings_long %>%
        filter(date == reference_date) %>%
        select(sector, current_pct = percentage)

    if (nrow(current_data) == 0) {
        return(NULL)
    }

    result <- current_data

    # Calculate changes for each period
    for (period in periods) {
        target_date <- reference_date %m-% months(period)

        # Find closest available date
        closest_date <- all_dates[which.min(abs(all_dates - target_date))]

        # Only use if within reasonable range (within 2 weeks of target)
        if (abs(closest_date - target_date) <= 15) {
            historical_data <- holdings_long %>%
                filter(date == closest_date) %>%
                select(sector, historical_pct = percentage)

            change_col <- paste0("change_", period, "m")

            result <- result %>%
                left_join(historical_data, by = "sector") %>%
                mutate(!!change_col := current_pct - historical_pct) %>%
                select(-historical_pct)
        }
    }

    return(result)
}

#' Create Empty Plot
#'
#' Creates a placeholder plot when data is unavailable
#'
#' @param message Message to display
#' @return ggplot2 object
create_empty_plot <- function(message = "No data available") {
    ggplot() +
        annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = message,
            size = 6,
            color = "#666666"
        ) +
        theme_void() +
        theme(
            plot.background = element_rect(fill = "#f8f9fa", color = NA),
            panel.background = element_rect(fill = "#f8f9fa", color = NA)
        ) +
        xlim(0, 1) +
        ylim(0, 1)
}

#' Format Treasury Data Summary
#'
#' Creates a summary of the treasury data for display
#'
#' @param holdings_ts Holdings time series data
#' @param bond_holdings Bond-level holdings data
#' @return List with summary statistics
#' @export
format_treasury_data_summary <- function(holdings_ts = NULL, bond_holdings = NULL) {

    summary_list <- list()

    if (!is.null(holdings_ts) && nrow(holdings_ts) > 0) {
        summary_list$holdings_date_range <- range(holdings_ts$date, na.rm = TRUE)
        summary_list$holdings_n_months <- length(unique(holdings_ts$date))
        summary_list$holdings_sectors <- unique(holdings_ts$sector)
    }

    if (!is.null(bond_holdings) && nrow(bond_holdings) > 0) {
        summary_list$bonds_date_range <- range(bond_holdings$file_date, na.rm = TRUE)
        summary_list$bonds_n_months <- length(unique(bond_holdings$file_date))
        summary_list$bond_types <- unique(bond_holdings$bond_type)
        summary_list$n_bonds <- length(unique(bond_holdings$bond[!grepl("TOTAL", bond_holdings$bond)]))
    }

    return(summary_list)
}

# ================================================================================
# EXPORT STATUS
# ================================================================================
message("Treasury Holdings plot generators loaded successfully")
