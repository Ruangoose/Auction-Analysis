pacman::p_load(ggplot2)

rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

# ============================================================================
# SA Government Bond Holdings - Master Script
# ============================================================================
#
# This script sources all processing functions for SA government bond holdings
# data downloaded from the National Treasury website.
#
# Quick Start:
#   source("sa_bond_holdings_master.R")
#   results <- process_all_sa_bond_data()
#
# For more details, see README.md
# ============================================================================

# Check and load required packages
required_packages <- c("readxl", "dplyr", "tidyr", "lubridate", "stringr", "zoo", "httr")

for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        message(sprintf("Installing required package: %s", pkg))
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

# Get the directory where this script is located
script_dir <- getSrcDirectory(function() {})
if (script_dir == "") {
    script_dir <- getwd()
}

# Source all component scripts
message("Loading SA Bond Holdings processing functions...")

source(file.path(script_dir, "/bond-dashboard/Modules/download/process_bond_holdings.R"))
message("  ✓ Basic processing functions loaded")

source(file.path(script_dir, "/bond-dashboard/Modules/download/download_sa_bond_holdings.R"))
message("  ✓ Basic processing functions loaded")


source(file.path(script_dir, "bond-dashboard/Modules/download/process_bond_holdings_tidy.R"))
message("  ✓ Tidy processing functions loaded")

source(file.path(script_dir, "bond-dashboard/Modules/download/process_holdings_timeseries.R"))
message("  ✓ Time series processing functions loaded")

source(file.path(script_dir, "bond-dashboard/Modules/download/validate_and_repair.R"))
message("  ✓ File validation and repair utilities loaded")

message("\n=== Functions Available ===")
message("\nData Download:")
message("  download_sa_bond_holdings() - Download Excel files from Treasury website")
message("\nFile Validation & Repair:")
message("  validate_bond_files()         - Check files for corruption")
message("  repair_bond_files()           - Fix corrupted files (RECOMMENDED)")
message("  redownload_corrupted_files()  - Re-download problematic files")
message("\nData Processing:")
message("  process_all_sa_bond_data() - Process all data (RECOMMENDED)")
message("  process_sa_bond_holdings() - Basic processing")
message("  process_sa_bond_holdings_tidy() - Enhanced processing with tidy format")
message("  process_holdings_timeseries() - Historical time series only")
message("\nData Loading:")
message("  load_bond_data() - Load processed RDS files")
message("\nFor detailed usage examples, see README.md\n")

# ============================================================================
# Convenience wrapper function
# ============================================================================

complete_bond_data_workflow <- function(
        download_files = FALSE,
        validate_files = TRUE,
        start_date = "2018-02-01",
        end_date = NULL,  # Default to last month (1-month publication lag)
        source_folder = "bond_holdings",
        output_folder = "bond_holdings_rds",
        overwrite = FALSE
) {
    # Complete workflow: download + validate + process

    # Set end_date to last month if not specified
    if (is.null(end_date) && download_files) {
        current_month_start <- floor_date(Sys.Date(), "month")
        end_date <- current_month_start - days(1)
    }

    cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
    cat("SA GOVERNMENT BOND HOLDINGS - COMPLETE WORKFLOW\n")
    cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

    # Step 1: Download (optional)
    if (download_files) {
        cat("STEP 1: Downloading Excel files from Treasury website\n")
        cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")

        download_results <- download_sa_bond_holdings(
            start_date = start_date,
            end_date = end_date,
            dest_folder = source_folder,
            download_latest = TRUE
        )

        cat("\n")
        cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
    } else {
        cat("Skipping download step (download_files = FALSE)\n\n")
        download_results <- NULL
    }

    # Step 2: Validate files (optional but recommended)
    if (validate_files) {
        cat("STEP 2: Validating Excel files\n")
        cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")

        validation_results <- validate_bond_files(source_folder, verbose = TRUE)

        corrupted <- validation_results %>% filter(status != "Valid")

        if (nrow(corrupted) > 0) {
            cat("\n⚠ WARNING: Found", nrow(corrupted), "corrupted file(s).\n")
            cat("These files will be skipped during processing.\n")
            cat("To fix them, run: repair_bond_files()\n\n")
        } else {
            cat("\n✓ All files validated successfully!\n\n")
        }

        cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
    } else {
        cat("Skipping validation step (validate_files = FALSE)\n\n")
        validation_results <- NULL
    }

    # Step 3: Process all data
    step_num <- if (download_files || validate_files) {
        if (download_files && validate_files) 3 else 2
    } else 1

    cat(sprintf("STEP %d: Processing all data\n", step_num))
    cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")

    process_results <- process_all_sa_bond_data(
        source_folder = source_folder,
        output_folder = output_folder,
        overwrite = overwrite
    )

    cat("\n")
    cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
    cat("WORKFLOW COMPLETE!\n")
    cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

    cat("Next steps:\n")
    cat("  # Load and analyze your data\n")
    cat("  holdings <- readRDS('bond_holdings_rds/holdings_historical_long.rds')\n")
    cat("  all_bonds <- load_bond_data('all_values', format = 'long')\n\n")

    invisible(list(
        download_results = download_results,
        validation_results = if(validate_files) validation_results else NULL,
        process_results = process_results
    ))
}

# ============================================================================
# Quick reference functions
# ============================================================================

list_available_datasets <- function(folder = "bond_holdings_rds") {
    # List all processed datasets

    if (!dir.exists(folder)) {
        cat("Output folder does not exist yet.\n")
        cat("Run process_all_sa_bond_data() first.\n")
        return(invisible(NULL))
    }

    files <- list.files(folder, pattern = "\\.rds$", full.names = FALSE)

    if (length(files) == 0) {
        cat("No RDS files found in", folder, "\n")
        cat("Run process_all_sa_bond_data() first.\n")
        return(invisible(NULL))
    }

    cat("Available datasets in", folder, ":\n\n")

    # Group by category
    holdings <- files[grepl("holdings_historical", files)]
    fixed <- files[grepl("fixed_rate", files)]
    ilb <- files[grepl("ilb_", files)]
    frn <- files[grepl("frn_", files)]
    sukuk <- files[grepl("sukuk_", files)]
    combined <- files[grepl("all_bonds", files)]

    print_files <- function(title, file_list) {
        if (length(file_list) > 0) {
            cat(title, "\n")
            for (f in file_list) {
                cat("  •", f, "\n")
            }
            cat("\n")
        }
    }

    print_files("Historical Time Series:", holdings)
    print_files("Fixed Rate Bonds:", fixed)
    print_files("Inflation-Linked Bonds:", ilb)
    print_files("Floating Rate Notes:", frn)
    print_files("Sukuk:", sukuk)
    print_files("Combined Datasets:", combined)

    invisible(files)
}

get_latest_data_date <- function(folder = "bond_holdings_rds") {
    # Get the most recent date in the processed data

    file_path <- file.path(folder, "all_bonds_values_long.rds")

    if (!file.exists(file_path)) {
        cat("Processed data not found.\n")
        cat("Run process_all_sa_bond_data() first.\n")
        return(invisible(NULL))
    }

    data <- readRDS(file_path)
    latest_date <- max(data$file_date, na.rm = TRUE)

    cat("Latest data date:", format(latest_date, "%B %Y"), "\n")
    return(invisible(latest_date))
}

# ============================================================================
# Print loaded message
# ============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("SA BOND HOLDINGS PROCESSING - READY\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("\nRecommended workflow:\n")
cat("  1. results <- process_all_sa_bond_data()\n")
cat("  2. list_available_datasets()\n")
cat("  3. data <- load_bond_data('all_values', format = 'long')\n")
cat("\nOr use the complete workflow:\n")
cat("  complete_bond_data_workflow(download_files = TRUE)\n")
cat("\nFor help: see README.md\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")




# ✅ Let function handle everything automatically
download_sa_bond_holdings(download_latest = TRUE)


complete_bond_data_workflow(
        download_files = FALSE,
        validate_files = TRUE,
        start_date = "2018-02-01",
        end_date = NULL,  # Default to last month (1-month publication lag)
        source_folder = "bond_holdings",
        output_folder = "bond_holdings_rds",
        overwrite = FALSE
)

# ============================================================================
# STEP 2: Load Processed Data
# ============================================================================

# Check what's available
list_available_datasets()
get_latest_data_date()

# Load datasets for analysis
holdings_long <- readRDS("bond_holdings_rds/holdings_historical_long.rds")
all_bonds_values <- load_bond_data("all_values", format = "long")
all_bonds_pct <- load_bond_data("all_pct", format = "long")

# ============================================================================
# EXAMPLE 1: Non-Resident Holdings Over Time
# ============================================================================

cat("\n=== Example 1: Non-Resident Holdings Trend ===\n")

# Calculate and plot
p1 <- holdings_long %>%
    filter(sector == "Non-residents") %>%
    ggplot(aes(x = date, y = percentage)) +
    geom_line(linewidth = 1, color = "#E74C3C") +
    geom_hline(yintercept = 0.3, linetype = "dashed", color = "gray50") +
    scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = seq(0, 0.5, 0.05)
    ) +
    labs(
        title = "Non-Resident Holdings of SA Government Bonds",
        subtitle = "Historical trend (2006 - present)",
        x = NULL,
        y = "Holdings (%)",
        caption = "Source: National Treasury"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank()
    )

print(p1)
# ggsave("non_resident_holdings.png", p1, width = 10, height = 6, dpi = 300)

# ============================================================================
# EXAMPLE 2: Sector Composition - Latest Month
# ============================================================================

cat("\n=== Example 2: Current Sector Holdings ===\n")

latest_holdings <- holdings_long %>%
    filter(date == max(date)) %>%
    arrange(desc(percentage))

print(latest_holdings)

p2 <- latest_holdings %>%
    mutate(sector = forcats::fct_reorder(sector, percentage)) %>%
    ggplot(aes(x = sector, y = percentage, fill = sector)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = "Set2") +
    labs(
        title = "SA Government Bond Holdings by Sector",
        subtitle = paste("As of", format(max(holdings_long$date), "%B %Y")),
        x = NULL,
        y = "Holdings (%)"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.y = element_blank()
    )

print(p2)
# ggsave("sector_holdings_current.png", p2, width = 10, height = 6, dpi = 300)

# ============================================================================
# EXAMPLE 3: Specific Bond Analysis - R2030
# ============================================================================

cat("\n=== Example 3: R2030 Holdings by Sector ===\n")

r2030_holdings <- all_bonds_values %>%
    filter(bond == "R2030 (2030)") %>%
    mutate(value_bn = value / 1e9)

# Latest holdings
latest_r2030 <- r2030_holdings %>%
    filter(file_date == max(file_date)) %>%
    arrange(desc(value_bn)) %>%
    select(sector, value_bn)

print(latest_r2030)

# Time series by sector
p3 <- r2030_holdings %>%
    filter(sector %in% c("Non-residents", "Banks", "Other financial institutions",
                         "Official pension funds")) %>%
    ggplot(aes(x = file_date, y = value_bn, color = sector)) +
    geom_line(linewidth = 1) +
    scale_color_brewer(palette = "Dark2") +
    labs(
        title = "R2030 Holdings by Major Sectors",
        subtitle = "Evolution over time",
        x = NULL,
        y = "Holdings (R billions)",
        color = "Sector"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom"
    )

print(p3)
# ggsave("r2030_holdings_by_sector.png", p3, width = 10, height = 6, dpi = 300)

# ============================================================================
# EXAMPLE 4: Bond Type Distribution
# ============================================================================

cat("\n=== Example 4: Holdings by Bond Type ===\n")

bond_type_summary <- all_bonds_values %>%
    filter(file_date == max(file_date)) %>%
    group_by(bond_type) %>%
    summarise(
        total_value = sum(value, na.rm = TRUE),
        n_bonds = n_distinct(bond)
    ) %>%
    mutate(
        total_value_bn = total_value / 1e9,
        percentage = total_value / sum(total_value)
    ) %>%
    arrange(desc(total_value))

print(bond_type_summary)

p4 <- all_bonds_values %>%
    group_by(file_date, bond_type) %>%
    summarise(total = sum(value, na.rm = TRUE) / 1e9, .groups = "drop") %>%
    ggplot(aes(x = file_date, y = total, fill = bond_type)) +
    geom_area(alpha = 0.7) +
    scale_fill_brewer(palette = "Set1") +
    labs(
        title = "SA Government Bond Holdings by Type",
        subtitle = "Composition over time",
        x = NULL,
        y = "Total Holdings (R billions)",
        fill = "Bond Type"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom"
    )

print(p4)
# ggsave("holdings_by_bond_type.png", p4, width = 10, height = 6, dpi = 300)

# ============================================================================
# EXAMPLE 5: Pension Fund Holdings Analysis
# ============================================================================

cat("\n=== Example 5: Pension Fund Holdings ===\n")

pension_holdings <- holdings_long %>%
    filter(sector %in% c("Local pension funds", "Official pension funds",
                         "Private self-administered funds")) %>%
    group_by(date) %>%
    summarise(total_pension = sum(percentage, na.rm = TRUE))

p5 <- pension_holdings %>%
    ggplot(aes(x = date, y = total_pension)) +
    geom_line(linewidth = 1, color = "#2ECC71") +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
        title = "Combined Pension Fund Holdings",
        subtitle = "Includes local, official, and private pension funds",
        x = NULL,
        y = "Total Holdings (%)",
        caption = "Trend line: LOESS smoothing"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))

print(p5)
# ggsave("pension_fund_holdings.png", p5, width = 10, height = 6, dpi = 300)

# ============================================================================
# EXAMPLE 6: Yield Curve Analysis (by maturity year)
# ============================================================================

cat("\n=== Example 6: Holdings Distribution by Maturity ===\n")

maturity_dist <- all_bonds_values %>%
    filter(file_date == max(file_date)) %>%
    group_by(maturity_year) %>%
    summarise(
        total_value = sum(value, na.rm = TRUE) / 1e9,
        n_bonds = n_distinct(bond)
    ) %>%
    filter(!is.na(maturity_year))

p6 <- maturity_dist %>%
    ggplot(aes(x = maturity_year, y = total_value)) +
    geom_col(fill = "#3498DB", alpha = 0.8) +
    geom_text(aes(label = n_bonds), vjust = -0.5, size = 3) +
    labs(
        title = "Bond Holdings by Maturity Year",
        subtitle = paste("As of", format(max(all_bonds_values$file_date), "%B %Y"),
                         "(numbers show count of bonds)"),
        x = "Maturity Year",
        y = "Total Holdings (R billions)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))

print(p6)
# ggsave("holdings_by_maturity.png", p6, width = 12, height = 6, dpi = 300)

# ============================================================================
# EXAMPLE 7: Month-over-Month Changes
# ============================================================================

cat("\n=== Example 7: Recent Changes in Non-Resident Holdings ===\n")

non_res_changes <- holdings_long %>%
    filter(sector == "Non-residents") %>%
    arrange(date) %>%
    mutate(
        pct_change = (percentage - lag(percentage)),
        abs_change = abs(pct_change)
    ) %>%
    filter(!is.na(pct_change)) %>%
    arrange(desc(abs_change)) %>%
    head(10)

print(non_res_changes %>% select(date, percentage, pct_change))

# ============================================================================
# EXAMPLE 8: Cross-Sectional Analysis - Latest Month
# ============================================================================

cat("\n=== Example 8: Heatmap of Holdings by Sector and Bond ===\n")

# Get top 10 bonds by total value
top_bonds <- all_bonds_values %>%
    filter(file_date == max(file_date)) %>%
    group_by(bond) %>%
    summarise(total = sum(value, na.rm = TRUE)) %>%
    slice_max(total, n = 10) %>%
    pull(bond)

heatmap_data <- all_bonds_pct %>%
    filter(file_date == max(file_date), bond %in% top_bonds) %>%
    select(sector, bond, value)

p8 <- heatmap_data %>%
    ggplot(aes(x = bond, y = sector, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
        low = "#2C3E50", mid = "#3498DB", high = "#E74C3C",
        midpoint = 0.25,
        labels = scales::percent_format(accuracy = 1),
        name = "Holdings %"
    ) +
    labs(
        title = "Holdings Distribution: Top 10 Bonds by Sector",
        subtitle = paste("As of", format(max(all_bonds_pct$file_date), "%B %Y")),
        x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
    )

print(p8)
# ggsave("holdings_heatmap.png", p8, width = 12, height = 8, dpi = 300)

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n\n=== Summary Statistics ===\n")

cat("\nData Coverage:\n")
cat("  Historical time series:",
    format(min(holdings_long$date), "%B %Y"), "to",
    format(max(holdings_long$date), "%B %Y"), "\n")
cat("  Bond-specific data:",
    format(min(all_bonds_values$file_date), "%B %Y"), "to",
    format(max(all_bonds_values$file_date), "%B %Y"), "\n")

cat("\nLatest Month Summary:\n")
latest_date <- max(holdings_long$date)
latest_summary <- holdings_long %>%
    filter(date == latest_date) %>%
    arrange(desc(percentage))

for (i in 1:nrow(latest_summary)) {
    cat(sprintf("  %s: %.1f%%\n",
                latest_summary$sector[i],
                latest_summary$percentage[i] * 100))
}

cat("\nBond Type Distribution (Latest):\n")
for (i in 1:nrow(bond_type_summary)) {
    cat(sprintf("  %s: R%.1fbn (%.1f%%, %d bonds)\n",
                bond_type_summary$bond_type[i],
                bond_type_summary$total_value_bn[i],
                bond_type_summary$percentage[i] * 100,
                bond_type_summary$n_bonds[i]))
}

cat("\n=== Analysis Complete ===\n")
cat("\nTo save plots, uncomment the ggsave() lines in the script.\n")
