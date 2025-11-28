process_holdings_timeseries <- function(source_folder = "bond_holdings",
                                        output_folder = "bond_holdings_rds",
                                        overwrite = FALSE,
                                        verbose = TRUE) {

    # Load required packages
    require(readxl)
    require(dplyr)
    require(tidyr)
    require(lubridate)
    require(stringr)

    # Create output folder if it doesn't exist
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }

    # Helper function for filling down NA values (zoo fallback)
    fill_down_na <- function(vec) {
        if (requireNamespace("zoo", quietly = TRUE)) {
            return(zoo::na.locf(vec, na.rm = FALSE))
        } else {
            # Base R fallback
            result <- vec
            current_val <- NA
            for (i in seq_along(result)) {
                if (!is.na(result[i])) {
                    current_val <- result[i]
                } else {
                    result[i] <- current_val
                }
            }
            return(result)
        }
    }

    # Get list of Excel files - we'll use the most recent one
    # (since Holdings sheet contains complete historical data)
    # Only .xlsx format (files from Jan 2023 onwards)
    excel_files <- list.files(source_folder,
                              pattern = "Historical government bond holdings.*\\.xlsx$",
                              full.names = TRUE)

    if (length(excel_files) == 0) {
        stop("No Excel files found in the specified folder.")
    }

    # Get the most recent VALID file
    if (verbose) cat("Finding most recent valid file...\n")

    file_info <- data.frame(
        filepath = excel_files,
        filename = basename(excel_files),
        date = as.Date(NA),
        is_valid = FALSE,
        stringsAsFactors = FALSE
    )

    # Extract dates and validate each file
    for (i in 1:nrow(file_info)) {
        filename <- file_info$filename[i]
        filepath <- file_info$filepath[i]

        # Extract date
        pattern <- "Historical government bond holdings ([A-Za-z]+) ([0-9]{4})\\.xlsx"
        matches <- regmatches(filename, regexec(pattern, filename))
        if (length(matches[[1]]) == 3) {
            date_string <- paste("01", matches[[1]][2], matches[[1]][3])
            file_info$date[i] <- tryCatch(
                as.Date(date_string, format = "%d %B %Y"),
                error = function(e) NA
            )
        }

        # Validate file
        if (file.exists(filepath) && file.size(filepath) > 10000) {
            tryCatch({
                test_sheets <- excel_sheets(filepath)
                if (length(test_sheets) > 0) {
                    file_info$is_valid[i] <- TRUE
                }
            }, error = function(e) {
                file_info$is_valid[i] <<- FALSE
            })
        }
    }

    # Get most recent valid file
    valid_files <- file_info[file_info$is_valid & !is.na(file_info$date), ]

    if (nrow(valid_files) == 0) {
        stop("No valid Excel files found in folder: ", source_folder,
             "\nAll files appear to be corrupted. Try running: repair_bond_files()")
    }

    most_recent_file <- valid_files$filepath[which.max(valid_files$date)]
    most_recent_date <- valid_files$date[which.max(valid_files$date)]

    if (verbose) {
        cat(sprintf("Using most recent valid file: %s\n", basename(most_recent_file)))
        cat(sprintf("File date: %s\n", format(most_recent_date, "%B %Y")))
        cat(sprintf("Valid files available: %d out of %d total\n\n", nrow(valid_files), length(excel_files)))
    }

    # Check if Holdings sheet exists
    sheets <- excel_sheets(most_recent_file)
    if (!"Holdings" %in% sheets) {
        stop("Holdings sheet not found in the file.")
    }

    # Read the Holdings sheet
    if (verbose) cat("Reading Holdings sheet...\n")
    holdings_raw <- read_excel(most_recent_file, sheet = "Holdings", skip = 0)

    # Rename columns for consistency
    col_names <- names(holdings_raw)
    col_names[1] <- "period"
    # Column 2 contains the month names (may be named "...2" or similar)
    if (length(col_names) >= 2) {
        col_names[2] <- "month"
    }
    col_names <- str_trim(col_names)  # Remove trailing spaces
    names(holdings_raw) <- col_names

    if (verbose) cat(sprintf("Raw data: %d rows, %d columns\n", nrow(holdings_raw), ncol(holdings_raw)))

    # Process the data - handle the nested year/month structure
    if (verbose) cat("\nProcessing nested time structure...\n")

    holdings_processed <- holdings_raw %>%
        mutate(
            # Identify year rows - match year with optional colon (e.g., "2017:" or "2025")
            is_year_row = !is.na(period) & str_detect(period, "^\\d{4}:?$"),
            # Extract year from period column when it contains a year
            year = if_else(is_year_row,
                           as.integer(str_extract(period, "\\d{4}")),
                           NA_integer_),
            # Check if this is an annual-only row (year marker with no month data)
            # Annual rows have year in period but NA/empty in month column
            is_annual_only = is_year_row & (is.na(month) | str_trim(month) == "")
        )

    # Fill down the year for subsequent month rows (use helper to avoid zoo dependency)
    holdings_processed$year <- fill_down_na(holdings_processed$year)

    holdings_processed <- holdings_processed %>%
        filter(!is_annual_only) %>%  # Remove annual-only rows (keep year rows that have month data)
        mutate(
            # Get month name from the month column (column 2), not period
            month_name = str_trim(month),
            # Create proper date
            date = case_when(
                # For rows with a valid month name (no digits)
                !is.na(month_name) & month_name != "" & !str_detect(month_name, "\\d{4}") ~
                    as.Date(paste("01", month_name, year), format = "%d %B %Y"),
                # For any other format
                TRUE ~ NA_Date_
            )
        ) %>%
        select(-period, -month, -is_year_row, -is_annual_only, -month_name, -year) %>%
        filter(!is.na(date)) %>%
        arrange(date)

    if (verbose) cat(sprintf("Processed data: %d rows\n", nrow(holdings_processed)))

    # Create wide format (as is)
    holdings_wide <- holdings_processed

    # Remove any auto-generated unnamed columns (like ...2, ...3, etc.) before pivoting
    # These are created when Excel file has empty/unnamed columns
    unnamed_cols <- grep("^\\.\\.\\.\\d+$", names(holdings_wide), value = TRUE)
    if (length(unnamed_cols) > 0) {
        if (verbose) cat(sprintf("Removing %d unnamed column(s): %s\n",
                    length(unnamed_cols),
                    paste(unnamed_cols, collapse = ", ")))
        holdings_wide <- holdings_wide %>%
            select(-any_of(unnamed_cols))
    }

    # Ensure all non-date columns are numeric (convert any character columns)
    holdings_wide <- holdings_wide %>%
        mutate(across(-date, ~as.numeric(as.character(.))))

    # Create long format
    if (verbose) cat("Creating long format...\n")

    sector_cols <- setdiff(names(holdings_wide), "date")

    holdings_long <- holdings_wide %>%
        pivot_longer(
            cols = all_of(sector_cols),
            names_to = "sector",
            values_to = "percentage"
        ) %>%
        mutate(
            sector = str_trim(sector),
            percentage = as.numeric(percentage),
            # Replace NA with 0 instead of filtering - ensures continuous data for area charts
            percentage = ifelse(is.na(percentage), 0, percentage)
        ) %>%
        # Filter out any invalid sectors (but keep rows with 0% holdings)
        filter(
            !is.na(sector),
            sector != "",
            !grepl("^NA$", sector, ignore.case = TRUE),
            !grepl("TOTAL", sector, ignore.case = TRUE)
        ) %>%
        arrange(date, sector)

    # Save datasets
    if (verbose) cat("\n=== Saving RDS files ===\n")

    # Save wide format
    wide_path <- file.path(output_folder, "holdings_historical_wide.rds")
    if (!file.exists(wide_path) || overwrite) {
        saveRDS(holdings_wide, wide_path)
        if (verbose) cat(sprintf("✓ Saved: holdings_historical_wide.rds (%d rows, %d cols)\n",
                    nrow(holdings_wide), ncol(holdings_wide)))
    } else {
        if (verbose) cat(sprintf("⊘ Skipped (already exists): holdings_historical_wide.rds\n"))
    }

    # Save long format
    long_path <- file.path(output_folder, "holdings_historical_long.rds")
    if (!file.exists(long_path) || overwrite) {
        saveRDS(holdings_long, long_path)
        if (verbose) cat(sprintf("✓ Saved: holdings_historical_long.rds (%d rows, %d cols)\n",
                    nrow(holdings_long), ncol(holdings_long)))
    } else {
        if (verbose) cat(sprintf("⊘ Skipped (already exists): holdings_historical_long.rds\n"))
    }

    # Print summary
    if (verbose) {
        cat("\n=== Summary ===\n")
        cat(sprintf("Date range: %s to %s\n",
                    min(holdings_wide$date),
                    max(holdings_wide$date)))
        cat(sprintf("Number of observations: %d\n", nrow(holdings_wide)))
        cat(sprintf("Sectors: %s\n", paste(sector_cols, collapse = ", ")))
    }

    # Return both formats
    invisible(list(
        wide = holdings_wide,
        long = holdings_long
    ))
}

# Complete processing workflow function
process_all_sa_bond_data <- function(source_folder = "bond_holdings",
                                     output_folder = "bond_holdings_rds",
                                     overwrite = FALSE,
                                     verbose = TRUE) {

    if (verbose) {
        cat(strrep("=", 70), "\n")
        cat("PROCESSING ALL SA BOND HOLDINGS DATA\n")
        cat(strrep("=", 70), "\n\n")

        # Process historical holdings time series
        cat("STEP 1: Processing Historical Holdings Time Series\n")
        cat(strrep("-", 70), "\n")
    }

    holdings_result <- process_holdings_timeseries(
        source_folder = source_folder,
        output_folder = output_folder,
        overwrite = overwrite,
        verbose = verbose
    )

    if (verbose) {
        cat("\n")
        cat(strrep("=", 70), "\n\n")

        # Process all bond holdings by type
        cat("STEP 2: Processing Bond Holdings by Type and Sector\n")
        cat(strrep("-", 70), "\n")
    }

    bonds_result <- process_sa_bond_holdings_tidy(
        source_folder = source_folder,
        output_folder = output_folder,
        create_long_format = TRUE,
        overwrite = overwrite,
        verbose = verbose
    )

    if (verbose) {
        cat("\n")
        cat(strrep("=", 70), "\n")
        cat("PROCESSING COMPLETE!\n")
        cat(strrep("=", 70), "\n\n")

        cat("All datasets saved to:", output_folder, "\n\n")

        cat("Available datasets:\n")
        cat("  1. holdings_historical_wide.rds / holdings_historical_long.rds\n")
        cat("  2. fixed_rate_values_wide.rds / fixed_rate_values_long.rds\n")
        cat("  3. fixed_rate_pct_wide.rds / fixed_rate_pct_long.rds\n")
        cat("  4. ilb_values_wide.rds / ilb_values_long.rds\n")
        cat("  5. ilb_pct_wide.rds / ilb_pct_long.rds\n")
        cat("  6. frn_values_wide.rds / frn_values_long.rds\n")
        cat("  7. frn_pct_wide.rds / frn_pct_long.rds\n")
        cat("  8. sukuk_values_wide.rds / sukuk_values_long.rds\n")
        cat("  9. sukuk_pct_wide.rds / sukuk_pct_long.rds\n")
        cat(" 10. all_bonds_values_long.rds\n")
        cat(" 11. all_bonds_pct_long.rds\n\n")
    }

    invisible(list(
        holdings = holdings_result,
        bonds = bonds_result
    ))
}

# Example usage:
# ------------------------
# library(readxl)
# library(dplyr)
# library(tidyr)
# library(lubridate)
# library(stringr)
#
# # Process everything at once
# results <- process_all_sa_bond_data(
#   source_folder = "bond_holdings",
#   output_folder = "bond_holdings_rds",
#   overwrite = FALSE
# )
#
# # Or process individually:
#
# # 1. Process historical holdings time series
# holdings <- process_holdings_timeseries(
#   source_folder = "bond_holdings",
#   output_folder = "bond_holdings_rds"
# )
#
# # 2. Process bond-specific data
# bonds <- process_sa_bond_holdings_tidy(
#   source_folder = "bond_holdings",
#   output_folder = "bond_holdings_rds",
#   create_long_format = TRUE
# )
#
# # Load and use the data
# holdings_ts <- load_bond_data("holdings", format = "long")  # This won't work yet
# # Use readRDS directly for holdings:
# holdings_long <- readRDS("bond_holdings_rds/holdings_historical_long.rds")
#
# # Example: Plot non-resident holdings over time
# library(ggplot2)
#
# holdings_long %>%
#   filter(sector == "Non-residents") %>%
#   ggplot(aes(x = date, y = percentage)) +
#   geom_line(linewidth = 1) +
#   scale_y_continuous(labels = scales::percent) +
#   labs(title = "Non-Resident Holdings of SA Government Bonds",
#        subtitle = "Percentage of total outstanding",
#        x = NULL, y = "Holdings (%)") +
#   theme_minimal()