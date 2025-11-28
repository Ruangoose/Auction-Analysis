# ============================================================================
# SA Government Bond Holdings - Download Function (CORRECTED)
# ============================================================================
#
# This is the corrected version that accounts for the 1-month publication lag
# Treasury publishes data for month X in month X+1
#
# IMPORTANT: Use this instead of your original download_sa_bond_holdings function
# ============================================================================

download_sa_bond_holdings <- function(start_date = "2018-02-01",
                                      end_date = NULL,  # Will default to previous month
                                      dest_folder = "bond_holdings",
                                      download_latest = FALSE,
                                      sleep_time = 1,
                                      max_retries = 3,
                                      verbose = TRUE) {

    # Load required packages
    require(httr)
    require(lubridate)

    # If end_date is NULL, set it to the last day of the previous month
    if (is.null(end_date)) {
        end_date <- floor_date(Sys.Date(), "month") - days(1)
    }

    # Create destination folder if it doesn't exist
    if (!dir.exists(dest_folder)) {
        dir.create(dest_folder, recursive = TRUE)
    }

    # Generate sequence of all months in the range
    start <- floor_date(as.Date(start_date), "month")
    end <- floor_date(as.Date(end_date), "month")
    all_dates <- seq(start, end, by = "month")

    # Helper function to extract date from filename
    extract_date_from_filename <- function(filename) {
        # Extract month and year from filename (handles both .xls and .xlsx, and "Feb" variant)
        pattern <- "Historical government bond holdings ([A-Za-z]+) ([0-9]{4})\\.(xls|xlsx)"
        matches <- regmatches(filename, regexec(pattern, filename))

        if (length(matches[[1]]) >= 3) {
            month_name <- matches[[1]][2]
            year <- matches[[1]][3]

            # Handle "Feb" abbreviation (Feb 2025 special case)
            if (month_name == "Feb") {
                month_name <- "February"
            }

            date_string <- paste("01", month_name, year)
            tryCatch({
                return(as.Date(date_string, format = "%d %B %Y"))
            }, error = function(e) {
                return(NA)
            })
        }
        return(NA)
    }

    # If download_latest is TRUE, filter to only missing files
    if (download_latest) {
        existing_files <- list.files(dest_folder, pattern = "Historical government bond holdings.*\\.(xls|xlsx)$")

        if (length(existing_files) > 0) {
            existing_dates <- sapply(existing_files, extract_date_from_filename)
            existing_dates <- as.Date(existing_dates[!is.na(existing_dates)], origin = "1970-01-01")

            # Find missing dates by comparing all_dates with existing_dates
            missing_dates <- all_dates[!all_dates %in% existing_dates]

            if (verbose) {
                cat(sprintf("Found %d existing files in folder.\n", length(existing_files)))
                cat(sprintf("Checking for missing files between %s and %s...\n",
                            format(start, "%B %Y"),
                            format(end, "%B %Y")))
                cat(sprintf("Missing files: %d\n\n", length(missing_dates)))
            }

            # Update dates to only missing ones
            dates <- missing_dates

        } else {
            if (verbose) cat("No existing files found. Downloading all files in date range.\n\n")
            dates <- all_dates
        }
    } else {
        dates <- all_dates
    }

    # Check if there are any dates to download
    if (length(dates) == 0) {
        if (verbose) cat("No new files to download. Your folder is up to date!\n")
        return(invisible(data.frame()))
    }

    # Base URL
    base_url <- "https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Holdings%20of%20Domestic%20Bonds/"

    # Date threshold for format change (January 2023)
    format_change_date <- as.Date("2023-01-01")

    # Known filename variations (Treasury sometimes uses abbreviations)
    known_variations <- list(
        "2025-02-01" = "Feb"  # February 2025 uses "Feb" instead of "February"
    )

    # Initialize results tracking
    results <- data.frame(
        date = character(),
        status = character(),
        filename = character(),
        retries = integer(),
        stringsAsFactors = FALSE
    )

    # Helper function to attempt download with retry logic
    download_with_retry <- function(url, dest_file, max_retries, sleep_time) {
        for (attempt in 1:max_retries) {
            tryCatch({
                response <- GET(url,
                               write_disk(dest_file, overwrite = TRUE),
                               timeout(60))

                if (status_code(response) == 200) {
                    # Verify file is not empty/corrupted
                    if (file.exists(dest_file) && file.size(dest_file) > 1000) {
                        return(list(success = TRUE, status = "Downloaded", attempts = attempt))
                    } else {
                        if (file.exists(dest_file)) file.remove(dest_file)
                        return(list(success = FALSE, status = "File too small or empty", attempts = attempt))
                    }
                } else {
                    if (file.exists(dest_file)) file.remove(dest_file)
                    if (attempt < max_retries) {
                        Sys.sleep(sleep_time * attempt)  # Exponential backoff
                        next
                    }
                    return(list(success = FALSE, status = paste0("HTTP ", status_code(response)), attempts = attempt))
                }
            }, error = function(e) {
                if (file.exists(dest_file)) file.remove(dest_file)
                if (attempt < max_retries) {
                    Sys.sleep(sleep_time * attempt)
                    return(NULL)  # Signal to retry
                }
                return(list(success = FALSE, status = paste0("Error: ", e$message), attempts = attempt))
            })
        }
        return(list(success = FALSE, status = "Max retries exceeded", attempts = max_retries))
    }

    # Loop through each date
    for (date in dates) {
        date <- as.Date(date, origin = "1970-01-01")
        date_str <- as.character(date)

        # Format the date for the URL
        month_name <- format(date, "%B")  # Full month name (e.g., "February")
        year <- format(date, "%Y")

        # Check for known filename variations
        if (date_str %in% names(known_variations)) {
            month_name <- known_variations[[date_str]]
        }

        # Determine file extension based on date
        file_ext <- if (date >= format_change_date) "xlsx" else "xls"

        # Construct filename
        filename <- paste0("Historical government bond holdings ", month_name, " ", year, ".", file_ext)

        # Destination path
        dest_file <- file.path(dest_folder, filename)

        # Skip if file already exists (extra safety check)
        if (file.exists(dest_file)) {
            if (verbose) cat(sprintf("Skipping %s %s (already exists)...\n", month_name, year))
            next
        }

        # Construct URL with proper encoding
        encoded_filename <- URLencode(filename, reserved = TRUE)
        url <- paste0(base_url, encoded_filename)

        # Try to download with retry
        if (verbose) cat(sprintf("Downloading: %s %s (.%s)...", month_name, year, file_ext))

        result <- download_with_retry(url, dest_file, max_retries, sleep_time)

        if (is.null(result)) {
            # Retry logic returned NULL, try again
            result <- download_with_retry(url, dest_file, max_retries, sleep_time)
        }

        if (result$success) {
            if (verbose) cat(sprintf(" SUCCESS%s\n", if(result$attempts > 1) paste0(" (attempt ", result$attempts, ")") else ""))
        } else {
            if (verbose) cat(sprintf(" FAILED (%s)\n", result$status))
        }

        results <- rbind(results, data.frame(
            date = date_str,
            status = if(result$success) "Downloaded" else paste0("Failed - ", result$status),
            filename = filename,
            retries = result$attempts
        ))

        # Sleep to avoid hammering the server
        Sys.sleep(sleep_time)
    }

    # Print summary
    if (verbose) {
        cat("\n=== Download Summary ===\n")
        cat(sprintf("Total attempts: %d\n", nrow(results)))
        cat(sprintf("Successful downloads: %d\n", sum(results$status == "Downloaded")))
        cat(sprintf("Failed downloads: %d\n", sum(results$status != "Downloaded")))

        if (any(results$retries > 1)) {
            cat(sprintf("Downloads requiring retries: %d\n", sum(results$retries > 1)))
        }
    }

    return(invisible(results))
}

# ============================================================================
# IMPORTANT NOTES ABOUT THIS FUNCTION
# ============================================================================
#
# This function handles several nuances in the Treasury data:
#
# 1. FILE EXTENSION CHANGES (January 2023)
#    - Before Jan 2023: .xls format
#    - Jan 2023 onwards: .xlsx format
#    - Function automatically uses correct extension based on date
#
# 2. SPECIAL FILENAME CASE (February 2025)
#    - February 2025 uses "Feb" instead of "February" in filename
#    - Example: "Historical government bond holdings Feb 2025.xlsx"
#    - All other months use full month name
#
# 3. PUBLICATION LAG (1 month)
#    - Data for month X is published in month X+1
#    - Default end_date = NULL automatically stops at previous month
#    - Example: Running in November 2025 downloads up to October 2025
#
# 4. FILE VALIDATION
#    - Both .xls and .xlsx files are handled in pattern matching
#    - "Feb" abbreviation is normalized to "February" during date extraction
#    - Failed downloads are automatically removed
#
# ============================================================================

# Usage Examples:
#
# # Download all available data (stops at last month automatically)
# download_sa_bond_holdings()
#
# # Download only missing files
# download_sa_bond_holdings(download_latest = TRUE)
#
# # Download specific range (will use .xls for 2018-2022, .xlsx for 2023+)
# download_sa_bond_holdings(start_date = "2018-01-01", end_date = "2025-10-31")
#