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
                                      sleep_time = 1) {

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

    # If download_latest is TRUE, filter to only missing files
    if (download_latest) {
        existing_files <- list.files(dest_folder, pattern = "Historical government bond holdings.*\\.(xls|xlsx)$")

        if (length(existing_files) > 0) {
            # Extract dates from existing filenames
            extract_date <- function(filename) {
                # Extract month and year from filename (handles both .xls and .xlsx, and "Feb" variant)
                pattern <- "Historical government bond holdings ([A-Za-z]+) ([0-9]{4})\\.(xls|xlsx)"
                matches <- regmatches(filename, regexec(pattern, filename))

                if (length(matches[[1]]) >= 3) {
                    month_name <- matches[[1]][2]
                    year <- matches[[1]][3]

                    # Handle "Feb" abbreviation
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

            existing_dates <- sapply(existing_files, extract_date)
            existing_dates <- as.Date(existing_dates[!is.na(existing_dates)], origin = "1970-01-01")

            # Find missing dates by comparing all_dates with existing_dates
            missing_dates <- all_dates[!all_dates %in% existing_dates]

            cat(sprintf("Found %d existing files in folder.\n", length(existing_files)))
            cat(sprintf("Checking for missing files between %s and %s...\n",
                        format(start, "%B %Y"),
                        format(end, "%B %Y")))
            cat(sprintf("Missing files: %d\n\n", length(missing_dates)))

            # Update dates to only missing ones
            dates <- missing_dates

        } else {
            cat("No existing files found. Downloading all files in date range.\n\n")
            dates <- all_dates
        }
    } else {
        dates <- all_dates
    }

    # Check if there are any dates to download
    if (length(dates) == 0) {
        cat("No new files to download. Your folder is up to date!\n")
        return(invisible(data.frame()))
    }

    # Base URL
    base_url <- "https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Holdings%20of%20Domestic%20Bonds/"

    # Date threshold for format change (January 2023)
    format_change_date <- as.Date("2023-01-01")

    # Initialize results tracking
    results <- data.frame(
        date = character(),
        status = character(),
        filename = character(),
        stringsAsFactors = FALSE
    )

    # Loop through each date
    for (date in dates) {
        date <- as.Date(date, origin = "1970-01-01")

        # Format the date for the URL
        month_name <- format(date, "%B")  # Full month name (e.g., "February")
        year <- format(date, "%Y")

        # Special case: February 2025 uses "Feb" instead of "February"
        if (date == as.Date("2025-02-01")) {
            month_name <- "Feb"
        }

        # Determine file extension based on date
        file_ext <- if (date >= format_change_date) "xlsx" else "xls"

        # Construct filename
        filename <- paste0("Historical government bond holdings ", month_name, " ", year, ".", file_ext)

        # Destination path
        dest_file <- file.path(dest_folder, filename)

        # Skip if file already exists (extra safety check)
        if (file.exists(dest_file)) {
            cat(sprintf("Skipping %s %s (already exists)...\n", month_name, year))
            next
        }

        # Construct URL with proper encoding
        encoded_filename <- URLencode(filename, reserved = TRUE)
        url <- paste0(base_url, encoded_filename)

        # Try to download
        cat(sprintf("Attempting to download: %s %s (.%s)...", month_name, year, file_ext))

        tryCatch({
            response <- GET(url, write_disk(dest_file, overwrite = TRUE))

            if (status_code(response) == 200) {
                cat(" SUCCESS\n")
                results <- rbind(results, data.frame(
                    date = as.character(date),
                    status = "Downloaded",
                    filename = filename
                ))
            } else {
                cat(sprintf(" FAILED (HTTP %d)\n", status_code(response)))
                # Remove failed file if it was created
                if (file.exists(dest_file)) file.remove(dest_file)

                results <- rbind(results, data.frame(
                    date = as.character(date),
                    status = paste0("Failed - HTTP ", status_code(response)),
                    filename = filename
                ))
            }
        }, error = function(e) {
            cat(sprintf(" ERROR: %s\n", e$message))
            results <<- rbind(results, data.frame(
                date = as.character(date),
                status = paste0("Error: ", e$message),
                filename = filename
            ))
        })

        # Sleep to avoid hammering the server
        Sys.sleep(sleep_time)
    }

    # Print summary
    cat("\n=== Download Summary ===\n")
    cat(sprintf("Total attempts: %d\n", nrow(results)))
    cat(sprintf("Successful downloads: %d\n", sum(results$status == "Downloaded")))
    cat(sprintf("Failed downloads: %d\n", sum(results$status != "Downloaded")))

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