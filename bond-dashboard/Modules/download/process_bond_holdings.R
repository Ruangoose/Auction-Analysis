process_sa_bond_holdings <- function(source_folder = "bond_holdings",
                                     output_folder = "bond_holdings_rds",
                                     overwrite = FALSE) {

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

    # Get list of Excel files
    excel_files <- list.files(source_folder,
                              pattern = "Historical government bond holdings.*\\.xlsx$",
                              full.names = TRUE)

    if (length(excel_files) == 0) {
        stop("No Excel files found in the specified folder.")
    }

    cat(sprintf("Found %d Excel files to process.\n\n", length(excel_files)))

    # Helper function to validate Excel file
    is_valid_excel <- function(filepath) {
        # Check if file exists and has size > 0
        if (!file.exists(filepath)) return(FALSE)
        if (file.size(filepath) < 1000) return(FALSE)  # Less than 1KB is suspicious

        # Try to get sheet names as a validation test
        tryCatch({
            sheets <- excel_sheets(filepath)
            return(length(sheets) > 0)
        }, error = function(e) {
            return(FALSE)
        })
    }

    # Initialize storage for all data
    all_holdings <- list()
    all_fixed_rate_values <- list()
    all_fixed_rate_pct <- list()
    all_ilb_values <- list()
    all_ilb_pct <- list()
    all_frn_values <- list()
    all_frn_pct <- list()
    all_sukuk_values <- list()
    all_sukuk_pct <- list()

    # Process each file
    for (file_path in excel_files) {
        filename <- basename(file_path)
        cat(sprintf("Processing: %s\n", filename))

        # Validate file first
        if (!is_valid_excel(file_path)) {
            cat(sprintf("  ✗ SKIPPED - File is corrupted or invalid (size: %d bytes)\n",
                        file.size(file_path)))
            cat(sprintf("     Consider re-downloading this file.\n"))
            next
        }

        # Extract date from filename
        pattern <- "Historical government bond holdings ([A-Za-z]+) ([0-9]{4})\\.xlsx"
        matches <- regmatches(filename, regexec(pattern, filename))

        if (length(matches[[1]]) == 3) {
            month_name <- matches[[1]][2]
            year <- matches[[1]][3]
            date_string <- paste("01", month_name, year)
            file_date <- as.Date(date_string, format = "%d %B %Y")
        } else {
            cat(sprintf("  Warning: Could not extract date from filename. Skipping.\n"))
            next
        }

        # Get sheet names
        sheets <- excel_sheets(file_path)

        # Process each sheet type
        tryCatch({
            # 1. Holdings (historical time series)
            if ("Holdings" %in% sheets) {
                holdings <- read_excel(file_path, sheet = "Holdings", skip = 0)
                # Add file date as a column
                holdings$file_date <- file_date
                all_holdings[[filename]] <- holdings
            }

            # 2. Fixed rate bond (Values)
            if ("Fixed rate bond (Values)" %in% sheets) {
                fixed_values <- read_excel(file_path, sheet = "Fixed rate bond (Values)", skip = 1)
                # Remove the date header row that becomes column names
                # First row is the date, second row should be headers
                fixed_values$file_date <- file_date
                all_fixed_rate_values[[filename]] <- fixed_values
            }

            # 3. Fixed rate bond % (Values)
            if ("Fixed rate bond % (Values)" %in% sheets) {
                fixed_pct <- read_excel(file_path, sheet = "Fixed rate bond % (Values)", skip = 1)
                fixed_pct$file_date <- file_date
                all_fixed_rate_pct[[filename]] <- fixed_pct
            }

            # 4. ILB (Values)
            if ("ILB (Values)" %in% sheets) {
                ilb_values <- read_excel(file_path, sheet = "ILB (Values)", skip = 1)
                ilb_values$file_date <- file_date
                all_ilb_values[[filename]] <- ilb_values
            }

            # 5. ILB % (Values)
            if ("ILB % (Values)" %in% sheets) {
                ilb_pct <- read_excel(file_path, sheet = "ILB % (Values)", skip = 1)
                ilb_pct$file_date <- file_date
                all_ilb_pct[[filename]] <- ilb_pct
            }

            # 6. FRN (Values)
            if ("FRN (Values)" %in% sheets) {
                frn_values <- read_excel(file_path, sheet = "FRN (Values)", skip = 1)
                frn_values$file_date <- file_date
                all_frn_values[[filename]] <- frn_values
            }

            # 7. FRN % (Values)
            if ("FRN % (Values)" %in% sheets) {
                frn_pct <- read_excel(file_path, sheet = "FRN % (Values)", skip = 1)
                frn_pct$file_date <- file_date
                all_frn_pct[[filename]] <- frn_pct
            }

            # 8. Sukuk (Values)
            if ("Sukuk (Values)" %in% sheets) {
                sukuk_values <- read_excel(file_path, sheet = "Sukuk (Values)", skip = 1)
                sukuk_values$file_date <- file_date
                all_sukuk_values[[filename]] <- sukuk_values
            }

            # 9. Sukuk % (Values)
            if ("Sukuk % (Values)" %in% sheets) {
                sukuk_pct <- read_excel(file_path, sheet = "Sukuk % (Values)", skip = 1)
                sukuk_pct$file_date <- file_date
                all_sukuk_pct[[filename]] <- sukuk_pct
            }

            cat(sprintf("  ✓ Successfully processed\n"))

        }, error = function(e) {
            cat(sprintf("  ✗ Error processing file: %s\n", e$message))
        })
    }

    # Combine all data by sheet type
    cat("\n=== Combining data ===\n")

    # Helper function to safely bind rows
    safe_bind_rows <- function(data_list, list_name) {
        if (length(data_list) > 0) {
            cat(sprintf("Combining %d files for %s...\n", length(data_list), list_name))

            # Coerce all numeric columns to actually be numeric
            # This handles cases where some files have "k" or other text in numeric cells
            data_list_coerced <- lapply(data_list, function(df) {
                # Get all columns except Sector code, ...1 (unnamed first column), and file_date
                numeric_cols <- setdiff(names(df), c("Sector code", "...1", "file_date"))

                # Convert to numeric, suppressing warnings about NAs from non-numeric values
                df %>%
                    mutate(across(
                        all_of(numeric_cols),
                        ~ suppressWarnings(as.numeric(as.character(.)))
                    ))
            })

            combined <- bind_rows(data_list_coerced)

            # Rename ...1 column to Sector if it exists
            if ("...1" %in% names(combined)) {
                combined <- combined %>% rename(Sector = ...1)
            }

            return(combined)
        } else {
            cat(sprintf("No data found for %s\n", list_name))
            return(NULL)
        }
    }

    # Helper function to process Holdings data with nested year/month structure
    process_holdings_data <- function(holdings_list) {
        if (length(holdings_list) == 0) {
            return(NULL)
        }

        cat(sprintf("Processing Holdings data from %d files...\n", length(holdings_list)))

        # Process each file's holdings data
        processed_list <- lapply(holdings_list, function(df) {
            # Standardize column names
            col_names <- names(df)
            col_names[1] <- "Period"
            if (length(col_names) >= 2 && (col_names[2] == "...2" || is.na(col_names[2]) || col_names[2] == "")) {
                col_names[2] <- "Month"
            }
            names(df) <- col_names

            # Process the nested year/month structure
            df_processed <- df %>%
                mutate(
                    # Identify year rows (contain just year and colon)
                    is_year = !is.na(Period) & str_detect(as.character(Period), "^\\d{4}:?$"),
                    # Extract year
                    year = if_else(is_year,
                                   as.integer(str_extract(as.character(Period), "\\d{4}")),
                                   NA_integer_)
                )

            # Fill down the year for subsequent month rows
            if (requireNamespace("zoo", quietly = TRUE)) {
                df_processed <- df_processed %>%
                    mutate(year = zoo::na.locf(year, na.rm = FALSE))
            } else {
                # Alternative without zoo package
                current_year <- NA_integer_
                for (i in 1:nrow(df_processed)) {
                    if (!is.na(df_processed$is_year[i]) && df_processed$is_year[i]) {
                        current_year <- df_processed$year[i]
                    }
                    if (is.na(df_processed$year[i])) {
                        df_processed$year[i] <- current_year
                    }
                }
            }

            # Remove year-only rows and process dates
            df_processed <- df_processed %>%
                filter(!is_year | is.na(is_year)) %>%
                mutate(
                    # Get month name from the Month column
                    month_name = if_else(!is.na(Month), str_trim(as.character(Month)), NA_character_)
                )

            # Create date column safely
            df_processed$date <- NA_Date_
            for (i in 1:nrow(df_processed)) {
                if (!is.na(df_processed$month_name[i]) && !is.na(df_processed$year[i])) {
                    # Try to parse month name with year
                    date_str <- paste("01", df_processed$month_name[i], df_processed$year[i])
                    df_processed$date[i] <- tryCatch(
                        as.Date(date_str, format = "%d %B %Y"),
                        error = function(e) NA_Date_
                    )
                } else if (is.na(df_processed$month_name[i]) && !is.na(df_processed$year[i])) {
                    # Use December as default for year-only rows
                    df_processed$date[i] <- as.Date(paste(df_processed$year[i], "12", "01", sep = "-"))
                }
            }

            df_processed <- df_processed %>%
                select(-Period, -Month, -is_year, -month_name, -year) %>%
                filter(!is.na(date))

            # Convert all remaining columns to numeric (they should be percentages/proportions)
            numeric_cols <- setdiff(names(df_processed), c("date", "file_date"))
            df_processed <- df_processed %>%
                mutate(across(
                    all_of(numeric_cols),
                    ~ suppressWarnings(as.numeric(as.character(.)))
                ))

            return(df_processed)
        })

        # Combine all processed data
        combined <- bind_rows(processed_list) %>%
            arrange(date, file_date)

        cat(sprintf("  ✓ Processed %d total rows\n", nrow(combined)))

        return(combined)
    }

    # Use special processing for Holdings data (has nested year/month structure)
    combined_holdings <- process_holdings_data(all_holdings)

    # Use regular processing for other sheets
    combined_fixed_values <- safe_bind_rows(all_fixed_rate_values, "Fixed Rate (Values)")
    combined_fixed_pct <- safe_bind_rows(all_fixed_rate_pct, "Fixed Rate (%)")
    combined_ilb_values <- safe_bind_rows(all_ilb_values, "ILB (Values)")
    combined_ilb_pct <- safe_bind_rows(all_ilb_pct, "ILB (%)")
    combined_frn_values <- safe_bind_rows(all_frn_values, "FRN (Values)")
    combined_frn_pct <- safe_bind_rows(all_frn_pct, "FRN (%)")
    combined_sukuk_values <- safe_bind_rows(all_sukuk_values, "Sukuk (Values)")
    combined_sukuk_pct <- safe_bind_rows(all_sukuk_pct, "Sukuk (%)")

    # Save all datasets as RDS
    cat("\n=== Saving RDS files ===\n")

    save_if_exists <- function(data, filename) {
        if (!is.null(data) && nrow(data) > 0) {
            filepath <- file.path(output_folder, filename)
            if (!file.exists(filepath) || overwrite) {
                saveRDS(data, filepath)
                cat(sprintf("✓ Saved: %s (%d rows, %d cols)\n",
                            filename, nrow(data), ncol(data)))
                return(TRUE)
            } else {
                cat(sprintf("⊘ Skipped (already exists): %s\n", filename))
                return(FALSE)
            }
        } else {
            cat(sprintf("⊘ No data to save for: %s\n", filename))
            return(FALSE)
        }
    }

    save_if_exists(combined_holdings, "holdings_historical.rds")
    save_if_exists(combined_fixed_values, "fixed_rate_bond_values.rds")
    save_if_exists(combined_fixed_pct, "fixed_rate_bond_pct.rds")
    save_if_exists(combined_ilb_values, "ilb_values.rds")
    save_if_exists(combined_ilb_pct, "ilb_pct.rds")
    save_if_exists(combined_frn_values, "frn_values.rds")
    save_if_exists(combined_frn_pct, "frn_pct.rds")
    save_if_exists(combined_sukuk_values, "sukuk_values.rds")
    save_if_exists(combined_sukuk_pct, "sukuk_pct.rds")

    # Create summary
    cat("\n=== Processing Summary ===\n")
    cat(sprintf("Total files processed: %d\n", length(excel_files)))
    cat(sprintf("Output folder: %s\n", output_folder))
    cat(sprintf("\nDatasets created:\n"))

    datasets <- list(
        "Holdings (Historical)" = combined_holdings,
        "Fixed Rate Values" = combined_fixed_values,
        "Fixed Rate Percentages" = combined_fixed_pct,
        "ILB Values" = combined_ilb_values,
        "ILB Percentages" = combined_ilb_pct,
        "FRN Values" = combined_frn_values,
        "FRN Percentages" = combined_frn_pct,
        "Sukuk Values" = combined_sukuk_values,
        "Sukuk Percentages" = combined_sukuk_pct
    )

    for (name in names(datasets)) {
        data <- datasets[[name]]
        if (!is.null(data)) {
            cat(sprintf("  • %s: %d rows, %d columns\n",
                        name, nrow(data), ncol(data)))
        }
    }

    # Return list of all datasets
    invisible(list(
        holdings = combined_holdings,
        fixed_rate_values = combined_fixed_values,
        fixed_rate_pct = combined_fixed_pct,
        ilb_values = combined_ilb_values,
        ilb_pct = combined_ilb_pct,
        frn_values = combined_frn_values,
        frn_pct = combined_frn_pct,
        sukuk_values = combined_sukuk_values,
        sukuk_pct = combined_sukuk_pct
    ))
}

# Function to load a specific dataset
load_bond_data <- function(dataset_name, folder = "bond_holdings_rds") {

    valid_names <- c(
        "holdings", "holdings_historical",
        "fixed_values", "fixed_rate_bond_values",
        "fixed_pct", "fixed_rate_bond_pct",
        "ilb_values",
        "ilb_pct",
        "frn_values",
        "frn_pct",
        "sukuk_values",
        "sukuk_pct"
    )

    # Map friendly names to actual filenames
    file_map <- list(
        "holdings" = "holdings_historical.rds",
        "holdings_historical" = "holdings_historical.rds",
        "fixed_values" = "fixed_rate_bond_values.rds",
        "fixed_rate_bond_values" = "fixed_rate_bond_values.rds",
        "fixed_pct" = "fixed_rate_bond_pct.rds",
        "fixed_rate_bond_pct" = "fixed_rate_bond_pct.rds",
        "ilb_values" = "ilb_values.rds",
        "ilb_pct" = "ilb_pct.rds",
        "frn_values" = "frn_values.rds",
        "frn_pct" = "frn_pct.rds",
        "sukuk_values" = "sukuk_values.rds",
        "sukuk_pct" = "sukuk_pct.rds"
    )

    if (!dataset_name %in% valid_names) {
        stop(sprintf("Invalid dataset name. Valid options are: %s",
                     paste(unique(names(file_map)), collapse = ", ")))
    }

    filename <- file_map[[dataset_name]]
    filepath <- file.path(folder, filename)

    if (!file.exists(filepath)) {
        stop(sprintf("File not found: %s\nHave you run process_sa_bond_holdings() first?",
                     filepath))
    }

    data <- readRDS(filepath)
    cat(sprintf("Loaded: %s (%d rows, %d cols)\n",
                filename, nrow(data), ncol(data)))

    return(data)
}

# Example usage:
# ------------------------
# # Process all Excel files and create RDS files
results <- process_sa_bond_holdings(
  source_folder = "bond_holdings",
  output_folder = "bond_holdings_rds",
  overwrite = FALSE
)
#
# # Load a specific dataset
# holdings <- load_bond_data("holdings")
# fixed_values <- load_bond_data("fixed_values")
# ilb_pct <- load_bond_data("ilb_pct")