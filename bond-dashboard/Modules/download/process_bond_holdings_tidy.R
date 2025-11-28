process_sa_bond_holdings_tidy <- function(source_folder = "bond_holdings",
                                          output_folder = "bond_holdings_rds",
                                          create_long_format = TRUE,
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

    # Initialize storage for all data
    all_fixed_rate_values <- list()
    all_fixed_rate_pct <- list()
    all_ilb_values <- list()
    all_ilb_pct <- list()
    all_frn_values <- list()
    all_frn_pct <- list()
    all_sukuk_values <- list()
    all_sukuk_pct <- list()

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

        # Helper function to process bond holdings sheets
        process_bond_sheet <- function(sheet_name, value_type = "value") {
            tryCatch({
                # Read the sheet, skipping the date header row
                data <- read_excel(file_path, sheet = sheet_name, skip = 1)

                # Rename first column to sector
                names(data)[1] <- "sector"

                # Add metadata columns
                data <- data %>%
                    mutate(
                        file_date = file_date,
                        data_type = value_type,
                        .before = 1
                    )

                # Clean up sector names (remove trailing spaces)
                data$sector <- str_trim(data$sector)

                return(data)
            }, error = function(e) {
                cat(sprintf("    Error processing %s: %s\n", sheet_name, e$message))
                return(NULL)
            })
        }

        # Process each sheet type
        tryCatch({
            # Fixed rate bonds
            if ("Fixed rate bond (Values)" %in% sheets) {
                fixed_values <- process_bond_sheet("Fixed rate bond (Values)", "value")
                if (!is.null(fixed_values)) {
                    all_fixed_rate_values[[filename]] <- fixed_values
                }
            }

            if ("Fixed rate bond % (Values)" %in% sheets) {
                fixed_pct <- process_bond_sheet("Fixed rate bond % (Values)", "percentage")
                if (!is.null(fixed_pct)) {
                    all_fixed_rate_pct[[filename]] <- fixed_pct
                }
            }

            # ILB
            if ("ILB (Values)" %in% sheets) {
                ilb_values <- process_bond_sheet("ILB (Values)", "value")
                if (!is.null(ilb_values)) {
                    all_ilb_values[[filename]] <- ilb_values
                }
            }

            if ("ILB % (Values)" %in% sheets) {
                ilb_pct <- process_bond_sheet("ILB % (Values)", "percentage")
                if (!is.null(ilb_pct)) {
                    all_ilb_pct[[filename]] <- ilb_pct
                }
            }

            # FRN
            if ("FRN (Values)" %in% sheets) {
                frn_values <- process_bond_sheet("FRN (Values)", "value")
                if (!is.null(frn_values)) {
                    all_frn_values[[filename]] <- frn_values
                }
            }

            if ("FRN % (Values)" %in% sheets) {
                frn_pct <- process_bond_sheet("FRN % (Values)", "percentage")
                if (!is.null(frn_pct)) {
                    all_frn_pct[[filename]] <- frn_pct
                }
            }

            # Sukuk
            if ("Sukuk (Values)" %in% sheets) {
                sukuk_values <- process_bond_sheet("Sukuk (Values)", "value")
                if (!is.null(sukuk_values)) {
                    all_sukuk_values[[filename]] <- sukuk_values
                }
            }

            if ("Sukuk % (Values)" %in% sheets) {
                sukuk_pct <- process_bond_sheet("Sukuk % (Values)", "percentage")
                if (!is.null(sukuk_pct)) {
                    all_sukuk_pct[[filename]] <- sukuk_pct
                }
            }

            cat(sprintf("  ✓ Successfully processed\n"))

        }, error = function(e) {
            cat(sprintf("  ✗ Error processing file: %s\n", e$message))
        })
    }

    # Combine all data by sheet type
    cat("\n=== Combining and tidying data ===\n")

    # Helper function to combine and optionally pivot to long format
    combine_and_tidy <- function(data_list, list_name, bond_type, create_long) {
        if (length(data_list) == 0) {
            cat(sprintf("No data found for %s\n", list_name))
            return(list(wide = NULL, long = NULL))
        }

        cat(sprintf("Processing %d files for %s...\n", length(data_list), list_name))

        # Coerce all numeric columns to handle mixed types (e.g., "k" in numeric cells)
        data_list_coerced <- lapply(data_list, function(df) {
            # Get all columns except file_date, data_type, and sector
            numeric_cols <- setdiff(names(df), c("file_date", "data_type", "sector"))

            # Convert to numeric, handling any non-numeric values
            df %>%
                mutate(across(
                    all_of(numeric_cols),
                    ~ suppressWarnings(as.numeric(as.character(.)))
                ))
        })

        # Combine all files
        combined_wide <- bind_rows(data_list_coerced) %>%
            arrange(file_date, sector)

        # Create long format if requested
        if (create_long) {
            combined_long <- combined_wide %>%
                pivot_longer(
                    cols = -c(file_date, data_type, sector),
                    names_to = "bond",
                    values_to = "value"
                ) %>%
                filter(!is.na(value)) %>%
                mutate(
                    bond_type = bond_type,
                    # Extract maturity year from bond name
                    maturity_year = as.integer(str_extract(bond, "\\d{4}"))
                ) %>%
                arrange(file_date, sector, bond)

            return(list(wide = combined_wide, long = combined_long))
        } else {
            return(list(wide = combined_wide, long = NULL))
        }
    }

    # Process each dataset
    fixed_values_data <- combine_and_tidy(all_fixed_rate_values, "Fixed Rate (Values)", "Fixed Rate", create_long_format)
    fixed_pct_data <- combine_and_tidy(all_fixed_rate_pct, "Fixed Rate (%)", "Fixed Rate", create_long_format)
    ilb_values_data <- combine_and_tidy(all_ilb_values, "ILB (Values)", "ILB", create_long_format)
    ilb_pct_data <- combine_and_tidy(all_ilb_pct, "ILB (%)", "ILB", create_long_format)
    frn_values_data <- combine_and_tidy(all_frn_values, "FRN (Values)", "FRN", create_long_format)
    frn_pct_data <- combine_and_tidy(all_frn_pct, "FRN (%)", "FRN", create_long_format)
    sukuk_values_data <- combine_and_tidy(all_sukuk_values, "Sukuk (Values)", "Sukuk", create_long_format)
    sukuk_pct_data <- combine_and_tidy(all_sukuk_pct, "Sukuk (%)", "Sukuk", create_long_format)

    # Save all datasets as RDS
    cat("\n=== Saving RDS files ===\n")

    save_dataset <- function(data_list, base_name) {
        saved_files <- c()

        # Save wide format
        if (!is.null(data_list$wide) && nrow(data_list$wide) > 0) {
            filepath <- file.path(output_folder, paste0(base_name, "_wide.rds"))
            if (!file.exists(filepath) || overwrite) {
                saveRDS(data_list$wide, filepath)
                cat(sprintf("✓ Saved: %s (%d rows, %d cols)\n",
                            basename(filepath), nrow(data_list$wide), ncol(data_list$wide)))
                saved_files <- c(saved_files, filepath)
            } else {
                cat(sprintf("⊘ Skipped (already exists): %s\n", basename(filepath)))
            }
        }

        # Save long format
        if (!is.null(data_list$long) && nrow(data_list$long) > 0) {
            filepath <- file.path(output_folder, paste0(base_name, "_long.rds"))
            if (!file.exists(filepath) || overwrite) {
                saveRDS(data_list$long, filepath)
                cat(sprintf("✓ Saved: %s (%d rows, %d cols)\n",
                            basename(filepath), nrow(data_list$long), ncol(data_list$long)))
                saved_files <- c(saved_files, filepath)
            } else {
                cat(sprintf("⊘ Skipped (already exists): %s\n", basename(filepath)))
            }
        }

        return(saved_files)
    }

    save_dataset(fixed_values_data, "fixed_rate_values")
    save_dataset(fixed_pct_data, "fixed_rate_pct")
    save_dataset(ilb_values_data, "ilb_values")
    save_dataset(ilb_pct_data, "ilb_pct")
    save_dataset(frn_values_data, "frn_values")
    save_dataset(frn_pct_data, "frn_pct")
    save_dataset(sukuk_values_data, "sukuk_values")
    save_dataset(sukuk_pct_data, "sukuk_pct")

    # Create a combined long format dataset with all bond types
    if (create_long_format) {
        cat("\n=== Creating combined dataset ===\n")

        all_values_long <- bind_rows(
            fixed_values_data$long,
            ilb_values_data$long,
            frn_values_data$long,
            sukuk_values_data$long
        ) %>%
            filter(data_type == "value") %>%
            select(-data_type)

        all_pct_long <- bind_rows(
            fixed_pct_data$long,
            ilb_pct_data$long,
            frn_pct_data$long,
            sukuk_pct_data$long
        ) %>%
            filter(data_type == "percentage") %>%
            select(-data_type)

        if (nrow(all_values_long) > 0) {
            filepath <- file.path(output_folder, "all_bonds_values_long.rds")
            if (!file.exists(filepath) || overwrite) {
                saveRDS(all_values_long, filepath)
                cat(sprintf("✓ Saved combined values: %s (%d rows)\n",
                            basename(filepath), nrow(all_values_long)))
            }
        }

        if (nrow(all_pct_long) > 0) {
            filepath <- file.path(output_folder, "all_bonds_pct_long.rds")
            if (!file.exists(filepath) || overwrite) {
                saveRDS(all_pct_long, filepath)
                cat(sprintf("✓ Saved combined percentages: %s (%d rows)\n",
                            basename(filepath), nrow(all_pct_long)))
            }
        }
    }

    # Create summary
    cat("\n=== Processing Summary ===\n")
    cat(sprintf("Total files processed: %d\n", length(excel_files)))
    cat(sprintf("Output folder: %s\n", output_folder))

    # Return list of all datasets
    invisible(list(
        fixed_rate_values = fixed_values_data,
        fixed_rate_pct = fixed_pct_data,
        ilb_values = ilb_values_data,
        ilb_pct = ilb_pct_data,
        frn_values = frn_values_data,
        frn_pct = frn_pct_data,
        sukuk_values = sukuk_values_data,
        sukuk_pct = sukuk_pct_data
    ))
}

# Function to load bond data (updated for new file structure)
load_bond_data <- function(dataset_name,
                           format = "long",
                           folder = "bond_holdings_rds") {

    # Validate format
    if (!format %in% c("wide", "long")) {
        stop("format must be either 'wide' or 'long'")
    }

    # Map dataset names to base filenames
    file_map <- c(
        "fixed_values" = "fixed_rate_values",
        "fixed_pct" = "fixed_rate_pct",
        "ilb_values" = "ilb_values",
        "ilb_pct" = "ilb_pct",
        "frn_values" = "frn_values",
        "frn_pct" = "frn_pct",
        "sukuk_values" = "sukuk_values",
        "sukuk_pct" = "sukuk_pct",
        "all_values" = "all_bonds_values",
        "all_pct" = "all_bonds_pct"
    )

    if (!dataset_name %in% names(file_map)) {
        stop(sprintf("Invalid dataset name. Valid options are: %s",
                     paste(names(file_map), collapse = ", ")))
    }

    base_name <- file_map[[dataset_name]]
    filename <- paste0(base_name, "_", format, ".rds")
    filepath <- file.path(folder, filename)

    if (!file.exists(filepath)) {
        stop(sprintf("File not found: %s\nHave you run process_sa_bond_holdings_tidy() first?",
                     filepath))
    }

    data <- readRDS(filepath)
    cat(sprintf("Loaded: %s (%d rows, %d cols)\n",
                filename, nrow(data), ncol(data)))

    return(data)
}

# Example usage:
# ------------------------
# # Process all Excel files and create RDS files (both wide and long format)
# results <- process_sa_bond_holdings_tidy(
#   source_folder = "bond_holdings",
#   output_folder = "bond_holdings_rds",
#   create_long_format = TRUE,
#   overwrite = FALSE
# )
#
# # Load datasets in long format (best for analysis and visualization)
# fixed_values <- load_bond_data("fixed_values", format = "long")
# all_bonds <- load_bond_data("all_values", format = "long")
#
# # Load in wide format (one row per date-sector combination)
# fixed_values_wide <- load_bond_data("fixed_values", format = "wide")
#
# # Example analysis: Plot holdings over time for a specific bond
# library(ggplot2)
#
# all_bonds %>%
#   filter(bond == "R2030 (2030)", sector == "Non-residents") %>%
#   ggplot(aes(x = file_date, y = value / 1e9)) +
#   geom_line() +
#   labs(title = "Non-resident Holdings of R2030",
#        x = "Date", y = "Holdings (R billions)") +
#   theme_minimal()