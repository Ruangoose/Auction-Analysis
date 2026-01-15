# ================================================================================
# BOND DATA LOADER MODULE
# Dynamic Excel-based data loading with automatic caching
# ================================================================================

#' Check if cache is stale (Excel newer than cache or cache doesn't exist)
#'
#' @param excel_path Path to the Excel source file
#' @param cache_path Path to the RDS cache file
#' @return TRUE if cache needs to be refreshed
is_cache_stale <- function(excel_path, cache_path) {
    # Cache is stale if it doesn't exist
    if (!file.exists(cache_path)) {
        message("  Cache does not exist - will create from Excel")
        return(TRUE)
    }

    # If Excel doesn't exist, use cache as fallback
    if (!file.exists(excel_path)) {
        message("  Excel file not found - using existing cache as fallback")
        return(FALSE)
    }

    # Compare modification times
    excel_mtime <- file.mtime(excel_path)
    cache_mtime <- file.mtime(cache_path)

    is_stale <- excel_mtime > cache_mtime

    if (is_stale) {
        message(sprintf("  Cache is stale (Excel: %s > Cache: %s)",
                       format(excel_mtime, "%Y-%m-%d %H:%M:%S"),
                       format(cache_mtime, "%Y-%m-%d %H:%M:%S")))
    } else {
        message(sprintf("  Cache is fresh (Excel: %s <= Cache: %s)",
                       format(excel_mtime, "%Y-%m-%d %H:%M:%S"),
                       format(cache_mtime, "%Y-%m-%d %H:%M:%S")))
    }

    return(is_stale)
}


#' Detect available bonds dynamically from a data frame
#'
#' @param df Data frame with bond columns (date column + bond columns)
#' @return Character vector of bond names (excluding 'date')
detect_available_bonds <- function(df) {
    bond_cols <- names(df)[tolower(names(df)) != "date"]
    message(sprintf("  Detected %d bonds: %s", length(bond_cols), paste(bond_cols, collapse = ", ")))
    return(bond_cols)
}


#' Load and pivot a time series sheet from Excel
#'
#' @param excel_path Path to the Excel file
#' @param sheet_name Name of the sheet to read
#' @param value_name Name for the value column after pivoting
#' @param available_bonds Vector of available bond names to filter
#' @return Tibble in long format with date, bond, and value columns
load_time_series_sheet <- function(excel_path, sheet_name, value_name, available_bonds) {
    message(sprintf("  Loading sheet: %s -> %s", sheet_name, value_name))

    tryCatch({
        df <- readxl::read_excel(excel_path, sheet = sheet_name)

        # Get columns that exist in this sheet and are in available_bonds (or date)
        valid_cols <- intersect(names(df), c("date", available_bonds))

        if (!"date" %in% valid_cols) {
            warning(sprintf("Sheet '%s' missing 'date' column", sheet_name))
            return(NULL)
        }

        result <- df %>%
            dplyr::select(dplyr::all_of(valid_cols)) %>%
            dplyr::mutate(date = lubridate::as_date(date)) %>%
            tidyr::pivot_longer(
                cols = -date,
                names_to = "bond",
                values_to = value_name
            ) %>%
            dplyr::filter(!is.na(.data[[value_name]]))

        message(sprintf("    Loaded %d rows for %s", nrow(result), value_name))
        return(result)

    }, error = function(e) {
        warning(sprintf("Error loading sheet '%s': %s", sheet_name, e$message))
        return(NULL)
    })
}


#' Load coupon data (static - joins by bond only)
#'
#' @param excel_path Path to the Excel file
#' @param available_bonds Vector of available bond names
#' @return Tibble with bond and coupon columns
load_coupon_data <- function(excel_path, available_bonds) {
    message("  Loading coupon data (static)")

    tryCatch({
        df <- readxl::read_excel(excel_path, sheet = "cpn")

        # Get columns that exist and are in available_bonds
        valid_cols <- intersect(names(df), available_bonds)

        if (length(valid_cols) == 0) {
            warning("No valid bond columns found in coupon sheet")
            return(NULL)
        }

        result <- df %>%
            dplyr::select(dplyr::all_of(valid_cols)) %>%
            tidyr::pivot_longer(
                cols = dplyr::everything(),
                names_to = "bond",
                values_to = "coupon"
            ) %>%
            dplyr::filter(!is.na(coupon)) %>%
            dplyr::distinct(bond, coupon)

        message(sprintf("    Loaded coupon for %d bonds", nrow(result)))
        return(result)

    }, error = function(e) {
        warning(sprintf("Error loading coupon data: %s", e$message))
        return(NULL)
    })
}


#' Load auction data with new columns
#'
#' @param excel_path Path to the Excel file
#' @return Tibble with auction data in long format
load_auction_data <- function(excel_path) {
    message("  Loading auction data")

    tryCatch({
        df <- readxl::read_excel(excel_path, sheet = "auctions")

        message(sprintf("    Raw auction columns: %s", paste(names(df), collapse = ", ")))

        result <- df %>%
            dplyr::mutate(
                # Parse date columns
                announcement_date = lubridate::as_date(announce_date),
                offer_date = lubridate::as_date(offer_date),
                settle_date = lubridate::as_date(sett_date),
                mature_date = lubridate::as_date(mat_date),

                # Standardize column names (keep originals as aliases)
                offer_amount = offer,
                bids_received = bids,

                # Calculate auction tail (worst - best bid)
                auction_tail = dplyr::if_else(
                    !is.na(worst_bid) & !is.na(best_bid),
                    worst_bid - best_bid,
                    NA_real_
                ),

                # Create date key for joining
                date = offer_date
            )

        # Select all auction columns - use if_else for columns that may not exist
        # First check which columns exist
        existing_cols <- names(result)

        # Build select list dynamically
        base_cols <- c("date", "bond", "offer_date", "announcement_date",
                      "settle_date", "mature_date", "offer_amount", "allocation",
                      "bids_received", "bid_to_cover")

        # New auction columns (may not exist in older data)
        new_cols <- c("bond_coupon", "clearing_yield", "non_comps",
                     "number_bids_received", "best_bid", "worst_bid", "auction_tail")

        # Keep only columns that exist
        select_cols <- intersect(c(base_cols, new_cols), existing_cols)

        result <- result %>%
            dplyr::select(dplyr::all_of(select_cols))

        message(sprintf("    Loaded %d auction records with %d columns",
                       nrow(result), ncol(result)))

        return(result)

    }, error = function(e) {
        warning(sprintf("Error loading auction data: %s", e$message))
        return(NULL)
    })
}


#' Load all bond data from Excel file
#'
#' @param excel_path Path to the Excel file
#' @return Tibble with all bond data merged
load_from_excel <- function(excel_path) {
    message("╔════════════════════════════════════════════════════════╗")
    message("║        LOADING BOND DATA FROM EXCEL                    ║")
    message("╚════════════════════════════════════════════════════════╝")
    message(sprintf("  Source: %s", excel_path))

    # Step 1: Detect available bonds from mod_dur sheet
    message("\n[1/4] Detecting available bonds...")
    mod_dur_raw <- readxl::read_excel(excel_path, sheet = "mod_dur")
    available_bonds <- detect_available_bonds(mod_dur_raw)

    # Step 2: Load all time series sheets
    message("\n[2/4] Loading time series data...")

    # Define sheet mappings: sheet_name -> value_column_name
    time_series_sheets <- list(
        "mod_dur" = "modified_duration",
        "ytm" = "yield_to_maturity",
        "dur" = "duration",
        "conv" = "convexity",
        "clean_price" = "clean_price",
        "full_price" = "full_price",
        "bpv" = "basis_point_value",
        "accrued" = "accrued_interest"
    )

    # Load each time series sheet
    ts_data_list <- lapply(names(time_series_sheets), function(sheet) {
        load_time_series_sheet(
            excel_path = excel_path,
            sheet_name = sheet,
            value_name = time_series_sheets[[sheet]],
            available_bonds = available_bonds
        )
    })
    names(ts_data_list) <- names(time_series_sheets)

    # Step 3: Load coupon data (static)
    message("\n[3/4] Loading coupon data...")
    cpn_df <- load_coupon_data(excel_path, available_bonds)

    # Step 4: Load auction data
    message("\n[4/4] Loading auction data...")
    auction_df <- load_auction_data(excel_path)

    # Step 5: Merge all data
    message("\n[5/5] Merging all data...")

    # Start with modified duration as the base
    full_df <- ts_data_list[["mod_dur"]]

    if (is.null(full_df)) {
        stop("Failed to load modified_duration - cannot proceed")
    }

    # Join all other time series data
    for (sheet in names(ts_data_list)) {
        if (sheet == "mod_dur") next  # Already have this as base

        sheet_data <- ts_data_list[[sheet]]
        if (!is.null(sheet_data)) {
            full_df <- full_df %>%
                dplyr::left_join(sheet_data, by = c("date", "bond"))
        }
    }

    # Join coupon data (by bond only - static)
    if (!is.null(cpn_df)) {
        full_df <- full_df %>%
            dplyr::left_join(cpn_df, by = "bond")
    }

    # Join auction data
    if (!is.null(auction_df)) {
        full_df <- full_df %>%
            dplyr::left_join(auction_df, by = c("date", "bond"))
    }

    # Summary
    message("\n╔════════════════════════════════════════════════════════╗")
    message("║          EXCEL DATA LOADED SUCCESSFULLY                ║")
    message("╚════════════════════════════════════════════════════════╝")
    message(sprintf("  Total bonds:       %d", length(unique(full_df$bond))))
    message(sprintf("  Total rows:        %s", format(nrow(full_df), big.mark = ",")))
    message(sprintf("  Date range:        %s to %s",
                   min(full_df$date, na.rm = TRUE),
                   max(full_df$date, na.rm = TRUE)))
    message(sprintf("  Columns:           %d", ncol(full_df)))
    message(sprintf("  Column names:      %s", paste(names(full_df), collapse = ", ")))

    return(tibble::as_tibble(full_df))
}


#' Main bond data loader with caching
#'
#' @param excel_path Path to the Excel source file
#' @param cache_path Path to the RDS cache file
#' @param force_refresh Force reload from Excel even if cache is fresh
#' @return Tibble with full bond data
load_bond_data <- function(
    excel_path = "data/Siyanda Bonds.xlsx",
    cache_path = "data/processed_bond_data.rds",
    force_refresh = FALSE
) {
    message("╔════════════════════════════════════════════════════════╗")
    message("║          BOND DATA LOADER                              ║")
    message("╚════════════════════════════════════════════════════════╝")
    message(sprintf("  Excel source:  %s", excel_path))
    message(sprintf("  Cache file:    %s", cache_path))
    message(sprintf("  Force refresh: %s", force_refresh))

    # Check if we need to reload from Excel
    needs_refresh <- force_refresh || is_cache_stale(excel_path, cache_path)

    if (needs_refresh) {
        message("\n→ Loading fresh data from Excel...")

        # Load from Excel
        full_df <- load_from_excel(excel_path)

        # Save to cache
        tryCatch({
            # Ensure cache directory exists
            cache_dir <- dirname(cache_path)
            if (!dir.exists(cache_dir)) {
                dir.create(cache_dir, recursive = TRUE)
            }

            saveRDS(full_df, cache_path)
            message(sprintf("\n✓ Cache saved to: %s", cache_path))
        }, error = function(e) {
            warning(sprintf("Failed to save cache: %s", e$message))
        })

        return(full_df)

    } else {
        message("\n→ Loading data from cache (faster)...")

        full_df <- tryCatch({
            readRDS(cache_path)
        }, error = function(e) {
            warning(sprintf("Cache read failed, falling back to Excel: %s", e$message))
            load_from_excel(excel_path)
        })

        message(sprintf("✓ Loaded %s rows from cache", format(nrow(full_df), big.mark = ",")))

        return(tibble::as_tibble(full_df))
    }
}


#' Validate required columns exist in the loaded data
#'
#' @param df Data frame to validate
#' @return TRUE if all required columns exist, FALSE otherwise
validate_bond_data_columns <- function(df) {
    required_cols <- c(
        "date", "bond",
        "yield_to_maturity", "modified_duration", "duration", "convexity"
    )

    missing <- setdiff(required_cols, names(df))

    if (length(missing) > 0) {
        warning(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
        return(FALSE)
    }

    return(TRUE)
}


# ================================================================================
# BOND MATURITY FILTERING FUNCTIONS
# Dynamic filtering to exclude matured bonds based on selected date range
# ================================================================================

#' Infer maturity date from bond name
#'
#' SA government bonds follow naming conventions:
#' - R2030, R2048 format: Year is the 4-digit number
#' - R186, R213 format: R1XX means 20XX (e.g., R186 -> 2026)
#'
#' @param bond_name Character string of bond name (e.g., "R2030", "R186")
#' @return Date object representing estimated maturity date, or NA
infer_maturity_from_name <- function(bond_name) {
    # Extract numeric portion
    num <- gsub("R", "", bond_name, ignore.case = TRUE)

    year <- NA_integer_

    if (nchar(num) == 4 && grepl("^\\d{4}$", num)) {
        # R2030, R2048 format -> year is the number
        year <- as.integer(num)
    } else if (nchar(num) == 3 && grepl("^\\d{3}$", num)) {
        # R186, R213 format
        first_digit <- as.integer(substr(num, 1, 1))
        last_two <- as.integer(substr(num, 2, 3))

        if (first_digit == 1) {
            # R1XX -> 20XX (e.g., R186 -> 2026)
            year <- 2000 + last_two
        } else if (first_digit == 2) {
            # R2XX -> 20XX (e.g., R209 -> 2009, R213 -> 2013)
            year <- 2000 + as.integer(num) %% 100
        }
    }

    # Return Dec 31 of maturity year as conservative estimate
    # Actual date should come from auction data when available
    if (!is.na(year) && year >= 2000 && year <= 2100) {
        return(as.Date(paste0(year, "-12-31")))
    }

    return(NA_Date_)
}


#' Create bond metadata table with maturity dates from multiple sources
#'
#' Priority for maturity date:
#' 1. Auction data (mature_date column) - most reliable
#' 2. Inferred from bond name convention
#' 3. Last data date (assumes bond matured if no more data)
#'
#' @param full_df Data frame with all bond time series data
#' @param auction_df Optional data frame with auction data including mature_date
#' @return Tibble with bond metadata including maturity dates and status
create_bond_metadata <- function(full_df, auction_df = NULL) {
    message("╔════════════════════════════════════════════════════════╗")
    message("║        CREATING BOND METADATA TABLE                    ║")
    message("╚════════════════════════════════════════════════════════╝")

    # Method 1: Extract maturity dates from auction data (preferred)
    if (!is.null(auction_df) && "mature_date" %in% names(auction_df)) {
        maturity_from_auctions <- auction_df %>%
            dplyr::filter(!is.na(mature_date)) %>%
            dplyr::group_by(bond) %>%
            dplyr::summarise(
                maturity_date = max(mature_date, na.rm = TRUE),
                .groups = "drop"
            )
        message(sprintf("  Maturity dates from auctions: %d bonds", nrow(maturity_from_auctions)))
    } else {
        maturity_from_auctions <- tibble::tibble(bond = character(), maturity_date = as.Date(character()))
        message("  No auction maturity data available")
    }

    # Method 2: Get last data date for each bond (fallback indicator)
    last_data_date <- full_df %>%
        dplyr::filter(!is.na(yield_to_maturity)) %>%
        dplyr::group_by(bond) %>%
        dplyr::summarise(
            last_data_date = max(date, na.rm = TRUE),
            first_data_date = min(date, na.rm = TRUE),
            data_points = dplyr::n(),
            .groups = "drop"
        )

    # Combine all methods with priority: auction > inferred > last_data
    all_bonds <- unique(full_df$bond)

    bond_metadata <- tibble::tibble(bond = all_bonds) %>%
        dplyr::left_join(maturity_from_auctions, by = "bond") %>%
        dplyr::left_join(last_data_date, by = "bond") %>%
        dplyr::mutate(
            # Inferred maturity from name
            inferred_maturity = purrr::map(bond, infer_maturity_from_name) %>%
                purrr::map(~ if (is.null(.x) || length(.x) == 0) NA_Date_ else .x) %>%
                do.call(c, .),

            # Final maturity date: auction > inferred > last_data
            final_maturity_date = dplyr::case_when(
                !is.na(maturity_date) ~ maturity_date,
                !is.na(inferred_maturity) ~ inferred_maturity,
                # If bond has no data in recent period, assume it may have matured
                TRUE ~ last_data_date
            ),

            # Data source for transparency
            maturity_source = dplyr::case_when(
                !is.na(maturity_date) ~ "auction_data",
                !is.na(inferred_maturity) ~ "name_inference",
                TRUE ~ "last_data_date"
            ),

            # Status based on today's date
            is_matured = final_maturity_date < Sys.Date(),

            # Days to maturity (negative if matured)
            days_to_maturity = as.integer(final_maturity_date - Sys.Date()),

            # Time to maturity in years
            years_to_maturity = days_to_maturity / 365.25
        )

    # Summary statistics
    n_matured <- sum(bond_metadata$is_matured, na.rm = TRUE)
    n_active <- sum(!bond_metadata$is_matured, na.rm = TRUE)
    n_unknown <- sum(is.na(bond_metadata$is_matured))

    message(sprintf("  Total bonds:     %d", nrow(bond_metadata)))
    message(sprintf("  Active bonds:    %d", n_active))
    message(sprintf("  Matured bonds:   %d", n_matured))
    message(sprintf("  Unknown status:  %d", n_unknown))

    # List matured bonds for transparency
    matured_bonds <- bond_metadata %>%
        dplyr::filter(is_matured == TRUE) %>%
        dplyr::arrange(final_maturity_date)

    if (nrow(matured_bonds) > 0) {
        message("\n  Matured bonds:")
        for (i in seq_len(min(10, nrow(matured_bonds)))) {
            message(sprintf("    - %s: matured %s (source: %s)",
                           matured_bonds$bond[i],
                           format(matured_bonds$final_maturity_date[i], "%Y-%m-%d"),
                           matured_bonds$maturity_source[i]))
        }
        if (nrow(matured_bonds) > 10) {
            message(sprintf("    ... and %d more", nrow(matured_bonds) - 10))
        }
    }

    return(bond_metadata)
}


#' Get list of active (non-matured) bonds for a given date range
#'
#' A bond is considered "active" for analysis if:
#' - It has NOT matured before the END of the selected period
#' - This ensures we only analyze bonds that were tradeable
#'
#' @param bond_metadata Tibble created by create_bond_metadata()
#' @param start_date Start of the analysis period (Date)
#' @param end_date End of the analysis period (Date)
#' @param include_unknown Whether to include bonds with unknown maturity (default TRUE)
#' @return Character vector of active bond names
get_active_bonds <- function(bond_metadata, start_date, end_date, include_unknown = TRUE) {
    # Bond is active if it matures ON or AFTER the end of analysis period
    active_bonds <- bond_metadata %>%
        dplyr::filter(
            # Bond maturity is on or after the end of analysis period
            final_maturity_date >= end_date |
            # Or include bonds with unknown maturity (conservative approach)
            (include_unknown & is.na(final_maturity_date))
        ) %>%
        dplyr::pull(bond)

    return(active_bonds)
}


#' Get list of matured bonds for a given date range (for reference/display)
#'
#' @param bond_metadata Tibble created by create_bond_metadata()
#' @param end_date End of the analysis period (Date)
#' @return Tibble with matured bond information
get_matured_bonds <- function(bond_metadata, end_date) {
    matured_bonds <- bond_metadata %>%
        dplyr::filter(
            !is.na(final_maturity_date),
            final_maturity_date < end_date
        ) %>%
        dplyr::select(bond, final_maturity_date, maturity_source, days_to_maturity) %>%
        dplyr::arrange(dplyr::desc(final_maturity_date))

    return(matured_bonds)
}


#' Create display labels for bonds showing maturity status
#'
#' @param bond_metadata Tibble created by create_bond_metadata()
#' @return Tibble with bond and display_label columns
create_bond_labels <- function(bond_metadata) {
    bond_metadata %>%
        dplyr::mutate(
            display_label = dplyr::case_when(
                is_matured ~ paste0(bond, " (Matured)"),
                days_to_maturity < 365 ~ paste0(bond, " (Matures ", format(final_maturity_date, "%b %Y"), ")"),
                TRUE ~ bond
            )
        ) %>%
        dplyr::select(bond, display_label, is_matured, final_maturity_date)
}
