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
