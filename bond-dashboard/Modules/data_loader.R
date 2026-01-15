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
#' @param run_diagnostics Run diagnostic checks during loading (default TRUE)
#' @return Tibble with all bond data merged
load_from_excel <- function(excel_path, run_diagnostics = TRUE) {
    message("╔════════════════════════════════════════════════════════╗")
    message("║        LOADING BOND DATA FROM EXCEL                    ║")
    message("╚════════════════════════════════════════════════════════╝")
    message(sprintf("  Source: %s", excel_path))

    # Step 1: Detect available bonds from BOTH required sheets
    # This ensures we only load bonds that exist in both ytm and mod_dur
    message("\n[1/5] Detecting available bonds...")

    # IMPROVED: Detect common bonds across required sheets to prevent data loss
    available_bonds <- detect_common_bonds(excel_path)

    if (length(available_bonds) == 0) {
        # Fallback to original method if common detection fails
        message("  WARNING: Common bond detection failed, falling back to mod_dur only")
        mod_dur_raw <- readxl::read_excel(excel_path, sheet = "mod_dur")
        available_bonds <- detect_available_bonds(mod_dur_raw)
    }

    message(sprintf("  Bonds to load: %s", paste(sort(available_bonds), collapse = ", ")))

    # Step 2: Load all time series sheets
    message("\n[2/5] Loading time series data...")

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

    # DIAGNOSTIC CHECKPOINT 1: Verify YTM loaded correctly
    if (run_diagnostics && !is.null(ts_data_list[["ytm"]])) {
        ytm_check <- ts_data_list[["ytm"]] %>%
            dplyr::filter(date == max(date)) %>%
            dplyr::filter(yield_to_maturity < 5 | yield_to_maturity > 15)
        if (nrow(ytm_check) > 0) {
            message("\n  ⚠ WARNING: Bonds with unusual YTM values:")
            for (i in 1:min(nrow(ytm_check), 5)) {
                message(sprintf("    %s: %.2f%%", ytm_check$bond[i], ytm_check$yield_to_maturity[i]))
            }
        }
    }

    # Step 3: Load coupon data (static)
    message("\n[3/5] Loading coupon data...")
    cpn_df <- load_coupon_data(excel_path, available_bonds)

    # Step 4: Load auction data
    message("\n[4/5] Loading auction data...")
    auction_df <- load_auction_data(excel_path)

    # Step 5: Merge all data
    message("\n[5/5] Merging all data...")

    # Start with modified duration as the base
    full_df <- ts_data_list[["mod_dur"]]

    if (is.null(full_df)) {
        stop("Failed to load modified_duration - cannot proceed")
    }

    # Track rows before and after joins for diagnostics
    rows_before <- nrow(full_df)

    # Join all other time series data
    for (sheet in names(time_series_sheets)) {
        if (sheet == "mod_dur") next  # Already have this as base

        sheet_data <- ts_data_list[[sheet]]
        if (!is.null(sheet_data)) {
            full_df <- full_df %>%
                dplyr::left_join(sheet_data, by = c("date", "bond"))
        }
    }

    # DIAGNOSTIC CHECKPOINT 2: Check for NA values introduced by joins
    if (run_diagnostics) {
        na_ytm <- sum(is.na(full_df$yield_to_maturity))
        na_dur <- sum(is.na(full_df$modified_duration))
        if (na_ytm > 0 || na_dur > 0) {
            message(sprintf("\n  ⚠ WARNING: Join introduced NA values - YTM: %d, Duration: %d",
                            na_ytm, na_dur))
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

    rows_after <- nrow(full_df)

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

    # DIAGNOSTIC CHECKPOINT 3: Final data quality check
    if (run_diagnostics) {
        message("\n  Performing final data quality check...")
        latest <- full_df %>%
            dplyr::filter(date == max(date, na.rm = TRUE)) %>%
            dplyr::select(bond, yield_to_maturity, modified_duration)

        bad_data <- latest %>%
            dplyr::filter(
                is.na(yield_to_maturity) | yield_to_maturity < 5 | yield_to_maturity > 15 |
                is.na(modified_duration) | modified_duration < 0.5
            )

        if (nrow(bad_data) > 0) {
            message("  ⚠ QUALITY ISSUE DETECTED - Bonds with problematic values:")
            for (i in 1:nrow(bad_data)) {
                row <- bad_data[i,]
                message(sprintf("    %s: YTM=%.2f%%, ModDur=%.2f",
                                row$bond,
                                ifelse(is.na(row$yield_to_maturity), 0, row$yield_to_maturity),
                                ifelse(is.na(row$modified_duration), 0, row$modified_duration)))
            }
            message("  RECOMMENDATION: Check Excel source data or run diagnose_bond_data_loading()")
        } else {
            message(sprintf("  ✓ All %d bonds have valid YTM and duration values", nrow(latest)))
        }
    }

    return(tibble::as_tibble(full_df))
}


#' Main bond data loader with caching
#'
#' Loads bond data from Excel with automatic caching for faster subsequent loads.
#' Includes data quality verification to detect corrupted values.
#'
#' @param excel_path Path to the Excel source file
#' @param cache_path Path to the RDS cache file
#' @param force_refresh Force reload from Excel even if cache is fresh
#' @param verify_quality Run data quality verification after loading (default TRUE)
#' @return Tibble with full bond data
#'
#' @examples
#' # Normal load (uses cache if available)
#' data <- load_bond_data()
#'
#' # Force fresh load from Excel (use if data appears corrupted)
#' data <- load_bond_data(force_refresh = TRUE)
#'
#' # Skip quality verification for faster loading
#' data <- load_bond_data(verify_quality = FALSE)
#'
#' @export
load_bond_data <- function(
    excel_path = "data/Siyanda Bonds.xlsx",
    cache_path = "data/processed_bond_data.rds",
    force_refresh = FALSE,
    verify_quality = TRUE
) {
    message("╔════════════════════════════════════════════════════════╗")
    message("║          BOND DATA LOADER                              ║")
    message("╚════════════════════════════════════════════════════════╝")
    message(sprintf("  Excel source:  %s", excel_path))
    message(sprintf("  Cache file:    %s", cache_path))
    message(sprintf("  Force refresh: %s", force_refresh))
    message(sprintf("  Verify quality: %s", verify_quality))

    # Check if we need to reload from Excel
    needs_refresh <- force_refresh || is_cache_stale(excel_path, cache_path)

    if (needs_refresh) {
        message("\n→ Loading fresh data from Excel...")

        # Load from Excel with diagnostics
        full_df <- load_from_excel(excel_path, run_diagnostics = verify_quality)

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

    } else {
        message("\n→ Loading data from cache (faster)...")

        full_df <- tryCatch({
            readRDS(cache_path)
        }, error = function(e) {
            warning(sprintf("Cache read failed, falling back to Excel: %s", e$message))
            load_from_excel(excel_path, run_diagnostics = verify_quality)
        })

        message(sprintf("✓ Loaded %s rows from cache", format(nrow(full_df), big.mark = ",")))
    }

    # Data quality verification (runs regardless of source)
    if (verify_quality && !is.null(full_df) && nrow(full_df) > 0) {
        quality_check <- verify_bond_data_quality(full_df)

        # Check if there are problematic bonds
        if (!is.null(quality_check)) {
            n_bad <- sum(!(quality_check$ytm_ok & quality_check$dur_ok))
            if (n_bad > 0 && !force_refresh) {
                message("\n  ════════════════════════════════════════════════════════")
                message(sprintf("  ⚠ %d bond(s) with suspicious values detected!", n_bad))
                message("  Consider running: load_bond_data(force_refresh = TRUE)")
                message("  Or run diagnostics: diagnose_bond_data_loading('data/Siyanda Bonds.xlsx')")
                message("  ════════════════════════════════════════════════════════\n")
            }
        }
    }

    return(tibble::as_tibble(full_df))
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
# DATA QUALITY VERIFICATION FUNCTIONS
# Diagnostic tools to detect and prevent bond data corruption
# ================================================================================

#' Check column alignment across all Excel sheets
#'
#' Verifies that bond columns exist consistently across required sheets.
#' This prevents data loss from join operations when bonds are missing from some sheets.
#'
#' @param excel_path Path to the Excel file
#' @return List with alignment check results
#' @export
check_column_alignment <- function(excel_path) {
    message("\n╔════════════════════════════════════════════════════════╗")
    message("║          COLUMN ALIGNMENT CHECK                        ║")
    message("╚════════════════════════════════════════════════════════╝")

    sheets <- c("ytm", "mod_dur", "dur", "conv", "clean_price", "bpv", "accrued", "full_price")
    required_sheets <- c("ytm", "mod_dur")

    all_cols <- list()
    for (sheet in sheets) {
        tryCatch({
            df <- readxl::read_excel(excel_path, sheet = sheet)
            cols <- names(df)[tolower(names(df)) != "date"]
            all_cols[[sheet]] <- cols
            message(sprintf("  %s: %d bond columns", sheet, length(cols)))
        }, error = function(e) {
            message(sprintf("  %s: ERROR - %s", sheet, e$message))
        })
    }

    # Find common bonds across all sheets
    common_bonds <- Reduce(intersect, all_cols)
    message(sprintf("\n  Common bonds across all sheets: %d", length(common_bonds)))
    message(sprintf("  %s", paste(sort(common_bonds), collapse = ", ")))

    # Find bonds missing from some sheets
    all_bonds <- unique(unlist(all_cols))
    missing_issues <- list()
    for (bond in sort(all_bonds)) {
        missing_from <- sheets[!sapply(all_cols, function(x) bond %in% x)]
        if (length(missing_from) > 0) {
            message(sprintf("  ⚠ %s missing from: %s", bond, paste(missing_from, collapse = ", ")))
            missing_issues[[bond]] <- missing_from
        }
    }

    # Check required sheets specifically
    required_common <- Reduce(intersect, all_cols[required_sheets])
    message(sprintf("\n  Common bonds in required sheets (ytm, mod_dur): %d", length(required_common)))

    message("╚════════════════════════════════════════════════════════╝\n")

    return(list(
        common_all_sheets = common_bonds,
        common_required = required_common,
        all_bonds = all_bonds,
        missing_issues = missing_issues,
        sheet_columns = all_cols
    ))
}


#' Detect bonds that exist in ALL required sheets
#'
#' Returns only bonds that have data in both ytm and mod_dur sheets to prevent
#' data loss from join operations.
#'
#' @param excel_path Path to the Excel file
#' @return Character vector of bond names present in all required sheets
detect_common_bonds <- function(excel_path) {
    required_sheets <- c("ytm", "mod_dur")

    bond_lists <- lapply(required_sheets, function(sheet) {
        tryCatch({
            df <- readxl::read_excel(excel_path, sheet = sheet)
            names(df)[tolower(names(df)) != "date"]
        }, error = function(e) {
            character(0)
        })
    })

    common_bonds <- Reduce(intersect, bond_lists)
    message(sprintf("  Common bonds in required sheets: %d", length(common_bonds)))

    return(common_bonds)
}


#' Verify bond data quality after loading
#'
#' Comprehensive data quality verification that checks for corrupted or invalid values.
#' This should be called after data loading to ensure data integrity.
#'
#' @param data Data frame with bond data
#' @param ytm_min Minimum valid yield (default 5%)
#' @param ytm_max Maximum valid yield (default 15%)
#' @param dur_min Minimum valid modified duration (default 0.5 years)
#' @return Tibble with quality check results for each bond
#' @export
verify_bond_data_quality <- function(data, ytm_min = 5, ytm_max = 15, dur_min = 0.5) {
    message("\n╔════════════════════════════════════════════════════════╗")
    message("║        DATA QUALITY VERIFICATION                       ║")
    message("╚════════════════════════════════════════════════════════╝")

    if (is.null(data) || nrow(data) == 0) {
        message("  ERROR: No data to verify!")
        return(NULL)
    }

    latest_date <- max(data$date, na.rm = TRUE)
    latest <- data %>% dplyr::filter(date == latest_date)

    message(sprintf("  Latest date: %s", latest_date))
    message(sprintf("  Bonds on latest date: %d", dplyr::n_distinct(latest$bond)))

    # Check each bond
    quality_check <- latest %>%
        dplyr::group_by(bond) %>%
        dplyr::summarise(
            ytm = dplyr::first(yield_to_maturity),
            mod_dur = dplyr::first(modified_duration),
            coupon = dplyr::first(coupon),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            ytm_ok = !is.na(ytm) & ytm > ytm_min & ytm < ytm_max,
            dur_ok = !is.na(mod_dur) & mod_dur > dur_min,
            status = dplyr::case_when(
                ytm_ok & dur_ok ~ "OK",
                !ytm_ok & !dur_ok ~ "BAD (ytm & dur)",
                !ytm_ok ~ "BAD ytm",
                !dur_ok ~ "BAD dur"
            )
        ) %>%
        dplyr::arrange(dplyr::desc(ytm_ok), bond)

    message("\n  Bond Data Quality:")
    for (i in 1:nrow(quality_check)) {
        row <- quality_check[i,]
        status_icon <- if(row$ytm_ok && row$dur_ok) "✓" else "✗"
        message(sprintf("    %s %-6s: YTM=%6.2f%%, ModDur=%5.2f  %s",
                        status_icon, row$bond, row$ytm, row$mod_dur, row$status))
    }

    n_good <- sum(quality_check$ytm_ok & quality_check$dur_ok)
    n_bad <- nrow(quality_check) - n_good

    message(sprintf("\n  Summary: %d good, %d problematic", n_good, n_bad))

    if (n_bad > 0) {
        message("\n  ⚠ PROBLEMATIC BONDS DETECTED!")
        bad_bonds <- quality_check %>% dplyr::filter(!(ytm_ok & dur_ok))
        for (i in 1:nrow(bad_bonds)) {
            row <- bad_bonds[i,]
            message(sprintf("    %s: ytm=%.2f, mod_dur=%.2f - %s",
                            row$bond, row$ytm, row$mod_dur, row$status))
        }
        message("\n  RECOMMENDED ACTION: Force cache refresh with force_refresh=TRUE")
    }

    message("╚════════════════════════════════════════════════════════╝\n")

    return(quality_check)
}


#' Diagnose bond data loading with checkpoints
#'
#' Detailed diagnostic function that tracks data through the loading pipeline
#' to identify where corruption might be occurring.
#'
#' @param excel_path Path to the Excel file
#' @return List with diagnostic results at each checkpoint
#' @export
diagnose_bond_data_loading <- function(excel_path) {
    message("\n╔════════════════════════════════════════════════════════╗")
    message("║        BOND DATA LOADING DIAGNOSTICS                   ║")
    message("╚════════════════════════════════════════════════════════╝")

    results <- list()

    # CHECKPOINT 1: Raw YTM values
    message("\n[CHECKPOINT 1] Raw YTM Sheet")
    ytm_raw <- readxl::read_excel(excel_path, sheet = "ytm")
    latest_row <- ytm_raw %>% dplyr::slice_tail(n = 1)

    message(sprintf("  Columns: %s", paste(names(ytm_raw), collapse = ", ")))
    message(sprintf("  Latest date: %s", latest_row$date))
    message("  Latest YTM values:")
    for (col in names(latest_row)[-1]) {
        val <- latest_row[[col]]
        if (!is.na(val)) {
            status <- if(val > 5 && val < 15) "✓" else "⚠ PROBLEM"
            message(sprintf("    %s: %.2f%% %s", col, val, status))
        }
    }
    results$ytm_raw <- latest_row

    # CHECKPOINT 2: Raw mod_dur values
    message("\n[CHECKPOINT 2] Raw mod_dur Sheet")
    dur_raw <- readxl::read_excel(excel_path, sheet = "mod_dur")
    latest_dur <- dur_raw %>% dplyr::slice_tail(n = 1)

    message(sprintf("  Latest date: %s", latest_dur$date))
    message("  Latest duration values:")
    for (col in names(latest_dur)[-1]) {
        val <- latest_dur[[col]]
        if (!is.na(val)) {
            status <- if(val > 0.5) "✓" else "⚠ PROBLEM"
            message(sprintf("    %s: %.2f years %s", col, val, status))
        }
    }
    results$dur_raw <- latest_dur

    # CHECKPOINT 3: After pivot_longer on YTM
    message("\n[CHECKPOINT 3] After YTM pivot_longer")
    available_bonds <- detect_available_bonds(dur_raw)
    ytm_pivoted <- ytm_raw %>%
        dplyr::select(dplyr::all_of(intersect(names(ytm_raw), c("date", available_bonds)))) %>%
        dplyr::mutate(date = lubridate::as_date(date)) %>%
        tidyr::pivot_longer(-date, names_to = "bond", values_to = "yield_to_maturity") %>%
        dplyr::filter(!is.na(yield_to_maturity))

    latest_ytm_check <- ytm_pivoted %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::arrange(bond)

    bad_ytm <- latest_ytm_check %>%
        dplyr::filter(yield_to_maturity < 5 | is.na(yield_to_maturity))
    if (nrow(bad_ytm) > 0) {
        message("  ⚠ WARNING: Bonds with bad YTM values after pivot:")
        print(bad_ytm)
    } else {
        message(sprintf("  ✓ All %d bonds have valid YTM values", nrow(latest_ytm_check)))
    }
    results$ytm_pivoted <- latest_ytm_check

    message("\n╚════════════════════════════════════════════════════════╝\n")
    return(results)
}


# ================================================================================
# BOND MATURITY FILTERING FUNCTIONS
# Dynamic filtering to exclude matured bonds based on selected date range
# ================================================================================

# =============================================================================
# IMPORTANT: Name inference has been REMOVED!
# =============================================================================
# SA Government Bond naming conventions do NOT indicate maturity year!
# Examples:
#   - R186 matures February 2026 (not 2086 or 1986)
#   - R213 matures February 2031 (not 2013)
#   - R2030 matures March 2030 (coincidentally matches, but not a rule)
#
# The ONLY reliable source for maturity dates is the mature_date column
# from auction data (mat_date in Excel).
#
# The old infer_maturity_from_name function has been removed because it
# incorrectly assumed bond names encode maturity years.
# =============================================================================


#' Create bond metadata table with maturity dates from auction data ONLY
#'
#' IMPORTANT: Maturity dates are ONLY sourced from auction data (mature_date column).
#' SA Government Bond naming conventions do NOT indicate maturity year!
#' Bonds with unknown maturity are assumed ACTIVE.
#'
#' @param full_df Data frame with all bond time series data
#' @param auction_df Optional data frame with auction data including mature_date
#' @return Tibble with bond metadata including maturity dates and status
create_bond_metadata <- function(full_df, auction_df = NULL) {
    message("\n╔════════════════════════════════════════════════════════╗")
    message("║        CREATING BOND METADATA TABLE                    ║")
    message("╚════════════════════════════════════════════════════════╝")

    # Get all unique bonds from the data
    all_bonds <- sort(unique(full_df$bond))

    # METHOD 1: Extract maturity dates from auction data - THIS IS THE ONLY RELIABLE SOURCE!
    if (!is.null(auction_df) && "mature_date" %in% names(auction_df)) {
        maturity_from_data <- auction_df %>%
            dplyr::filter(!is.na(mature_date)) %>%
            dplyr::group_by(bond) %>%
            dplyr::summarise(
                maturity_date = max(mature_date, na.rm = TRUE),
                .groups = "drop"
            )
        message(sprintf("  Maturity dates from auction data: %d bonds", nrow(maturity_from_data)))
    } else {
        maturity_from_data <- tibble::tibble(bond = character(), maturity_date = as.Date(character()))
        message("  WARNING: No auction maturity data available!")
    }

    # METHOD 2: Get last available data date for each bond (informational only, NOT for filtering)
    last_data_dates <- full_df %>%
        dplyr::filter(!is.na(yield_to_maturity)) %>%
        dplyr::group_by(bond) %>%
        dplyr::summarise(
            last_data_date = max(date, na.rm = TRUE),
            first_data_date = min(date, na.rm = TRUE),
            data_points = dplyr::n(),
            .groups = "drop"
        )

    # Build the metadata table
    bond_metadata <- tibble::tibble(bond = all_bonds) %>%
        dplyr::left_join(maturity_from_data, by = "bond") %>%
        dplyr::left_join(last_data_dates, by = "bond") %>%
        dplyr::mutate(
            # Final maturity date: USE auction data mature_date if available, otherwise NA
            # DO NOT INFER FROM BOND NAME! SA bond naming does not encode maturity!
            final_maturity_date = maturity_date,

            # Source of maturity info
            maturity_source = dplyr::case_when(
                !is.na(maturity_date) ~ "auction_data",
                TRUE ~ "unknown"
            ),

            # Is the bond matured? Only if we KNOW the maturity date AND it's passed
            # Bonds with unknown maturity are assumed ACTIVE (conservative approach)
            is_matured = dplyr::case_when(
                !is.na(final_maturity_date) & final_maturity_date < Sys.Date() ~ TRUE,
                is.na(final_maturity_date) ~ FALSE,  # Unknown = assume ACTIVE!
                TRUE ~ FALSE
            ),

            # Days to maturity (NA if unknown)
            days_to_maturity = dplyr::case_when(
                !is.na(final_maturity_date) ~ as.integer(final_maturity_date - Sys.Date()),
                TRUE ~ NA_integer_
            ),

            # Time to maturity in years (NA if unknown)
            years_to_maturity = dplyr::case_when(
                !is.na(days_to_maturity) ~ days_to_maturity / 365.25,
                TRUE ~ NA_real_
            )
        )

    # Summary statistics
    n_with_maturity <- sum(!is.na(bond_metadata$final_maturity_date))
    n_matured <- sum(bond_metadata$is_matured, na.rm = TRUE)
    n_active <- sum(!bond_metadata$is_matured, na.rm = TRUE)
    n_unknown <- sum(is.na(bond_metadata$final_maturity_date))

    message(sprintf("  Total bonds:      %d", nrow(bond_metadata)))
    message(sprintf("  Active bonds:     %d", n_active))
    message(sprintf("  Matured bonds:    %d", n_matured))
    message(sprintf("  Unknown maturity: %d (assumed ACTIVE)", n_unknown))

    # List matured bonds for transparency
    matured_bonds <- bond_metadata %>%
        dplyr::filter(is_matured == TRUE) %>%
        dplyr::arrange(final_maturity_date)

    if (nrow(matured_bonds) > 0) {
        message("\n  Matured bonds (confirmed from auction data):")
        for (i in seq_len(min(15, nrow(matured_bonds)))) {
            message(sprintf("    - %s: matured %s",
                           matured_bonds$bond[i],
                           format(matured_bonds$final_maturity_date[i], "%Y-%m-%d")))
        }
        if (nrow(matured_bonds) > 15) {
            message(sprintf("    ... and %d more", nrow(matured_bonds) - 15))
        }
    }

    # List bonds with unknown maturity (these will be treated as active!)
    unknown_bonds <- bond_metadata %>%
        dplyr::filter(is.na(final_maturity_date))

    if (nrow(unknown_bonds) > 0) {
        message("\n  Bonds with unknown maturity (assumed ACTIVE):")
        message(sprintf("    %s", paste(unknown_bonds$bond, collapse = ", ")))
        message("    (These bonds may be older issues without auction data)")
    }

    return(bond_metadata)
}


#' Get list of active (non-matured) bonds for a given date range
#'
#' A bond is considered ACTIVE if:
#' 1. It has NOT matured before the END of the selected period, OR
#' 2. We don't know when it matures (assume active - conservative approach)
#'
#' IMPORTANT: Bonds with unknown maturity are ALWAYS included. The SA bond
#' naming convention does NOT encode maturity dates, so we cannot infer
#' maturity from the name. Unknown = assume active.
#'
#' @param bond_metadata Tibble created by create_bond_metadata()
#' @param start_date Start of the analysis period (Date)
#' @param end_date End of the analysis period (Date)
#' @param include_unknown Whether to include bonds with unknown maturity (default TRUE, should stay TRUE!)
#' @return Character vector of active bond names
get_active_bonds <- function(bond_metadata, start_date, end_date, include_unknown = TRUE) {
    # A bond is ACTIVE if:
    # 1. Unknown maturity = assume active (we can't infer from names!)
    # 2. Known maturity that is ON or AFTER the analysis period end

    active_bonds <- bond_metadata %>%
        dplyr::filter(
            # Unknown maturity = always assume active
            is.na(final_maturity_date) |
            # Known maturity that is AFTER or ON the analysis period end
            final_maturity_date >= end_date
        ) %>%
        dplyr::pull(bond)

    message(sprintf("  Active bonds for period %s to %s: %d of %d",
                    start_date, end_date, length(active_bonds), nrow(bond_metadata)))

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
                !is.na(days_to_maturity) & days_to_maturity < 365 ~ paste0(bond, " (Matures ", format(final_maturity_date, "%b %Y"), ")"),
                TRUE ~ bond
            )
        ) %>%
        dplyr::select(bond, display_label, is_matured, final_maturity_date)
}


#' Diagnostic function to verify maturity dates are loaded correctly
#'
#' Call this after loading data to verify that maturity dates from auction
#' data are being used correctly and no incorrect name inference is happening.
#'
#' @param full_df Data frame with bond data including mature_date
#' @return Tibble with maturity verification results (also prints diagnostics)
verify_maturity_dates <- function(full_df) {
    message("\n╔════════════════════════════════════════════════════════╗")
    message("║        MATURITY DATE VERIFICATION                      ║")
    message("╚════════════════════════════════════════════════════════╝")

    if (!"mature_date" %in% names(full_df)) {
        message("  WARNING: mature_date column not found in data!")
        message("  Maturity filtering will assume all bonds are ACTIVE.")
        return(NULL)
    }

    maturity_check <- full_df %>%
        dplyr::filter(!is.na(mature_date)) %>%
        dplyr::group_by(bond) %>%
        dplyr::summarise(
            maturity = max(mature_date, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::arrange(maturity) %>%
        dplyr::mutate(
            status = ifelse(maturity < Sys.Date(), "MATURED", "ACTIVE"),
            days_remaining = as.integer(maturity - Sys.Date())
        )

    # Count statistics
    n_matured <- sum(maturity_check$status == "MATURED")
    n_active <- sum(maturity_check$status == "ACTIVE")
    n_total <- nrow(maturity_check)

    message(sprintf("\n  Bonds with known maturity dates: %d", n_total))
    message(sprintf("  - MATURED: %d", n_matured))
    message(sprintf("  - ACTIVE:  %d", n_active))

    # Show matured bonds
    if (n_matured > 0) {
        message("\n  Matured bonds:")
        matured <- maturity_check %>% dplyr::filter(status == "MATURED")
        for (i in seq_len(min(10, nrow(matured)))) {
            message(sprintf("    - %s: matured %s (%d days ago)",
                           matured$bond[i],
                           format(matured$maturity[i], "%Y-%m-%d"),
                           abs(matured$days_remaining[i])))
        }
        if (nrow(matured) > 10) {
            message(sprintf("    ... and %d more", nrow(matured) - 10))
        }
    }

    # Show upcoming maturities (within 1 year)
    upcoming <- maturity_check %>%
        dplyr::filter(status == "ACTIVE", days_remaining <= 365) %>%
        dplyr::arrange(maturity)

    if (nrow(upcoming) > 0) {
        message("\n  Bonds maturing within 1 year:")
        for (i in seq_len(min(5, nrow(upcoming)))) {
            message(sprintf("    - %s: matures %s (%d days remaining)",
                           upcoming$bond[i],
                           format(upcoming$maturity[i], "%Y-%m-%d"),
                           upcoming$days_remaining[i]))
        }
    }

    # List bonds WITHOUT maturity data
    all_bonds <- unique(full_df$bond)
    bonds_with_maturity <- maturity_check$bond
    bonds_without_maturity <- setdiff(all_bonds, bonds_with_maturity)

    if (length(bonds_without_maturity) > 0) {
        message("\n  Bonds WITHOUT maturity data (assumed ACTIVE):")
        message(sprintf("    %s", paste(sort(bonds_without_maturity), collapse = ", ")))
        message("    (No auction data found for these - they will be included in analysis)")
    }

    message("╚════════════════════════════════════════════════════════╝\n")

    return(maturity_check)
}
