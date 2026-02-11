# ================================================================================
# BOND DATA LOADER MODULE
# Dynamic Excel-based data loading with automatic caching
# ================================================================================

# ==============================================================================
# ROBUST DATA LOADING FOR SA GOVERNMENT BOND DASHBOARD
# Fixes: Corrupt placeholders, matured bond contamination, #N/A handling
# ==============================================================================

# ==============================================================================
# MINIMUM OBSERVATIONS THRESHOLD
# Very low threshold to include new bond issuances with limited historical data
# Set to 1 to include bonds with ANY valid data (even a single observation)
# Bonds with fewer observations than this will be excluded with a warning
# ==============================================================================
MIN_OBSERVATIONS <- 1  # Require only 1 observation minimum to include all bonds with any valid data

# ==============================================================================
# CORRUPTION DETECTION AND VALIDATION FUNCTIONS
# Added 2026-01-15 to fix bond data corruption (YTM=1.0%, ModDur=1.0)
# ==============================================================================

#' Check if cache data is corrupted
#'
#' Detects corruption signatures: YTM=1.0%, ModDur=1.0 or ModDur=0.0
#' SA Government bonds should never have these exact values
#'
#' @param cache_data Data frame to check for corruption
#' @return TRUE if data is corrupted, FALSE if clean
is_cache_corrupted <- function(cache_data) {
  if (is.null(cache_data) || nrow(cache_data) == 0) {
    return(TRUE)  # Empty data is considered corrupted
  }

  # Check for corruption signatures
  has_placeholder_ytm <- any(
    abs(cache_data$yield_to_maturity - 1.0) < 0.01,
    na.rm = TRUE
  )

  has_placeholder_dur <- any(
    abs(cache_data$modified_duration - 1.0) < 0.01 |
    abs(cache_data$modified_duration) < 0.01,
    na.rm = TRUE
  )

  # Check for unrealistic YTM range (valid bonds should have YTM > 1.5%)
  ytm_out_of_range <- any(
    cache_data$yield_to_maturity < 1.5 | cache_data$yield_to_maturity > 20,
    na.rm = TRUE
  )

  is_corrupted <- has_placeholder_ytm || has_placeholder_dur

  if (is_corrupted) {
    message("  Corruption signatures detected:")
    if (has_placeholder_ytm) {
      bad_bonds <- unique(cache_data$bond[abs(cache_data$yield_to_maturity - 1.0) < 0.01])
      message("    - Placeholder YTM (1.0%): ", paste(bad_bonds, collapse = ", "))
    }
    if (has_placeholder_dur) {
      bad_bonds_dur <- unique(cache_data$bond[
        abs(cache_data$modified_duration - 1.0) < 0.01 |
        abs(cache_data$modified_duration) < 0.01
      ])
      message("    - Placeholder ModDur (0.0 or 1.0): ", paste(bad_bonds_dur, collapse = ", "))
    }
  }

  return(is_corrupted)
}


#' Validate bond data before caching
#'
#' Comprehensive validation to prevent corrupted data from being saved to cache
#' Checks for placeholder values, valid ranges, and data variation
#'
#' @param df Data frame with bond data
#' @return TRUE if data is valid, throws error if corrupted
#' @export
validate_data_before_cache <- function(df) {
  message("\n  Validating data before caching...")

  if (is.null(df) || nrow(df) == 0) {
    stop("DATA VALIDATION FAILED - Empty dataframe")
  }

  # Build list of validation checks
  checks <- list()

  # Check 1: No placeholder YTM values (exactly 1.0)
  placeholder_ytm_count <- sum(abs(df$yield_to_maturity - 1.0) < 0.01, na.rm = TRUE)
  checks$no_placeholder_ytm <- placeholder_ytm_count == 0

  # Check 2: No placeholder duration values — only check active (non-matured) bonds
  # Matured bonds legitimately have near-zero duration; short-dated bonds can have ModDur < 1.0
  active_mask <- if ("mature_date" %in% names(df)) {
    is.na(df$mature_date) | df$mature_date >= Sys.Date()
  } else {
    rep(TRUE, nrow(df))
  }
  placeholder_dur_count <- sum(
    active_mask & (abs(df$modified_duration) < 0.05),
    na.rm = TRUE
  )
  checks$no_placeholder_dur <- placeholder_dur_count == 0

  # Check 3: YTM in reasonable range (1.5% to 20% for SA bonds)
  checks$ytm_in_range <- all(
    df$yield_to_maturity > 1.5 & df$yield_to_maturity < 20,
    na.rm = TRUE
  )

  # Check 4: Duration in reasonable range (0.1 to 30 years)
  checks$dur_in_range <- all(
    df$modified_duration > 0.1 & df$modified_duration < 30,
    na.rm = TRUE
  )

  # Check 5: Sufficient YTM variation (should have many unique values)
  unique_ytm <- length(unique(round(df$yield_to_maturity, 2)))
  checks$ytm_has_variation <- unique_ytm > 5

  # Check 6: No NA values in critical columns
  checks$no_na_ytm <- sum(is.na(df$yield_to_maturity)) == 0
  checks$no_na_dur <- sum(is.na(df$modified_duration)) == 0

  # Report results
  failed_checks <- names(checks)[!unlist(checks)]

  if (length(failed_checks) > 0) {
    message("  VALIDATION FAILED:")
    for (check in failed_checks) {
      message("    - ", check)
    }

    # Detailed report for debugging
    if (!checks$no_placeholder_ytm) {
      bad <- unique(df$bond[abs(df$yield_to_maturity - 1.0) < 0.01])
      message("    Bonds with YTM=1.0%: ", paste(bad, collapse = ", "))
    }
    if (!checks$no_placeholder_dur) {
      bad <- unique(df$bond[active_mask & (abs(df$modified_duration) < 0.05)])
      message("    Active bonds with ModDur near 0: ", paste(bad, collapse = ", "))
    }

    stop(paste0(
      "DATA VALIDATION FAILED - REFUSING TO CACHE CORRUPTED DATA\n",
      "Failed checks: ", paste(failed_checks, collapse = ", "), "\n",
      "This prevents corruption from being persisted."
    ))
  }

  message("  All validation checks PASSED")
  return(TRUE)
}


#' Debug helper: Track bond values through processing pipeline
#'
#' Logs bonds with suspicious values at each processing stage
#'
#' @param df Data frame to check
#' @param stage_name Name of the processing stage for logging
#' @return Invisible NULL (for side effects only)
debug_bond_values <- function(df, stage_name) {
  if (is.null(df) || nrow(df) == 0) {
    message(sprintf("  [%s] Empty dataframe!", stage_name))
    return(invisible(NULL))
  }

  suspect_bonds <- df %>%
    dplyr::filter(
      (abs(yield_to_maturity - 1.0) < 0.01) |
      (abs(modified_duration - 1.0) < 0.01) |
      (abs(modified_duration) < 0.01)
    ) %>%
    dplyr::distinct(bond) %>%
    dplyr::pull(bond)

  if (length(suspect_bonds) > 0) {
    message(sprintf("  [%s] SUSPICIOUS VALUES DETECTED in bonds: %s",
                    stage_name,
                    paste(suspect_bonds, collapse = ", ")))
  } else {
    message(sprintf("  [%s] All bonds have valid values", stage_name))
  }

  return(invisible(NULL))
}


# ==============================================================================
# MATURITY DATE LOADING FROM DEDICATED SHEET
# Added 2026-01-15 to fix incorrect maturity inference from auction data
# ==============================================================================

#' Debug helper to trace bond counts through the loading process
#'
#' Logs the number of unique bonds and checks for specific expected bonds
#' to help identify where bonds might be getting dropped.
#'
#' @param data Data frame with a 'bond' column
#' @param step_name Name of the current processing step
#' @param expected_bonds Optional vector of bond names to check for
#' @export
debug_bond_count <- function(data, step_name, expected_bonds = NULL) {
    if (is.null(data) || nrow(data) == 0) {
        message(sprintf("[DEBUG %s] No data (NULL or 0 rows)", step_name))
        return(invisible(NULL))
    }

    n_bonds <- dplyr::n_distinct(data$bond)
    bonds <- sort(unique(data$bond))
    message(sprintf("[DEBUG %s] Bonds: %d -> %s", step_name, n_bonds, paste(bonds, collapse = ", ")))

    # Check for specific missing bonds (commonly dropped in past issues)
    if (is.null(expected_bonds)) {
        # Default set of bonds that have been problematic in the past
        expected_bonds <- c("R2032", "R2033", "R2035", "R2038", "R2039",
                           "R2040", "R2042", "R2044", "R2053", "R187", "R188")
    }

    missing <- setdiff(expected_bonds, bonds)
    if (length(missing) > 0) {
        message(sprintf("[DEBUG %s] ⚠ MISSING: %s", step_name, paste(missing, collapse = ", ")))
    }

    return(invisible(n_bonds))
}


#' Load maturity dates from dedicated maturity_date sheet
#'
#' This function reads maturity dates from a dedicated Excel sheet, which is
#' the authoritative source for bond maturity information. Falls back gracefully
#' if the sheet doesn't exist.
#'
#' The maturity_date sheet can be in any of these formats:
#' - Long format: columns (bond, mat_date) - preferred format
#' - Long format: columns (bond, maturity_date) - alternative
#' - Wide format: columns (date, bond1, bond2, ...) with maturity dates as values
#'
#' @param excel_path Path to the Excel file
#' @return Tibble with columns (bond, maturity_date) or NULL if sheet unavailable
#' @export
load_maturity_dates <- function(excel_path) {
  tryCatch({
    # Check if sheet exists
    available_sheets <- readxl::excel_sheets(excel_path)

    if (!"maturity_date" %in% available_sheets) {
      message("  INFO: maturity_date sheet not found in Excel file")
      message("        Available sheets: ", paste(available_sheets, collapse = ", "))
      return(NULL)
    }

    mat_df <- readxl::read_excel(excel_path, sheet = "maturity_date",
                                  na = c("", "NA", "#N/A", "N/A", "#VALUE!", "#REF!"),
                                  guess_max = 21474836)

    if (is.null(mat_df) || nrow(mat_df) == 0) {
      message("  WARNING: maturity_date sheet is empty")
      return(NULL)
    }

    # Debug output
    message("  maturity_date sheet columns: ", paste(names(mat_df), collapse = ", "))
    message("  maturity_date sheet rows: ", nrow(mat_df))

    # CASE 1: Long format with mat_date column (bond, mat_date) - use directly
    if ("bond" %in% names(mat_df) && "mat_date" %in% names(mat_df)) {
      message("  ✓ maturity_date sheet is in LONG format (bond, mat_date) - using directly")

      maturity_lookup <- mat_df %>%
        dplyr::select(bond, mat_date) %>%
        dplyr::rename(maturity_date = mat_date) %>%
        dplyr::mutate(maturity_date = as.Date(maturity_date)) %>%
        dplyr::filter(!is.na(maturity_date) & !is.na(bond)) %>%
        dplyr::distinct(bond, .keep_all = TRUE)

      message("  ✓ Loaded ", nrow(maturity_lookup), " bonds from maturity_date sheet")
      return(maturity_lookup)
    }

    # CASE 2: Long format with maturity_date column (bond, maturity_date) - use directly
    if ("bond" %in% names(mat_df) && "maturity_date" %in% names(mat_df)) {
      message("  ✓ maturity_date sheet is in LONG format (bond, maturity_date) - using directly")

      maturity_lookup <- mat_df %>%
        dplyr::select(bond, maturity_date) %>%
        dplyr::mutate(maturity_date = as.Date(maturity_date)) %>%
        dplyr::filter(!is.na(maturity_date) & !is.na(bond)) %>%
        dplyr::distinct(bond, .keep_all = TRUE)

      message("  ✓ Loaded ", nrow(maturity_lookup), " bonds from maturity_date sheet")
      return(maturity_lookup)
    }

    # CASE 3: Wide format (date column + bond columns) - pivot to long
    if ("date" %in% names(mat_df)) {
      message("  maturity_date sheet is in WIDE format - pivoting")

      bond_cols <- setdiff(names(mat_df), "date")

      maturity_lookup <- mat_df %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(bond_cols),
          names_to = "bond",
          values_to = "maturity_date"
        ) %>%
        dplyr::mutate(maturity_date = as.Date(maturity_date)) %>%
        dplyr::filter(!is.na(maturity_date)) %>%
        dplyr::group_by(bond) %>%
        dplyr::summarise(
          maturity_date = dplyr::first(stats::na.omit(maturity_date)),
          .groups = "drop"
        )

      message("  ✓ Loaded ", nrow(maturity_lookup), " bonds from maturity_date sheet")
      return(maturity_lookup)
    }

    stop("maturity_date sheet has unexpected format. Expected columns: (bond, mat_date) or (bond, maturity_date) or (date, bond1, bond2, ...)")

  }, error = function(e) {
    warning("Could not load maturity_date sheet: ", e$message)
    return(NULL)
  })
}


#' Load and process bond data from Excel file (ROBUST VERSION)
#'
#' This function implements comprehensive fixes for:
#' 1. Corrupt placeholder values (1.00% yield, 1.00 duration)
#' 2. Matured bond contamination
#' 3. Excel #N/A values not being properly converted to NA
#' 4. Missing data rows showing fallback values
#'
#' @param file_path Path to the Excel file
#' @param reference_date Date to filter matured bonds (default: today)
#' @return List containing full_df, bond_metadata, auction_summary, maturity_lookup
#' @export
load_bond_data_robust <- function(file_path, reference_date = Sys.Date()) {

  message("=== LOADING BOND DATA (ROBUST) ===")
  message("File: ", file_path)
  message("Reference date: ", reference_date)

  # --------------------------------------------------------------------------
  # STEP 1: Build Maturity Lookup (PRIMARY: maturity_date sheet, FALLBACK: auctions)
  # --------------------------------------------------------------------------
  message("\n[1/6] Building maturity lookup...")

  # Load auction data (needed for other metrics even if not used for maturity)
  auction_raw <- tryCatch({
    readxl::read_excel(file_path, sheet = "auctions",
                       na = c("", "NA", "#N/A", "N/A", "#VALUE!", "#REF!"),
                       guess_max = 21474836) %>%
      dplyr::mutate(
        mat_date = lubridate::as_date(mat_date),
        offer_date = lubridate::as_date(offer_date)
      )
  }, error = function(e) {
    warning("Failed to read auctions sheet: ", e$message)
    NULL
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # PRIMARY SOURCE: Load maturity dates from dedicated maturity_date sheet
  # This is the authoritative source - auction data inference is FALLBACK only
  # ═══════════════════════════════════════════════════════════════════════════
  message("  Checking for maturity_date sheet (PRIMARY SOURCE)...")
  maturity_from_sheet <- load_maturity_dates(file_path)

  if (!is.null(maturity_from_sheet) && nrow(maturity_from_sheet) > 0) {
    message("  Using maturity dates from maturity_date sheet (PRIMARY SOURCE)")

    maturity_lookup <- maturity_from_sheet %>%
      dplyr::mutate(
        is_matured = maturity_date < reference_date,
        days_to_maturity = as.numeric(maturity_date - reference_date)
      )

    # Add auction info if available
    if (!is.null(auction_raw)) {
      auction_info <- auction_raw %>%
        dplyr::group_by(bond) %>%
        dplyr::summarise(
          first_auction = min(offer_date, na.rm = TRUE),
          last_auction = max(offer_date, na.rm = TRUE),
          total_auctions = dplyr::n(),
          .groups = "drop"
        )

      maturity_lookup <- maturity_lookup %>%
        dplyr::left_join(auction_info, by = "bond")
    }

  } else {
    # ═══════════════════════════════════════════════════════════════════════════
    # FALLBACK: Infer maturity from auction data (less reliable)
    # ═══════════════════════════════════════════════════════════════════════════
    message("  maturity_date sheet not available - falling back to auction data inference")
    warning("Using auction data for maturity inference - consider adding a maturity_date sheet for accuracy")

    if (!is.null(auction_raw)) {
      # Create single maturity per bond (use most recent auction's maturity)
      maturity_lookup <- auction_raw %>%
        dplyr::filter(!is.na(mat_date)) %>%
        dplyr::group_by(bond) %>%
        dplyr::summarise(
          maturity_date = dplyr::first(mat_date),  # All auctions for same bond have same maturity
          first_auction = min(offer_date, na.rm = TRUE),
          last_auction = max(offer_date, na.rm = TRUE),
          total_auctions = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          is_matured = maturity_date < reference_date,
          days_to_maturity = as.numeric(maturity_date - reference_date)
        )
    } else {
      # No maturity info available at all - create empty lookup
      message("  WARNING: No maturity data available - all bonds will be treated as active")
      maturity_lookup <- tibble::tibble(
        bond = character(),
        maturity_date = as.Date(character()),
        is_matured = logical(),
        days_to_maturity = numeric()
      )
    }
  }

  # Report maturity status
  matured_bonds <- maturity_lookup %>% dplyr::filter(is_matured) %>% dplyr::pull(bond)
  active_bonds <- maturity_lookup %>% dplyr::filter(!is_matured) %>% dplyr::pull(bond)

  message("  - Total bonds with maturity info: ", nrow(maturity_lookup))
  message("  - Matured bonds (will be filtered out): ", paste(sort(matured_bonds), collapse = ", "))
  message("  - Active bonds: ", paste(sort(active_bonds), collapse = ", "))

  # --------------------------------------------------------------------------
  # STEP 2: Helper Function to Load Time Series Sheets (with #N/A handling)
  # --------------------------------------------------------------------------

  load_ts_sheet_robust <- function(sheet_name, value_col) {
    message("\n  Loading sheet: ", sheet_name)

    tryCatch({
      # CRITICAL: na parameter handles Excel error values
      # CRITICAL: guess_max scans all rows to correctly type columns with late-starting data
      df <- readxl::read_excel(file_path, sheet = sheet_name,
                               na = c("", "NA", "#N/A", "N/A", "#VALUE!", "#REF!", "#DIV/0!"),
                               guess_max = 21474836)

      # Ensure date column exists and is proper format
      if (!"date" %in% names(df)) {
        stop("Sheet '", sheet_name, "' missing 'date' column")
      }

      # Get all bond columns (everything except date)
      bond_cols <- setdiff(names(df), "date")
      message("    - Found ", length(bond_cols), " bond columns")

      # Pivot to long format
      df_long <- df %>%
        dplyr::mutate(date = lubridate::as_date(date)) %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(bond_cols),
          names_to = "bond",
          values_to = value_col
        ) %>%
        # CRITICAL: Force numeric conversion and filter out NAs
        dplyr::mutate(
          !!value_col := as.numeric(!!rlang::sym(value_col))
        ) %>%
        # Remove rows where the value is NA (bond didn't exist yet or #N/A)
        dplyr::filter(!is.na(!!rlang::sym(value_col)))

      message("    - Rows after removing NA: ", nrow(df_long))

      return(df_long)

    }, error = function(e) {
      warning("Failed to load sheet '", sheet_name, "': ", e$message)
      return(NULL)
    })
  }

  # --------------------------------------------------------------------------
  # STEP 3: Load Core Time Series Data
  # --------------------------------------------------------------------------
  message("\n[2/6] Loading core time series...")

  ytm_df <- load_ts_sheet_robust("ytm", "yield_to_maturity")
  mod_dur_df <- load_ts_sheet_robust("mod_dur", "modified_duration")
  dur_df <- load_ts_sheet_robust("dur", "duration")
  conv_df <- load_ts_sheet_robust("conv", "convexity")

  # Check that core data loaded
  if (is.null(ytm_df) || is.null(mod_dur_df)) {
    stop("Failed to load essential data (ytm or mod_dur)")
  }

  # DEBUG: Track bond counts after loading (before value filtering)
  debug_bond_count(ytm_df, "robust: after ytm load (raw)")
  debug_bond_count(mod_dur_df, "robust: after mod_dur load (raw)")

  # --------------------------------------------------------------------------
  # STEP 4: Load Optional Sheets
  # --------------------------------------------------------------------------
  message("\n[3/6] Loading optional time series...")

  clean_price_df <- load_ts_sheet_robust("clean_price", "clean_price")
  full_price_df <- load_ts_sheet_robust("full_price", "full_price")
  bpv_df <- load_ts_sheet_robust("bpv", "basis_point_value")
  accrued_df <- load_ts_sheet_robust("accrued", "accrued_interest")

  # --------------------------------------------------------------------------
  # STEP 5: Load Coupon Data (Special - Single Row)
  # --------------------------------------------------------------------------
  message("\n[4/6] Loading coupon data...")

  cpn_df <- tryCatch({
    df <- readxl::read_excel(file_path, sheet = "cpn",
                             na = c("", "NA", "#N/A", "N/A", "#VALUE!", "#REF!"),
                             guess_max = 21474836)
    bond_cols <- setdiff(names(df), "date")

    df %>%
      dplyr::select(-date) %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "bond",
        values_to = "coupon"
      ) %>%
      dplyr::mutate(coupon = as.numeric(coupon)) %>%
      dplyr::filter(!is.na(coupon)) %>%
      dplyr::distinct(bond, .keep_all = TRUE)  # One coupon per bond

  }, error = function(e) {
    warning("Failed to load coupon data: ", e$message)
    return(NULL)
  })

  # --------------------------------------------------------------------------
  # STEP 6: Combine All Data (FIXED: Use inner_join for critical fields)
  # --------------------------------------------------------------------------
  message("\n[5/6] Combining datasets...")

  # FIX: Use INNER JOIN for critical fields (YTM and ModDur)
  # This ensures we only keep rows where BOTH values exist
  # Previously left_join introduced NA values that caused corruption

  # Start with YTM as base (most critical)
  full_df <- ytm_df

  # CRITICAL FIX: Inner join for modified duration (must have both YTM and ModDur)
  if (!is.null(mod_dur_df)) {
    rows_before <- nrow(full_df)
    full_df <- full_df %>%
      dplyr::inner_join(mod_dur_df, by = c("date", "bond"))
    rows_after <- nrow(full_df)
    message(sprintf("    - Inner join with mod_dur: %d -> %d rows", rows_before, rows_after))
    debug_bond_count(full_df, "robust: after INNER join")
  }

  # CRITICAL FIX: Validate immediately after core join
  # Filter out any rows with invalid YTM or ModDur values
  # NOTE: Changed YTM threshold from 2% to 1.5% to keep bonds with lower but valid yields
  bonds_before_filter <- unique(full_df$bond)
  rows_before_filter <- nrow(full_df)
  full_df <- full_df %>%
    dplyr::filter(
      !is.na(yield_to_maturity),
      !is.na(modified_duration),
      yield_to_maturity > 1.5,  # Filter out placeholder values only (was 2%)
      yield_to_maturity < 20,   # SA bonds never above 20%
      modified_duration > 0.1   # Duration must be positive and reasonable
    )
  rows_after_filter <- nrow(full_df)
  bonds_after_filter <- unique(full_df$bond)

  if (rows_before_filter != rows_after_filter) {
    message(sprintf("    - Filtered out %d rows with invalid YTM/ModDur values",
                    rows_before_filter - rows_after_filter))
  }

  # DEBUG: Show which bonds were lost during value filtering
  lost_bonds <- setdiff(bonds_before_filter, bonds_after_filter)
  if (length(lost_bonds) > 0) {
    message(sprintf("    ⚠ Bonds lost during value filter (all data was placeholders): %s",
                    paste(sort(lost_bonds), collapse = ", ")))
    message("      These bonds have NO valid YTM data (all values <= 1.5%% or NA)")
  }
  debug_bond_count(full_df, "robust: after quality filter")

  # Debug checkpoint: Check for suspicious values after core join
  debug_bond_values(full_df, "After YTM+ModDur join")

  # Join duration (optional - use left_join)
  if (!is.null(dur_df)) {
    full_df <- full_df %>%
      dplyr::left_join(dur_df, by = c("date", "bond"))
  }

  # Join convexity (optional - use left_join)
  if (!is.null(conv_df)) {
    full_df <- full_df %>%
      dplyr::left_join(conv_df, by = c("date", "bond"))
  }

  # Join coupon (by bond only, not date)
  if (!is.null(cpn_df)) {
    full_df <- full_df %>%
      dplyr::left_join(cpn_df, by = "bond")
  }

  # Join optional sheets
  if (!is.null(clean_price_df)) {
    full_df <- full_df %>%
      dplyr::left_join(clean_price_df, by = c("date", "bond"))
  }

  if (!is.null(full_price_df)) {
    full_df <- full_df %>%
      dplyr::left_join(full_price_df, by = c("date", "bond"))
  }

  if (!is.null(bpv_df)) {
    full_df <- full_df %>%
      dplyr::left_join(bpv_df, by = c("date", "bond"))
  }

  if (!is.null(accrued_df)) {
    full_df <- full_df %>%
      dplyr::left_join(accrued_df, by = c("date", "bond"))
  }

  # Join maturity information (by bond only)
  full_df <- full_df %>%
    dplyr::left_join(
      maturity_lookup %>% dplyr::select(bond, maturity_date, is_matured, days_to_maturity),
      by = "bond"
    )

  # Join auction metrics (by date AND bond)
  auction_metrics <- auction_raw %>%
    dplyr::select(bond, date = offer_date, offer, bids, bid_to_cover, allocation) %>%
    dplyr::filter(!is.na(date))

  full_df <- full_df %>%
    dplyr::left_join(auction_metrics, by = c("date", "bond"))

  message("  - Combined rows: ", nrow(full_df))
  message("  - Unique bonds before filtering: ", dplyr::n_distinct(full_df$bond))

  # --------------------------------------------------------------------------
  # STEP 7: Add Calculated Fields and Filter
  # --------------------------------------------------------------------------
  message("\n[6/6] Adding calculated fields and filtering matured bonds...")

  full_df <- full_df %>%
    dplyr::mutate(
      # Time-based groupings
      year = lubridate::year(date),
      quarter = lubridate::quarter(date),
      month = lubridate::month(date),
      week = lubridate::week(date),

      # Calculate DV01 if not provided
      dv01 = dplyr::case_when(
        !is.na(basis_point_value) ~ basis_point_value,
        !is.na(modified_duration) & !is.na(full_price) ~ modified_duration * full_price / 10000,
        !is.na(modified_duration) ~ modified_duration * 100 / 10000,  # Assume par
        TRUE ~ NA_real_
      ),

      # Maturity bucket based on actual time to maturity
      maturity_bucket = dplyr::case_when(
        days_to_maturity <= 3*365 ~ "Short (0-3Y)",
        days_to_maturity <= 7*365 ~ "Medium (3-7Y)",
        days_to_maturity <= 12*365 ~ "Long (7-12Y)",
        days_to_maturity > 12*365 ~ "Ultra-Long (12Y+)",
        TRUE ~ "Unknown"
      ),

      # Auction success flag
      auction_success = dplyr::case_when(
        is.na(bid_to_cover) ~ NA_character_,
        bid_to_cover >= 3 ~ "Strong",
        bid_to_cover >= 2 ~ "Adequate",
        TRUE ~ "Weak"
      )
    ) %>%
    # CRITICAL: Remove matured bonds
    dplyr::filter(!is_matured | is.na(is_matured)) %>%
    # CRITICAL: Remove rows where core data is missing (NO FALLBACKS!)
    dplyr::filter(!is.na(yield_to_maturity) & !is.na(modified_duration)) %>%
    # Arrange by date and bond
    dplyr::arrange(date, bond)

  message("  - Final rows after maturity filter: ", nrow(full_df))
  message("  - Final unique bonds: ", dplyr::n_distinct(full_df$bond))
  debug_bond_count(full_df, "robust: FINAL OUTPUT")

  # --------------------------------------------------------------------------
  # Create Summary Tables
  # --------------------------------------------------------------------------

  # Bond metadata summary
  bond_metadata <- full_df %>%
    dplyr::group_by(bond) %>%
    dplyr::summarise(
      coupon = dplyr::first(stats::na.omit(coupon)),
      maturity_date = dplyr::first(maturity_date),
      avg_duration = mean(modified_duration, na.rm = TRUE),
      avg_convexity = mean(convexity, na.rm = TRUE),
      latest_ytm = dplyr::last(stats::na.omit(yield_to_maturity)),
      first_date = min(date),
      last_date = max(date),
      total_observations = dplyr::n(),
      total_auctions = sum(!is.na(bid_to_cover)),
      avg_bid_cover = mean(bid_to_cover, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(avg_duration)

  # Auction summary
  auction_summary <- auction_raw %>%
    dplyr::filter(bond %in% unique(full_df$bond)) %>%  # Only active bonds
    dplyr::filter(!is.na(bid_to_cover)) %>%
    dplyr::group_by(bond) %>%
    dplyr::summarise(
      total_auctions = dplyr::n(),
      total_offered = sum(offer, na.rm = TRUE) / 1e9,
      avg_bid_cover = mean(bid_to_cover, na.rm = TRUE),
      min_bid_cover = min(bid_to_cover, na.rm = TRUE),
      max_bid_cover = max(bid_to_cover, na.rm = TRUE),
      last_auction = max(offer_date),
      .groups = "drop"
    )

  # --------------------------------------------------------------------------
  # Quality Report
  # --------------------------------------------------------------------------
  message("\n=== DATA QUALITY REPORT ===")
  message("Date Range: ", min(full_df$date), " to ", max(full_df$date))
  message("Total Observations: ", format(nrow(full_df), big.mark = ","))
  message("Unique Active Bonds: ", dplyr::n_distinct(full_df$bond))
  message("Active Bonds: ", paste(sort(unique(full_df$bond)), collapse = ", "))

  # Validation checks
  message("\n=== VALIDATION CHECKS ===")

  # Check for impossible values
  impossible_ytm <- full_df %>% dplyr::filter(yield_to_maturity < 0 | yield_to_maturity > 30)
  if (nrow(impossible_ytm) > 0) {
    warning("Found ", nrow(impossible_ytm), " rows with impossible YTM values")
  } else {
    message("  All YTM values in valid range (0-30%)")
  }

  impossible_dur <- full_df %>% dplyr::filter(modified_duration < 0 | modified_duration > 40)
  if (nrow(impossible_dur) > 0) {
    warning("Found ", nrow(impossible_dur), " rows with impossible duration values")
  } else {
    message("  All modified duration values in valid range (0-40)")
  }

  # Check for placeholder values (should be none - filtered at source)
  placeholder_values <- full_df %>%
    dplyr::filter(yield_to_maturity <= 1.5 | modified_duration <= 0.1 | modified_duration == 1)
  if (nrow(placeholder_values) > 0) {
    warning("UNEXPECTED: Found ", nrow(placeholder_values), " rows with placeholder values - these should have been filtered at source!")
    unique_bonds <- unique(placeholder_values$bond)
    warning("Affected bonds: ", paste(unique_bonds, collapse = ", "))
  } else {
    message("  ✓ No placeholder values detected (filtering working correctly)")
  }

  message("\n=== DATA LOADING COMPLETE ===\n")

  # Return as list
  return(list(
    full_df = tibble::as_tibble(full_df),
    bond_metadata = bond_metadata,
    auction_summary = auction_summary,
    maturity_lookup = maturity_lookup,
    auction_raw = auction_raw
  ))
}


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
        # CRITICAL: guess_max scans all rows to correctly type columns with late-starting data
        df <- readxl::read_excel(excel_path, sheet = sheet_name,
                                 na = c("", "NA", "#N/A", "N/A", "#VALUE!", "#REF!", "#DIV/0!"),
                                 guess_max = 21474836)

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

        # =====================================================================
        # CRITICAL FIX: Filter out placeholder values for YTM and ModDur
        # New bonds (R2033, R2038, R2042, R187, R188) have placeholder values
        # (1.0 or 0.0) in Excel before their issuance dates.
        # These are NOT real data and must be excluded.
        # =====================================================================
        if (value_name == "yield_to_maturity") {
            rows_before <- nrow(result)
            # SA bonds never have YTM below 2% - filter out placeholder values
            result <- result %>%
                dplyr::filter(yield_to_maturity > 1.5)
            rows_after <- nrow(result)
            if (rows_before != rows_after) {
                message(sprintf("    Filtered out %d placeholder YTM values (<=1.5%%)",
                                rows_before - rows_after))
            }
        }

        if (value_name == "modified_duration") {
            rows_before <- nrow(result)
            # Valid duration is always > 0.1 for SA bonds - filter out placeholder values
            # NOTE: Removed modified_duration != 1.0 filter as it was dropping legitimate data
            result <- result %>%
                dplyr::filter(modified_duration > 0.1)
            rows_after <- nrow(result)
            if (rows_before != rows_after) {
                message(sprintf("    Filtered out %d placeholder ModDur values (<=0.1)",
                                rows_before - rows_after))
            }
        }

        # =====================================================================
        # FIX: Count observations per bond and track which bonds have data
        # Use MIN_OBSERVATIONS threshold to allow newer bonds with limited data
        # =====================================================================
        bond_counts <- result %>%
            dplyr::group_by(bond) %>%
            dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")

        # Keep bonds with at least MIN_OBSERVATIONS (very low to include new issuances)
        bonds_with_data <- bond_counts %>%
            dplyr::filter(n_obs >= MIN_OBSERVATIONS) %>%
            dplyr::pull(bond)

        # Debug: Show which bonds would be excluded due to insufficient data
        excluded_bonds <- setdiff(available_bonds, bonds_with_data)
        if (length(excluded_bonds) > 0) {
            excluded_info <- bond_counts %>%
                dplyr::filter(bond %in% excluded_bonds)
            # Check if any excluded bonds have SOME data (just below threshold)
            has_some_data <- excluded_info %>%
                dplyr::filter(n_obs > 0)
            if (nrow(has_some_data) > 0) {
                message(sprintf("    ⚠ Bonds excluded from %s (< %d obs): %s",
                                value_name, MIN_OBSERVATIONS,
                                paste(has_some_data$bond, collapse = ", ")))
                message(sprintf("      Observation counts: %s",
                                paste(sprintf("%s=%d", has_some_data$bond, has_some_data$n_obs),
                                      collapse = ", ")))
                message("      Consider lowering MIN_OBSERVATIONS if these are valid new issuances")
            }
            # Also note bonds with ZERO observations (all placeholder values filtered out)
            has_no_data <- setdiff(excluded_bonds, has_some_data$bond)
            if (length(has_no_data) > 0) {
                message(sprintf("    ⚠ Bonds with NO valid %s data (all placeholders): %s",
                                value_name, paste(has_no_data, collapse = ", ")))
            }
        }

        # Filter to bonds with sufficient data
        result <- result %>%
            dplyr::filter(bond %in% bonds_with_data)

        message(sprintf("    Loaded %d rows for %s (%d bonds)",
                        nrow(result), value_name, length(bonds_with_data)))
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
        df <- readxl::read_excel(excel_path, sheet = "cpn",
                                 guess_max = 21474836)

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
        df <- readxl::read_excel(excel_path, sheet = "auctions",
                                 guess_max = 21474836)

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

        # ═══════════════════════════════════════════════════════════════════════
        # CLEARING YIELD VALIDATION AND NORMALIZATION
        # Fix mixed units issue: some values stored as decimals (0.0185),
        # others as percentages (11.88). SA govt bonds should be 5-15%.
        # ═══════════════════════════════════════════════════════════════════════
        if ("clearing_yield" %in% names(result)) {
            cy_values <- as.numeric(result$clearing_yield)
            cy_values <- cy_values[!is.na(cy_values)]

            if (length(cy_values) > 0) {
                # Count values by type
                n_looks_decimal <- sum(cy_values < 0.5, na.rm = TRUE)
                n_looks_percent <- sum(cy_values >= 1 & cy_values <= 20, na.rm = TRUE)

                message(sprintf("    Clearing yield analysis: %d values < 0.5 (decimals), %d values 1-20 (percentages)",
                                n_looks_decimal, n_looks_percent))

                # Store original for debugging
                result <- result %>%
                    dplyr::mutate(clearing_yield_raw = clearing_yield)

                # Normalize: Convert decimals to percentages
                result <- result %>%
                    dplyr::mutate(
                        clearing_yield = dplyr::case_when(
                            is.na(clearing_yield) ~ NA_real_,
                            # Values < 0.5 are almost certainly decimals (0.0185 -> 1.85%)
                            clearing_yield < 0.5 ~ clearing_yield * 100,
                            # Values 0.5-1.0 are ambiguous but likely decimals in SA context
                            clearing_yield >= 0.5 & clearing_yield < 1 ~ clearing_yield * 100,
                            # Values 1-20 are already in percentage form
                            clearing_yield >= 1 & clearing_yield <= 20 ~ clearing_yield,
                            # Values > 20 are suspicious but keep as is
                            TRUE ~ clearing_yield
                        )
                    )

                # Validate results and warn if still problematic
                cy_fixed <- result$clearing_yield[!is.na(result$clearing_yield)]
                if (length(cy_fixed) > 0) {
                    n_in_range <- sum(cy_fixed >= 5 & cy_fixed <= 15, na.rm = TRUE)
                    pct_in_range <- n_in_range / length(cy_fixed) * 100

                    message(sprintf("    After normalization: %.1f%% of clearing yields in expected 5-15%% range",
                                    pct_in_range))

                    if (pct_in_range < 70) {
                        message("    ⚠️ WARNING: Many clearing yields still outside expected range - check Excel source data!")
                    }
                }
            }
        }

        # Apply same normalization to best_bid and worst_bid if present
        for (yield_col in c("best_bid", "worst_bid")) {
            if (yield_col %in% names(result)) {
                result <- result %>%
                    dplyr::mutate(
                        !!yield_col := dplyr::case_when(
                            is.na(.data[[yield_col]]) ~ NA_real_,
                            .data[[yield_col]] < 0.5 ~ .data[[yield_col]] * 100,
                            .data[[yield_col]] >= 0.5 & .data[[yield_col]] < 1 ~ .data[[yield_col]] * 100,
                            TRUE ~ .data[[yield_col]]
                        )
                    )
            }
        }

        message(sprintf("    Loaded %d auction records with %d columns",
                       nrow(result), ncol(result)))

        return(result)

    }, error = function(e) {
        warning(sprintf("Error loading auction data: %s", e$message))
        return(NULL)
    })
}


# ================================================================================
# BOND MATURITY LOOKUP - COMPREHENSIVE MATURITY DATE MANAGEMENT
# Fixes Problem 1 & 2: Ensures all bonds have proper maturity dates
# ================================================================================

#' Create comprehensive bond maturity lookup table
#'
#' Creates a lookup table with one row per bond containing the maturity date.
#' Priority order for maturity date sources:
#' 1. PRIMARY: Dedicated maturity_date sheet in Excel (authoritative source)
#' 2. FALLBACK: Auction data mat_date column (if maturity_date sheet unavailable)
#'
#' Note: Manual/hardcoded maturity entries have been removed to prevent conflicts.
#' All maturity dates should come from the Excel file.
#'
#' @param auction_df Auction dataframe with mat_date/mature_date column (optional, used as fallback)
#' @param excel_path Path to Excel file to read maturity_date sheet (optional but recommended)
#' @return Tibble with one row per bond containing: bond, mature_date, maturity_source
#' @export
create_bond_maturity_lookup <- function(auction_df = NULL, excel_path = NULL) {
    message("\n╔════════════════════════════════════════════════════════╗")
    message("║        CREATING BOND MATURITY LOOKUP                   ║")
    message("╚════════════════════════════════════════════════════════╝")

    # ═══════════════════════════════════════════════════════════════════════
    # PRIMARY SOURCE: Dedicated maturity_date sheet
    # This is the authoritative source for bond maturity dates
    # ═══════════════════════════════════════════════════════════════════════
    maturity_from_sheet <- NULL

    if (!is.null(excel_path)) {
        message("  [1] Checking for maturity_date sheet (PRIMARY SOURCE)...")
        maturity_from_sheet <- load_maturity_dates(excel_path)

        if (!is.null(maturity_from_sheet) && nrow(maturity_from_sheet) > 0) {
            message("  Using maturity dates from maturity_date sheet (PRIMARY SOURCE)")

            # Convert to standard format
            combined <- maturity_from_sheet %>%
                dplyr::rename(mature_date = maturity_date) %>%
                dplyr::mutate(maturity_source = "maturity_date_sheet")

            # Summary
            message(sprintf("\n  Bond maturity lookup created: %d bonds total", nrow(combined)))
            message(sprintf("    - From maturity_date sheet: %d", nrow(combined)))
            message(sprintf("    Bonds: %s", paste(sort(combined$bond), collapse = ", ")))

            message("╚════════════════════════════════════════════════════════╝\n")
            return(combined)
        }
    }

    # ═══════════════════════════════════════════════════════════════════════
    # FALLBACK: Extract maturity dates from auction data
    # Only used if maturity_date sheet is unavailable
    # ═══════════════════════════════════════════════════════════════════════
    message("  [2] maturity_date sheet not available - falling back to auction data...")
    warning("maturity_date sheet not available - using auction data for maturity inference (less reliable)")

    auction_maturities <- tibble::tibble(bond = character(), mature_date = as.Date(character()), maturity_source = character())

    if (!is.null(auction_df) && "mature_date" %in% names(auction_df)) {
        auction_maturities <- auction_df %>%
            dplyr::filter(!is.na(mature_date)) %>%
            dplyr::group_by(bond) %>%
            dplyr::summarise(
                mature_date = max(mature_date, na.rm = TRUE),
                maturity_source = "auction_data_fallback",
                .groups = "drop"
            )
        message(sprintf("  Maturity dates from auction data: %d bonds", nrow(auction_maturities)))
    } else {
        message("  WARNING: No mature_date column found in auction data!")
    }

    # Note: Manual/hardcoded maturity entries have been REMOVED
    # All maturity data should come from the Excel file (either maturity_date sheet or auctions sheet)
    # This prevents conflicts and ensures a single source of truth

    combined <- auction_maturities

    # Summary
    message(sprintf("\n  Bond maturity lookup created: %d bonds total", nrow(combined)))
    message(sprintf("    - From auction data (fallback): %d", nrow(combined)))

    if (nrow(combined) > 0) {
        message(sprintf("    Bonds: %s", paste(sort(combined$bond), collapse = ", ")))
    } else {
        message("    WARNING: No maturity dates available - bonds with unknown maturity will be assumed ACTIVE")
    }

    message("╚════════════════════════════════════════════════════════╝\n")

    return(combined)
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
        mod_dur_raw <- readxl::read_excel(excel_path, sheet = "mod_dur",
                                           guess_max = 21474836)
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

    # DEBUG: Track bond counts after loading time series sheets
    if (!is.null(ts_data_list[["mod_dur"]])) {
        debug_bond_count(ts_data_list[["mod_dur"]], "after mod_dur load")
    }
    if (!is.null(ts_data_list[["ytm"]])) {
        debug_bond_count(ts_data_list[["ytm"]], "after ytm load")
    }

    # DIAGNOSTIC CHECKPOINT 1: Verify YTM loaded correctly
    if (run_diagnostics && !is.null(ts_data_list[["ytm"]])) {
        # Show bond data availability summary
        ytm_summary <- ts_data_list[["ytm"]] %>%
            dplyr::group_by(bond) %>%
            dplyr::summarise(
                first_date = min(date),
                last_date = max(date),
                min_ytm = min(yield_to_maturity),
                max_ytm = max(yield_to_maturity),
                n_obs = dplyr::n(),
                .groups = "drop"
            )

        # Show new bonds (data starts after 2024-01-01)
        new_bonds <- ytm_summary %>%
            dplyr::filter(first_date > as.Date("2024-01-01"))
        if (nrow(new_bonds) > 0) {
            message("\n  📊 New bonds (data starts after 2024-01-01):")
            for (i in 1:nrow(new_bonds)) {
                row <- new_bonds[i, ]
                message(sprintf("    %s: %s to %s (%d obs, YTM %.1f%%-%.1f%%)",
                                row$bond, row$first_date, row$last_date, row$n_obs,
                                row$min_ytm, row$max_ytm))
            }
        }

        # Check for unusual YTM values on latest date (after placeholder filtering)
        ytm_check <- ts_data_list[["ytm"]] %>%
            dplyr::filter(date == max(date)) %>%
            dplyr::filter(yield_to_maturity < 5 | yield_to_maturity > 15)
        if (nrow(ytm_check) > 0) {
            message("\n  ⚠ WARNING: Bonds with unusual YTM values on latest date:")
            for (i in 1:min(nrow(ytm_check), 5)) {
                message(sprintf("    %s: %.2f%%", ytm_check$bond[i], ytm_check$yield_to_maturity[i]))
            }
        } else {
            message("    ✓ All YTM values on latest date are within normal range (5-15%)")
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

    # =========================================================================
    # CRITICAL FIX: Use INNER JOIN for critical fields to prevent NA corruption
    # =========================================================================

    # Start with modified duration as the base
    full_df <- ts_data_list[["mod_dur"]]

    if (is.null(full_df)) {
        stop("Failed to load modified_duration - cannot proceed")
    }

    # Track rows before and after joins for diagnostics
    rows_before <- nrow(full_df)
    message(sprintf("    Base (mod_dur): %d rows", rows_before))

    # CRITICAL FIX: INNER JOIN for YTM (must have both mod_dur and ytm)
    ytm_data <- ts_data_list[["ytm"]]
    if (!is.null(ytm_data)) {
        full_df <- full_df %>%
            dplyr::inner_join(ytm_data, by = c("date", "bond"))
        message(sprintf("    After INNER join with ytm: %d rows (was %d)", nrow(full_df), rows_before))
        debug_bond_count(full_df, "after INNER join")
    } else {
        stop("Failed to load YTM data - cannot proceed")
    }

    # CRITICAL FIX: Filter out invalid values immediately after core join
    # NOTE: Changed YTM threshold from 2% to 1.5% to keep bonds with lower but valid yields
    bonds_before_filter <- unique(full_df$bond)
    rows_before_filter <- nrow(full_df)
    full_df <- full_df %>%
        dplyr::filter(
            !is.na(yield_to_maturity),
            !is.na(modified_duration),
            yield_to_maturity > 1.5,  # Filter out placeholder values only (was 2%)
            yield_to_maturity < 20,   # SA bonds never above 20%
            modified_duration > 0.1   # Duration must be positive and reasonable
        )
    rows_after_filter <- nrow(full_df)
    bonds_after_filter <- unique(full_df$bond)

    if (rows_before_filter != rows_after_filter) {
        message(sprintf("    Filtered out %d rows with invalid YTM/ModDur values",
                        rows_before_filter - rows_after_filter))
    }

    # DEBUG: Show which bonds were lost during value filtering
    lost_bonds <- setdiff(bonds_before_filter, bonds_after_filter)
    if (length(lost_bonds) > 0) {
        message(sprintf("    ⚠ Bonds lost during value filter (all data was placeholders): %s",
                        paste(sort(lost_bonds), collapse = ", ")))
        message("      These bonds have NO valid data after joining (all YTM <= 1.5%% or NA)")
    }
    debug_bond_count(full_df, "after quality filter")

    # Debug checkpoint for critical fields
    debug_bond_values(full_df, "After YTM+ModDur core join")

    # LEFT JOIN for remaining optional time series data
    for (sheet in names(time_series_sheets)) {
        if (sheet %in% c("mod_dur", "ytm")) next  # Already joined these

        sheet_data <- ts_data_list[[sheet]]
        if (!is.null(sheet_data)) {
            full_df <- full_df %>%
                dplyr::left_join(sheet_data, by = c("date", "bond"))
        }
    }

    # DIAGNOSTIC CHECKPOINT 2: Verify no corruption after all joins
    if (run_diagnostics) {
        na_ytm <- sum(is.na(full_df$yield_to_maturity))
        na_dur <- sum(is.na(full_df$modified_duration))
        if (na_ytm > 0 || na_dur > 0) {
            message(sprintf("\n  ⚠ WARNING: NA values in critical fields - YTM: %d, Duration: %d",
                            na_ytm, na_dur))
        } else {
            message("    ✓ No NA values in critical fields (YTM, ModDur)")
        }
        debug_bond_values(full_df, "After all joins")
    }

    # Join coupon data (by bond only - static)
    if (!is.null(cpn_df)) {
        full_df <- full_df %>%
            dplyr::left_join(cpn_df, by = "bond")
    }

    # ═══════════════════════════════════════════════════════════════════════════
    # FIX: PROPER MATURITY DATE HANDLING
    # Problem: Previously joined auction_df by (date, bond), so mature_date
    # only appeared on auction dates (sparse). Now we:
    # 1. Create a maturity LOOKUP table (one row per bond)
    # 2. Join maturity dates by BOND ONLY (propagates to all dates)
    # 3. Join auction METRICS by date+bond (for date-specific data like bid_to_cover)
    # ═══════════════════════════════════════════════════════════════════════════

    if (!is.null(auction_df)) {
        message("\n  Applying maturity date fix (join by bond only)...")

        # Step 1: Create maturity lookup table (uses maturity_date sheet as PRIMARY source)
        bond_maturity_lookup <- create_bond_maturity_lookup(auction_df, excel_path)

        # Step 2: Join MATURITY DATES by BOND ONLY (propagates to all dates!)
        full_df <- full_df %>%
            dplyr::left_join(
                bond_maturity_lookup %>% dplyr::select(bond, mature_date),
                by = "bond"
            )

        # Check maturity coverage
        n_with_maturity <- sum(!is.na(full_df$mature_date))
        n_total <- nrow(full_df)
        pct_coverage <- round(100 * n_with_maturity / n_total, 1)
        message(sprintf("  ✓ Maturity date coverage: %d/%d rows (%.1f%%)",
                       n_with_maturity, n_total, pct_coverage))

        # Step 3: Join AUCTION METRICS by date+bond (for date-specific data)
        # These columns are only relevant on auction dates
        auction_metrics <- auction_df %>%
            dplyr::select(
                date, bond,
                # Keep these from original - date-specific metrics
                dplyr::any_of(c(
                    "offer_amount", "allocation", "bids_received", "bid_to_cover",
                    "offer_date", "announcement_date", "settle_date",
                    "bond_coupon", "clearing_yield", "non_comps",
                    "number_bids_received", "best_bid", "worst_bid", "auction_tail"
                ))
            ) %>%
            # Remove mature_date from this join (already joined above)
            dplyr::select(-dplyr::any_of("mature_date"))

        full_df <- full_df %>%
            dplyr::left_join(auction_metrics, by = c("date", "bond"))

        message("  ✓ Auction metrics joined by (date, bond)")

    } else {
        message("  ⚠ No auction data - maturity dates will be NA")
    }

    # ═══════════════════════════════════════════════════════════════════════════
    # Calculate time_to_maturity now that mature_date is properly populated
    # ═══════════════════════════════════════════════════════════════════════════
    full_df <- full_df %>%
        dplyr::mutate(
            time_to_maturity = dplyr::case_when(
                !is.na(mature_date) ~ as.numeric(difftime(mature_date, date, units = "days")) / 365.25,
                TRUE ~ NA_real_
            ),
            maturity_bucket = dplyr::case_when(
                is.na(time_to_maturity) ~ "Unknown",
                time_to_maturity <= 0 ~ "Matured",
                time_to_maturity <= 3 ~ "Short (0-3y)",
                time_to_maturity <= 7 ~ "Medium (3-7y)",
                time_to_maturity <= 12 ~ "Long (7-12y)",
                time_to_maturity > 12 ~ "Ultra-Long (12y+)",
                TRUE ~ "Unknown"
            )
        )

    # Log maturity bucket distribution
    if (run_diagnostics) {
        bucket_dist <- full_df %>%
            dplyr::filter(date == max(date, na.rm = TRUE)) %>%
            dplyr::count(maturity_bucket) %>%
            dplyr::arrange(dplyr::desc(n))
        message("\n  Maturity bucket distribution (latest date):")
        for (i in 1:nrow(bucket_dist)) {
            message(sprintf("    %s: %d bonds", bucket_dist$maturity_bucket[i], bucket_dist$n[i]))
        }
    }

    rows_after <- nrow(full_df)

    # DEBUG: Final bond count check
    debug_bond_count(full_df, "FINAL OUTPUT")

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

    # =========================================================================
    # CRITICAL FIX: Check for cache corruption BEFORE deciding to use it
    # =========================================================================
    cache_is_corrupted <- FALSE

    if (file.exists(cache_path) && !force_refresh) {
        message("\n  Checking cache for corruption...")
        tryCatch({
            cache_data <- readRDS(cache_path)
            cache_is_corrupted <- is_cache_corrupted(cache_data)

            if (cache_is_corrupted) {
                message("  ⚠ CORRUPTED CACHE DETECTED - Will reload from Excel")
                # Delete the corrupted cache file
                file.remove(cache_path)
                message("  Corrupted cache file deleted")
            } else {
                message("  ✓ Cache integrity check PASSED")
            }
        }, error = function(e) {
            message("  ⚠ Cache read error - will reload from Excel")
            cache_is_corrupted <- TRUE
        })
    }

    # Check if we need to reload from Excel (including corruption check)
    needs_refresh <- force_refresh || cache_is_corrupted || is_cache_stale(excel_path, cache_path)

    if (needs_refresh) {
        message("\n→ Loading fresh data from Excel...")

        # Load from Excel with diagnostics
        full_df <- load_from_excel(excel_path, run_diagnostics = verify_quality)

        # =====================================================================
        # CRITICAL FIX: Validate data BEFORE caching to prevent corruption
        # =====================================================================
        tryCatch({
            validate_data_before_cache(full_df)

            # Ensure cache directory exists
            cache_dir <- dirname(cache_path)
            if (!dir.exists(cache_dir)) {
                dir.create(cache_dir, recursive = TRUE)
            }

            saveRDS(full_df, cache_path)
            message(sprintf("\n✓ Cache saved to: %s", cache_path))
        }, error = function(e) {
            warning(sprintf("Cache validation/save failed: %s", e$message))
            message("  Data was NOT cached due to validation failure")
            message("  The data will be used for this session but may have issues")
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
            df <- readxl::read_excel(excel_path, sheet = sheet,
                                     guess_max = 21474836)
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
            df <- readxl::read_excel(excel_path, sheet = sheet,
                                     guess_max = 21474836)
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
    ytm_raw <- readxl::read_excel(excel_path, sheet = "ytm",
                                   guess_max = 21474836)
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
    dur_raw <- readxl::read_excel(excel_path, sheet = "mod_dur",
                                   guess_max = 21474836)
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
#' IMPORTANT: Maturity dates are sourced from:
#' 1. PRIMARY: maturity_date sheet in Excel (if excel_path provided)
#' 2. FALLBACK: auction data (mature_date column)
#' SA Government Bond naming conventions do NOT indicate maturity year!
#' Bonds with unknown maturity are assumed ACTIVE.
#'
#' @param full_df Data frame with all bond time series data
#' @param auction_df Optional data frame with auction data including mature_date
#' @param excel_path Optional path to Excel file to read maturity_date sheet
#' @return Tibble with bond metadata including maturity dates and status
create_bond_metadata <- function(full_df, auction_df = NULL, excel_path = NULL) {
    message("\n╔════════════════════════════════════════════════════════╗")
    message("║        CREATING BOND METADATA TABLE                    ║")
    message("╚════════════════════════════════════════════════════════╝")

    # Get all unique bonds from the data
    all_bonds <- sort(unique(full_df$bond))

    # ═══════════════════════════════════════════════════════════════════════════
    # PRIMARY SOURCE: Maturity dates from dedicated maturity_date sheet
    # ═══════════════════════════════════════════════════════════════════════════
    maturity_from_data <- NULL
    maturity_source_name <- "unknown"

    if (!is.null(excel_path)) {
        message("  Checking for maturity_date sheet (PRIMARY SOURCE)...")
        maturity_from_sheet <- load_maturity_dates(excel_path)

        if (!is.null(maturity_from_sheet) && nrow(maturity_from_sheet) > 0) {
            maturity_from_data <- maturity_from_sheet
            maturity_source_name <- "maturity_date_sheet"
            message(sprintf("  Using maturity_date sheet: %d bonds", nrow(maturity_from_data)))
        }
    }

    # ═══════════════════════════════════════════════════════════════════════════
    # FALLBACK: Extract maturity dates from auction data
    # ═══════════════════════════════════════════════════════════════════════════
    if (is.null(maturity_from_data) || nrow(maturity_from_data) == 0) {
        if (!is.null(auction_df) && "mature_date" %in% names(auction_df)) {
            maturity_from_data <- auction_df %>%
                dplyr::filter(!is.na(mature_date)) %>%
                dplyr::group_by(bond) %>%
                dplyr::summarise(
                    maturity_date = max(mature_date, na.rm = TRUE),
                    .groups = "drop"
                )
            maturity_source_name <- "auction_data_fallback"
            message(sprintf("  Maturity dates from auction data (fallback): %d bonds", nrow(maturity_from_data)))
        } else {
            maturity_from_data <- tibble::tibble(bond = character(), maturity_date = as.Date(character()))
            message("  WARNING: No maturity data available!")
        }
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

            # Source of maturity info (uses variable from outer scope)
            maturity_source = dplyr::case_when(
                !is.na(maturity_date) ~ maturity_source_name,
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
        message(sprintf("\n  Matured bonds (confirmed from %s):", maturity_source_name))
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


#' Get list of bonds to include in analysis for a given date range
#'
#' INCLUSION LOGIC (per user requirements):
#' - Bonds maturing BEFORE start_date: EXCLUDE (already matured, no longer tradeable)
#' - Bonds maturing BETWEEN start_date and end_date: INCLUDE (will be marked as "Maturing")
#' - Bonds maturing AFTER end_date: INCLUDE (will be marked as "Active")
#' - Unknown maturity: INCLUDE (conservative approach - assume active)
#'
#' IMPORTANT: Bonds with unknown maturity are ALWAYS included. The SA bond
#' naming convention does NOT encode maturity dates, so we cannot infer
#' maturity from the name. Unknown = assume active.
#'
#' @param bond_metadata Tibble created by create_bond_metadata()
#' @param start_date Start of the analysis period (Date)
#' @param end_date End of the analysis period (Date)
#' @param include_unknown Whether to include bonds with unknown maturity (default TRUE, should stay TRUE!)
#' @return Character vector of bond names to include in analysis
get_active_bonds <- function(bond_metadata, start_date, end_date, include_unknown = TRUE) {
    # Include bonds that:
    # 1. Unknown maturity = assume active (we can't infer from names!)
    # 2. Known maturity that is ON or AFTER the start_date (not already matured)
    #    - This includes bonds maturing DURING the period (will be marked "Maturing")
    #    - And bonds maturing AFTER the period (will be marked "Active")

    active_bonds <- bond_metadata %>%
        dplyr::filter(
            # Unknown maturity = always assume active
            is.na(final_maturity_date) |
            # Include if maturity is ON or AFTER the analysis START date
            # (Only exclude bonds that already matured BEFORE the analysis period)
            final_maturity_date >= start_date
        ) %>%
        dplyr::pull(bond)

    message(sprintf("  Bonds for analysis period %s to %s: %d of %d (excluding already-matured)",
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
