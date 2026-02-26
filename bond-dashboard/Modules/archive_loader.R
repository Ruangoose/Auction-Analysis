# ================================================================================
# ARCHIVE DATA LOADER MODULE
# Handles CSV archive ingestion from archive_staging/ and hybrid data loading
# that combines archived .rds data with current Excel data.
#
# Part of the SA Government Bond Analytics Dashboard for Insele Capital Partners.
# ================================================================================

# CSV prefix -> (sheet_name, value_column) mapping
# This is the single source of truth for the 8 archived time series.
ARCHIVE_SERIES_MAP <- list(
  ytm           = list(sheet = "ytm",         col = "yield_to_maturity"),
  mod_dur       = list(sheet = "mod_dur",     col = "modified_duration"),
  dur           = list(sheet = "dur",         col = "duration"),
  conv          = list(sheet = "conv",        col = "convexity"),
  clean_price   = list(sheet = "clean_price", col = "clean_price"),
  bpv           = list(sheet = "bpv",         col = "basis_point_value"),
  accrued       = list(sheet = "accrued",     col = "accrued_interest"),
  full_price    = list(sheet = "full_price",  col = "full_price")
)


# ==============================================================================
# ARCHIVE INGESTION: process_archive_staging()
# ==============================================================================

#' Process staged CSV archives into the master bond_archive.rds
#'
#' On app startup this function checks data/archive_staging/ for CSVs exported
#' by the Excel VBA archiving macro. Each CSV is read, pivoted to long format,
#' merged (with deduplication) into data/bond_archive.rds, and then deleted.
#'
#' @param data_dir Path to the data/ directory (contains archive_staging/ and bond_archive.rds)
#' @return Invisible TRUE if files were processed, FALSE otherwise
#' @export
process_archive_staging <- function(data_dir = "data") {

  staging_dir  <- file.path(data_dir, "archive_staging")
  archive_path <- file.path(data_dir, "bond_archive.rds")

  # Ensure the staging directory exists (VBA expects it)
  if (!dir.exists(staging_dir)) {
    dir.create(staging_dir, recursive = TRUE)
    message("[Archive] Created archive_staging/ directory")
  }

  # List CSV files in the staging directory

  csv_files <- list.files(staging_dir, pattern = "\\.csv$", full.names = TRUE)

  if (length(csv_files) == 0) {
    message("[Archive] No staged CSVs found — nothing to process")
    return(invisible(FALSE))
  }

  message(sprintf("[Archive] Found %d staged CSV(s) to process", length(csv_files)))

  # Load existing archive (or create empty structure)
  archive <- load_or_create_archive(archive_path)

  # Track what we processed for the summary log
  processed_log <- list()

  for (csv_path in csv_files) {
    fname <- basename(csv_path)
    message(sprintf("[Archive]   Processing: %s", fname))

    # Parse the CSV prefix to identify the series
    # Expected format: {prefix}_archive_{YYYYMMDD}.csv
    series_prefix <- sub("_archive_.*$", "", fname)

    if (!series_prefix %in% names(ARCHIVE_SERIES_MAP)) {
      warning(sprintf("[Archive]   Skipping unrecognised CSV prefix '%s' in file: %s",
                      series_prefix, fname))
      next
    }

    series_info <- ARCHIVE_SERIES_MAP[[series_prefix]]
    value_col   <- series_info$col

    # Read CSV safely
    series_df <- tryCatch({
      read_archive_csv(csv_path, value_col)
    }, error = function(e) {
      warning(sprintf("[Archive]   ERROR reading %s: %s — skipping", fname, e$message))
      NULL
    })

    if (is.null(series_df) || nrow(series_df) == 0) {
      warning(sprintf("[Archive]   No usable rows in %s — skipping", fname))
      next
    }

    # Merge into archive (dedup by date+bond, keep newer values)
    existing <- archive[[value_col]]
    if (!is.null(existing) && nrow(existing) > 0) {
      combined <- dplyr::bind_rows(existing, series_df)
      # Keep the last occurrence (from the newly-staged CSV) on duplicate keys
      combined <- combined %>%
        dplyr::group_by(date, bond) %>%
        dplyr::slice_tail(n = 1) %>%
        dplyr::ungroup()
    } else {
      combined <- series_df
    }

    archive[[value_col]] <- combined

    processed_log[[fname]] <- list(
      series    = value_col,
      rows      = nrow(series_df),
      date_min  = min(series_df$date),
      date_max  = max(series_df$date),
      n_bonds   = dplyr::n_distinct(series_df$bond)
    )
  }

  # Save updated archive — only if we actually processed something

  if (length(processed_log) > 0) {
    tryCatch({
      saveRDS(archive, archive_path)
      message(sprintf("[Archive] Saved updated bond_archive.rds (%s)",
                      format(file.size(archive_path), big.mark = ",")))
    }, error = function(e) {
      warning(sprintf("[Archive] FAILED to save bond_archive.rds: %s", e$message))
      warning("[Archive] Staged CSVs will NOT be deleted (data safety)")
      return(invisible(FALSE))
    })

    # Delete consumed CSVs only after successful save
    for (csv_path in csv_files) {
      fname <- basename(csv_path)
      if (!fname %in% names(processed_log)) next  # skip files we didn't process

      tryCatch({
        file.remove(csv_path)
        if (file.exists(csv_path)) {
          warning(sprintf("[Archive]   Could not delete %s — file still exists", fname))
        } else {
          message(sprintf("[Archive]   Deleted: %s", fname))
        }
      }, error = function(e) {
        warning(sprintf("[Archive]   Could not delete %s: %s", fname, e$message))
      })
    }

    # Print summary
    message("[Archive] === Processing Summary ===")
    total_rows <- 0
    for (fname in names(processed_log)) {
      info <- processed_log[[fname]]
      message(sprintf("[Archive]   %s -> %s: %s rows, %d bonds, dates %s to %s",
                      fname, info$series,
                      format(info$rows, big.mark = ","),
                      info$n_bonds,
                      info$date_min, info$date_max))
      total_rows <- total_rows + info$rows
    }
    message(sprintf("[Archive] Total: %d files processed, %s rows ingested",
                    length(processed_log), format(total_rows, big.mark = ",")))
  }

  return(invisible(length(processed_log) > 0))
}


#' Read a single archive CSV and pivot to long format
#'
#' @param csv_path Full path to the CSV file
#' @param value_col Name for the value column after pivoting
#' @return Tibble with columns: date, bond, <value_col>
read_archive_csv <- function(csv_path, value_col) {

  # Use data.table::fread if available for speed, else base read.csv
  if (requireNamespace("data.table", quietly = TRUE)) {
    raw <- data.table::fread(csv_path, na.strings = c("", "NA", "#N/A", "#VALUE!", "#REF!"),
                             data.table = FALSE)
  } else {
    raw <- utils::read.csv(csv_path, na.strings = c("", "NA", "#N/A", "#VALUE!", "#REF!"),
                           stringsAsFactors = FALSE, check.names = FALSE)
  }

  if (nrow(raw) == 0) return(tibble::tibble())

  # Ensure date column exists
  if (!"date" %in% tolower(names(raw))) {
    stop("CSV missing 'date' column")
  }
  # Normalise the date column name to lowercase
  names(raw)[tolower(names(raw)) == "date"] <- "date"

  # Bond columns = everything except date
  bond_cols <- setdiff(names(raw), "date")

  if (length(bond_cols) == 0) {
    stop("CSV has no bond columns")
  }

  # Force bond columns to numeric (handles empty strings)
  for (col in bond_cols) {
    raw[[col]] <- suppressWarnings(as.numeric(raw[[col]]))
  }

  # Parse dates
  raw$date <- as.Date(raw$date)

  # Pivot to long format
  long_df <- raw %>%
    tidyr::pivot_longer(
      cols  = dplyr::all_of(bond_cols),
      names_to  = "bond",
      values_to = value_col
    ) %>%
    dplyr::filter(!is.na(!!rlang::sym(value_col))) %>%
    tibble::as_tibble()

  return(long_df)
}


#' Load existing bond_archive.rds or create an empty archive structure
#'
#' @param archive_path Path to bond_archive.rds
#' @return Named list of tibbles, one per series
load_or_create_archive <- function(archive_path) {

  if (file.exists(archive_path)) {
    tryCatch({
      archive <- readRDS(archive_path)
      message(sprintf("[Archive] Loaded existing bond_archive.rds (%s)",
                      format(file.size(archive_path), big.mark = ",")))
      # Ensure all expected series keys exist
      for (info in ARCHIVE_SERIES_MAP) {
        if (is.null(archive[[info$col]])) {
          archive[[info$col]] <- tibble::tibble(
            date = as.Date(character()),
            bond = character()
          )
          archive[[info$col]][[info$col]] <- numeric()
        }
      }
      return(archive)
    }, error = function(e) {
      warning(sprintf("[Archive] Could not read bond_archive.rds: %s — creating fresh", e$message))
    })
  }

  # Create empty structure
  archive <- list()
  for (info in ARCHIVE_SERIES_MAP) {
    df <- tibble::tibble(
      date = as.Date(character()),
      bond = character()
    )
    df[[info$col]] <- numeric()
    archive[[info$col]] <- df
  }
  message("[Archive] Created new empty archive structure")
  return(archive)
}


# ==============================================================================
# HYBRID DATA LOADING: load_bond_data_hybrid()
# ==============================================================================

#' Load bond data from archive .rds + current Excel, combining seamlessly
#'
#' This is the main entry point that replaces the Excel-only loading path.
#' It produces the same output structure as the existing load_bond_data_robust().
#'
#' Flow:
#'   1. process_archive_staging() — ingest any new CSVs
#'   2. Read bond_archive.rds (historical data)
#'   3. Read current Excel sheets (recent ~90 days + non-archived sheets)
#'   4. Combine archive + Excel per series (dedup by date+bond)
#'   5. Join all series, add derived fields, return standard result list
#'
#' @param excel_path Path to the .xlsm / .xlsx Excel file
#' @param data_dir   Path to the data/ directory (for archive_staging/ and bond_archive.rds)
#' @param reference_date Date for maturity filtering (default: today)
#' @return Named list: full_df, bond_metadata, auction_summary, maturity_lookup, auction_raw
#' @export
load_bond_data_hybrid <- function(excel_path,
                                  data_dir = dirname(excel_path),
                                  reference_date = Sys.Date()) {

  message("=== HYBRID BOND DATA LOADING ===")
  message("Excel: ", excel_path)
  message("Data dir: ", data_dir)
  message("Reference date: ", reference_date)

  # ---- Step 0: Process any staged archive CSVs ----
  process_archive_staging(data_dir)

  archive_path <- file.path(data_dir, "bond_archive.rds")

  # ---- Step 1: Load archived data (may be empty on first run) ----
  archive <- load_or_create_archive(archive_path)

  archive_has_data <- any(sapply(archive, function(df) !is.null(df) && nrow(df) > 0))
  if (archive_has_data) {
    archive_rows <- sum(sapply(archive, nrow))
    message(sprintf("[Hybrid] Archive loaded: %s total rows across %d series",
                    format(archive_rows, big.mark = ","),
                    sum(sapply(archive, function(df) nrow(df) > 0))))
  } else {
    message("[Hybrid] No archive data — loading everything from Excel")
  }

  # ---- Step 2: Verify Excel file exists ----
  if (!file.exists(excel_path)) {
    # Try to work from archive alone if Excel is unavailable
    if (archive_has_data) {
      warning("[Hybrid] Excel file not found: ", excel_path,
              " — attempting archive-only load")
    } else {
      stop("[Hybrid] Excel file not found and no archive exists: ", excel_path)
    }
  }

  excel_available <- file.exists(excel_path)

  # ---- Step 3: Load time series from Excel (the 8 archived series) ----
  excel_ts <- list()

  if (excel_available) {
    message("\n[Hybrid] Loading time series from Excel...")

    for (prefix in names(ARCHIVE_SERIES_MAP)) {
      info <- ARCHIVE_SERIES_MAP[[prefix]]
      sheet_name <- info$sheet
      value_col  <- info$col

      excel_ts[[value_col]] <- tryCatch({
        load_ts_sheet_for_hybrid(excel_path, sheet_name, value_col)
      }, error = function(e) {
        warning(sprintf("[Hybrid]   Sheet '%s' failed: %s", sheet_name, e$message))
        NULL
      })
    }
  }

  # ---- Step 4: Combine archive + Excel for each series ----
  message("\n[Hybrid] Combining archive + Excel data...")

  combined_ts <- list()
  for (prefix in names(ARCHIVE_SERIES_MAP)) {
    info      <- ARCHIVE_SERIES_MAP[[prefix]]
    value_col <- info$col

    arch_df  <- archive[[value_col]]
    excel_df <- excel_ts[[value_col]]

    combined_ts[[value_col]] <- combine_series(arch_df, excel_df, value_col)
  }

  # Log combined date ranges
  for (value_col in names(combined_ts)) {
    df <- combined_ts[[value_col]]
    if (!is.null(df) && nrow(df) > 0) {
      message(sprintf("[Hybrid]   %s: %s rows, %d bonds, %s to %s",
                      value_col,
                      format(nrow(df), big.mark = ","),
                      dplyr::n_distinct(df$bond),
                      min(df$date), max(df$date)))
    }
  }

  # ---- Step 5: Load non-archived sheets from Excel ----
  message("\n[Hybrid] Loading non-archived sheets from Excel...")

  # Maturity lookup (STEP 1 from load_bond_data_robust)
  maturity_lookup <- NULL
  auction_raw     <- NULL
  cpn_df          <- NULL

  if (excel_available) {
    # Auction data
    auction_raw <- tryCatch({
      readxl::read_excel(excel_path, sheet = "auctions",
                         na = c("", "NA", "#N/A", "N/A", "#VALUE!", "#REF!"),
                         guess_max = 21474836) %>%
        dplyr::mutate(
          mat_date   = lubridate::as_date(mat_date),
          offer_date = lubridate::as_date(offer_date)
        )
    }, error = function(e) {
      warning("[Hybrid] Failed to read auctions sheet: ", e$message)
      NULL
    })

    # Maturity dates (primary: maturity_date sheet, fallback: auctions)
    maturity_from_sheet <- load_maturity_dates(excel_path)
    if (!is.null(maturity_from_sheet) && nrow(maturity_from_sheet) > 0) {
      message("[Hybrid] Using maturity dates from maturity_date sheet")
      maturity_lookup <- maturity_from_sheet %>%
        dplyr::mutate(
          is_matured      = maturity_date < reference_date,
          days_to_maturity = as.numeric(maturity_date - reference_date)
        )
      if (!is.null(auction_raw)) {
        auction_info <- auction_raw %>%
          dplyr::group_by(bond) %>%
          dplyr::summarise(
            first_auction  = min(offer_date, na.rm = TRUE),
            last_auction   = max(offer_date, na.rm = TRUE),
            total_auctions = dplyr::n(),
            .groups = "drop"
          )
        maturity_lookup <- maturity_lookup %>%
          dplyr::left_join(auction_info, by = "bond")
      }
    } else if (!is.null(auction_raw)) {
      message("[Hybrid] Falling back to auction data for maturity inference")
      maturity_lookup <- auction_raw %>%
        dplyr::filter(!is.na(mat_date)) %>%
        dplyr::group_by(bond) %>%
        dplyr::summarise(
          maturity_date  = dplyr::first(mat_date),
          first_auction  = min(offer_date, na.rm = TRUE),
          last_auction   = max(offer_date, na.rm = TRUE),
          total_auctions = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          is_matured       = maturity_date < reference_date,
          days_to_maturity = as.numeric(maturity_date - reference_date)
        )
    }

    # Coupon data
    cpn_df <- tryCatch({
      df <- readxl::read_excel(excel_path, sheet = "cpn",
                               na = c("", "NA", "#N/A", "N/A", "#VALUE!", "#REF!"),
                               guess_max = 21474836)
      df %>%
        dplyr::select(-dplyr::any_of("date")) %>%
        tidyr::pivot_longer(
          cols      = dplyr::everything(),
          names_to  = "bond",
          values_to = "coupon"
        ) %>%
        dplyr::mutate(coupon = as.numeric(coupon)) %>%
        dplyr::filter(!is.na(coupon)) %>%
        dplyr::distinct(bond, .keep_all = TRUE)
    }, error = function(e) {
      warning("[Hybrid] Failed to load coupon data: ", e$message)
      NULL
    })
  }

  # Empty maturity_lookup fallback
  if (is.null(maturity_lookup)) {
    maturity_lookup <- tibble::tibble(
      bond             = character(),
      maturity_date    = as.Date(character()),
      is_matured       = logical(),
      days_to_maturity = numeric()
    )
  }

  # ---- Step 6: Join all series into master full_df ----
  message("\n[Hybrid] Joining all series into master dataframe...")

  ytm_df     <- combined_ts[["yield_to_maturity"]]
  mod_dur_df <- combined_ts[["modified_duration"]]

  if (is.null(ytm_df) || nrow(ytm_df) == 0) stop("[Hybrid] No YTM data available")
  if (is.null(mod_dur_df) || nrow(mod_dur_df) == 0) stop("[Hybrid] No modified duration data available")

  # Inner join for critical fields
  full_df <- ytm_df %>%
    dplyr::inner_join(mod_dur_df, by = c("date", "bond"))

  message(sprintf("[Hybrid]   After YTM+ModDur inner join: %s rows",
                  format(nrow(full_df), big.mark = ",")))

  # Filter invalid values
  rows_before <- nrow(full_df)
  full_df <- full_df %>%
    dplyr::filter(
      !is.na(yield_to_maturity),
      !is.na(modified_duration),
      yield_to_maturity > 1.5,
      yield_to_maturity < 20,
      modified_duration > 0.1
    )
  rows_filtered <- rows_before - nrow(full_df)
  if (rows_filtered > 0) {
    message(sprintf("[Hybrid]   Filtered %d rows with invalid YTM/ModDur values", rows_filtered))
  }

  debug_bond_count(full_df, "hybrid: after quality filter")

  # Left join optional series
  optional_series <- c("duration", "convexity", "clean_price", "full_price",
                       "basis_point_value", "accrued_interest")
  for (series_col in optional_series) {
    series_data <- combined_ts[[series_col]]
    if (!is.null(series_data) && nrow(series_data) > 0) {
      full_df <- full_df %>%
        dplyr::left_join(series_data, by = c("date", "bond"))
    }
  }

  # Join coupon (by bond only)
  if (!is.null(cpn_df)) {
    full_df <- full_df %>%
      dplyr::left_join(cpn_df, by = "bond")
  }

  # Join maturity info (by bond only)
  full_df <- full_df %>%
    dplyr::left_join(
      maturity_lookup %>% dplyr::select(bond, maturity_date, is_matured, days_to_maturity),
      by = "bond"
    )

  # Join auction metrics (by date AND bond)
  if (!is.null(auction_raw)) {
    auction_metrics <- auction_raw %>%
      dplyr::select(bond, date = offer_date, offer, bids, bid_to_cover, allocation) %>%
      dplyr::filter(!is.na(date))
    full_df <- full_df %>%
      dplyr::left_join(auction_metrics, by = c("date", "bond"))
  }

  # ---- Step 7: Add calculated fields & filter matured bonds ----
  message("\n[Hybrid] Adding calculated fields...")

  full_df <- full_df %>%
    dplyr::mutate(
      year    = lubridate::year(date),
      quarter = lubridate::quarter(date),
      month   = lubridate::month(date),
      week    = lubridate::week(date),

      dv01 = dplyr::case_when(
        !is.na(basis_point_value)                      ~ basis_point_value,
        !is.na(modified_duration) & !is.na(full_price) ~ modified_duration * full_price / 10000,
        !is.na(modified_duration)                      ~ modified_duration * 100 / 10000,
        TRUE ~ NA_real_
      ),

      maturity_bucket = dplyr::case_when(
        days_to_maturity <= 3 * 365  ~ "Short (0-3Y)",
        days_to_maturity <= 7 * 365  ~ "Medium (3-7Y)",
        days_to_maturity <= 12 * 365 ~ "Long (7-12Y)",
        days_to_maturity > 12 * 365  ~ "Ultra-Long (12Y+)",
        TRUE ~ "Unknown"
      ),

      auction_success = dplyr::case_when(
        is.na(bid_to_cover)   ~ NA_character_,
        bid_to_cover >= 3     ~ "Strong",
        bid_to_cover >= 2     ~ "Adequate",
        TRUE                  ~ "Weak"
      )
    ) %>%
    dplyr::filter(!is_matured | is.na(is_matured)) %>%
    dplyr::filter(!is.na(yield_to_maturity) & !is.na(modified_duration)) %>%
    dplyr::arrange(date, bond)

  message(sprintf("[Hybrid]   Final rows: %s", format(nrow(full_df), big.mark = ",")))
  message(sprintf("[Hybrid]   Unique active bonds: %d", dplyr::n_distinct(full_df$bond)))
  message(sprintf("[Hybrid]   Date range: %s to %s", min(full_df$date), max(full_df$date)))

  # ---- Step 8: Build summary tables ----
  bond_metadata <- full_df %>%
    dplyr::group_by(bond) %>%
    dplyr::summarise(
      coupon             = dplyr::first(stats::na.omit(coupon)),
      maturity_date      = dplyr::first(maturity_date),
      avg_duration       = mean(modified_duration, na.rm = TRUE),
      avg_convexity      = mean(convexity, na.rm = TRUE),
      latest_ytm         = dplyr::last(stats::na.omit(yield_to_maturity)),
      first_date         = min(date),
      last_date          = max(date),
      total_observations = dplyr::n(),
      total_auctions     = sum(!is.na(bid_to_cover)),
      avg_bid_cover      = mean(bid_to_cover, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(avg_duration)

  auction_summary <- if (!is.null(auction_raw)) {
    auction_raw %>%
      dplyr::filter(bond %in% unique(full_df$bond)) %>%
      dplyr::filter(!is.na(bid_to_cover)) %>%
      dplyr::group_by(bond) %>%
      dplyr::summarise(
        total_auctions = dplyr::n(),
        total_offered  = sum(offer, na.rm = TRUE) / 1e9,
        avg_bid_cover  = mean(bid_to_cover, na.rm = TRUE),
        min_bid_cover  = min(bid_to_cover, na.rm = TRUE),
        max_bid_cover  = max(bid_to_cover, na.rm = TRUE),
        last_auction   = max(offer_date),
        .groups = "drop"
      )
  } else {
    tibble::tibble()
  }

  # ---- Quality report ----
  message("\n=== HYBRID LOAD QUALITY REPORT ===")
  message("Date Range: ", min(full_df$date), " to ", max(full_df$date))
  message("Total Observations: ", format(nrow(full_df), big.mark = ","))
  message("Unique Active Bonds: ", dplyr::n_distinct(full_df$bond))
  message("Active Bonds: ", paste(sort(unique(full_df$bond)), collapse = ", "))
  if (archive_has_data) {
    message("Data source: Archive (.rds) + Excel (hybrid)")
  } else {
    message("Data source: Excel only (no archive yet)")
  }
  message("=== HYBRID DATA LOADING COMPLETE ===\n")

  return(list(
    full_df         = tibble::as_tibble(full_df),
    bond_metadata   = bond_metadata,
    auction_summary = auction_summary,
    maturity_lookup = maturity_lookup,
    auction_raw     = auction_raw
  ))
}


# ==============================================================================
# HELPER: Load a time series sheet from Excel (for hybrid use)
# ==============================================================================

#' Load a single time series sheet from Excel, pivot to long format
#'
#' Similar to the existing load_ts_sheet_robust() but returns a clean long-format
#' tibble without filtering for a fixed bond universe (bonds are discovered
#' dynamically from the sheet itself).
#'
#' @param excel_path Path to the Excel file
#' @param sheet_name Sheet name to read
#' @param value_col  Name for the value column
#' @return Tibble with columns: date, bond, <value_col>
load_ts_sheet_for_hybrid <- function(excel_path, sheet_name, value_col) {
  message(sprintf("[Hybrid]   Loading Excel sheet: %s -> %s", sheet_name, value_col))

  df <- readxl::read_excel(
    excel_path, sheet = sheet_name,
    na = c("", "NA", "#N/A", "N/A", "#VALUE!", "#REF!", "#DIV/0!"),
    guess_max = 21474836
  )

  if (!"date" %in% names(df)) {
    stop(sprintf("Sheet '%s' missing 'date' column", sheet_name))
  }

  bond_cols <- setdiff(names(df), "date")
  if (length(bond_cols) == 0) {
    stop(sprintf("Sheet '%s' has no bond columns", sheet_name))
  }

  df_long <- df %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(bond_cols),
      names_to  = "bond",
      values_to = value_col
    ) %>%
    dplyr::mutate(!!value_col := as.numeric(!!rlang::sym(value_col))) %>%
    dplyr::filter(!is.na(!!rlang::sym(value_col)))

  message(sprintf("[Hybrid]     %s rows, %d bonds", format(nrow(df_long), big.mark = ","), length(bond_cols)))
  return(tibble::as_tibble(df_long))
}


# ==============================================================================
# HELPER: Combine archive + Excel data for a single series
# ==============================================================================

#' Combine archive and Excel data for one series, deduplicating by date+bond
#'
#' On overlap, Excel data (the more recent source) takes precedence.
#'
#' @param archive_df  Tibble from bond_archive.rds (may be NULL or empty)
#' @param excel_df    Tibble from current Excel sheet (may be NULL or empty)
#' @param value_col   Name of the value column
#' @return Combined, deduplicated tibble
combine_series <- function(archive_df, excel_df, value_col) {

  has_archive <- !is.null(archive_df) && nrow(archive_df) > 0
  has_excel   <- !is.null(excel_df)   && nrow(excel_df)   > 0

  if (!has_archive && !has_excel) return(NULL)
  if (!has_archive) return(excel_df)
  if (!has_excel)   return(archive_df)

  # Tag source for dedup preference (Excel wins on overlap)
  archive_df$.source <- "archive"
  excel_df$.source   <- "excel"

  combined <- dplyr::bind_rows(archive_df, excel_df) %>%
    dplyr::arrange(date, bond, dplyr::desc(.source == "excel")) %>%
    dplyr::group_by(date, bond) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-".source")

  return(combined)
}
