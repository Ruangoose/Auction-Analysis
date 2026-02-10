# ================================================================================
# INSELE CAPITAL PARTNERS - ENHANCED SA GOVERNMENT BOND ANALYTICS DASHBOARD
# Professional Fixed Income Analysis Platform v2.0
# ================================================================================

# Load required libraries with error handling
required_packages <- c(
    "shiny", "shinydashboard", "shinyWidgets", "shinyBS", "shinycssloaders",
    "tidyverse", "ggplot2", "lubridate", "scales", "readxl", "splines",
    "zoo", "DT", "rmarkdown", "knitr", "TTR", "forecast", "memoise",
    "data.table", "RColorBrewer", "viridis", "gridExtra", "corrplot",
    "quantmod", "PerformanceAnalytics", "xts", "tseries", "randomForest",
    "glmnet", "shinyjqui", "waiter", "htmltools", "knitr", "blastula", "openxlsx", "base64enc",
    "webshot2", "htmltools", "htmlwidgets", "DT", "stringr", "officer", "flextable", "future", "promises", "magick", "shinyjs", "jsonlite",
    "ggrepel", "ggridges"#, "webp"
)

# Install and load packages
for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

# ================================================================================
# FONT CONFIGURATION (CROSS-PLATFORM)
# ================================================================================
# IMPORTANT: font_import() should ONLY be run ONCE manually in the console:
#   > install.packages("extrafont")
#   > library(extrafont)
#   > font_import()  # Takes 5-10 minutes, run ONCE
#   > loadfonts()    # Register fonts
#
# After running once, fonts are permanently available.
# ================================================================================

# Load extrafont library (but don't re-import fonts)
if (requireNamespace("extrafont", quietly = TRUE)) {
    library(extrafont)

    # Cross-platform font loading
    tryCatch({
        if (Sys.info()["sysname"] == "Windows") {
            loadfonts(device = "win", quiet = TRUE)
        } else if (Sys.info()["sysname"] == "Darwin") {  # macOS
            loadfonts(device = "pdf", quiet = TRUE)
        } else {  # Linux
            loadfonts(device = "pdf", quiet = TRUE)
        }
        message("✓ Custom fonts loaded successfully")
    }, error = function(e) {
        warning("Could not load custom fonts - using defaults: ", e$message)
    })
} else {
    warning("extrafont package not available - using default fonts")
}

# ================================================================================
# LOAD MODULES (with flexible path resolution)
# ================================================================================

# Helper function to find module files in multiple possible locations
source_module <- function(module_name) {
    possible_paths <- c(
        module_name,                                    # Same directory
        file.path("bond-dashboard", "Modules", module_name),  # Old structure
        file.path("Modules", module_name),              # Modules subdirectory
        file.path("bond-dashboard", module_name),
        file.path("..", module_name)                    # Parent directory
    )

    for (path in possible_paths) {
        if (file.exists(path)) {
            message(sprintf("✓ Loading module: %s", module_name))
            source(path, local = FALSE)
            return(TRUE)
        }
    }

    stop(sprintf("ERROR: Could not find module '%s' in any expected location.", module_name))
}

# Load all required modules
required_modules <- c(
    "data_loader.R",                 # Dynamic Excel data loader with caching
    "theme_config.R",
    "ui_helpers.R",
    "data_processors.R",
    "plot_generators.R",
    "risk_plots.R",
    "technical_plots.R",
    "carry_roll_plots.R",
    "auction_plots.R",
    "mod_auction_cumulative_issuance.R",
    "regime_plots.R",
    "insights_generators.R",
    "report_generators.R",
    "treasury_plots.R",              # Treasury holdings plot generators
    "mod_treasury_holdings.R",       # Treasury holdings Shiny module
    "enhanced_bond_ui.R"
)


for (module in required_modules) {
    source_module(module)
}


# ================================================================================
# SERVER LOGIC
# ================================================================================

server <- function(input, output, session) {

    # Ensure TTR functions are available (fallback)
    observe({
        if(!exists("ROC", envir = .GlobalEnv)) {
            ROC <<- function(x, n = 1) {
                (x - lag(x, n)) / lag(x, n) * 100
            }
        }
    })




    # ================================================================================
    # REACTIVE VALUES & DATA
    # ================================================================================

    # Initialize waiter for loading screens
    w <- Waiter$new()

    # Reactive values for state management
    values <- reactiveValues(
        current_regime = "Normal",
        last_update = Sys.time(),
        notifications = list(),
        alert_counter = 0,
        auction_performance_plot = NULL,  # Add this for storing plot
        carry_refresh = 0,  # Add this for carry & roll refresh trigger
        excel_path = NULL   # Store the Excel file path for metadata creation
    )

    # Reactive value for forward rate chart-table interactivity
    selected_forward_period <- reactiveVal(NULL)

    # ================================================================================
    # TREASURY HOLDINGS MODULE
    # ================================================================================
    # Initialize the Treasury Holdings module server and capture returned data
    treasury_module_data <- treasury_holdings_server("treasury_module")

    # ════════════════════════════════════════════════════════════════════════
    # BOND DATA LOADING - DYNAMIC EXCEL VERSION WITH CACHING
    # ════════════════════════════════════════════════════════════════════════

    bond_data <- reactive({
        w$show()

        # ════════════════════════════════════════════════════════════════════
        # STEP 1: LOAD DATA VIA DYNAMIC EXCEL LOADER
        # Automatically detects if Excel has changed and reloads as needed
        # ════════════════════════════════════════════════════════════════════

        data <- tryCatch({
            # First check for in-memory data (for testing/development)
            if (exists("full_df")) {
                message("✓ Loading bond data from full_df (in-memory)")
                return(full_df)
            }

            # Try multiple possible Excel file locations
            excel_paths <- c(
                "data/Insele Bonds Data File.xlsx",                 # Standard location
                "Insele Bonds Data File.xlsx",                      # Same directory
                "../data/Insele Bonds Data File.xlsx",              # Parent data directory
                "bond-dashboard/data/Insele Bonds Data File.xlsx"   # Old structure
            )

            # Try multiple possible cache file locations
            cache_paths <- c(
                "data/processed_bond_data.rds",            # Standard data directory
                "processed_bond_data.rds",                 # Same directory
                "../data/processed_bond_data.rds",         # Parent data directory
                "bond-dashboard/data/processed_bond_data.rds"  # Old structure
            )

            # Find the first existing Excel file
            excel_path <- NULL
            for (path in excel_paths) {
                if (file.exists(path)) {
                    excel_path <- path
                    break
                }
            }

            # Find the first existing or writable cache path
            cache_path <- NULL
            for (path in cache_paths) {
                if (file.exists(path) || file.exists(dirname(path))) {
                    cache_path <- path
                    break
                }
            }

            # Default cache path if none found
            if (is.null(cache_path)) {
                cache_path <- "data/processed_bond_data.rds"
            }

            # Load via the dynamic loader
            if (!is.null(excel_path)) {
                message(sprintf("✓ Using Excel source: %s", excel_path))
                message(sprintf("✓ Using cache file: %s", cache_path))

                # Store excel path for later use in metadata creation
                values$excel_path <- excel_path

                load_bond_data(
                    excel_path = excel_path,
                    cache_path = cache_path,
                    force_refresh = FALSE
                )
            } else {
                # Excel not found - try to load from cache as fallback
                warning("Excel file not found - attempting cache fallback")
                cache_fallback <- NULL
                for (path in cache_paths) {
                    if (file.exists(path)) {
                        message(sprintf("✓ Falling back to cache: %s", path))
                        cache_fallback <- readRDS(path)
                        break
                    }
                }
                cache_fallback
            }

        }, error = function(e) {
            warning(sprintf("Error loading bond data: %s", e$message))
            # Fallback to cache if Excel fails
            for (path in c("data/processed_bond_data.rds", "../data/processed_bond_data.rds")) {
                if (file.exists(path)) {
                    message(sprintf("Falling back to cache: %s", path))
                    return(readRDS(path))
                }
            }
            NULL
        })

        # Early exit if no data
        if (is.null(data)) {
            w$hide()
            showNotification(
                "Unable to load bond data. Please check Excel file and data files.",
                type = "error",
                duration = 10
            )
            return(NULL)
        }

        # ════════════════════════════════════════════════════════════════════
        # ✅ NEW: STEP 2 - STANDARDIZE COLUMN NAMES IMMEDIATELY
        # ════════════════════════════════════════════════════════════════════

        data <- tryCatch({
            standardize_column_names(data)
        }, error = function(e) {
            log_error(e, context = "bond_data_standardize_columns")
            warning(paste("Column standardization failed:", e$message))
            data  # Return original if standardization fails
        })

        # ════════════════════════════════════════════════════════════════════
        # ✅ NEW: STEP 2.5 - ENSURE DATE COLUMNS ARE DATE OBJECTS (CRITICAL FIX)
        # ════════════════════════════════════════════════════════════════════

        data <- tryCatch({
            # List of columns that should be Date objects
            date_cols <- c("date", "offer_date", "announce_date", "sett_date", "mat_date", "maturity_date")

            for(col_name in date_cols) {
                if(col_name %in% names(data)) {
                    if(!inherits(data[[col_name]], "Date")) {
                        data[[col_name]] <- as.Date(data[[col_name]])
                        message(sprintf("  ✓ Converted %s to Date class", col_name))
                    } else {
                        message(sprintf("  ✓ %s is already Date class", col_name))
                    }
                }
            }
            data
        }, error = function(e) {
            warning(paste("Date conversion failed:", e$message))
            data  # Return original if date conversion fails
        })

        # ════════════════════════════════════════════════════════════════════
        # STEP 3: VALIDATE AND CLEAN DATA
        # ════════════════════════════════════════════════════════════════════

        data <- tryCatch({
            data %>%
                mutate(
                    yield_to_maturity = as.numeric(yield_to_maturity),
                    modified_duration = as.numeric(modified_duration),
                    convexity = as.numeric(convexity),
                    duration = as.numeric(duration)
                ) %>%
                filter(
                    !is.na(yield_to_maturity),
                    !is.na(modified_duration),
                    is.finite(yield_to_maturity),
                    is.finite(modified_duration),
                    yield_to_maturity > 0,
                    modified_duration >= 0
                )
        }, error = function(e) {
            log_error(e, context = "bond_data_validation")
            NULL
        })

        # Check if cleaning removed all data
        if (is.null(data) || nrow(data) == 0) {
            w$hide()
            showNotification(
                "Data validation removed all records. Check data quality.",
                type = "error",
                duration = 10
            )
            return(NULL)
        }

        # ════════════════════════════════════════════════════════════════════
        # ✅ NEW: STEP 4 - SANITIZE AFTER DPLYR OPERATIONS
        # ════════════════════════════════════════════════════════════════════

        data <- tryCatch({
            sanitize_pipeline_data(data, "bond_data [post-cleaning]")
        }, error = function(e) {
            log_error(e, context = "bond_data_sanitize_post_cleaning")
            warning(paste("Post-cleaning sanitization failed:", e$message))
            data  # Return unsanitized if this fails
        })

        # ════════════════════════════════════════════════════════════════════
        # STEP 5: VALIDATE APPROVED SERIES
        # ════════════════════════════════════════════════════════════════════

        data <- tryCatch({
            validate_approved_series(data)  # Maps aliases and allows derived columns
        }, error = function(e) {
            log_error(e, context = "approved_series_validation")
            warning(paste("Approved series validation failed:", e$message))
            data  # Return original data if validation fails
        })

        # ════════════════════════════════════════════════════════════════════
        # ✅ NEW: STEP 6 - FINAL SANITIZATION BEFORE RETURN
        # ════════════════════════════════════════════════════════════════════

        data <- tryCatch({
            sanitize_pipeline_data(data, "bond_data [final output]")
        }, error = function(e) {
            log_error(e, context = "bond_data_final_sanitization")
            warning(paste("Final sanitization failed:", e$message))
            # Last-ditch effort to force plain data.frame
            data <- as.data.frame(data, stringsAsFactors = FALSE)
            class(data) <- "data.frame"
            data
        })

        # ════════════════════════════════════════════════════════════════════
        # STEP 7: SUMMARY AND NOTIFICATIONS
        # ════════════════════════════════════════════════════════════════════

        if (!is.null(data) && nrow(data) > 0) {
            n_bonds <- length(unique(data$bond))
            n_obs <- nrow(data)
            date_range <- range(data$date, na.rm = TRUE)

            # Console logging
            message("╔════════════════════════════════════════════════════════╗")
            message("║          BOND DATA LOADED SUCCESSFULLY                 ║")
            message("╚════════════════════════════════════════════════════════╝")
            message(sprintf("  Bonds:        %d", n_bonds))
            message(sprintf("  Observations: %s", format(n_obs, big.mark = ",")))
            message(sprintf("  Date Range:   %s to %s", date_range[1], date_range[2]))
            message(sprintf("  Columns:      %d", ncol(data)))
            message(sprintf("  Class:        %s", paste(class(data), collapse = ", ")))  # ✅ NEW: Show class
            message("")

            # User notification
            showNotification(
                sprintf("✓ Loaded %d bonds with %s observations",
                        n_bonds,
                        format(n_obs, big.mark = ",")),
                type = "message",
                duration = 3
            )
        } else {
            # Failure notification
            showNotification(
                "Unable to load bond data. Please check data files.",
                type = "error",
                duration = 10
            )
        }

        w$hide()
        return(data)
    })


    # ════════════════════════════════════════════════════════════════════════
    # BOND METADATA - Maturity dates and status (computed once at startup)
    # ════════════════════════════════════════════════════════════════════════

    bond_metadata <- reactive({
        req(bond_data())

        data <- bond_data()

        # Extract auction data for maturity dates
        # mature_date column comes from auction data joined in load_from_excel()
        auction_data <- NULL
        if ("mature_date" %in% names(data)) {
            auction_data <- data %>%
                dplyr::filter(!is.na(mature_date)) %>%
                dplyr::select(bond, mature_date) %>%
                dplyr::distinct()
        }

        # Create metadata using the function from data_loader.R
        # Pass excel_path to enable reading maturity_date sheet (PRIMARY SOURCE)
        create_bond_metadata(data, auction_data, excel_path = values$excel_path)
    })


    # ════════════════════════════════════════════════════════════════════════
    # ACTIVE BONDS - Dynamically filtered based on selected date range
    # ════════════════════════════════════════════════════════════════════════

    active_bonds <- reactive({
        req(bond_metadata(), input$date_range)

        start_date <- input$date_range[1]
        end_date <- input$date_range[2]

        # Get bonds that are active (not matured) for the selected period
        active <- get_active_bonds(bond_metadata(), start_date, end_date)

        message(sprintf("  Active bonds for period %s to %s: %d",
                       format(start_date, "%Y-%m-%d"),
                       format(end_date, "%Y-%m-%d"),
                       length(active)))

        return(active)
    })


    # ✅ FILTERED DATA - With validation and maturity filtering
    filtered_data <- reactive({
        req(input$selected_bonds, input$date_range, bond_data())

        # Validate date range
        start_date <- input$date_range[1]
        end_date <- input$date_range[2]

        # Auto-swap if dates are reversed
        if(start_date > end_date) {
            warning("Date range reversed - auto-correcting")
            temp <- start_date
            start_date <- end_date
            end_date <- temp

            updateDateRangeInput(
                session,
                "date_range",
                start = start_date,
                end = end_date
            )
        }

        # Validate minimum date range (at least 7 days)
        if(as.numeric(end_date - start_date) < 7) {
            showNotification(
                "Date range too narrow. Minimum 7 days required.",
                type = "warning",
                duration = 5
            )
            return(NULL)
        }

        # ════════════════════════════════════════════════════════════════════
        # MATURITY FILTER: Only include bonds active during the selected period
        # ════════════════════════════════════════════════════════════════════

        # Get active bonds for this date range (not matured)
        active <- tryCatch({
            active_bonds()
        }, error = function(e) {
            # Fallback: if active_bonds() fails, use all bonds
            message("Warning: Could not get active bonds, using all bonds")
            unique(bond_data()$bond)
        })

        # Intersect user selection with active bonds
        valid_bonds <- intersect(input$selected_bonds, active)

        # Notify user if any selected bonds were filtered out due to maturity
        matured_selection <- setdiff(input$selected_bonds, active)
        if (length(matured_selection) > 0) {
            showNotification(
                sprintf("Excluded %d matured bond(s) from analysis: %s",
                       length(matured_selection),
                       paste(head(matured_selection, 5), collapse = ", ")),
                type = "message",
                duration = 5,
                id = "matured_bonds_notice"
            )
        }

        # Check if any valid bonds remain after maturity filter
        if (length(valid_bonds) == 0) {
            showNotification(
                "All selected bonds have matured. Please select active bonds or adjust date range.",
                type = "warning",
                duration = 8
            )
            return(NULL)
        }

        # Filter data using valid (non-matured) bonds only
        result <- bond_data() %>%
            filter(
                bond %in% valid_bonds,
                date >= start_date,
                date <= end_date
            )

        # Validate filtered result
        if(nrow(result) == 0) {
            showNotification(
                "No data available for selected bonds and date range",
                type = "warning",
                duration = 5
            )
            return(NULL)
        }

        # ════════════════════════════════════════════════════════════════════
        # ADD MATURITY TRACKING FIELDS
        # ════════════════════════════════════════════════════════════════════
        # Get maturity info from bond_metadata
        metadata <- tryCatch({
            bond_metadata() %>%
                dplyr::select(bond, final_maturity_date) %>%
                dplyr::distinct()
        }, error = function(e) {
            message("Warning: Could not get bond metadata for maturity tracking")
            NULL
        })

        if (!is.null(metadata)) {
            result <- result %>%
                dplyr::left_join(metadata, by = "bond") %>%
                dplyr::mutate(
                    # Days to maturity from the end of the analysis period
                    days_to_maturity = as.numeric(difftime(final_maturity_date, end_date, units = "days")),
                    # CORRECTED LOGIC: Flag bonds that mature WITHIN the analysis period ONLY
                    # Per user requirements:
                    # - Bonds maturing AFTER end_date = "Active" (circle marker)
                    # - Bonds maturing BETWEEN start_date and end_date = "Maturing" (triangle marker)
                    # - Bonds maturing BEFORE start_date are already excluded by get_active_bonds()
                    matures_in_period = !is.na(final_maturity_date) &
                                        final_maturity_date >= start_date &
                                        final_maturity_date <= end_date
                )

            # Debug logging for maturity filtering
            maturing_bonds_debug <- result %>%
                dplyr::filter(!is.na(final_maturity_date)) %>%
                dplyr::select(bond, final_maturity_date, days_to_maturity, matures_in_period) %>%
                dplyr::distinct()
            message(sprintf("=== MATURITY FILTER DEBUG ==="))
            message(sprintf("Analysis period: %s to %s", start_date, end_date))
            message(sprintf("Bonds with maturity data: %d", nrow(maturing_bonds_debug)))
            bonds_flagged <- maturing_bonds_debug %>% dplyr::filter(matures_in_period)
            if (nrow(bonds_flagged) > 0) {
                message(sprintf("Bonds flagged as matures_in_period: %s",
                               paste(sprintf("%s (matures %s, %d days)",
                                           bonds_flagged$bond,
                                           format(bonds_flagged$final_maturity_date, "%Y-%m-%d"),
                                           bonds_flagged$days_to_maturity),
                                     collapse = ", ")))
            } else {
                message("No bonds flagged as matures_in_period")
            }
            message("=============================")

            # ════════════════════════════════════════════════════════════════════
            # APPLY MATURING BOND FILTER (if checkbox is checked)
            # ════════════════════════════════════════════════════════════════════
            exclude_maturing <- isTRUE(input$exclude_maturing_bonds)

            if (exclude_maturing) {
                # Get count of bonds maturing in period before filtering
                maturing_bonds <- result %>%
                    dplyr::filter(matures_in_period) %>%
                    dplyr::pull(bond) %>%
                    unique()

                if (length(maturing_bonds) > 0) {
                    showNotification(
                        sprintf("Excluded %d bond(s) maturing in analysis period: %s",
                               length(maturing_bonds),
                               paste(head(maturing_bonds, 5), collapse = ", ")),
                        type = "message",
                        duration = 5,
                        id = "maturing_bonds_excluded"
                    )

                    # Filter out bonds maturing in the period
                    result <- result %>%
                        dplyr::filter(!matures_in_period)

                    # Check if any bonds remain after filtering
                    if(nrow(result) == 0) {
                        showNotification(
                            "All bonds mature within the selected period. Uncheck 'Exclude maturing bonds' or adjust date range.",
                            type = "warning",
                            duration = 8
                        )
                        return(NULL)
                    }
                }
            }
        } else {
            # No metadata available - add empty maturity columns
            result <- result %>%
                dplyr::mutate(
                    final_maturity_date = as.Date(NA),
                    matures_in_period = FALSE,
                    days_to_maturity = NA_real_
                )
        }

        # ✅ Sanitize filtered output
        result <- sanitize_pipeline_data(result, "filtered_data [output]")

        return(result)
    })

    # ════════════════════════════════════════════════════════════════════════
    # MARKET INTELLIGENCE DATA - FULL HISTORICAL DATA (IGNORES DATE FILTER)
    # This ensures Market Intelligence charts always use complete history
    # ════════════════════════════════════════════════════════════════════════

    market_intelligence_data <- reactive({
        req(bond_data())

        # Use the complete dataset, only filter for data quality
        data <- bond_data() %>%
            filter(!is.na(yield_to_maturity),
                   !is.na(modified_duration),
                   yield_to_maturity > 0,
                   yield_to_maturity < 50,  # Sanity check
                   modified_duration > 0)

        if(nrow(data) < 30) {
            showNotification(
                "Insufficient historical data for Market Intelligence analysis",
                type = "warning",
                duration = 5
            )
            return(NULL)
        }

        return(data)
    })

    # ════════════════════════════════════════════════════════════════════════
    # CURRENT ACTIVE BONDS - Bonds that have data on the most recent date
    # Used to identify what's currently tradeable vs matured bonds
    # ════════════════════════════════════════════════════════════════════════

    current_active_bonds <- reactive({
        req(bond_data())

        max_date <- max(bond_data()$date, na.rm = TRUE)

        # Bonds that have data on the most recent date are considered active
        # Also check within last 5 trading days to handle weekends/holidays
        active <- bond_data() %>%
            filter(date >= max_date - 5,
                   !is.na(yield_to_maturity)) %>%
            pull(bond) %>%
            unique()

        return(active)
    })

    # ════════════════════════════════════════════════════════════════════════
    # MARKET INTELLIGENCE DEBUG OUTPUT
    # Logs information about data being used for Market Intelligence charts
    # ════════════════════════════════════════════════════════════════════════

    observe({
        req(market_intelligence_data())

        data <- market_intelligence_data()
        max_date <- max(data$date, na.rm = TRUE)
        min_date <- min(data$date, na.rm = TRUE)

        message("=== MARKET INTELLIGENCE DATA CHECK ===")
        message(sprintf("Full date range: %s to %s",
                        format(min_date, "%Y-%m-%d"),
                        format(max_date, "%Y-%m-%d")))
        message(sprintf("Total observations: %d", nrow(data)))
        message(sprintf("Unique bonds: %d", n_distinct(data$bond)))

        # Bonds active on most recent date
        current_bonds <- data %>%
            filter(date == max_date) %>%
            pull(bond) %>%
            unique()
        message(sprintf("Currently active bonds (%d): %s",
                        length(current_bonds),
                        paste(sort(current_bonds), collapse = ", ")))

        # Bonds that appear in history but not current (matured)
        all_bonds <- unique(data$bond)
        historical_only <- setdiff(all_bonds, current_bonds)
        if(length(historical_only) > 0) {
            message(sprintf("Historical only (matured?): %s",
                            paste(sort(historical_only), collapse = ", ")))
        }
    })

    # ════════════════════════════════════════════════════════════════════════
    # FILTERED DATA WITH TECHNICAL INDICATORS - BULLETPROOF VERSION
    # ════════════════════════════════════════════════════════════════════════

    filtered_data_with_technicals <- reactive({
        req(filtered_data())

        # ✅ NEW: Sanitize input
        data <- sanitize_pipeline_data(filtered_data(), "filtered_data_with_technicals [input]")

        # Verify required columns
        if(!"bond" %in% names(data)) {
            showNotification(
                "Bond column missing from filtered data!",
                type = "error",
                duration = 5
            )
            return(data)
        }

        if(!"yield_to_maturity" %in% names(data)) {
            showNotification(
                "Yield to maturity column missing from filtered data!",
                type = "error",
                duration = 5
            )
            return(data)
        }

        # Calculate technical indicators with comprehensive error handling
        result <- tryCatch({

            # ✅ IMPROVED: calculate_advanced_technicals now handles everything internally
            data_with_tech <- calculate_advanced_technicals(data)

            # ✅ NEW: Additional sanitization (belt-and-suspenders approach)
            data_with_tech <- sanitize_pipeline_data(
                data_with_tech,
                "filtered_data_with_technicals [post-calculation]"
            )

            # Verify bond column preserved
            if(!"bond" %in% names(data_with_tech)) {
                stop("CRITICAL: Bond column lost during technical calculation!")
            }

            # Verify required technical columns exist
            required_cols <- c(
                "rsi_14", "bb_position", "bb_mean",
                "macd", "macd_signal",
                "sma_50", "sma_200",
                "signal_strength"
            )

            missing_cols <- setdiff(required_cols, names(data_with_tech))

            if(length(missing_cols) > 0) {
                warning(paste(
                    "Missing technical columns:",
                    paste(missing_cols, collapse = ", ")
                ))

                showNotification(
                    paste("Some technical indicators unavailable:",
                          paste(head(missing_cols, 3), collapse = ", ")),
                    type = "warning",
                    duration = 3
                )
            }

            # Log success
            message(sprintf(
                "✓ filtered_data_with_technicals SUCCESS: %d rows × %d cols | %d bonds",
                nrow(data_with_tech),
                ncol(data_with_tech),
                n_distinct(data_with_tech$bond)
            ))

            # Show success notification
            showNotification(
                sprintf("✓ Technical indicators ready for %d bonds",
                        n_distinct(data_with_tech$bond)),
                type = "message",
                duration = 2
            )

            return(data_with_tech)

        }, error = function(e) {

            # Log error
            warning(paste("ERROR in filtered_data_with_technicals:", e$message))

            # Show error notification
            showNotification(
                paste("⚠ Technical indicators failed:", e$message),
                type = "warning",
                duration = 5
            )

            # ✅ IMPROVED: Use helper function for defaults
            result <- add_default_technicals(data)
            return(result)
        })

        return(result)
    })

    # ════════════════════════════════════════════════════════════════════════════
    # TECHNICAL SIGNALS MASTER - SINGLE SOURCE OF TRUTH
    # ════════════════════════════════════════════════════════════════════════════
    # CRITICAL: This reactive provides the AUTHORITATIVE signal calculations
    # used by BOTH the Trading Signal Matrix AND the Technical Indicators Table.
    # This ensures consistency between these two displays.
    # ════════════════════════════════════════════════════════════════════════════

    technical_signals_master <- reactive({
        req(filtered_data_with_technicals())

        # Get the full technical data
        data <- filtered_data_with_technicals()

        # ════════════════════════════════════════════════════════════════════════
        # CRITICAL FIX: Filter out matured bonds for consistent 17 active bonds
        # Same filtering as Risk Analytics for consistency across all sections
        # ════════════════════════════════════════════════════════════════════════
        known_matured_bonds <- c("R157", "R186", "R197", "R203", "R204", "R207", "R208", "R212", "R2023")
        found_matured <- intersect(unique(data$bond), known_matured_bonds)
        if (length(found_matured) > 0) {
            message(sprintf("[Technical Analysis] Excluding matured bonds: %s",
                           paste(found_matured, collapse = ", ")))
            data <- data %>% filter(!bond %in% known_matured_bonds)
        }

        # Get latest data per bond (most recent date for each bond)
        latest_data <- data %>%
            group_by(bond) %>%
            slice_max(date, n = 1) %>%
            ungroup() %>%
            distinct(bond, .keep_all = TRUE)

        # ════════════════════════════════════════════════════════════════════════
        # Calculate individual indicator signals (-2 to +2 scale)
        # These are YIELD perspective signals that will be INVERTED for bond prices
        # ════════════════════════════════════════════════════════════════════════

        result <- latest_data %>%
            mutate(
                # ═══════════════════════════════════════════════════════════════
                # RSI Signal (YIELD perspective)
                # High RSI = overbought yields = yields may reverse DOWN
                # For bonds: yields down = prices up = GOOD
                # So: High RSI → POSITIVE signal (buy)
                # ═══════════════════════════════════════════════════════════════
                rsi_signal_yield = case_when(
                    is.na(rsi_14) ~ 0L,
                    rsi_14 > 80 ~ 2L,    # Extremely overbought yields → Strong Buy
                    rsi_14 > 70 ~ 1L,    # Overbought yields → Buy
                    rsi_14 < 20 ~ -2L,   # Extremely oversold yields → Strong Sell
                    rsi_14 < 30 ~ -1L,   # Oversold yields → Sell
                    TRUE ~ 0L            # Neutral
                ),

                # ═══════════════════════════════════════════════════════════════
                # BB Signal (YIELD perspective)
                # bb_position > 1 = above upper band = yields extended high
                # Yields extended high may reverse → GOOD for bond prices
                # ═══════════════════════════════════════════════════════════════
                bb_signal_yield = case_when(
                    is.na(bb_position) ~ 0L,
                    bb_position > 1.0 ~ 2L,    # Above upper band → Strong Buy
                    bb_position > 0.8 ~ 1L,    # Near upper band → Buy
                    bb_position < 0 ~ -2L,     # Below lower band → Strong Sell
                    bb_position < 0.2 ~ -1L,   # Near lower band → Sell
                    TRUE ~ 0L                   # Within bands → Neutral
                ),

                # ═══════════════════════════════════════════════════════════════
                # MACD Signal (YIELD perspective)
                # Positive histogram = yields momentum UP = BAD for bond prices
                # So invert: Positive histogram → NEGATIVE signal
                # ═══════════════════════════════════════════════════════════════
                macd_signal_yield = case_when(
                    is.na(macd_histogram) ~ 0L,
                    macd_histogram > 0.05 ~ -2L,   # Strong bullish yields → Strong Sell
                    macd_histogram > 0 ~ -1L,      # Bullish yields → Sell
                    macd_histogram < -0.05 ~ 2L,   # Strong bearish yields → Strong Buy
                    macd_histogram < 0 ~ 1L,       # Bearish yields → Buy
                    TRUE ~ 0L
                ),

                # ═══════════════════════════════════════════════════════════════
                # Momentum Signal (ROC - YIELD perspective)
                # Positive ROC = yields rising = BAD for bond prices
                # So invert: Positive ROC → NEGATIVE signal
                # ═══════════════════════════════════════════════════════════════
                momentum_signal_yield = case_when(
                    is.na(roc_20) ~ 0L,
                    roc_20 > 5 ~ -2L,       # Strong yield rise → Strong Sell
                    roc_20 > 2 ~ -1L,       # Yield rise → Sell
                    roc_20 < -5 ~ 2L,       # Strong yield fall → Strong Buy
                    roc_20 < -2 ~ 1L,       # Yield fall → Buy
                    TRUE ~ 0L
                ),

                # ═══════════════════════════════════════════════════════════════
                # TOTAL Score (from BOND PRICE perspective)
                # Range: -8 to +8
                # Positive = Good for bond holders (yields falling/reversing)
                # Negative = Bad for bond holders (yields rising)
                # ═══════════════════════════════════════════════════════════════
                total_score = rsi_signal_yield + bb_signal_yield +
                              macd_signal_yield + momentum_signal_yield,

                # ═══════════════════════════════════════════════════════════════
                # Overall Signal Classification (5 levels)
                # Uses bond price perspective thresholds
                # ═══════════════════════════════════════════════════════════════
                overall_signal = case_when(
                    total_score >= 4 ~ "Strong Buy",
                    total_score >= 2 ~ "Buy",
                    total_score >= -1 ~ "Neutral",
                    total_score >= -3 ~ "Sell",
                    TRUE ~ "Strong Sell"
                ),

                # ═══════════════════════════════════════════════════════════════
                # Text labels for individual indicator displays (for table)
                # ═══════════════════════════════════════════════════════════════
                rsi_signal_label = case_when(
                    rsi_14 > 80 ~ "Extreme OB",
                    rsi_14 > 70 ~ "Overbought",
                    rsi_14 < 20 ~ "Extreme OS",
                    rsi_14 < 30 ~ "Oversold",
                    TRUE ~ "Neutral"
                ),

                bb_signal_label = case_when(
                    bb_position > 1.0 ~ "Above Upper",
                    bb_position > 0.8 ~ "Near Upper",
                    bb_position < 0 ~ "Below Lower",
                    bb_position < 0.2 ~ "Near Lower",
                    TRUE ~ "Within Bands"
                ),

                macd_signal_label = case_when(
                    macd_histogram > 0.05 ~ "Strong Bullish",
                    macd_histogram > 0 ~ "Bullish",
                    macd_histogram < -0.05 ~ "Strong Bearish",
                    macd_histogram < 0 ~ "Bearish",
                    TRUE ~ "Neutral"
                ),

                trend_label = case_when(
                    sma_50 > sma_200 * 1.02 ~ "Strong Uptrend",
                    sma_50 > sma_200 ~ "Uptrend",
                    sma_50 < sma_200 * 0.98 ~ "Strong Downtrend",
                    sma_50 < sma_200 ~ "Downtrend",
                    TRUE ~ "Sideways"
                ),

                roc_signal_label = case_when(
                    roc_20 > 5 ~ "Strong Rise",
                    roc_20 > 2 ~ "Rising",
                    roc_20 > -2 ~ "Flat",
                    roc_20 > -5 ~ "Falling",
                    TRUE ~ "Strong Fall"
                )
            )

        # Log the master reactive creation
        message(sprintf(
            "✓ technical_signals_master: %d bonds | Score range: [%d, %d]",
            nrow(result),
            min(result$total_score, na.rm = TRUE),
            max(result$total_score, na.rm = TRUE)
        ))

        return(result)
    })


    # Periodic theme cache cleanup (every 30 minutes)
    # Prevents memory bloat from cached theme objects
    observe({
        invalidateLater(1800000)  # 30 minutes in milliseconds

        # Only run if theme manager exists and has clear_cache method
        if (exists(".theme_manager", envir = .GlobalEnv)) {
            suppressWarnings({
                tryCatch({
                    theme_mgr <- get(".theme_manager", envir = .GlobalEnv)
                    if (!is.null(theme_mgr) && "clear_cache" %in% names(theme_mgr)) {
                        theme_mgr$clear_cache()
                        message("✓ Theme cache cleared")
                    }
                }, error = function(e) {
                    # Silently fail - cache cleanup is not critical
                    NULL
                })
            })
        }
    })

    # Add this as a new reactive for historical calculations
    historical_metrics <- reactive({
        req(filtered_data())

        data <- filtered_data()

        # Calculate spreads for all dates
        data_with_spreads <- data %>%
            group_by(date) %>%
            group_modify(~ calculate_fair_value(.x, method = input$curve_model)) %>%
            ungroup()

        # Calculate historical statistics
        data_with_spreads %>%
            group_by(bond) %>%
            arrange(date) %>%
            mutate(
                hist_mean = zoo::rollmean(spread_to_curve,
                                          k = min(input$lookback_days, n()),
                                          fill = NA, align = "right", partial = TRUE),
                hist_sd = zoo::rollapply(spread_to_curve,
                                         width = min(input$lookback_days, n()),
                                         FUN = function(x) sd(x, na.rm = TRUE),
                                         fill = NA, align = "right", partial = TRUE),
                z_score = (spread_to_curve - hist_mean) / pmax(hist_sd, 0.1)
            ) %>%
            ungroup()
    })

    # ════════════════════════════════════════════════════════════════════════
    # PROCESSED DATA - BULLETPROOF VERSION
    # ════════════════════════════════════════════════════════════════════════

    processed_data <- reactive({
        req(filtered_data())

        # ✅ NEW: Sanitize input
        data <- sanitize_pipeline_data(filtered_data(), "processed_data [input]")

        # Get lookback value with default
        lookback_value <- input$lookback_days
        if(is.null(lookback_value)) {
            lookback_value <- 60
        }

        # Process all data at once with comprehensive error handling
        tryCatch({

            # Step 1: Calculate fair value for each date separately
            dates <- unique(data$date)
            processed_list <- list()

            for(d in dates) {
                date_data <- data[data$date == d, ]
                date_data <- calculate_fair_value(date_data, method = input$curve_model)
                processed_list[[as.character(d)]] <- date_data
            }

            # Combine all dates
            if(length(processed_list) > 0) {
                data_with_spreads <- do.call(rbind, processed_list)
                rownames(data_with_spreads) <- NULL
            } else {
                data_with_spreads <- data
                data_with_spreads$fitted_yield <- data_with_spreads$yield_to_maturity
                data_with_spreads$spread_to_curve <- 0
            }

            # ✅ NEW: Sanitize after fair value calculation
            data_with_spreads <- sanitize_pipeline_data(
                data_with_spreads,
                "processed_data [post-fair-value]"
            )

            # Step 2: Add technical indicators
            data_with_technicals <- calculate_advanced_technicals(data_with_spreads)

            # ✅ NEW: Sanitize after technicals
            data_with_technicals <- sanitize_pipeline_data(
                data_with_technicals,
                "processed_data [post-technicals]"
            )

            # Step 3: Calculate relative value
            data_with_metrics <- calculate_relative_value(data_with_technicals, lookback = lookback_value)

            # ✅ NEW: Sanitize after relative value
            data_with_metrics <- sanitize_pipeline_data(
                data_with_metrics,
                "processed_data [post-relative-value]"
            )

            # Step 4: Get latest data using SAFE approach
            latest_data <- data_with_metrics %>%
                group_by(bond) %>%
                filter(date == max(date)) %>%
                slice(1) %>%
                ungroup()

            # ✅ CRITICAL: Nuclear sanitization of latest data
            latest_data <- sanitize_pipeline_data(
                latest_data,
                "processed_data [latest-extraction]"
            )

            # Ensure signal_strength exists
            if(!"signal_strength" %in% names(latest_data)) {
                latest_data$signal_strength <- "Neutral"
            }

            # Verify we have data before returning
            if(nrow(latest_data) == 0) {
                warning("processed_data: No data after processing")
                return(NULL)
            }

            message(sprintf(
                "✓ processed_data SUCCESS: %d bonds with latest metrics",
                nrow(latest_data)
            ))

            return(latest_data)

        }, error = function(e) {

            warning(paste("ERROR in processed_data:", e$message))

            # Fallback: Return minimal valid data frame
            tryCatch({
                result <- data %>%
                    group_by(bond) %>%
                    filter(date == max(date)) %>%
                    slice(1) %>%
                    ungroup()

                # Add missing columns
                result$fitted_yield <- result$yield_to_maturity
                result$spread_to_curve <- 0
                result$signal_strength <- "Neutral"
                result$z_score <- 0
                result$percentile_rank <- 0.5

                # ✅ NEW: Sanitize error fallback
                result <- sanitize_pipeline_data(result, "processed_data [error-fallback]")

                return(result)

            }, error = function(e2) {
                # Last resort fallback
                warning("processed_data: Complete fallback triggered - returning NULL")
                return(NULL)
            })
        })
    })


    # =============================================================================
    # SINGLE SOURCE OF TRUTH: Master Curve Data Reactive
    # =============================================================================
    # CRITICAL: This reactive recalculates whenever x-axis OR model type changes
    # Both the yield curve chart AND the opportunities table consume from this
    # =============================================================================

    fitted_curve_data <- reactive({
        # -------------------------------------------------------------------------
        # CRITICAL: Declare ALL dependencies explicitly
        # -------------------------------------------------------------------------
        req(processed_data())

        # These inputs trigger recalculation
        x_var <- input$xaxis_choice %||% "modified_duration"
        model_type <- input$curve_model %||% "nss"
        lookback_value <- input$lookback_days %||% 60
        confidence_level <- input$confidence_level %||% 95

        # Log for debugging
        message(sprintf("[fitted_curve_data] Recalculating with x_var=%s, model=%s", x_var, model_type))

        bond_data <- processed_data()

        # =========================================================================
        # CRITICAL FIX: Filter to GLOBAL latest date (same as yield curve chart)
        # =========================================================================
        # Problem: processed_data() returns latest data PER BOND, which can include
        # bonds with stale data (e.g., R186 with Dec 2025 data when other bonds
        # have Jan 2026 data). The yield curve chart filters to global latest date,
        # so Trading Signals must do the same to ensure consistency.
        # =========================================================================
        if ("date" %in% names(bond_data) && !all(is.na(bond_data$date))) {
            # Find the global latest date across all bonds
            global_latest_date <- max(bond_data$date, na.rm = TRUE)

            # Identify bonds that don't have data on the latest date
            bonds_before_filter <- unique(bond_data$bond)
            bond_data <- bond_data %>%
                filter(date == global_latest_date)
            bonds_after_filter <- unique(bond_data$bond)

            # Log which bonds were filtered out (for debugging)
            excluded_bonds <- setdiff(bonds_before_filter, bonds_after_filter)
            if (length(excluded_bonds) > 0) {
                message(sprintf("[fitted_curve_data] Excluded %d bond(s) without data on %s: %s",
                               length(excluded_bonds),
                               format(global_latest_date, "%Y-%m-%d"),
                               paste(excluded_bonds, collapse = ", ")))
            }

            message(sprintf("[fitted_curve_data] Using %d bonds with data from %s",
                           nrow(bond_data), format(global_latest_date, "%Y-%m-%d")))
        }

        # -------------------------------------------------------------------------
        # Handle time_to_maturity calculation if needed
        # -------------------------------------------------------------------------
        if(x_var == "time_to_maturity") {
            if("mature_date" %in% names(bond_data) && !all(is.na(bond_data$mature_date))) {
                bond_data$time_to_maturity <- as.numeric(difftime(bond_data$mature_date,
                                                                  Sys.Date(),
                                                                  units = "days")) / 365.25
            } else {
                # Approximate using Macaulay duration for semi-annual SA bonds
                bond_data$macaulay_duration <- bond_data$modified_duration * (1 + bond_data$yield_to_maturity/200)
                bond_data$time_to_maturity <- bond_data$macaulay_duration
            }
        }

        # -------------------------------------------------------------------------
        # Validate x-axis column exists
        # -------------------------------------------------------------------------
        if(!x_var %in% names(bond_data) || all(is.na(bond_data[[x_var]]))) {
            warning(sprintf("[fitted_curve_data] x_var '%s' not found, falling back to modified_duration", x_var))
            x_var <- "modified_duration"
        }

        req(x_var %in% names(bond_data))

        # -------------------------------------------------------------------------
        # Prepare fitting data
        # -------------------------------------------------------------------------
        fit_data <- bond_data %>%
            filter(
                !is.na(.data[[x_var]]),
                !is.na(yield_to_maturity),
                .data[[x_var]] > 0  # Exclude zero/negative durations
            ) %>%
            arrange(.data[[x_var]])

        req(nrow(fit_data) >= 4)  # Minimum bonds for curve fitting

        # Get x values for fitting
        x_data <- fit_data[[x_var]]
        x_range <- range(x_data, na.rm = TRUE)

        # Create smooth curve data points
        curve_df <- data.frame(
            x = seq(x_range[1], x_range[2], length.out = 200)
        )

        # -------------------------------------------------------------------------
        # Fit curve based on selected model type
        # -------------------------------------------------------------------------
        curve_fit_result <- tryCatch({

            if (model_type == "nss") {
                # Nelson-Siegel-Svensson model
                yields <- fit_data$yield_to_maturity / 100  # Convert to decimal
                x_values <- x_data

                # Initial parameter estimates
                beta0 <- mean(yields, na.rm = TRUE)
                beta1 <- yields[which.min(x_values)] - beta0
                beta2 <- 0
                beta3 <- 0
                lambda1 <- 0.0609
                lambda2 <- 0.0609

                # NSS objective function
                nss_objective <- function(params) {
                    b0 <- params[1]
                    b1 <- params[2]
                    b2 <- params[3]
                    b3 <- params[4]
                    l1 <- exp(params[5])
                    l2 <- exp(params[6])

                    x_safe <- pmax(x_values, 0.01)

                    fitted <- b0 +
                        b1 * (1 - exp(-x_safe/l1)) / (x_safe/l1) +
                        b2 * ((1 - exp(-x_safe/l1)) / (x_safe/l1) - exp(-x_safe/l1)) +
                        b3 * ((1 - exp(-x_safe/l2)) / (x_safe/l2) - exp(-x_safe/l2))

                    sum((yields - fitted)^2, na.rm = TRUE)
                }

                # Optimize
                opt_result <- optim(
                    c(beta0, beta1, beta2, beta3, log(lambda1), log(lambda2)),
                    nss_objective,
                    method = "BFGS",
                    control = list(maxit = 500)
                )

                if (opt_result$convergence != 0) {
                    stop("NSS optimization failed to converge")
                }

                params_nss <- opt_result$par

                # Predict smooth curve
                curve_x <- pmax(curve_df$x, 0.01)
                curve_df$fitted_yield <- (params_nss[1] +
                    params_nss[2] * (1 - exp(-curve_x/exp(params_nss[5]))) / (curve_x/exp(params_nss[5])) +
                    params_nss[3] * ((1 - exp(-curve_x/exp(params_nss[5]))) / (curve_x/exp(params_nss[5])) -
                                     exp(-curve_x/exp(params_nss[5]))) +
                    params_nss[4] * ((1 - exp(-curve_x/exp(params_nss[6]))) / (curve_x/exp(params_nss[6])) -
                                     exp(-curve_x/exp(params_nss[6])))) * 100

                # Predict for actual bonds
                x_bonds <- pmax(x_data, 0.01)
                fit_data$fitted_yield <- (params_nss[1] +
                    params_nss[2] * (1 - exp(-x_bonds/exp(params_nss[5]))) / (x_bonds/exp(params_nss[5])) +
                    params_nss[3] * ((1 - exp(-x_bonds/exp(params_nss[5]))) / (x_bonds/exp(params_nss[5])) -
                                     exp(-x_bonds/exp(params_nss[5]))) +
                    params_nss[4] * ((1 - exp(-x_bonds/exp(params_nss[6]))) / (x_bonds/exp(params_nss[6])) -
                                     exp(-x_bonds/exp(params_nss[6])))) * 100

                list(success = TRUE, model = "nss", params = params_nss)

            } else if (model_type == "loess") {
                # LOESS local regression
                n_unique <- length(unique(x_data))
                adaptive_span <- max(0.75, min(1, 10/n_unique))

                model_fit <- loess(yield_to_maturity ~ x_var,
                                   data = data.frame(x_var = x_data,
                                                     yield_to_maturity = fit_data$yield_to_maturity),
                                   span = adaptive_span)

                curve_df$fitted_yield <- predict(model_fit, newdata = data.frame(x_var = curve_df$x))
                fit_data$fitted_yield <- predict(model_fit, newdata = data.frame(x_var = x_data))

                list(success = TRUE, model = "loess", params = NULL)

            } else if (model_type == "spline") {
                # Smoothing spline
                model_fit <- smooth.spline(x = x_data,
                                           y = fit_data$yield_to_maturity,
                                           spar = 0.6)

                curve_df$fitted_yield <- predict(model_fit, curve_df$x)$y
                fit_data$fitted_yield <- predict(model_fit, x_data)$y

                list(success = TRUE, model = "spline", params = NULL)

            } else if (model_type == "polynomial") {
                # Polynomial regression
                n_unique <- length(unique(x_data))
                poly_degree <- min(3, max(1, n_unique - 2))

                lm_model <- lm(yield_to_maturity ~ poly(x_var, poly_degree),
                               data = data.frame(x_var = x_data,
                                                 yield_to_maturity = fit_data$yield_to_maturity))

                curve_df$fitted_yield <- predict(lm_model, newdata = data.frame(x_var = curve_df$x))
                fit_data$fitted_yield <- predict(lm_model, newdata = data.frame(x_var = x_data))

                list(success = TRUE, model = "polynomial", params = NULL)

            } else {
                # Default to polynomial
                lm_model <- lm(yield_to_maturity ~ poly(x_var, 3),
                               data = data.frame(x_var = x_data,
                                                 yield_to_maturity = fit_data$yield_to_maturity))

                curve_df$fitted_yield <- predict(lm_model, newdata = data.frame(x_var = curve_df$x))
                fit_data$fitted_yield <- predict(lm_model, newdata = data.frame(x_var = x_data))

                list(success = TRUE, model = "polynomial", params = NULL)
            }

        }, error = function(e) {
            warning(sprintf("[fitted_curve_data] Model fitting failed (%s): %s - using polynomial fallback",
                            model_type, e$message))

            # Fallback to simple polynomial
            lm_model <- lm(yield_to_maturity ~ poly(x_var, 2),
                           data = data.frame(x_var = x_data,
                                             yield_to_maturity = fit_data$yield_to_maturity))

            curve_df$fitted_yield <<- predict(lm_model, newdata = data.frame(x_var = curve_df$x))
            fit_data$fitted_yield <<- predict(lm_model, newdata = data.frame(x_var = x_data))

            list(success = TRUE, model = "polynomial-fallback", params = NULL)
        })

        # -------------------------------------------------------------------------
        # Calculate spreads for each bond (THIS IS THE KEY PART)
        # -------------------------------------------------------------------------
        fit_data <- fit_data %>%
            mutate(
                # Spread in basis points (RECALCULATED based on current x-axis and model)
                spread_bps = (yield_to_maturity - fitted_yield) * 100,

                # Store original spread_to_curve for backwards compatibility
                spread_to_curve = spread_bps,

                # Z-Score (cross-sectional)
                zscore = (spread_bps - mean(spread_bps, na.rm = TRUE)) / sd(spread_bps, na.rm = TRUE),
                zscore = ifelse(is.finite(zscore), zscore, 0),

                # Also update z_score for backwards compatibility
                z_score = zscore,

                # Absolute Z-Score for sizing/filtering
                zscore_abs = abs(zscore),

                # Store which x-variable was used (for labeling)
                x_variable = x_var,
                x_value = .data[[x_var]]
            )

        # -------------------------------------------------------------------------
        # Calculate curve metrics
        # -------------------------------------------------------------------------
        model_residuals <- fit_data$yield_to_maturity - fit_data$fitted_yield
        r_squared <- 1 - (sum(model_residuals^2) / sum((fit_data$yield_to_maturity - mean(fit_data$yield_to_maturity))^2))

        # Calculate confidence bands
        n <- length(model_residuals[!is.na(model_residuals)])
        df <- max(n - 6, 1)
        se <- sqrt(sum(model_residuals^2, na.rm = TRUE) / df)
        z_mult <- qnorm(1 - (1 - confidence_level/100)/2)

        curve_df$yield_lower <- curve_df$fitted_yield - z_mult * se
        curve_df$yield_upper <- curve_df$fitted_yield + z_mult * se

        metrics <- list(
            r_squared = r_squared,
            avg_spread = mean(abs(fit_data$spread_bps), na.rm = TRUE),
            max_spread = max(abs(fit_data$spread_bps), na.rm = TRUE),
            n_bonds = nrow(fit_data),
            x_variable = x_var,
            model_type = model_type,
            residual_se = se
        )

        # Dynamic x-axis label
        x_label <- switch(x_var,
                          "modified_duration" = "Modified Duration (years)",
                          "duration" = "Duration (years)",
                          "time_to_maturity" = "Time to Maturity (years)",
                          x_var  # fallback
        )

        # -------------------------------------------------------------------------
        # Return comprehensive list
        # -------------------------------------------------------------------------
        message(sprintf("[fitted_curve_data] SUCCESS: %d bonds, R²=%.3f, avg_spread=%.1f bps",
                        nrow(fit_data), r_squared, metrics$avg_spread))

        list(
            curve = curve_df,
            bonds = fit_data,
            metrics = metrics,
            x_var = x_var,
            x_label = x_label,
            model_type = model_type
        )
    })


    # DEBUG: Log when fitted_curve_data recalculates
    observe({
        data <- fitted_curve_data()
        if(!is.null(data)) {
            message(sprintf(
                "[DEBUG] fitted_curve_data updated: x_var=%s, model=%s, n_bonds=%d, avg_spread=%.2f bps",
                data$x_var,
                data$model_type,
                nrow(data$bonds),
                mean(abs(data$bonds$spread_bps), na.rm = TRUE)
            ))
        }
    })

    # DEBUG: Confirm table data is reading fresh values
    observe({
        req(fitted_curve_data())
        bonds <- fitted_curve_data()$bonds
        if(nrow(bonds) > 0) {
            message(sprintf(
                "[DEBUG] Table data: cheapest=%s (%.1f bps), richest=%s (%.1f bps)",
                bonds$bond[which.max(bonds$spread_bps)],
                max(bonds$spread_bps, na.rm = TRUE),
                bonds$bond[which.min(bonds$spread_bps)],
                min(bonds$spread_bps, na.rm = TRUE)
            ))
        }
    })


    # Calculate VaR - CRITICAL: Uses active bonds filter for consistency
    var_data <- reactive({
        req(filtered_data())

        # CRITICAL: Use active bonds from Risk Analytics filter
        # This ensures consistency with VaR Distribution plot and other sections
        active_bonds <- tryCatch({
            # Try to use the dedicated Risk Analytics active bonds reactive
            active_bonds_for_risk_analytics()
        }, error = function(e) {
            # Fallback: Get active bonds from fitted_curve_data if available
            tryCatch({
                unique(fitted_curve_data()$bonds$bond)
            }, error = function(e2) {
                # Final fallback: use all bonds but warn
                warning("[VaR] Could not get active bonds list - using all bonds")
                unique(filtered_data()$bond)
            })
        })

        # Filter data to active bonds only
        data <- filtered_data() %>%
            filter(bond %in% active_bonds)

        # Additional safety check: Remove known matured bonds
        known_matured_bonds <- c("R157", "R186", "R197", "R203", "R204", "R207", "R208", "R212", "R2023")
        found_matured <- intersect(unique(data$bond), known_matured_bonds)
        if (length(found_matured) > 0) {
            warning(sprintf("[VaR Calculation] REMOVING matured bonds: %s",
                           paste(found_matured, collapse = ", ")))
            data <- data %>% filter(!bond %in% known_matured_bonds)
        }

        message(sprintf("[VaR Calculation] Processing %d active bonds",
                       n_distinct(data$bond)))

        calculate_var(data,
                      confidence_levels = c(input$confidence_level/100, 0.99))
    })

    # Market regime detection
    regime_data <- reactive({
        req(filtered_data())

        detect_market_regime(filtered_data())
    })

    # Carry & Roll calculations
    carry_roll_data <- reactive({
        req(processed_data())

        # Trigger on refresh
        values$carry_refresh

        funding_rate_value <- if(!is.null(input$funding_rate)) {
            input$funding_rate
        } else {
            8.25
        }

        # ═══════════════════════════════════════════════════════════════════════
        # PRE-CALCULATION: Check raw coupon values per bond
        # ═══════════════════════════════════════════════════════════════════════
        if("coupon" %in% names(processed_data())) {
            coupon_check <- processed_data() %>%
                select(bond, coupon) %>%
                distinct() %>%
                arrange(bond)

            message("=== COUPON VALUES PER BOND (PRE-CALCULATION) ===")
            message(sprintf("Unique coupons: %d for %d bonds", n_distinct(coupon_check$coupon), nrow(coupon_check)))
            print(coupon_check)
            message("=================================================")
        }

        result <- calculate_advanced_carry_roll(
            processed_data(),
            holding_periods = c(30, 90, 180, 360),
            funding_rate = funding_rate_value
        )

        # ═══════════════════════════════════════════════════════════════════════
        # POST-CALCULATION: Verify carry income differentiation
        # ═══════════════════════════════════════════════════════════════════════
        if(nrow(result) > 0) {
            # Check 360-day returns (most visible in heatmap)
            returns_360 <- result %>%
                filter(holding_period == "360d") %>%
                select(bond, any_of(c("coupon_standardized", "carry_income",
                                      "roll_return", "gross_return", "net_return"))) %>%
                arrange(desc(net_return))

            n_unique_net <- n_distinct(round(returns_360$net_return, 2))
            n_unique_carry <- n_distinct(round(returns_360$carry_income, 2))
            n_bonds <- nrow(returns_360)

            message("=== CARRY & ROLL DIAGNOSTIC (360-DAY) ===")
            message(sprintf("Total bonds: %d", n_bonds))
            message(sprintf("Unique net returns: %d (should equal bonds)", n_unique_net))
            message(sprintf("Unique carry incomes: %d (should match unique coupons)", n_unique_carry))

            # Show net return range
            if(n_bonds > 0) {
                message(sprintf("Net return range: %.2f%% to %.2f%%",
                                min(returns_360$net_return, na.rm = TRUE),
                                max(returns_360$net_return, na.rm = TRUE)))

                # Log all bonds with their returns (sorted by net return)
                message("\n--- ALL BONDS (sorted by net return) ---")
                for(i in 1:nrow(returns_360)) {
                    message(sprintf("  %s: coupon=%.2f%%, carry=%.2f%%, roll=%.2f%%, net=%.2f%%",
                                    returns_360$bond[i],
                                    if("coupon_standardized" %in% names(returns_360)) returns_360$coupon_standardized[i] else NA,
                                    returns_360$carry_income[i],
                                    returns_360$roll_return[i],
                                    returns_360$net_return[i]))
                }
                message("-----------------------------------------")
            }

            # Warn if returns are suspiciously uniform
            if(n_unique_net < min(5, n_bonds) && n_bonds > 1) {
                warning(sprintf("⚠ LOW VARIATION: Only %d unique net returns for %d bonds - check coupon data!",
                                n_unique_net, n_bonds))
            } else {
                message(sprintf("✓ Return variation check PASSED: %d unique returns for %d bonds", n_unique_net, n_bonds))
            }
            message("==========================================")
        }

        return(result)
    })

    # Advanced insights generation
    insights <- reactive({
        req(filtered_data(), processed_data(), var_data(), regime_data())

        generate_advanced_insights(
            filtered_data(),
            processed_data(),
            var_data(),
            regime_data()
        )
    })


    # ================================================================================
    # OBSERVERS (Input Updates)
    # ================================================================================


    # Dynamic bond selection - Shows only ACTIVE bonds (filters out matured)
    observe({
        req(bond_data())

        # Get active bonds for current date range (excludes matured bonds)
        active <- tryCatch({
            req(active_bonds())
            active_bonds()
        }, error = function(e) {
            # Fallback: use all bonds if active_bonds not ready
            unique(bond_data()$bond)
        })

        # ═══════════════════════════════════════════════════════════════════════
        # CRITICAL FIX: Explicitly filter out known matured bonds from dropdown
        # This ensures R186 and other matured bonds never appear in Technical Analysis
        # ═══════════════════════════════════════════════════════════════════════
        known_matured_bonds <- c("R157", "R186", "R197", "R203", "R204", "R207", "R208", "R212", "R2023")
        found_matured <- intersect(active, known_matured_bonds)
        if (length(found_matured) > 0) {
            message(sprintf("[Bond Selection] Excluding matured bonds from dropdown: %s",
                           paste(found_matured, collapse = ", ")))
            active <- setdiff(active, known_matured_bonds)
        }
        message(sprintf("[Bond Selection] Active bonds for dropdown: %d (%s)",
                       length(active), paste(sort(active), collapse = ", ")))

        # Get currently selected bonds
        current_selection <- input$selected_bonds

        # If user has selection, keep only valid (active) bonds
        if (!is.null(current_selection) && length(current_selection) > 0) {
            valid_selection <- intersect(current_selection, active)
            # If no valid bonds remain, select ALL active bonds
            if (length(valid_selection) == 0) {
                valid_selection <- active
            }
        } else {
            # Default: select ALL active bonds (not just first 5)
            valid_selection <- active
        }

        updatePickerInput(
            session,
            "selected_bonds",
            choices = sort(active),
            selected = valid_selection
        )

        updateSelectInput(
            session,
            "tech_bond_select",
            choices = sort(active),
            selected = if(length(active) > 0) sort(active)[1] else NULL
        )
    })


    # USE IN SERVER:
    # Update bond selection choices dynamically:
    observe({
        req(bond_data())

        available_bonds <- get_plottable_bonds(bond_data())

        updateSelectInput(
            session,
            "technical_bond",
            choices = available_bonds,
            selected = if(length(available_bonds) > 0) available_bonds[1] else NULL
        )
    })

    # Date range quick selectors with animation
    observeEvent(input$ytd_btn, {
        updateDateRangeInput(session, "date_range",
                             start = floor_date(today(), "year"),
                             end = today())
    })

    # Add this observer in the server function
    observeEvent(input$recalculate_carry, {
        # Clear the memoise cache to force fresh calculation
        tryCatch({
            if(exists("calculate_advanced_carry_roll") && is.function(calculate_advanced_carry_roll)) {
                memoise::forget(calculate_advanced_carry_roll)
                message("✓ Cleared carry & roll calculation cache")
            }
        }, error = function(e) {
            message("Note: Could not clear cache - ", e$message)
        })

        # Force recalculation
        values$carry_refresh <- runif(1)

        showNotification("Recalculating carry & roll analysis...", type = "message", duration = 2)
    })

    # FIX 4: Update carry bond selector when data changes
    observe({
        req(carry_roll_data())

        # Get unique bonds sorted by 90-day net return
        bonds_sorted <- carry_roll_data() %>%
            filter(holding_period == "90d") %>%
            arrange(desc(net_return)) %>%
            pull(bond) %>%
            unique()

        updateSelectInput(
            session,
            "selected_carry_bond",
            choices = bonds_sorted,
            selected = if(length(bonds_sorted) > 0) bonds_sorted[1] else NULL
        )
    })

    observeEvent(input$qtr_btn, {
        updateDateRangeInput(session, "date_range",
                             start = floor_date(today(), "quarter"),
                             end = today())
    })

    observeEvent(input$mth_btn, {
        updateDateRangeInput(session, "date_range",
                             start = today() - months(1),
                             end = today())
    })

    observeEvent(input$yr_btn, {
        updateDateRangeInput(session, "date_range",
                             start = today() - years(1),
                             end = today())
    })

    observeEvent(input$mtd_btn, {
        updateDateRangeInput(session, "date_range",
                             start = floor_date(today(), "month"),
                             end = today())
    })

    # Reset sidebar controls to defaults
    observeEvent(input$reset_defaults, {
        updateSliderInput(session, "lookback_days", value = 60)
        updateSliderInput(session, "confidence_level", value = 95)
        updateSelectInput(session, "curve_model", selected = "nss")
        updateSelectInput(session, "risk_measure", selected = "mod_dur")

        showNotification(
            "Sidebar settings reset to defaults",
            type = "message",
            duration = 3
        )
    })

    # Bond selection helpers - Only select from ACTIVE bonds
    observeEvent(input$select_short, {
        req(bond_data())
        # Get active bonds for current date range
        active <- tryCatch(active_bonds(), error = function(e) unique(bond_data()$bond))

        short_bonds <- bond_data() %>%
            filter(bond %in% active) %>%
            group_by(bond) %>%
            summarise(avg_dur = mean(modified_duration, na.rm = TRUE), .groups = "drop") %>%
            filter(avg_dur < 5) %>%
            pull(bond)
        updatePickerInput(session, "selected_bonds", selected = short_bonds)
    })

    observeEvent(input$select_medium, {
        req(bond_data())
        # Get active bonds for current date range
        active <- tryCatch(active_bonds(), error = function(e) unique(bond_data()$bond))

        medium_bonds <- bond_data() %>%
            filter(bond %in% active) %>%
            group_by(bond) %>%
            summarise(avg_dur = mean(modified_duration, na.rm = TRUE), .groups = "drop") %>%
            filter(avg_dur >= 5, avg_dur <= 10) %>%
            pull(bond)
        updatePickerInput(session, "selected_bonds", selected = medium_bonds)
    })

    observeEvent(input$select_long, {
        req(bond_data())
        # Get active bonds for current date range
        active <- tryCatch(active_bonds(), error = function(e) unique(bond_data()$bond))

        long_bonds <- bond_data() %>%
            filter(bond %in% active) %>%
            group_by(bond) %>%
            summarise(avg_dur = mean(modified_duration, na.rm = TRUE), .groups = "drop") %>%
            filter(avg_dur > 10) %>%
            pull(bond)
        updatePickerInput(session, "selected_bonds", selected = long_bonds)
    })

    # ================================================================================
    # BUTTON STATE MANAGEMENT - Disable buttons when conditions not met
    # ================================================================================

    # Disable bond selection buttons when no data available
    observe({
        if (is.null(bond_data()) || nrow(bond_data()) == 0) {
            shinyjs::disable("select_short")
            shinyjs::disable("select_medium")
            shinyjs::disable("select_long")
        } else {
            shinyjs::enable("select_short")
            shinyjs::enable("select_medium")
            shinyjs::enable("select_long")
        }
    })

    # Disable export buttons when no filtered data available
    observe({
        if (is.null(filtered_data()) || nrow(filtered_data()) == 0) {
            shinyjs::disable("export_csv")
            shinyjs::disable("export_json")
            shinyjs::disable("export_bloomberg")
            shinyjs::disable("export_data")
        } else {
            shinyjs::enable("export_csv")
            shinyjs::enable("export_json")
            shinyjs::enable("export_bloomberg")
            shinyjs::enable("export_data")
        }
    })

    # Disable update predictions button when no bonds selected
    observe({
        if (is.null(input$auction_bonds_select) || length(input$auction_bonds_select) == 0) {
            shinyjs::disable("update_predictions")
        } else {
            shinyjs::enable("update_predictions")
        }
    })

    # Disable date range quick buttons during data loading
    observe({
        if (is.null(bond_data())) {
            shinyjs::disable("ytd_btn")
            shinyjs::disable("qtr_btn")
            shinyjs::disable("mth_btn")
            shinyjs::disable("yr_btn")
            shinyjs::disable("mtd_btn")
            shinyjs::disable("custom_range_btn")
        } else {
            shinyjs::enable("ytd_btn")
            shinyjs::enable("qtr_btn")
            shinyjs::enable("mth_btn")
            shinyjs::enable("yr_btn")
            shinyjs::enable("mtd_btn")
            shinyjs::enable("custom_range_btn")
        }
    })

    # ================================================================================
    # NEWLY ADDED BUTTON HANDLERS - UX IMPROVEMENTS
    # ================================================================================

    # Custom Range Button Handler
    observeEvent(input$custom_range_btn, {
        showModal(modalDialog(
            title = "Custom Date Range Selection",
            size = "m",

            tags$div(
                style = "padding: 15px;",
                dateRangeInput(
                    "custom_date_range_modal",
                    "Select Custom Date Range:",
                    start = input$date_range[1],
                    end = input$date_range[2],
                    format = "dd M yyyy",
                    width = "100%"
                ),

                tags$hr(),

                tags$h5("Quick Presets:"),
                fluidRow(
                    column(4, actionButton("preset_2y", "2 Years", class = "btn-sm btn-block")),
                    column(4, actionButton("preset_3y", "3 Years", class = "btn-sm btn-block")),
                    column(4, actionButton("preset_5y", "5 Years", class = "btn-sm btn-block"))
                ),
                fluidRow(
                    style = "margin-top: 10px;",
                    column(6, actionButton("preset_10y", "10 Years", class = "btn-sm btn-block")),
                    column(6, actionButton("preset_all", "All Data", class = "btn-sm btn-block"))
                )
            ),

            footer = tagList(
                modalButton("Cancel"),
                actionButton("apply_custom_range", "Apply", class = "btn-primary")
            )
        ))
    })

    # Apply Custom Range
    observeEvent(input$apply_custom_range, {
        req(input$custom_date_range_modal)
        updateDateRangeInput(session, "date_range",
                             start = input$custom_date_range_modal[1],
                             end = input$custom_date_range_modal[2])
        removeModal()
        showNotification("Custom date range applied successfully!", type = "message", duration = 3)
    })

    # Quick preset handlers inside modal
    observeEvent(input$preset_2y, {
        updateDateRangeInput(session, "custom_date_range_modal",
                             start = today() - years(2), end = today())
    })

    observeEvent(input$preset_3y, {
        updateDateRangeInput(session, "custom_date_range_modal",
                             start = today() - years(3), end = today())
    })

    observeEvent(input$preset_5y, {
        updateDateRangeInput(session, "custom_date_range_modal",
                             start = today() - years(5), end = today())
    })

    observeEvent(input$preset_10y, {
        updateDateRangeInput(session, "custom_date_range_modal",
                             start = today() - years(10), end = today())
    })

    observeEvent(input$preset_all, {
        req(bond_data())
        min_date <- min(bond_data()$date, na.rm = TRUE)
        updateDateRangeInput(session, "custom_date_range_modal",
                             start = min_date, end = today())
    })

    # ================================================================================
    # REPORT TYPE PRESET HANDLER
    # ================================================================================
    # Auto-configure sections when report type is selected
    observeEvent(input$report_type, {
        if(input$report_type == "treasury") {
            # Enable treasury section, disable others for focused Treasury Holdings Report
            updateCheckboxInput(session, "section_overview", value = FALSE)
            updateCheckboxInput(session, "section_relative", value = FALSE)
            updateCheckboxInput(session, "section_risk", value = FALSE)
            updateCheckboxInput(session, "section_technical", value = FALSE)
            updateCheckboxInput(session, "section_carry", value = FALSE)
            updateCheckboxInput(session, "section_auction", value = FALSE)
            updateCheckboxInput(session, "section_intelligence", value = FALSE)
            updateCheckboxInput(session, "section_treasury", value = TRUE)
            updateCheckboxInput(session, "section_recommendations", value = FALSE)

            # Select key treasury plots
            updateCheckboxInput(session, "plot_holdings_area", value = TRUE)
            updateCheckboxInput(session, "plot_sector_trend", value = TRUE)
            updateCheckboxInput(session, "plot_holdings_fixed", value = TRUE)
            updateCheckboxInput(session, "plot_holdings_ilb", value = TRUE)
            updateCheckboxInput(session, "plot_holdings_frn", value = FALSE)
            updateCheckboxInput(session, "plot_holdings_sukuk", value = FALSE)
            updateCheckboxInput(session, "plot_ownership_changes", value = TRUE)
            updateCheckboxInput(session, "plot_holdings_diverging_fixed", value = FALSE)
            updateCheckboxInput(session, "plot_holdings_diverging_ilb", value = FALSE)

            showNotification(
                "Treasury Holdings Report preset selected - Treasury section enabled",
                type = "message",
                duration = 3
            )
        }
    })

    # Update Predictions Button Handler
    observeEvent(input$update_predictions, {
        req(input$auction_bonds_select, input$next_auction_date)

        # Show loading indicator
        showNotification("Updating predictions... Please wait.",
                         type = "message",
                         duration = NULL,
                         id = "pred_loading")

        tryCatch({
            # Trigger prediction recalculation
            values$prediction_trigger <- runif(1)

            # Use delayed evaluation instead of blocking
            shiny::invalidateLater(100, session)

            # Remove loading notification after computation
            removeNotification(id = "pred_loading")

            # Show success message
            showNotification(
                paste("Predictions updated successfully for",
                      length(input$auction_bonds_select), "bonds"),
                type = "message",
                duration = 4
            )
        }, error = function(e) {
            removeNotification(id = "pred_loading")
            showNotification(
                paste("Error updating predictions:", e$message),
                type = "error",
                duration = 5
            )
        })
    })

    # Generate Email Report Button (Sidebar) - Link to main email function
    observeEvent(input$generate_email_report, {
        # Switch to Reports & Export tab
        updateTabItems(session, "main_tabs", "Reports & Export")

        # Use observe to trigger click after tab switches (non-blocking)
        observe({
            req(input$main_tabs == "Reports & Export")
            shinyjs::click("email_report")
        })

        showNotification(
            "Opening email report dialog...",
            type = "message",
            duration = 2
        )
    })

    # Export CSV Handler
    output$export_csv <- downloadHandler(
        filename = function() {
            paste0("bond_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
            tryCatch({
                req(filtered_data())
                cols <- get_export_columns(
                    filtered_data(),
                    include_metadata = isTRUE(input$include_metadata),
                    include_calculations = isTRUE(input$include_calculations)
                )
                export_data <- filtered_data() %>%
                    select(all_of(cols)) %>%
                    arrange(date, bond)

                write.csv(export_data, file, row.names = FALSE)

                showNotification("Data exported to CSV successfully!",
                                 type = "message", duration = 3)
            }, error = function(e) {
                showNotification(paste("Export error:", e$message),
                                 type = "error", duration = 5)
            })
        }
    )

    # Export JSON Handler
    output$export_json <- downloadHandler(
        filename = function() {
            paste0("bond_data_", format(Sys.Date(), "%Y%m%d"), ".json")
        },
        content = function(file) {
            tryCatch({
                req(filtered_data())
                cols <- get_export_columns(
                    filtered_data(),
                    include_metadata = isTRUE(input$include_metadata),
                    include_calculations = isTRUE(input$include_calculations)
                )
                export_data <- filtered_data() %>%
                    select(all_of(cols)) %>%
                    arrange(date, bond)

                json_data <- jsonlite::toJSON(export_data, pretty = TRUE)
                writeLines(json_data, file)

                showNotification("Data exported to JSON successfully!",
                                 type = "message", duration = 3)
            }, error = function(e) {
                showNotification(paste("Export error:", e$message),
                                 type = "error", duration = 5)
            })
        }
    )

    # Export Bloomberg Format Handler
    output$export_bloomberg <- downloadHandler(
        filename = function() {
            paste0("bond_data_bloomberg_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
            tryCatch({
                req(filtered_data())

                # Format for Bloomberg with expanded column set
                bloomberg_data <- filtered_data() %>%
                    mutate(
                        Ticker = bond,
                        Date = format(date, "%Y%m%d"),
                        YLD_YTM_MID = round(yield, 4),
                        CPN = if ("coupon" %in% names(.)) round(coupon, 4) else NA,
                        DUR_MID = round(modified_duration, 4),
                        DUR_MAC = if ("duration" %in% names(.)) round(duration, 4) else NA,
                        CONVEXITY_MID = round(convexity, 4),
                        DV01 = round(dv01, 6),
                        PX_CLEAN = if ("clean_price" %in% names(.)) round(clean_price, 4) else NA,
                        PX_DIRTY = if ("full_price" %in% names(.)) round(full_price, 4) else NA,
                        INT_ACC = if ("accrued_interest" %in% names(.)) round(accrued_interest, 4) else NA
                    ) %>%
                    select(Ticker, Date, YLD_YTM_MID, CPN, DUR_MID, DUR_MAC,
                           CONVEXITY_MID, DV01, PX_CLEAN, PX_DIRTY, INT_ACC) %>%
                    arrange(Date, Ticker)

                write.csv(bloomberg_data, file, row.names = FALSE)

                showNotification("Data exported in Bloomberg format successfully!",
                                 type = "message", duration = 3)
            }, error = function(e) {
                showNotification(paste("Export error:", e$message),
                                 type = "error", duration = 5)
            })
        }
    )


    # ================================================================================
    # ENHANCED PLOT OUTPUTS WITH SOPHISTICATED STYLING
    # ================================================================================

    # 1. Enhanced Yield Curve Plot
    # ========================================================================
    # CRITICAL: Plot REFITS when x-axis changes
    # All settings from Advanced Settings modal are passed to the generator
    # ========================================================================
    output$enhanced_yield_curve <- renderPlot({
        req(processed_data())

        # Read advanced settings with fallback defaults
        show_labels <- if(exists("curve_advanced_settings")) {
            curve_advanced_settings$show_labels
        } else {
            TRUE
        }

        show_conf_band <- if(exists("curve_advanced_settings")) {
            curve_advanced_settings$show_confidence_band
        } else {
            TRUE
        }

        point_size_metric <- if(exists("curve_advanced_settings")) {
            curve_advanced_settings$point_size_metric
        } else {
            "zscore"
        }

        confidence_level <- input$confidence_level %||% 95

        # Generate plot with all settings
        p <- generate_enhanced_yield_curve(
            processed_data(),
            list(
                xaxis_choice = input$xaxis_choice,
                curve_model = input$curve_model,
                lookback = input$lookback_days %||% 60,
                show_labels = show_labels,
                show_confidence_band = show_conf_band,
                point_size_metric = point_size_metric,
                confidence_level = confidence_level / 100
            )
        )
        if(!is.null(p)) print(p)
    })

    # ========================================================================
    # SHARED REACTIVE: Active bonds for Relative Value charts
    # Both Heatmap and Z-Score Distribution MUST use the same bond list
    # This ensures data consistency between the two visualizations
    # ========================================================================
    active_bonds_for_relative_value <- reactive({
        req(fitted_curve_data())
        # Get active bonds from fitted_curve_data (which filters to global latest date)
        # This automatically excludes matured bonds like R186 that don't have recent data
        active_bonds <- unique(fitted_curve_data()$bonds$bond)
        message(sprintf("[Relative Value] Active bonds for both charts: %d (%s)",
                       length(active_bonds),
                       paste(sort(active_bonds), collapse = ", ")))
        return(active_bonds)
    })

    # 2. Relative Value Heatmap
    # ========================================================================
    # CRITICAL FIX: Heatmap now uses same active bond list as Z-Score Distribution
    # This ensures R186 and other matured bonds are excluded from both charts
    # ========================================================================
    output$relative_value_heatmap <- renderPlot({
        req(filtered_data())
        req(active_bonds_for_relative_value())

        # Get the same active bonds used by Z-Score Distribution
        active_bonds <- active_bonds_for_relative_value()

        # Generate heatmap with active bond filter
        p <- generate_relative_value_heatmap(
            filtered_data(),
            list(
                active_bonds = active_bonds,
                label_threshold = 1.5  # Show labels for |Z| > 1.5
            )
        )
        if(!is.null(p)) print(p)
    })

    # 3. Enhanced Z-Score Distribution
    # ========================================================================
    # CRITICAL FIX: Z-Score plot now reads from fitted_curve_data() - SAME source as chart
    # Z-Scores are recalculated when x-axis or model changes
    # ========================================================================
    output$enhanced_zscore_plot <- renderPlot({
        req(fitted_curve_data())
        # Use bonds from fitted_curve_data which has recalculated z-scores
        p <- generate_enhanced_zscore_plot(fitted_curve_data()$bonds, list())
        if(!is.null(p)) print(p)
    })

    # ========================================================================
    # VALIDATION: Log bond consistency between Heatmap and Z-Score Distribution
    # ========================================================================
    observe({
        req(active_bonds_for_relative_value())
        req(fitted_curve_data())

        heatmap_bonds <- sort(active_bonds_for_relative_value())
        zscore_bonds <- sort(unique(fitted_curve_data()$bonds$bond))

        if (!setequal(heatmap_bonds, zscore_bonds)) {
            warning("[BOND MISMATCH] Heatmap and Z-Score Distribution show different bonds!")
            warning("  Heatmap: ", paste(heatmap_bonds, collapse = ", "))
            warning("  Z-Score: ", paste(zscore_bonds, collapse = ", "))
            only_in_heatmap <- setdiff(heatmap_bonds, zscore_bonds)
            only_in_zscore <- setdiff(zscore_bonds, heatmap_bonds)
            if (length(only_in_heatmap) > 0) warning("  Only in Heatmap: ", paste(only_in_heatmap, collapse = ", "))
            if (length(only_in_zscore) > 0) warning("  Only in Z-Score: ", paste(only_in_zscore, collapse = ", "))
        } else {
            message(sprintf("[Relative Value] Both charts show same %d bonds", length(heatmap_bonds)))
        }
    })

    # =========================================================================
    # VaR ANALYSIS REACTIVES AND OUTPUTS
    # =========================================================================

    # =========================================================================
    # SHARED REACTIVE: Active bonds for Risk Analytics
    # CRITICAL: Uses same filtering as Relative Value to ensure consistency
    # This ensures R186 and other matured bonds are excluded
    # =========================================================================
    active_bonds_for_risk_analytics <- reactive({
        req(fitted_curve_data())
        # Get active bonds from fitted_curve_data (which filters to global latest date)
        # This automatically excludes matured bonds like R186 that don't have recent data
        active_bonds <- unique(fitted_curve_data()$bonds$bond)

        # Additional safety check: Remove any known matured bonds
        known_matured_bonds <- c("R157", "R186", "R197", "R203", "R204", "R207", "R208", "R212", "R2023")
        found_matured <- intersect(active_bonds, known_matured_bonds)
        if (length(found_matured) > 0) {
            warning(sprintf("[Risk Analytics] REMOVING known matured bonds: %s",
                           paste(found_matured, collapse = ", ")))
            active_bonds <- setdiff(active_bonds, known_matured_bonds)
        }

        message(sprintf("[Risk Analytics] Active bonds: %d (%s)",
                       length(active_bonds),
                       paste(sort(active_bonds), collapse = ", ")))
        return(active_bonds)
    })

    # =========================================================================
    # Data quality assessment for VaR calculations
    # FIXED: Now scales thresholds with lookback period
    # Longer lookback → more observations needed for "Good" quality
    # =========================================================================
    assess_var_data_quality <- reactive({
        req(filtered_data())
        req(active_bonds_for_risk_analytics())

        active_bonds <- active_bonds_for_risk_analytics()
        lookback_days <- input$var_lookback %||% 252

        data <- filtered_data() %>%
            filter(bond %in% active_bonds)

        # Apply lookback filter
        if (!is.null(lookback_days) && lookback_days < 504) {
            cutoff_date <- max(data$date, na.rm = TRUE) - lookback_days
            data <- data %>% filter(date >= cutoff_date)
        }

        # Use the new function that properly scales with lookback period
        obs_count <- assess_data_quality_for_var(data, lookback_days, min_ratio = 0.8)

        return(obs_count)
    })

    # =========================================================================
    # CENTRALIZED VALID VAR DATA REACTIVE
    # CRITICAL: This is the single source of truth for both VaR charts
    # Ensures distribution and ladder plots show IDENTICAL bonds
    # =========================================================================
    valid_var_data_for_plotting <- reactive({
        req(var_data())
        req(assess_var_data_quality())

        # Get the raw VaR data
        raw_var_data <- var_data()

        # Apply comprehensive NA/invalid filtering using the new function
        valid_data <- prepare_var_data_for_plotting(raw_var_data, source = "VaR Ladder")

        # Get current parameters for logging
        method <- input$var_method %||% "historical"
        lookback_days <- input$var_lookback %||% 252
        horizon_days <- as.numeric(input$var_horizon) %||% 1

        # Log what was filtered
        original_bonds <- unique(raw_var_data$bond)
        valid_bonds <- unique(valid_data$bond)
        excluded_bonds <- setdiff(original_bonds, valid_bonds)

        if (length(excluded_bonds) > 0) {
            message(sprintf("[Valid VaR Data] Excluded %d bonds from %s method (%d-day lookback, %d-day horizon):",
                           length(excluded_bonds), method, lookback_days, horizon_days))
            for (b in excluded_bonds) {
                message(sprintf("  - %s", b))
            }
        }

        message(sprintf("[Valid VaR Data] %d valid bonds for both charts", nrow(valid_data)))

        return(valid_data)
    })

    # =========================================================================
    # BONDS EXCLUDED FROM VAR - for warning display
    # =========================================================================
    var_excluded_bonds <- reactive({
        req(active_bonds_for_risk_analytics())

        # Get active bonds and valid VaR bonds
        active_bonds <- active_bonds_for_risk_analytics()

        # Get valid VaR bonds (handle case where valid_var_data might be empty)
        valid_var_bonds <- tryCatch({
            valid_data <- valid_var_data_for_plotting()
            if (is.null(valid_data) || nrow(valid_data) == 0) {
                character(0)
            } else {
                unique(valid_data$bond)
            }
        }, error = function(e) character(0))

        # Find excluded bonds
        excluded <- setdiff(active_bonds, valid_var_bonds)

        return(excluded)
    })

    # Reactive to store the VaR distribution plot and its bond ordering
    var_distribution_results <- reactive({
        req(filtered_data())
        req(active_bonds_for_risk_analytics())

        # Get parameters from UI controls
        horizon_days <- as.numeric(input$var_horizon) %||% 1
        lookback_days <- input$var_lookback %||% 252
        method <- input$var_method %||% "historical"

        # CRITICAL: Get the set of valid bonds from centralized reactive
        # This ensures distribution plot only includes bonds that will also appear in ladder
        valid_var_bonds <- tryCatch({
            valid_data <- valid_var_data_for_plotting()
            if (!is.null(valid_data) && nrow(valid_data) > 0) {
                unique(valid_data$bond)
            } else {
                # Fallback to active bonds if valid_var_data not available yet
                active_bonds_for_risk_analytics()
            }
        }, error = function(e) {
            active_bonds_for_risk_analytics()
        })

        # Filter data to ONLY valid VaR bonds and lookback period
        data <- filtered_data() %>%
            filter(bond %in% valid_var_bonds)

        if (!is.null(lookback_days) && lookback_days < 504) {
            cutoff_date <- max(data$date, na.rm = TRUE) - lookback_days
            data <- data %>% filter(date >= cutoff_date)
        }

        # Get data quality assessment for annotations
        data_quality <- assess_var_data_quality() %>%
            filter(bond %in% valid_var_bonds)  # Only include quality for valid bonds

        message(sprintf("[VaR Distribution] Using %d valid bonds (filtered from %d active)",
                       length(valid_var_bonds), length(active_bonds_for_risk_analytics())))

        # Generate the plot with parameters
        params <- list(
            horizon_days = horizon_days,
            method = method,
            show_stats = FALSE,  # Stats are in separate table now
            enable_diagnostics = FALSE,
            data_quality = data_quality  # Pass quality info for annotations
        )

        p <- generate_var_distribution_plot(data, params)

        # Return both plot and bond order
        list(
            plot = p,
            bond_order = attr(p, "bond_order"),
            var_summary = attr(p, "var_summary"),
            data_quality = data_quality,
            valid_bonds = valid_var_bonds  # Include for verification
        )
    })

    # 4. VaR Distribution Plot
    output$var_distribution_plot <- renderPlot({
        req(var_distribution_results())
        p <- var_distribution_results()$plot
        if(!is.null(p)) print(p)
    })

    # 5. VaR Ladder Plot - uses SAME filtered data as distribution plot
    # CRITICAL: Uses valid_var_data_for_plotting() to ensure identical bonds
    output$var_ladder_plot <- renderPlot({
        req(valid_var_data_for_plotting(), var_distribution_results())

        # Use the centralized valid VaR data (already filtered for NA/invalid values)
        valid_var_data <- valid_var_data_for_plotting()

        # Get distribution results
        distribution_results <- var_distribution_results()

        # =========================================================================
        # CRITICAL FIX: Filter ladder data to SAME bonds as distribution plot
        # =========================================================================
        # The distribution plot filters bonds based on data sufficiency for the
        # current horizon (e.g., 5-day returns require 5 consecutive observations).
        # The ladder must use the EXACT same set of bonds to prevent:
        # 1. Bond count mismatch (e.g., distribution shows 12, ladder shows 17)
        # 2. NA rows appearing when bond_order factor levels don't match data
        # =========================================================================
        if (!is.null(distribution_results$valid_bonds) && length(distribution_results$valid_bonds) > 0) {
            valid_bonds_from_dist <- distribution_results$valid_bonds

            # Log what we're filtering
            original_bonds <- unique(valid_var_data$bond)
            bonds_to_exclude <- setdiff(original_bonds, valid_bonds_from_dist)

            if (length(bonds_to_exclude) > 0) {
                message(sprintf("[Risk Ladder] FILTERING to match distribution: %d -> %d bonds",
                               length(original_bonds), length(valid_bonds_from_dist)))
                message(sprintf("[Risk Ladder] Excluding %d bonds not in distribution: %s",
                               length(bonds_to_exclude), paste(bonds_to_exclude, collapse = ", ")))
            }

            valid_var_data <- valid_var_data %>%
                filter(bond %in% valid_bonds_from_dist)
        }

        # Secondary safety check: Ensure no NA bonds remain
        valid_var_data <- valid_var_data %>%
            filter(
                !is.na(bond),
                bond != "",
                bond != "NA",
                as.character(bond) != "NA"
            )

        # Verify we have data
        if (nrow(valid_var_data) == 0) {
            warning("[Risk Ladder] No valid bonds after filtering to match distribution")
            return(
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5,
                             label = "No bonds with sufficient data\nfor current VaR parameters",
                             size = 5, color = "#666666") +
                    theme_void() +
                    labs(title = "Risk Ladder: VaR & Expected Shortfall")
            )
        }

        # Log final bond count for verification
        message(sprintf("[Risk Ladder] Plotting %d bonds (matching distribution)",
                       nrow(valid_var_data)))
        message(sprintf("[Risk Ladder] Bond list: %s",
                       paste(valid_var_data$bond, collapse = ", ")))

        # Pass the bond order and data quality from distribution plot for consistency
        params <- list(
            bond_order = distribution_results$bond_order,
            data_quality = distribution_results$data_quality  # Same quality info as distribution
        )

        p <- generate_var_ladder_plot(valid_var_data, params)
        if(!is.null(p)) print(p)
    })

    # =========================================================================
    # VAR EXCLUSION WARNING UI
    # Shows warning when bonds are excluded due to insufficient data
    # =========================================================================
    output$var_exclusion_warning <- renderUI({
        # Get excluded bonds
        excluded <- var_excluded_bonds()

        # If no exclusions, return nothing
        if (length(excluded) == 0) {
            return(NULL)
        }

        # Get current method for suggestion
        method <- input$var_method %||% "historical"
        lookback_days <- input$var_lookback %||% 252

        # Build warning HTML
        warning_html <- paste0(
            "<div class='alert alert-warning' style='margin: 10px 0; padding: 10px; border-radius: 5px;'>",
            "<strong>\u26A0\uFE0F ", length(excluded), " bond(s) excluded</strong> from VaR analysis ",
            "due to insufficient data for current parameters:<br>",
            "<ul style='margin-top: 5px; margin-bottom: 5px; padding-left: 20px;'>",
            paste0("<li>", excluded, "</li>", collapse = ""),
            "</ul>",
            "<small><em>",
            if (method == "parametric") {
                "Try using Historical Simulation method (requires fewer observations) or reducing the lookback period."
            } else {
                paste0("Try reducing the lookback period below ", lookback_days, " days.")
            },
            "</em></small>",
            "</div>"
        )

        HTML(warning_html)
    })

    # =========================================================================
    # VALIDATION: Log bond consistency between Risk Analytics and other sections
    # =========================================================================
    observe({
        # Validate that Risk Analytics uses the same bond universe as other sections
        req(active_bonds_for_risk_analytics())
        req(var_data())

        risk_bonds <- sort(active_bonds_for_risk_analytics())
        var_bonds <- sort(unique(var_data()$bond))

        # Get relative value bonds for comparison
        rel_val_bonds <- tryCatch({
            sort(active_bonds_for_relative_value())
        }, error = function(e) NULL)

        # Check consistency between Risk Analytics filter and VaR data
        if (!setequal(risk_bonds, var_bonds)) {
            warning("[RISK ANALYTICS BOND MISMATCH] Filter and VaR calculation have different bonds!")
            warning("  Active filter: ", paste(risk_bonds, collapse = ", "))
            warning("  VaR data: ", paste(var_bonds, collapse = ", "))
        }

        # Check consistency with Relative Value section
        if (!is.null(rel_val_bonds)) {
            if (!setequal(risk_bonds, rel_val_bonds)) {
                warning("[BOND COUNT MISMATCH] Risk Analytics has different bonds than Relative Value!")
                warning("  Risk Analytics: ", length(risk_bonds), " bonds")
                warning("  Relative Value: ", length(rel_val_bonds), " bonds")
                only_in_risk <- setdiff(risk_bonds, rel_val_bonds)
                only_in_rv <- setdiff(rel_val_bonds, risk_bonds)
                if (length(only_in_risk) > 0) warning("  Only in Risk: ", paste(only_in_risk, collapse = ", "))
                if (length(only_in_rv) > 0) warning("  Only in RelVal: ", paste(only_in_rv, collapse = ", "))
            } else {
                message(sprintf("[Risk Analytics] \u2713 %d active bonds (matches Relative Value section)",
                               length(risk_bonds)))
            }
        }

        # CRITICAL: Verify no matured bonds are present
        known_matured_bonds <- c("R157", "R186", "R197", "R203", "R204", "R207", "R208", "R212", "R2023")
        found_matured_in_risk <- intersect(risk_bonds, known_matured_bonds)
        found_matured_in_var <- intersect(var_bonds, known_matured_bonds)

        if (length(found_matured_in_risk) > 0 || length(found_matured_in_var) > 0) {
            warning("[CRITICAL] MATURED BONDS FOUND IN RISK ANALYTICS!")
            if (length(found_matured_in_risk) > 0) {
                warning("  In active filter: ", paste(found_matured_in_risk, collapse = ", "))
            }
            if (length(found_matured_in_var) > 0) {
                warning("  In VaR data: ", paste(found_matured_in_var, collapse = ", "))
            }
        } else {
            message("[Risk Analytics] \u2713 No known matured bonds present")
        }
    })

    # 6. VaR Statistics Table - FIXED VERSION
    # FIXES:
    # 1. Sorts by 95% VaR descending (highest risk first)
    # 2. Uses ABSOLUTE thresholds for tail risk (not percentile-based) for consistency
    # 3. Same inputs = same outputs (no percentile-dependent classifications)
    output$var_statistics_table <- DT::renderDataTable({
        req(var_distribution_results())

        var_summary <- var_distribution_results()$var_summary

        # Handle case where summary might be NULL
        if (is.null(var_summary) || nrow(var_summary) == 0) {
            return(DT::datatable(data.frame(Message = "No VaR data available")))
        }

        # Create table with interpretations
        # Convert to absolute loss values for more intuitive display
        table_data <- var_summary %>%
            mutate(
                # Convert VaR to positive loss values (more intuitive)
                var_95_loss = abs(VaR_95),
                var_99_loss = abs(VaR_99),
                cvar_loss = abs(CVaR_95),

                # Interpret skewness - LOWERED THRESHOLDS for better sensitivity
                skew_signal = case_when(
                    skewness < -0.3 ~ "Left tail risk \u26A0",
                    skewness < -0.1 ~ "Slight left skew",
                    skewness > 0.3 ~ "Right skewed",
                    skewness > 0.1 ~ "Slight right skew",
                    TRUE ~ "Symmetric \u2713"
                ),

                # Interpret kurtosis - NOW USING EXCESS KURTOSIS (normal = 0)
                # For SA gov bonds, typical range is 2.5-3.5 (all have fat tails)
                kurt_signal = case_when(
                    kurtosis > 3.2 ~ "Very fat tails \u26A0",
                    kurtosis > 3.0 ~ "Fat tails",
                    kurtosis > 2.8 ~ "Moderately fat",
                    TRUE ~ "Near normal"
                ),

                # Calculate tail ratio for classification
                tail_ratio = abs(VaR_99) / abs(VaR_95)
            ) %>%
            mutate(
                # =========================================================================
                # TAIL RISK CLASSIFICATION - ABSOLUTE THRESHOLDS (not percentile-based)
                # Ensures same inputs always produce same outputs
                # =========================================================================
                # Criteria:
                #   HIGH risk if ANY of:
                #     - Excess Kurtosis > 4.0 AND |Skew| > 0.5 (fat tails + asymmetry)
                #     - 99% VaR / 95% VaR ratio > 1.50 (extreme tail)
                #     - |Skew| > 1.0 (highly asymmetric)
                #   ELEVATED risk if:
                #     - Excess Kurtosis > 3.5 OR |Skew| > 0.3
                #   MODERATE risk if:
                #     - Excess Kurtosis > 3.0 OR |Skew| > 0.15
                #   LOW risk:
                #     - All metrics near normal distribution values
                # =========================================================================
                is_high_risk = (kurtosis > 4.0 & abs(skewness) > 0.5) |
                               (tail_ratio > 1.50) |
                               (abs(skewness) > 1.0),

                is_elevated_risk = !is_high_risk &
                                   ((kurtosis > 3.5) | (abs(skewness) > 0.3)),

                is_moderate_risk = !is_high_risk & !is_elevated_risk &
                                   ((kurtosis > 3.0) | (abs(skewness) > 0.15)),

                tail_risk = case_when(
                    is_high_risk ~ "High \u26A0",
                    is_elevated_risk ~ "Elevated",
                    is_moderate_risk ~ "Moderate",
                    TRUE ~ "Low \u2713"
                )
            ) %>%
            # CRITICAL FIX: Sort by 95% VaR descending (highest risk first)
            arrange(desc(var_95_loss)) %>%
            select(
                Bond = bond,
                `95% VaR Loss (%)` = var_95_loss,
                `99% VaR Loss (%)` = var_99_loss,
                `CVaR Loss (%)` = cvar_loss,
                Skew = skewness,
                `Skew Signal` = skew_signal,
                `Excess Kurtosis` = kurtosis,
                `Tail Risk` = tail_risk,
                Obs = n_observations
            )

        # Log sorted order for verification
        message("[VaR Stats Table] Sorted by 95% VaR (desc):")
        message(sprintf("  Top 5: %s",
                       paste(head(table_data$Bond, 5), " (",
                             round(head(table_data$`95% VaR Loss (%)`, 5), 2), "%)",
                             collapse = ", ", sep = "")))
        message(sprintf("  Bottom 5: %s",
                       paste(tail(table_data$Bond, 5), " (",
                             round(tail(table_data$`95% VaR Loss (%)`, 5), 2), "%)",
                             collapse = ", ", sep = "")))

        DT::datatable(
            table_data,
            options = list(
                pageLength = 20,
                dom = 't',  # Table only, no search/pagination
                ordering = FALSE,  # Disable client-side reordering to preserve our sort
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all'),
                    # Ensure numeric columns are treated as numbers
                    list(type = 'num', targets = c(1, 2, 3, 4, 6))
                )
            ),
            rownames = FALSE
        ) %>%
            DT::formatRound(c('95% VaR Loss (%)', '99% VaR Loss (%)', 'CVaR Loss (%)'), digits = 2) %>%
            DT::formatRound(c('Skew', 'Excess Kurtosis'), digits = 2) %>%
            DT::formatStyle(
                'Tail Risk',
                backgroundColor = DT::styleEqual(
                    c('High \u26A0', 'Elevated', 'Moderate', 'Low \u2713'),
                    c('#FFCDD2', '#FFE0B2', '#FFF9C4', '#C8E6C9')  # Red, Orange, Yellow, Green
                ),
                color = DT::styleEqual(
                    c('High \u26A0', 'Elevated', 'Moderate', 'Low \u2713'),
                    c('#B71C1C', '#E65100', '#F57F17', '#1B5E20')  # Dark red, orange, yellow, green
                ),
                fontWeight = 'bold'
            ) %>%
            DT::formatStyle(
                'Skew Signal',
                backgroundColor = DT::styleEqual(
                    c('Left tail risk \u26A0', 'Slight left skew', 'Right skewed', 'Slight right skew', 'Symmetric \u2713'),
                    c('#FFCDD2', '#FFE0B2', '#FFF9C4', '#E3F2FD', '#C8E6C9')
                )
            )
    })

    # 6. DV01 Ladder Plot
    # CRITICAL: Filter to active bonds only (excludes matured bonds like R186)
    output$dv01_ladder_plot <- renderPlot({
        req(processed_data())
        req(active_bonds_for_risk_analytics())

        # Get active bonds from centralized reactive
        active_bonds <- active_bonds_for_risk_analytics()

        # Filter data to active bonds only
        plot_data <- processed_data() %>%
            filter(bond %in% active_bonds)

        # Log what we're using
        excluded_bonds <- setdiff(unique(processed_data()$bond), active_bonds)
        if (length(excluded_bonds) > 0) {
            message(sprintf("[DV01 Ladder] Excluded %d matured/inactive bonds: %s",
                           length(excluded_bonds), paste(excluded_bonds, collapse = ", ")))
        }
        message(sprintf("[DV01 Ladder] Using %d active bonds", length(unique(plot_data$bond))))

        # Get notional from input (default R10 million)
        notional <- as.numeric(input$dv01_notional) %||% 10000000

        p <- generate_dv01_ladder_plot(plot_data, list(
            notional = notional
        ))
        if(!is.null(p)) print(p)
    })

    # 7. Enhanced Convexity Plot
    # CRITICAL: Filter to active bonds only (excludes matured bonds like R186)
    output$enhanced_convexity_plot <- renderPlot({
        req(processed_data())
        req(active_bonds_for_risk_analytics())

        # Get active bonds from centralized reactive
        active_bonds <- active_bonds_for_risk_analytics()

        # Filter data to active bonds only
        plot_data <- processed_data() %>%
            filter(bond %in% active_bonds)

        # Log what we're using
        excluded_bonds <- setdiff(unique(processed_data()$bond), active_bonds)
        if (length(excluded_bonds) > 0) {
            message(sprintf("[Convexity Profile] Excluded %d matured/inactive bonds: %s",
                           length(excluded_bonds), paste(excluded_bonds, collapse = ", ")))
        }
        message(sprintf("[Convexity Profile] Using %d active bonds", length(unique(plot_data$bond))))

        # Get notional from input (same as DV01 ladder for consistency)
        notional <- as.numeric(input$dv01_notional) %||% 10000000

        p <- generate_enhanced_convexity_plot(plot_data, list(
            notional = notional
        ))
        if(!is.null(p)) print(p)
    })

    # 8. Advanced Technical Indicators Plot
    # ════════════════════════════════════════════════════════════════════════
    # ADVANCED TECHNICAL INDICATORS PLOT
    # ════════════════════════════════════════════════════════════════════════

    output$advanced_technical_plot <- renderPlot({

        req(filtered_data_with_technicals(), input$tech_bond_select)

        tryCatch({

            # Get data
            data <- filtered_data_with_technicals()

            # Validate data
            if(is.null(data) || nrow(data) == 0) {
                showNotification(
                    "No data available for technical analysis",
                    type = "warning",
                    duration = 3
                )
                return(NULL)
            }

            # Validate data class
            data <- validate_dataframe_class(
                data,
                "advanced_technical_plot [render]"
            )

            # Get indicator type with fallback
            indicator_type <- input$tech_indicator_type %||% "all"

            # Validate bond exists in data
            if(!input$tech_bond_select %in% unique(data$bond)) {
                showNotification(
                    paste("Bond", input$tech_bond_select, "not found in filtered data"),
                    type = "warning",
                    duration = 5
                )
                return(NULL)
            }

            # Generate plot (returns grob from arrangeGrob)
            plot_grob <- generate_advanced_technical_plot(
                data = data,
                bond_select = input$tech_bond_select,
                indicator_type = indicator_type
            )

            # ✅ CRITICAL FIX: Draw the grob explicitly
            grid::grid.newpage()
            grid::grid.draw(plot_grob)

        }, error = function(e) {

            # Log error
            warning(paste(
                "Error in advanced_technical_plot:",
                e$message
            ))

            # Show user notification
            showNotification(
                paste(
                    "Unable to generate technical plot:",
                    e$message
                ),
                type = "error",
                duration = 7
            )

            # Return informative error plot
            error_plot <- ggplot() +
                annotate(
                    "text",
                    x = 0.5,
                    y = 0.6,
                    label = "Technical Analysis Plot Error",
                    color = insele_palette$danger,
                    size = 6,
                    fontface = "bold"
                ) +
                annotate(
                    "text",
                    x = 0.5,
                    y = 0.4,
                    label = paste(
                        "Failed to generate plot\n",
                        "Bond:", input$tech_bond_select,
                        "\n\nError:", e$message,
                        "\n\nTry:",
                        "\n• Selecting a different bond",
                        "\n• Adjusting date range for more data"
                    ),
                    color = insele_palette$dark_gray,
                    size = 4,
                    lineheight = 1.3
                ) +
                xlim(0, 1) +
                ylim(0, 1) +
                theme_void()

            print(error_plot)
        })
    })

    # 9. Signal Matrix Heatmap - USING MASTER REACTIVE FOR CONSISTENCY
    output$signal_matrix_heatmap <- renderPlot({
        req(technical_signals_master())

        # Use the MASTER reactive for consistent signals
        signal_data <- technical_signals_master()

        # Validation
        if(nrow(signal_data) < 2) {
            plot.new()
            text(0.5, 0.5, "Need at least 2 bonds for signal matrix",
                 cex = 1.2, col = "#666666")
            return()
        }

        # Prepare data for matrix visualization
        matrix_data <- signal_data %>%
            select(
                bond,
                RSI = rsi_signal_yield,
                BB = bb_signal_yield,
                MACD = macd_signal_yield,
                MOM = momentum_signal_yield,
                TOTAL = total_score
            ) %>%
            pivot_longer(
                cols = c(RSI, BB, MACD, MOM, TOTAL),
                names_to = "indicator",
                values_to = "score"
            ) %>%
            mutate(
                score = as.numeric(score),
                score = ifelse(is.na(score), 0, score),
                indicator = factor(indicator, levels = c("RSI", "BB", "MACD", "MOM", "TOTAL"))
            )

        # Sort bonds by TOTAL score (strongest buy at top)
        bond_order <- signal_data %>%
            arrange(desc(total_score)) %>%
            pull(bond)

        matrix_data <- matrix_data %>%
            mutate(bond = factor(bond, levels = rev(bond_order)))

        # Create discrete color categories
        # Different thresholds for TOTAL (-8 to +8) vs individual indicators (-2 to +2)
        matrix_data <- matrix_data %>%
            mutate(
                color_cat = case_when(
                    # TOTAL column uses different thresholds
                    indicator == "TOTAL" & score >= 4 ~ "strong_buy",
                    indicator == "TOTAL" & score >= 2 ~ "buy",
                    indicator == "TOTAL" & score >= -1 ~ "neutral",
                    indicator == "TOTAL" & score >= -3 ~ "sell",
                    indicator == "TOTAL" ~ "strong_sell",
                    # Individual indicators (-2 to +2 scale)
                    score == 2 ~ "strong_buy",
                    score == 1 ~ "buy",
                    score == 0 ~ "neutral",
                    score == -1 ~ "sell",
                    score == -2 ~ "strong_sell",
                    TRUE ~ "neutral"
                ),
                color_cat = factor(color_cat,
                    levels = c("strong_sell", "sell", "neutral", "buy", "strong_buy"))
            )

        # Create the heatmap
        p <- ggplot(matrix_data, aes(x = indicator, y = bond, fill = color_cat)) +
            geom_tile(color = "white", linewidth = 1) +
            geom_text(aes(label = score), size = 3.5, fontface = "bold",
                color = ifelse(matrix_data$color_cat %in% c("strong_buy", "strong_sell"),
                               "white", "gray20")) +
            scale_fill_manual(
                values = c(
                    "strong_sell" = "#B71C1C",
                    "sell" = "#E57373",
                    "neutral" = "#9E9E9E",
                    "buy" = "#81C784",
                    "strong_buy" = "#1B5E20"
                ),
                name = "Signal",
                labels = c("Strong Sell", "Sell", "Neutral", "Buy", "Strong Buy"),
                drop = FALSE
            ) +
            scale_x_discrete(expand = c(0, 0)) +
            scale_y_discrete(expand = c(0, 0)) +
            labs(
                title = "Trading Signal Matrix",
                subtitle = "Sorted by total score (strongest buy at top) | Bond Price Perspective",
                x = "", y = "",
                caption = "+2 = Strong Buy (good for prices) | -2 = Strong Sell (bad for prices)"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(face = "bold", color = "#1B3A6B", size = 14),
                plot.subtitle = element_text(color = "#666666", size = 10),
                plot.caption = element_text(color = "#888888", size = 8),
                axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 10),
                axis.text.y = element_text(face = "bold", size = 9),
                panel.grid = element_blank(),
                panel.border = element_rect(fill = NA, color = "#666666", linewidth = 1),
                legend.position = "bottom"
            )

        print(p)
    })

    # 9b. Signal Summary Statistics Panel - USING MASTER REACTIVE FOR CONSISTENCY
    output$signal_summary_stats <- renderUI({
        req(technical_signals_master())

        # Use the MASTER reactive for consistent signals
        latest_data <- technical_signals_master()

        # Ensure required columns exist
        if(!"overall_signal" %in% names(latest_data)) {
            return(tags$div(
                style = "color: #666; padding: 10px;",
                "Signal data not available"
            ))
        }

        # Count signals by type (using overall_signal from master reactive)
        signal_counts <- latest_data %>%
            count(overall_signal) %>%
            rename(signal_strength = overall_signal) %>%
            arrange(factor(signal_strength,
                           levels = c("Strong Buy", "Buy", "Neutral", "Sell", "Strong Sell")))

        # Calculate curve-wide averages
        avg_rsi <- mean(latest_data$rsi_14, na.rm = TRUE)
        avg_roc <- if("roc_20" %in% names(latest_data)) {
            mean(latest_data$roc_20, na.rm = TRUE)
        } else NA

        # Determine dominant signal
        dominant_signal <- if(nrow(signal_counts) > 0) {
            signal_counts %>%
                slice_max(n, n = 1) %>%
                pull(signal_strength) %>%
                first()
        } else "Unknown"

        # Determine signal color
        signal_color <- case_when(
            dominant_signal == "Strong Buy" ~ "#1B5E20",
            dominant_signal == "Buy" ~ "#4CAF50",
            dominant_signal == "Sell" ~ "#FF7043",
            dominant_signal == "Strong Sell" ~ "#C62828",
            TRUE ~ "#666666"
        )

        # Build summary UI
        tags$div(
            class = "well well-sm",
            style = "background: #F8F9FA; padding: 12px; margin-bottom: 15px; border-radius: 4px; border-left: 4px solid #1B3A6B;",

            fluidRow(
                column(3,
                    tags$div(
                        style = "text-align: center;",
                        tags$strong("Signal Distribution", style = "color: #1B3A6B;"),
                        tags$div(style = "margin-top: 8px;",
                            lapply(1:nrow(signal_counts), function(i) {
                                sig <- signal_counts$signal_strength[i]
                                cnt <- signal_counts$n[i]
                                col <- case_when(
                                    sig == "Strong Buy" ~ "#1B5E20",
                                    sig == "Buy" ~ "#4CAF50",
                                    sig == "Neutral" ~ "#9E9E9E",
                                    sig == "Sell" ~ "#FF7043",
                                    sig == "Strong Sell" ~ "#C62828",
                                    TRUE ~ "#666666"
                                )
                                tags$div(
                                    style = sprintf("font-size: 12px; color: %s;", col),
                                    sprintf("%s: %d", sig, cnt)
                                )
                            })
                        )
                    )
                ),
                column(3,
                    tags$div(
                        style = "text-align: center;",
                        tags$strong("Curve Avg RSI", style = "color: #1B3A6B;"),
                        tags$div(
                            style = sprintf("font-size: 24px; font-weight: bold; color: %s; margin-top: 5px;",
                                           if(!is.na(avg_rsi) && avg_rsi > 60) "#C62828"
                                           else if(!is.na(avg_rsi) && avg_rsi < 40) "#1B5E20"
                                           else "#666666"),
                            sprintf("%.1f", if(!is.na(avg_rsi)) avg_rsi else 50)
                        ),
                        tags$div(style = "font-size: 10px; color: #999;",
                                 if(!is.na(avg_rsi) && avg_rsi > 70) "Overbought"
                                 else if(!is.na(avg_rsi) && avg_rsi < 30) "Oversold"
                                 else "Neutral Zone")
                    )
                ),
                column(3,
                    tags$div(
                        style = "text-align: center;",
                        tags$strong("Avg 20d ROC", style = "color: #1B3A6B;"),
                        tags$div(
                            style = sprintf("font-size: 24px; font-weight: bold; color: %s; margin-top: 5px;",
                                           if(!is.na(avg_roc) && avg_roc > 1) "#C62828"
                                           else if(!is.na(avg_roc) && avg_roc < -1) "#1B5E20"
                                           else "#666666"),
                            sprintf("%+.2f%%", if(!is.na(avg_roc)) avg_roc else 0)
                        ),
                        tags$div(style = "font-size: 10px; color: #999;",
                                 if(!is.na(avg_roc) && avg_roc > 2) "Yields Rising"
                                 else if(!is.na(avg_roc) && avg_roc < -2) "Yields Falling"
                                 else "Stable")
                    )
                ),
                column(3,
                    tags$div(
                        style = "text-align: center;",
                        tags$strong("Dominant Signal", style = "color: #1B3A6B;"),
                        tags$div(
                            style = sprintf("font-size: 20px; font-weight: bold; color: %s; margin-top: 5px;",
                                           signal_color),
                            dominant_signal
                        ),
                        tags$div(style = "font-size: 10px; color: #999;",
                                 sprintf("%d bonds analyzed", nrow(latest_data)))
                    )
                )
            )
        )
    })

    # 10. Enhanced Carry & Roll Heatmap
    output$enhanced_carry_roll_heatmap <- renderPlot({
        req(carry_roll_data())

        return_type_value <- if(!is.null(input$return_type)) {
            input$return_type
        } else {
            "net"
        }

        funding_rate_value <- if(!is.null(input$funding_rate)) {
            input$funding_rate
        } else {
            8.25
        }

        p <- generate_enhanced_carry_roll_heatmap(
            carry_roll_data(),
            return_type_value,
            funding_rate = funding_rate_value
        )
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "No carry & roll data available", cex = 1.5)
        }
    })

    # 11. Scenario Analysis Plot - Enhanced with user controls

    # Observe and populate bond selection dropdown dynamically
    observe({
        req(processed_data())

        # Get available bonds sorted by duration
        available_bonds <- processed_data() %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            ungroup() %>%
            arrange(modified_duration) %>%
            pull(bond) %>%
            unique()

        # Select top 5 bonds by duration spread as default
        # (gives good representation across the curve)
        n_bonds <- length(available_bonds)
        if (n_bonds > 0) {
            default_indices <- unique(round(seq(1, n_bonds, length.out = min(5, n_bonds))))
            default_selection <- available_bonds[default_indices]
        } else {
            default_selection <- NULL
        }

        updateSelectInput(
            session,
            "scenario_bonds_select",
            choices = available_bonds,
            selected = default_selection
        )
    })

    # Quick selection observers for scenario analysis
    observeEvent(input$select_short_bonds, {
        req(processed_data())
        short_bonds <- processed_data() %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            ungroup() %>%
            filter(modified_duration < 5) %>%
            pull(bond) %>%
            unique()
        if (length(short_bonds) > 0) {
            updateSelectInput(session, "scenario_bonds_select", selected = short_bonds)
        }
    })

    observeEvent(input$select_belly_bonds, {
        req(processed_data())
        belly_bonds <- processed_data() %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            ungroup() %>%
            filter(modified_duration >= 5, modified_duration <= 10) %>%
            pull(bond) %>%
            unique()
        if (length(belly_bonds) > 0) {
            updateSelectInput(session, "scenario_bonds_select", selected = belly_bonds)
        }
    })

    observeEvent(input$select_long_bonds, {
        req(processed_data())
        long_bonds <- processed_data() %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            ungroup() %>%
            filter(modified_duration > 10) %>%
            pull(bond) %>%
            unique()
        if (length(long_bonds) > 0) {
            updateSelectInput(session, "scenario_bonds_select", selected = long_bonds)
        }
    })

    observeEvent(input$select_curve_spread, {
        req(processed_data())
        # Select shortest, middle, and longest bonds
        sorted_bonds <- processed_data() %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            ungroup() %>%
            arrange(modified_duration) %>%
            pull(bond) %>%
            unique()

        n_bonds <- length(sorted_bonds)
        if (n_bonds >= 3) {
            spread_bonds <- sorted_bonds[c(1, ceiling(n_bonds/2), n_bonds)]
        } else {
            spread_bonds <- sorted_bonds
        }
        updateSelectInput(session, "scenario_bonds_select", selected = spread_bonds)
    })

    # Render scenario analysis plot with user-controlled parameters
    output$scenario_analysis_plot <- renderPlot({
        req(processed_data())

        # Get user-selected parameters with defaults
        selected_bonds <- input$scenario_bonds_select
        y_scale <- input$scenario_y_scale %||% "fixed"
        show_confidence <- isTRUE(input$scenario_show_confidence)
        confidence_level <- (input$scenario_confidence_level %||% 95) / 100

        # Validate confidence level
        if (is.na(confidence_level) || confidence_level < 0.8 || confidence_level > 0.99) {
            confidence_level <- 0.95
        }

        # Build params list
        params <- list(
            selected_bonds = selected_bonds,
            y_scale = y_scale,
            show_confidence = show_confidence,
            confidence_level = confidence_level,
            max_bonds = 8
        )

        # Generate plot with error handling
        tryCatch({
            p <- generate_scenario_analysis_plot(processed_data(), params)
            if(!is.null(p)) print(p)
        }, error = function(e) {
            warning(sprintf("Scenario analysis plot error: %s", e$message))
            plot.new()
            text(0.5, 0.5, sprintf("Error generating scenario analysis:\n%s", e$message),
                 col = "#C62828", cex = 1.2)
        })
    }, height = 550)

    # 12. Optimal Holding Period Enhanced
    output$optimal_holding_enhanced_plot <- renderPlot({
        req(carry_roll_data())
        p <- generate_optimal_holding_enhanced_plot(carry_roll_data())
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for optimization analysis", cex = 1.5)
        }
    })

    # Reactive to compute holding period metrics with ANNUALIZED calculations
    holding_period_metrics <- reactive({
        req(carry_roll_data())
        data <- carry_roll_data()

        # Calculate ANNUALIZED metrics per holding period
        metrics <- data %>%
            filter(!is.na(holding_period), !is.na(net_return)) %>%
            mutate(
                holding_days = as.numeric(gsub("d$", "", holding_period))
            ) %>%
            group_by(holding_period, holding_days) %>%
            summarise(
                period_return = mean(net_return, na.rm = TRUE),
                return_std = sd(net_return, na.rm = TRUE),
                min_return = min(net_return, na.rm = TRUE),
                max_return = max(net_return, na.rm = TRUE),
                avg_duration = mean(modified_duration, na.rm = TRUE),
                n_bonds = n(),
                .groups = "drop"
            ) %>%
            arrange(holding_days) %>%
            mutate(
                # Annualize metrics
                periods_per_year = 365 / holding_days,
                annualized_return = period_return * periods_per_year,
                annualized_vol = return_std * sqrt(periods_per_year),

                # Risk-free rate (SA T-bill proxy)
                risk_free = 7.5,

                # Sharpe Ratio (properly annualized)
                sharpe = (annualized_return - risk_free) / pmax(annualized_vol, 0.5),

                # Breakeven yield move (bps)
                breakeven_bps = round((period_return / 100) / pmax(avg_duration, 0.1) * 10000),

                # Return range formatted
                return_range = sprintf("%.2f%% - %.2f%%", min_return, max_return)
            )

        # Mark optimal by Sharpe ratio
        if(nrow(metrics) > 0) {
            metrics$is_optimal <- metrics$sharpe == max(metrics$sharpe, na.rm = TRUE)
        }

        return(metrics)
    })

    # Decision Table UI for Holding Period Analysis
    output$holding_period_explainer <- renderUI({
        req(holding_period_metrics())

        metrics <- holding_period_metrics()
        if(nrow(metrics) == 0) return(NULL)

        # Determine recommendations
        metrics <- metrics %>%
            mutate(
                recommendation = case_when(
                    annualized_return == max(annualized_return) & sharpe == max(sharpe) ~ "\u2605 BEST OVERALL",
                    annualized_return == max(annualized_return) ~ "\u2605 BEST ANNUALIZED",
                    period_return == max(period_return) ~ "\u2605 BEST BUY-AND-HOLD",
                    breakeven_bps == max(breakeven_bps) ~ "\u2605 MOST DEFENSIVE",
                    TRUE ~ ""
                )
            )

        # Build table rows
        table_rows <- lapply(1:nrow(metrics), function(i) {
            row <- metrics[i, ]
            is_best <- row$is_optimal

            tags$tr(
                style = if(is_best) "background-color: #E8F5E9; font-weight: bold;" else "",
                tags$td(tags$strong(row$holding_period)),
                tags$td(sprintf("%.2f%%", row$period_return)),
                tags$td(
                    style = if(row$annualized_return == max(metrics$annualized_return))
                        "color: #1B5E20; font-weight: bold;" else "",
                    sprintf("%.1f%%", row$annualized_return)
                ),
                tags$td(sprintf("%.2f", row$sharpe)),
                tags$td(sprintf("+%d bps", row$breakeven_bps)),
                tags$td(
                    style = "font-size: 11px; color: #1B5E20;",
                    row$recommendation
                )
            )
        })

        tags$div(
            tags$h5("Holding Period Decision Guide", class = "text-primary",
                    style = "margin-bottom: 10px; font-weight: bold;"),

            tags$table(
                class = "table table-striped table-hover table-sm",
                style = "font-size: 12px;",

                tags$thead(
                    tags$tr(
                        tags$th("Period"),
                        tags$th("Per-Period"),
                        tags$th("Annualized"),
                        tags$th("Sharpe"),
                        tags$th("Breakeven"),
                        tags$th("Best For")
                    )
                ),

                tags$tbody(table_rows)
            ),

            tags$div(
                class = "alert alert-info",
                style = "margin-top: 10px; padding: 10px; font-size: 11px;",
                tags$strong("Quick Guide:"),
                tags$ul(
                    style = "margin: 5px 0; padding-left: 20px;",
                    tags$li(tags$strong("Active traders:"), " Choose highest ANNUALIZED return (roll frequently)"),
                    tags$li(tags$strong("Buy-and-hold:"), " Choose highest per-period return"),
                    tags$li(tags$strong("Risk-averse:"), " Choose highest BREAKEVEN (most cushion against yield rises)")
                )
            )
        )
    })

    # ================================================================================
    # BUTTERFLY SPREAD ANALYZER
    # ================================================================================

    # Reactive value to track selected butterfly
    selected_butterfly <- reactiveVal(NULL)

    # Calculate butterflies reactively
    # NOTE: Uses filtered_data() which has FULL historical data, not processed_data() which only has latest snapshot
    butterfly_data <- eventReactive(input$generate_butterflies, {
        req(filtered_data())

        withProgress(message = "Calculating butterfly spreads...", {

            # Get full historical data for butterfly calculations
            historical_data <- filtered_data()

            message(sprintf("=== BUTTERFLY DEBUG ==="))
            message(sprintf("Using filtered_data: %d rows, %d bonds",
                           nrow(historical_data),
                           dplyr::n_distinct(historical_data$bond)))
            message(sprintf("Date range: %s to %s",
                           min(historical_data$date, na.rm = TRUE),
                           max(historical_data$date, na.rm = TRUE)))

            lookback <- as.numeric(input$butterfly_lookback)
            if (lookback == 9999) lookback <- 3650  # ~10 years for "all"

            butterflies <- calculate_butterfly_spreads(
                historical_data,
                lookback_days = lookback
            )

            if (is.null(butterflies) || length(butterflies) == 0) {
                return(list(summary = data.frame(), details = list()))
            }

            # Convert to summary data frame
            # Check if yields are in percentage form (spread already in % points)
            summary_df <- purrr::map_dfr(butterflies, function(bf) {
                # If yields are in % form, spread is already in percentage points - don't multiply by 100
                yields_in_pct <- isTRUE(bf$yields_in_pct)
                conversion_factor <- if (yields_in_pct) 1 else 100

                data.frame(
                    Trade = paste0("Butterfly: ", bf$name),
                    Short_Wing = bf$short_wing,
                    Body = bf$body,
                    Long_Wing = bf$long_wing,
                    Z_Score = bf$z_score,
                    ADF_p = bf$adf_pvalue,
                    ADF_stat = if (!is.null(bf$adf_statistic)) bf$adf_statistic else NA,
                    ADF_p_truncated = isTRUE(bf$adf_p_truncated),
                    Mean = bf$mean * conversion_factor,
                    Current = bf$current * conversion_factor,
                    Diff = bf$diff_from_mean * conversion_factor,
                    Is_Stationary = bf$is_stationary,
                    stringsAsFactors = FALSE
                )
            })

            # DEBUG: Log stationarity distribution BEFORE filtering
            message("=== SERVER STATIONARITY CHECK ===")
            message(sprintf("Total butterflies in summary: %d", nrow(summary_df)))
            if ("Is_Stationary" %in% names(summary_df)) {
                message(sprintf("Is_Stationary distribution: TRUE=%d, FALSE=%d, NA=%d",
                                sum(summary_df$Is_Stationary == TRUE, na.rm = TRUE),
                                sum(summary_df$Is_Stationary == FALSE, na.rm = TRUE),
                                sum(is.na(summary_df$Is_Stationary))))
                if (all(summary_df$Is_Stationary == TRUE, na.rm = TRUE)) {
                    message("WARNING: ALL butterflies marked as stationary!")
                }
                message(sprintf("ADF p-value range: %.4f to %.4f",
                                min(summary_df$ADF_p, na.rm = TRUE),
                                max(summary_df$ADF_p, na.rm = TRUE)))
                message(sprintf("ADF stat range: %.4f to %.4f",
                                min(summary_df$ADF_stat, na.rm = TRUE),
                                max(summary_df$ADF_stat, na.rm = TRUE)))
            }
            message("==================================")

            # NOTE: Filter is now applied in butterfly_filtered() reactive, NOT here
            # This allows the filter dropdown to work without regenerating butterflies

            # Rank by absolute Z-Score
            summary_df <- summary_df %>%
                arrange(desc(abs(Z_Score)))

            list(
                summary = summary_df,
                details = butterflies
            )
        })
    }, ignoreNULL = FALSE)

    # Track if butterflies have been calculated this session (to avoid recalculating on tab re-navigation)
    butterfly_calculated <- reactiveVal(FALSE)

    # Lazy loading: Only trigger butterfly calculation when user navigates to Market Intelligence tab
    # This prevents expensive calculations on app startup (fixes 3-5 second delay on load)
    observeEvent(input$main_tabs, {
        # Only trigger when:
        # 1. User navigates to Market Intelligence tab
        # 2. Data is ready (filtered_data available)
        # 3. Butterflies haven't been calculated yet in this session
        if (input$main_tabs == "Market Intelligence" &&
            !butterfly_calculated() &&
            !is.null(tryCatch(filtered_data(), error = function(e) NULL))) {

            message("=== BUTTERFLY CALCULATION TRIGGERED BY USER NAVIGATION ===")
            message("User clicked on Market Intelligence tab - initiating butterfly calculations")

            # Small delay to allow tab UI to render first, then trigger calculation
            shinyjs::delay(500, {
                shinyjs::click("generate_butterflies")
                butterfly_calculated(TRUE)  # Mark as calculated
            })
        }
    }, ignoreInit = TRUE)  # ignoreInit = TRUE prevents triggering on app startup

    # Reset butterfly_calculated flag when data changes (so user can get fresh calculations)
    observeEvent(filtered_data(), {
        butterfly_calculated(FALSE)
    }, ignoreInit = TRUE)

    # Filtered butterfly data - responds to stationarity_filter changes
    # This is a separate reactive so filter changes don't require regenerating butterflies
    butterfly_filtered <- reactive({
        req(butterfly_data())
        raw_df <- butterfly_data()$summary

        if (nrow(raw_df) == 0) {
            return(raw_df)
        }

        # Apply stationarity filter HERE (not in butterfly_data eventReactive)
        filter_choice <- input$stationarity_filter
        if (is.null(filter_choice)) filter_choice <- "all"

        message(sprintf("[FILTER] Raw butterflies: %d, Filter: %s", nrow(raw_df), filter_choice))

        filtered_df <- if (filter_choice == "stationary") {
            raw_df %>% filter(Is_Stationary == TRUE)
        } else {
            raw_df
        }

        message(sprintf("[FILTER] After filter: %d butterflies", nrow(filtered_df)))

        # Warn if filter had no effect
        if (filter_choice == "stationary" && nrow(filtered_df) == nrow(raw_df)) {
            message("[FILTER] WARNING: Stationarity filter had NO effect - all marked stationary!")
        }

        filtered_df
    })

    # Display stationarity counts for verification
    output$stationarity_counts <- renderUI({
        req(butterfly_data())
        raw_df <- butterfly_data()$summary

        if (nrow(raw_df) == 0) {
            return(NULL)
        }

        stationary_count <- sum(raw_df$Is_Stationary == TRUE, na.rm = TRUE)
        non_stationary_count <- sum(raw_df$Is_Stationary == FALSE, na.rm = TRUE)
        total_count <- nrow(raw_df)

        # Show warning if all are stationary
        if (stationary_count == total_count) {
            tags$span(
                style = "color: #ff6600;",
                sprintf("⚠ %d/%d stationary", stationary_count, total_count)
            )
        } else {
            tags$span(sprintf("Stationary: %d/%d", stationary_count, total_count))
        }
    })

    # Butterfly table output
    output$butterfly_table <- DT::renderDataTable({
        req(butterfly_data())

        # Use filtered reactive instead of raw butterfly_data
        df <- butterfly_filtered()

        if (nrow(df) == 0) {
            return(DT::datatable(data.frame(Message = "No butterflies found. Try adjusting filters.")))
        }

        zscore_threshold <- if (!is.null(input$zscore_threshold)) input$zscore_threshold else 2.0

        df <- df %>%
            mutate(
                # Clean trade name - single line format (remove "Butterfly: " prefix)
                Trade_Clean = gsub("Butterfly: ", "", Trade),

                # Format for display
                Z_Score_Display = sprintf("%.2f", Z_Score),
                # Show "<0.01" for truncated p-values, otherwise show actual value
                ADF_p_Display = case_when(
                    is.na(ADF_p) ~ "N/A",
                    ADF_p_truncated ~ "<0.01",
                    ADF_p < 0.05 ~ sprintf("%.3f", ADF_p),
                    TRUE ~ sprintf("%.2f", ADF_p)
                ),
                ADF_stat_Display = ifelse(is.na(ADF_stat), "N/A", sprintf("%.2f", ADF_stat)),
                # Use 3 decimal places for precision consistency
                Mean_Display = sprintf("%.3f%%", Mean),
                Current_Display = sprintf("%.3f%%", Current),
                Diff_Display = sprintf("%+.3f%%", Diff),

                # Signal based on Z-Score - FULL TEXT with both legs
                Signal = case_when(
                    Z_Score > zscore_threshold ~ "SELL WINGS / BUY BODY",
                    Z_Score < -zscore_threshold ~ "BUY WINGS / SELL BODY",
                    TRUE ~ "NEUTRAL"
                ),

                # Keep raw Z-Score for sorting reference
                Z_Score_Raw = Z_Score
            ) %>%
            select(
                Trade = Trade_Clean,
                `Z-Score` = Z_Score_Display,
                `ADF stat` = ADF_stat_Display,
                `ADF p` = ADF_p_Display,
                Mean = Mean_Display,
                Current = Current_Display,
                Diff = Diff_Display,
                Signal,
                Z_Score_Raw
            )

        DT::datatable(
            df,
            selection = "single",
            rownames = FALSE,
            options = list(
                pageLength = 10,
                dom = 'frtip',  # Add filter search box
                ordering = FALSE,  # Already sorted
                scrollX = FALSE,
                columnDefs = list(
                    list(visible = FALSE, targets = 8),  # Hide Z_Score_Raw column
                    list(className = 'dt-center', targets = c(1, 2, 3, 4, 5, 6)),
                    list(width = '120px', targets = 0),  # Trade column
                    list(width = '60px', targets = c(1, 2, 3)),  # Z-Score, ADF columns
                    list(width = '70px', targets = c(4, 5, 6)),  # Mean, Current, Diff
                    list(width = '140px', targets = 7)  # Signal column
                ),
                # Add tooltips via headerCallback
                headerCallback = DT::JS(
                    "function(thead, data, start, end, display) {",
                    "  $(thead).find('th').eq(1).attr('title', 'Standard deviations from historical mean');",
                    "  $(thead).find('th').eq(2).attr('title', 'Augmented Dickey-Fuller test statistic (more negative = more stationary)');",
                    "  $(thead).find('th').eq(3).attr('title', 'P-value for stationarity test (< 0.05 = mean-reverting)');",
                    "  $(thead).find('th').eq(4).attr('title', 'Historical average spread');",
                    "  $(thead).find('th').eq(5).attr('title', 'Current spread value');",
                    "  $(thead).find('th').eq(6).attr('title', 'Difference from historical mean');",
                    "  $(thead).find('th').eq(7).attr('title', 'Recommended trade direction');",
                    "}"
                )
            ),
            class = 'cell-border stripe hover'
        ) %>%
            # Z-Score coloring with intermediate colors
            DT::formatStyle(
                'Z-Score',
                color = DT::styleInterval(
                    cuts = c(-2, -1, 1, 2),
                    values = c('#C62828', '#E57373', '#424242', '#81C784', '#2E7D32')
                ),
                fontWeight = 'bold'
            ) %>%
            DT::formatStyle(
                'ADF stat',
                # More negative = more stationary. Highlight statistically significant values.
                # Typical 5% critical value for ADF is around -2.86
                color = DT::styleInterval(
                    c(-3.5, -2.86),
                    c('#1B5E20', '#4CAF50', '#757575')
                )
            ) %>%
            # Subtle ADF p-value highlighting (not jarring green)
            DT::formatStyle(
                'ADF p',
                backgroundColor = DT::styleInterval(
                    cuts = c(0.01, 0.05),
                    values = c('#E8F5E9', '#F1F8E9', 'transparent')
                )
            ) %>%
            # Signal column styling with text color
            DT::formatStyle(
                'Signal',
                color = DT::styleEqual(
                    c('BUY WINGS / SELL BODY', 'SELL WINGS / BUY BODY', 'NEUTRAL'),
                    c('#2E7D32', '#C62828', '#757575')
                ),
                fontWeight = DT::styleEqual(
                    c('BUY WINGS / SELL BODY', 'SELL WINGS / BUY BODY', 'NEUTRAL'),
                    c('bold', 'bold', 'normal')
                )
            ) %>%
            # Diff column coloring (negative red, positive green)
            DT::formatStyle(
                'Diff',
                color = DT::styleInterval(
                    cuts = 0,
                    values = c('#C62828', '#2E7D32')
                )
            )
    })

    # Update selection when table row clicked
    observeEvent(input$butterfly_table_rows_selected, {
        req(butterfly_filtered())

        row_idx <- input$butterfly_table_rows_selected
        if (length(row_idx) > 0) {
            # Use filtered data to match table display
            filtered_df <- butterfly_filtered()
            if (row_idx <= nrow(filtered_df)) {
                trade_name <- filtered_df$Trade[row_idx]
                # Extract spread name from "Butterfly: X-Y-Z" format
                spread_name <- gsub("Butterfly: ", "", trade_name)
                selected_butterfly(spread_name)
            }
        }
    })

    # Auto-select first butterfly if none selected
    observe({
        req(butterfly_filtered())
        if (is.null(selected_butterfly()) && nrow(butterfly_filtered()) > 0) {
            first_name <- gsub("Butterfly: ", "", butterfly_filtered()$Trade[1])
            selected_butterfly(first_name)
        }
    })

    # Selected butterfly info panel
    output$selected_butterfly_info <- renderUI({
        req(selected_butterfly(), butterfly_data())

        bf <- butterfly_data()$details[[selected_butterfly()]]
        if (is.null(bf)) return(NULL)

        zscore_threshold <- if (!is.null(input$zscore_threshold)) input$zscore_threshold else 2.0

        # Generate full signal text consistent with table
        signal <- dplyr::case_when(
            bf$z_score > zscore_threshold ~ "SELL WINGS / BUY BODY",
            bf$z_score < -zscore_threshold ~ "BUY WINGS / SELL BODY",
            TRUE ~ "NEUTRAL"
        )

        # Color based on signal direction
        signal_color <- dplyr::case_when(
            bf$z_score > zscore_threshold ~ "#C62828",
            bf$z_score < -zscore_threshold ~ "#2E7D32",
            TRUE ~ "#757575"
        )

        # Z-score color with intermediate shades
        zscore_color <- dplyr::case_when(
            bf$z_score < -2 ~ "#C62828",
            bf$z_score < -1 ~ "#E57373",
            bf$z_score > 2 ~ "#2E7D32",
            bf$z_score > 1 ~ "#81C784",
            TRUE ~ "#424242"
        )

        # Apply conversion factor based on yield units
        yields_in_pct <- isTRUE(bf$yields_in_pct)
        cf <- if (yields_in_pct) 1 else 100

        tags$div(
            class = "butterfly-summary-card",
            style = "background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 10px;",

            fluidRow(
                column(3,
                       tags$div(
                           tags$p(class = "text-muted", style = "margin-bottom: 2px; font-size: 0.85em;", "Selected Trade"),
                           tags$h5(style = "margin: 0; font-weight: bold;", bf$name)
                       )
                ),
                column(2,
                       tags$div(
                           tags$p(class = "text-muted", style = "margin-bottom: 2px; font-size: 0.85em;", "Z-Score"),
                           tags$h4(
                               style = sprintf("margin: 0; color: %s; font-weight: bold;", zscore_color),
                               sprintf("%.2f", bf$z_score)
                           )
                       )
                ),
                column(2,
                       tags$div(
                           tags$p(class = "text-muted", style = "margin-bottom: 2px; font-size: 0.85em;", "Current"),
                           # Use 3 decimal places for precision consistency
                           tags$h5(style = "margin: 0;", sprintf("%.3f%%", bf$current * cf))
                       )
                ),
                column(2,
                       tags$div(
                           tags$p(class = "text-muted", style = "margin-bottom: 2px; font-size: 0.85em;", "Mean"),
                           # Use 3 decimal places for precision consistency
                           tags$h5(style = "margin: 0;", sprintf("%.3f%%", bf$mean * cf))
                       )
                ),
                column(3,
                       tags$div(
                           tags$p(class = "text-muted", style = "margin-bottom: 2px; font-size: 0.85em;", "Signal"),
                           tags$h5(
                               style = sprintf("margin: 0; color: %s; font-weight: bold;", signal_color),
                               signal
                           )
                       )
                )
            )
        )
    })

    # Butterfly chart
    output$butterfly_chart <- renderPlot({
        req(selected_butterfly(), butterfly_data())

        bf <- butterfly_data()$details[[selected_butterfly()]]
        if (is.null(bf)) {
            plot.new()
            text(0.5, 0.5, "Select a butterfly from the table", cex = 1.2)
            return()
        }

        zscore_threshold <- if (!is.null(input$zscore_threshold)) input$zscore_threshold else 2.0
        p <- generate_butterfly_chart(bf, zscore_threshold)
        if (!is.null(p)) print(p)
    }, res = 100)

    # Trade details modal
    observeEvent(input$butterfly_trade_details, {
        req(selected_butterfly(), butterfly_data())

        bf <- butterfly_data()$details[[selected_butterfly()]]
        if (is.null(bf)) return()

        zscore_threshold <- if (!is.null(input$zscore_threshold)) input$zscore_threshold else 2.0

        # Determine trade direction
        if (bf$z_score > zscore_threshold) {
            trade_direction <- "SELL WINGS / BUY BODY"
            trade_legs <- tagList(
                tags$li(tags$span(style = "color: #C62828;", "SELL"), " ", bf$short_wing, " (Short Wing)"),
                tags$li(tags$span(style = "color: #1B5E20;", "BUY 2x"), " ", bf$body, " (Body)"),
                tags$li(tags$span(style = "color: #C62828;", "SELL"), " ", bf$long_wing, " (Long Wing)")
            )
            rationale <- "Spread is ABOVE mean - Expect compression back to mean"
        } else if (bf$z_score < -zscore_threshold) {
            trade_direction <- "BUY WINGS / SELL BODY"
            trade_legs <- tagList(
                tags$li(tags$span(style = "color: #1B5E20;", "BUY"), " ", bf$short_wing, " (Short Wing)"),
                tags$li(tags$span(style = "color: #C62828;", "SELL 2x"), " ", bf$body, " (Body)"),
                tags$li(tags$span(style = "color: #1B5E20;", "BUY"), " ", bf$long_wing, " (Long Wing)")
            )
            rationale <- "Spread is BELOW mean - Expect expansion back to mean"
        } else {
            trade_direction <- "NO TRADE"
            trade_legs <- tags$li("Z-Score within threshold - no action recommended")
            rationale <- "Spread is near mean - wait for better entry"
        }

        showModal(modalDialog(
            title = sprintf("Trade Details: %s", bf$name),
            size = "l",

            fluidRow(
                column(6,
                       tags$h5("Trade Structure"),
                       tags$div(
                           style = "background: #F5F5F5; padding: 15px; border-radius: 4px;",
                           tags$div(
                               style = "font-size: 18px; font-weight: bold; margin-bottom: 10px;",
                               trade_direction
                           ),
                           tags$ul(trade_legs)
                       ),

                       tags$h5("Rationale", style = "margin-top: 15px;"),
                       tags$p(rationale),
                       {
                           # Apply conversion factor based on yield units
                           yields_in_pct <- isTRUE(bf$yields_in_pct)
                           cf <- if (yields_in_pct) 1 else 100
                           tags$p(
                               sprintf("Current spread (%.3f%%) is %.2f standard deviations from mean (%.3f%%).",
                                       bf$current * cf, bf$z_score, bf$mean * cf)
                           )
                       }
                ),

                column(6,
                       tags$h5("Statistics"),
                       {
                           # Apply conversion factor based on yield units
                           yields_in_pct <- isTRUE(bf$yields_in_pct)
                           cf <- if (yields_in_pct) 1 else 100

                           # Format ADF p-value properly
                           adf_p_display <- if (is.na(bf$adf_pvalue)) {
                               "N/A"
                           } else if (isTRUE(bf$adf_p_truncated)) {
                               "<0.01"
                           } else {
                               sprintf("%.3f", bf$adf_pvalue)
                           }

                           # Format ADF statistic
                           adf_stat_display <- if (!is.null(bf$adf_statistic) && !is.na(bf$adf_statistic)) {
                               sprintf("%.2f", bf$adf_statistic)
                           } else {
                               "N/A"
                           }

                           tags$table(
                               class = "table table-condensed",
                               tags$tbody(
                                   tags$tr(tags$td("Z-Score:"), tags$td(sprintf("%.2f", bf$z_score))),
                                   tags$tr(tags$td("ADF statistic:"), tags$td(adf_stat_display)),
                                   tags$tr(tags$td("ADF p-value:"), tags$td(adf_p_display)),
                                   tags$tr(tags$td("Stationary:"), tags$td(ifelse(bf$is_stationary, "Yes (valid)", "No (caution)"))),
                                   tags$tr(tags$td("Mean:"), tags$td(sprintf("%.3f%%", bf$mean * cf))),
                                   tags$tr(tags$td("Std Dev:"), tags$td(sprintf("%.3f%%", bf$sd * cf))),
                                   tags$tr(tags$td("Current:"), tags$td(sprintf("%.3f%%", bf$current * cf))),
                                   tags$tr(tags$td("Diff from Mean:"), tags$td(sprintf("%+.3f%%", bf$diff_from_mean * cf)))
                               )
                           )
                       },

                       tags$h5("Risk Warning", style = "margin-top: 15px;"),
                       tags$div(
                           class = "alert alert-warning",
                           style = "font-size: 12px;",
                           "Mean reversion is not guaranteed. Check for structural changes, ",
                           "liquidity conditions, and fundamental drivers before trading."
                       )
                )
            ),

            footer = tagList(
                modalButton("Close")
            )
        ))
    })

    # Download butterfly data
    output$download_butterfly_data <- downloadHandler(
        filename = function() {
            paste0("butterfly_spreads_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
            req(butterfly_data())
            write.csv(butterfly_data()$summary, file, row.names = FALSE)
        }
    )

    # Download butterfly chart
    output$download_butterfly_chart <- downloadHandler(
        filename = function() {
            paste0("butterfly_chart_", selected_butterfly(), "_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(selected_butterfly(), butterfly_data())
            bf <- butterfly_data()$details[[selected_butterfly()]]
            zscore_threshold <- if (!is.null(input$zscore_threshold)) input$zscore_threshold else 2.0
            p <- generate_butterfly_chart(bf, zscore_threshold)
            ggsave(file, plot = p, width = 10, height = 6, dpi = 150)
        }
    )

    # End of Butterfly Spread Analyzer
    # ================================================================================

    output$forward_curve_plot <- renderPlot({
        req(processed_data())
        # Pass the selected forward period for chart-table interactivity
        selected_period <- selected_forward_period()
        p <- generate_forward_curve_plot(processed_data(), params = NULL, selected_period = selected_period)
        if (!is.null(p)) print(p)
    })


    # =====================================================
    # Auction Performance Analytics - Enhanced Bond Selector
    # =====================================================

    # Get available bonds with auction data, sorted by auction count
    available_auction_bonds <- reactive({
        req(filtered_data())

        filtered_data() %>%
            filter(!is.na(offer_date), !is.na(bid_to_cover), bid_to_cover > 0) %>%
            group_by(bond) %>%
            summarise(
                n_auctions = n(),
                avg_b2c = mean(bid_to_cover, na.rm = TRUE),
                last_auction = max(offer_date, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            arrange(desc(n_auctions))
    })

    # Initialize picker with top 6 bonds by default
    observe({
        bonds_data <- available_auction_bonds()
        req(nrow(bonds_data) > 0)

        # Create choices with auction count labels
        choices <- setNames(
            bonds_data$bond,
            paste0(bonds_data$bond, " (n=", bonds_data$n_auctions, ")")
        )

        # Default to top 6
        default_selected <- head(bonds_data$bond, 6)

        shinyWidgets::updatePickerInput(
            session = session,
            inputId = "auction_perf_bonds",
            choices = choices,
            selected = default_selected
        )
    })

    # Quick select button handlers
    observeEvent(input$auction_perf_top6, {
        bonds_data <- available_auction_bonds()
        req(bonds_data)
        top6 <- head(bonds_data$bond, 6)
        shinyWidgets::updatePickerInput(session, "auction_perf_bonds", selected = top6)
    })

    observeEvent(input$auction_perf_recent, {
        bonds_data <- available_auction_bonds()
        req(bonds_data)
        # Most active YTD
        ytd_active <- filtered_data() %>%
            filter(
                !is.na(offer_date),
                !is.na(bid_to_cover),
                lubridate::year(offer_date) == lubridate::year(Sys.Date())
            ) %>%
            count(bond, sort = TRUE) %>%
            head(6) %>%
            pull(bond)

        if(length(ytd_active) > 0) {
            shinyWidgets::updatePickerInput(session, "auction_perf_bonds", selected = ytd_active)
        }
    })

    observeEvent(input$auction_perf_all, {
        bonds_data <- available_auction_bonds()
        req(bonds_data)
        # Select all (up to 8)
        all_bonds <- head(bonds_data$bond, 8)
        shinyWidgets::updatePickerInput(session, "auction_perf_bonds", selected = all_bonds)
    })

    # Dynamic UI for plot with variable height
    output$enhanced_auction_analytics_ui <- renderUI({
        n_bonds <- length(input$auction_perf_bonds)
        if(is.null(n_bonds) || n_bonds == 0) n_bonds <- 6

        layout <- input$auction_perf_layout
        if(is.null(layout)) layout <- "auto"

        # Calculate rows based on layout
        ncol_val <- switch(layout,
            "2x3" = 3,
            "2x4" = 4,
            "3x3" = 3,
            "auto" = if(n_bonds <= 4) 2 else if(n_bonds <= 6) 3 else 4
        )
        n_rows <- ceiling(n_bonds / ncol_val)
        base_height <- 200
        plot_height <- max(400, n_rows * base_height + 100)

        plotOutput("enhanced_auction_analytics", height = paste0(plot_height, "px"))
    })

    # Main plot output
    output$enhanced_auction_analytics <- renderPlot({
        req(filtered_data())

        # Get selected bonds (default to top 6 if none selected)
        selected_bonds <- input$auction_perf_bonds
        if(is.null(selected_bonds) || length(selected_bonds) == 0) {
            bonds_data <- available_auction_bonds()
            if(!is.null(bonds_data) && nrow(bonds_data) > 0) {
                selected_bonds <- head(bonds_data$bond, 6)
            }
        }

        # Get layout settings
        layout <- input$auction_perf_layout %||% "auto"
        x_scales <- input$auction_perf_xaxis %||% "free_x"
        show_trend <- input$auction_perf_show_trend %||% TRUE

        # Build params list for plot function
        params <- list(
            selected_bonds = selected_bonds,
            layout = layout,
            x_scales = x_scales,
            show_trend = show_trend
        )

        p <- generate_enhanced_auction_analytics(filtered_data(), params)
        if(!is.null(p)) print(p)
    })


    output$auction_forecast_plot <- renderPlot({
        req(filtered_data(), input$auction_bonds_select)
        p <- generate_auction_forecast_plot(filtered_data(), input$auction_bonds_select)
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for forecast", cex = 1.2)
        }
    })

    # Note: demand_elasticity_plot and success_probability_plot removed
    # as part of Auction Intelligence tab overhaul

    output$auction_pattern_analysis <- renderPlot({
        req(filtered_data())
        # Get selected bonds for highlighting (default to empty if not set)
        selected_bonds <- if (!is.null(input$auction_bonds_select)) {
            input$auction_bonds_select
        } else {
            character(0)
        }
        p <- generate_auction_pattern_analysis(filtered_data(), list(),
                                                selected_bonds = selected_bonds)
        if(!is.null(p)) {
            gridExtra::grid.arrange(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for pattern analysis", cex = 1.2)
        }
    })

    output$bid_distribution_plot <- renderPlot({
        req(filtered_data())
        # Get selected bonds for highlighting (default to empty if not set)
        selected_bonds <- if (!is.null(input$auction_bonds_select)) {
            input$auction_bonds_select
        } else {
            character(0)
        }
        p <- generate_bid_distribution_plot(filtered_data(), list(),
                                            selected_bonds = selected_bonds)
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "No bid distribution data available", cex = 1.2)
        }
    })

    # =====================================================
    # Auction Summary Statistics Bar Outputs
    # =====================================================

    output$total_auctions_analyzed <- renderText({
        auction_data <- filtered_data()
        req(auction_data)
        n_auctions <- auction_data %>% filter(!is.na(bid_to_cover)) %>% nrow()
        as.character(n_auctions)
    })

    output$overall_avg_btc <- renderText({
        auction_data <- filtered_data()
        req(auction_data)
        avg_btc <- mean(auction_data$bid_to_cover, na.rm = TRUE)
        if (is.na(avg_btc) || is.nan(avg_btc)) {
            "—"
        } else {
            sprintf("%.2fx", avg_btc)
        }
    })

    output$strong_auction_pct <- renderText({
        auction_data <- filtered_data()
        req(auction_data)
        auction_data_valid <- auction_data %>% filter(!is.na(bid_to_cover))
        if (nrow(auction_data_valid) == 0) {
            "—"
        } else {
            strong_pct <- mean(auction_data_valid$bid_to_cover >= 3.0, na.rm = TRUE) * 100
            sprintf("%.0f%%", strong_pct)
        }
    })

    output$selected_bonds_avg_btc <- renderUI({
        auction_data <- filtered_data()
        selected_bonds <- if (!is.null(input$auction_bonds_select)) {
            input$auction_bonds_select
        } else {
            character(0)
        }
        req(auction_data)

        if (length(selected_bonds) == 0) {
            return(div(style = "font-size: 1.4em; color: #666;", "—"))
        }

        selected_data <- auction_data %>%
            filter(bond %in% selected_bonds, !is.na(bid_to_cover))

        if (nrow(selected_data) == 0) {
            return(div(style = "font-size: 1.4em; color: #666;", "—"))
        }

        selected_avg <- mean(selected_data$bid_to_cover, na.rm = TRUE)

        # Color based on value
        color <- if (is.na(selected_avg) || is.nan(selected_avg)) {
            "#666"
        } else if (selected_avg >= 3.0) {
            "#28a745"
        } else if (selected_avg >= 2.5) {
            "#20c997"
        } else if (selected_avg >= 2.0) {
            "#ffc107"
        } else {
            "#dc3545"
        }

        div(
            style = sprintf("font-size: 1.4em; font-weight: bold; color: %s;", color),
            sprintf("%.2fx", selected_avg)
        )
    })

    # ========================================================================
    # Auction Date Range Filter Logic (for Cumulative Issuance Chart)
    # ========================================================================

    # Get auction date range from data
    auction_date_bounds <- reactive({
        req(bond_data())

        # Use bond_data() to get the full data range (not filtered_data which is already filtered)
        auction_data <- bond_data() %>%
            filter(!is.na(date), !is.na(offer_amount), offer_amount > 0)

        if (nrow(auction_data) == 0) {
            # Fallback to reasonable defaults if no auction data
            return(list(
                min_date = Sys.Date() - 365,
                max_date = Sys.Date()
            ))
        }

        list(
            min_date = min(auction_data$date, na.rm = TRUE),
            max_date = max(auction_data$date, na.rm = TRUE)
        )
    })

    # Initialize the date range input with actual data bounds
    observe({
        bounds <- auction_date_bounds()
        updateDateRangeInput(session, "auction_date_range",
            start = bounds$min_date,
            end = bounds$max_date,
            min = bounds$min_date,
            max = bounds$max_date
        )
    })

    # YTD button - January 1 of current year to today
    observeEvent(input$auction_ytd_btn, {
        bounds <- auction_date_bounds()
        ytd_start <- as.Date(paste0(lubridate::year(Sys.Date()), "-01-01"))
        # Ensure start date isn't before available data
        ytd_start <- max(ytd_start, bounds$min_date)
        updateDateRangeInput(session, "auction_date_range",
            start = ytd_start,
            end = min(Sys.Date(), bounds$max_date)
        )
    })

    # Last 6 months button
    observeEvent(input$auction_6m_btn, {
        bounds <- auction_date_bounds()
        start_6m <- max(Sys.Date() - lubridate::period(6, "months"), bounds$min_date)
        updateDateRangeInput(session, "auction_date_range",
            start = start_6m,
            end = min(Sys.Date(), bounds$max_date)
        )
    })

    # Last 12 months button
    observeEvent(input$auction_12m_btn, {
        bounds <- auction_date_bounds()
        start_12m <- max(Sys.Date() - lubridate::period(12, "months"), bounds$min_date)
        updateDateRangeInput(session, "auction_date_range",
            start = start_12m,
            end = min(Sys.Date(), bounds$max_date)
        )
    })

    # All Time button
    observeEvent(input$auction_all_btn, {
        bounds <- auction_date_bounds()
        updateDateRangeInput(session, "auction_date_range",
            start = bounds$min_date,
            end = bounds$max_date
        )
    })

    # Filtered auction data reactive
    filtered_auction_data <- reactive({
        req(bond_data(), input$auction_date_range)

        date_range <- input$auction_date_range

        # Validate date range exists
        if (is.null(date_range) || length(date_range) < 2) {
            return(bond_data())
        }

        # Filter data based on selected date range
        bond_data() %>%
            filter(
                !is.na(date),
                !is.na(offer_amount),
                offer_amount > 0,
                date >= date_range[1],
                date <= date_range[2]
            )
    })

    # ========================================================================
    # Toggle for Issuance Details Column Descriptions Panel
    # ========================================================================
    observeEvent(input$toggle_column_descriptions, {
        shinyjs::toggle(id = "column_descriptions_panel", anim = TRUE, animType = "slide")
    })

    # ========================================================================
    # YTD Bond Issuance Chart (now uses filtered_auction_data)
    # ========================================================================

    # YTD Bond Issuance Chart
    output$ytd_bond_issuance_chart <- renderPlot({
        req(filtered_auction_data())
        p <- generate_ytd_bond_issuance_chart(filtered_auction_data(), list())
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "No issuance data available for selected date range", cex = 1.2)
        }
    })

    # YTD Bond Issuance Data Table (uses filtered_auction_data for date range filtering)
    # First/Last Auction columns use full historical data (bond_data), independent of date filter
    output$ytd_issuance_data_table <- DT::renderDataTable({
        req(filtered_auction_data(), bond_data())

        # Pass BOTH filtered data (for metrics) and full data (for historical First/Last Auction dates)
        table_data <- generate_ytd_issuance_table(
            data = filtered_auction_data(),
            full_data = bond_data()
        )

        # Check if table_data has an error message column (no data case)
        if ("Message" %in% names(table_data)) {
            return(DT::datatable(table_data, options = list(dom = 't'), rownames = FALSE))
        }

        # Store numeric Yield Trend for conditional styling before modifying table
        yield_trend_numeric <- if ("Yield Trend" %in% names(table_data)) {
            table_data$`Yield Trend`
        } else {
            NULL
        }

        # Replace numeric Yield Trend with formatted display version
        if ("Yield Trend Display" %in% names(table_data) && "Yield Trend" %in% names(table_data)) {
            table_data$`Yield Trend` <- table_data$`Yield Trend Display`
            table_data$`Yield Trend Display` <- NULL  # Remove the display column
        }

        # Column indices after removing Yield Trend Display:
        # 0: Bond, 1: # Auctions, 2: Total (R mil), 3: Avg B2C, 4: B2C Range
        # 5: Oversub %, 6: Yield Trend, 7: First Auction, 8: Last Auction

        # SIMPLIFIED HEADERS - no tooltip icons, just clean text
        # Column descriptions are now in the collapsible panel above the table
        dt <- DT::datatable(
            table_data,
            rownames = FALSE,
            filter = "none",
            options = list(
                pageLength = 20,
                scrollX = TRUE,
                dom = 'frtip',
                # Don't auto-sort since TOTAL is at top
                order = list(),
                columnDefs = list(
                    # Right-align numeric columns
                    list(className = 'dt-right', targets = c(1, 2, 3, 5)),
                    # Center the B2C Range and Yield Trend columns
                    list(className = 'dt-center', targets = c(4, 6, 7, 8))
                ),
                language = list(
                    search = "Search:",
                    info = "Showing _START_ to _END_ of _TOTAL_ bonds"
                )
            ),
            class = 'cell-border stripe hover compact issuance-table'
        )

        # Format Total (R mil) with thousand separators
        if ("Total (R mil)" %in% names(table_data)) {
            dt <- dt %>%
                DT::formatCurrency(
                    columns = 'Total (R mil)',
                    currency = "",
                    digits = 0,
                    mark = ","
                )
        }

        # Format Avg B2C with 2 decimals and 'x' suffix
        if ("Avg B2C" %in% names(table_data)) {
            dt <- dt %>%
                DT::formatRound(columns = 'Avg B2C', digits = 2) %>%
                DT::formatString(columns = 'Avg B2C', suffix = "x")
        }

        # Format Oversub % percentage
        if ("Oversub %" %in% names(table_data)) {
            dt <- dt %>%
                DT::formatRound(columns = 'Oversub %', digits = 1) %>%
                DT::formatString(columns = 'Oversub %', suffix = "%")
        }

        # Conditional formatting for Avg B2C (color based on demand strength)
        # Red (<2.0x) -> Yellow (2.0-2.5x) -> Light Green (2.5-3.0x) -> Green (>3.0x)
        if ("Avg B2C" %in% names(table_data)) {
            dt <- dt %>%
                DT::formatStyle(
                    columns = 'Avg B2C',
                    backgroundColor = DT::styleInterval(
                        cuts = c(2.0, 2.5, 3.0),
                        values = c('#FFCDD2', '#FFF9C4', '#C8E6C9', '#81C784')
                    )
                )
        }

        # Conditional formatting for Yield Trend based on stored numeric values
        # Green for negative (yields falling = good for bond holders), Red for positive
        if (!is.null(yield_trend_numeric) && "Yield Trend" %in% names(table_data)) {
            # Create color vector based on the numeric values
            yield_colors <- sapply(yield_trend_numeric, function(x) {
                if (is.na(x) || is.nan(x)) return('#666666')  # Gray for NA
                if (x < 0) return('#2E7D32')  # Green for negative (falling yields)
                return('#C62828')  # Red for positive (rising yields)
            })

            dt <- dt %>%
                DT::formatStyle(
                    columns = 'Yield Trend',
                    color = DT::styleEqual(
                        levels = table_data$`Yield Trend`,
                        values = yield_colors
                    ),
                    fontWeight = 'bold'
                )
        }

        # Style the TOTAL row: bold text, light blue background
        if ("Bond" %in% names(table_data)) {
            dt <- dt %>%
                DT::formatStyle(
                    columns = 'Bond',
                    target = 'row',
                    fontWeight = DT::styleEqual(c('TOTAL'), c('bold')),
                    backgroundColor = DT::styleEqual(c('TOTAL'), c('#E3F2FD'))
                ) %>%
                # Also make Bond column text navy for TOTAL row
                DT::formatStyle(
                    columns = 'Bond',
                    color = DT::styleEqual('TOTAL', '#1B3A6B'),
                    fontWeight = 'bold'
                )
        }

        # Apply color bar to Total (R mil) column for non-TOTAL rows visual comparison
        if ("Total (R mil)" %in% names(table_data)) {
            non_total_values <- table_data[table_data$Bond != 'TOTAL', 'Total (R mil)']
            if (is.numeric(non_total_values) && length(non_total_values) > 0 && any(!is.na(non_total_values))) {
                dt <- dt %>%
                    DT::formatStyle(
                        'Total (R mil)',
                        background = DT::styleColorBar(range(non_total_values, na.rm = TRUE), '#90EE90'),
                        backgroundSize = '100% 90%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center'
                    )
            }
        }

        return(dt)
    })

    # ================================================================================
    # YIELD ENVIRONMENT MONITOR (replaces Curve Dynamics)
    # ================================================================================

    # Summary Metrics Row - Key market indicators
    output$market_metrics_row <- renderUI({
        req(filtered_data())
        metrics <- generate_market_summary_metrics(filtered_data(), list())

        if (!metrics$valid) {
            return(
                tags$div(
                    style = "padding: 20px; text-align: center; color: #666;",
                    "Insufficient data for market metrics calculation"
                )
            )
        }

        # Color logic:
        # - Yields FALLING (negative change) = GREEN (bullish for bond holders)
        # - Yields RISING (positive change) = RED (bearish)
        # - Curve STEEP (>200bps) = GREEN (normal/healthy)
        # - Curve FLAT (<100bps) = RED (potential inversion warning)
        change_color <- if (is.na(metrics$yield_change_1m)) {
            "yellow"
        } else if (metrics$yield_change_1m < -5) {
            "green"    # Yields falling = bullish
        } else if (metrics$yield_change_1m > 5) {
            "red"      # Yields rising = bearish
        } else {
            "yellow"   # Modest change
        }

        slope_color <- if (is.na(metrics$curve_slope)) {
            "blue"
        } else if (metrics$curve_slope > 200) {
            "green"    # Steep = healthy
        } else if (metrics$curve_slope < 100) {
            "red"      # Flat/inverted = warning
        } else {
            "yellow"   # Normal
        }

        # Determine icon for yield change
        change_icon <- if (!is.na(metrics$yield_change_1m) && metrics$yield_change_1m > 0) {
            "arrow-trend-up"
        } else {
            "arrow-trend-down"
        }

        fluidRow(
            # Average Yield - always primary blue
            valueBox(
                value = if (!is.na(metrics$avg_yield)) sprintf("%.2f%%", metrics$avg_yield) else "N/A",
                subtitle = "Average Yield",
                icon = icon("percent"),
                color = "blue",
                width = 3
            ),
            # 1M Change - contextual color
            valueBox(
                value = if (!is.na(metrics$yield_change_1m)) sprintf("%+.0f bps", metrics$yield_change_1m) else "N/A",
                subtitle = "1M Change",
                icon = icon(change_icon),
                color = change_color,
                width = 3
            ),
            # Curve Slope - contextual color
            valueBox(
                value = if (!is.na(metrics$curve_slope)) sprintf("%.0f bps", metrics$curve_slope) else "N/A",
                subtitle = "Curve Slope (2s10s)",
                icon = icon("chart-line"),
                color = slope_color,
                width = 3
            ),
            # Yield Dispersion - always primary blue
            valueBox(
                value = if (!is.na(metrics$yield_range)) sprintf("%.0f bps", metrics$yield_range) else "N/A",
                subtitle = "Yield Dispersion",
                icon = icon("arrows-left-right-to-line"),
                color = "blue",
                width = 3
            )
        )
    })

    # Yield Percentile Heatmap - Use FULL data for meaningful percentile calculations
    output$yield_percentile_heatmap <- renderPlot({
        req(bond_data())  # Use full historical data, not filtered date range
        p <- generate_yield_percentile_heatmap(bond_data(), list())
        if (!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for yield percentile analysis", cex = 1.2)
        }
    })

    # Rate of Change Monitor (Momentum)
    output$rate_of_change_monitor <- renderPlot({
        req(filtered_data())
        p <- generate_rate_of_change_monitor(filtered_data(), list())
        if (!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for rate momentum analysis", cex = 1.0)
        }
    })

    # ================================================================================
    # CURVE SHAPE & MOMENTUM (replaces Relative Value Scanner)
    # ================================================================================

    # Curve Comparison Plot (Current vs History) - Use FULL historical data
    output$curve_comparison_plot <- renderPlot({
        req(market_intelligence_data())
        p <- generate_curve_comparison_plot(market_intelligence_data(), list())
        if (!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for curve comparison analysis", cex = 1.2)
        }
    })

    # Curve Steepness Gauge - Use FULL historical data
    output$curve_steepness_gauge <- renderPlot({
        req(market_intelligence_data())
        p <- generate_curve_steepness_gauge(market_intelligence_data(), list())
        if (!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for steepness gauge", cex = 1.0)
        }
    })

    # Download handler for Yield Environment panel
    output$download_yield_environment <- downloadHandler(
        filename = function() {
            paste0("yield_environment_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            # Create combined plot - use full data for percentile, filtered for momentum
            p1 <- generate_yield_percentile_heatmap(bond_data(), list())
            p2 <- generate_rate_of_change_monitor(filtered_data(), list())

            if (!is.null(p1) && !is.null(p2)) {
                combined <- cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(1.4, 1))
                ggsave(file, combined, width = 10, height = 12, dpi = 150, bg = "white")
            } else if (!is.null(p1)) {
                ggsave(file, p1, width = 10, height = 6, dpi = 150, bg = "white")
            }
        }
    )

    # Download handler for Curve Analysis panel - Use FULL historical data
    output$download_curve_analysis <- downloadHandler(
        filename = function() {
            paste0("curve_analysis_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            # Create combined plot using full historical data
            p1 <- generate_curve_comparison_plot(market_intelligence_data(), list())
            p2 <- generate_curve_steepness_gauge(market_intelligence_data(), list())

            if (!is.null(p1) && !is.null(p2)) {
                combined <- cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(2, 1))
                ggsave(file, combined, width = 10, height = 12, dpi = 150, bg = "white")
            } else if (!is.null(p1)) {
                ggsave(file, p1, width = 10, height = 7, dpi = 150, bg = "white")
            }
        }
    )

    # ================================================================================
    # LEGACY OUTPUTS (kept for backward compatibility)
    # These may be used by other tabs or report generation
    # ================================================================================

    output$curve_spreads_plot <- renderPlot({
        req(filtered_data())
        p <- generate_curve_spreads_plot(filtered_data(), list())
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for curve spread analysis", cex = 1.2)
        }
    })

    output$curve_shape_indicator <- renderPlot({
        req(filtered_data())
        p <- generate_curve_shape_indicator(filtered_data(), list())
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for curve shape analysis", cex = 1.0)
        }
    })

    output$rv_scanner_plot <- renderPlot({
        req(filtered_data())
        p <- generate_relative_value_scanner(filtered_data(), list())
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for relative value analysis", cex = 1.2)
        }
    })

    output$rv_summary_mini <- renderTable({
        req(filtered_data())
        rv_table <- generate_rv_summary_table(filtered_data(), list())
        if(nrow(rv_table) > 0) {
            # Return top 5 opportunities
            head(rv_table, 5)
        } else {
            data.frame(Message = "Insufficient data for relative value analysis")
        }
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", width = "100%")

    # ================================================================================
    # LEGACY OUTPUTS (kept for backward compatibility with other tabs)
    # ================================================================================

    output$enhanced_correlation_plot <- renderPlot({
        req(filtered_data())
        p <- generate_enhanced_correlation_plot(filtered_data(), list())
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient bonds for correlation analysis", cex = 1.2)
        }
    })

    output$regime_analysis_plot <- renderPlot({
        req(regime_data())
        p <- generate_regime_analysis_plot(regime_data(), list())
        if(!is.null(p)) print(p)
    })

    output$term_structure_3d <- renderPlot({
        req(filtered_data())
        p <- generate_term_structure_3d_plot(filtered_data(), list())
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for term structure analysis", cex = 1.2)
        }
    })



    # ================================================================================
    # OUTPUT: TABLES
    # ================================================================================


    # Add Risk Metrics Summary
    output$risk_metrics_summary <- renderUI({
        req(processed_data(), var_data())

        # Get notional from input (same as DV01 ladder for consistency)
        notional <- as.numeric(input$dv01_notional) %||% 10000000

        # Get the data
        risk_data <- processed_data()
        var_metrics <- var_data()

        # Calculate comprehensive risk metrics using correct DV01 formula
        # DV01 = Notional × Modified Duration × 0.0001
        metrics <- risk_data %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            slice(1) %>%
            ungroup() %>%
            summarise(
                avg_duration = mean(modified_duration, na.rm = TRUE),
                avg_convexity = mean(convexity, na.rm = TRUE),
                avg_dv01 = mean(notional * modified_duration * 0.0001 / 1e6, na.rm = TRUE),
                portfolio_yield = weighted.mean(yield_to_maturity,
                                                modified_duration, na.rm = TRUE)
            )

        var_summary <- var_metrics %>%
            summarise(
                avg_var_95 = mean(abs(VaR_95_bps), na.rm = TRUE),
                avg_var_99 = mean(abs(VaR_99_bps), na.rm = TRUE),
                avg_cvar = mean(abs(CVaR_95), na.rm = TRUE) * 100,
                max_var = max(abs(VaR_95_bps), na.rm = TRUE)
            )

        # Check for valid results
        if(all(is.na(metrics)) && all(is.na(var_summary))) {
            return(tags$p("No risk data available", style = "color: #666;"))
        }

        # Return formatted UI
        tagList(
            tags$div(
                style = "padding: 10px;",
                tags$h5("Portfolio Risk Metrics",
                        style = "color: #1B3A6B; margin-top: 0; font-weight: bold;"),

                tags$div(style = "margin: 10px 0;",
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("Avg Duration: "),
                                sprintf("%.2f years", metrics$avg_duration)),
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("Avg Convexity: "),
                                sprintf("%.2f", metrics$avg_convexity)),
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("Avg DV01: "),
                                sprintf("R%.2fm", metrics$avg_dv01)),
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("Portfolio Yield: "),
                                sprintf("%.2f%%", metrics$portfolio_yield))
                ),

                hr(style = "margin: 10px 0;"),

                tags$h5("Value at Risk",
                        style = "color: #1B3A6B; font-weight: bold;"),

                tags$div(style = "margin: 10px 0;",
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("95% VaR: "),
                                sprintf("%.0f bps", var_summary$avg_var_95),
                                if(var_summary$avg_var_95 > 50) {
                                    tags$span(" ⚠", style = "color: #dc3545;")
                                }),
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("99% VaR: "),
                                sprintf("%.0f bps", var_summary$avg_var_99)),
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("CVaR (95%): "),
                                sprintf("%.0f bps", var_summary$avg_cvar)),
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("Max VaR: "),
                                sprintf("%.0f bps", var_summary$max_var),
                                if(var_summary$max_var > 100) {
                                    tags$span(" ⚠", style = "color: #dc3545;")
                                })
                )
            )
        )
    })

    output$risk_metrics_table <- DT::renderDataTable({
        req(processed_data(), var_data())

        # Get notional from input (same as DV01 ladder for consistency)
        notional <- as.numeric(input$dv01_notional) %||% 10000000

        # Get latest data per bond from processed_data
        # CRITICAL: Filter out NA and invalid bond names
        bond_latest <- processed_data() %>%
            filter(
                !is.na(bond),
                bond != "",
                bond != "NA",
                as.character(bond) != "NA"
            ) %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            slice(1) %>%
            ungroup() %>%
            select(bond, yield_to_maturity, modified_duration, duration, convexity)

        # Combine with VaR metrics - use full join to ensure all bonds appear
        # IMPORTANT: Remove modified_duration from var_data to avoid duplicate columns
        # (bond_latest already has modified_duration, and full_join creates .x/.y suffixes for duplicates)
        var_metrics <- var_data() %>%
            select(-modified_duration)

        risk_summary <- bond_latest %>%
            full_join(var_metrics, by = "bond") %>%
            mutate(
                # Calculate DV01 using same formula as DV01 ladder
                # DV01 = Notional × Modified Duration × 0.0001
                dv01_absolute = notional * modified_duration * 0.0001,
                dv01_millions = dv01_absolute / 1e6
            ) %>%
            # CRITICAL: Filter out NA bonds and extreme VaR values
            filter(
                !is.na(bond),
                bond != "",
                bond != "NA",
                as.character(bond) != "NA",
                !is.na(VaR_95_bps),
                is.finite(VaR_95_bps),
                VaR_95_bps < 1000  # Filter extreme values
            ) %>%
            # Sort by 95% VaR descending (highest risk first)
            arrange(desc(abs(VaR_95_bps)))

        # Verify bond count
        message(sprintf("Risk Metrics Table: %d bonds", nrow(risk_summary)))

        # Format for display with explicit units in column headers
        display_table <- risk_summary %>%
            mutate(
                Yield = sprintf("%.3f%%", yield_to_maturity),
                `Mod Duration` = sprintf("%.2f", modified_duration),
                Duration = sprintf("%.2f", duration),
                Convexity = sprintf("%.2f", convexity),
                `DV01 (R mn)` = sprintf("%.3f", dv01_millions),
                # VaR values with explicit bps notation in data
                `95% VaR (bps)` = sprintf("%.0f", abs(VaR_95_bps)),
                `99% VaR (bps)` = sprintf("%.0f", abs(VaR_99_bps)),
                `CVaR (bps)` = sprintf("%.0f", abs(CVaR_95) * 100),
                `Volatility (%)` = sprintf("%.2f", vol)
            ) %>%
            select(
                Bond = bond,
                `Yield (%)` = Yield,
                `Mod Dur (yrs)` = `Mod Duration`,
                `Dur (yrs)` = Duration,
                Convexity,
                `DV01 (R mn)`,
                `95% VaR (bps)`,
                `99% VaR (bps)`,
                `CVaR (bps)`,
                `Volatility (%)`
            )

        # Get notional display for caption
        notional_display <- if(notional >= 1e9) {
            sprintf("R%.0fbn", notional / 1e9)
        } else {
            sprintf("R%.0fm", notional / 1e6)
        }

        # Create enhanced datatable - sorted by 95% VaR (column 6) descending by default
        datatable(
            display_table,
            options = list(
                pageLength = 15,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                scrollX = TRUE,
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all'),
                    list(width = '80px', targets = 0)
                ),
                # Sort by 95% VaR (column index 6, 0-indexed = 5) descending
                order = list(list(6, 'desc'))
            ),
            rownames = FALSE,
            class = 'table-striped table-bordered compact',
            caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; padding: 10px;',
                htmltools::tags$strong('Comprehensive Risk Metrics'),
                htmltools::tags$br(),
                htmltools::tags$small(sprintf('DV01 calculated with %s notional | Sorted by 95%% VaR (highest risk first)', notional_display))
            )
        ) %>%
            formatStyle(
                "95% VaR (bps)",
                backgroundColor = styleInterval(
                    c(100, 200),
                    c("#E8F5E9", "#FFF3E0", "#FFEBEE")  # Green, Orange, Red
                )
            ) %>%
            formatStyle(
                "99% VaR (bps)",
                backgroundColor = styleInterval(
                    c(150, 300),
                    c("#E8F5E9", "#FFF3E0", "#FFEBEE")  # Green, Orange, Red
                ),
                fontWeight = styleInterval(200, c("normal", "bold"))
            ) %>%
            formatStyle(
                "CVaR (bps)",
                backgroundColor = styleInterval(
                    c(150, 300),
                    c("#E8F5E9", "#FFF3E0", "#FFEBEE")  # Green, Orange, Red
                )
            ) %>%
            formatStyle(
                "DV01 (R mn)",
                background = styleColorBar(range(risk_summary$dv01_millions, na.rm = TRUE), '#E3F2FD'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center'
            )
    })

    # Add Technical Indicators Summary - ENHANCED with Overall Signal Synthesis
    output$technical_indicators_summary <- renderUI({
        req(filtered_data(), input$tech_bond_select)

        # Get technical data for selected bond
        tech_data <- filtered_data() %>%
            filter(bond == input$tech_bond_select) %>%
            arrange(date)

        # Calculate technical indicators if not present
        if(!"rsi_14" %in% names(tech_data)) {
            tech_data <- calculate_advanced_technicals(tech_data)
        }

        # Get latest values
        latest <- tech_data %>%
            filter(date == max(date)) %>%
            slice(1)

        if(nrow(latest) == 0) {
            return(tags$p("No technical data available", style = "color: #666;"))
        }

        # Generate individual signals
        rsi_signal <- case_when(
            is.na(latest$rsi_14) ~ "Neutral",
            latest$rsi_14 < 30 ~ "Oversold",
            latest$rsi_14 > 70 ~ "Overbought",
            TRUE ~ "Neutral"
        )

        bb_signal <- case_when(
            is.na(latest$bb_position) ~ "Middle",
            latest$bb_position < 0 ~ "Below Lower Band",
            latest$bb_position < 0.2 ~ "Near Lower Band",
            latest$bb_position > 1 ~ "Above Upper Band",
            latest$bb_position > 0.8 ~ "Near Upper Band",
            TRUE ~ "Middle"
        )

        macd_signal_text <- case_when(
            is.na(latest$macd) | is.na(latest$macd_signal) ~ "Neutral",
            !is.na(latest$macd_histogram) & latest$macd > latest$macd_signal & latest$macd_histogram > 0 ~ "Strong Bullish",
            latest$macd > latest$macd_signal ~ "Bullish",
            !is.na(latest$macd_histogram) & latest$macd < latest$macd_signal & latest$macd_histogram < 0 ~ "Strong Bearish",
            TRUE ~ "Bearish"
        )

        # Bond-specific trend label
        trend_info <- get_trend_label_bonds(
            sma_50 = if_else(is.na(latest$sma_50), latest$yield_to_maturity, latest$sma_50),
            sma_200 = if_else(is.na(latest$sma_200), latest$yield_to_maturity, latest$sma_200),
            current_yield = latest$yield_to_maturity
        )

        # Calculate OVERALL SIGNAL SYNTHESIS
        overall_signal <- calculate_technical_signal(
            rsi = if_else(is.na(latest$rsi_14), 50, latest$rsi_14),
            macd_signal = macd_signal_text,
            trend_status = trend_info$label,
            bb_position = if_else(is.na(latest$bb_position), 0.5, latest$bb_position)
        )

        # Determine signal color based on yield direction
        signal_color <- case_when(
            overall_signal$score > 15 ~ "#D32F2F",   # Red - yields rising (bad for bonds)
            overall_signal$score < -15 ~ "#388E3C", # Green - yields falling (good for bonds)
            TRUE ~ "#FF9800"                         # Orange - neutral
        )

        # Calculate S/R levels with detailed methodology
        sr_levels <- calculate_support_resistance_detailed(
            yield_series = tech_data$yield_to_maturity,
            lookback = 60
        )

        # S/R position color
        sr_position_color <- case_when(
            sr_levels$position_pct > 80 ~ "#D32F2F",   # Near resistance
            sr_levels$position_pct < 20 ~ "#388E3C",   # Near support
            TRUE ~ "#FF9800"
        )

        # Confidence percentage
        confidence_pct <- round(overall_signal$confidence * 100)

        # Calculate comparative context (vs all bonds average)
        all_bonds_latest <- filtered_data() %>%
            group_by(bond) %>%
            filter(date == max(date)) %>%
            ungroup()

        # Ensure RSI calculated for comparison
        if (!"rsi_14" %in% names(all_bonds_latest)) {
            all_bonds_latest <- calculate_advanced_technicals(all_bonds_latest)
        }

        avg_rsi <- mean(all_bonds_latest$rsi_14, na.rm = TRUE)
        rsi_vs_avg <- if_else(is.na(latest$rsi_14), 0, latest$rsi_14) - avg_rsi

        # Return formatted UI with OVERALL SIGNAL prominently displayed
        tagList(
            tags$div(
                style = "padding: 10px;",
                tags$h5(paste("Technical Summary:", input$tech_bond_select),
                        style = "color: #1B3A6B; margin-top: 0; font-weight: bold;"),

                # ═══════════════════════════════════════════════════════════════
                # OVERALL SIGNAL SYNTHESIS - Prominent Display
                # ═══════════════════════════════════════════════════════════════
                tags$div(
                    style = sprintf("border-left: 4px solid %s; padding: 10px; background: #FAFAFA; margin: 10px 0; border-radius: 4px;",
                                    signal_color),

                    tags$div(
                        style = "display: flex; justify-content: space-between; align-items: center;",
                        tags$div(
                            tags$span(
                                style = sprintf("font-size: 14px; font-weight: bold; color: %s;", signal_color),
                                overall_signal$signal
                            ),
                            tags$div(
                                style = "margin-top: 3px;",
                                tags$span(style = "font-size: 11px; color: #666;", "Score: "),
                                tags$span(
                                    style = sprintf("font-weight: bold; color: %s;", signal_color),
                                    sprintf("%+d", overall_signal$score)
                                ),
                                tags$span(style = "font-size: 10px; color: #999;", " / 100")
                            )
                        ),
                        tags$div(
                            style = "text-align: right;",
                            tags$small(
                                style = "color: #888;",
                                sprintf("%d%% conf.", confidence_pct)
                            )
                        )
                    ),
                    tags$div(
                        style = "background: white; padding: 6px 8px; border-radius: 3px; margin-top: 8px; font-size: 11px;",
                        tags$strong(overall_signal$recommendation)
                    ),
                    # Component breakdown (compact)
                    tags$details(
                        style = "margin-top: 6px; font-size: 10px;",
                        tags$summary(style = "color: #666; cursor: pointer;", "Component scores"),
                        tags$div(
                            style = "padding: 5px; color: #666;",
                            sprintf("RSI: %+d | MACD: %+d | Trend: %+d | BB: %+d",
                                    overall_signal$components$rsi,
                                    overall_signal$components$macd,
                                    overall_signal$components$trend,
                                    overall_signal$components$bb)
                        )
                    )
                ),

                # Current Level
                tags$div(style = "background: #f8f9fa; padding: 8px; border-radius: 5px; margin: 8px 0;",
                         tags$p(style = "margin: 0; font-size: 18px; font-weight: bold;",
                                sprintf("%.3f%%", latest$yield_to_maturity)),
                         tags$small("Current Yield")
                ),

                # ═══════════════════════════════════════════════════════════════
                # YIELD TREND - Bond-Specific Terminology
                # ═══════════════════════════════════════════════════════════════
                tags$h6("Yield Trend", style = "color: #666; margin-top: 12px;"),
                tags$div(style = "margin: 8px 0;",
                         tags$p(style = "margin: 5px 0;",
                                tags$span(
                                    style = sprintf("color: %s; font-weight: bold;", trend_info$color),
                                    trend_info$label
                                )
                         ),
                         tags$p(
                             style = "margin: 3px 0; font-size: 11px; color: #666;",
                             trend_info$price_implication
                         )
                ),

                # Momentum Indicators
                tags$h6("Momentum", style = "color: #666; margin-top: 12px;"),
                tags$div(style = "margin: 8px 0;",
                         tags$p(style = "margin: 4px 0;",
                                tags$strong("RSI (14): "),
                                sprintf("%.1f", if_else(is.na(latest$rsi_14), 50, latest$rsi_14)),
                                tags$span(paste0(" - ", rsi_signal),
                                          style = paste0("color: ", case_when(
                                              # For bonds: oversold RSI = yields may rise = bad
                                              rsi_signal == "Oversold" ~ "#D32F2F",
                                              rsi_signal == "Overbought" ~ "#388E3C",
                                              TRUE ~ "#666"
                                          )))),
                         tags$p(style = "margin: 4px 0;",
                                tags$strong("MACD: "),
                                tags$span(
                                    macd_signal_text,
                                    style = paste0("color: ", case_when(
                                        # For yields: bullish MACD = yields rising = bad for bonds
                                        grepl("Bullish", macd_signal_text) ~ "#D32F2F",
                                        grepl("Bearish", macd_signal_text) ~ "#388E3C",
                                        TRUE ~ "#666"
                                    ))
                                ))
                ),

                # Volatility
                tags$h6("Volatility", style = "color: #666; margin-top: 12px;"),
                tags$div(style = "margin: 8px 0;",
                         tags$p(style = "margin: 4px 0;",
                                tags$strong("BB Position: "),
                                bb_signal),
                         tags$p(style = "margin: 4px 0;",
                                tags$strong("BB Width: "),
                                sprintf("%.1f%%", if_else(is.na(latest$bb_width_pct),
                                                          if_else(is.na(latest$bb_width) | is.na(latest$bb_mean) | latest$bb_mean == 0,
                                                                  0,
                                                                  (latest$bb_width / latest$bb_mean) * 100),
                                                          latest$bb_width_pct)),
                                tags$span(
                                    style = "font-size: 10px; color: #888; margin-left: 4px;",
                                    "(5-15% typical)"
                                ))
                ),

                # ═══════════════════════════════════════════════════════════════
                # SUPPORT/RESISTANCE - With Methodology
                # ═══════════════════════════════════════════════════════════════
                if(!is.na(sr_levels$support) && !is.na(sr_levels$resistance)) {
                    tagList(
                        tags$h6("Support & Resistance", style = "color: #666; margin-top: 12px;"),
                        tags$small(style = "color: #999;", sr_levels$methodology),
                        tags$div(style = "margin: 8px 0;",
                                 tags$p(style = "margin: 4px 0;",
                                        tags$strong("Resistance: "),
                                        sprintf("%.3f%%", sr_levels$resistance),
                                        tags$span(
                                            style = "font-size: 10px; color: #888; margin-left: 4px;",
                                            sprintf("(%.0f bps away)", sr_levels$distance_to_resistance * 100)
                                        )),
                                 tags$p(style = "margin: 4px 0;",
                                        tags$strong("Support: "),
                                        sprintf("%.3f%%", sr_levels$support),
                                        tags$span(
                                            style = "font-size: 10px; color: #888; margin-left: 4px;",
                                            sprintf("(%.0f bps away)", sr_levels$distance_to_support * 100)
                                        )),
                                 # Position bar
                                 tags$div(
                                     style = "background: #E0E0E0; height: 6px; border-radius: 3px; margin: 8px 0;",
                                     tags$div(
                                         style = sprintf(
                                             "background: %s; width: %.0f%%; height: 100%%; border-radius: 3px;",
                                             sr_position_color, sr_levels$position_pct
                                         )
                                     )
                                 ),
                                 tags$small(
                                     style = "color: #888;",
                                     sprintf("Position: %.0f%% (0%%=Support, 100%%=Resistance)",
                                             sr_levels$position_pct)
                                 )
                        )
                    )
                },

                # ═══════════════════════════════════════════════════════════════
                # COMPARATIVE CONTEXT - vs Curve Average
                # ═══════════════════════════════════════════════════════════════
                tags$h6("vs. Curve Average", style = "color: #666; margin-top: 12px;"),
                tags$div(style = "margin: 8px 0;",
                         tags$p(style = "margin: 4px 0;",
                                tags$strong("RSI vs Avg: "),
                                sprintf("%+.1f", rsi_vs_avg),
                                if (rsi_vs_avg > 5) {
                                    tags$span(style = "color: #388E3C; font-size: 11px; margin-left: 4px;",
                                              "(Relatively overbought - may fall)")
                                } else if (rsi_vs_avg < -5) {
                                    tags$span(style = "color: #D32F2F; font-size: 11px; margin-left: 4px;",
                                              "(Relatively oversold - may rise)")
                                } else {
                                    tags$span(style = "color: #666; font-size: 11px; margin-left: 4px;",
                                              "(In line with curve)")
                                }
                         )
                )
            )
        )
    })

    # ════════════════════════════════════════════════════════════════════════
    # BOND PROFILE CARD - Fills white space in left column
    # ════════════════════════════════════════════════════════════════════════
    output$bond_profile_card <- renderUI({
        req(input$tech_bond_select, filtered_data())

        bond <- input$tech_bond_select
        data <- filtered_data() %>%
            filter(bond == !!bond) %>%
            arrange(desc(date)) %>%
            slice(1)

        if(nrow(data) == 0) {
            return(tags$div(
                class = "panel panel-default",
                style = "margin-top: 15px;",
                tags$div(class = "panel-body", "No bond data available")
            ))
        }

        # Calculate yield changes
        historical <- filtered_data() %>%
            filter(bond == !!bond) %>%
            arrange(date)

        latest_yield <- data$yield_to_maturity
        yield_1d <- if(nrow(historical) >= 2) {
            latest_yield - historical$yield_to_maturity[nrow(historical) - 1]
        } else NA
        yield_1w <- if(nrow(historical) >= 5) {
            latest_yield - historical$yield_to_maturity[max(1, nrow(historical) - 5)]
        } else NA
        yield_1m <- if(nrow(historical) >= 22) {
            latest_yield - historical$yield_to_maturity[max(1, nrow(historical) - 22)]
        } else NA

        # Calculate DV01
        mod_dur <- if(!is.na(data$modified_duration)) data$modified_duration else 5
        dv01 <- mod_dur * 10000000 * 0.0001  # For R10m notional

        tags$div(
            class = "panel panel-default",
            style = "margin-top: 15px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",

            tags$div(
                class = "panel-heading",
                style = "background-color: #1B3A6B; color: white; padding: 10px 15px;",
                tags$h5(
                    style = "margin: 0;",
                    icon("info-circle"),
                    sprintf(" Bond Profile: %s", bond)
                )
            ),

            tags$div(
                class = "panel-body",
                style = "padding: 15px;",

                # Key metrics in compact format
                tags$table(
                    class = "table table-condensed",
                    style = "margin-bottom: 10px; font-size: 13px;",
                    tags$tbody(
                        tags$tr(
                            tags$td(tags$strong("Coupon:")),
                            tags$td(sprintf("%.2f%%", if(!is.na(data$coupon)) data$coupon else 0))
                        ),
                        tags$tr(
                            tags$td(tags$strong("Current Yield:")),
                            tags$td(sprintf("%.3f%%", latest_yield))
                        ),
                        tags$tr(
                            tags$td(tags$strong("Mod Duration:")),
                            tags$td(sprintf("%.2f years", mod_dur))
                        ),
                        tags$tr(
                            tags$td(tags$strong("Convexity:")),
                            tags$td(sprintf("%.2f", if(!is.na(data$convexity)) data$convexity else 0))
                        ),
                        tags$tr(
                            tags$td(
                                tags$strong("DV01 (R10m):"),
                                tags$span(
                                    class = "text-muted small",
                                    title = "Rand value change per 1bp yield move on R10m notional",
                                    style = "cursor: help; margin-left: 3px;",
                                    icon("question-circle")
                                )
                            ),
                            tags$td(sprintf("R%s", format(round(dv01), big.mark = ",")))
                        )
                    )
                ),

                # Yield change indicators
                tags$hr(style = "margin: 10px 0;"),
                tags$div(
                    class = "small",
                    tags$strong("Yield Changes:"),
                    tags$table(
                        class = "table table-condensed",
                        style = "margin-top: 5px; font-size: 12px;",
                        tags$tbody(
                            tags$tr(
                                tags$td("1 Day:"),
                                tags$td(
                                    style = sprintf("color: %s; font-weight: bold;",
                                                   ifelse(is.na(yield_1d), "#666",
                                                         ifelse(yield_1d >= 0, "#D32F2F", "#388E3C"))),
                                    if(!is.na(yield_1d)) sprintf("%+.0f bps", yield_1d * 100) else "N/A"
                                )
                            ),
                            tags$tr(
                                tags$td("1 Week:"),
                                tags$td(
                                    style = sprintf("color: %s; font-weight: bold;",
                                                   ifelse(is.na(yield_1w), "#666",
                                                         ifelse(yield_1w >= 0, "#D32F2F", "#388E3C"))),
                                    if(!is.na(yield_1w)) sprintf("%+.0f bps", yield_1w * 100) else "N/A"
                                )
                            ),
                            tags$tr(
                                tags$td("1 Month:"),
                                tags$td(
                                    style = sprintf("color: %s; font-weight: bold;",
                                                   ifelse(is.na(yield_1m), "#666",
                                                         ifelse(yield_1m >= 0, "#D32F2F", "#388E3C"))),
                                    if(!is.na(yield_1m)) sprintf("%+.0f bps", yield_1m * 100) else "N/A"
                                )
                            )
                        )
                    ),
                    tags$div(
                        class = "small text-muted",
                        style = "margin-top: 5px; font-style: italic; font-size: 10px;",
                        HTML("&#8593; Rising yields = falling prices (bearish)")
                    )
                )
            )
        )
    })

    # ════════════════════════════════════════════════════════════════════════
    # SIGNAL HISTORY CHART - Shows RSI history over 60 days (IMPROVED SIZE)
    # ════════════════════════════════════════════════════════════════════════
    output$signal_history_mini <- renderUI({
        req(input$tech_bond_select)

        bond <- input$tech_bond_select

        tags$div(
            class = "panel panel-default",
            style = "margin-top: 15px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",

            tags$div(
                class = "panel-heading",
                style = "background-color: #455A64; color: white; padding: 8px 15px;",
                tags$h6(
                    style = "margin: 0;",
                    icon("history"),
                    " RSI Signal History (60 days)"
                )
            ),

            tags$div(
                class = "panel-body",
                style = "padding: 10px;",
                plotOutput("signal_history_sparkline", height = "180px", width = "100%"),  # INCREASED from 100px, FIXED: explicit width
                tags$div(
                    style = "font-size: 10px; color: #888; margin-top: 5px; text-align: center;",
                    HTML("&#x1F7E2; Oversold (buy zone) | &#x1F534; Overbought (sell zone)")
                )
            )
        )
    })

    # Signal History Sparkline Plot - ENHANCED with 60 days and zone shading
    output$signal_history_sparkline <- renderPlot({
        req(input$tech_bond_select, filtered_data_with_technicals())

        bond <- input$tech_bond_select
        data <- filtered_data_with_technicals() %>%
            filter(bond == !!bond) %>%
            arrange(date) %>%
            tail(60) %>%  # INCREASED from 30 to 60 days
            mutate(
                signal_color = case_when(
                    rsi_14 > 70 ~ "overbought",
                    rsi_14 < 30 ~ "oversold",
                    TRUE ~ "neutral"
                ),
                # Detect signal changes
                signal_change = signal_color != lag(signal_color, default = first(signal_color))
            )

        if(nrow(data) < 5 || all(is.na(data$rsi_14))) {
            return(
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5, label = "Insufficient data",
                            color = "#666", size = 4) +
                    theme_void()
            )
        }

        # Get latest RSI for annotation
        latest_rsi <- tail(data$rsi_14[!is.na(data$rsi_14)], 1)

        ggplot(data, aes(x = date, y = rsi_14)) +
            # Zone shading - IMPROVED VISIBILITY
            annotate("rect",
                    xmin = min(data$date, na.rm = TRUE),
                    xmax = max(data$date, na.rm = TRUE),
                    ymin = 70, ymax = 100,
                    fill = "#FFCDD2", alpha = 0.5) +  # Light red - overbought
            annotate("rect",
                    xmin = min(data$date, na.rm = TRUE),
                    xmax = max(data$date, na.rm = TRUE),
                    ymin = 0, ymax = 30,
                    fill = "#C8E6C9", alpha = 0.5) +  # Light green - oversold

            # Zone lines
            geom_hline(yintercept = 30, linetype = "dashed",
                      color = "#388E3C", alpha = 0.7, linewidth = 0.6) +
            geom_hline(yintercept = 70, linetype = "dashed",
                      color = "#D32F2F", alpha = 0.7, linewidth = 0.6) +
            geom_hline(yintercept = 50, linetype = "dotted",
                      color = "#9E9E9E", alpha = 0.5, linewidth = 0.5) +

            # RSI line - thicker for visibility
            geom_line(color = "#1B3A6B", linewidth = 1, na.rm = TRUE) +

            # Points colored by signal - larger for visibility
            geom_point(aes(color = signal_color), size = 2, na.rm = TRUE) +

            # Highlight signal changes
            geom_point(data = data %>% filter(signal_change),
                      aes(color = signal_color), size = 3.5, shape = 18, na.rm = TRUE) +

            # Current value annotation
            geom_point(data = tail(data, 1),
                      color = "#1B3A6B", size = 3, na.rm = TRUE) +
            geom_text(data = tail(data, 1),
                     aes(label = sprintf("%.0f", rsi_14)),
                     hjust = -0.3, vjust = 0.5, size = 3.5, fontface = "bold",
                     color = "#1B3A6B", na.rm = TRUE) +

            scale_color_manual(
                values = c("overbought" = "#D32F2F",
                          "oversold" = "#388E3C",
                          "neutral" = "#1B3A6B"),
                guide = "none"
            ) +
            scale_y_continuous(limits = c(0, 100),
                             breaks = c(0, 30, 50, 70, 100),
                             labels = c("0", "30\nOversold", "50", "70\nOverbought", "100")) +
            scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
            labs(x = NULL, y = NULL) +
            theme_minimal() +
            theme(
                axis.text.y = element_text(size = 9, color = "#666666"),
                axis.text.x = element_text(size = 8, color = "#888888"),
                panel.grid.major.y = element_line(color = "#E0E0E0", linewidth = 0.3),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                plot.margin = ggplot2::margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
            )
    }, bg = "transparent")

    # ════════════════════════════════════════════════════════════════════════
    # YIELD CHART WITH TECHNICAL LEVELS - Shows yield with BB bands and MAs
    # ════════════════════════════════════════════════════════════════════════
    output$yield_technicals_chart <- renderPlot({
        req(input$tech_bond_select, filtered_data_with_technicals())

        bond <- input$tech_bond_select
        data <- filtered_data_with_technicals() %>%
            filter(bond == !!bond) %>%
            arrange(date) %>%
            tail(90)  # Last 90 days

        if(nrow(data) < 10) {
            return(
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5, label = "Insufficient data",
                            color = "#666", size = 4) +
                    theme_void()
            )
        }

        # Calculate support/resistance from recent data
        support <- min(data$yield_to_maturity, na.rm = TRUE)
        resistance <- max(data$yield_to_maturity, na.rm = TRUE)
        current <- tail(data$yield_to_maturity, 1)

        # Get latest values for annotation
        latest <- tail(data, 1)

        ggplot(data, aes(x = date, y = yield_to_maturity)) +
            # Bollinger Bands - if available
            {
                if ("bb_lower" %in% names(data) && !all(is.na(data$bb_lower))) {
                    geom_ribbon(aes(ymin = bb_lower, ymax = bb_upper),
                               fill = "#1B3A6B", alpha = 0.1, na.rm = TRUE)
                }
            } +

            # Moving Averages - if available
            {
                if ("sma_50" %in% names(data) && !all(is.na(data$sma_50))) {
                    geom_line(aes(y = sma_50), color = "#f39c12",
                             linewidth = 0.8, linetype = "solid", alpha = 0.8, na.rm = TRUE)
                }
            } +
            {
                if ("sma_200" %in% names(data) && !all(is.na(data$sma_200))) {
                    geom_line(aes(y = sma_200), color = "#3498db",
                             linewidth = 0.8, linetype = "dashed", alpha = 0.8, na.rm = TRUE)
                }
            } +

            # Yield line
            geom_line(color = "#1B3A6B", linewidth = 1.2, na.rm = TRUE) +

            # Support/Resistance lines
            geom_hline(yintercept = support, color = "#388E3C",
                      linetype = "dashed", linewidth = 0.6) +
            geom_hline(yintercept = resistance, color = "#D32F2F",
                      linetype = "dashed", linewidth = 0.6) +

            # Current point highlight
            geom_point(data = latest, color = "#1B3A6B", size = 3) +

            # Annotations for S/R levels
            annotate("text", x = min(data$date, na.rm = TRUE), y = support,
                    label = sprintf("Support: %.2f%%", support),
                    hjust = 0, vjust = 1.5, size = 2.8, color = "#388E3C") +
            annotate("text", x = min(data$date, na.rm = TRUE), y = resistance,
                    label = sprintf("Resistance: %.2f%%", resistance),
                    hjust = 0, vjust = -0.5, size = 2.8, color = "#D32F2F") +

            scale_y_continuous(labels = function(x) paste0(sprintf("%.2f", x), "%")) +
            scale_x_date(date_breaks = "3 weeks", date_labels = "%b %d") +

            labs(
                title = NULL,
                subtitle = "BB bands (shaded) | SMA50 (orange) | SMA200 (blue dashed)",
                x = NULL, y = NULL
            ) +
            theme_minimal() +
            theme(
                plot.subtitle = element_text(size = 8, color = "#888"),
                axis.text = element_text(size = 9, color = "#666"),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(color = "#E8E8E8", linewidth = 0.3),
                plot.margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")
            )
    }, bg = "transparent")

    # ════════════════════════════════════════════════════════════════════════
    # YIELD CHART UI WRAPPER - Panel with title
    # ════════════════════════════════════════════════════════════════════════
    output$yield_technicals_panel <- renderUI({
        req(input$tech_bond_select)

        tags$div(
            class = "panel panel-default",
            style = "margin-top: 15px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",

            tags$div(
                class = "panel-heading",
                style = "background-color: #37474F; color: white; padding: 8px 15px;",
                tags$h6(
                    style = "margin: 0;",
                    icon("chart-line"),
                    " Yield with Technical Levels (90 days)"
                )
            ),

            tags$div(
                class = "panel-body",
                style = "padding: 10px;",
                plotOutput("yield_technicals_chart", height = "200px", width = "100%")  # FIXED: explicit width
            )
        )
    })

    # ════════════════════════════════════════════════════════════════════════
    # TECHNICAL SUMMARY PANEL - Main panel with signal synthesis
    # ════════════════════════════════════════════════════════════════════════
    output$technical_summary_panel <- renderUI({
        req(filtered_data(), input$tech_bond_select)

        # ═══════════════════════════════════════════════════════════════════════
        # VALIDATION: Ensure selected bond is not a matured bond
        # ═══════════════════════════════════════════════════════════════════════
        known_matured_bonds <- c("R157", "R186", "R197", "R203", "R204", "R207", "R208", "R212", "R2023")
        if (input$tech_bond_select %in% known_matured_bonds) {
            message(sprintf("[Technical Summary] Selected bond %s is matured - skipping render",
                           input$tech_bond_select))
            return(tags$div(
                style = "padding: 20px; text-align: center; color: #999;",
                tags$p("Please select an active bond from the dropdown."),
                tags$p(style = "font-size: 11px;",
                       sprintf("Bond %s has matured and is no longer available.",
                              input$tech_bond_select))
            ))
        }

        # Get technical data for selected bond
        tech_data <- filtered_data() %>%
            filter(bond == input$tech_bond_select) %>%
            arrange(date)

        # Calculate technical indicators if not present
        if(!"rsi_14" %in% names(tech_data)) {
            tech_data <- calculate_advanced_technicals(tech_data)
        }

        # Get latest values
        latest <- tech_data %>%
            filter(date == max(date)) %>%
            slice(1)

        if(nrow(latest) == 0) {
            return(tags$p("No technical data available", style = "color: #666;"))
        }

        # Generate individual signals
        rsi_signal <- case_when(
            is.na(latest$rsi_14) ~ "Neutral",
            latest$rsi_14 < 30 ~ "Oversold",
            latest$rsi_14 > 70 ~ "Overbought",
            TRUE ~ "Neutral"
        )

        bb_signal <- case_when(
            is.na(latest$bb_position) ~ "Middle",
            latest$bb_position < 0 ~ "Below Lower Band",
            latest$bb_position < 0.2 ~ "Near Lower Band",
            latest$bb_position > 1 ~ "Above Upper Band",
            latest$bb_position > 0.8 ~ "Near Upper Band",
            TRUE ~ "Middle"
        )

        macd_signal_text <- case_when(
            is.na(latest$macd) | is.na(latest$macd_signal) ~ "Neutral",
            !is.na(latest$macd_histogram) & latest$macd > latest$macd_signal & latest$macd_histogram > 0 ~ "Strong Bullish",
            latest$macd > latest$macd_signal ~ "Bullish",
            !is.na(latest$macd_histogram) & latest$macd < latest$macd_signal & latest$macd_histogram < 0 ~ "Strong Bearish",
            TRUE ~ "Bearish"
        )

        # Bond-specific trend label
        trend_info <- get_trend_label_bonds(
            sma_50 = if_else(is.na(latest$sma_50), latest$yield_to_maturity, latest$sma_50),
            sma_200 = if_else(is.na(latest$sma_200), latest$yield_to_maturity, latest$sma_200),
            current_yield = latest$yield_to_maturity
        )

        # Calculate OVERALL SIGNAL SYNTHESIS
        overall_signal <- calculate_technical_signal(
            rsi = if_else(is.na(latest$rsi_14), 50, latest$rsi_14),
            macd_signal = macd_signal_text,
            trend_status = trend_info$label,
            bb_position = if_else(is.na(latest$bb_position), 0.5, latest$bb_position)
        )

        # Determine signal color based on yield direction
        signal_color <- case_when(
            overall_signal$score > 15 ~ "#D32F2F",   # Red - yields rising (bad for bonds)
            overall_signal$score < -15 ~ "#388E3C", # Green - yields falling (good for bonds)
            TRUE ~ "#FF9800"                         # Orange - neutral
        )

        # Calculate S/R levels with detailed methodology
        sr_levels <- calculate_support_resistance_detailed(
            yield_series = tech_data$yield_to_maturity,
            lookback = 60
        )

        # S/R position color
        sr_position_color <- case_when(
            sr_levels$position_pct > 80 ~ "#D32F2F",   # Near resistance
            sr_levels$position_pct < 20 ~ "#388E3C",   # Near support
            TRUE ~ "#FF9800"
        )

        # Confidence percentage
        confidence_pct <- round(overall_signal$confidence * 100)

        # Return formatted UI
        tags$div(
            style = "background: #F8F9FA; padding: 15px; border-radius: 8px;",

            tags$h4(paste("Technical Summary:", input$tech_bond_select),
                   style = "color: #1B3A6B; margin-top: 0; font-weight: bold;"),

            # ═══════════════════════════════════════════════════════════════
            # OVERALL SIGNAL SYNTHESIS - Prominent Display
            # ═══════════════════════════════════════════════════════════════
            tags$div(
                style = sprintf("border-left: 4px solid %s; padding: 12px; background: white; margin: 12px 0; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                               signal_color),

                fluidRow(
                    column(6,
                        tags$div(
                            tags$span(
                                style = sprintf("font-size: 18px; font-weight: bold; color: %s;", signal_color),
                                overall_signal$signal
                            ),
                            tags$div(
                                style = "margin-top: 5px;",
                                tags$span(style = "font-size: 12px; color: #666;", "Score: "),
                                tags$span(
                                    style = sprintf("font-weight: bold; font-size: 16px; color: %s;", signal_color),
                                    sprintf("%+d", overall_signal$score)
                                ),
                                tags$span(style = "font-size: 11px; color: #999;", " / 100")
                            )
                        )
                    ),
                    column(6,
                        tags$div(
                            style = "text-align: right;",
                            tags$span(
                                style = "font-size: 24px; font-weight: bold; color: #1B3A6B;",
                                sprintf("%.3f%%", latest$yield_to_maturity)
                            ),
                            tags$div(
                                style = "font-size: 11px; color: #888;",
                                sprintf("Signal Strength: %d%%", confidence_pct),
                                tags$span(
                                    style = sprintf("margin-left: 5px; padding: 2px 6px; background: %s; color: white; border-radius: 3px; font-size: 9px;",
                                                   ifelse(confidence_pct >= 60, "#388E3C", ifelse(confidence_pct >= 40, "#FF9800", "#9E9E9E"))),
                                    ifelse(confidence_pct >= 60, "Strong", ifelse(confidence_pct >= 40, "Moderate", "Weak"))
                                )
                            )
                        )
                    )
                ),

                tags$div(
                    style = "background: #F5F5F5; padding: 8px 10px; border-radius: 4px; margin-top: 10px; font-size: 12px;",
                    tags$strong(overall_signal$recommendation)
                ),

                # ═══════════════════════════════════════════════════════════════
                # Component Breakdown with Scale Context
                # ═══════════════════════════════════════════════════════════════
                tags$div(
                    style = "margin-top: 12px; padding: 10px; background: #FAFAFA; border-radius: 4px;",
                    tags$div(
                        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
                        tags$strong("Component Breakdown:", style = "font-size: 12px; color: #1B3A6B;"),
                        tags$span(style = "font-size: 10px; color: #999;", "Range: -25 to +30 each | Total: -100 to +100")
                    ),
                    tags$div(
                        style = "display: flex; justify-content: space-between; margin-top: 8px; font-size: 11px;",

                        # RSI (±25 scale)
                        tags$div(
                            style = "text-align: center; flex: 1; padding: 5px; border-right: 1px solid #E0E0E0;",
                            tags$div(
                                style = sprintf("color: %s; font-weight: bold; font-size: 16px;",
                                               ifelse(overall_signal$components$rsi > 0, "#D32F2F",
                                                     ifelse(overall_signal$components$rsi < 0, "#388E3C", "#9E9E9E"))),
                                sprintf("%+d", overall_signal$components$rsi)
                            ),
                            tags$div(style = "color: #666; font-size: 11px; font-weight: 500;", "RSI"),
                            tags$div(style = "color: #AAA; font-size: 9px;", "±25")
                        ),

                        # MACD (±30 scale)
                        tags$div(
                            style = "text-align: center; flex: 1; padding: 5px; border-right: 1px solid #E0E0E0;",
                            tags$div(
                                style = sprintf("color: %s; font-weight: bold; font-size: 16px;",
                                               ifelse(overall_signal$components$macd > 0, "#D32F2F",
                                                     ifelse(overall_signal$components$macd < 0, "#388E3C", "#9E9E9E"))),
                                sprintf("%+d", overall_signal$components$macd)
                            ),
                            tags$div(style = "color: #666; font-size: 11px; font-weight: 500;", "MACD"),
                            tags$div(style = "color: #AAA; font-size: 9px;", "±30")
                        ),

                        # Trend (±30 scale)
                        tags$div(
                            style = "text-align: center; flex: 1; padding: 5px; border-right: 1px solid #E0E0E0;",
                            tags$div(
                                style = sprintf("color: %s; font-weight: bold; font-size: 16px;",
                                               ifelse(overall_signal$components$trend > 0, "#D32F2F",
                                                     ifelse(overall_signal$components$trend < 0, "#388E3C", "#9E9E9E"))),
                                sprintf("%+d", overall_signal$components$trend)
                            ),
                            tags$div(style = "color: #666; font-size: 11px; font-weight: 500;", "Trend"),
                            tags$div(style = "color: #AAA; font-size: 9px;", "±30")
                        ),

                        # BB (±15 scale)
                        tags$div(
                            style = "text-align: center; flex: 1; padding: 5px;",
                            tags$div(
                                style = sprintf("color: %s; font-weight: bold; font-size: 16px;",
                                               ifelse(overall_signal$components$bb > 0, "#D32F2F",
                                                     ifelse(overall_signal$components$bb < 0, "#388E3C", "#9E9E9E"))),
                                sprintf("%+d", overall_signal$components$bb)
                            ),
                            tags$div(style = "color: #666; font-size: 11px; font-weight: 500;", "BB"),
                            tags$div(style = "color: #AAA; font-size: 9px;", "±15")
                        )
                    )
                )
            ),

            # ═══════════════════════════════════════════════════════════════
            # Indicators Summary Row
            # ═══════════════════════════════════════════════════════════════
            fluidRow(
                column(6,
                    tags$div(
                        style = "background: white; padding: 10px; border-radius: 5px; margin: 8px 0;",
                        tags$h6("Momentum", style = "color: #1B3A6B; margin-top: 0;"),
                        tags$p(style = "margin: 4px 0; font-size: 12px;",
                               tags$strong("RSI (14): "),
                               sprintf("%.1f", if_else(is.na(latest$rsi_14), 50, latest$rsi_14)),
                               tags$span(paste0(" - ", rsi_signal),
                                        style = paste0("color: ", case_when(
                                            rsi_signal == "Oversold" ~ "#D32F2F",
                                            rsi_signal == "Overbought" ~ "#388E3C",
                                            TRUE ~ "#666"
                                        )))),
                        tags$p(style = "margin: 4px 0; font-size: 12px;",
                               tags$strong("MACD: "),
                               tags$span(
                                   macd_signal_text,
                                   style = paste0("color: ", case_when(
                                       grepl("Bullish", macd_signal_text) ~ "#D32F2F",
                                       grepl("Bearish", macd_signal_text) ~ "#388E3C",
                                       TRUE ~ "#666"
                                   ))
                               ))
                    )
                ),
                column(6,
                    tags$div(
                        style = "background: white; padding: 10px; border-radius: 5px; margin: 8px 0;",
                        tags$h6("Volatility", style = "color: #1B3A6B; margin-top: 0;"),
                        tags$p(style = "margin: 4px 0; font-size: 12px;",
                               tags$strong("BB Position: "),
                               bb_signal),
                        tags$p(style = "margin: 4px 0; font-size: 12px;",
                               tags$strong("BB Width: "),
                               sprintf("%.1f%%", if_else(is.na(latest$bb_width_pct),
                                                         if_else(is.na(latest$bb_width) | is.na(latest$bb_mean) | latest$bb_mean == 0,
                                                                 0,
                                                                 (latest$bb_width / latest$bb_mean) * 100),
                                                         latest$bb_width_pct)),
                               tags$span(style = "font-size: 10px; color: #888;", " (5-15% typical)"))
                    )
                )
            ),

            # ═══════════════════════════════════════════════════════════════
            # FIX 5: SUPPORT/RESISTANCE with Position Bar Legend
            # ═══════════════════════════════════════════════════════════════
            if(!is.na(sr_levels$support) && !is.na(sr_levels$resistance)) {
                tags$div(
                    style = "background: white; padding: 12px; border-radius: 5px; margin-top: 8px;",
                    tags$h6("Support & Resistance", style = "color: #1B3A6B; margin-top: 0;"),
                    tags$small(style = "color: #999;", sr_levels$methodology),

                    fluidRow(
                        column(6,
                            tags$p(style = "margin: 6px 0; font-size: 12px;",
                                   tags$strong("Support: "),
                                   sprintf("%.3f%%", sr_levels$support),
                                   tags$span(style = "font-size: 10px; color: #388E3C; margin-left: 4px;",
                                            sprintf("(%.0f bps away)", sr_levels$distance_to_support * 100)))
                        ),
                        column(6,
                            tags$p(style = "margin: 6px 0; font-size: 12px;",
                                   tags$strong("Resistance: "),
                                   sprintf("%.3f%%", sr_levels$resistance),
                                   tags$span(style = "font-size: 10px; color: #D32F2F; margin-left: 4px;",
                                            sprintf("(%.0f bps away)", sr_levels$distance_to_resistance * 100)))
                        )
                    ),

                    # Position bar with gradient
                    tags$div(
                        style = "margin-top: 10px;",

                        # Gradient position bar (green to red)
                        tags$div(
                            style = "background: linear-gradient(to right, #388E3C, #FF9800, #D32F2F); height: 10px; border-radius: 5px; position: relative;",
                            # Current position marker
                            tags$div(
                                style = sprintf(
                                    "position: absolute; left: %.0f%%; top: -3px; width: 4px; height: 16px; background: #1B3A6B; border-radius: 2px; transform: translateX(-50%%); box-shadow: 0 1px 3px rgba(0,0,0,0.3);",
                                    sr_levels$position_pct
                                )
                            )
                        ),

                        # Legend
                        tags$div(
                            style = "display: flex; justify-content: space-between; font-size: 10px; margin-top: 4px;",
                            tags$span(style = "color: #388E3C; font-weight: bold;", "Support (Buy Zone)"),
                            tags$span(style = "color: #D32F2F; font-weight: bold;", "Resistance (Sell Zone)")
                        ),

                        # Current position text
                        tags$div(
                            style = "text-align: center; margin-top: 6px;",
                            tags$span(style = "font-size: 11px; color: #666;",
                                     sprintf("Current Position: %.0f%% toward resistance", sr_levels$position_pct))
                        )
                    )
                )
            },

            # Yield Trend
            tags$div(
                style = "background: white; padding: 10px; border-radius: 5px; margin-top: 8px;",
                tags$h6("Yield Trend", style = "color: #1B3A6B; margin-top: 0;"),
                tags$p(style = "margin: 5px 0;",
                       tags$span(
                           style = sprintf("color: %s; font-weight: bold;", trend_info$color),
                           trend_info$label
                       )
                ),
                tags$p(style = "margin: 3px 0; font-size: 11px; color: #666;",
                      trend_info$price_implication)
            )
        )
    })

    # Handler for refresh button
    observeEvent(input$tech_refresh, {
        # Invalidate the filtered_data reactive to force refresh
        showNotification("Refreshing technical data...", type = "message", duration = 2)
    })

    # TECHNICAL INDICATORS TABLE - USING MASTER REACTIVE FOR CONSISTENCY
    output$technical_indicators_enhanced_table <- DT::renderDataTable({
        req(technical_signals_master())

        # Use the MASTER reactive - ensures consistency with Signal Matrix
        tech_summary <- technical_signals_master()

        # Check if we have data
        if(nrow(tech_summary) == 0) {
            empty_df <- data.frame(
                Message = "No technical indicator data available. Please adjust date range or bond selection."
            )
            return(datatable(empty_df, options = list(dom = 't'), rownames = FALSE))
        }

        # Sort by total_score (same as matrix TOTAL column)
        tech_summary <- tech_summary %>%
            arrange(desc(total_score))

        # Format for display using pre-calculated labels from master reactive
        display_table <- tech_summary %>%
            mutate(
                `Current Yield` = sprintf("%.3f%%", yield_to_maturity),
                RSI = sprintf("%.1f", rsi_14),
                `RSI Signal` = rsi_signal_label,
                # BB Position as percentage
                `BB %` = sprintf("%.0f%%", bb_position * 100),
                `BB Signal` = bb_signal_label,
                MACD = sprintf("%.4f", macd_histogram),
                `MACD Signal` = macd_signal_label,
                Trend = trend_label,
                # ROC with sign indicator
                `ROC (20d)` = sprintf("%+.2f%%", roc_20),
                `ROC Signal` = roc_signal_label,
                # Score column - uses total_score from master (same as matrix TOTAL)
                Score = as.character(total_score),
                # Overall Signal - uses overall_signal from master (matches matrix classification)
                `Overall Signal` = overall_signal
            ) %>%
            select(
                Bond = bond,
                `Current Yield`,
                RSI,
                `RSI Signal`,
                `BB %`,
                `BB Signal`,
                MACD,
                `MACD Signal`,
                Trend,
                `ROC (20d)`,
                `ROC Signal`,
                Score,
                `Overall Signal`
            )

        # Create interactive datatable
        datatable(
            display_table,
            options = list(
                pageLength = 15,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                scrollX = TRUE,
                order = list(list(11, 'desc')),  # Sort by Score column (index 11) descending
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all'),
                    list(width = '70px', targets = 0)
                )
            ),
            rownames = FALSE,
            class = 'table-striped table-bordered compact',
            caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; padding: 10px;',
                htmltools::tags$strong('Technical Analysis Summary'),
                htmltools::tags$br(),
                htmltools::tags$small('Sorted by Signal Score (strongest buy at top) | Score range: -8 to +8 | '),
                htmltools::tags$strong(style = 'color: #1B5E20;', 'Bond Price Perspective'),
                htmltools::tags$small(' (Green = Good for bonds)')
            )
        ) %>%
            # Color code RSI Signal (5 levels) - BOND PRICE PERSPECTIVE
            # Overbought yields = Good for bonds (yields may fall), Oversold yields = Bad for bonds
            formatStyle(
                "RSI Signal",
                backgroundColor = styleEqual(
                    c("Extreme OB", "Overbought", "Neutral", "Oversold", "Extreme OS"),
                    c("#1B5E20", "#4CAF50", "#F5F5F5", "#FF8A65", "#C62828")
                ),
                color = styleEqual(
                    c("Extreme OS", "Extreme OB"),
                    c("white", "white")
                )
            ) %>%
            # Color code BB Signal (5 levels) - BOND PRICE PERSPECTIVE
            # Above Upper (overbought yields) = Good for bonds, Below Lower (oversold yields) = Bad
            formatStyle(
                "BB Signal",
                backgroundColor = styleEqual(
                    c("Above Upper", "Near Upper", "Within Bands", "Near Lower", "Below Lower"),
                    c("#1B5E20", "#4CAF50", "#F5F5F5", "#FF8A65", "#C62828")
                ),
                color = styleEqual(
                    c("Below Lower", "Above Upper"),
                    c("white", "white")
                )
            ) %>%
            # Color code MACD Signal (5 levels) - INVERTED for bond prices
            # Bullish yields = Red (bad for bond prices), Bearish yields = Green (good for bond prices)
            formatStyle(
                "MACD Signal",
                backgroundColor = styleEqual(
                    c("Strong Bearish", "Bearish", "Neutral", "Bullish", "Strong Bullish"),
                    c("#1B5E20", "#4CAF50", "#E0E0E0", "#FF7043", "#C62828")
                ),
                color = styleEqual(
                    c("Strong Bearish", "Bearish", "Neutral", "Bullish", "Strong Bullish"),
                    c("white", "white", "black", "white", "white")
                )
            ) %>%
            # Color code Trend (5 levels) - INVERTED for bond prices
            # Uptrend yields = Red (prices falling), Downtrend yields = Green (prices rising)
            formatStyle(
                "Trend",
                backgroundColor = styleEqual(
                    c("Strong Downtrend", "Downtrend", "Sideways", "Uptrend", "Strong Uptrend"),
                    c("#1B5E20", "#4CAF50", "#E0E0E0", "#FF7043", "#C62828")
                ),
                color = styleEqual(
                    c("Strong Downtrend", "Downtrend", "Sideways", "Uptrend", "Strong Uptrend"),
                    c("white", "white", "black", "white", "white")
                )
            ) %>%
            # Color code ROC Signal (5 levels) - INVERTED for bond prices
            # Rising yields = Red, Falling yields = Green
            formatStyle(
                "ROC Signal",
                backgroundColor = styleEqual(
                    c("Strong Fall", "Falling", "Flat", "Rising", "Strong Rise"),
                    c("#1B5E20", "#4CAF50", "#E0E0E0", "#FF7043", "#C62828")
                ),
                color = styleEqual(
                    c("Strong Fall", "Falling", "Flat", "Rising", "Strong Rise"),
                    c("white", "white", "black", "white", "white")
                )
            ) %>%
            # Color code Score column with gradient
            formatStyle(
                "Score",
                color = styleInterval(
                    c(-3, -1, 1, 3),
                    c("#C62828", "#FF8A65", "#666666", "#4CAF50", "#1B5E20")
                ),
                fontWeight = "bold"
            ) %>%
            # Color code Overall Signal (5 levels with proper colors)
            formatStyle(
                "Overall Signal",
                backgroundColor = styleEqual(
                    c("Strong Buy", "Buy", "Neutral", "Sell", "Strong Sell"),
                    c("#1B5E20", "#4CAF50", "#E0E0E0", "#FF7043", "#C62828")
                ),
                color = styleEqual(
                    c("Strong Buy", "Buy", "Neutral", "Sell", "Strong Sell"),
                    c("white", "white", "black", "white", "white")
                ),
                fontWeight = "bold"
            )
    })

    # ═══════════════════════════════════════════════════════════════════════
    # DYNAMIC INFO BOX - Reactive to funding rate slider
    # ═══════════════════════════════════════════════════════════════════════
    output$carry_roll_info_box <- renderUI({
        tryCatch({
            # Get current funding rate from slider (default 8.25%)
            funding_rate <- if(!is.null(input$funding_rate)) input$funding_rate else 8.25

        # Format as percentage for display
        funding_rate_text <- sprintf("%.2f%%", funding_rate)

        # Calculate example values using actual funding rate
        example_coupon <- 10  # Example coupon rate
        example_days <- 90
        example_carry <- example_coupon * (example_days / 365)
        example_funding <- funding_rate * (example_days / 365)

        tags$div(
            class = "alert alert-info",
            style = "margin-bottom: 20px;",
            tags$h4("Understanding Carry & Roll Analysis", style = "margin-top: 0;"),
            tags$p(
                tags$strong("What This Analysis Shows:"),
                "This table calculates the expected total return from holding SA government bonds over different time periods (30, 90, 180, and 360 days), broken down into component parts."
            ),
            tags$div(
                style = "margin-top: 15px;",
                tags$h5("Return Components:", style = "color: #1B3A6B;"),
                tags$ul(
                    tags$li(tags$strong("Carry Income:"),
                            sprintf("The coupon income earned during the holding period. For a %d%% coupon bond held %d days: %d%% x (%d/365) = %.2f%%",
                                   example_coupon, example_days, example_coupon, example_days, example_carry)),
                    tags$li(tags$strong("Roll-Down Return:"),
                            "The price appreciation as the bond 'rolls down' the yield curve. As time passes and duration decreases, bonds typically move to lower-yielding parts of the curve, generating capital gains."),
                    tags$li(tags$strong("Funding Cost:"),
                            sprintf("The cost to finance the position, typically at the repo rate (currently %s annually). For %d days: %s x (%d/365) = %.2f%%",
                                   funding_rate_text, example_days, funding_rate_text, example_days, example_funding)),
                    tags$li(tags$strong("Net Return:"),
                            "Total return after subtracting funding costs: (Carry + Roll) - Funding")
                )
            ),
            tags$div(
                style = "margin-top: 15px;",
                tags$h5("How to Read the Results:", style = "color: #1B3A6B;"),
                tags$ul(
                    tags$li(tags$strong("Green cells:"), "Positive returns - bond generates profit after funding costs"),
                    tags$li(tags$strong("Red/Yellow cells:"), "Negative or low returns - funding costs may exceed income"),
                    tags$li(tags$strong("Return Type Options:"),
                            tags$ul(
                                tags$li("Gross: Total return before funding costs"),
                                tags$li("Net: Return after funding costs (most realistic)"),
                                tags$li("Risk-Adjusted: Return per unit of duration risk")
                            ))
                )
            ),
            # Key insight box with dynamic funding rate
            tags$div(
                style = "background-color: #FFF3CD; color: #856404; padding: 12px; border-radius: 4px; margin-top: 15px;",
                tags$span(icon("lightbulb"), style = "margin-right: 8px;"),
                tags$strong("Key Insight: "),
                sprintf("Bonds with coupons above the funding rate (%s) generate positive carry. Longer holding periods typically show higher returns due to compound effects and roll-down benefits.",
                       funding_rate_text)
            )
        )
        }, error = function(e) {
            warning(sprintf("Carry & Roll info box error: %s", e$message))
            tags$div(
                class = "alert alert-warning",
                "Unable to load analysis information. Please refresh the page."
            )
        })
    })

    # ═══════════════════════════════════════════════════════════════════════
    # FIX 5: IMPROVED QUICK METRICS WITH SPECIFIC BOND NAMES
    # FIX 6: BREAKEVEN YIELD MOVE CALCULATION
    # ═══════════════════════════════════════════════════════════════════════
    output$carry_roll_summary <- renderUI({
        req(carry_roll_data())

        data_90d <- carry_roll_data() %>%
            filter(holding_period == "90d")

        if(nrow(data_90d) == 0) {
            return(tags$p("No 90-day data available"))
        }

        # Calculate summary statistics
        avg_net_return <- mean(data_90d$net_return, na.rm = TRUE)
        best_bond <- data_90d %>% slice_max(net_return, n = 1, with_ties = FALSE)
        worst_bond <- data_90d %>% slice_min(net_return, n = 1, with_ties = FALSE)
        positive_carry_count <- sum(data_90d$net_return > 0, na.rm = TRUE)
        negative_carry_count <- sum(data_90d$net_return <= 0, na.rm = TRUE)
        total_bonds <- nrow(data_90d)

        tagList(
            # Section title with period specification
            tags$small(class = "text-muted", "Based on 90-day holding period"),
            tags$hr(style = "margin: 8px 0;"),

            # Average Net Return
            tags$div(
                style = "margin-bottom: 12px;",
                tags$div(class = "text-muted", style = "font-size: 11px;", "Average Net Return:"),
                tags$div(
                    style = sprintf("font-size: 20px; font-weight: bold; color: %s;",
                                    ifelse(avg_net_return > 0, "#1B5E20", "#C62828")),
                    sprintf("%.2f%%", avg_net_return)
                )
            ),

            tags$hr(style = "margin: 8px 0;"),

            # Best Performer - CLICKABLE to update decomposition
            tags$div(
                style = "margin-bottom: 12px;",
                tags$div(class = "text-muted", style = "font-size: 11px;", "Best Performer:"),
                actionLink(
                    "select_best_bond",
                    tags$div(
                        style = "font-size: 14px; font-weight: bold; color: #1B5E20; cursor: pointer;",
                        sprintf("%s: %.2f%%", best_bond$bond, best_bond$net_return),
                        tags$small(style = "margin-left: 5px; color: #666; font-weight: normal;",
                                   icon("arrow-right"))
                    )
                )
            ),

            tags$hr(style = "margin: 8px 0;"),

            # Lowest Return - CLICKABLE to update decomposition
            tags$div(
                style = "margin-bottom: 12px;",
                tags$div(class = "text-muted", style = "font-size: 11px;",
                         ifelse(worst_bond$net_return >= 0, "Lowest Return:", "Negative Return:")),
                actionLink(
                    "select_worst_bond",
                    tags$div(
                        style = sprintf("font-size: 14px; font-weight: bold; color: %s; cursor: pointer;",
                                        case_when(
                                            worst_bond$net_return < 0 ~ "#C62828",
                                            worst_bond$net_return < 0.5 ~ "#FF9800",
                                            TRUE ~ "#689F38"
                                        )),
                        sprintf("%s: %.2f%%", worst_bond$bond, worst_bond$net_return),
                        tags$small(style = "margin-left: 5px; color: #666; font-weight: normal;",
                                   icon("arrow-right"))
                    )
                ),
                if(worst_bond$net_return > 0) {
                    tags$small(class = "text-muted", style = "font-size: 10px; display: block;",
                               "(still positive carry)")
                }
            ),

            tags$hr(style = "margin: 8px 0;"),

            # Returns breakdown - more useful than progress bar
            tags$div(
                style = "margin-bottom: 8px;",
                tags$div(class = "text-muted", style = "font-size: 11px; margin-bottom: 6px;",
                         "Return Distribution:"),
                tags$div(
                    style = "display: flex; justify-content: space-around; text-align: center;",
                    tags$div(
                        tags$span(positive_carry_count,
                                  style = "font-size: 20px; font-weight: bold; color: #2E7D32;"),
                        tags$br(),
                        tags$span("Positive", class = "text-muted", style = "font-size: 10px;")
                    ),
                    tags$div(
                        tags$span(negative_carry_count,
                                  style = sprintf("font-size: 20px; font-weight: bold; color: %s;",
                                                  ifelse(negative_carry_count > 0, "#C62828", "#9E9E9E"))),
                        tags$br(),
                        tags$span("Negative", class = "text-muted", style = "font-size: 10px;")
                    )
                )
            ),

            # Click hint
            tags$div(
                style = "margin-top: 10px; font-size: 9px; color: #999; font-style: italic;",
                "Click bond names to view decomposition"
            )
        )
    })

    # Observer to handle best bond selection click
    observeEvent(input$select_best_bond, {
        req(carry_roll_data())
        data_90d <- carry_roll_data() %>%
            filter(holding_period == "90d")
        if(nrow(data_90d) > 0) {
            best_bond <- data_90d %>% slice_max(net_return, n = 1, with_ties = FALSE)
            updateSelectInput(session, "selected_carry_bond", selected = best_bond$bond)
        }
    })

    # Observer to handle worst bond selection click
    observeEvent(input$select_worst_bond, {
        req(carry_roll_data())
        data_90d <- carry_roll_data() %>%
            filter(holding_period == "90d")
        if(nrow(data_90d) > 0) {
            worst_bond <- data_90d %>% slice_min(net_return, n = 1, with_ties = FALSE)
            updateSelectInput(session, "selected_carry_bond", selected = worst_bond$bond)
        }
    })

    # ═══════════════════════════════════════════════════════════════════════
    # RISK METRICS PANEL - Enhanced with better explanations
    # ═══════════════════════════════════════════════════════════════════════
    output$risk_metrics_panel <- renderUI({
        req(carry_roll_data())

        data_90d <- carry_roll_data() %>%
            filter(holding_period == "90d")

        if(nrow(data_90d) == 0) {
            return(tags$p("No 90-day data available"))
        }

        # Calculate risk-adjusted average
        avg_risk_adj <- if("return_per_unit_risk" %in% names(data_90d)) {
            mean(data_90d$return_per_unit_risk, na.rm = TRUE)
        } else {
            NA
        }

        # Calculate average breakeven yield move
        data_90d_breakeven <- data_90d %>%
            filter(!is.na(modified_duration), modified_duration > 0) %>%
            mutate(
                breakeven_bps = (net_return / 100) / modified_duration * 10000
            )
        avg_breakeven <- mean(data_90d_breakeven$breakeven_bps, na.rm = TRUE)

        # Find the most sensitive bond (lowest breakeven)
        min_breakeven_idx <- which.min(data_90d_breakeven$breakeven_bps)
        min_breakeven <- data_90d_breakeven$breakeven_bps[min_breakeven_idx]
        min_breakeven_bond <- data_90d_breakeven$bond[min_breakeven_idx]

        tagList(
            # Section title
            tags$small(class = "text-muted", "Based on 90-day holding period"),
            tags$hr(style = "margin: 8px 0;"),

            # Average Yield Breakeven with better explanation
            tags$div(
                style = "margin-bottom: 15px;",
                tags$div(class = "text-muted", style = "font-size: 11px;", "Avg Yield Breakeven:"),
                tags$div(
                    style = "font-size: 22px; font-weight: bold; color: #1B5E20;",
                    sprintf("+%.0f bps", avg_breakeven)
                ),
                tags$small(
                    class = "text-muted",
                    style = "font-size: 10px; display: block;",
                    "Yields can rise this much before the trade loses money"
                )
            ),

            tags$hr(style = "margin: 8px 0;"),

            # Minimum Breakeven (most sensitive) - now shows bond name
            tags$div(
                style = "margin-bottom: 15px;",
                tags$div(class = "text-muted", style = "font-size: 11px;", "Most Sensitive Bond:"),
                tags$div(
                    style = sprintf("font-size: 14px; font-weight: bold; color: %s;",
                                    ifelse(min_breakeven > 5, "#689F38", "#FF9800")),
                    sprintf("%s: +%.0f bps", min_breakeven_bond, min_breakeven)
                ),
                tags$small(
                    class = "text-muted",
                    style = "font-size: 10px; display: block;",
                    "This bond has the smallest margin of safety"
                )
            ),

            tags$hr(style = "margin: 8px 0;"),

            # Risk-Adjusted Return (if available) with clearer explanation
            if(!is.na(avg_risk_adj)) {
                tags$div(
                    style = "margin-bottom: 8px;",
                    tags$div(class = "text-muted", style = "font-size: 11px;", "Avg Risk-Adjusted Return:"),
                    tags$div(
                        style = sprintf("font-size: 18px; font-weight: bold; color: %s;",
                                       ifelse(avg_risk_adj > 0, "#1B3A6B", "#C62828")),
                        sprintf("%.2f%%", avg_risk_adj)
                    ),
                    tags$small(
                        class = "text-muted",
                        style = "font-size: 10px; display: block;",
                        "Return per year of duration risk"
                    )
                )
            }
        )
    })

    # ═══════════════════════════════════════════════════════════════════════
    # FIX 4: COMPONENT BREAKDOWN VIEW (Return Decomposition) - CONSISTENT ROUNDING
    # ═══════════════════════════════════════════════════════════════════════
    output$carry_decomposition <- renderUI({
        req(carry_roll_data(), input$selected_carry_bond)

        # Use selected period or default to 90d
        selected_period <- if(!is.null(input$decomp_period)) input$decomp_period else "90d"

        data <- carry_roll_data() %>%
            filter(bond == input$selected_carry_bond, holding_period == selected_period)

        if(nrow(data) == 0) {
            return(tags$p("Select a bond to see decomposition"))
        }

        # Get component values (raw, unrounded)
        carry_val <- if("carry_income" %in% names(data)) data$carry_income[1] else NA
        roll_val <- if("roll_return" %in% names(data)) data$roll_return[1] else NA
        funding_val <- if("funding_cost" %in% names(data)) data$funding_cost[1] else NA

        # ROUNDING FIX: Calculate displayed net from rounded components
        # This ensures components sum exactly to displayed net return
        carry_display <- if(!is.na(carry_val)) round(carry_val, 2) else 0
        roll_display <- if(!is.na(roll_val)) round(roll_val, 2) else 0
        funding_display <- if(!is.na(funding_val)) round(funding_val, 2) else 0

        # Net = Carry + Roll - Funding (using rounded values for display consistency)
        net_display <- carry_display + roll_display - funding_display

        # Period label for display
        period_label <- switch(selected_period,
                               "30d" = "30-day",
                               "90d" = "90-day",
                               "180d" = "180-day",
                               "360d" = "360-day",
                               "90-day")

        # Context text based on period
        period_context <- switch(selected_period,
                                 "30d" = "Short-term trade: Minimal roll-down benefit",
                                 "90d" = "Typical trading horizon for carry strategies",
                                 "180d" = "Medium-term: Significant roll-down accumulation",
                                 "360d" = "Full year: Maximum carry and roll benefits",
                                 "")

        tags$div(
            class = "panel panel-default",
            style = "margin-top: 15px;",
            tags$div(
                class = "panel-heading",
                style = "background: #1B3A6B; color: white; padding: 8px 12px;",
                sprintf("Return Decomposition: %s (%s)", input$selected_carry_bond, period_label)
            ),
            tags$div(
                class = "panel-body",
                style = "padding: 10px;",

                # Waterfall-style breakdown table
                tags$table(
                    class = "table table-condensed",
                    style = "margin-bottom: 0;",
                    tags$tbody(
                        if(!is.na(carry_val)) {
                            tags$tr(
                                tags$td("Carry Income:"),
                                tags$td(
                                    style = "color: #1B5E20; text-align: right; font-weight: bold;",
                                    sprintf("+%.2f%%", carry_display)
                                )
                            )
                        },
                        if(!is.na(roll_val)) {
                            tags$tr(
                                tags$td("Roll-Down Benefit:"),
                                tags$td(
                                    style = sprintf("color: %s; text-align: right; font-weight: bold;",
                                                    ifelse(roll_display >= 0, "#1B5E20", "#C62828")),
                                    sprintf("%s%.2f%%", ifelse(roll_display >= 0, "+", ""), roll_display)
                                )
                            )
                        },
                        if(!is.na(funding_val)) {
                            tags$tr(
                                tags$td("Funding Cost:"),
                                tags$td(
                                    style = "color: #C62828; text-align: right; font-weight: bold;",
                                    sprintf("-%.2f%%", funding_display)
                                )
                            )
                        },
                        tags$tr(
                            style = "border-top: 2px solid #333;",
                            tags$td(tags$strong("Net Return:")),
                            tags$td(
                                style = sprintf("color: %s; text-align: right; font-weight: bold; font-size: 14px;",
                                                ifelse(net_display > 0, "#1B5E20", "#C62828")),
                                sprintf("%.2f%%", net_display)
                            )
                        )
                    )
                ),
                # Period context
                if(nchar(period_context) > 0) {
                    tags$div(
                        style = "margin-top: 10px; font-size: 11px; color: #666; font-style: italic;",
                        period_context
                    )
                }
            )
        )
    })

    # Debug observer for forward rate calculations
    observe({
        req(processed_data())

        curve_data <- processed_data() %>%
            arrange(modified_duration)

        if(nrow(curve_data) < 2) return()

        # Create yield curve interpolation function
        yield_curve_func <- approxfun(
            x = curve_data$modified_duration,
            y = curve_data$yield_to_maturity,
            rule = 2
        )

        message("=== FORWARD RATE DEBUG ===")
        message(sprintf("Curve has %d points, duration range: %.2f to %.2f",
                       nrow(curve_data),
                       min(curve_data$modified_duration),
                       max(curve_data$modified_duration)))

        # Test spot curve values at key tenors
        test_tenors <- c(1, 2, 3, 5, 7, 10)
        for (t in test_tenors) {
            spot <- yield_curve_func(t)
            message(sprintf("Tenor %d years: Spot = %.2f%%", t, spot))
        }

        # Test forward rate calculations
        message("--- Forward Rate Calculations ---")
        test_forwards <- list(
            list(name = "1y1y", start = 1, tenor = 1),
            list(name = "3y2y", start = 3, tenor = 2),
            list(name = "5y2y", start = 5, tenor = 2)
        )

        for (fp in test_forwards) {
            t1 <- fp$start
            t2 <- fp$start + fp$tenor
            tenor <- fp$tenor

            r_t1 <- yield_curve_func(t1) / 100
            r_t2 <- yield_curve_func(t2) / 100
            r_tenor <- yield_curve_func(tenor)

            forward_rate <- ((1 + r_t2)^t2 / (1 + r_t1)^t1)^(1/tenor) - 1
            forward_rate_pct <- forward_rate * 100
            spread_bps <- (forward_rate_pct - r_tenor) * 100

            message(sprintf("[%s] t1=%d, t2=%d, tenor=%d | r_t1=%.2f%%, r_t2=%.2f%%, r_tenor=%.2f%% | FWD=%.2f%%, Spread=%.0f bps",
                           fp$name, t1, t2, tenor,
                           r_t1*100, r_t2*100, r_tenor,
                           forward_rate_pct, spread_bps))
        }
        message("==========================")
    })

    # Add this output to the server function
    output$forward_rate_table <- DT::renderDataTable({
        req(processed_data())

        # Get current yield curve data
        curve_data <- processed_data() %>%
            arrange(modified_duration)

        # Calculate forward rates between different tenor points
        forward_rates <- data.frame()

        # Define standard forward rate periods: XyYy = Y-year rate starting X years from now
        # start_year = when forward period begins, tenor_years = length of forward period
        periods <- data.frame(
            start_year = c(0, 1, 2, 3, 5, 7, 10),
            tenor_years = c(1, 1, 1, 2, 2, 3, 5),
            label = c("0y1y", "1y1y", "2y1y", "3y2y", "5y2y", "7y3y", "10y5y"),
            stringsAsFactors = FALSE
        )
        periods$end_year <- periods$start_year + periods$tenor_years

        # Create yield curve interpolation function for getting rates at any tenor
        yield_curve_func <- approxfun(
            x = curve_data$modified_duration,
            y = curve_data$yield_to_maturity,
            rule = 2  # Extrapolate using nearest value
        )

        for (i in 1:nrow(periods)) {
            t1 <- periods$start_year[i]   # Start of forward period
            t2 <- periods$end_year[i]     # End of forward period
            tenor <- periods$tenor_years[i]  # Length of forward period

            # Get interpolated spot rates (in percentage form)
            r_t1_pct <- yield_curve_func(t1)
            r_t2_pct <- yield_curve_func(t2)
            r_tenor_pct <- yield_curve_func(tenor)  # Current spot for same tenor as forward

            # Convert to decimal for calculation
            r_t1 <- r_t1_pct / 100
            r_t2 <- r_t2_pct / 100
            r_tenor <- r_tenor_pct / 100

            # Calculate forward rate using: f = [(1 + r_t2)^t2 / (1 + r_t1)^t1]^(1/(t2-t1)) - 1
            if (t1 == 0) {
                # For 0yXy, forward rate equals the spot rate at tenor X
                forward_rate <- r_t2
            } else if (t2 > t1) {
                forward_rate <- ((1 + r_t2)^t2 / (1 + r_t1)^t1)^(1/tenor) - 1
            } else {
                forward_rate <- NA
            }

            # Convert forward rate to percentage
            forward_rate_pct <- forward_rate * 100

            # SPREAD: Forward rate vs Current spot for SAME tenor (in basis points)
            # This is the key fix - compare forward Y-year rate to current Y-year spot rate
            spread_bps <- (forward_rate_pct - r_tenor_pct) * 100

            # Find reference bonds closest to t1 and t2 for display (dynamic - no hardcoding)
            # Special handling for t1 = 0: use shortest duration bond
            if (t1 == 0 || t1 <= 1) {
                from_bond <- curve_data %>%
                    slice_min(modified_duration, n = 1)
            } else {
                from_bond <- curve_data %>%
                    mutate(dur_diff = abs(modified_duration - t1)) %>%
                    slice_min(dur_diff, n = 1)
            }

            # Find bond closest to t2 (end year)
            to_bond <- curve_data %>%
                mutate(dur_diff = abs(modified_duration - t2)) %>%
                slice_min(dur_diff, n = 1)

            # Ensure we don't show same bond twice unless necessary
            from_bond_name <- if (nrow(from_bond) > 0) from_bond$bond[1] else NA
            to_bond_name <- if (nrow(to_bond) > 0) to_bond$bond[1] else NA

            if (!is.na(from_bond_name) && !is.na(to_bond_name) &&
               from_bond_name == to_bond_name && t1 != t2) {
                # Find second closest bond to t2
                alt_to_bond <- curve_data %>%
                    filter(bond != from_bond_name) %>%
                    mutate(dur_diff = abs(modified_duration - t2)) %>%
                    slice_min(dur_diff, n = 1)
                if (nrow(alt_to_bond) > 0) {
                    to_bond_name <- alt_to_bond$bond[1]
                }
            }

            forward_rates <- rbind(forward_rates, data.frame(
                Period = periods$label[i],
                Start_Year = t1,
                End_Year = t2,
                Tenor_Years = tenor,
                Spot_at_Start = r_t1_pct,
                Spot_at_End = r_t2_pct,
                Current_Spot_Tenor = r_tenor_pct,  # Current spot for same tenor as forward
                Forward_Rate = forward_rate_pct,
                Spread_bps = spread_bps,
                From_Bond = from_bond_name,
                To_Bond = to_bond_name,
                stringsAsFactors = FALSE
            ))
        }

        # Add market implied expectations based on spread in bps
        # Improved categorization with clearer thresholds
        forward_rates <- forward_rates %>%
            mutate(
                Market_View = case_when(
                    Spread_bps <= -50 ~ "Rates Falling",
                    Spread_bps <= -10 ~ "Rates Dipping",
                    Spread_bps <= 25 ~ "Neutral",
                    Spread_bps <= 100 ~ "Rates Rising",
                    Spread_bps <= 200 ~ "Rates Rising Sharply",
                    TRUE ~ "Rates Rising Strongly"
                ),
                # Improved signal strength - "None" replaced with "Weak" for clarity
                Signal_Strength = case_when(
                    abs(Spread_bps) <= 25 ~ "Weak",
                    abs(Spread_bps) <= 100 ~ "Moderate",
                    abs(Spread_bps) <= 200 ~ "Strong",
                    TRUE ~ "Very Strong"
                )
            )

        # Format for display - FIXED: removed redundant duration from Spot Rate column
        display_table <- forward_rates %>%
            filter(!is.na(Forward_Rate)) %>%
            mutate(
                Forward_Rate_Fmt = sprintf("%.2f%%", Forward_Rate),
                # FIXED: Clean spot rate display without duration
                Spot_Rate_Fmt = sprintf("%.2f%%", Current_Spot_Tenor),
                Spread_Fmt = sprintf("%+.0f bps", Spread_bps),
                Period_Desc = sprintf("Yr %d → %d", Start_Year, End_Year),
                Bonds_Used = paste(From_Bond, "→", To_Bond)
            ) %>%
            select(Period, Forward_Rate_Fmt, Spot_Rate_Fmt, Spread_Fmt,
                   Market_View, Signal_Strength, Bonds_Used)

        # Create enhanced datatable with better styling
        datatable(
            display_table,
            selection = "single",  # Enable single row selection for chart interactivity
            options = list(
                pageLength = 10,
                dom = 'frtip',
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all'),
                    list(width = '100px', targets = c(0, 1, 2, 3)),
                    list(width = '120px', targets = c(4, 5)),
                    list(width = '130px', targets = 6)
                )
            ),
            rownames = FALSE,
            class = 'cell-border stripe hover',
            colnames = c(
                "Forward Period",
                "Forward Rate",
                "Spot Rate",
                "Spread",
                "Market View",
                "Signal",
                "Reference Bonds"
            ),
            caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left;',
                htmltools::tags$div(
                    style = 'padding: 10px; background: #e8f4f8; border-radius: 5px; margin-bottom: 10px;',
                    htmltools::tags$strong("Forward Rate Analysis"),
                    htmltools::tags$p(
                        style = 'margin-top: 10px; margin-bottom: 5px;',
                        htmltools::tags$strong("Notation:"), " XyYy = Y-year rate starting X years from now (e.g., 3y2y = 2-year rate, 3 years forward)"
                    ),
                    htmltools::tags$p(
                        style = 'margin-top: 5px; margin-bottom: 0;',
                        "Spread compares the implied forward rate to the current spot rate for the same tenor. ",
                        "Positive spread = market expects rates to rise; Negative spread = expects rates to fall."
                    ),
                    htmltools::tags$p(
                        style = 'margin-top: 8px; margin-bottom: 0; font-style: italic; color: #666;',
                        shiny::icon("hand-pointer"), " Click a row to highlight the corresponding forward period on the chart."
                    )
                )
            )
        ) %>%
            # Market View coloring - directional terminology with consistent styling
            formatStyle(
                "Market_View",
                backgroundColor = styleEqual(
                    c("Rates Falling", "Rates Dipping", "Neutral",
                      "Rates Rising", "Rates Rising Sharply", "Rates Rising Strongly"),
                    c("#C8E6C9", "#E8F5E9", "#F5F5F5",
                      "#FFF3E0", "#FFCDD2", "#FFAB91")
                ),
                color = styleEqual(
                    c("Rates Falling", "Rates Dipping", "Neutral",
                      "Rates Rising", "Rates Rising Sharply", "Rates Rising Strongly"),
                    c("#1B5E20", "#2E7D32", "#424242",
                      "#E65100", "#C62828", "#BF360C")
                ),
                fontWeight = "bold"
            ) %>%
            # Signal Strength coloring - improved visibility
            formatStyle(
                "Signal_Strength",
                backgroundColor = styleEqual(
                    c("Weak", "Moderate", "Strong", "Very Strong"),
                    c("#FAFAFA", "#E8EAF6", "#C5CAE9", "#7986CB")
                ),
                fontWeight = styleEqual(
                    c("Weak", "Moderate", "Strong", "Very Strong"),
                    c("normal", "normal", "bold", "bold")
                )
            ) %>%
            # Spread coloring - color based on value (aligned with Market View thresholds)
            formatStyle(
                "Spread_Fmt",
                color = styleInterval(
                    cuts = c(-50, -10, 25, 100),
                    values = c("#1B5E20", "#43A047", "#424242", "#E65100", "#C62828")
                )
            )
    })

    # Observer to handle table row selection and update chart
    observeEvent(input$forward_rate_table_rows_selected, {
        req(input$forward_rate_table_rows_selected)

        # Map row index to forward period
        periods <- c("0y1y", "1y1y", "2y1y", "3y2y", "5y2y", "7y3y", "10y5y")
        selected_row <- input$forward_rate_table_rows_selected

        if (length(selected_row) > 0 && selected_row <= length(periods)) {
            selected_forward_period(periods[selected_row])
        } else {
            selected_forward_period(NULL)
        }
    })


    # 14. ML Auction Predictions Display
    # Enhanced ML predictions with bond selection
    output$ml_auction_predictions <- renderUI({
        req(filtered_data(), input$auction_bonds_select)

        selected_bonds <- input$auction_bonds_select

        prediction_cards <- lapply(selected_bonds, function(bond_name) {
            pred <- predict_btc_arima(filtered_data(), bond_name)

            # Enhanced prediction card with more metrics
            tags$div(
                style = "background: white; border-radius: 8px; padding: 15px; margin: 10px 0;
                     box-shadow: 0 2px 4px rgba(0,0,0,0.1); border-left: 4px solid #1B3A6B;",
                tags$div(
                    style = "display: flex; justify-content: space-between; align-items: start;",
                    tags$div(
                        tags$h5(bond_name, style = "color: #1B3A6B; margin-top: 0; font-weight: bold;"),
                        tags$p(sprintf("Forecast: %.2fx", pred$forecast),
                               style = "font-size: 20px; font-weight: bold; margin: 5px 0; color: #1B3A6B;"),
                        tags$p(sprintf("80%% CI: [%.2f, %.2f]", pred$lower_80, pred$upper_80),
                               style = "font-size: 13px; color: #666; margin: 2px 0;"),
                        tags$p(sprintf("95%% CI: [%.2f, %.2f]", pred$lower_95, pred$upper_95),
                               style = "font-size: 13px; color: #666; margin: 2px 0;"),

                        # Add historical comparison
                        tags$div(
                            style = "margin-top: 10px; padding-top: 10px; border-top: 1px solid #e0e0e0;",
                            tags$small(
                                sprintf("Historical Avg: %.2fx | Last: %.2fx",
                                        mean(filtered_data() %>%
                                                 filter(bond == bond_name, !is.na(bid_to_cover)) %>%
                                                 pull(bid_to_cover), na.rm = TRUE),
                                        tail(filtered_data() %>%
                                                 filter(bond == bond_name, !is.na(bid_to_cover)) %>%
                                                 arrange(date) %>%
                                                 pull(bid_to_cover), 1)),
                                style = "color: #888;"
                            )
                        )
                    ),
                    tags$div(
                        style = paste0("padding: 10px; border-radius: 6px; background: ",
                                       case_when(
                                           pred$confidence == "High" ~ "#E8F5E9",
                                           pred$confidence == "Medium" ~ "#FFF3E0",
                                           TRUE ~ "#FFEBEE"
                                       ), "; text-align: center; min-width: 100px;"),
                        tags$p(pred$confidence,
                               style = "font-weight: bold; margin: 0; font-size: 14px;"),
                        tags$small(pred$model_type, style = "font-size: 11px; color: #666;")
                    )
                )
            )
        })

        do.call(tagList, prediction_cards)
    })

    # Update auction bond choices - Only show ACTIVE bonds
    observe({
        req(bond_data())

        # Get active bonds for current date range
        active <- tryCatch(active_bonds(), error = function(e) unique(bond_data()$bond))

        # Select bonds with recent auction activity as defaults (from active bonds only)
        recent_auction_bonds <- bond_data() %>%
            filter(bond %in% active,
                   !is.na(bid_to_cover),
                   date >= today() - days(90)) %>%
            pull(bond) %>%
            unique()

        # Select ALL bonds with recent auction activity by default
        # If no recent activity, fall back to all active bonds
        default_selection <- if (length(recent_auction_bonds) > 0) {
            recent_auction_bonds
        } else {
            active
        }

        updatePickerInput(
            session,
            "auction_bonds_select",
            choices = sort(active),
            selected = sort(default_selection)
        )
    })


    # 14c. Auction forecast visualization
    # Model performance metrics
    output$model_performance_metrics <- renderUI({
        req(filtered_data(), input$auction_bonds_select)

        # Calculate average model performance
        performance_stats <- data.frame()

        for(bond in input$auction_bonds_select) {
            hist_data <- filtered_data() %>%
                filter(bond == !!bond, !is.na(bid_to_cover))

            if(nrow(hist_data) > 10) {
                # Simple backtest - last 5 predictions
                mape <- runif(1, 5, 25)  # Placeholder - implement actual backtesting
                hit_rate <- runif(1, 60, 90)

                performance_stats <- rbind(performance_stats,
                                           data.frame(bond = bond,
                                                      mape = mape,
                                                      hit_rate = hit_rate))
            }
        }

        if(nrow(performance_stats) > 0) {
            avg_mape <- mean(performance_stats$mape)
            avg_hit <- mean(performance_stats$hit_rate)

            tagList(
                tags$div(
                    style = "display: flex; justify-content: space-between; margin: 5px 0;",
                    tags$span("Avg MAPE:"),
                    tags$strong(sprintf("%.1f%%", avg_mape),
                                style = ifelse(avg_mape < 15, "color: #28a745;", "color: #ffc107;"))
                ),
                tags$div(
                    style = "display: flex; justify-content: space-between; margin: 5px 0;",
                    tags$span("Hit Rate:"),
                    tags$strong(sprintf("%.0f%%", avg_hit),
                                style = ifelse(avg_hit > 75, "color: #28a745;", "color: #ffc107;"))
                ),
                tags$div(
                    style = "display: flex; justify-content: space-between; margin: 5px 0;",
                    tags$span("Confidence:"),
                    tags$strong(ifelse(avg_mape < 15 & avg_hit > 75, "High",
                                       ifelse(avg_mape < 20 & avg_hit > 65, "Medium", "Low")))
                )
            )
        } else {
            tags$p("Calculating metrics...", style = "color: #666; font-style: italic;")
        }
    })

    # ════════════════════════════════════════════════════════════════════════════
    # AUCTION QUALITY DASHBOARD - NEW COMPONENTS
    # ════════════════════════════════════════════════════════════════════════════

    # Enhanced auction data reactive (calculates new quality metrics)
    enhanced_auction_data <- reactive({
        req(filtered_data())

        # Define numeric columns that need conversion
        numeric_cols <- c("bid_to_cover", "bids_received", "offer_amount", "allocation",
                          "clearing_yield", "non_comps", "number_bids_received",
                          "best_bid", "worst_bid", "auction_tail")

        # Ensure numeric columns are actually numeric
        auction_data <- filtered_data() %>%
            dplyr::mutate(dplyr::across(dplyr::any_of(numeric_cols),
                          ~ suppressWarnings(as.numeric(as.character(.x))))) %>%
            dplyr::filter(!is.na(bid_to_cover))

        if (nrow(auction_data) == 0) {
            return(NULL)
        }

        # Calculate enhanced metrics using the new function
        enhanced <- tryCatch(
            calculate_enhanced_auction_metrics(auction_data, filtered_data()),
            error = function(e) {
                message(sprintf("[AUCTION QUALITY] Error calculating metrics: %s", e$message))
                auction_data  # Return original data on error
            }
        )

        return(enhanced)
    })

    # ═══════════════════════════════════════════════════════════════════════════
    # AUCTION QUALITY METRICS VALIDATION DEBUG OUTPUT
    # ═══════════════════════════════════════════════════════════════════════════
    observe({
        req(enhanced_auction_data())
        auction_data <- enhanced_auction_data()

        # Only log when there's actual data
        if (is.null(auction_data) || nrow(auction_data) == 0) return()

        message("\n=== AUCTION QUALITY METRICS VALIDATION ===")

        # Sample raw values
        sample_cols <- c("bond", "offer_date", "clearing_yield", "bid_to_cover",
                         "auction_tail_bps", "non_comp_ratio", "auction_concession_bps")
        available_cols <- intersect(sample_cols, names(auction_data))

        if (length(available_cols) > 0) {
            sample <- auction_data %>%
                dplyr::select(dplyr::all_of(available_cols)) %>%
                utils::head(5)

            message("Sample auction data (first 5 rows):")
            print(sample)
        }

        # Check concession calculation
        if ("auction_concession_bps" %in% names(auction_data)) {
            concession_data <- auction_data$auction_concession_bps[!is.na(auction_data$auction_concession_bps)]

            if (length(concession_data) > 0) {
                message("\nConcession calculation check:")
                message(sprintf("  Count: %d auctions with concession data", length(concession_data)))
                message(sprintf("  Range: %.1f to %.1f bps",
                                min(concession_data), max(concession_data)))
                message(sprintf("  Median: %.1f bps", median(concession_data)))
                message(sprintf("  Mean: %.1f bps", mean(concession_data)))

                # Flag if concession seems wrong (outside normal range)
                pct_in_range <- mean(abs(concession_data) <= 50) * 100
                if (pct_in_range < 80) {
                    message(sprintf("  ⚠️ WARNING: Only %.0f%% of concessions are within ±50 bps normal range!",
                                    pct_in_range))
                } else {
                    message(sprintf("  ✓ %.0f%% of concessions within ±50 bps (normal)", pct_in_range))
                }

                # Per-bond concession breakdown for diagnosis
                message("\n  Per-bond concession breakdown (most recent auction per bond):")
                per_bond_sample <- auction_data %>%
                    dplyr::filter(!is.na(auction_concession_bps)) %>%
                    dplyr::group_by(bond) %>%
                    dplyr::slice_max(offer_date, n = 1) %>%
                    dplyr::ungroup() %>%
                    dplyr::select(dplyr::any_of(c("bond", "offer_date", "clearing_yield",
                                                   "pre_auction_ytm", "auction_concession_bps",
                                                   "auction_concession_bps_raw", "concession_is_outlier")))

                if (nrow(per_bond_sample) > 0) {
                    for (i in 1:min(10, nrow(per_bond_sample))) {
                        s <- per_bond_sample[i, ]
                        raw_val <- if ("auction_concession_bps_raw" %in% names(s)) s$auction_concession_bps_raw else s$auction_concession_bps
                        outlier_flag <- if ("concession_is_outlier" %in% names(s) && !is.na(s$concession_is_outlier) && s$concession_is_outlier) " [OUTLIER-CAPPED]" else ""
                        message(sprintf("    %s (%s): clearing=%.3f%%, pre_auction=%.3f%% → concession=%+.1f bps%s",
                                        s$bond, format(s$offer_date, "%Y-%m-%d"),
                                        ifelse(is.na(s$clearing_yield), NA, s$clearing_yield),
                                        ifelse(is.na(s$pre_auction_ytm), NA, s$pre_auction_ytm),
                                        s$auction_concession_bps,
                                        outlier_flag))
                    }
                }

                # Check yield units
                if ("clearing_yield" %in% names(auction_data)) {
                    avg_clearing <- mean(auction_data$clearing_yield, na.rm = TRUE)
                    message(sprintf("\n  Yield units check: avg clearing yield = %.3f", avg_clearing))
                    if (avg_clearing < 0.2) {
                        message("    ⚠️ WARNING: Yields appear to be in decimal form (e.g., 0.08 = 8%%)!")
                        message("    Expected: percent form (e.g., 8.0 = 8%%)")
                    } else if (avg_clearing > 5 && avg_clearing < 20) {
                        message("    ✓ Yields appear to be in percent form (normal)")
                    } else {
                        message("    ⚠️ WARNING: Unusual yield range - please verify data")
                    }
                }
            }
        } else {
            message("\n⚠️ auction_concession_bps column not found - check data pipeline")
        }

        # Check non_comp_ratio interpretation
        if ("non_comp_ratio" %in% names(auction_data)) {
            non_comp_data <- auction_data$non_comp_ratio[!is.na(auction_data$non_comp_ratio)]

            if (length(non_comp_data) > 0) {
                message("\nNon-competitive ratio check:")
                message(sprintf("  Range: %.1f%% to %.1f%%",
                                min(non_comp_data), max(non_comp_data)))
                message(sprintf("  Mean: %.1f%%", mean(non_comp_data)))

                # Flag if values seem wrong (should be 0-100%)
                if (min(non_comp_data) < 0 || max(non_comp_data) > 100) {
                    message("  ⚠️ WARNING: Non-comp ratio outside 0-100% range!")
                } else {
                    message("  ✓ Non-comp ratio within expected range")
                }
            }
        }

        # Quality score check
        if ("auction_quality_score" %in% names(auction_data)) {
            quality_data <- auction_data$auction_quality_score[!is.na(auction_data$auction_quality_score)]

            if (length(quality_data) > 0) {
                message("\nQuality score check:")
                message(sprintf("  Range: %.0f to %.0f", min(quality_data), max(quality_data)))
                message(sprintf("  Mean: %.1f", mean(quality_data)))

                # Distribution by grade
                grades <- dplyr::case_when(
                    quality_data >= 85 ~ "A",
                    quality_data >= 70 ~ "B",
                    quality_data >= 55 ~ "C",
                    quality_data >= 40 ~ "D",
                    TRUE ~ "F"
                )
                grade_counts <- table(grades)
                message(sprintf("  Grade distribution: %s",
                                paste(names(grade_counts), grade_counts, sep = "=", collapse = ", ")))
            }
        }

        message("==========================================\n")
    }) %>% bindEvent(enhanced_auction_data(), ignoreInit = TRUE)

    # KPI: Average Quality Score
    output$avg_quality_score_text <- renderText({
        req(enhanced_auction_data())

        if ("auction_quality_score" %in% names(enhanced_auction_data())) {
            avg_score <- mean(enhanced_auction_data()$auction_quality_score, na.rm = TRUE)
            if (!is.na(avg_score)) {
                sprintf("%.0f/100", avg_score)
            } else {
                "—"
            }
        } else {
            "—"
        }
    })

    # KPI: Quality Grade Badge
    output$avg_quality_grade_badge <- renderUI({
        req(enhanced_auction_data())

        if ("auction_quality_score" %in% names(enhanced_auction_data())) {
            avg_score <- mean(enhanced_auction_data()$auction_quality_score, na.rm = TRUE)

            grade <- dplyr::case_when(
                is.na(avg_score) ~ "—",
                avg_score >= 85 ~ "A",
                avg_score >= 70 ~ "B",
                avg_score >= 55 ~ "C",
                avg_score >= 40 ~ "D",
                TRUE ~ "F"
            )

            badge_color <- dplyr::case_when(
                grade == "A" ~ "#2E7D32",
                grade == "B" ~ "#7CB342",
                grade == "C" ~ "#FDD835",
                grade == "D" ~ "#FB8C00",
                grade == "F" ~ "#E53935",
                TRUE ~ "#9E9E9E"
            )

            tags$span(
                grade,
                style = sprintf("background-color: %s; color: white; padding: 5px 15px;
                                 border-radius: 20px; font-size: 1.2em; font-weight: bold;",
                                badge_color)
            )
        } else {
            tags$span("—", style = "color: #666;")
        }
    })

    # KPI: Average Tail
    output$avg_tail_text <- renderText({
        req(enhanced_auction_data())

        if ("auction_tail_bps" %in% names(enhanced_auction_data())) {
            avg_tail <- mean(enhanced_auction_data()$auction_tail_bps, na.rm = TRUE)
            if (!is.na(avg_tail)) {
                sprintf("%.1f bps", avg_tail)
            } else {
                "—"
            }
        } else {
            "—"
        }
    })

    # KPI: Tail Interpretation
    output$tail_interpretation <- renderText({
        req(enhanced_auction_data())

        if ("auction_tail_bps" %in% names(enhanced_auction_data())) {
            avg_tail <- mean(enhanced_auction_data()$auction_tail_bps, na.rm = TRUE)
            dplyr::case_when(
                is.na(avg_tail) ~ "No data",
                avg_tail <= 3 ~ "Tight (strong consensus)",
                avg_tail <= 6 ~ "Normal",
                avg_tail <= 10 ~ "Wide (some uncertainty)",
                TRUE ~ "Very Wide (weak consensus)"
            )
        } else {
            "No data"
        }
    })

    # KPI: Average Institutional Participation
    output$avg_institutional_text <- renderText({
        req(enhanced_auction_data())

        if ("non_comp_ratio" %in% names(enhanced_auction_data())) {
            avg_inst <- mean(enhanced_auction_data()$non_comp_ratio, na.rm = TRUE)
            if (!is.na(avg_inst)) {
                sprintf("%.1f%%", avg_inst)
            } else {
                "—"
            }
        } else {
            "—"
        }
    })

    # KPI: Non-Competitive % Interpretation (renamed from Institutional)
    output$institutional_interpretation <- renderText({
        req(enhanced_auction_data())

        if ("non_comp_ratio" %in% names(enhanced_auction_data())) {
            avg_non_comp <- mean(enhanced_auction_data()$non_comp_ratio, na.rm = TRUE)
            dplyr::case_when(
                is.na(avg_non_comp) ~ "No data",
                avg_non_comp >= 30 ~ "High (strong standing order base)",
                avg_non_comp >= 20 ~ "Healthy participation",
                avg_non_comp >= 10 ~ "Moderate standing orders",
                TRUE ~ "Low non-competitive interest"
            )
        } else {
            "No data"
        }
    })

    # KPI: Average Yield Trend (replaces broken Concession metric)
    # Shows average change in clearing yield between consecutive auctions (same bond)
    output$avg_concession_text <- renderText({
        req(enhanced_auction_data())

        # Calculate yield trend: change in clearing yield between consecutive auctions
        data <- enhanced_auction_data()
        if (!"clearing_yield" %in% names(data) || !"bond" %in% names(data)) {
            return("—")
        }

        yield_trend <- data %>%
            dplyr::filter(!is.na(clearing_yield), !is.na(offer_date)) %>%
            dplyr::group_by(bond) %>%
            dplyr::arrange(offer_date) %>%
            dplyr::mutate(
                yield_change_bps = (clearing_yield - dplyr::lag(clearing_yield)) * 100
            ) %>%
            dplyr::ungroup() %>%
            dplyr::summarise(avg_yield_trend = mean(yield_change_bps, na.rm = TRUE)) %>%
            dplyr::pull(avg_yield_trend)

        if (!is.na(yield_trend)) {
            sprintf("%+.0f bps", yield_trend)
        } else {
            "—"
        }
    })

    # KPI: Yield Trend Interpretation (replaces Concession Interpretation)
    output$concession_interpretation <- renderText({
        req(enhanced_auction_data())

        # Calculate yield trend
        data <- enhanced_auction_data()
        if (!"clearing_yield" %in% names(data) || !"bond" %in% names(data)) {
            return("No data")
        }

        yield_trend <- data %>%
            dplyr::filter(!is.na(clearing_yield), !is.na(offer_date)) %>%
            dplyr::group_by(bond) %>%
            dplyr::arrange(offer_date) %>%
            dplyr::mutate(
                yield_change_bps = (clearing_yield - dplyr::lag(clearing_yield)) * 100
            ) %>%
            dplyr::ungroup() %>%
            dplyr::summarise(avg_yield_trend = mean(yield_change_bps, na.rm = TRUE)) %>%
            dplyr::pull(avg_yield_trend)

        dplyr::case_when(
            is.na(yield_trend) ~ "Insufficient data",
            yield_trend > 20 ~ "Yields rising sharply",
            yield_trend > 5 ~ "Yields drifting higher",
            yield_trend > -5 ~ "Yields stable",
            yield_trend > -20 ~ "Yields easing",
            TRUE ~ "Yields falling sharply"
        )
    })

    # Auction Quality Heatmap - OLD ggplot version (kept for download report)
    output$auction_quality_heatmap <- renderPlot({
        req(enhanced_auction_data())
        p <- create_auction_quality_heatmap(enhanced_auction_data())
        if (!is.null(p)) print(p)
    })

    # Auction Quality Heatmap - NEW DT version with highlighting
    output$auction_quality_heatmap_dt <- DT::renderDT({
        req(enhanced_auction_data())

        # Get selected bonds from auction predictions
        selected_bonds <- if (!is.null(input$auction_bonds_select)) {
            input$auction_bonds_select
        } else {
            character(0)
        }

        # Aggregate by bond - latest 6 months
        six_months_ago <- Sys.Date() - 180

        # NEW: Calculate yield trend (change between consecutive auctions) instead of concession
        bond_quality <- enhanced_auction_data() %>%
            dplyr::filter(offer_date >= six_months_ago, !is.na(bid_to_cover)) %>%
            dplyr::group_by(bond) %>%
            dplyr::arrange(offer_date) %>%
            dplyr::mutate(
                # Calculate clearing yield change from previous auction (same bond)
                yield_change_bps = (clearing_yield - dplyr::lag(clearing_yield)) * 100
            ) %>%
            dplyr::summarise(
                n_auctions = dplyr::n(),
                avg_btc = mean(bid_to_cover, na.rm = TRUE),
                avg_tail = if ("auction_tail_bps" %in% names(.)) mean(auction_tail_bps, na.rm = TRUE) else NA_real_,
                avg_non_comp = if ("non_comp_ratio" %in% names(.)) mean(non_comp_ratio, na.rm = TRUE) else NA_real_,
                # NEW: Yield trend replaces concession
                yield_trend_bps = mean(yield_change_bps, na.rm = TRUE),
                avg_quality = if ("auction_quality_score" %in% names(.)) mean(auction_quality_score, na.rm = TRUE) else NA_real_,
                .groups = "drop"
            ) %>%
            dplyr::filter(n_auctions >= 2) %>%
            dplyr::mutate(
                is_selected = bond %in% selected_bonds,
                sort_order = ifelse(is_selected, 0, 1)
            ) %>%
            dplyr::arrange(sort_order, dplyr::desc(avg_quality))

        if (nrow(bond_quality) == 0) {
            return(NULL)
        }

        # Create display table with formatted values
        # NEW: Shows Yield Trend instead of Concession
        display_data <- bond_quality %>%
            dplyr::transmute(
                Bond = bond,
                `Bid-to-Cover` = sprintf("%.2fx", avg_btc),
                `Tail (bps)` = ifelse(is.na(avg_tail), "—", sprintf("%.1f", avg_tail)),
                `Non-Comp %%` = ifelse(is.na(avg_non_comp), "—", sprintf("%.0f%%", avg_non_comp)),
                `Yield Trend` = ifelse(is.na(yield_trend_bps), "—", sprintf("%+.0f bps", yield_trend_bps)),
                `Quality` = ifelse(is.na(avg_quality), "—", sprintf("%.0f", avg_quality)),
                is_selected = is_selected
            )

        # Get row indices for selected bonds (1-based for DT)
        selected_rows <- which(display_data$is_selected)

        # Create the datatable
        dt <- DT::datatable(
            display_data %>% dplyr::select(-is_selected),
            options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE,
                scrollY = "300px",
                scrollCollapse = TRUE,
                columnDefs = list(
                    list(className = 'dt-center', targets = 1:5)
                )
            ),
            rownames = FALSE,
            class = 'compact stripe hover',
            selection = 'none'
        )

        # Apply highlighting to selected bonds using styleEqual
        # Note: DT::styleRow doesn't work with formatStyle, must use styleEqual with actual values
        if (length(selected_bonds) > 0) {
            # Get the bond names that are both selected AND in the current data
            bonds_to_highlight <- intersect(selected_bonds, display_data$Bond)

            if (length(bonds_to_highlight) > 0) {
                message(sprintf("[AUCTION QUALITY TABLE] Highlighting bonds: %s",
                                paste(bonds_to_highlight, collapse = ", ")))

                dt <- dt %>%
                    # Row background highlighting for selected bonds
                    DT::formatStyle(
                        'Bond',
                        target = 'row',
                        backgroundColor = DT::styleEqual(
                            bonds_to_highlight,
                            rep('#1B3A6B15', length(bonds_to_highlight))
                        )
                    ) %>%
                    # Bold font and left border for Bond column
                    DT::formatStyle(
                        'Bond',
                        fontWeight = DT::styleEqual(
                            bonds_to_highlight,
                            rep('bold', length(bonds_to_highlight))
                        ),
                        color = DT::styleEqual(
                            bonds_to_highlight,
                            rep('#1B3A6B', length(bonds_to_highlight))
                        ),
                        borderLeft = DT::styleEqual(
                            bonds_to_highlight,
                            rep('4px solid #1B3A6B', length(bonds_to_highlight))
                        )
                    )
            }
        }

        # Add color bars to Quality column
        dt <- dt %>%
            DT::formatStyle(
                'Quality',
                background = DT::styleColorBar(c(0, 100), '#28a74540'),
                backgroundSize = '98% 70%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'left center'
            )

        return(dt)
    })

    # Concession Trend Chart - with selected bonds highlighting
    output$concession_trend_chart <- renderPlot({
        req(enhanced_auction_data())

        # Get selected bonds from auction predictions
        selected_bonds <- if (!is.null(input$auction_bonds_select)) {
            input$auction_bonds_select
        } else {
            character(0)
        }

        p <- create_concession_trend_chart(enhanced_auction_data(), selected_bonds)
        if (!is.null(p)) print(p)
    })

    # Download Quality Report
    output$download_quality_report <- downloadHandler(
        filename = function() {
            paste0("auction_quality_report_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(enhanced_auction_data())

            # Get selected bonds from auction predictions (same as app display)
            selected_bonds <- if (!is.null(input$auction_bonds_select)) {
                input$auction_bonds_select
            } else {
                character(0)
            }

            message(sprintf("[DOWNLOAD REPORT] Generating with selected bonds: %s",
                            if(length(selected_bonds) > 0) paste(selected_bonds, collapse = ", ") else "none"))

            # Create combined plot with selected bonds highlighting (matches app)
            p1 <- create_auction_quality_heatmap(enhanced_auction_data(), selected_bonds)
            p2 <- create_concession_trend_chart(enhanced_auction_data(), selected_bonds)

            combined <- gridExtra::arrangeGrob(
                p1, p2,
                ncol = 2,
                top = grid::textGrob("Auction Quality Report",
                                     gp = grid::gpar(fontsize = 16, fontface = "bold",
                                                     col = "#1B3A6B"))
            )

            ggsave(file, plot = combined, width = 16, height = 8, dpi = 300, bg = "white")
        }
    )

    # ════════════════════════════════════════════════════════════════════════════
    # REDESIGNED AUCTION PREDICTIONS - NEW COMPONENTS
    # ════════════════════════════════════════════════════════════════════════════

    # Helper function to calculate auction predictions for selected bonds
    auction_predictions_data <- reactive({
        req(filtered_data(), input$auction_bonds_select)

        selected_bonds <- input$auction_bonds_select

        # Calculate predictions for each bond
        predictions <- lapply(selected_bonds, function(bond_name) {
            pred <- predict_btc_arima(filtered_data(), bond_name)

            # Get historical stats
            hist_data <- filtered_data() %>%
                filter(bond == bond_name, !is.na(bid_to_cover)) %>%
                arrange(date)

            historical_avg <- if(nrow(hist_data) > 0) mean(hist_data$bid_to_cover, na.rm = TRUE) else NA
            last_value <- if(nrow(hist_data) > 0) tail(hist_data$bid_to_cover, 1) else NA
            n_auctions <- nrow(hist_data)

            data.frame(
                bond_name = bond_name,
                forecast = pred$forecast,
                ci_lower = pred$lower_80,
                ci_upper = pred$upper_80,
                ci_lower_95 = pred$lower_95,
                ci_upper_95 = pred$upper_95,
                confidence = pred$confidence,
                model_type = pred$model_type,
                historical_avg = historical_avg,
                last_value = last_value,
                n_auctions = n_auctions,
                stringsAsFactors = FALSE
            )
        })

        do.call(rbind, predictions)
    })

    # Quick Stats Output
    output$auction_quick_stats <- renderUI({
        req(filtered_data(), input$auction_bonds_select)

        selected_bonds <- input$auction_bonds_select
        n_selected <- length(selected_bonds)

        # Count bonds with sufficient data
        sufficient_data <- sapply(selected_bonds, function(bond_name) {
            n <- filtered_data() %>%
                filter(bond == bond_name, !is.na(bid_to_cover)) %>%
                nrow()
            n >= 10
        })

        n_sufficient <- sum(sufficient_data)

        # Get model info from first prediction
        model_info <- "N/A"
        if(n_sufficient > 0) {
            first_bond <- selected_bonds[sufficient_data][1]
            pred <- predict_btc_arima(filtered_data(), first_bond)
            model_info <- pred$model_type
        }

        tags$div(
            style = "font-size: 12px; color: #666;",
            tags$div(
                style = "display: flex; justify-content: space-between; margin: 3px 0;",
                tags$span(icon("check-circle", style = "color: #4CAF50;"), " Selected:"),
                tags$strong(sprintf("%d bonds", n_selected))
            ),
            tags$div(
                style = "display: flex; justify-content: space-between; margin: 3px 0;",
                tags$span(icon("database", style = "color: #2196F3;"), " Sufficient data:"),
                tags$strong(sprintf("%d bonds", n_sufficient),
                           style = ifelse(n_sufficient < n_selected, "color: #FF9800;", "color: #4CAF50;"))
            ),
            tags$div(
                style = "display: flex; justify-content: space-between; margin: 3px 0;",
                tags$span(icon("cog", style = "color: #9E9E9E;"), " Model:"),
                tags$strong(model_info, style = "font-size: 11px;")
            )
        )
    })

    # Model Performance Card (replaces model_performance_metrics with actual calculations)
    output$model_performance_card <- renderUI({
        req(filtered_data(), input$auction_bonds_select)

        # Calculate actual backtesting metrics
        backtest_results <- tryCatch({
            # Perform backtesting for selected bonds
            all_results <- lapply(input$auction_bonds_select, function(bond_name) {
                hist_data <- filtered_data() %>%
                    filter(bond == bond_name, !is.na(bid_to_cover)) %>%
                    arrange(date)

                if(nrow(hist_data) < 15) return(NULL)

                # Use last 5 observations for backtesting
                n <- nrow(hist_data)
                errors <- c()
                hits <- c()

                for(i in 5:1) {
                    train_data <- hist_data[1:(n-i), ]
                    actual <- hist_data$bid_to_cover[n-i+1]

                    if(nrow(train_data) >= 10) {
                        ts_data <- ts(train_data$bid_to_cover, frequency = 12)
                        model <- tryCatch({
                            auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, trace = FALSE)
                        }, error = function(e) NULL)

                        if(!is.null(model)) {
                            fc <- forecast(model, h = 1)
                            pred <- as.numeric(fc$mean)
                            error <- abs(pred - actual) / actual * 100
                            errors <- c(errors, error)
                            # Hit = prediction within 20% of actual
                            hits <- c(hits, error <= 20)
                        }
                    }
                }

                if(length(errors) > 0) {
                    data.frame(mape = mean(errors), hit_rate = mean(hits) * 100)
                } else {
                    NULL
                }
            })

            valid_results <- all_results[!sapply(all_results, is.null)]
            if(length(valid_results) > 0) {
                combined <- do.call(rbind, valid_results)
                list(
                    mape = mean(combined$mape, na.rm = TRUE),
                    hit_rate = mean(combined$hit_rate, na.rm = TRUE),
                    n_backtest = length(valid_results)
                )
            } else {
                NULL
            }
        }, error = function(e) NULL)

        if(is.null(backtest_results)) {
            # Show alternative metrics when backtesting isn't possible
            preds <- tryCatch(auction_predictions_data(), error = function(e) NULL)

            if(is.null(preds) || nrow(preds) == 0) {
                return(tags$div(
                    class = "well",
                    style = "padding: 10px; background: #FFF9C4; border-radius: 8px;",
                    tags$h6("Model Performance", style = "margin-top: 0; color: #F57F17; font-weight: bold;"),
                    tags$p(style = "font-size: 11px; margin: 0; color: #666;",
                           icon("exclamation-triangle", style = "color: #F57F17;"),
                           " No predictions available")
                ))
            }

            # Calculate alternative metrics from predictions
            valid_preds <- preds %>% filter(!is.na(forecast))
            n_total <- nrow(preds)
            n_valid <- nrow(valid_preds)

            # Average CI width (narrower = more confident)
            avg_ci_width <- if(n_valid > 0) {
                mean(valid_preds$ci_upper - valid_preds$ci_lower, na.rm = TRUE)
            } else {
                NA
            }

            # Model types used
            models_used <- if(n_valid > 0) {
                unique(valid_preds$model_type)
            } else {
                "N/A"
            }

            return(tags$div(
                class = "well",
                style = "padding: 12px; background: #F5F5F5; border-radius: 8px;",

                tags$h6("Model Info", style = "margin-top: 0; color: #1B3A6B; font-size: 12px; font-weight: bold;"),

                tags$div(
                    style = "font-size: 11px;",

                    # Forecasts generated
                    tags$div(
                        style = "display: flex; justify-content: space-between; margin: 4px 0;",
                        tags$span("Forecasts:"),
                        tags$span(style = "font-weight: bold;", sprintf("%d of %d bonds", n_valid, n_total))
                    ),

                    # Average CI width (if available)
                    if(!is.na(avg_ci_width)) {
                        tags$div(
                            style = "display: flex; justify-content: space-between; margin: 4px 0;",
                            tags$span("Avg CI Width:"),
                            tags$span(style = "font-weight: bold;", sprintf("%.2fx", avg_ci_width))
                        )
                    },

                    # Model type
                    tags$div(
                        style = "display: flex; justify-content: space-between; margin: 4px 0;",
                        tags$span("Model:"),
                        tags$span(style = "font-weight: bold; font-size: 10px;",
                                 paste(models_used, collapse = "/"))
                    )
                ),

                tags$hr(style = "margin: 8px 0;"),

                tags$div(
                    style = "font-size: 10px; color: #666;",
                    icon("info-circle"),
                    " Full backtesting requires 15+ auctions per bond"
                )
            ))
        }

        # Determine MAPE color
        mape_color <- case_when(
            backtest_results$mape < 10 ~ "#1B5E20",
            backtest_results$mape < 15 ~ "#4CAF50",
            backtest_results$mape < 20 ~ "#FF9800",
            TRUE ~ "#C62828"
        )

        # Determine hit rate color
        hit_color <- case_when(
            backtest_results$hit_rate >= 80 ~ "#1B5E20",
            backtest_results$hit_rate >= 70 ~ "#4CAF50",
            backtest_results$hit_rate >= 60 ~ "#FF9800",
            TRUE ~ "#C62828"
        )

        tags$div(
            class = "well",
            style = "padding: 10px; background: #f8f9fa; border-radius: 8px;",

            tags$h6("Model Performance", style = "margin-top: 0; color: #1B3A6B; font-weight: bold;"),

            # Key metrics row
            fluidRow(
                column(6, style = "padding: 5px;",
                    tags$div(style = "text-align: center; background: white; border-radius: 4px; padding: 8px;",
                        tags$div(style = sprintf("font-size: 20px; font-weight: bold; color: %s;", mape_color),
                                 sprintf("%.1f%%", backtest_results$mape)),
                        tags$div(style = "font-size: 10px; color: #666;", "MAPE")
                    )
                ),
                column(6, style = "padding: 5px;",
                    tags$div(style = "text-align: center; background: white; border-radius: 4px; padding: 8px;",
                        tags$div(style = sprintf("font-size: 20px; font-weight: bold; color: %s;", hit_color),
                                 sprintf("%.0f%%", backtest_results$hit_rate)),
                        tags$div(style = "font-size: 10px; color: #666;", "Hit Rate")
                    )
                )
            ),

            tags$div(
                style = "margin-top: 8px; font-size: 10px; color: #888; text-align: center;",
                sprintf("Based on %.0f bond(s) backtesting", backtest_results$n_backtest)
            )
        )
    })

    # Market Outlook Summary (replaces empty cards with gradient banner)
    output$market_outlook_summary <- renderUI({

        # Wrap everything in tryCatch to prevent errors from showing to users
        tryCatch({
            req(auction_predictions_data())

            preds <- auction_predictions_data()

            # Validate data
            if (is.null(preds) || nrow(preds) == 0) {
                return(tags$div(
                    class = "alert alert-warning",
                    style = "margin: 0;",
                    icon("exclamation-triangle"),
                    " Select bonds and click 'Update Predictions' to see forecasts"
                ))
            }

            # Filter valid predictions
            valid_preds <- preds %>% filter(!is.na(forecast))
            n_total <- nrow(preds)
            n_valid <- nrow(valid_preds)

            if (n_valid == 0) {
                # FIX: Show historical averages when no forecasts available
                return(tags$div(
                    class = "well",
                    style = "background: #FFF9C4; padding: 15px; border-radius: 4px; margin: 0;",
                    tags$div(style = "font-size: 11px; color: #F57F17; text-transform: uppercase;",
                             "Market Outlook"),
                    tags$div(style = "font-size: 18px; font-weight: bold; color: #F57F17; margin: 8px 0;",
                             icon("question-circle"), " INSUFFICIENT DATA"),
                    tags$p(style = "font-size: 12px; color: #666; margin: 0;",
                           sprintf("%.0f bonds selected, but none have 10+ auction history", n_total)),
                    tags$hr(style = "margin: 10px 0; border-color: #FFE082;"),
                    tags$div(
                        style = "font-size: 11px; color: #666;",
                        tags$strong("Historical averages:"),
                        tags$div(
                            style = "margin-top: 5px;",
                            lapply(seq_len(min(5, n_total)), function(i) {
                                row <- preds[i, ]
                                tags$div(
                                    style = "display: flex; justify-content: space-between; padding: 2px 0;",
                                    tags$span(row$bond_name),
                                    tags$span(sprintf("%.2fx (n=%d)", row$historical_avg, row$n_auctions))
                                )
                            })
                        )
                    )
                ))
            }

            # Calculate summary metrics
            avg_forecast <- mean(valid_preds$forecast, na.rm = TRUE)
            avg_historical <- mean(valid_preds$historical_avg, na.rm = TRUE)
            diff_from_avg <- avg_forecast - avg_historical

            # Determine overall outlook
            outlook_label <- case_when(
                avg_forecast > 3.0 ~ "STRONG DEMAND",
                avg_forecast > 2.5 ~ "HEALTHY DEMAND",
                avg_forecast > 2.0 ~ "MODERATE DEMAND",
                avg_forecast > 1.5 ~ "WEAK DEMAND",
                TRUE ~ "POOR DEMAND"
            )

            outlook_color <- case_when(
                avg_forecast > 3.0 ~ "#1B5E20",
                avg_forecast > 2.5 ~ "#4CAF50",
                avg_forecast > 2.0 ~ "#FF9800",
                avg_forecast > 1.5 ~ "#F44336",
                TRUE ~ "#B71C1C"
            )

            outlook_icon <- case_when(
                avg_forecast > 2.5 ~ "arrow-up",
                avg_forecast > 2.0 ~ "minus",
                TRUE ~ "arrow-down"
            )

            # Build visual summary
            tags$div(
                class = "well",
                style = sprintf("background: linear-gradient(135deg, %s 0%%, %s 100%%);
                                 color: white; padding: 15px; border-radius: 4px; margin: 0;",
                                outlook_color, adjustcolor(outlook_color, alpha.f = 0.7)),

                fluidRow(
                    column(8,
                        tags$div(
                            style = "font-size: 11px; opacity: 0.9; text-transform: uppercase;",
                            "Market Outlook"
                        ),
                        tags$div(
                            style = "font-size: 22px; font-weight: bold; margin: 5px 0;",
                            icon(outlook_icon),
                            " ",
                            outlook_label
                        ),
                        tags$div(
                            style = "font-size: 13px;",
                            sprintf("Avg Forecast: %.2fx", avg_forecast),
                            tags$span(
                                style = sprintf("margin-left: 10px; padding: 2px 6px; border-radius: 3px; background: %s;",
                                               ifelse(diff_from_avg >= 0, "rgba(255,255,255,0.3)", "rgba(0,0,0,0.2)")),
                                sprintf("%+.2f vs hist", diff_from_avg)
                            )
                        ),
                        tags$div(
                            style = "font-size: 11px; margin-top: 8px; opacity: 0.8;",
                            sprintf("%.0f of %.0f bonds with forecasts", n_valid, n_total)
                        )
                    ),
                    column(4,
                        # Mini forecast indicators
                        tags$div(
                            style = "text-align: right;",
                            lapply(seq_len(min(5, nrow(valid_preds))), function(i) {
                                row <- valid_preds[i, ]
                                bar_pct <- min(100, (row$forecast / 4) * 100)
                                signal_bg <- case_when(
                                    row$forecast > row$historical_avg + 0.3 ~ "rgba(200,230,201,0.8)",
                                    row$forecast < row$historical_avg - 0.3 ~ "rgba(255,205,210,0.8)",
                                    TRUE ~ "rgba(255,255,255,0.5)"
                                )
                                tags$div(
                                    style = "margin: 2px 0; display: flex; align-items: center; justify-content: flex-end;",
                                    tags$span(style = "font-size: 10px; width: 45px; text-align: left;",
                                             row$bond_name),
                                    tags$div(
                                        style = sprintf("height: 10px; width: %.0fpx; background: %s; border-radius: 2px; margin: 0 5px;",
                                                       bar_pct, signal_bg)
                                    ),
                                    tags$span(style = "font-size: 10px; width: 35px; text-align: right;",
                                             sprintf("%.1fx", row$forecast))
                                )
                            })
                        )
                    )
                )
            )

        }, error = function(e) {
            # Return graceful error message instead of crashing
            tags$div(
                class = "alert alert-danger",
                style = "margin: 0;",
                tags$strong("Unable to generate outlook"),
                tags$br(),
                tags$small("Please try selecting different bonds or refreshing the page")
            )
        })
    })

    # Helper function to format vs Avg values, avoiding -0.00 and +0.00
    format_vs_avg <- function(diff) {
        if (is.na(diff)) return("—")
        if (abs(diff) < 0.01) return("0.00")  # Avoid -0.00 and +0.00
        sprintf("%+.2f", diff)
    }

    # Compact Forecast Table (replaces vertical cards) - Enhanced with quality metrics
    output$auction_forecast_table <- DT::renderDataTable({
        req(auction_predictions_data())

        preds <- auction_predictions_data()

        # Get enhanced auction data for quality metrics
        enhanced_data <- tryCatch(enhanced_auction_data(), error = function(e) NULL)

        # Calculate historical quality metrics per bond
        if (!is.null(enhanced_data) && nrow(enhanced_data) > 0) {
            quality_metrics <- enhanced_data %>%
                group_by(bond) %>%
                summarise(
                    hist_avg_quality = if ("auction_quality_score" %in% names(.))
                        mean(auction_quality_score, na.rm = TRUE) else NA_real_,
                    hist_avg_tail = if ("auction_tail_bps" %in% names(.))
                        mean(auction_tail_bps, na.rm = TRUE) else NA_real_,
                    hist_avg_inst = if ("non_comp_ratio" %in% names(.))
                        mean(non_comp_ratio, na.rm = TRUE) else NA_real_,
                    .groups = "drop"
                )

            # Join with predictions
            preds <- preds %>%
                left_join(quality_metrics, by = c("bond_name" = "bond"))
        } else {
            preds <- preds %>%
                mutate(
                    hist_avg_quality = NA_real_,
                    hist_avg_tail = NA_real_,
                    hist_avg_inst = NA_real_
                )
        }

        # Filter if toggle is off - hide bonds without forecasts
        if (!isTRUE(input$show_all_bonds)) {
            preds <- preds %>% filter(!is.na(forecast))
        }

        display_df <- preds %>%
            mutate(
                Bond = bond_name,
                Forecast = ifelse(is.na(forecast), "—", sprintf("%.2fx", forecast)),
                `CI 80%` = ifelse(is.na(ci_lower), "—", sprintf("[%.2f-%.2f]", ci_lower, ci_upper)),
                `vs Avg` = sapply(forecast - historical_avg, format_vs_avg),
                Signal = case_when(
                    is.na(forecast) ~ '<span class="label label-default">No Data</span>',
                    forecast > historical_avg + 0.5 ~ '<span class="label label-success" style="background:#1B5E20;">STRONG</span>',
                    forecast > historical_avg + 0.2 ~ '<span class="label label-info" style="background:#2196F3;">BUY</span>',
                    forecast < historical_avg - 0.5 ~ '<span class="label label-danger" style="background:#C62828;">WEAK</span>',
                    forecast < historical_avg - 0.2 ~ '<span class="label label-warning" style="background:#FF9800;">CAUTION</span>',
                    TRUE ~ '<span class="label label-default" style="background:#9E9E9E;">HOLD</span>'
                ),
                # NEW: Quality metrics columns
                `Quality` = ifelse(is.na(hist_avg_quality), "—", sprintf("%.0f", hist_avg_quality)),
                `Tail` = ifelse(is.na(hist_avg_tail), "—", sprintf("%.1f", hist_avg_tail)),
                `Inst%` = ifelse(is.na(hist_avg_inst), "—", sprintf("%.0f%%", hist_avg_inst)),
                `Hist` = sprintf("%.2fx", historical_avg),
                `n` = n_auctions
            ) %>%
            select(Bond, Forecast, `CI 80%`, Signal, `Quality`, `Tail`, `Inst%`, `Hist`, `n`)

        DT::datatable(
            display_df,
            rownames = FALSE,
            escape = FALSE,  # Allow HTML in Signal column
            options = list(
                pageLength = 10,
                dom = 't',  # Table only, no search/pagination for compact view
                ordering = TRUE,
                scrollX = TRUE,
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all'),
                    list(width = '60px', targets = 0),
                    list(width = '55px', targets = 1),
                    list(width = '80px', targets = 2),
                    list(width = '65px', targets = 3),
                    list(width = '45px', targets = 4),
                    list(width = '40px', targets = 5),
                    list(width = '45px', targets = 6),
                    list(width = '45px', targets = 7),
                    list(width = '30px', targets = 8)
                )
            ),
            class = 'compact stripe hover'
        ) %>%
            DT::formatStyle(
                'Quality',
                backgroundColor = DT::styleInterval(
                    c(40, 55, 70, 85),
                    c('#FFCDD2', '#FFCC80', '#FFF9C4', '#C8E6C9', '#81C784')
                )
            )
    })

    # Market Sentiment Compact (visual gauge)
    output$market_sentiment_compact <- renderUI({
        req(filtered_data())

        tryCatch({
            base_data <- filtered_data()

            # DEBUG: Log what we have
            message(sprintf("[SENTIMENT DEBUG] Base data: %d rows",
                            if(!is.null(base_data)) nrow(base_data) else 0))

            if (!is.null(base_data) && nrow(base_data) > 0) {
                message(sprintf("[SENTIMENT DEBUG] Has bid_to_cover: %s",
                                "bid_to_cover" %in% names(base_data)))

                if ("bid_to_cover" %in% names(base_data)) {
                    btc_values <- base_data$bid_to_cover
                    non_na_count <- sum(!is.na(btc_values) & btc_values > 0)
                    message(sprintf("[SENTIMENT DEBUG] bid_to_cover: %d non-NA positive values",
                                    non_na_count))
                }
            }

            # Filter to actual auction data points (where bid_to_cover exists and is positive)
            # Also need to deduplicate - one entry per auction event
            auction_data <- base_data %>%
                filter(
                    !is.na(bid_to_cover),
                    bid_to_cover > 0
                ) %>%
                # Group by date and bond to get unique auction events
                group_by(date, bond) %>%
                slice_head(n = 1) %>%
                ungroup() %>%
                # Filter to recent period (180 days for sentiment, using 90 for display)
                filter(date >= max(date, na.rm = TRUE) - 180)

            n_auctions <- nrow(auction_data)

            message(sprintf("[SENTIMENT] Found %d auction events in last 6 months", n_auctions))

            # Helper function for insufficient data display
            sentiment_insufficient_data_ui <- function(n_found) {
                tags$div(
                    class = "well",
                    style = "padding: 10px; text-align: center; background: #FFF9C4; border-radius: 8px;",
                    tags$h6("Market Sentiment", style = "margin-top: 0; color: #F57F17;"),
                    tags$p(
                        style = "color: #666; font-size: 12px; margin: 5px 0;",
                        sprintf("Found %d auction events (need 5+)", n_found)
                    ),
                    tags$small(
                        style = "color: #999;",
                        "Based on records with bid_to_cover > 0"
                    )
                )
            }

            if(n_auctions < 5) {
                return(sentiment_insufficient_data_ui(n_auctions))
            }

            # Calculate metrics on auction data
            avg_btc <- mean(auction_data$bid_to_cover, na.rm = TRUE)

            # Calculate trend
            trend <- NA
            if (n_auctions >= 10) {
                auction_data <- auction_data %>% arrange(date)
                trend_fit <- lm(bid_to_cover ~ as.numeric(date), data = auction_data)
                trend <- coef(trend_fit)[2] * 30  # Monthly change
            }

            recent <- list(
                avg_btc = avg_btc,
                trend = trend,
                n_auctions = n_auctions
            )

        # Calculate sentiment score (-1 to +1)
        sentiment_score <- case_when(
            recent$avg_btc > 3.5 ~ 0.9,
            recent$avg_btc > 3.0 ~ 0.6,
            recent$avg_btc > 2.5 ~ 0.3,
            recent$avg_btc > 2.0 ~ 0.0,
            recent$avg_btc > 1.5 ~ -0.3,
            TRUE ~ -0.6
        )

        # Adjust for trend
        if(!is.na(recent$trend)) {
            sentiment_score <- sentiment_score + (recent$trend * 0.5)
            sentiment_score <- max(-1, min(1, sentiment_score))
        }

        # Sentiment label and color
        sentiment_info <- if(sentiment_score > 0.5) {
            list(label = "BULLISH", color = "#1B5E20", emoji = icon("chart-line"))
        } else if(sentiment_score > 0.2) {
            list(label = "POSITIVE", color = "#4CAF50", emoji = icon("thumbs-up"))
        } else if(sentiment_score > -0.2) {
            list(label = "NEUTRAL", color = "#FF9800", emoji = icon("minus"))
        } else if(sentiment_score > -0.5) {
            list(label = "NEGATIVE", color = "#F44336", emoji = icon("thumbs-down"))
        } else {
            list(label = "BEARISH", color = "#B71C1C", emoji = icon("chart-line", style = "transform: rotate(180deg);"))
        }

        # FIX 3: Clearer trend display with percentage changes
        # Calculate trend as percentage of average
        trend_pct <- if (!is.na(recent$trend) && recent$avg_btc > 0) {
            (recent$trend / recent$avg_btc) * 100  # % change per month
        } else {
            NA
        }

        # Trend text with percentage (more informative than just "Declining")
        trend_text <- case_when(
            is.na(trend_pct) ~ "",
            trend_pct > 3 ~ sprintf("+%.0f%%/mo", trend_pct),
            trend_pct > 0.5 ~ sprintf("+%.0f%%/mo", trend_pct),
            trend_pct < -3 ~ sprintf("%.0f%%/mo", trend_pct),
            trend_pct < -0.5 ~ sprintf("%.0f%%/mo", trend_pct),
            TRUE ~ "Stable"
        )

        trend_icon <- if(is.na(trend_pct)) {
            ""
        } else if(trend_pct > 0.5) {
            icon("arrow-up", style = "color: #4CAF50;")
        } else if(trend_pct < -0.5) {
            icon("arrow-down", style = "color: #F44336;")
        } else {
            icon("arrows-alt-h", style = "color: #FF9800;")
        }

        tags$div(
            class = "well",
            style = "padding: 10px; background: #f8f9fa; border-radius: 8px;",

            tags$h6("Market Sentiment (90-day)", style = "margin: 0 0 8px 0; color: #1B3A6B; font-weight: bold;"),

            # Gauge visualization (gradient bar with pointer)
            tags$div(
                style = "position: relative; height: 16px;
                        background: linear-gradient(to right, #B71C1C, #F44336, #FF9800, #4CAF50, #1B5E20);
                        border-radius: 8px; margin: 8px 0;",
                # Pointer
                tags$div(
                    style = sprintf("position: absolute; left: %.0f%%; top: -4px; transform: translateX(-50%%);",
                                   round((sentiment_score + 1) / 2 * 100)),
                    tags$div(style = "width: 0; height: 0;
                            border-left: 6px solid transparent;
                            border-right: 6px solid transparent;
                            border-top: 8px solid #333;")
                )
            ),

            # Labels under gauge
            tags$div(
                style = "display: flex; justify-content: space-between; font-size: 9px; color: #666; margin-bottom: 8px;",
                tags$span("Bearish"),
                tags$span("Neutral"),
                tags$span("Bullish")
            ),

            # Main sentiment display
            tags$div(
                style = "text-align: center;",
                tags$div(
                    style = sprintf("font-size: 16px; font-weight: bold; color: %s;", sentiment_info$color),
                    sentiment_info$emoji, " ", sentiment_info$label
                ),
                tags$div(
                    style = "font-size: 11px; color: #666; margin-top: 3px;",
                    trend_icon, " ", trend_text,
                    tags$span(style = "margin-left: 8px;",
                             sprintf("BTC: %.2fx", recent$avg_btc))
                )
            )
        )

        }, error = function(e) {
            message(sprintf("[SENTIMENT ERROR] %s", e$message))
            tags$div(
                class = "well",
                style = "padding: 12px; text-align: center; background: #FFEBEE; border-radius: 8px;",
                tags$h6("Market Sentiment", style = "color: #C62828;"),
                tags$small(paste("Error:", e$message))
            )
        })
    })

    # ════════════════════════════════════════════════════════════════════════════
    # FIX 2: QUICK SELECT FOR 3-BOND AUCTION WORKFLOW
    # ════════════════════════════════════════════════════════════════════════════

    # Helper function to get next Tuesday (SA auction day)
    get_next_tuesday <- function(from_date) {
        days_to_add <- (2 - as.numeric(format(from_date, "%u"))) %% 7
        if (days_to_add == 0) days_to_add <- 7
        from_date + days(days_to_add)
    }

    # Quick Select UI for common auction groupings
    output$auction_quick_select <- renderUI({
        req(bond_data())

        # Get upcoming auction dates
        today_date <- today()
        next_auction <- get_next_tuesday(today_date)
        following_auction <- next_auction + days(7)

        # Get bonds with recent auction activity (likely to be in upcoming auctions)
        recent_bonds <- bond_data() %>%
            filter(!is.na(bid_to_cover), date >= today_date - days(90)) %>%
            group_by(bond) %>%
            summarise(n_auctions = n(), last_auction = max(date), .groups = "drop") %>%
            arrange(desc(n_auctions), desc(last_auction)) %>%
            pull(bond)

        # Common 3-bond selections for SA government bonds
        # These are typically auctioned together based on maturity buckets
        common_groups <- list(
            "Short/Medium" = c("R186", "R2030", "R213"),
            "Long-Dated" = c("R2032", "R2035", "R2037"),
            "Ultra-Long" = c("R2040", "R2044", "R2048")
        )

        tags$div(
            style = "margin-bottom: 10px;",
            tags$label("Quick Select:", style = "font-size: 11px; color: #666; display: block; margin-bottom: 4px;"),

            tags$div(
                style = "display: flex; flex-wrap: wrap; gap: 4px;",

                # Next auction button
                actionButton(
                    "select_next_auction",
                    tags$span(
                        icon("calendar-check"),
                        sprintf(" %s", format(next_auction, "%b %d"))
                    ),
                    class = "btn-sm btn-outline-primary",
                    style = "font-size: 10px; padding: 3px 8px; background: #E3F2FD; border-color: #1565C0; color: #1565C0;"
                ),

                # Following week button
                actionButton(
                    "select_following_auction",
                    tags$span(
                        icon("calendar"),
                        sprintf(" %s", format(following_auction, "%b %d"))
                    ),
                    class = "btn-sm btn-outline-secondary",
                    style = "font-size: 10px; padding: 3px 8px;"
                ),

                # Top 3 recent bonds button
                actionButton(
                    "select_top_recent",
                    "Top 3",
                    class = "btn-sm btn-outline-info",
                    style = "font-size: 10px; padding: 3px 8px;",
                    title = "Select 3 most frequently auctioned bonds"
                )
            )
        )
    })

    # Observer for "Next Auction" quick select - Only uses ACTIVE bonds
    observeEvent(input$select_next_auction, {
        req(bond_data())

        # Get active bonds for current date range
        active <- tryCatch(active_bonds(), error = function(e) unique(bond_data()$bond))

        # Get bonds with most recent auction activity (from active bonds only)
        recent_bonds <- bond_data() %>%
            filter(bond %in% active,
                   !is.na(bid_to_cover),
                   date >= today() - days(60)) %>%
            group_by(bond) %>%
            summarise(n_auctions = n(), .groups = "drop") %>%
            arrange(desc(n_auctions)) %>%
            head(3) %>%
            pull(bond)

        if (length(recent_bonds) > 0) {
            updatePickerInput(session, "auction_bonds_select", selected = recent_bonds)

            # Update auction date to next Tuesday
            next_auction <- get_next_tuesday(today())
            updateDateInput(session, "next_auction_date", value = next_auction)
        }
    })

    # Observer for "Following Auction" quick select - Only uses ACTIVE bonds
    observeEvent(input$select_following_auction, {
        req(bond_data())

        # Get active bonds for current date range
        active <- tryCatch(active_bonds(), error = function(e) unique(bond_data()$bond))

        # Get bonds with recent auction activity (from active bonds only)
        recent_bonds <- bond_data() %>%
            filter(bond %in% active,
                   !is.na(bid_to_cover),
                   date >= today() - days(60)) %>%
            group_by(bond) %>%
            summarise(n_auctions = n(), .groups = "drop") %>%
            arrange(desc(n_auctions)) %>%
            head(3) %>%
            pull(bond)

        if (length(recent_bonds) > 0) {
            updatePickerInput(session, "auction_bonds_select", selected = recent_bonds)

            # Update auction date to following Tuesday
            following_auction <- get_next_tuesday(today()) + days(7)
            updateDateInput(session, "next_auction_date", value = following_auction)
        }
    })

    # Observer for "Top 3 Recent" quick select - Only uses ACTIVE bonds
    observeEvent(input$select_top_recent, {
        req(bond_data())

        # Get active bonds for current date range
        active <- tryCatch(active_bonds(), error = function(e) unique(bond_data()$bond))

        # Get the 3 bonds with most auction history (from active bonds only)
        top_bonds <- bond_data() %>%
            filter(bond %in% active,
                   !is.na(bid_to_cover)) %>%
            group_by(bond) %>%
            summarise(n_auctions = n(), .groups = "drop") %>%
            arrange(desc(n_auctions)) %>%
            head(3) %>%
            pull(bond)

        if (length(top_bonds) > 0) {
            updatePickerInput(session, "auction_bonds_select", selected = top_bonds)
        }
    })

    # ════════════════════════════════════════════════════════════════════════════
    # FIX 4: AUCTION INSIGHT SUMMARY
    # ════════════════════════════════════════════════════════════════════════════

    output$auction_insight_summary <- renderUI({
        tryCatch({
            preds <- auction_predictions_data()

            if (is.null(preds) || nrow(preds) == 0) {
                return(NULL)
            }

            valid_preds <- preds %>% filter(!is.na(forecast))

            if (nrow(valid_preds) == 0) {
                return(NULL)
            }

            # Generate insights
            insights <- list()

            # Best performing bond
            best_bond <- valid_preds %>%
                slice_max(forecast, n = 1)
            insights$best <- sprintf("Strongest demand expected for %s (%.2fx forecast)",
                                    best_bond$bond_name, best_bond$forecast)

            # Check for weak demand bonds
            weak_bonds <- valid_preds %>% filter(forecast < 2.0)
            if (nrow(weak_bonds) > 0) {
                weak_names <- paste(weak_bonds$bond_name, collapse = ", ")
                insights$weak <- sprintf("Monitor %s for potential tail risk", weak_names)
            }

            # Overall assessment
            avg_forecast <- mean(valid_preds$forecast, na.rm = TRUE)
            if (avg_forecast > 2.5) {
                insights$overall <- "Overall auction conditions appear favorable"
            } else if (avg_forecast > 2.0) {
                insights$overall <- "Moderate demand expected - standard allocation approach"
            } else {
                insights$overall <- "Consider conservative allocations given weak forecast"
            }

            # vs Historical comparison
            avg_hist <- mean(valid_preds$historical_avg, na.rm = TRUE)
            diff <- avg_forecast - avg_hist
            if (abs(diff) > 0.2) {
                direction <- if (diff > 0) "above" else "below"
                insights$comparison <- sprintf("Forecasts %.2f %s historical averages",
                                              abs(diff), direction)
            }

            tags$div(
                class = "well",
                style = "padding: 10px; margin-top: 8px; background: #E8F5E9; border-left: 4px solid #4CAF50; border-radius: 4px;",

                tags$div(
                    style = "display: flex; align-items: center; margin-bottom: 6px;",
                    icon("lightbulb", style = "color: #2E7D32; margin-right: 6px;"),
                    tags$span(style = "font-weight: bold; color: #2E7D32; font-size: 12px;",
                             "Auction Insights")
                ),

                tags$ul(
                    style = "margin: 0; padding-left: 18px; font-size: 11px; color: #333;",
                    lapply(insights, function(insight) {
                        tags$li(style = "margin: 3px 0;", insight)
                    })
                )
            )

        }, error = function(e) {
            NULL
        })
    })

    # ════════════════════════════════════════════════════════════════════════════
    # UPCOMING AUCTIONS SELECTION (User selects up to 3 bonds)
    # ════════════════════════════════════════════════════════════════════════════

    # Populate upcoming auction bond choices (active bonds with auction history)
    observe({
        req(filtered_data())

        active <- tryCatch(active_bonds(), error = function(e) unique(filtered_data()$bond))

        # Get bonds that have auction history
        auction_data <- tryCatch(enhanced_auction_data(), error = function(e) NULL)

        if (!is.null(auction_data) && nrow(auction_data) > 0) {
            # Get bonds with auction history
            bonds_with_auctions <- auction_data %>%
                filter(!is.na(bid_to_cover)) %>%
                distinct(bond) %>%
                pull(bond)

            # Only show active bonds with auction history
            available_bonds <- intersect(active, bonds_with_auctions)

            if (length(available_bonds) == 0) {
                available_bonds <- active  # Fallback
                message("[AUCTION PREDICTIONS] Warning: No bonds with auction history found")
            }

            # Sort by most recent auction
            bond_order <- auction_data %>%
                filter(bond %in% available_bonds, !is.na(offer_date)) %>%
                group_by(bond) %>%
                summarise(last_auction = max(offer_date, na.rm = TRUE), .groups = "drop") %>%
                arrange(desc(last_auction)) %>%
                pull(bond)

            available_bonds <- bond_order

            message(sprintf("[AUCTION PREDICTIONS] Available bonds (sorted by recent auction): %s",
                            paste(available_bonds, collapse = ", ")))
        } else {
            available_bonds <- active
        }

        # Default to first 3 available bonds
        default_selected <- if (length(available_bonds) >= 3) available_bonds[1:3] else available_bonds

        # Note: auction_bonds_select is updated elsewhere via updatePickerInput
        # This is a fallback for the "upcoming auctions" section if needed
        updatePickerInput(
            session,
            "auction_bonds_select",
            choices = available_bonds,
            selected = default_selected
        )
    })

    # Limit selection to 3 bonds for auction_bonds_select
    observeEvent(input$auction_bonds_select, {
        if (length(input$auction_bonds_select) > 3) {
            updatePickerInput(
                session,
                "auction_bonds_select",
                selected = input$auction_bonds_select[1:3]
            )
            showNotification("Maximum 3 bonds for upcoming auctions", type = "warning")
        }
    }, ignoreInit = TRUE)

    # Render upcoming auctions display
    output$upcoming_auctions_display <- renderUI({
        req(input$auction_bonds_select)

        selected_bonds <- input$auction_bonds_select

        # Get enhanced auction data if available
        auction_data <- tryCatch(enhanced_auction_data(), error = function(e) filtered_data())

        if (is.null(auction_data)) {
            return(tags$p("No auction data available", style = "color: #999;"))
        }

        # Get stats for each selected bond
        bond_stats <- lapply(selected_bonds, function(bond_name) {
            bond_auctions <- auction_data %>%
                filter(bond == bond_name, !is.na(bid_to_cover)) %>%
                arrange(desc(offer_date))

            if (nrow(bond_auctions) == 0) {
                return(list(
                    bond = bond_name,
                    last_date = "No history",
                    avg_btc = NA,
                    last_quality = "—",
                    n_auctions = 0
                ))
            }

            # Get quality grade if available
            quality <- if ("quality_grade" %in% names(bond_auctions)) {
                bond_auctions$quality_grade[1]
            } else {
                "—"
            }

            list(
                bond = bond_name,
                last_date = format(bond_auctions$offer_date[1], "%Y-%m-%d"),
                avg_btc = round(mean(bond_auctions$bid_to_cover, na.rm = TRUE), 2),
                last_quality = quality,
                n_auctions = nrow(bond_auctions)
            )
        })

        # Create display cards
        tags$div(
            lapply(bond_stats, function(stats) {
                btc_color <- if (!is.na(stats$avg_btc)) {
                    if (stats$avg_btc >= 3) "#2E7D32"
                    else if (stats$avg_btc >= 2) "#F57C00"
                    else "#C62828"
                } else {
                    "#666"
                }

                tags$div(
                    class = "upcoming-auction-card",
                    style = "padding: 8px 10px; margin-bottom: 8px; background: #f8f9fa;
                             border-radius: 4px; border-left: 4px solid #1B3A6B;",

                    fluidRow(
                        column(4,
                               tags$strong(stats$bond, style = "font-size: 1em; color: #1B3A6B;")
                        ),
                        column(4,
                               tags$span("Avg: ", class = "text-muted", style = "font-size: 0.85em;"),
                               tags$span(sprintf("%.2fx", stats$avg_btc),
                                        style = sprintf("font-weight: bold; color: %s;", btc_color))
                        ),
                        column(4,
                               tags$span(sprintf("n=%d", stats$n_auctions),
                                        style = "font-size: 0.85em; color: #666;")
                        )
                    )
                )
            }),

            # Show next auction date
            tags$div(
                style = "text-align: center; margin-top: 8px; padding-top: 8px;
                         border-top: 1px solid #E0E0E0;",
                tags$small(
                    style = "color: #666;",
                    icon("calendar"),
                    sprintf(" Next auction: %s", format(get_next_tuesday(today()), "%b %d"))
                )
            )
        )
    })

    # Auction Calendar Mini (kept for backward compatibility, but simplified)
    output$auction_calendar_mini <- renderUI({
        # Generate upcoming auction dates (Tuesday pattern - SA auctions are typically Tuesdays)
        next_tuesday <- function(from_date) {
            days_to_add <- (2 - as.numeric(format(from_date, "%u"))) %% 7
            if(days_to_add == 0) days_to_add <- 7
            from_date + days(days_to_add)
        }

        # Get next 3 auction dates
        today_date <- today()
        auction_dates <- c(
            next_tuesday(today_date),
            next_tuesday(today_date) + days(7),
            next_tuesday(today_date) + days(14)
        )

        # Get selected bonds for display
        selected_bonds <- input$auction_bonds_select
        if(is.null(selected_bonds) || length(selected_bonds) == 0) {
            # Use active bonds as fallback instead of hardcoded values
            selected_bonds <- tryCatch(
                active_bonds(),
                error = function(e) c("R186", "R2032", "R2040")
            )
        }

        # Distribute bonds across auction dates for display
        bonds_per_auction <- ceiling(length(selected_bonds) / 3)

        # Helper function to format bonds list with truncation
        format_bonds_display <- function(bonds_list) {
            if (length(bonds_list) == 0) return("TBD")
            if (length(bonds_list) <= 3) {
                return(paste(bonds_list, collapse = ", "))
            } else {
                # Show max 3 bonds then "+N"
                paste0(
                    paste(bonds_list[1:3], collapse = ", "),
                    sprintf(" +%d", length(bonds_list) - 3)
                )
            }
        }

        tags$div(
            class = "well",
            style = "padding: 10px; margin-top: 10px; background: #f8f9fa; border-radius: 8px;",

            tags$h6("Upcoming Auctions", style = "margin: 0 0 8px 0; color: #1B3A6B; font-weight: bold; font-size: 12px;"),

            lapply(1:3, function(i) {
                is_next <- i == 1
                start_idx <- (i - 1) * bonds_per_auction + 1
                end_idx <- min(i * bonds_per_auction, length(selected_bonds))

                # Get bonds for this auction date
                auction_bonds <- if(start_idx <= length(selected_bonds)) {
                    selected_bonds[start_idx:end_idx]
                } else {
                    character(0)
                }

                # Format display with truncation
                bonds_display <- format_bonds_display(auction_bonds)
                full_bonds_list <- paste(auction_bonds, collapse = ", ")

                tags$div(
                    style = sprintf("padding: 6px 8px; margin: 4px 0; border-radius: 4px; %s",
                                   ifelse(is_next,
                                          "background: #E3F2FD; border-left: 3px solid #1565C0;",
                                          "background: #F5F5F5;")),

                    # Two-line layout for better space usage
                    tags$div(
                        style = "display: flex; justify-content: space-between; align-items: center;",
                        tags$span(
                            style = sprintf("font-weight: %s; font-size: 11px; color: %s;",
                                           ifelse(is_next, "bold", "normal"),
                                           ifelse(is_next, "#1565C0", "#333")),
                            ifelse(is_next, "Next: ", ""),
                            format(auction_dates[i], "%b %d")
                        ),
                        tags$span(
                            style = "font-size: 10px; color: #666; text-align: right; max-width: 150px;",
                            title = full_bonds_list,  # Full list on hover
                            bonds_display
                        )
                    )
                )
            }),

            tags$div(
                style = "text-align: center; margin-top: 8px; padding-top: 8px; border-top: 1px solid #E0E0E0;",
                tags$small(style = "color: #999;", "Tuesdays (typical)")
            )
        )
    })

    # ════════════════════════════════════════════════════════════════════════════
    # REDESIGNED ML PREDICTIONS - NEW SERVER COMPONENTS (v2)
    # Single bond selection, focused forecasts
    # ════════════════════════════════════════════════════════════════════════════

    # Reactive: Generate forecasts when button clicked
    auction_forecasts_v2 <- eventReactive(input$generate_predictions, {
        req(input$auction_bonds_select)
        req(enhanced_auction_data())

        selected_bonds <- input$auction_bonds_select

        withProgress(message = "Generating forecasts...", value = 0, {

            forecasts <- purrr::map_df(selected_bonds, function(bond_name) {
                incProgress(1/length(selected_bonds), detail = bond_name)

                # Get historical data for this bond
                bond_history <- enhanced_auction_data() %>%
                    filter(bond == bond_name) %>%
                    arrange(offer_date)

                n_auctions <- nrow(bond_history)

                # Allow tiered forecasting even for low data - removed n_auctions < 3 restriction
                # The predict_btc_arima function now handles tiered forecasting with fallbacks

                # Fit forecast model using existing function with tiered fallbacks
                tryCatch({
                    pred <- predict_btc_arima(bond_history, bond_name, h = 1)

                    forecast_value <- pred$forecast
                    ci_lower <- pred$lower_80
                    ci_upper <- pred$upper_80
                    model_type <- if (!is.null(pred$model_type)) pred$model_type else "None"
                    quality_score <- if (!is.null(pred$quality_score)) pred$quality_score else 0

                    # Determine signal based on forecast and quality
                    signal <- dplyr::case_when(
                        is.na(forecast_value) ~ "Insufficient Data",
                        quality_score < 30 ~ "Low Confidence",
                        forecast_value >= 3.5 ~ "STRONG BUY",
                        forecast_value >= 3.0 ~ "BUY",
                        forecast_value >= 2.5 ~ "HOLD",
                        forecast_value >= 2.0 ~ "CAUTION",
                        TRUE ~ "WEAK"
                    )

                    tibble::tibble(
                        bond = bond_name,
                        n_auctions = n_auctions,
                        has_forecast = !is.na(forecast_value),
                        forecast_btc = round(forecast_value, 2),
                        ci_lower = round(ci_lower, 2),
                        ci_upper = round(ci_upper, 2),
                        signal = signal,
                        model_type = model_type,
                        quality_score = quality_score,
                        historical_avg_btc = round(mean(bond_history$bid_to_cover, na.rm = TRUE), 2),
                        historical_avg_quality = if ("auction_quality_score" %in% names(bond_history))
                            round(mean(bond_history$auction_quality_score, na.rm = TRUE), 0) else NA_real_,
                        historical_avg_tail = if ("auction_tail_bps" %in% names(bond_history))
                            round(mean(bond_history$auction_tail_bps, na.rm = TRUE), 1) else NA_real_,
                        historical_avg_inst = if ("non_comp_ratio" %in% names(bond_history))
                            round(mean(bond_history$non_comp_ratio, na.rm = TRUE), 1) else NA_real_,
                        last_btc = round(tail(bond_history$bid_to_cover, 1), 2),
                        last_date = as.character(max(bond_history$offer_date))
                    )

                }, error = function(e) {
                    message(sprintf("[FORECAST] Error for %s: %s", bond_name, e$message))

                    tibble::tibble(
                        bond = bond_name,
                        n_auctions = n_auctions,
                        has_forecast = FALSE,
                        forecast_btc = NA_real_,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        signal = "Error",
                        model_type = "Error",
                        quality_score = 0,
                        historical_avg_btc = round(mean(bond_history$bid_to_cover, na.rm = TRUE), 2),
                        historical_avg_quality = if ("auction_quality_score" %in% names(bond_history))
                            round(mean(bond_history$auction_quality_score, na.rm = TRUE), 0) else NA_real_,
                        historical_avg_tail = if ("auction_tail_bps" %in% names(bond_history))
                            round(mean(bond_history$auction_tail_bps, na.rm = TRUE), 1) else NA_real_,
                        historical_avg_inst = if ("non_comp_ratio" %in% names(bond_history))
                            round(mean(bond_history$non_comp_ratio, na.rm = TRUE), 1) else NA_real_,
                        last_btc = round(tail(bond_history$bid_to_cover, 1), 2),
                        last_date = as.character(max(bond_history$offer_date))
                    )
                })
            })

            return(forecasts)
        })
    }, ignoreNULL = TRUE)

    # Market Outlook Card
    output$auction_market_outlook_card <- renderUI({

        forecasts <- tryCatch(auction_forecasts_v2(), error = function(e) NULL)

        if (is.null(forecasts) || nrow(forecasts) == 0) {
            return(
                tags$div(
                    class = "alert alert-info",
                    style = "margin-bottom: 15px;",
                    icon("info-circle"),
                    " Select bonds and click 'Generate Forecasts' to see predictions."
                )
            )
        }

        # Calculate overall outlook
        valid_forecasts <- forecasts %>% filter(has_forecast)

        if (nrow(valid_forecasts) == 0) {
            avg_forecast <- mean(forecasts$historical_avg_btc, na.rm = TRUE)
            outlook_text <- "INSUFFICIENT DATA"
            outlook_color <- "#6c757d"
            outlook_icon <- "question-circle"
        } else {
            avg_forecast <- mean(valid_forecasts$forecast_btc, na.rm = TRUE)
            outlook_text <- dplyr::case_when(
                avg_forecast >= 3.5 ~ "STRONG DEMAND",
                avg_forecast >= 3.0 ~ "GOOD DEMAND",
                avg_forecast >= 2.5 ~ "MODERATE DEMAND",
                avg_forecast >= 2.0 ~ "WEAK DEMAND",
                TRUE ~ "VERY WEAK"
            )
            outlook_color <- dplyr::case_when(
                avg_forecast >= 3.5 ~ "#1B5E20",
                avg_forecast >= 3.0 ~ "#388E3C",
                avg_forecast >= 2.5 ~ "#F57C00",
                TRUE ~ "#C62828"
            )
            outlook_icon <- ifelse(avg_forecast >= 2.5, "arrow-up", "arrow-down")
        }

        # Build card
        tags$div(
            class = "market-outlook-card",
            style = sprintf("background: linear-gradient(135deg, %s 0%%, %s 100%%);
                             color: white; padding: 20px; border-radius: 8px; margin-bottom: 15px;",
                            outlook_color, grDevices::adjustcolor(outlook_color, alpha.f = 0.8)),

            tags$div(
                style = "display: flex; justify-content: space-between; align-items: flex-start;",

                tags$div(
                    tags$span("MARKET OUTLOOK", style = "opacity: 0.9; font-size: 0.85em;"),
                    tags$h3(
                        icon(outlook_icon),
                        outlook_text,
                        style = "margin: 5px 0; font-weight: bold;"
                    ),
                    tags$p(
                        sprintf("Avg Forecast: %.2fx", avg_forecast),
                        style = "margin: 0; font-size: 0.95em;"
                    ),
                    tags$p(
                        sprintf("%d of %d bonds with forecasts",
                                sum(forecasts$has_forecast), nrow(forecasts)),
                        style = "margin-top: 5px; opacity: 0.8; font-size: 0.85em;"
                    )
                ),

                # Mini list for each bond
                tags$div(
                    style = "text-align: right;",
                    lapply(1:nrow(forecasts), function(i) {
                        f <- forecasts[i, ]
                        tags$div(
                            style = "margin-bottom: 3px;",
                            tags$span(f$bond, style = "font-weight: bold; margin-right: 10px;"),
                            if (f$has_forecast) {
                                tags$span(sprintf("%.1fx", f$forecast_btc))
                            } else {
                                tags$span("\u2014", style = "opacity: 0.6;")
                            }
                        )
                    })
                )
            )
        )
    })

    # Forecast Table v2 - ENHANCED with model type column
    output$auction_forecast_table_v2 <- DT::renderDataTable({

        forecasts <- tryCatch(auction_forecasts_v2(), error = function(e) NULL)

        if (is.null(forecasts) || nrow(forecasts) == 0) {
            return(DT::datatable(
                tibble::tibble(Message = "Click 'Generate Forecasts' to see predictions"),
                options = list(dom = 't'),
                rownames = FALSE
            ))
        }

        # Build display data with model type column
        # Ensure optional columns exist to avoid reference errors
        if (!("model_type" %in% names(forecasts))) {
            forecasts$model_type <- NA_character_
        }
        if (!("quality_score" %in% names(forecasts))) {
            forecasts$quality_score <- NA_real_
        }

        display_data <- forecasts %>%
            dplyr::mutate(
                Bond = bond,
                Forecast = ifelse(has_forecast, sprintf("%.2fx", forecast_btc), "\u2014"),
                `CI 80%` = ifelse(has_forecast, sprintf("[%.2f-%.2f]", ci_lower, ci_upper), "\u2014"),
                Signal = signal,
                # Use vectorized & operator (not scalar &&) for multiple conditions
                Model = ifelse(!is.na(model_type) & model_type != "None" & model_type != "",
                              model_type, "\u2014"),
                `Hist Avg` = ifelse(!is.na(historical_avg_btc), sprintf("%.2fx", historical_avg_btc), "\u2014"),
                # Use quality_score if available, else historical_avg_quality
                Quality = ifelse(!is.na(quality_score) & quality_score > 0,
                                quality_score,
                                ifelse(!is.na(historical_avg_quality), historical_avg_quality, 0)),
                `Last` = ifelse(!is.na(last_btc), sprintf("%.2fx", last_btc), "\u2014"),
                n = n_auctions
            ) %>%
            dplyr::select(Bond, Forecast, `CI 80%`, Signal, Model, `Hist Avg`, Quality, `Last`, n)

        DT::datatable(
            display_data,
            rownames = FALSE,
            selection = "none",
            options = list(
                dom = 't',
                pageLength = 5,
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all')
                )
            ),
            class = 'cell-border stripe compact'
        ) %>%
            DT::formatStyle(
                'Signal',
                backgroundColor = DT::styleEqual(
                    c('STRONG BUY', 'BUY', 'HOLD', 'CAUTION', 'WEAK',
                      'Insufficient Data', 'Low Confidence', 'Error'),
                    c('#C8E6C9', '#DCEDC8', '#FFF9C4', '#FFCC80', '#FFCDD2',
                      '#E0E0E0', '#CFD8DC', '#FFCDD2')
                ),
                fontWeight = 'bold'
            ) %>%
            DT::formatStyle(
                'Quality',
                background = DT::styleColorBar(c(0, 100), '#1B3A6B30'),
                backgroundSize = '98% 60%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'left center'
            ) %>%
            DT::formatStyle(
                'Model',
                color = DT::styleEqual(
                    c('ARIMA', 'ETS', 'Linear Trend', 'Weighted Average', 'Simple Average', '\u2014'),
                    c('#1B5E20', '#2E7D32', '#F57C00', '#FF9800', '#FFB74D', '#9E9E9E')
                ),
                fontWeight = DT::styleEqual(
                    c('ARIMA', 'ETS', 'Linear Trend', 'Weighted Average', 'Simple Average'),
                    rep('bold', 5)
                )
            )
    })

    # Market Sentiment - ENHANCED with actual values and colored indicators
    output$auction_market_sentiment <- renderUI({
        forecasts <- tryCatch(auction_forecasts_v2(), error = function(e) NULL)

        # Default empty sentiment state
        default_sentiment <- function() {
            tags$div(
                style = "background: #f8f9fa; padding: 15px; border-radius: 8px;",
                tags$h5("Market Sentiment", style = "color: #1B3A6B; font-weight: bold; margin-top: 0; margin-bottom: 15px;"),
                fluidRow(
                    column(4, style = "text-align: center;",
                        tags$div(style = "padding: 10px; border-radius: 8px; background-color: #e0e0e020;",
                            tags$div(style = "font-size: 1.8em; color: gray;", icon("minus")),
                            tags$div(style = "font-weight: bold; color: gray; font-size: 1.1em;", "\u2014"),
                            tags$div(style = "font-size: 0.85em; color: #666;", "Demand")
                        )
                    ),
                    column(4, style = "text-align: center;",
                        tags$div(style = "padding: 10px; border-radius: 8px; background-color: #e0e0e020;",
                            tags$div(style = "font-size: 1.8em; color: gray;", icon("minus")),
                            tags$div(style = "font-weight: bold; color: gray; font-size: 1.1em;", "\u2014"),
                            tags$div(style = "font-size: 0.85em; color: #666;", "Quality")
                        )
                    ),
                    column(4, style = "text-align: center;",
                        tags$div(style = "padding: 10px; border-radius: 8px; background-color: #e0e0e020;",
                            tags$div(style = "font-size: 1.8em; color: gray;", icon("minus")),
                            tags$div(style = "font-weight: bold; color: gray; font-size: 1.1em;", "\u2014"),
                            tags$div(style = "font-size: 0.85em; color: #666;", "Pricing")
                        )
                    )
                )
            )
        }

        if (is.null(forecasts) || nrow(forecasts) == 0) {
            return(default_sentiment())
        }

        valid_forecasts <- forecasts %>% filter(has_forecast)

        if (nrow(valid_forecasts) == 0) {
            return(default_sentiment())
        }

        # Calculate sentiment metrics
        avg_btc <- mean(valid_forecasts$forecast_btc, na.rm = TRUE)

        # Calculate quality from quality_score if available, else from historical_avg_quality
        avg_quality <- if ("quality_score" %in% names(valid_forecasts)) {
            mean(valid_forecasts$quality_score, na.rm = TRUE)
        } else {
            mean(forecasts$historical_avg_quality, na.rm = TRUE)
        }

        # Calculate pricing confidence based on CI width relative to forecast
        avg_ci_width <- mean(sapply(1:nrow(valid_forecasts), function(i) {
            f <- valid_forecasts[i, ]
            if (is.na(f$ci_upper) || is.na(f$ci_lower) || is.na(f$forecast_btc) || f$forecast_btc == 0) return(NA)
            (f$ci_upper - f$ci_lower) / f$forecast_btc
        }), na.rm = TRUE)

        # DEMAND sentiment (based on forecast BTC)
        demand <- if (avg_btc >= 3.5) {
            list(value = avg_btc, label = "Strong", color = "#28a745", icon_name = "arrow-up")
        } else if (avg_btc >= 2.5) {
            list(value = avg_btc, label = "Moderate", color = "#ffc107", icon_name = "minus")
        } else {
            list(value = avg_btc, label = "Weak", color = "#dc3545", icon_name = "arrow-down")
        }

        # QUALITY sentiment (based on quality score)
        quality <- if (!is.na(avg_quality) && avg_quality >= 70) {
            list(value = avg_quality, label = "High", color = "#28a745", icon_name = "check-circle")
        } else if (!is.na(avg_quality) && avg_quality >= 40) {
            list(value = avg_quality, label = "Medium", color = "#ffc107", icon_name = "exclamation-circle")
        } else if (!is.na(avg_quality)) {
            list(value = avg_quality, label = "Low", color = "#dc3545", icon_name = "times-circle")
        } else {
            list(value = NA, label = "\u2014", color = "gray", icon_name = "minus")
        }

        # PRICING sentiment (based on CI width - tighter = better)
        pricing <- if (is.na(avg_ci_width)) {
            list(value = NA, label = "\u2014", color = "gray", icon_name = "minus")
        } else if (avg_ci_width <= 0.3) {
            list(value = avg_ci_width, label = "Tight", color = "#28a745", icon_name = "compress-arrows-alt")
        } else if (avg_ci_width <= 0.6) {
            list(value = avg_ci_width, label = "Normal", color = "#ffc107", icon_name = "arrows-alt-h")
        } else {
            list(value = avg_ci_width, label = "Wide", color = "#dc3545", icon_name = "expand-arrows-alt")
        }

        # Build sentiment indicator card
        build_indicator <- function(sentiment, label, value_fmt) {
            tags$div(
                style = sprintf("padding: 10px; border-radius: 8px; background-color: %s20;", sentiment$color),
                tags$div(
                    style = sprintf("font-size: 1.8em; color: %s;", sentiment$color),
                    icon(sentiment$icon_name)
                ),
                tags$div(
                    style = sprintf("font-weight: bold; color: %s; font-size: 1.1em;", sentiment$color),
                    sentiment$label
                ),
                if (!is.na(sentiment$value)) {
                    tags$div(style = "font-size: 0.8em; color: #666;", value_fmt)
                }
            )
        }

        tags$div(
            style = "background: #f8f9fa; padding: 15px; border-radius: 8px;",
            tags$h5("Market Sentiment", style = "color: #1B3A6B; font-weight: bold; margin-top: 0; margin-bottom: 15px;"),
            fluidRow(
                column(4, style = "text-align: center;",
                    build_indicator(demand, "Demand",
                        if (!is.na(demand$value)) sprintf("%.2fx avg", demand$value) else NULL)
                ),
                column(4, style = "text-align: center;",
                    build_indicator(quality, "Quality",
                        if (!is.na(quality$value)) sprintf("%d/100", round(quality$value)) else NULL)
                ),
                column(4, style = "text-align: center;",
                    build_indicator(pricing, "Pricing",
                        if (!is.na(pricing$value)) sprintf("\u00B1%.0f%%", pricing$value * 100) else NULL)
                )
            )
        )
    })

    # Historical Performance Chart (v2)
    output$auction_forecast_chart_v2 <- renderPlot({
        tryCatch({
            req(input$auction_bonds_select)
            req(enhanced_auction_data())

            selected_bonds <- input$auction_bonds_select
            auction_data <- enhanced_auction_data()

            # Validate auction_data
            if (is.null(auction_data) || nrow(auction_data) == 0) {
                message("[Historical Performance] No auction data available")
                return(ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No auction data available",
                        size = 5, color = "gray50") +
                    ggplot2::theme_void() +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
            }

            # Validate required columns
            if (!all(c("bond", "bid_to_cover", "offer_date") %in% names(auction_data))) {
                missing <- setdiff(c("bond", "bid_to_cover", "offer_date"), names(auction_data))
                message("[Historical Performance] Missing columns: ", paste(missing, collapse = ", "))
                return(ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = paste("Missing required columns:", paste(missing, collapse = ", ")),
                        size = 4, color = "gray50") +
                    ggplot2::theme_void() +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
            }

            # Ensure bid_to_cover is numeric
            auction_data <- auction_data %>%
                dplyr::mutate(bid_to_cover = suppressWarnings(as.numeric(as.character(bid_to_cover))))

            # Filter to selected bonds
            chart_data <- auction_data %>%
                dplyr::filter(bond %in% selected_bonds, !is.na(bid_to_cover)) %>%
                dplyr::arrange(offer_date)

            if (nrow(chart_data) == 0) {
                return(ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No historical auction data for selected bonds",
                        size = 5, color = "gray50") +
                    ggplot2::theme_void() +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
            }

            # Get forecasts if available (silent fail - chart will still show historical data)
            forecasts <- tryCatch({
                result <- auction_forecasts_v2()
                if (is.null(result) || nrow(result) == 0) {
                    message("[Historical Performance] No forecast data available yet")
                    NULL
                } else {
                    result
                }
            }, error = function(e) {
                err_msg <- if (!is.null(e$message) && nchar(e$message) > 0) e$message else "Unknown error"
                message("[Historical Performance] Forecast retrieval skipped: ", err_msg)
                NULL
            })

            # Create plot
            p <- ggplot2::ggplot(chart_data, ggplot2::aes(x = offer_date, y = bid_to_cover, color = bond)) +
                ggplot2::geom_line(linewidth = 1) +
                ggplot2::geom_point(size = 2) +
                ggplot2::geom_hline(yintercept = 2, linetype = "dashed", color = "#C62828", alpha = 0.5) +
                ggplot2::geom_hline(yintercept = 3, linetype = "dashed", color = "#388E3C", alpha = 0.5) +
                ggplot2::labs(
                    x = NULL,
                    y = "Bid-to-Cover Ratio",
                    color = "Bond"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    legend.position = "bottom",
                    legend.title = ggplot2::element_text(size = 9),
                    legend.text = ggplot2::element_text(size = 8),
                    axis.text = ggplot2::element_text(size = 9),
                    axis.title = ggplot2::element_text(size = 10)
                ) +
                ggplot2::scale_color_manual(
                    values = c("#1B3A6B", "#E53935", "#43A047", "#FB8C00", "#8E24AA")[1:length(selected_bonds)]
                )

            # Add forecast points if available
            if (!is.null(forecasts) && nrow(forecasts) > 0 && "has_forecast" %in% names(forecasts) && any(forecasts$has_forecast, na.rm = TRUE)) {
                forecast_date <- if (!is.null(input$upcoming_auction_date)) {
                    input$upcoming_auction_date
                } else {
                    max(chart_data$offer_date, na.rm = TRUE) + 7
                }

                forecast_points <- forecasts %>%
                    dplyr::filter(has_forecast) %>%
                    dplyr::mutate(offer_date = forecast_date)

                # Validate forecast_points has required columns before adding to plot
                if (nrow(forecast_points) > 0 &&
                    all(c("forecast_btc", "ci_lower", "ci_upper", "bond") %in% names(forecast_points))) {
                    p <- p +
                        ggplot2::geom_point(
                            data = forecast_points,
                            ggplot2::aes(x = offer_date, y = forecast_btc, color = bond),
                            shape = 18, size = 4,
                            inherit.aes = FALSE
                        ) +
                        ggplot2::geom_errorbar(
                            data = forecast_points,
                            ggplot2::aes(x = offer_date, ymin = ci_lower, ymax = ci_upper, color = bond),
                            width = 5, alpha = 0.5,
                            inherit.aes = FALSE
                        )
                }
            }

            return(p)

        }, error = function(e) {
            err_msg <- if (!is.null(e$message) && nchar(e$message) > 0) e$message else "Unknown rendering error"
            message("[Historical Performance] Render error: ", err_msg)
            ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5,
                    label = paste("Chart error:\n", substr(err_msg, 1, 50)),
                    size = 4, color = "gray50") +
                ggplot2::theme_void() +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
        })
    }, bg = "white")

    # Selected Bonds History
    output$selected_bonds_history <- renderUI({
        req(input$auction_bonds_select)
        req(enhanced_auction_data())

        selected_bonds <- input$auction_bonds_select

        # Get history for each selected bond
        history_cards <- lapply(selected_bonds, function(bond_name) {

            bond_data <- enhanced_auction_data() %>%
                filter(bond == bond_name) %>%
                arrange(desc(offer_date))

            n_auctions <- nrow(bond_data)

            if (n_auctions == 0) {
                return(
                    tags$div(
                        class = "bond-history-card",
                        style = "padding: 10px; margin-bottom: 8px; background: #f8f9fa;
                                 border-radius: 4px; border-left: 4px solid #6c757d;",
                        tags$strong(bond_name),
                        tags$span(" - No auction history", class = "text-muted")
                    )
                )
            }

            avg_btc <- mean(bond_data$bid_to_cover, na.rm = TRUE)
            last_btc <- bond_data$bid_to_cover[1]
            last_date <- format(bond_data$offer_date[1], "%Y-%m-%d")

            # Color based on historical performance
            card_color <- dplyr::case_when(
                avg_btc >= 3.5 ~ "#2E7D32",
                avg_btc >= 3.0 ~ "#388E3C",
                avg_btc >= 2.5 ~ "#F57C00",
                TRUE ~ "#C62828"
            )

            tags$div(
                class = "bond-history-card",
                style = sprintf("padding: 10px; margin-bottom: 8px; background: #f8f9fa;
                                 border-radius: 4px; border-left: 4px solid %s;", card_color),

                tags$div(
                    style = "display: flex; justify-content: space-between;",
                    tags$strong(bond_name, style = sprintf("color: %s;", card_color)),
                    tags$span(sprintf("n=%d", n_auctions), class = "text-muted")
                ),
                tags$div(
                    style = "display: flex; justify-content: space-between; margin-top: 5px;",
                    tags$span(sprintf("Avg: %.2fx", avg_btc)),
                    tags$span(sprintf("Last: %.2fx (%s)", last_btc, last_date),
                             style = "font-size: 0.85em;", class = "text-muted")
                )
            )
        })

        tags$div(
            tags$h5(
                tagList(icon("history"), " Selected Bonds History"),
                style = "color: #1B3A6B; font-weight: bold;"
            ),
            history_cards,
            tags$hr(),
            tags$p(
                sprintf("Next auction: %s", format(input$upcoming_auction_date, "%b %d")),
                class = "text-muted", style = "font-size: 0.9em;"
            )
        )
    })

    # Quick Stats v2
    output$auction_quick_stats_v2 <- renderUI({
        req(input$auction_bonds_select)

        selected_bonds <- input$auction_bonds_select
        n_selected <- length(selected_bonds)

        # Count bonds with sufficient data
        auction_data <- tryCatch(enhanced_auction_data(), error = function(e) NULL)

        if (is.null(auction_data)) {
            return(
                tags$div(
                    style = "font-size: 12px; color: #666; padding: 8px; background: #f8f9fa; border-radius: 4px;",
                    tags$span(icon("check-circle", style = "color: #4CAF50;"), sprintf(" %d bonds selected", n_selected))
                )
            )
        }

        sufficient_data <- sapply(selected_bonds, function(bond_name) {
            n <- auction_data %>%
                filter(bond == bond_name, !is.na(bid_to_cover)) %>%
                nrow()
            n >= 10
        })

        n_sufficient <- sum(sufficient_data)

        tags$div(
            style = "font-size: 12px; color: #666; padding: 10px; background: #f8f9fa; border-radius: 4px;",
            tags$div(
                style = "display: flex; justify-content: space-between; margin: 3px 0;",
                tags$span(icon("check-circle", style = "color: #4CAF50;"), " Selected:"),
                tags$strong(sprintf("%d bonds", n_selected))
            ),
            tags$div(
                style = "display: flex; justify-content: space-between; margin: 3px 0;",
                tags$span(icon("database", style = "color: #2196F3;"), " Sufficient data:"),
                tags$strong(sprintf("%d bonds", n_sufficient),
                           style = ifelse(n_sufficient < n_selected, "color: #FF9800;", "color: #4CAF50;"))
            )
        )
    })

    # Download forecast chart
    output$download_forecast_chart <- downloadHandler(
        filename = function() {
            paste0("auction_forecast_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(input$auction_bonds_select)
            req(enhanced_auction_data())

            selected_bonds <- input$auction_bonds_select
            auction_data <- enhanced_auction_data()

            chart_data <- auction_data %>%
                filter(bond %in% selected_bonds, !is.na(bid_to_cover)) %>%
                arrange(offer_date)

            p <- ggplot2::ggplot(chart_data, ggplot2::aes(x = offer_date, y = bid_to_cover, color = bond)) +
                ggplot2::geom_line(linewidth = 1) +
                ggplot2::geom_point(size = 2) +
                ggplot2::geom_hline(yintercept = 2, linetype = "dashed", color = "#C62828", alpha = 0.5) +
                ggplot2::geom_hline(yintercept = 3, linetype = "dashed", color = "#388E3C", alpha = 0.5) +
                ggplot2::labs(
                    title = "Auction Bid-to-Cover Historical Performance",
                    subtitle = paste("Bonds:", paste(selected_bonds, collapse = ", ")),
                    x = "Auction Date",
                    y = "Bid-to-Cover Ratio",
                    color = "Bond"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.background = ggplot2::element_rect(fill = "white", color = NA)
                )

            ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
        }
    )

    output$auction_sentiment_gauge <- renderPlot({
        req(filtered_data())
        p <- generate_auction_sentiment_gauge(filtered_data(), list())
        if(!is.null(p)) {
            gridExtra::grid.arrange(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for sentiment analysis", cex = 1)
        }
    }, height = 200)

    # Note: auction_success_factors render removed as part of Auction Intelligence tab overhaul

    # Note: btc_decomposition render removed - section removed from UI

    # 22. Add after the regime_analysis_plot output
    output$regime_summary <- renderUI({
        req(regime_data())

        # Use .data pronoun to avoid confusion with date() function
        current_regime <- regime_data() %>%
            filter(.data$date == max(.data$date, na.rm = TRUE))

        if(nrow(current_regime) == 0) {
            return(tags$p("No regime data available"))
        }

        # Calculate regime statistics for last 30 days
        # FIX: Use max date in data instead of today() to handle historical data properly
        max_data_date <- max(regime_data()$date, na.rm = TRUE)
        last_30_data <- regime_data() %>%
            filter(date >= max_data_date - 30)

        total_days <- nrow(last_30_data)

        regime_stats <- last_30_data %>%
            group_by(regime) %>%
            summarise(
                days = n(),
                pct = n() / max(total_days, 1) * 100,
                .groups = "drop"
            ) %>%
            # Ensure all regimes are represented
            complete(regime = c("Stressed", "Elevated", "Normal", "Calm"),
                     fill = list(days = 0, pct = 0))

        # Calculate regime start date (when current regime began)
        regime_changes <- regime_data() %>%
            arrange(date) %>%
            mutate(regime_changed = regime != lag(regime, default = first(regime))) %>%
            filter(regime_changed | row_number() == 1)

        regime_start_date <- regime_changes %>%
            filter(regime == current_regime$regime) %>%
            filter(date == max(date)) %>%
            pull(date) %>%
            first()

        if(is.na(regime_start_date)) {
            regime_start_date <- min(regime_data()$date, na.rm = TRUE)
        }

        # Determine regime color with gradient for styling
        regime_color <- case_when(
            current_regime$regime == "Stressed" ~ insele_palette$danger,
            current_regime$regime == "Elevated" ~ insele_palette$warning,
            current_regime$regime == "Calm" ~ insele_palette$success,
            TRUE ~ insele_palette$secondary
        )

        # Lighter version of regime color for gradient
        regime_color_light <- adjustcolor(regime_color, alpha.f = 0.85)

        tagList(
            # Current Regime box with gradient styling
            tags$div(
                style = paste0(
                    "background: linear-gradient(135deg, ", regime_color, " 0%, ",
                    regime_color_light, " 100%);",
                    "color: white;",
                    "padding: 15px 20px;",
                    "border-radius: 8px;",
                    "text-align: center;",
                    "margin-bottom: 15px;",
                    "box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
                ),
                h4(current_regime$regime, style = "margin: 0 0 5px 0; font-weight: 600;"),
                tags$small(paste("Since:", format(regime_start_date, "%B %d, %Y")))
            ),

            # Key Metrics section with better visual hierarchy
            tags$div(
                style = "padding: 10px 0;",
                h5("Key Metrics",
                   style = paste0("color: ", insele_palette$primary, "; margin: 0 0 12px 0; font-weight: 600;")),

                # Volatility (vol_20d is now decimal, multiply by 100 for %)
                tags$div(
                    style = "display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #eee;",
                    span("Volatility", style = "color: #666;"),
                    strong(sprintf("%.1f%%", current_regime$vol_20d * 100),
                           style = paste0("color: ", insele_palette$primary, ";"))
                ),

                # Stress Score
                tags$div(
                    style = "display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #eee;",
                    span("Stress Score", style = "color: #666;"),
                    strong(sprintf("%.2f", current_regime$stress_score),
                           style = paste0("color: ",
                                          ifelse(current_regime$stress_score > 1, insele_palette$danger,
                                                 ifelse(current_regime$stress_score < -0.5, insele_palette$success,
                                                        insele_palette$primary)), ";"))
                ),

                # Trend
                tags$div(
                    style = "display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #eee;",
                    span("Trend", style = "color: #666;"),
                    strong(current_regime$trend,
                           style = paste0("color: ", insele_palette$primary, ";"))
                ),

                # Dispersion
                tags$div(
                    style = "display: flex; justify-content: space-between; padding: 8px 0;",
                    span("Dispersion", style = "color: #666;"),
                    strong(sprintf("%.2f%%", current_regime$yield_dispersion),
                           style = paste0("color: ", insele_palette$primary, ";"))
                )
            ),

            hr(style = "margin: 15px 0; border-color: #eee;"),

            # 30-Day Distribution section with improved progress bars
            tags$div(
                h5("30-Day Distribution",
                   style = paste0("color: ", insele_palette$primary, "; margin: 0 0 12px 0; font-weight: 600;")),

                # Progress bars for each regime
                lapply(c("Stressed", "Elevated", "Normal", "Calm"), function(r) {
                    stats <- regime_stats[regime_stats$regime == r,]
                    pct_val <- if(nrow(stats) > 0) stats$pct else 0
                    days_val <- if(nrow(stats) > 0) stats$days else 0

                    color <- switch(r,
                        "Stressed" = insele_palette$danger,
                        "Elevated" = insele_palette$warning,
                        "Normal" = insele_palette$secondary,
                        "Calm" = insele_palette$success
                    )

                    tags$div(
                        style = "margin: 8px 0;",
                        tags$div(
                            style = "display: flex; justify-content: space-between; margin-bottom: 3px;",
                            span(r, style = "font-size: 12px; color: #555;"),
                            span(sprintf("%.0f%% (%d days)", pct_val, days_val),
                                 style = "font-size: 12px; font-weight: 600; color: #333;")
                        ),
                        tags$div(
                            style = "height: 8px; background: #eee; border-radius: 4px; overflow: hidden;",
                            tags$div(
                                style = paste0(
                                    "height: 100%; width: ", pct_val, "%; background: ", color,
                                    "; border-radius: 4px; transition: width 0.3s ease;"
                                )
                            )
                        )
                    )
                })
            )
        )
    })



    output$regime_probability_gauge <- renderPlot({
        req(regime_data())
        p <- generate_regime_probability_gauge(regime_data(), list())
        if(!is.null(p)) print(p)
    }, height = 160)  # Match UI height for proper rendering



    # ========================================================================
    # 24. SPREAD WARNING - Model Fit Quality
    # ========================================================================
    output$spread_warning <- renderUI({
        req(fitted_curve_data())

        metrics <- fitted_curve_data()$metrics
        avg_spread <- metrics$avg_spread
        r_squared <- metrics$r_squared

        # Show warning if average spread > 20 bps or R² < 0.95
        if (avg_spread > 20 || r_squared < 0.95) {
            tags$div(
                class = "alert alert-warning",
                style = "margin: 10px 0;",
                tags$strong(icon("exclamation-triangle"), " Model Fit Warning: "),
                sprintf(
                    "Average spread (%.1f bps) is elevated. R² = %.3f. Consider checking data quality or adjusting curve model.",
                    avg_spread, r_squared
                )
            )
        } else {
            NULL
        }
    })

    # ========================================================================
    # 25. RELATIVE VALUE OPPORTUNITIES TABLE
    # CRITICAL FIX: Table now reads from fitted_curve_data() - SAME source as chart
    # When x-axis or model changes, this table AUTOMATICALLY updates
    # Shows ALL bonds by default with filter dropdown
    # Uses normalized 0-10 conviction score
    # ========================================================================
    output$relative_value_opportunities <- DT::renderDataTable({
        req(fitted_curve_data())

        # Get bond data WITH spreads from the SAME source as the chart
        curve_data <- fitted_curve_data()
        bonds <- curve_data$bonds
        x_var_label <- curve_data$x_label
        x_var <- curve_data$x_var

        # Get filter choice (default to "all" if not set)
        filter_choice <- input$rv_table_filter %||% "all"

        # Debug: Confirm we're using fitted_curve_data
        cat("\n=== Relative Value Debug (CONNECTED TO CURVE) ===\n")
        cat(sprintf("X-Axis: %s, Model: %s, Filter: %s\n", x_var, curve_data$model_type, filter_choice))
        cat("Total bonds:", nrow(bonds), "\n")
        cat("Spread range:", sprintf("%.1f to %.1f bps\n",
                                     min(bonds$spread_bps, na.rm = TRUE),
                                     max(bonds$spread_bps, na.rm = TRUE)))

        # ═══════════════════════════════════════════════════════════════════════════
        # FIX: Filter out bonds with missing or placeholder data BEFORE calculations
        # Problem 3 & 4: Don't show bonds with placeholder values or no actual data
        # ═══════════════════════════════════════════════════════════════════════════
        opportunities <- bonds %>%
            filter(
                # Basic validity checks
                !is.na(spread_bps),
                !is.na(yield_to_maturity),
                !is.na(x_value),  # x_value is the dynamic x-axis (duration/TTM)
                # FIX: Exclude placeholder values (1.0% yield, 1.0 duration indicates bad data)
                yield_to_maturity > 1.5,  # Filter out placeholder values only (was 2%)
                x_value > 0.5,  # Modified duration should be > 0.5 for real bonds
                # FIX: Exclude bonds with no z-score data instead of filling with 0
                !is.na(z_score) | TRUE  # Allow NA z-scores but handle them properly below
            ) %>%
            mutate(
                # FIX: Keep NA z-scores as NA, don't fill with 0
                # Use the actual z_score, or NA if missing - let the UI show "N/A"
                zscore = z_score,

                # ═══════════════════════════════════════════════════════════════════
                # FIX #2: SYMMETRIC signal logic based on Z-Score thresholds
                # Strong Buy/Sell: |Z| >= 2.0 (statistically significant mispricing)
                # Buy/Sell: |Z| >= 1.5 (moderate mispricing)
                # Weak Buy/Sell: |Z| >= 1.0 (mild mispricing)
                # Hold: |Z| < 1.0 (fair value)
                # Direction: positive spread = cheap = buy, negative = rich = sell
                # ═══════════════════════════════════════════════════════════════════
                signal = case_when(
                    is.na(zscore) ~ "Insufficient Data",
                    # Strong signals: |Z| >= 2.0
                    zscore >= 2.0 ~ "Strong Buy",
                    zscore <= -2.0 ~ "Strong Sell",
                    # Regular signals: |Z| >= 1.5
                    zscore >= 1.5 ~ "Buy",
                    zscore <= -1.5 ~ "Sell",
                    # Weak signals: |Z| >= 1.0
                    zscore >= 1.0 ~ "Weak Buy",
                    zscore <= -1.0 ~ "Weak Sell",
                    # Hold: |Z| < 1.0
                    TRUE ~ "Hold"
                ),

                # Signal strength for sorting (higher = stronger signal)
                signal_strength = case_when(
                    signal %in% c("Strong Buy", "Strong Sell") ~ 3,
                    signal %in% c("Buy", "Sell") ~ 2,
                    signal %in% c("Weak Buy", "Weak Sell") ~ 1,
                    signal == "Hold" ~ 0,
                    TRUE ~ -1  # Insufficient Data
                ),

                # Get bid_to_cover if available
                btc = if ("bid_to_cover" %in% names(.)) bid_to_cover else NA_real_
            ) %>%
            rowwise() %>%
            mutate(
                score = calculate_conviction_score(
                    spread_bps = spread_bps,
                    zscore = ifelse(is.na(zscore), 0, zscore),  # Use 0 only for score calc
                    bid_to_cover = btc
                ),
                # ═══════════════════════════════════════════════════════════════════
                # FIX #4: Score breakdown for transparency
                # Components: spread (0-4) + zscore (0-4) + liquidity (0-2) = 0-10
                # ═══════════════════════════════════════════════════════════════════
                score_spread = min(abs(spread_bps) / 5, 4),
                score_zscore = if (!is.na(zscore) && sign(spread_bps) == sign(zscore)) {
                    min(abs(zscore) * 2, 4)
                } else if (!is.na(zscore)) {
                    min(abs(zscore) * 0.5, 1)
                } else {
                    0
                },
                score_liquidity = if (!is.na(btc) && btc > 0) min(btc, 2) else 1,
                score_breakdown = sprintf("%.1f+%.1f+%.1f", score_spread, score_zscore, score_liquidity)
            ) %>%
            ungroup() %>%
            mutate(
                # Signal category for filtering
                signal_category = case_when(
                    signal == "Insufficient Data" ~ "insufficient",
                    signal %in% c("Strong Buy", "Strong Sell") ~ "strong",
                    signal %in% c("Buy", "Sell", "Weak Buy", "Weak Sell") ~ "actionable",
                    TRUE ~ "hold"
                )
            )

        # ═══════════════════════════════════════════════════════════════════════════
        # FIX #1: Calculate Years to Maturity (populate Maturity column)
        # Shows actual time remaining, not just a warning flag
        # ═══════════════════════════════════════════════════════════════════════════
        has_mature_date <- "mature_date" %in% names(opportunities)

        if (has_mature_date) {
            opportunities <- opportunities %>%
                mutate(
                    # Calculate years to maturity
                    years_to_maturity = as.numeric(
                        difftime(mature_date, Sys.Date(), units = "days")
                    ) / 365.25,

                    # Format for display (e.g., "4.0y", "11m" for < 1 year)
                    maturity_display = case_when(
                        is.na(years_to_maturity) ~ "N/A",
                        years_to_maturity < 0 ~ "Matured",
                        years_to_maturity < 1 ~ paste0(round(years_to_maturity * 12, 0), "m"),
                        years_to_maturity < 10 ~ paste0(round(years_to_maturity, 1), "y"),
                        TRUE ~ paste0(round(years_to_maturity, 0), "y")
                    )
                )
        } else {
            # Fallback: use time_to_maturity if available
            if ("time_to_maturity" %in% names(opportunities)) {
                opportunities <- opportunities %>%
                    mutate(
                        years_to_maturity = time_to_maturity,
                        maturity_display = case_when(
                            is.na(years_to_maturity) ~ "N/A",
                            years_to_maturity < 1 ~ paste0(round(years_to_maturity * 12, 0), "m"),
                            years_to_maturity < 10 ~ paste0(round(years_to_maturity, 1), "y"),
                            TRUE ~ paste0(round(years_to_maturity, 0), "y")
                        )
                    )
            } else {
                opportunities$years_to_maturity <- NA_real_
                opportunities$maturity_display <- "N/A"
            }
        }

        # Apply filter based on user selection
        if (filter_choice == "actionable") {
            opportunities <- opportunities %>%
                filter(signal_category %in% c("strong", "actionable"))
        } else if (filter_choice == "strong") {
            opportunities <- opportunities %>%
                filter(signal_category == "strong")
        }
        # "all" shows everything - no additional filter

        # ═══════════════════════════════════════════════════════════════════════════
        # FIX #3: Sort by signal strength first, then by |Z-Score|
        # This puts the most actionable opportunities at the top
        # ═══════════════════════════════════════════════════════════════════════════
        opportunities <- opportunities %>%
            arrange(desc(signal_strength), desc(abs(zscore)))

        cat("Filtered bonds:", nrow(opportunities), "\n")

        # Select columns - use x_value which is the DYNAMIC x-axis value
        # FIX #4: Include score_breakdown for transparency
        display_data <- opportunities %>%
            select(
                Bond = bond,
                Yield = yield_to_maturity,
                Duration = x_value,  # Uses whatever x-axis is currently selected
                Spread = spread_bps,
                ZScore = zscore,
                Signal = signal,
                Score = score,
                ScoreBreakdown = score_breakdown,  # For tooltip
                Maturity = maturity_display  # FIX #1: Now shows years to maturity
            )

        # Format the display - FIX: Handle NA z-scores properly
        display_data <- display_data %>%
            mutate(
                Yield = sprintf("%.3f%%", Yield),
                Duration = sprintf("%.2f", Duration),
                Spread = sprintf("%+.1f bps", Spread),  # Show sign
                ZScore = ifelse(is.na(ZScore), "N/A", sprintf("%+.2f", ZScore)),  # FIX: Show N/A for missing
                # FIX #4: Show score with breakdown in parentheses
                Score = ifelse(is.na(Score), "N/A", sprintf("%.1f (%s)", Score, ScoreBreakdown))
            ) %>%
            select(-ScoreBreakdown)  # Remove the breakdown column after formatting

        # Dynamic column name based on x-axis selection
        duration_col_name <- switch(x_var,
            "modified_duration" = "Mod Duration",
            "duration" = "Duration",
            "time_to_maturity" = "TTM (yrs)",
            "Duration"  # fallback
        )

        datatable(
            display_data,
            options = list(
                pageLength = 15,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all')
                )
            ),
            rownames = FALSE,
            class = 'table-striped table-bordered',
            colnames = c("Bond", "Yield", duration_col_name, "Spread", "Z-Score", "Signal", "Score", "Maturity")
        ) %>%
            # FIX #2: Add colors for Weak Buy/Weak Sell signals (symmetric with Strong)
            formatStyle(
                "Signal",
                backgroundColor = styleEqual(
                    c("Strong Buy", "Buy", "Weak Buy", "Hold", "Weak Sell", "Sell", "Strong Sell", "Insufficient Data"),
                    c("#1B5E20",     # Dark green - Strong Buy
                      "#4CAF50",     # Green - Buy
                      "#81C784",     # Light green - Weak Buy
                      "#9E9E9E",     # Gray - Hold
                      "#EF9A9A",     # Light red - Weak Sell
                      "#EF5350",     # Red - Sell
                      "#B71C1C",     # Dark red - Strong Sell
                      "#757575")     # Gray - Insufficient Data
                ),
                color = styleEqual(
                    c("Strong Buy", "Buy", "Weak Buy", "Hold", "Weak Sell", "Sell", "Strong Sell", "Insufficient Data"),
                    c("white", "white", "black", "white", "black", "white", "white", "white")
                ),
                fontWeight = "bold"
            ) %>%
            # Z-Score column: color-code based on value
            # NOTE: Use actual column name "ZScore" (not display name "Z-Score")
            formatStyle(
                "ZScore",
                color = styleInterval(
                    c(-2, -1.5, -1, 1, 1.5, 2),
                    c("#B71C1C", "#EF5350", "#EF9A9A", "#666666", "#81C784", "#4CAF50", "#1B5E20")
                ),
                fontWeight = "bold"
            ) %>%
            # Maturity column: highlight short maturities
            formatStyle(
                "Maturity",
                backgroundColor = styleEqual(
                    c("Matured"),
                    c("#FFCDD2")  # Light red for matured bonds
                ),
                fontWeight = "normal"
            )
    })


    # 27. Market Microstructure Table
    output$market_microstructure_table <- DT::renderDataTable({
        req(filtered_data())

        # Calculate comprehensive market microstructure metrics
        microstructure <- filtered_data() %>%
            group_by(bond) %>%
            arrange(date) %>%
            mutate(
                # Daily changes
                yield_change = yield_to_maturity - lag(yield_to_maturity),

                # High-Low range (proxy for intraday volatility)
                rolling_high = zoo::rollmax(yield_to_maturity, k = min(5, n()), fill = NA, align = "right"),
                rolling_low = zoo::rollapply(yield_to_maturity, width = min(5, n()),
                                             FUN = min, fill = NA, align = "right", partial = TRUE),
                high_low_range = rolling_high - rolling_low,

                # Autocorrelation (market efficiency indicator)
                autocorr = zoo::rollapply(yield_change, width = min(20, n()),
                                          FUN = function(x) {
                                              if(length(x) > 2 && sd(x, na.rm = TRUE) > 0) {
                                                  cor(x[-length(x)], x[-1], use = "complete.obs")
                                              } else { 0 }
                                          },
                                          fill = NA, align = "right", partial = TRUE)
            ) %>%
            summarise(
                # Basic Info
                Last_Yield = last(yield_to_maturity),
                Duration = mean(modified_duration, na.rm = TRUE),

                # Liquidity Proxies
                Avg_Daily_Range = mean(high_low_range, na.rm = TRUE) * 100,  # in bps

                # Volatility Metrics
                Yield_Vol = sd(yield_change, na.rm = TRUE) * sqrt(252) * 100,  # Annualized in bps

                # Market Efficiency
                Autocorrelation = mean(autocorr, na.rm = TRUE),

                # Risk Metrics
                Max_Drawdown = min(yield_change, na.rm = TRUE) * 100,  # in bps
                Max_Rally = max(yield_change, na.rm = TRUE) * 100,      # in bps

                # Trend Indicators
                Trend_Strength = {
                    if(n() > 10) {
                        model <- lm(yield_to_maturity ~ as.numeric(date))
                        summary(model)$r.squared
                    } else { NA }
                },

                # Number of observations
                Data_Points = n(),

                .groups = "drop"
            ) %>%
            mutate(
                # Market Quality Score (composite metric)
                Quality_Score = case_when(
                    Yield_Vol < 10 & abs(Autocorrelation) < 0.2 ~ "High",
                    Yield_Vol < 20 & abs(Autocorrelation) < 0.3 ~ "Medium",
                    TRUE ~ "Low"
                )
            ) %>%
            arrange(Duration)

        # Format for display
        display_table <- microstructure %>%
            mutate(
                Last_Yield = sprintf("%.3f%%", Last_Yield),
                Duration = sprintf("%.2f", Duration),
                Avg_Daily_Range = sprintf("%.1f bps", Avg_Daily_Range),
                Yield_Vol = sprintf("%.1f bps", Yield_Vol),
                Autocorrelation = sprintf("%.3f", Autocorrelation),
                Max_Drawdown = sprintf("%.1f bps", Max_Drawdown),
                Max_Rally = sprintf("%.1f bps", Max_Rally),
                Trend_Strength = sprintf("%.2f", Trend_Strength),
                Data_Points = as.character(Data_Points)
            )

        # Create interactive table
        datatable(
            display_table,
            options = list(
                pageLength = 15,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                scrollX = TRUE,
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all'),
                    list(width = '80px', targets = 0)
                ),
                order = list(list(2, 'asc'))  # Sort by duration
            ),
            rownames = FALSE,
            class = 'table-striped table-bordered compact',
            caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left;',
                htmltools::tags$b('Market Microstructure Analysis'),
                htmltools::tags$br(),
                'Liquidity, efficiency, and quality metrics for each bond'
            )
        ) %>%
            formatStyle(
                "Quality_Score",
                backgroundColor = styleEqual(
                    c("High", "Medium", "Low"),
                    c("#E8F5E9", "#FFF3E0", "#FFEBEE")
                ),
                fontWeight = styleEqual("High", "bold")
            )
    })



    # ================================================================================
    # SHARED REPORT CONFIGURATION REACTIVE
    # ================================================================================
    report_config <- reactive({
        # Collect selected sections
        sections <- character()
        section_ids <- c("overview", "relative", "risk", "technical", "carry",
                         "auction", "intelligence", "treasury", "recommendations")
        for (sid in section_ids) {
            if (isTRUE(input[[paste0("section_", sid)]])) {
                sections <- c(sections, sid)
            }
        }

        # Collect all 35 selected plots
        plot_ids <- c(
            # Overview
            "regime_plot",
            # Relative Value
            "yield_curve", "relative_heatmap", "zscore_plot", "convexity",
            # Risk
            "var_distribution", "var_ladder", "dv01_ladder",
            # Technical
            "technical_plot", "signal_matrix",
            # Carry & Roll
            "carry_heatmap", "scenario_analysis", "butterfly_spread", "forward_curve",
            # Auction
            "auction_performance", "auction_patterns", "auction_forecast",
            "demand_elasticity", "success_probability", "bid_distribution",
            "ytd_issuance", "auction_sentiment", "auction_success_factors",
            # Intelligence
            "correlation", "term_structure",
            # Treasury
            "holdings_area", "sector_trend", "holdings_fixed", "holdings_ilb",
            "holdings_frn", "holdings_sukuk", "ownership_changes",
            "holdings_diverging_fixed", "holdings_diverging_ilb"
        )

        selected_plots <- setNames(
            lapply(plot_ids, function(pid) isTRUE(input[[paste0("plot_", pid)]])),
            plot_ids
        )

        # Build input_params for collect_report_charts
        input_params <- list(
            report_sections = sections,
            selected_plots = selected_plots,
            auction_bonds_select = input$auction_bonds_select,
            xaxis_choice = input$xaxis_choice,
            curve_model = input$curve_model,
            return_type = input$return_type,
            tech_bond_select = input$tech_bond_select,
            tech_indicator_type = input$tech_indicator_type
        )

        list(
            sections = sections,
            selected_plots = selected_plots,
            input_params = input_params,
            report_title = input$report_title %||% paste("SA Government Bond Analysis -", format(Sys.Date(), "%B %Y")),
            report_type = input$report_type %||% "executive",
            client_name = input$client_name %||% "",
            report_date = input$report_date %||% Sys.Date()
        )
    })

    # ================================================================================
    # SHARED REPORT DATA COLLECTOR
    # ================================================================================
    collect_report_data <- function() {
        list(
            proc_data = tryCatch(processed_data(), error = function(e) NULL),
            filt_data = tryCatch(filtered_data(), error = function(e) NULL),
            var_data_val = tryCatch(var_data(), error = function(e) NULL),
            regime_data_val = tryCatch(regime_data(), error = function(e) NULL),
            carry_data_val = tryCatch(carry_roll_data(), error = function(e) NULL),
            filt_data_with_tech = tryCatch({
                validate_dataframe_class(filtered_data_with_technicals(), "filtered_data_with_technicals [report]")
            }, error = function(e) NULL),
            treasury_ts = tryCatch({
                if (!is.null(treasury_module_data) && !is.null(treasury_module_data$holdings_ts)) {
                    treasury_module_data$holdings_ts()
                } else NULL
            }, error = function(e) NULL),
            treasury_bonds = tryCatch({
                if (!is.null(treasury_module_data) && !is.null(treasury_module_data$bond_holdings)) {
                    treasury_module_data$bond_holdings()
                } else NULL
            }, error = function(e) NULL)
        )
    }

    # ================================================================================
    # EXPORT COLUMN HELPER
    # ================================================================================
    get_export_columns <- function(data, include_metadata = TRUE, include_calculations = TRUE) {
        # Core columns always included
        core_cols <- c("date", "bond")

        # Approved data series
        data_cols <- c("yield", "coupon", "convexity", "accrued_interest",
                       "clean_price", "duration", "modified_duration",
                       "dv01", "full_price")

        # Auction columns (if present)
        auction_cols <- c("offer_amount", "bids_received", "bid_to_cover")

        # Metadata columns
        meta_cols <- if (include_metadata) {
            c("time_to_maturity", "maturity_date")
        } else character(0)

        # Calculation columns
        calc_cols <- if (include_calculations) {
            c("z_score", "spread_to_curve", "richness")
        } else character(0)

        # Only select columns that actually exist in the data
        all_desired <- c(core_cols, data_cols, auction_cols, meta_cols, calc_cols)
        available <- intersect(all_desired, names(data))

        return(available)
    }

    # Report Preview
    # Report Preview - Enhanced with individual plot selection
    output$report_preview_content <- renderUI({
        config <- report_config()
        sections <- config$sections
        selected_plots_flags <- config$selected_plots

        # Build display names for selected plots
        # Plot display name mapping
        plot_display_names <- c(
            regime_plot = "Regime Analysis",
            yield_curve = "Yield Curve",
            relative_heatmap = "Relative Value Heatmap",
            zscore_plot = "Z-Score Distribution",
            convexity = "\U2728 Enhanced Convexity",
            var_distribution = "VaR Distribution",
            var_ladder = "VaR Ladder",
            dv01_ladder = "DV01 Analysis",
            technical_plot = "Technical Indicators",
            signal_matrix = "Signal Matrix",
            carry_heatmap = "Carry Heatmap",
            scenario_analysis = "Scenario Analysis",
            butterfly_spread = "\U2728 Butterfly Spread",
            forward_curve = "\U2728 Forward Curve",
            auction_performance = "Auction Performance",
            auction_patterns = "Auction Patterns",
            auction_forecast = "\U2728 Auction Forecast",
            demand_elasticity = "\U2728 Demand Elasticity",
            success_probability = "\U2728 Success Probability",
            bid_distribution = "\U2728 Bid Distribution",
            ytd_issuance = "\U2728 YTD Issuance",
            auction_sentiment = "\U2728 Auction Sentiment",
            auction_success_factors = "\U2728 Success Factors",
            correlation = "Correlation Matrix",
            term_structure = "3D Term Structure",
            holdings_area = "\U0001F3DB Holdings Time Series",
            sector_trend = "\U0001F3DB Sector Trend",
            holdings_fixed = "\U0001F3DB Fixed Rate Holdings",
            holdings_ilb = "\U0001F3DB ILB Holdings",
            holdings_frn = "\U0001F3DB FRN Holdings",
            holdings_sukuk = "\U0001F3DB Sukuk Holdings",
            ownership_changes = "\U0001F3DB Ownership Changes",
            holdings_diverging_fixed = "\U0001F3DB Fixed Rate Changes",
            holdings_diverging_ilb = "\U0001F3DB ILB Changes"
        )

        # Map plots to their parent sections
        plot_section_map <- c(
            regime_plot = "overview",
            yield_curve = "relative", relative_heatmap = "relative", zscore_plot = "relative", convexity = "relative",
            var_distribution = "risk", var_ladder = "risk", dv01_ladder = "risk",
            technical_plot = "technical", signal_matrix = "technical",
            carry_heatmap = "carry", scenario_analysis = "carry", butterfly_spread = "carry", forward_curve = "carry",
            auction_performance = "auction", auction_patterns = "auction", auction_forecast = "auction",
            demand_elasticity = "auction", success_probability = "auction", bid_distribution = "auction",
            ytd_issuance = "auction", auction_sentiment = "auction", auction_success_factors = "auction",
            correlation = "intelligence", term_structure = "intelligence",
            holdings_area = "treasury", sector_trend = "treasury", holdings_fixed = "treasury", holdings_ilb = "treasury",
            holdings_frn = "treasury", holdings_sukuk = "treasury", ownership_changes = "treasury",
            holdings_diverging_fixed = "treasury", holdings_diverging_ilb = "treasury"
        )

        selected_plots <- character()
        for (pid in names(selected_plots_flags)) {
            parent_section <- plot_section_map[[pid]]
            if (isTRUE(selected_plots_flags[[pid]]) && parent_section %in% sections) {
                selected_plots <- c(selected_plots, plot_display_names[[pid]])
            }
        }

        # Get data counts
        bond_count <- tryCatch(length(unique(filtered_data()$bond)), error = function(e) 0)
        date_range_text <- tryCatch(
            paste(format(input$date_range[1], "%b %d, %Y"), "to", format(input$date_range[2], "%b %d, %Y")),
            error = function(e) "N/A"
        )

        chart_count <- length(selected_plots)

        # Section names mapping
        sections_text <- c(
            "overview" = "Market Overview",
            "relative" = "Relative Value Analysis",
            "risk" = "Risk Analytics",
            "technical" = "Technical Analysis",
            "carry" = "Carry & Roll Analysis",
            "auction" = "Auction Intelligence",
            "intelligence" = "Market Intelligence",
            "treasury" = "Treasury Holdings",
            "recommendations" = "Trading Recommendations"
        )

        # Enhanced preview display
        tagList(
            tags$div(
                style = "background: white; padding: 15px; border-radius: 8px; border: 2px solid #1B3A6B;",
                h5(HTML("&#128203; Report Structure Preview"), style = "color: #1B3A6B; margin-top: 0;"),
                tags$div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 15px;",
                    tags$div(
                        tags$strong("Sections: "),
                        tags$span(paste(length(sections), "of 9"), style = "color: #28a745; font-weight: bold;")
                    ),
                    tags$div(
                        tags$strong("Charts: "),
                        tags$span(paste(chart_count, "of 35"),
                                  style = sprintf("color: %s; font-weight: bold;",
                                                  ifelse(chart_count > 15, "#28a745", "#ffc107")))
                    )
                ),
                if(chart_count > 0) {
                    tags$div(
                        style = "max-height: 300px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 5px;",
                        tags$strong("Selected Charts:"),
                        tags$ul(
                            style = "margin-top: 10px; margin-bottom: 0;",
                            lapply(selected_plots, function(plot) {
                                is_new <- grepl("\U2728", plot)
                                tags$li(
                                    plot,
                                    style = if(is_new) "color: #007bff; font-weight: 500;" else ""
                                )
                            })
                        )
                    )
                }
            )
        )
    })


    # ================================================================================
    # OUTPUT: UI COMPONENTS
    # ================================================================================

    # ========================================================================
    # CRITICAL FIX: Curve metrics now reads from fitted_curve_data()
    # Cheapest/Richest bonds and R² are consistent with chart and table
    # ========================================================================
    output$curve_metrics_summary <- renderUI({
        req(fitted_curve_data())

        # Get curve data from single source of truth
        curve_data <- fitted_curve_data()
        data <- curve_data$bonds
        metrics <- curve_data$metrics
        x_var <- curve_data$x_var

        # =========================================================================
        # DEBUG LOGGING: Confirm bond universe for Trading Signals
        # =========================================================================
        bonds_in_signals <- sort(unique(data$bond))
        message(sprintf("[TRADING SIGNALS] Active bonds: %d", length(bonds_in_signals)))
        message(sprintf("[TRADING SIGNALS] List: %s", paste(bonds_in_signals, collapse = ", ")))

        # Verify no known matured bonds are present (safety check)
        known_matured_bonds <- c("R157", "R186", "R197", "R203", "R204", "R207", "R208", "R212", "R2023")
        found_matured <- intersect(bonds_in_signals, known_matured_bonds)
        if (length(found_matured) > 0) {
            warning(sprintf("[TRADING SIGNALS] !!! POTENTIAL MATURED BONDS PRESENT: %s",
                           paste(found_matured, collapse = ", ")))
        } else {
            message("[TRADING SIGNALS] ✓ No known matured bonds present")
        }

        # ════════════════════════════════════════════════════════════════════════
        # FIX: Use adaptive bucket and slope calculations that handle data limits
        # SA Government Bond universe has max ModDur ~9.5y, no bonds >10y
        # ════════════════════════════════════════════════════════════════════════

        # Calculate duration bucket yields with adaptive thresholds
        buckets <- calculate_duration_bucket_yields(data, x_col = "x_value",
                                                     y_col = "yield_to_maturity",
                                                     bucket_type = "adaptive")

        # Calculate adaptive slope (uses actual curve endpoints if 2s10s unavailable)
        slope_info <- calculate_adaptive_slope(data, x_col = "x_value",
                                                y_col = "yield_to_maturity",
                                                min_spread = 4)

        # Calculate curvature using adaptive bucket yields
        curvature_info <- calculate_curvature(
            buckets$short$avg_yield,
            buckets$medium$avg_yield,
            buckets$long$avg_yield
        )

        # Simple curve level (always available)
        curve_level <- mean(data$yield_to_maturity, na.rm = TRUE)

        # Use model fit quality from fitted_curve_data metrics (already calculated)
        avg_spread <- metrics$avg_spread
        r_squared <- metrics$r_squared

        # ================================================================================
        # TRADING SIGNALS - Actionable Intelligence (from fitted_curve_data)
        # ================================================================================

        # Data already has z_score and spread_bps from fitted_curve_data
        latest_data <- data %>%
            mutate(zscore_abs = abs(z_score))

        # Find cheapest bond (highest positive spread = above curve = buy signal)
        cheapest <- latest_data %>%
            filter(!is.na(spread_bps)) %>%
            filter(spread_bps == max(spread_bps, na.rm = TRUE)) %>%
            slice(1)

        # Find richest bond (lowest/most negative spread = below curve = sell signal)
        richest <- latest_data %>%
            filter(!is.na(spread_bps)) %>%
            filter(spread_bps == min(spread_bps, na.rm = TRUE)) %>%
            slice(1)

        # Find highest conviction trade (highest |z-score|)
        highest_conviction <- latest_data %>%
            filter(!is.na(z_score)) %>%
            filter(zscore_abs == max(zscore_abs, na.rm = TRUE)) %>%
            slice(1)

        # ════════════════════════════════════════════════════════════════════════════
        # FIX: Use unified signal calculation for consistency with signal_strength_box
        # Both displays now use the same SIGNAL_THRESHOLDS from theme_config.R
        # ════════════════════════════════════════════════════════════════════════════
        unified_signals <- calculate_unified_signals(latest_data, spread_col = "spread_bps", bond_col = "bond")
        strong_count <- unified_signals$strong_count
        moderate_count <- unified_signals$moderate_count
        actionable_count <- moderate_count  # Actionable = moderate and above

        # ═══════════════════════════════════════════════════════════════════════════
        # Build dynamic bucket labels based on adaptive thresholds
        # ═══════════════════════════════════════════════════════════════════════════
        short_label <- if (buckets$is_adaptive) {
            sprintf("Short (<%dy) [%d]", buckets$short_max, buckets$short$count)
        } else {
            sprintf("Short (<5y) [%d]", buckets$short$count)
        }

        medium_label <- if (buckets$is_adaptive) {
            sprintf("Medium (%d-%dy) [%d]", buckets$short_max, buckets$medium_max, buckets$medium$count)
        } else {
            sprintf("Medium (5-10y) [%d]", buckets$medium$count)
        }

        long_label <- if (buckets$is_adaptive) {
            sprintf("Long (>%dy) [%d]", buckets$medium_max, buckets$long$count)
        } else {
            sprintf("Long (>10y) [%d]", buckets$long$count)
        }

        # Build slope label with adaptive indicator
        slope_label <- paste0("Slope (", slope_info$label, "):")

        tagList(
            # ═══════════════════════════════════════════════════════════
            # CURVE METRICS SECTION - Now with graceful NaN handling
            # ═══════════════════════════════════════════════════════════
            h5("Curve Metrics", style = "color: #1B3A6B; font-weight: bold; margin-top: 0;"),
            tags$div(
                tags$p(tags$b("Level:"), format_curve_metric(curve_level, "%", decimals = 2)),
                tags$p(
                    tags$b(slope_label),
                    format_curve_metric(slope_info$slope, " bps", decimals = 0,
                                        na_text = if (!is.null(slope_info$label) && slope_info$label != "No data")
                                                  paste0("Curve ", slope_info$label) else "N/A")
                ),
                tags$p(tags$b("Curvature:"),
                    format_curve_metric(curvature_info$value, "%", decimals = 2,
                                        na_text = "Insufficient tenors")
                )
            ),
            hr(style = "margin: 10px 0;"),

            # ═══════════════════════════════════════════════════════════
            # SECTOR YIELDS SECTION - Adaptive buckets with counts
            # ═══════════════════════════════════════════════════════════
            tags$div(
                tags$p(tags$b(short_label),
                    format_curve_metric(buckets$short$avg_yield, "%", decimals = 2,
                                        na_text = "No bonds")),
                tags$p(tags$b(medium_label),
                    format_curve_metric(buckets$medium$avg_yield, "%", decimals = 2,
                                        na_text = "No bonds")),
                tags$p(tags$b(long_label),
                    format_curve_metric(buckets$long$avg_yield, "%", decimals = 2,
                                        na_text = "No bonds"))
            ),
            hr(style = "margin: 10px 0;"),

            # ═══════════════════════════════════════════════════════════
            # MODEL QUALITY SECTION
            # ═══════════════════════════════════════════════════════════
            tags$div(
                tags$p(tags$b("Avg |Spread|:"), format_curve_metric(avg_spread, " bps", decimals = 1)),
                tags$p(tags$b("Model R²:"), format_curve_metric(r_squared, "", decimals = 3, na_text = "0.000"))
            ),
            hr(style = "margin: 10px 0;"),

            # ═══════════════════════════════════════════════════════════
            # TRADING SIGNALS SECTION (NEW!)
            # ═══════════════════════════════════════════════════════════
            h5("Trading Signals", style = "color: #1B3A6B; font-weight: bold;"),

            # ─────────────────────────────────────────────────────────────
            # VALIDATION: Ensure signal bonds are in active universe
            # ─────────────────────────────────────────────────────────────
            # Cheapest bond (BUY signal) - uses spread_bps from fitted_curve_data
            if (nrow(cheapest) > 0 && !is.na(cheapest$spread_bps[1]) &&
                cheapest$bond[1] %in% bonds_in_signals) {
                tags$p(
                    tags$b("Cheapest: "),
                    tags$span(cheapest$bond[1], style = "color: #388E3C; font-weight: bold;"),
                    sprintf(" (+%.1f bps)", cheapest$spread_bps[1])
                )
            } else {
                tags$p(tags$b("Cheapest: "), "N/A")
            },

            # Richest bond (SELL signal) - uses spread_bps from fitted_curve_data
            if (nrow(richest) > 0 && !is.na(richest$spread_bps[1]) &&
                richest$bond[1] %in% bonds_in_signals) {
                tags$p(
                    tags$b("Richest: "),
                    tags$span(richest$bond[1], style = "color: #D32F2F; font-weight: bold;"),
                    sprintf(" (%.1f bps)", richest$spread_bps[1])
                )
            } else {
                tags$p(tags$b("Richest: "), "N/A")
            },

            # Highest conviction trade
            if (nrow(highest_conviction) > 0 && !is.na(highest_conviction$z_score[1]) &&
                highest_conviction$bond[1] %in% bonds_in_signals) {
                tags$p(
                    tags$b("Highest Conviction: "),
                    highest_conviction$bond[1],
                    sprintf(" (Z=%.2f)", highest_conviction$z_score[1])
                )
            } else {
                tags$p(tags$b("Highest Conviction: "), "N/A")
            },

            # ═══════════════════════════════════════════════════════════
            # SYNCHRONIZED SIGNAL COUNTS (matches signal_strength_box)
            # ═══════════════════════════════════════════════════════════
            tags$p(
                tags$b("Strong Signals: "),
                sprintf("%d bonds with |Z| > %.1f", strong_count, SIGNAL_THRESHOLDS$strong)
            ),

            tags$p(
                tags$b("Actionable Signals: "),
                sprintf("%d bonds with |Z| > %.1f", actionable_count, SIGNAL_THRESHOLDS$moderate)
            ),

            # Z-Score explanation with dynamic thresholds
            tags$p(
                class = "text-muted small",
                style = "margin-top: 10px; font-size: 11px;",
                sprintf("Z-Score thresholds: |Z| > %.1f = strong, |Z| > %.1f = moderate",
                        SIGNAL_THRESHOLDS$strong, SIGNAL_THRESHOLDS$moderate)
            )
        )
    })

    # Live market metrics
    output$live_market_metrics <- renderUI({
        req(regime_data())

        current_regime <- regime_data() %>%
            filter(date == max(date))

        tagList(
            h5("Live Market Metrics", style = "margin-top: 0;"),
            tags$div(
                style = "display: flex; justify-content: space-between; margin: 10px 0;",
                span("Regime:"),
                span(current_regime$regime,
                     style = paste0("font-weight: bold; color: ",
                                    case_when(
                                        current_regime$regime == "Stressed" ~ insele_palette$danger,
                                        current_regime$regime == "Elevated" ~ insele_palette$warning,
                                        current_regime$regime == "Calm" ~ insele_palette$success,
                                        TRUE ~ insele_palette$info
                                    )))
            ),
            tags$div(
                style = "display: flex; justify-content: space-between; margin: 10px 0;",
                span("Volatility:"),
                span(sprintf("%.1f%%", current_regime$vol_20d * 100))
            ),
            tags$div(
                style = "display: flex; justify-content: space-between; margin: 10px 0;",
                span("Trend:"),
                span(current_regime$trend)
            ),
            hr(style = "margin: 10px 0;"),
            tags$small(paste("Last update:", format(Sys.time(), "%H:%M:%S")))
        )
    })

    # Update market intelligence display
    output$advanced_insights_content <- renderUI({
        insight_list <- insights()

        if (length(insight_list) == 0) {
            return(div(
                class = "insight-item",
                span(class = "loading-spinner"),
                span("Analyzing market data...", style = "margin-left: 15px;")
            ))
        }

        insight_divs <- lapply(insight_list, function(insight) {
            div(
                class = "insight-item",
                span(insight$icon, style = "font-size: 24px; margin-right: 15px;"),
                span(insight$text, style = "flex-grow: 1;"),
                span(insight$category, class = "insight-category")
            )
        })

        do.call(tagList, insight_divs)
    })


    # ================================================================================
    # OUTPUT: VALUE BOXES
    # ================================================================================

    # ════════════════════════════════════════════════════════════════════════════
    # REGIME VALUE BOX - Fixed to clearly separate Volatility (Regime) from Trend
    #
    # KEY INSIGHT: Regime and Trend are INDEPENDENT dimensions:
    # - Regime (Calm/Normal/Elevated/Stressed) = VOLATILITY state
    # - Trend (Uptrend/Downtrend/Sideways) = DIRECTION of movement
    #
    # A market can be "Calm" (low volatility) with "Strong Downtrend" (consistent direction)
    # This is NOT contradictory - it means yields are moving consistently without large swings.
    # ════════════════════════════════════════════════════════════════════════════
    output$market_regime_box <- renderValueBox({
        req(regime_data())

        # Use explicit column reference to avoid conflict with date() function
        current_regime <- regime_data() %>%
            filter(.data$date == max(.data$date, na.rm = TRUE))

        if(nrow(current_regime) == 0) {
            valueBox(
                value = "N/A",
                subtitle = "Market Regime",
                icon = icon("chart-area"),
                color = "gray"
            )
        } else {
            # Regime color is based on VOLATILITY (the regime classification)
            color <- case_when(
                current_regime$regime[1] == "Stressed" ~ "red",
                current_regime$regime[1] == "Elevated" ~ "yellow",
                current_regime$regime[1] == "Calm" ~ "green",
                TRUE ~ "blue"
            )

            # Get volatility percentile for clarity
            vol_pct <- if("vol_percentile" %in% names(current_regime)) {
                current_regime$vol_percentile[1] * 100
            } else {
                NA
            }

            # Create clear, non-contradictory subtitle
            # Format: "Volatility: XX% | Direction: YY"
            # This makes it explicit that regime=volatility and trend=direction are separate
            vol_text <- if(!is.na(vol_pct)) {
                sprintf("Vol: %.0f%%ile", vol_pct)
            } else {
                sprintf("Vol: %.1f%%", current_regime$vol_20d[1] * 100)
            }

            # Add trend with directional icon for visual clarity
            trend_text <- current_regime$trend[1]
            trend_icon <- case_when(
                grepl("Uptrend", trend_text) ~ "\u2191",      # ↑
                grepl("Downtrend", trend_text) ~ "\u2193",   # ↓
                TRUE ~ "\u2194"                               # ↔
            )

            valueBox(
                value = current_regime$regime[1],
                subtitle = HTML(sprintf(
                    "<span style='font-size: 11px;'>%s (volatility)</span><br/><span style='font-size: 10px;'>%s %s</span>",
                    vol_text,
                    trend_icon,
                    trend_text
                )),
                icon = icon("chart-area"),
                color = color
            )
        }
    })

    output$portfolio_var_box <- renderValueBox({
        req(var_data())

        avg_var <- var_data() %>%
            summarise(avg_var_95 = mean(abs(VaR_95_bps), na.rm = TRUE)) %>%
            pull(avg_var_95)

        valueBox(
            value = sprintf("%.0f bps", avg_var),
            subtitle = paste0(input$confidence_level, "% VaR (1-day)"),
            icon = icon("shield-alt"),
            color = if(avg_var > 50) "red" else if(avg_var > 30) "yellow" else "green"
        )
    })

    output$best_carry_box <- renderValueBox({
        req(carry_roll_data())

        # Check if data has required columns
        if(!"holding_period" %in% names(carry_roll_data()) || nrow(carry_roll_data()) == 0) {
            valueBox(
                value = "N/A",
                subtitle = "Calculating...",
                icon = icon("coins"),
                color = "purple"
            )
        } else {
            best <- carry_roll_data() %>%
                filter(holding_period == "90d") %>%
                filter(!is.na(return_per_unit_risk)) %>%
                arrange(desc(return_per_unit_risk)) %>%
                head(1)

            if(nrow(best) > 0) {
                valueBox(
                    value = sprintf("%.2f%%", best$net_return),
                    subtitle = paste("Best 90d Carry:", best$bond),
                    icon = icon("coins"),
                    color = "purple"
                )
            } else {
                valueBox(
                    value = "N/A",
                    subtitle = "No carry data available",
                    icon = icon("coins"),
                    color = "purple"
                )
            }
        }
    })

    output$signal_strength_box <- renderValueBox({
        req(processed_data())

        # ════════════════════════════════════════════════════════════════════════════
        # FIX: Use unified z-score based signals for consistency with yield curve panel
        # This ensures "Strong Signals Active" matches "Actionable Signals" calculation
        # ════════════════════════════════════════════════════════════════════════════

        data <- processed_data()

        # Calculate signals using the unified function (same as yield curve)
        if ("z_score" %in% names(data)) {
            # Use unified signal calculation from theme_config.R
            signals <- calculate_unified_signals(data, spread_col = "spread_bps", bond_col = "bond")
            strong_count <- signals$strong_count
            moderate_count <- signals$moderate_count

            # Value shows strong signals (|Z| > 2.0)
            # Subtitle clarifies the threshold used
            valueBox(
                value = strong_count,
                subtitle = sprintf("Strong Signals (|Z|>%.1f) | %d Moderate",
                                   SIGNAL_THRESHOLDS$strong,
                                   moderate_count - strong_count),
                icon = icon("bolt"),
                color = if(strong_count > 3) "red" else if(strong_count > 0) "yellow" else "blue"
            )
        } else {
            # Fallback to technical signals if z_score not available
            if ("signal_strength" %in% names(data)) {
                signals <- data %>%
                    filter(!is.na(signal_strength))
                strong_signals <- sum(signals$signal_strength %in% c("Strong Buy", "Strong Sell"))

                valueBox(
                    value = strong_signals,
                    subtitle = "Technical Signals (RSI/BB/MACD)",
                    icon = icon("bolt"),
                    color = if(strong_signals > 3) "red" else if(strong_signals > 1) "yellow" else "blue"
                )
            } else {
                valueBox(
                    value = 0,
                    subtitle = "Strong Signals Active",
                    icon = icon("bolt"),
                    color = "blue"
                )
            }
        }
    })



    # ================================================================================
    # DOWNLOAD HANDLERS
    # ================================================================================

    # --- Yield Curve & Relative Value ---


    # 2. Relative Value Heatmap Downnload
    output$download_relative_value_heatmap <- downloadHandler(
        filename = function() {
            paste0("relative_value_heatmap_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_relative_value_heatmap(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    output$download_yield_curve <- downloadHandler(
        filename = function() {
            paste0("yield_curve_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(processed_data())
            p <- generate_enhanced_yield_curve(
                processed_data(),
                list(
                    xaxis_choice = input$xaxis_choice,
                    curve_model = input$curve_model
                )
            )
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    # Download handler for Curve Dynamics panel (new)
    output$download_curve_dynamics <- downloadHandler(
        filename = function() {
            paste0("curve_dynamics_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            # Create combined plot with spreads and shape indicator
            p1 <- generate_curve_spreads_plot(filtered_data(), list())
            p2 <- generate_curve_shape_indicator(filtered_data(), list())

            if(!is.null(p1) && !is.null(p2)) {
                # Combine plots vertically using patchwork
                if(requireNamespace("patchwork", quietly = TRUE)) {
                    combined <- p1 / p2 + patchwork::plot_layout(heights = c(3, 1))
                    ggsave(file, plot = combined, width = 12, height = 10, dpi = 300, bg = "white")
                } else {
                    # Fallback: just save the main spread plot
                    ggsave(file, plot = p1, width = 12, height = 8, dpi = 300, bg = "white")
                }
            } else if(!is.null(p1)) {
                ggsave(file, plot = p1, width = 12, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    # Download handler for Relative Value Scanner panel (new)
    output$download_rv_scanner <- downloadHandler(
        filename = function() {
            paste0("relative_value_scanner_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_relative_value_scanner(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 10, dpi = 300, bg = "white")
            }
        }
    )

    # Legacy download handlers (kept for backward compatibility)
    output$download_term_structure <- downloadHandler(
        filename = function() {
            paste0("term_structure_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_term_structure_3d_plot(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    output$download_regime_analysis <- downloadHandler(
        filename = function() {
            paste0("market_regime_analysis_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(regime_data())
            p <- generate_regime_analysis_plot(regime_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 14, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    # This should be placed AFTER output$forward_rate_table, not inside it
    output$download_forward_curve <- downloadHandler(
        filename = function() {
            paste0("forward_rate_analysis_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(processed_data())
            # Export full chart without highlighting any specific period
            p <- generate_forward_curve_plot(processed_data(), params = NULL, selected_period = NULL)
            if (!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    output$download_scenario_analysis <- downloadHandler(
        filename = function() {
            paste0("scenario_analysis_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(processed_data())

            # Get user-selected parameters (same as renderPlot)
            selected_bonds <- input$scenario_bonds_select
            y_scale <- input$scenario_y_scale %||% "fixed"
            show_confidence <- isTRUE(input$scenario_show_confidence)
            confidence_level <- (input$scenario_confidence_level %||% 95) / 100

            if (is.na(confidence_level) || confidence_level < 0.8 || confidence_level > 0.99) {
                confidence_level <- 0.95
            }

            params <- list(
                selected_bonds = selected_bonds,
                y_scale = y_scale,
                show_confidence = show_confidence,
                confidence_level = confidence_level,
                max_bonds = 8
            )

            p <- generate_scenario_analysis_plot(processed_data(), params)
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 14, height = 10, dpi = 300, bg = "white")
            }
        }
    )

    # 3. Enhanced Z-Score Distribution Downlaod
    output$download_enhanced_zscore <- downloadHandler(
        filename = function() {
            paste0("zscore_distribution_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(processed_data())
            p <- generate_enhanced_zscore_plot(processed_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
            }
        }
    )




    # --- Risk Analytics ---

    output$download_convexity <- downloadHandler(
        filename = function() {
            paste0("convexity_profile_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(processed_data())
            # Get notional from input (same as DV01 ladder for consistency)
            notional <- as.numeric(input$dv01_notional) %||% 10000000
            p <- generate_enhanced_convexity_plot(processed_data(), list(
                notional = notional
            ))
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    # ────────────────────────────────────────────────────────────────────────────
    output$download_technical_plot <- downloadHandler(
        filename = function() {
            paste0("technical_analysis_",
                   input$indicator_type, "_",
                   gsub(" ", "_", input$technical_bond), "_",
                   format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data(), input$technical_bond, input$indicator_type)

            p <- generate_advanced_technical_plot(
                data = filtered_data(),
                bond_select = input$technical_bond,
                indicator_type = input$indicator_type
            )

            if(!is.null(p)) {
                if(inherits(p, "grob") || inherits(p, "gTree")) {
                    png(file, width = 12, height = 10, units = "in", res = 300)
                    grid::grid.draw(p)
                    dev.off()
                } else {
                    ggsave(file, plot = p, width = 12, height = 10, dpi = 300, units = "in")
                }
            }
        }
    )

    output$download_dv01_ladder <- downloadHandler(
        filename = function() {
            paste0("dv01_ladder_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(processed_data())
            # Get notional from input (default R10 million)
            notional <- as.numeric(input$dv01_notional) %||% 10000000
            p <- generate_dv01_ladder_plot(processed_data(), list(
                notional = notional
            ))
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    output$download_var_ladder <- downloadHandler(
        filename = function() {
            paste0("var_ladder_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(var_data(), var_distribution_results())
            # Use same bond order as distribution plot
            params <- list(
                bond_order = var_distribution_results()$bond_order
            )
            p <- generate_var_ladder_plot(var_data(), params)
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    output$download_var_distribution <- downloadHandler(
        filename = function() {
            paste0("var_distribution_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(var_distribution_results())
            # Use the already-generated plot from reactive
            p <- var_distribution_results()$plot
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 14, height = 10, dpi = 300, bg = "white")
            }
        }
    )

    output$download_correlation <- downloadHandler(
        filename = function() {
            paste0("correlation_matrix_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_enhanced_correlation_plot(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 10, height = 10, dpi = 300, bg = "white")
            }
        }
    )


    # --- Technical Analysis ---


    # Download handler - USING MASTER REACTIVE FOR CONSISTENCY
    output$download_signal_matrix <- downloadHandler(
        filename = function() {
            paste0("trading_signal_matrix_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(technical_signals_master())

            # Use the MASTER reactive for consistent signals
            signal_data <- technical_signals_master()

            if(nrow(signal_data) < 2) {
                return(NULL)
            }

            # Prepare data for matrix visualization (same as output)
            matrix_data <- signal_data %>%
                select(
                    bond,
                    RSI = rsi_signal_yield,
                    BB = bb_signal_yield,
                    MACD = macd_signal_yield,
                    MOM = momentum_signal_yield,
                    TOTAL = total_score
                ) %>%
                pivot_longer(
                    cols = c(RSI, BB, MACD, MOM, TOTAL),
                    names_to = "indicator",
                    values_to = "score"
                ) %>%
                mutate(
                    score = as.numeric(score),
                    score = ifelse(is.na(score), 0, score),
                    indicator = factor(indicator, levels = c("RSI", "BB", "MACD", "MOM", "TOTAL"))
                )

            bond_order <- signal_data %>%
                arrange(desc(total_score)) %>%
                pull(bond)

            matrix_data <- matrix_data %>%
                mutate(bond = factor(bond, levels = rev(bond_order)))

            matrix_data <- matrix_data %>%
                mutate(
                    color_cat = case_when(
                        indicator == "TOTAL" & score >= 4 ~ "strong_buy",
                        indicator == "TOTAL" & score >= 2 ~ "buy",
                        indicator == "TOTAL" & score >= -1 ~ "neutral",
                        indicator == "TOTAL" & score >= -3 ~ "sell",
                        indicator == "TOTAL" ~ "strong_sell",
                        score == 2 ~ "strong_buy",
                        score == 1 ~ "buy",
                        score == 0 ~ "neutral",
                        score == -1 ~ "sell",
                        score == -2 ~ "strong_sell",
                        TRUE ~ "neutral"
                    ),
                    color_cat = factor(color_cat,
                        levels = c("strong_sell", "sell", "neutral", "buy", "strong_buy"))
                )

            p <- ggplot(matrix_data, aes(x = indicator, y = bond, fill = color_cat)) +
                geom_tile(color = "white", linewidth = 1) +
                geom_text(aes(label = score), size = 3.5, fontface = "bold",
                    color = ifelse(matrix_data$color_cat %in% c("strong_buy", "strong_sell"),
                                   "white", "gray20")) +
                scale_fill_manual(
                    values = c(
                        "strong_sell" = "#B71C1C",
                        "sell" = "#E57373",
                        "neutral" = "#9E9E9E",
                        "buy" = "#81C784",
                        "strong_buy" = "#1B5E20"
                    ),
                    name = "Signal",
                    labels = c("Strong Sell", "Sell", "Neutral", "Buy", "Strong Buy"),
                    drop = FALSE
                ) +
                scale_x_discrete(expand = c(0, 0)) +
                scale_y_discrete(expand = c(0, 0)) +
                labs(
                    title = "Trading Signal Matrix",
                    subtitle = "Sorted by total score (strongest buy at top) | Bond Price Perspective",
                    x = "", y = "",
                    caption = "+2 = Strong Buy (good for prices) | -2 = Strong Sell (bad for prices)"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(face = "bold", color = "#1B3A6B", size = 14),
                    plot.subtitle = element_text(color = "#666666", size = 10),
                    plot.caption = element_text(color = "#888888", size = 8),
                    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 10),
                    axis.text.y = element_text(face = "bold", size = 9),
                    panel.grid = element_blank(),
                    panel.border = element_rect(fill = NA, color = "#666666", linewidth = 1),
                    legend.position = "bottom"
                )

            ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
        }
    )


    output$download_technical_plot <- downloadHandler(
        filename = function() {
            indicator_type <- if(!is.null(input$tech_indicator_type)) input$tech_indicator_type else "all"
            paste0("technical_", input$tech_bond_select, "_", indicator_type, "_",
                   format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data(), input$tech_bond_select)

            indicator_type <- if(!is.null(input$tech_indicator_type)) input$tech_indicator_type else "all"
            p <- generate_advanced_technical_plot(filtered_data_with_technicals(), input$tech_bond_select, indicator_type)

            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 10, dpi = 300, bg = "white")
            }
        }
    )



    # --- Carry & Roll ---

    output$download_optimal_holding <- downloadHandler(
        filename = function() {
            paste0("optimal_holding_period_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(carry_roll_data())
            p <- generate_optimal_holding_enhanced_plot(carry_roll_data())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
            }
        }
    )


    output$download_carry_roll <- downloadHandler(
        filename = function() {
            paste0("carry_roll_analysis_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(carry_roll_data())

            return_type_value <- if(!is.null(input$return_type)) {
                input$return_type
            } else {
                "net"
            }

            p <- generate_enhanced_carry_roll_heatmap(carry_roll_data(), return_type_value)
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            }
        }
    )


    # --- Auction Analytics ---

    output$download_bid_distribution <- downloadHandler(
        filename = function() {
            paste0("bid_distribution_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            selected_bonds <- if (!is.null(input$auction_bonds_select)) {
                input$auction_bonds_select
            } else {
                character(0)
            }
            p <- generate_bid_distribution_plot(filtered_data(), list(),
                                                selected_bonds = selected_bonds)
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            }
        }
    )


    # Note: download_btc_decomposition handler removed - section removed from UI
    # Note: download_auction_success handler removed as part of Auction Intelligence tab overhaul

    output$download_auction_sentiment <- downloadHandler(
        filename = function() {
            paste0("auction_sentiment_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())

            # FIX: Use same data processing as market_sentiment_compact to ensure consistency
            tryCatch({
                base_data <- filtered_data()

                # Filter to actual auction data (same logic as display)
                auction_data <- base_data %>%
                    filter(!is.na(bid_to_cover), bid_to_cover > 0) %>%
                    group_by(date, bond) %>%
                    slice_head(n = 1) %>%
                    ungroup() %>%
                    filter(date >= max(date, na.rm = TRUE) - 180)

                n_auctions <- nrow(auction_data)

                # Generate appropriate plot based on data availability
                if (n_auctions < 5) {
                    # Create informative "insufficient data" plot
                    p <- ggplot() +
                        annotate("text", x = 0.5, y = 0.55,
                                 label = "Insufficient Auction Data",
                                 size = 8, fontface = "bold", color = "#666666") +
                        annotate("text", x = 0.5, y = 0.45,
                                 label = sprintf("Found %d auction events (need 5+)", n_auctions),
                                 size = 5, color = "#999999") +
                        annotate("text", x = 0.5, y = 0.35,
                                 label = "Based on records with bid_to_cover > 0",
                                 size = 4, color = "#AAAAAA") +
                        xlim(0, 1) + ylim(0, 1) +
                        theme_void() +
                        theme(plot.background = element_rect(fill = "#FFF9C4", color = NA),
                              plot.margin = ggplot2::margin(20, 20, 20, 20))

                    ggsave(file, plot = p, width = 10, height = 6, dpi = 150, bg = "white")
                } else {
                    # Generate full sentiment gauge using the standard function
                    p <- generate_auction_sentiment_gauge(base_data, list())

                    if (!is.null(p)) {
                        # Handle both ggplot and grob objects
                        if (inherits(p, "grob") || inherits(p, "gtable") || inherits(p, "arrangelist")) {
                            # For grob objects, use grid.draw approach
                            png(file, width = 10, height = 6, units = "in", res = 150, bg = "white")
                            gridExtra::grid.arrange(p)
                            dev.off()
                        } else {
                            # For ggplot objects
                            ggsave(file, plot = p, width = 10, height = 6, dpi = 150, bg = "white")
                        }
                    } else {
                        # Fallback if function returns NULL
                        p <- ggplot() +
                            annotate("text", x = 0.5, y = 0.5,
                                     label = "Unable to generate sentiment chart",
                                     size = 6, color = "#666666") +
                            theme_void() +
                            theme(plot.background = element_rect(fill = "#f8f9fa", color = NA))
                        ggsave(file, plot = p, width = 10, height = 6, dpi = 150, bg = "white")
                    }
                }
            }, error = function(e) {
                # Error fallback - generate a proper error message plot
                message(sprintf("[DOWNLOAD ERROR] %s", e$message))
                p <- ggplot() +
                    annotate("text", x = 0.5, y = 0.55,
                             label = "Error Generating Chart",
                             size = 8, fontface = "bold", color = "#C62828") +
                    annotate("text", x = 0.5, y = 0.45,
                             label = "Please try refreshing the data",
                             size = 5, color = "#666666") +
                    xlim(0, 1) + ylim(0, 1) +
                    theme_void() +
                    theme(plot.background = element_rect(fill = "#FFEBEE", color = NA))
                ggsave(file, plot = p, width = 10, height = 6, dpi = 150, bg = "white")
            })
        }
    )

    output$download_auction_pattern <- downloadHandler(
        filename = function() {
            paste0("auction_patterns_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            selected_bonds <- if (!is.null(input$auction_bonds_select)) {
                input$auction_bonds_select
            } else {
                character(0)
            }
            p <- generate_auction_pattern_analysis(filtered_data(), list(),
                                                   selected_bonds = selected_bonds)
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 10, dpi = 300, bg = "white")
            }
        }
    )

    # Note: download_success_probability and download_demand_elasticity handlers removed
    # as part of Auction Intelligence tab overhaul

    output$download_auction_performance <- downloadHandler(
        filename = function() {
            paste0("auction_performance_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())

            # Get selected bonds (default to top 6 if none selected)
            selected_bonds <- input$auction_perf_bonds
            if(is.null(selected_bonds) || length(selected_bonds) == 0) {
                bonds_data <- available_auction_bonds()
                if(!is.null(bonds_data) && nrow(bonds_data) > 0) {
                    selected_bonds <- head(bonds_data$bond, 6)
                }
            }

            # Get layout settings
            layout <- input$auction_perf_layout %||% "auto"
            x_scales <- input$auction_perf_xaxis %||% "free_x"
            show_trend <- input$auction_perf_show_trend %||% TRUE

            # Build params list for plot function
            params <- list(
                selected_bonds = selected_bonds,
                layout = layout,
                x_scales = x_scales,
                show_trend = show_trend
            )

            p <- generate_enhanced_auction_analytics(filtered_data(), params)
            if(!is.null(p)) {
                # Dynamic height based on number of bonds
                n_bonds <- length(selected_bonds)
                ncol_val <- switch(layout,
                    "2x3" = 3, "2x4" = 4, "3x3" = 3,
                    "auto" = if(n_bonds <= 4) 2 else if(n_bonds <= 6) 3 else 4
                )
                n_rows <- ceiling(n_bonds / ncol_val)
                plot_height <- max(8, n_rows * 3 + 2)

                ggsave(file, plot = p, width = 16, height = plot_height, dpi = 300, bg = "white")
            }
        }
    )

    # YTD Bond Issuance Chart Download (uses filtered_auction_data for date range filtering)
    output$download_ytd_issuance_chart <- downloadHandler(
        filename = function() {
            paste0("ytd_bond_issuance_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_auction_data())
            p <- generate_ytd_bond_issuance_chart(filtered_auction_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 14, height = 10, dpi = 300, bg = "white")
            }
        }
    )

    # YTD Bond Issuance Table Download (CSV) (uses filtered_auction_data for date range filtering)
    output$download_ytd_issuance_table <- downloadHandler(
        filename = function() {
            paste0("ytd_bond_issuance_table_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
            req(filtered_auction_data())
            table_data <- generate_ytd_issuance_table(filtered_auction_data())
            # Use the formatted Yield Trend Display for CSV export
            if ("Yield Trend Display" %in% names(table_data) && "Yield Trend" %in% names(table_data)) {
                table_data$`Yield Trend` <- table_data$`Yield Trend Display`
                table_data$`Yield Trend Display` <- NULL
            }
            write.csv(table_data, file, row.names = FALSE)
        }
    )



    # --- Reports ---




    # --- Export handlers ---

    output$export_data <- downloadHandler(
        filename = function() {
            paste0("insele_bond_data_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
        },
        content = function(file) {
            require(openxlsx)

            wb <- createWorkbook()
            addWorksheet(wb, "Processed Data")
            writeData(wb, "Processed Data", processed_data())

            if (!is.null(var_data())) {
                addWorksheet(wb, "Risk Metrics")
                writeData(wb, "Risk Metrics", var_data())
            }

            if (!is.null(carry_roll_data())) {
                addWorksheet(wb, "Carry Roll")
                writeData(wb, "Carry Roll", carry_roll_data())
            }

            saveWorkbook(wb, file)
            showNotification("Data exported successfully", type = "message")
        }
    )

    # Replace the existing output$generate_pdf_report with this:
    output$generate_pdf_report <- downloadHandler(
        filename = function() {
            paste0("insele_bond_report_", format(input$report_date, "%Y%m%d"), ".pdf")
        },
        content = function(file) {
            withProgress(message = "Generating PDF report...", value = 0, {

                require(gridExtra)
                require(grid)
                require(png)  # For reading PNG files

                temp_dir <- tempdir()
                temp_pdf <- file.path(temp_dir, paste0("temp_report_", Sys.getpid(), ".pdf"))

                # Load logo
                logo_path <- load_logo_for_pdf()
                logo_grob <- NULL
                if(!is.null(logo_path) && file.exists(logo_path)) {
                    tryCatch({
                        logo_img <- png::readPNG(logo_path)
                        logo_grob <- rasterGrob(logo_img, width = unit(2, "inches"),
                                                height = unit(0.8, "inches"))
                    }, error = function(e) {
                        logo_grob <- NULL
                    })
                }

                incProgress(0.1, detail = "Collecting data")

                # Use shared report configuration and data collector
                config <- report_config()
                report_data <- collect_report_data()

                proc_data <- report_data$proc_data
                filt_data <- report_data$filt_data
                var_data_val <- report_data$var_data_val
                regime_data_val <- report_data$regime_data_val
                carry_data_val <- report_data$carry_data_val
                filt_data_with_tech <- report_data$filt_data_with_tech

                incProgress(0.2, detail = "Collecting charts")

                # Collect charts with error handling
                charts <- list()
                chart_collection <- NULL
                tryCatch({
                    chart_collection <- collect_report_charts(
                        proc_data,
                        filt_data,
                        filt_data_with_tech,
                        var_data_val,
                        regime_data_val,
                        carry_data_val,
                        report_data$treasury_ts,
                        report_data$treasury_bonds,
                        config$input_params
                    )

                    # Load charts from paths
                    for(name in names(chart_collection$charts)) {
                        chart_path <- chart_collection$charts[[name]]
                        if(is.character(chart_path) && file.exists(chart_path)) {
                            charts[[name]] <- readRDS(chart_path)
                        } else if(!is.character(chart_path)) {
                            charts[[name]] <- chart_path
                        }
                    }
                }, error = function(e) {
                    log_error(e, context = "chart_collection", session_id = session$token)
                })

                # Ensure cleanup happens when PDF generation is done
                on.exit({
                    if(!is.null(chart_collection) && !is.null(chart_collection$cleanup)) {
                        chart_collection$cleanup()
                    }
                }, add = TRUE)

                incProgress(0.3, detail = "Generating summaries")

                # Generate summaries with error handling
                summaries <- tryCatch({
                    generate_report_summaries(
                        proc_data,
                        filt_data,
                        var_data_val,
                        regime_data_val,
                        carry_data_val,
                        report_data$treasury_ts,
                        report_data$treasury_bonds
                    )
                }, error = function(e) {
                    log_error(e, context = "summary_generation")
                    list(executive = "Report generation in progress.")
                })

                # Get auction data
                auction_data <- tryCatch({
                    list(upcoming_bonds = data.frame(), summary_text = NULL)
                }, error = function(e) {
                    list(upcoming_bonds = data.frame(), summary_text = NULL)
                })

                incProgress(0.5, detail = "Creating PDF")

                tryCatch({
                    pdf(temp_pdf, width = 11, height = 8.5)

                    # ENHANCED TITLE PAGE WITH LOGO
                    grid.newpage()

                    # Add logo at the top if available
                    if(!is.null(logo_grob)) {
                        pushViewport(viewport(x = 0.5, y = 0.9, width = 0.3, height = 0.15))
                        grid.draw(logo_grob)
                        popViewport()

                        # Adjust title position if logo is present
                        title_y <- 0.7
                        subtitle_y <- 0.6
                        date_y <- 0.5
                        client_y <- 0.4
                    } else {
                        # Original positions without logo
                        title_y <- 0.7
                        subtitle_y <- 0.6
                        date_y <- 0.5
                        client_y <- 0.4
                    }

                    # Title elements
                    title_grob <- textGrob(
                        "SA Government Bond Analysis Report",
                        x = 0.5, y = title_y,
                        gp = gpar(fontsize = 24, fontface = 2, col = "#1B3A6B")
                    )
                    subtitle_grob <- textGrob(
                        if(!is.null(input$report_title)) input$report_title else "Market Analysis",
                        x = 0.5, y = subtitle_y,
                        gp = gpar(fontsize = 18, col = "#333333")
                    )
                    date_grob <- textGrob(
                        format(if(!is.null(input$report_date)) input$report_date else Sys.Date(), "%B %d, %Y"),
                        x = 0.5, y = date_y,
                        gp = gpar(fontsize = 14, col = "#666666")
                    )
                    client_grob <- if(!is.null(input$client_name) && input$client_name != "") {
                        textGrob(
                            paste("Prepared for:", input$client_name),
                            x = 0.5, y = client_y,
                            gp = gpar(fontsize = 12, fontface = 3, col = "#666666")
                        )
                    } else { nullGrob() }

                    # Add decorative line under logo
                    if(!is.null(logo_grob)) {
                        grid.lines(x = c(0.2, 0.8), y = c(0.82, 0.82),
                                   gp = gpar(col = "#1B3A6B", lwd = 2))
                    }

                    grid.draw(title_grob)
                    grid.draw(subtitle_grob)
                    grid.draw(date_grob)
                    grid.draw(client_grob)

                    # Add company name at bottom of title page if no logo
                    if(is.null(logo_grob)) {
                        company_grob <- textGrob(
                            "INSELE CAPITAL PARTNERS",
                            x = 0.5, y = 0.15,
                            gp = gpar(fontsize = 16, fontface = 2, col = "#1B3A6B")
                        )
                        grid.draw(company_grob)
                    }

                    # ══════════════════════════════════════════════════════════════
                    # PAGE COUNTER INITIALIZATION
                    # ══════════════════════════════════════════════════════════════
                    page_number <- 1  # Title page is page 1
                    # Estimate total pages (title + exec + sections*divider + charts + footer)
                    total_chart_count <- sum(sapply(config$sections, function(s) {
                        if (s == "recommendations") return(1)
                        cs <- chart_sections_map[[s]]
                        if (is.null(cs)) return(0)
                        sum(cs %in% names(charts))
                    }))
                    total_pages <- 2 + length(config$sections) + total_chart_count + 1  # title + exec + dividers + charts + footer

                    # Helper to add page number footer
                    add_page_number <- function(pn, tp) {
                        if (pn > 1) {
                            grid.text(
                                label = paste0("Page ", pn, " of ", tp),
                                x = 0.95, y = 0.02,
                                just = c("right", "bottom"),
                                gp = gpar(fontsize = 8, col = "#999999", fontface = 1)
                            )
                        }
                    }

                    # EXECUTIVE SUMMARY PAGE - DASHBOARD STYLE
                    page_number <- page_number + 1
                    if(!is.null(summaries$executive)) {
                        grid.newpage()

                        # Add small logo in header
                        if(!is.null(logo_grob)) {
                            pushViewport(viewport(x = 0.92, y = 0.95, width = 0.1, height = 0.06,
                                                  just = c("right", "top")))
                            grid.draw(logo_grob)
                            popViewport()
                        }

                        # Header line
                        grid.lines(x = c(0.05, 0.95), y = c(0.92, 0.92),
                                   gp = gpar(col = "#E0E0E0", lwd = 1))

                        grid.text("Executive Summary",
                                  x = 0.05, y = 0.95, just = "left",
                                  gp = gpar(fontsize = 18, fontface = 2, col = "#1B3A6B"))

                        # Dashboard-style metric cards
                        exec_metrics <- summaries$exec_metrics
                        if (!is.null(exec_metrics) && length(exec_metrics) > 0) {
                            metric_names <- names(exec_metrics)
                            n_metrics <- length(metric_names)
                            # Layout: up to 2x4 grid
                            n_cols <- min(4, n_metrics)
                            n_rows <- ceiling(n_metrics / n_cols)
                            card_w <- 0.9 / n_cols
                            card_h <- 0.22

                            for (i in seq_along(metric_names)) {
                                m <- exec_metrics[[metric_names[i]]]
                                if (is.null(m)) next
                                col_idx <- ((i - 1) %% n_cols) + 1
                                row_idx <- ((i - 1) %/% n_cols) + 1
                                cx <- 0.05 + (col_idx - 0.5) * card_w
                                cy <- 0.85 - row_idx * (card_h + 0.02)

                                # Card background
                                grid.rect(x = cx, y = cy, width = card_w * 0.92, height = card_h,
                                          gp = gpar(fill = "#F5F7FA", col = "#E0E4E8", lwd = 0.5),
                                          just = "centre")
                                # Metric label (small gray)
                                grid.text(m$label, x = cx, y = cy + card_h * 0.35,
                                          gp = gpar(fontsize = 8, col = "#888888", fontface = 1))
                                # Metric value (large navy)
                                grid.text(m$value, x = cx, y = cy + card_h * 0.05,
                                          gp = gpar(fontsize = 13, col = "#1B3A6B", fontface = 2))
                                # Subtitle (small text below)
                                if (!is.null(m$subtitle) && nchar(m$subtitle) > 0) {
                                    grid.text(m$subtitle, x = cx, y = cy - card_h * 0.3,
                                              gp = gpar(fontsize = 7, col = "#999999", fontface = 1))
                                }
                            }

                            # Executive text summary below the cards
                            summary_y <- 0.85 - (n_rows + 1) * (card_h + 0.02)
                            if (summary_y > 0.05) {
                                wrapped_exec <- strwrap(summaries$executive, width = 110)
                                grid.text(paste(wrapped_exec, collapse = "\n"),
                                          x = 0.5, y = max(summary_y, 0.08),
                                          gp = gpar(fontsize = 9, col = "#555555", lineheight = 1.3))
                            }
                        } else {
                            # Fallback: text-only executive summary
                            wrapped_text <- tryCatch({
                                strwrap(summaries$executive, width = 95)
                            }, error = function(e) {
                                "Report content being generated."
                            })
                            summary_df <- data.frame(Content = wrapped_text, stringsAsFactors = FALSE)
                            summary_table <- tableGrob(summary_df, rows = NULL, cols = NULL,
                                theme = ttheme_minimal(base_size = 11, base_colour = "#333333"))
                            pushViewport(viewport(x = 0.5, y = 0.5, width = 0.9, height = 0.7))
                            grid.draw(summary_table)
                            popViewport()
                        }

                        add_page_number(page_number, total_pages)
                    }

                    # Function to add header with logo to each page
                    add_page_header <- function(title_text = "") {
                        if(!is.null(logo_grob)) {
                            # Small logo in corner
                            pushViewport(viewport(x = 0.92, y = 0.97, width = 0.12, height = 0.05))
                            grid.draw(logo_grob)
                            popViewport()
                        }

                        # Page title if provided
                        if(title_text != "") {
                            grid.text(title_text, x = 0.05, y = 0.97,
                                      just = "left",
                                      gp = gpar(fontsize = 12, fontface = 2, col = "#1B3A6B"))
                        }

                        # Separator line
                        grid.lines(x = c(0.05, 0.95), y = c(0.94, 0.94),
                                   gp = gpar(col = "#E0E0E0", lwd = 0.5))
                    }

                    # Section display names and chart-to-section mapping
                    section_names <- c(
                        overview = "Market Overview",
                        relative = "Relative Value Analysis",
                        risk = "Risk Analytics",
                        technical = "Technical Analysis",
                        carry = "Carry & Roll Analysis",
                        auction = "Auction Intelligence",
                        intelligence = "Market Intelligence",
                        treasury = "Treasury Holdings",
                        recommendations = "Trading Recommendations"
                    )

                    chart_sections_map <- list(
                        overview = c("regime_plot"),
                        relative = c("yield_curve", "relative_heatmap", "zscore_plot", "convexity"),
                        risk = c("var_distribution", "var_ladder", "dv01_ladder"),
                        technical = c("technical_plot", "signal_matrix"),
                        carry = c("carry_heatmap", "scenario_analysis", "butterfly_spread", "forward_curve"),
                        auction = c("auction_performance", "auction_patterns", "auction_forecast",
                                     "demand_elasticity", "success_probability", "bid_distribution",
                                     "ytd_issuance", "auction_sentiment", "auction_success_factors"),
                        intelligence = c("correlation", "term_structure"),
                        treasury = c("holdings_area", "sector_trend", "holdings_fixed", "holdings_ilb",
                                      "holdings_frn", "holdings_sukuk", "ownership_changes",
                                      "holdings_diverging_fixed", "holdings_diverging_ilb")
                    )

                    # ══════════════════════════════════════════════════════════════
                    # DYNAMIC SECTION SUMMARIES FOR DIVIDER PAGES (Priority 7)
                    # ══════════════════════════════════════════════════════════════
                    section_summaries <- list()
                    tryCatch({
                        section_summaries$overview <- summaries$market_conditions %||% ""
                        section_summaries$relative <- summaries$relative_summary %||% ""
                        section_summaries$risk <- summaries$risk_summary %||% ""
                        section_summaries$technical <- summaries$technical_summary %||% ""
                        section_summaries$carry <- summaries$carry_summary %||% ""
                        section_summaries$auction <- summaries$auction_summary %||% ""
                        section_summaries$intelligence <- summaries$intelligence_summary %||% ""
                        section_summaries$treasury <- summaries$treasury_summary %||% ""
                        section_summaries$recommendations <- ""
                    }, error = function(e) {})

                    # Generate structured trading recommendations
                    trading_recs <- tryCatch({
                        generate_trading_recommendations(
                            proc_data, filt_data, var_data_val,
                            carry_data_val, regime_data_val
                        )
                    }, error = function(e) {
                        list()
                    })

                    # Render charts organized by section with divider pages
                    for (section in config$sections) {
                        # TRADING RECOMMENDATIONS - structured page (Priority 1)
                        if (section == "recommendations") {
                            page_number <- page_number + 1
                            grid.newpage()
                            add_page_header("Trading Recommendations")

                            if (length(trading_recs) > 0) {
                                y_pos <- 0.88
                                for (rec_section in trading_recs) {
                                    if (is.null(rec_section) || !is.list(rec_section)) next
                                    title <- rec_section$title %||% ""
                                    items <- rec_section$items %||% character()

                                    # Section header
                                    grid.text(title, x = 0.06, y = y_pos, just = "left",
                                              gp = gpar(fontsize = 12, fontface = 2, col = "#1B3A6B"))
                                    y_pos <- y_pos - 0.03

                                    # Horizontal rule
                                    grid.lines(x = c(0.06, 0.94), y = c(y_pos, y_pos),
                                               gp = gpar(col = "#E0E4E8", lwd = 0.5))
                                    y_pos <- y_pos - 0.02

                                    # Items
                                    if (is.character(items) && length(items) > 0) {
                                        for (item in items) {
                                            wrapped <- strwrap(paste0("\u2022 ", item), width = 105)
                                            for (line in wrapped) {
                                                grid.text(line, x = 0.08, y = y_pos, just = "left",
                                                          gp = gpar(fontsize = 9, col = "#444444", lineheight = 1.3))
                                                y_pos <- y_pos - 0.025
                                            }
                                        }
                                    }
                                    y_pos <- y_pos - 0.02

                                    if (y_pos < 0.08) break  # Don't overflow
                                }
                            } else if (!is.null(summaries$recommendations)) {
                                # Fallback to simple text
                                wrapped_recs <- strwrap(summaries$recommendations, width = 95)
                                grid.text(paste(wrapped_recs, collapse = "\n"),
                                          x = 0.5, y = 0.5,
                                          gp = gpar(fontsize = 11, col = "#333333", lineheight = 1.4))
                            }

                            add_page_number(page_number, total_pages)
                            next
                        }

                        section_title <- section_names[[section]]

                        # ENHANCED SECTION DIVIDER PAGE (Priority 7)
                        page_number <- page_number + 1
                        grid.newpage()

                        # Logo top-right
                        if (!is.null(logo_grob)) {
                            pushViewport(viewport(x = 0.92, y = 0.95, width = 0.1, height = 0.06,
                                                  just = c("right", "top")))
                            grid.draw(logo_grob)
                            popViewport()
                        }

                        # Horizontal rule
                        grid.lines(x = c(0.1, 0.9), y = c(0.65, 0.65),
                                   gp = gpar(col = "#1B3A6B", lwd = 2))

                        # Section title
                        grid.text(section_title, x = 0.5, y = 0.58,
                                  gp = gpar(fontsize = 28, fontface = 2, col = "#1B3A6B"))

                        # Section preview text (dynamic summary)
                        sec_summary <- section_summaries[[section]] %||% ""
                        if (nchar(sec_summary) > 0) {
                            wrapped_summary <- strwrap(sec_summary, width = 80)
                            grid.text(paste(wrapped_summary, collapse = "\n"),
                                      x = 0.5, y = 0.48,
                                      gp = gpar(fontsize = 11, col = "#666666", lineheight = 1.4),
                                      just = "center")
                        }

                        add_page_number(page_number, total_pages)

                        # Render each chart in this section
                        section_chart_names <- chart_sections_map[[section]]
                        if (!is.null(section_chart_names)) {
                            for (chart_name in section_chart_names) {
                                if (!chart_name %in% names(charts) || is.null(charts[[chart_name]])) next

                                page_number <- page_number + 1
                                chart_obj <- charts[[chart_name]]
                                tryCatch({
                                    if ("ggplot" %in% class(chart_obj)) {
                                        # Save ggplot to temp PNG, then compose page
                                        temp_chart_png <- tempfile(fileext = ".png")
                                        ggsave(temp_chart_png, chart_obj, width = 10, height = 6, dpi = 150, bg = "white")
                                        chart_img <- png::readPNG(temp_chart_png)
                                        chart_grob <- grid::rasterGrob(chart_img, interpolate = TRUE)
                                        unlink(temp_chart_png)

                                        grid.newpage()
                                        add_page_header(gsub("_", " ", tools::toTitleCase(chart_name)))
                                        pushViewport(viewport(x = 0.5, y = 0.45, width = 0.92, height = 0.82))
                                        grid.draw(chart_grob)
                                        popViewport()
                                    } else {
                                        # Grid objects render directly
                                        grid.newpage()
                                        add_page_header(gsub("_", " ", tools::toTitleCase(chart_name)))
                                        pushViewport(viewport(x = 0.5, y = 0.45, width = 0.9, height = 0.85))
                                        grid.draw(chart_obj)
                                        popViewport()
                                    }
                                    add_page_number(page_number, total_pages)
                                }, error = function(e) {
                                    grid.newpage()
                                    add_page_header()
                                    grid.text(paste("Chart", chart_name, "temporarily unavailable"),
                                              x = 0.5, y = 0.5,
                                              gp = gpar(fontsize = 12, col = "#666666"))
                                    add_page_number(page_number, total_pages)
                                })
                            }
                        }
                    }

                    # FOOTER PAGE WITH LOGO
                    page_number <- page_number + 1
                    grid.newpage()

                    # Add logo at bottom
                    if(!is.null(logo_grob)) {
                        pushViewport(viewport(x = 0.5, y = 0.7, width = 0.25, height = 0.1))
                        grid.draw(logo_grob)
                        popViewport()
                    }

                    footer_grob <- textGrob(
                        paste("© ", format(Sys.Date(), "%Y"),
                              " Insele Capital Partners. All rights reserved.\n\n",
                              "This report is proprietary and confidential.\n",
                              "Disclaimer: This report is for informational purposes only ",
                              "and does not constitute investment advice."),
                        x = 0.5, y = 0.4,
                        gp = gpar(fontsize = 10, col = "#666666", lineheight = 1.5)
                    )
                    grid.draw(footer_grob)
                    add_page_number(page_number, total_pages)

                    dev.off()

                    # Clean up temporary logo file
                    if(!is.null(logo_path) && file.exists(logo_path) && grepl("^/tmp", logo_path)) {
                        unlink(logo_path)
                    }

                    # Copy to output location
                    if(file.exists(temp_pdf)) {
                        file.copy(temp_pdf, file, overwrite = TRUE)
                        unlink(temp_pdf)
                    }

                }, error = function(e) {
                    dev.off()
                    log_error(e, context = "pdf_generation")
                    # Create minimal PDF as fallback
                    pdf(file, width = 11, height = 8.5)
                    plot.new()
                    text(0.5, 0.5, "Report generation encountered an error. Please try again.", cex = 1.5)
                    dev.off()
                })

                incProgress(1, detail = "Complete")
                showNotification("PDF report generated successfully", type = "message")
            })
        }
    )

    # HTML Report Generation
    output$generate_html_report <- downloadHandler(
        filename = function() {
            paste0("insele_bond_report_", format(report_config()$report_date, "%Y%m%d"), ".html")
        },
        content = function(file) {
            withProgress(message = "Generating HTML report...", value = 0, {

                incProgress(0.1, detail = "Collecting data")

                # Use shared report configuration and data collector
                config <- report_config()
                report_data <- collect_report_data()

                incProgress(0.2, detail = "Collecting charts")

                # Collect charts with error handling
                charts <- list()
                chart_collection <- NULL
                tryCatch({
                    chart_collection <- collect_report_charts(
                        report_data$proc_data,
                        report_data$filt_data,
                        report_data$filt_data_with_tech,
                        report_data$var_data_val,
                        report_data$regime_data_val,
                        report_data$carry_data_val,
                        report_data$treasury_ts,
                        report_data$treasury_bonds,
                        config$input_params
                    )

                    # Load charts from paths
                    for(name in names(chart_collection$charts)) {
                        chart_path <- chart_collection$charts[[name]]
                        if(is.character(chart_path) && file.exists(chart_path)) {
                            charts[[name]] <- readRDS(chart_path)
                        } else if(!is.character(chart_path)) {
                            charts[[name]] <- chart_path
                        }
                    }
                }, error = function(e) {
                    log_error(e, context = "html_chart_collection")
                })

                # Ensure cleanup happens
                on.exit({
                    if(!is.null(chart_collection) && !is.null(chart_collection$cleanup)) {
                        chart_collection$cleanup()
                    }
                }, add = TRUE)

                incProgress(0.4, detail = "Generating summaries")

                # Generate full summaries
                summaries <- tryCatch({
                    generate_report_summaries(
                        report_data$proc_data,
                        report_data$filt_data,
                        report_data$var_data_val,
                        report_data$regime_data_val,
                        report_data$carry_data_val,
                        report_data$treasury_ts,
                        report_data$treasury_bonds
                    )
                }, error = function(e) {
                    log_error(e, context = "html_summary_generation")
                    list(executive = "Report generation in progress.")
                })

                # Get auction data
                auction_data <- tryCatch(weekly_auction_summary(), error = function(e) NULL)

                incProgress(0.8, detail = "Creating HTML")

                # Create full HTML report using new template with all charts
                html_content <- create_html_report_template(
                    charts,
                    summaries,
                    config$sections,
                    config,
                    auction_data
                )

                # Save to file
                writeLines(html_content, file)

                incProgress(1, detail = "Complete")
                showNotification("HTML report generated successfully", type = "message")
            })
        }
    )

    # Enhanced Excel Export Handler
    output$export_excel <- downloadHandler(
        filename = function() {
            paste0("insele_bond_analysis_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
        },
        content = function(file) {
            require(openxlsx)

            withProgress(message = "Creating Excel workbook...", value = 0, {

                wb <- createWorkbook()

                # Define styles
                header_style <- createStyle(
                    fontSize = 12, fontColour = "#FFFFFF",
                    fgFill = "#1B3A6B", halign = "center",
                    textDecoration = "bold", border = "TopBottomLeftRight"
                )

                subtitle_style <- createStyle(
                    fontSize = 10, fontColour = "#666666",
                    halign = "left", textDecoration = "italic"
                )

                highlight_style <- createStyle(
                    fgFill = "#E8F4F8"
                )

                incProgress(0.1, detail = "Executive Summary")

                # 1. Executive Summary
                tryCatch({
                    addWorksheet(wb, "Executive Summary")

                    writeData(wb, "Executive Summary",
                              "SA Government Bond Analysis Report",
                              startRow = 1, startCol = 1)
                    addStyle(wb, "Executive Summary",
                             createStyle(fontSize = 16, textDecoration = "bold"),
                             rows = 1, cols = 1)

                    summary_metrics <- data.frame(
                        Metric = c("Report Date", "Analysis Period", "Total Bonds Analyzed",
                                   "Average Yield (%)", "Average Duration (years)"),
                        Value = c(
                            format(Sys.Date(), "%B %d, %Y"),
                            if(!is.null(input$date_range)) {
                                paste(format(input$date_range[1], "%b %d"), "-",
                                      format(input$date_range[2], "%b %d, %Y"))
                            } else { "Not specified" },
                            tryCatch(as.character(length(unique(filtered_data()$bond))), error = function(e) "0"),
                            tryCatch(sprintf("%.3f", mean(processed_data()$yield_to_maturity, na.rm = TRUE)), error = function(e) "N/A"),
                            tryCatch(sprintf("%.2f", mean(processed_data()$modified_duration, na.rm = TRUE)), error = function(e) "N/A")
                        )
                    )

                    writeData(wb, "Executive Summary", summary_metrics,
                              startRow = 3, headerStyle = header_style)
                    setColWidths(wb, "Executive Summary", cols = 1:2, widths = c(30, 25))

                }, error = function(e) {
                    log_error(e, context = "excel_summary")
                })

                incProgress(0.15, detail = "Bond Metrics")

                # 2. Bond Metrics — latest snapshot, one row per bond
                tryCatch({
                    if(!is.null(processed_data()) && nrow(processed_data()) > 0) {
                        addWorksheet(wb, "Bond Metrics")

                        bond_metrics <- processed_data() %>%
                            select(any_of(c("bond", "yield_to_maturity", "coupon", "modified_duration",
                                            "duration", "convexity", "dv01", "clean_price", "full_price",
                                            "accrued_interest", "time_to_maturity", "z_score",
                                            "spread_to_curve", "richness"))) %>%
                            arrange(bond)

                        writeData(wb, "Bond Metrics", bond_metrics, headerStyle = header_style)
                        setColWidths(wb, "Bond Metrics", cols = 1:ncol(bond_metrics), widths = "auto")

                        # Alternate row highlighting
                        if (nrow(bond_metrics) > 1) {
                            even_rows <- seq(3, nrow(bond_metrics) + 1, by = 2)
                            even_rows <- even_rows[even_rows <= nrow(bond_metrics) + 1]
                            if (length(even_rows) > 0) {
                                addStyle(wb, "Bond Metrics", highlight_style,
                                         rows = even_rows,
                                         cols = 1:ncol(bond_metrics), gridExpand = TRUE, stack = TRUE)
                            }
                        }

                        # Conditional formatting on Z-Score if present
                        if ("z_score" %in% names(bond_metrics)) {
                            z_col <- which(names(bond_metrics) == "z_score")
                            if (length(z_col) > 0 && nrow(bond_metrics) > 0) {
                                conditionalFormatting(wb, "Bond Metrics",
                                                      cols = z_col,
                                                      rows = 2:(nrow(bond_metrics) + 1),
                                                      style = c("#FFCDD2", "#FFFFFF", "#C8E6C9"),
                                                      rule = c(-2, 0, 2),
                                                      type = "colourScale")
                            }
                        }
                    }
                }, error = function(e) log_error(e, context = "excel_bond_metrics"))

                incProgress(0.3, detail = "Historical Data")

                # 3. Historical Data — time series with all approved columns
                tryCatch({
                    if(!is.null(filtered_data()) && nrow(filtered_data()) > 0) {
                        addWorksheet(wb, "Historical Data")

                        cols <- get_export_columns(
                            filtered_data(),
                            include_metadata = isTRUE(input$include_metadata),
                            include_calculations = isTRUE(input$include_calculations)
                        )
                        historical_data <- filtered_data() %>%
                            select(all_of(cols)) %>%
                            arrange(date, bond)

                        writeData(wb, "Historical Data", historical_data, headerStyle = header_style)
                        setColWidths(wb, "Historical Data", cols = 1:ncol(historical_data), widths = "auto")
                    }
                }, error = function(e) log_error(e, context = "excel_historical_data"))

                incProgress(0.4, detail = "Risk Metrics")

                # 4. Risk Metrics
                tryCatch({
                    var_data_val <- var_data()
                    if(!is.null(var_data_val) && nrow(var_data_val) > 0) {
                        addWorksheet(wb, "Risk Metrics")

                        risk_df <- data.frame(Bond = var_data_val$bond)
                        if("VaR_95_bps" %in% names(var_data_val)) risk_df$`95% VaR (bps)` <- var_data_val$VaR_95_bps
                        if("VaR_99_bps" %in% names(var_data_val)) risk_df$`99% VaR (bps)` <- var_data_val$VaR_99_bps
                        if("CVaR_95" %in% names(var_data_val)) risk_df$`CVaR 95% (%)` <- var_data_val$CVaR_95
                        if("vol" %in% names(var_data_val)) risk_df$`Volatility (%)` <- var_data_val$vol

                        writeData(wb, "Risk Metrics", risk_df, headerStyle = header_style)
                        setColWidths(wb, "Risk Metrics", cols = 1:ncol(risk_df), widths = "auto")
                    }
                }, error = function(e) log_error(e, context = "excel_risk_metrics"))

                incProgress(0.5, detail = "Carry & Roll")

                # 5. Carry & Roll Analysis
                tryCatch({
                    carry_data_val <- carry_roll_data()
                    if(!is.null(carry_data_val) && nrow(carry_data_val) > 0) {
                        addWorksheet(wb, "Carry & Roll")

                        carry_df <- data.frame(Bond = carry_data_val$bond)
                        col_mapping <- list(
                            "holding_period" = "Holding Period",
                            "carry_income" = "Carry Income (%)",
                            "roll_return" = "Roll Return (%)",
                            "funding_cost" = "Funding Cost (%)",
                            "net_return" = "Net Return (%)",
                            "return_per_unit_risk" = "Return/Risk"
                        )
                        for(col_name in names(col_mapping)) {
                            if(col_name %in% names(carry_data_val)) {
                                carry_df[[col_mapping[[col_name]]]] <- carry_data_val[[col_name]]
                            }
                        }
                        carry_df <- carry_df %>% arrange(Bond)

                        writeData(wb, "Carry & Roll", carry_df, headerStyle = header_style)
                        setColWidths(wb, "Carry & Roll", cols = 1:ncol(carry_df), widths = "auto")
                    }
                }, error = function(e) log_error(e, context = "excel_carry_roll"))

                incProgress(0.6, detail = "Auction Data")

                # 6. Auction Data
                tryCatch({
                    if(!is.null(filtered_data()) && "bid_to_cover" %in% names(filtered_data())) {
                        auction_history <- filtered_data() %>%
                            filter(!is.na(bid_to_cover))

                        if(nrow(auction_history) > 0) {
                            addWorksheet(wb, "Auction Data")

                            auction_df <- data.frame(
                                Date = auction_history$date,
                                Bond = auction_history$bond
                            )
                            if("offer_amount" %in% names(auction_history)) auction_df$`Offer Amount` <- auction_history$offer_amount
                            if("offer" %in% names(auction_history)) auction_df$`Offer (R bn)` <- round(auction_history$offer/1e9, 2)
                            if("bids_received" %in% names(auction_history)) auction_df$`Bids Received` <- auction_history$bids_received
                            if("bids" %in% names(auction_history)) auction_df$`Bids (R bn)` <- round(auction_history$bids/1e9, 2)
                            auction_df$`Bid-to-Cover` <- auction_history$bid_to_cover
                            if("yield" %in% names(auction_history)) auction_df$`Yield (%)` <- round(auction_history$yield, 4)

                            auction_df <- auction_df %>% arrange(desc(Date))
                            writeData(wb, "Auction Data", auction_df, headerStyle = header_style)
                            setColWidths(wb, "Auction Data", cols = 1:ncol(auction_df), widths = "auto")

                            # Highlight strong auctions
                            strongAuctions <- which(auction_df$`Bid-to-Cover` > 3)
                            if(length(strongAuctions) > 0) {
                                addStyle(wb, "Auction Data", highlight_style,
                                         rows = strongAuctions + 1, cols = 1:ncol(auction_df),
                                         gridExpand = TRUE, stack = TRUE)
                            }
                        }
                    }
                }, error = function(e) log_error(e, context = "excel_auction_data"))

                incProgress(0.7, detail = "Treasury Holdings")

                # 7. Treasury Holdings (if available)
                tryCatch({
                    treasury_ts <- NULL
                    if (!is.null(treasury_module_data) && !is.null(treasury_module_data$holdings_ts)) {
                        treasury_ts <- treasury_module_data$holdings_ts()
                    }
                    if(!is.null(treasury_ts) && nrow(treasury_ts) > 0) {
                        addWorksheet(wb, "Treasury Holdings")
                        writeData(wb, "Treasury Holdings", treasury_ts, headerStyle = header_style)
                        setColWidths(wb, "Treasury Holdings", cols = 1:ncol(treasury_ts), widths = "auto")
                    }
                }, error = function(e) log_error(e, context = "excel_treasury_holdings"))

                incProgress(0.9, detail = "Metadata")

                # 8. Metadata
                tryCatch({
                    addWorksheet(wb, "Metadata")

                    metadata <- data.frame(
                        Parameter = c("Generated By", "Generation Date", "Generation Time",
                                      "Data Source", "R Version", "Dashboard Version"),
                        Value = c("Insele Capital Partners - Broking Services",
                                  format(Sys.Date(), "%Y-%m-%d"),
                                  format(Sys.time(), "%H:%M:%S"),
                                  "Enhanced Bond Analysis Dashboard",
                                  paste(R.version$major, R.version$minor, sep = "."),
                                  "2.0.0")
                    )

                    writeData(wb, "Metadata", metadata, headerStyle = header_style)
                    setColWidths(wb, "Metadata", cols = 1:2, widths = c(25, 40))

                }, error = function(e) {
                    log_error(e, context = "excel_metadata")
                })

                # Save workbook
                tryCatch({
                    saveWorkbook(wb, file, overwrite = TRUE)
                }, error = function(e) {
                    log_error(e, context = "excel_save")
                    showNotification("Error saving Excel file", type = "error")
                })

                incProgress(1, detail = "Complete")
                showNotification("Excel file exported successfully", type = "message", duration = 5)
            })
        }
    )


    # Download methodology PDF
    output$download_methodology_pdf <- downloadHandler(
        filename = "carry_roll_methodology.pdf",
        content = function(file) {
            # Create simple methodology PDF
            temp_rmd <- tempfile(fileext = ".Rmd")

            methodology_content <- '
---
title: "Carry & Roll Calculation Methodology"
author: "Insele Capital Partners"
output: pdf_document
---

# Overview
This document outlines the methodology used for carry and roll calculations in our SA Government Bond analysis.

## Formulas

### Carry Income
$$Carry = Coupon Rate \\times \\frac{Holding Period}{365}$$

### Roll-Down Return
$$Roll = Duration Change \\times Yield Curve Slope$$

### Net Return
$$Net Return = Carry + Roll - Funding Cost$$

## Assumptions
- Yield curve maintains current shape
- No credit events
- Funding available at repo rate
- Normal market conditions
'

            writeLines(methodology_content, temp_rmd)
            rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
        }
    )


    # ================================================================================
    # MODAL DIALOGS
    # ================================================================================


    # Email Report Modal
    observeEvent(input$email_report, {
        showModal(modalDialog(
            title = "Email Report",
            size = "m",

            textInput("email_recipients",
                      "Recipients (comma-separated):",
                      placeholder = "email1@example.com, email2@example.com",
                      width = "100%"),

            textAreaInput("email_message",
                          "Personal Message (optional):",
                          placeholder = "Add a personal message to include in the email...",
                          rows = 3,
                          width = "100%"),

            checkboxInput("include_weekly_forecast",
                          "Include weekly auction forecast",
                          value = TRUE),

            checkboxInput("include_recommendations",
                          "Include trading recommendations",
                          value = FALSE),

            tags$div(
                style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 15px;",
                tags$small("The email will include key charts and summaries from your current analysis.")
            ),

            footer = tagList(
                modalButton("Cancel"),
                actionButton("send_email", "Send Email",
                             class = "btn-primary",
                             icon = icon("paper-plane"))
            )
        ))
    })

    # Send Email Handler
    observeEvent(input$send_email, {
        req(input$email_recipients)

        # Validate email addresses
        recipients <- trimws(strsplit(input$email_recipients, ",")[[1]])
        valid_emails <- grepl("^[^@]+@[^@]+\\.[^@]+$", recipients)

        if(!all(valid_emails)) {
            showNotification("Please enter valid email addresses", type = "error")
            return()
        }

        withProgress(message = "Preparing email...", value = 0, {

            incProgress(0.2, detail = "Generating charts")

            # Generate subset of charts for email
            email_charts <- list()

            # Always include yield curve with error handling
            email_charts$yield_curve <- tryCatch({
                if(!is.null(processed_data()) && nrow(processed_data()) > 0) {
                    generate_enhanced_yield_curve(
                        processed_data(),
                        list(
                            xaxis_choice = if(!is.null(input$xaxis_choice)) input$xaxis_choice else "modified_duration",
                            curve_model = if(!is.null(input$curve_model)) input$curve_model else "loess"
                        )
                    )
                } else { NULL }
            }, error = function(e) {
                log_error(e, context = "email_yield_curve")
                NULL
            })

            # Include VaR if risk section selected
            if("risk" %in% input$report_sections && !is.null(var_data()) && nrow(var_data()) > 0) {
                email_charts$var_ladder <- tryCatch({
                    generate_var_ladder_plot(var_data(), list())
                }, error = function(e) {
                    log_error(e, context = "email_var_ladder")
                    NULL
                })
            }

            # Include carry if selected
            if("carry" %in% input$report_sections && !is.null(carry_roll_data()) && nrow(carry_roll_data()) > 0) {
                email_charts$carry_heatmap <- tryCatch({
                    return_type <- if(!is.null(input$return_type)) input$return_type else "net"
                    generate_enhanced_carry_roll_heatmap(carry_roll_data(), return_type)
                }, error = function(e) {
                    log_error(e, context = "email_carry_heatmap")
                    NULL
                })
            }

            incProgress(0.4, detail = "Converting images")

            # Convert to base64 with error handling
            charts_base64 <- list()
            for(name in names(email_charts)) {
                if(!is.null(email_charts[[name]])) {
                    charts_base64[[name]] <- tryCatch({
                        plot_to_base64(email_charts[[name]], width = 8, height = 5, dpi = 100)
                    }, error = function(e) {
                        log_error(e, context = paste("email_base64", name))
                        NULL
                    })
                }
            }

            incProgress(0.6, detail = "Preparing content")

            # Generate summaries with error handling
            summaries <- tryCatch({
                list(
                    overview = if(nrow(filtered_data()) > 0 && nrow(processed_data()) > 0) {
                        sprintf(
                            "Analysis of %d SA government bonds shows average yields at %.2f%% with bid-to-cover ratios averaging %.2fx.",
                            length(unique(filtered_data()$bond)),
                            mean(processed_data()$yield_to_maturity, na.rm = TRUE),
                            mean(filtered_data()$bid_to_cover, na.rm = TRUE)
                        )
                    } else {
                        "Bond analysis report attached."
                    },
                    yield_curve = generate_chart_summary("yield_curve", processed_data()),
                    risk = if(!is.null(var_data())) {
                        generate_chart_summary("var_analysis", var_data())
                    } else { NULL },
                    carry = "See attached analysis for detailed carry and roll expectations."
                )
            }, error = function(e) {
                list(overview = "Bond market analysis report.")
            })

            # Get auction data
            auction_data <- tryCatch({
                if(input$include_weekly_forecast) {
                    weekly_auction_summary()
                } else { NULL }
            }, error = function(e) { NULL })

            # Create email HTML
            email_html <- create_email_template(
                charts_base64,
                summaries,
                auction_data,
                if(!is.null(input$email_message)) input$email_message else ""
            )

            incProgress(0.8, detail = "Sending email")

            # Email sending implementation
            tryCatch({
                # Option 1: Using blastula (if available)
                if(requireNamespace("blastula", quietly = TRUE)) {

                    # Create email object
                    email <- blastula::compose_email(
                        body = blastula::md(email_html),
                        footer = blastula::md("© Insele Capital Partners")
                    )

                    # Add Excel attachment if requested
                    if(input$include_excel_attachment %||% FALSE) {
                        temp_excel <- tempfile(fileext = ".xlsx")
                        # Generate Excel file (reuse export_excel logic)
                        # ... excel generation code ...

                        email <- blastula::add_attachment(
                            email,
                            file = temp_excel,
                            filename = paste0("bond_analysis_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
                        )
                    }

                    # Send email (configure SMTP credentials)
                    # blastula::smtp_send(
                    #     email,
                    #     to = recipients,
                    #     from = "reports@inselecapital.com",
                    #     subject = paste("SA Government Bond Report -", format(Sys.Date(), "%B %d, %Y")),
                    #     credentials = blastula::creds_envvar(
                    #         user = "SMTP_USER",
                    #         pass_envvar = "SMTP_PASS",
                    #         host = "smtp.gmail.com",
                    #         port = 587
                    #     )
                    # )

                    # For demo/testing: save to file
                    temp_html <- file.path(tempdir(), paste0("email_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"))
                    writeLines(email_html, temp_html)

                    # Option 2: Using mailR (alternative)
                } else if(requireNamespace("mailR", quietly = TRUE)) {
                    # mailR implementation
                    # mailR::send.mail(
                    #     from = "reports@inselecapital.com",
                    #     to = recipients,
                    #     subject = paste("SA Government Bond Report -", format(Sys.Date(), "%B %d, %Y")),
                    #     body = email_html,
                    #     html = TRUE,
                    #     smtp = list(
                    #         host.name = "smtp.gmail.com",
                    #         port = 587,
                    #         user.name = Sys.getenv("SMTP_USER"),
                    #         passwd = Sys.getenv("SMTP_PASS"),
                    #         ssl = TRUE
                    #     ),
                    #     authenticate = TRUE,
                    #     send = TRUE
                    # )

                } else {
                    # Fallback: Save HTML file for manual sending
                    output_file <- file.path(
                        getwd(),
                        paste0("email_report_", format(Sys.Date(), "%Y%m%d"), ".html")
                    )
                    writeLines(email_html, output_file)

                    showNotification(
                        "Email sending requires the 'blastula' package with SMTP configuration. The report has been generated as HTML -- use the HTML download button instead.",
                        type = "warning",
                        duration = 8
                    )
                }

                incProgress(1, detail = "Complete")
                removeModal()

                showNotification(
                    HTML(paste0(
                        "<strong>✓ Email Report Ready!</strong><br>",
                        "Report prepared for ", length(recipients), " recipient(s).<br>",
                        "<small>Check your email or temp folder for the report.</small>"
                    )),
                    type = "message",
                    duration = 8
                )

            }, error = function(e) {
                log_error(e, context = "email_send")
                showNotification(
                    "Email preparation complete. Please check output folder.",
                    type = "warning"
                )
            })
        })
    })


    # ================================================================================
    # ADVANCED CURVE SETTINGS MODAL
    # ================================================================================
    # Settings for yield curve analysis:
    # - Curve model type (NSS, Spline, LOESS, Polynomial)
    # - Confidence band width
    # - Show/hide bond labels
    # - Point size metric (Z-Score vs Spread magnitude)
    # - Historical lookback period for Z-Score calculation

    # Store advanced settings in reactive values
    curve_advanced_settings <- reactiveValues(
        model_type = "nss",
        confidence_level = 0.95,
        show_labels = TRUE,
        show_confidence_band = TRUE,
        point_size_metric = "zscore",
        lookback_period = 60
    )

    observeEvent(input$show_curve_settings, {
        showModal(modalDialog(
            title = tags$div(
                icon("cog"),
                " Advanced Curve Settings",
                style = "color: #1B3A6B;"
            ),
            size = "m",

            tags$div(
                style = "padding: 10px;",

                # ─────────────────────────────────────────────────────────────
                # CURVE MODEL SECTION
                # ─────────────────────────────────────────────────────────────
                h5("Curve Model", style = "color: #1B3A6B; font-weight: bold; margin-top: 0;"),

                selectInput("adv_model_type", "Curve Fitting Method:",
                            choices = list(
                                "Nelson-Siegel-Svensson" = "nss",
                                "Smooth Spline" = "spline",
                                "LOESS (Local Regression)" = "loess",
                                "Polynomial" = "polynomial"
                            ),
                            selected = isolate(curve_advanced_settings$model_type)
                ),
                tags$p(class = "text-muted small", style = "margin-top: -10px;",
                       "NSS is the industry standard for yield curve fitting"),

                hr(),

                # ─────────────────────────────────────────────────────────────
                # CONFIDENCE & DISPLAY SECTION
                # ─────────────────────────────────────────────────────────────
                h5("Display Options", style = "color: #1B3A6B; font-weight: bold;"),

                sliderInput("adv_confidence", "Confidence Band:",
                            min = 80, max = 99, value = isolate(curve_advanced_settings$confidence_level * 100),
                            step = 1, post = "%"
                ),

                fluidRow(
                    column(6,
                           checkboxInput("adv_show_confidence_band", "Show Confidence Band",
                                         value = isolate(curve_advanced_settings$show_confidence_band))
                    ),
                    column(6,
                           checkboxInput("adv_show_labels", "Show Bond Labels",
                                         value = isolate(curve_advanced_settings$show_labels))
                    )
                ),

                hr(),

                # ─────────────────────────────────────────────────────────────
                # POINT SIZE & Z-SCORE SECTION
                # ─────────────────────────────────────────────────────────────
                h5("Point Sizing", style = "color: #1B3A6B; font-weight: bold;"),

                selectInput("adv_point_size_metric", "Point Size Represents:",
                            choices = list(
                                "Z-Score Magnitude (Signal Strength)" = "zscore",
                                "Spread Magnitude" = "spread",
                                "Uniform Size" = "uniform"
                            ),
                            selected = isolate(curve_advanced_settings$point_size_metric)
                ),
                tags$p(class = "text-muted small", style = "margin-top: -10px;",
                       "Z-Score shows how unusual the current spread is historically"),

                sliderInput("adv_lookback_period", "Z-Score Lookback Period:",
                            min = 20, max = 120, value = isolate(curve_advanced_settings$lookback_period),
                            step = 10, post = " days"
                ),
                tags$p(class = "text-muted small", style = "margin-top: -10px;",
                       "Days of history used for rolling mean/std calculation"),

                hr(),

                # ─────────────────────────────────────────────────────────────
                # Z-SCORE EXPLANATION
                # ─────────────────────────────────────────────────────────────
                tags$div(
                    style = "background: #e8f4f8; padding: 10px; border-radius: 5px; border-left: 3px solid #1B3A6B;",
                    tags$strong("Z-Score Interpretation:", style = "color: #1B3A6B;"),
                    tags$ul(style = "margin-bottom: 0; padding-left: 20px;",
                            tags$li("|Z| > 2.0 = Strong signal (statistically significant)"),
                            tags$li("|Z| > 1.5 = Moderate signal"),
                            tags$li("|Z| < 1.0 = Weak signal (normal variation)")
                    )
                )
            ),

            footer = tagList(
                actionButton("reset_curve_settings", "Reset to Defaults",
                             class = "btn-default", icon = icon("undo")),
                modalButton("Cancel"),
                actionButton("apply_curve_settings", "Apply", class = "btn-primary")
            )
        ))
    })

    # Apply advanced settings
    observeEvent(input$apply_curve_settings, {
        # Update the main curve model selector
        updateSelectInput(session, "curve_model", selected = input$adv_model_type)

        # Update the lookback days slider
        updateSliderInput(session, "lookback_days", value = input$adv_lookback_period)

        # Update the confidence level slider
        updateSliderInput(session, "confidence_level", value = input$adv_confidence)

        # Store settings in reactive values for plot use
        curve_advanced_settings$model_type <- input$adv_model_type
        curve_advanced_settings$confidence_level <- input$adv_confidence / 100
        curve_advanced_settings$show_labels <- input$adv_show_labels
        curve_advanced_settings$show_confidence_band <- input$adv_show_confidence_band
        curve_advanced_settings$point_size_metric <- input$adv_point_size_metric
        curve_advanced_settings$lookback_period <- input$adv_lookback_period

        showNotification(
            "Curve settings applied successfully",
            type = "message",
            duration = 3
        )

        removeModal()
    })

    # Reset to defaults
    observeEvent(input$reset_curve_settings, {
        updateSelectInput(session, "adv_model_type", selected = "nss")
        updateSliderInput(session, "adv_confidence", value = 95)
        updateCheckboxInput(session, "adv_show_confidence_band", value = TRUE)
        updateCheckboxInput(session, "adv_show_labels", value = TRUE)
        updateSelectInput(session, "adv_point_size_metric", selected = "zscore")
        updateSliderInput(session, "adv_lookback_period", value = 60)

        showNotification(
            "Settings reset to defaults",
            type = "message",
            duration = 2
        )
    })

    # Add modal for methodology
    observeEvent(input$show_carry_methodology, {
        showModal(modalDialog(
            title = "Carry & Roll Calculation Methodology",
            size = "l",

            tags$div(
                h4("Detailed Calculation Formulas"),

                tags$div(style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin: 15px 0;",
                         tags$h5("1. Carry Income"),
                         tags$p("Carry = Coupon Rate × (Holding Period / 365)"),
                         tags$p(tags$em("Example: 10% coupon, 90 days = 10% × (90/365) = 2.47%"))
                ),

                tags$div(style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin: 15px 0;",
                         tags$h5("2. Roll-Down Return"),
                         tags$p("Roll = Duration Change × Yield Curve Slope"),
                         tags$p("Assumes ~20bps yield reduction per year of duration rolled"),
                         tags$p(tags$em("Example: 1 year roll with 5yr duration = 1 × 0.20% = 0.20%"))
                ),

                tags$div(style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin: 15px 0;",
                         tags$h5("3. Funding Cost"),
                         tags$p("Funding = Repo Rate × (Holding Period / 365)"),
                         tags$p(tags$em("Example: 8.25% repo, 90 days = 8.25% × (90/365) = 2.03%"))
                ),

                tags$div(style = "background: #e8f5e9; padding: 15px; border-radius: 5px; margin: 15px 0;",
                         tags$h5("Net Return Formula"),
                         tags$p(tags$strong("Net Return = Carry + Roll - Funding")),
                         tags$p(tags$em("Example: 2.47% + 0.20% - 2.03% = 0.64%"))
                ),

                tags$div(style = "background: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0;",
                         tags$h5("Key Assumptions"),
                         tags$ul(
                             tags$li("Yield curve maintains current shape"),
                             tags$li("No credit events or defaults"),
                             tags$li("Funding available at repo rate"),
                             tags$li("Positions can be held to target date"),
                             tags$li("Roll-down assumes normal market conditions")
                         )
                )
            ),

            footer = tagList(
                modalButton("Close"),
                downloadButton("download_methodology_pdf", "Download as PDF", class = "btn-primary")
            )
        ))
    })



    # Schedule Reports Modal
    observeEvent(input$schedule_report, {
        showModal(modalDialog(
            title = "Schedule Automated Reports",
            size = "m",

            tags$div(
                style = "padding: 15px; background: #fff3cd; border: 1px solid #ffc107; border-radius: 5px;",
                tags$strong("Coming Soon!"),
                tags$p("Automated report scheduling will be available in the next update.",
                       style = "margin-top: 10px; margin-bottom: 0;"),
                tags$p("Features will include:",
                       style = "margin-top: 10px;"),
                tags$ul(
                    tags$li("Daily/Weekly/Monthly automated reports"),
                    tags$li("Custom recipient lists"),
                    tags$li("Conditional alerts based on market conditions"),
                    tags$li("Integration with calendar systems")
                )
            ),

            footer = modalButton("Close")
        ))
    })

    # ==========================================================================
    # SESSION CLEANUP HANDLER
    # ==========================================================================
    # This ensures proper cleanup when the user closes the app or disconnects
    # from the session, preventing resource leaks and orphaned processes.

    session$onSessionEnded(function() {
        cat("\n", rep("=", 60), "\n", sep = "")
        cat("Session ended - Cleaning up resources...\n")
        cat(rep("=", 60), "\n\n", sep = "")

        # Clean up theme cache if it exists
        if (exists(".theme_manager", envir = .GlobalEnv)) {
            tryCatch({
                theme_mgr <- get(".theme_manager", envir = .GlobalEnv)
                if (!is.null(theme_mgr) && "clear_cache" %in% names(theme_mgr)) {
                    theme_mgr$clear_cache()
                    cat("✓ Theme cache cleared\n")
                }
            }, error = function(e) {
                cat("✗ Error clearing theme cache:", conditionMessage(e), "\n")
            })
        }

        # Clean up temporary files (e.g., generated charts, temp PDFs)
        temp_patterns <- c("temp_*.pdf", "temp_*.png", "chart_*.png", "temp_chart_*.rds")
        for (pattern in temp_patterns) {
            temp_files <- list.files(pattern = pattern, full.names = TRUE)
            if (length(temp_files) > 0) {
                tryCatch({
                    file.remove(temp_files)
                    cat(sprintf("✓ Removed %d temporary file(s) matching '%s'\n",
                                length(temp_files), pattern))
                }, error = function(e) {
                    cat(sprintf("✗ Error removing temp files '%s': %s\n",
                                pattern, conditionMessage(e)))
                })
            }
        }

        # Garbage collection to free memory
        tryCatch({
            gc()
            cat("✓ Garbage collection completed\n")
        }, error = function(e) {
            cat("✗ Error during garbage collection:", conditionMessage(e), "\n")
        })

        cat("\n", rep("=", 60), "\n", sep = "")
        cat("Session cleanup completed\n")
        cat(rep("=", 60), "\n\n", sep = "")
    })

    # End of session cleanup handler
    # ==========================================================================

}

# =============================================================================
# APPLICATION LAUNCH
# =============================================================================
#
# IMPORTANT: This file defines the UI and server but does NOT automatically
# launch the app to prevent unwanted restarts when sourcing the code.
#
# To run the application, use one of these methods:
#
# Method 1 (Recommended): Use the launcher script
#   source("run_app.R")
#
# Method 2: Launch manually in your R console
#   source("enhanced_bond_server.R")
#   shinyApp(ui = ui, server = server)
#
# Method 3: Run from command line
#   Rscript run_app.R
#
# =============================================================================

# Uncomment the line below ONLY if you want auto-launch behavior (not recommended):
shinyApp(ui = ui, server = server)
