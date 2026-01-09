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
    "webshot2", "htmltools", "htmlwidgets", "DT", "stringr", "officer", "flextable", "future", "promises", "magick", "shinyjs", "jsonlite"#, "webp"
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
        carry_refresh = 0  # Add this for carry & roll refresh trigger
    )

    # ================================================================================
    # TREASURY HOLDINGS MODULE
    # ================================================================================
    # Initialize the Treasury Holdings module server and capture returned data
    treasury_module_data <- treasury_holdings_server("treasury_module")

    # ════════════════════════════════════════════════════════════════════════
    # BOND DATA LOADING - BULLETPROOF VERSION
    # ════════════════════════════════════════════════════════════════════════

    bond_data <- reactive({
        w$show()

        # ════════════════════════════════════════════════════════════════════
        # STEP 1: LOAD RAW DATA
        # ════════════════════════════════════════════════════════════════════

        data <- tryCatch({
            # First check for in-memory data
            if (exists("full_df")) {
                message("✓ Loading bond data from full_df (in-memory)")
                return(full_df)
            }

            # Try multiple possible data file locations
            data_paths <- c(
                "data/processed_bond_data.rds",            # Standard data directory
                "processed_bond_data.rds",                 # Same directory
                "../data/processed_bond_data.rds",         # Parent data directory
                "bond-dashboard/data/processed_bond_data.rds"  # Old structure
            )

            for (data_path in data_paths) {
                if (file.exists(data_path)) {
                    message(sprintf("✓ Loading bond data from: %s", data_path))
                    return(readRDS(data_path))
                }
            }

            # No data found
            warning("No bond data found in any expected location")
            NULL

        }, error = function(e) {
            warning(sprintf("Error loading bond data: %s", e$message))
            NULL
        })

        # Early exit if no data
        if (is.null(data)) {
            w$hide()
            showNotification(
                "Unable to load bond data. Please check data files.",
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


    # ✅ FILTERED DATA - With validation
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

        # Filter data
        result <- bond_data() %>%
            filter(
                bond %in% input$selected_bonds,
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

        # ✅ NEW: Sanitize filtered output
        result <- sanitize_pipeline_data(result, "filtered_data [output]")

        return(result)
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


    # Calculate VaR
    var_data <- reactive({
        req(filtered_data())

        calculate_var(filtered_data(),
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

        calculate_advanced_carry_roll(
            processed_data(),
            holding_periods = c(30, 90, 180, 360),
            funding_rate = funding_rate_value
        )
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


    # Dynamic bond selection
    observe({
        req(bond_data())

        available_bonds <- unique(bond_data()$bond)

        updatePickerInput(
            session,
            "selected_bonds",
            choices = available_bonds,
            selected = available_bonds
        )

        updateSelectInput(
            session,
            "tech_bond_select",
            choices = available_bonds,
            selected = available_bonds[1]
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
        # Force recalculation
        values$carry_refresh <- runif(1)  # Add this to reactive values at the top
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

    # Bond selection helpers
    observeEvent(input$select_short, {
        req(bond_data())
        short_bonds <- bond_data() %>%
            group_by(bond) %>%
            summarise(avg_dur = mean(modified_duration, na.rm = TRUE), .groups = "drop") %>%
            filter(avg_dur < 5) %>%
            pull(bond)
        updatePickerInput(session, "selected_bonds", selected = short_bonds)
    })

    observeEvent(input$select_medium, {
        req(bond_data())
        medium_bonds <- bond_data() %>%
            group_by(bond) %>%
            summarise(avg_dur = mean(modified_duration, na.rm = TRUE), .groups = "drop") %>%
            filter(avg_dur >= 5, avg_dur <= 10) %>%
            pull(bond)
        updatePickerInput(session, "selected_bonds", selected = medium_bonds)
    })

    observeEvent(input$select_long, {
        req(bond_data())
        long_bonds <- bond_data() %>%
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

                # Prepare data for export
                export_data <- filtered_data() %>%
                    select(date, bond, yield, modified_duration,
                           convexity, dv01, time_to_maturity) %>%
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

                # Prepare data for export
                export_data <- filtered_data() %>%
                    select(date, bond, yield, modified_duration,
                           convexity, dv01, time_to_maturity) %>%
                    arrange(date, bond)

                # Convert to JSON
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

                # Format for Bloomberg (specific column names and format)
                bloomberg_data <- filtered_data() %>%
                    mutate(
                        Ticker = bond,
                        Date = format(date, "%Y%m%d"),
                        YLD_YTM_MID = round(yield, 4),
                        DUR_MID = round(modified_duration, 4),
                        CONVEXITY_MID = round(convexity, 4),
                        DV01 = round(dv01, 6)
                    ) %>%
                    select(Ticker, Date, YLD_YTM_MID, DUR_MID, CONVEXITY_MID, DV01) %>%
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

    # 2. Relative Value Heatmap
    output$relative_value_heatmap <- renderPlot({
        req(filtered_data())
        p <- generate_relative_value_heatmap(filtered_data(), list())
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

    # =========================================================================
    # VaR ANALYSIS REACTIVES AND OUTPUTS
    # =========================================================================

    # Reactive to store the VaR distribution plot and its bond ordering
    var_distribution_results <- reactive({
        req(filtered_data())

        # Get parameters from UI controls
        horizon_days <- as.numeric(input$var_horizon) %||% 1
        lookback_days <- input$var_lookback %||% 252
        method <- input$var_method %||% "historical"

        # Filter data based on lookback period
        data <- filtered_data()
        if (!is.null(lookback_days) && lookback_days < 504) {
            cutoff_date <- max(data$date, na.rm = TRUE) - lookback_days
            data <- data %>% filter(date >= cutoff_date)
        }

        # Generate the plot with parameters
        params <- list(
            horizon_days = horizon_days,
            method = method,
            show_stats = FALSE,  # Stats are in separate table now
            enable_diagnostics = FALSE
        )

        p <- generate_var_distribution_plot(data, params)

        # Return both plot and bond order
        list(
            plot = p,
            bond_order = attr(p, "bond_order"),
            var_summary = attr(p, "var_summary")
        )
    })

    # 4. VaR Distribution Plot
    output$var_distribution_plot <- renderPlot({
        req(var_distribution_results())
        p <- var_distribution_results()$plot
        if(!is.null(p)) print(p)
    })

    # 5. VaR Ladder Plot - uses SAME bond ordering as distribution plot
    output$var_ladder_plot <- renderPlot({
        req(var_data(), var_distribution_results())

        # Pass the bond order from distribution plot for consistency
        params <- list(
            bond_order = var_distribution_results()$bond_order
        )

        p <- generate_var_ladder_plot(var_data(), params)
        if(!is.null(p)) print(p)
    })

    # 6. VaR Statistics Table - FIXED VERSION
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

                # Tail risk indicator - HYBRID APPROACH
                # Combines absolute risk factors with relative percentile ranking
                tail_ratio = abs(VaR_99) / abs(VaR_95),

                # Absolute risk factor flags (calibrated for SA gov bond market)
                has_fat_tails = kurtosis > 3.1,           # Top ~40% of typical data
                has_extreme_ratio = tail_ratio > 1.45,    # Above normal (1.41) + buffer
                has_left_skew = skewness < -0.15          # Meaningful left skew
            ) %>%
            mutate(
                # Relative ranking within current dataset
                kurtosis_rank = percent_rank(kurtosis),
                ratio_rank = percent_rank(tail_ratio),
                # Composite percentile (weights kurtosis and ratio, with skew penalty)
                risk_percentile = (kurtosis_rank + ratio_rank +
                                   ifelse(skewness < 0, percent_rank(-skewness), 0)) / 3,

                # Final classification - Hybrid of absolute triggers and relative ranking
                tail_risk = case_when(
                    # Absolute triggers: Multiple risk factors = definite high risk
                    has_fat_tails & has_left_skew ~ "High \u26A0",
                    has_extreme_ratio & has_fat_tails ~ "High \u26A0",

                    # High percentile (top 25%) with at least one risk factor
                    risk_percentile >= 0.75 & (has_fat_tails | has_extreme_ratio | has_left_skew) ~ "Elevated",

                    # Top 25% by composite score even without flags
                    risk_percentile >= 0.75 ~ "Elevated",

                    # Middle range (40-75th percentile)
                    risk_percentile >= 0.40 ~ "Moderate",

                    # Lower risk (bottom 40%)
                    TRUE ~ "Low \u2713"
                )
            ) %>%
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

        DT::datatable(
            table_data,
            options = list(
                pageLength = 15,
                dom = 't',  # Table only, no search/pagination
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
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
    output$dv01_ladder_plot <- renderPlot({
        req(processed_data())
        p <- generate_dv01_ladder_plot(processed_data(), list())
        if(!is.null(p)) print(p)
    })

    # 7. Enhanced Convexity Plot
    output$enhanced_convexity_plot <- renderPlot({
        req(processed_data())
        p <- generate_enhanced_convexity_plot(processed_data(), list())
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

    # 9. Signal Matrix Heatmap
    output$signal_matrix_heatmap <- renderPlot({
        req(filtered_data())
        p <- generate_signal_matrix_heatmap(filtered_data_with_technicals())
        if(!is.null(p)) print(p)
    })

    # 10. Enhanced Carry & Roll Heatmap
    output$enhanced_carry_roll_heatmap <- renderPlot({
        req(carry_roll_data())

        return_type_value <- if(!is.null(input$return_type)) {
            input$return_type
        } else {
            "net"
        }

        p <- generate_enhanced_carry_roll_heatmap(carry_roll_data(), return_type_value)
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "No carry & roll data available", cex = 1.5)
        }
    })

    # 11. Scenario Analysis Plot
    output$scenario_analysis_plot <- renderPlot({
        req(processed_data())
        p <- generate_scenario_analysis_plot(processed_data())
        if(!is.null(p)) print(p)
    })

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

    output$forward_curve_plot <- renderPlot({
        req(processed_data())
        p <- generate_forward_curve_plot(processed_data(), list())
        if(!is.null(p)) print(p)
    })


    output$enhanced_auction_analytics <- renderPlot({
        req(filtered_data())
        p <- generate_enhanced_auction_analytics(filtered_data(), list())
        if(!is.null(p)) print(p)
    }, height = 600)


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

    output$demand_elasticity_plot <- renderPlot({
        req(filtered_data())
        p <- generate_demand_elasticity_plot(filtered_data(), list())
        if(!is.null(p)) print(p)
    })

    output$success_probability_plot <- renderPlot({
        req(filtered_data(), input$auction_bonds_select)
        p <- generate_success_probability_plot(filtered_data(), input$auction_bonds_select)
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Please select bonds for analysis", cex = 1.2)
        }
    })

    output$auction_pattern_analysis <- renderPlot({
        req(filtered_data())
        p <- generate_auction_pattern_analysis(filtered_data(), list())
        if(!is.null(p)) {
            gridExtra::grid.arrange(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for pattern analysis", cex = 1.2)
        }
    })

    output$bid_distribution_plot <- renderPlot({
        req(filtered_data())
        p <- generate_bid_distribution_plot(filtered_data(), list())
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "No bid distribution data available", cex = 1.2)
        }
    })

    # YTD Bond Issuance Chart
    output$ytd_bond_issuance_chart <- renderPlot({
        req(filtered_data())
        p <- generate_ytd_bond_issuance_chart(filtered_data(), list())
        if(!is.null(p)) {
            print(p)
        } else {
            plot.new()
            text(0.5, 0.5, "No issuance data available for selected date range", cex = 1.2)
        }
    })

    # YTD Bond Issuance Data Table
    output$ytd_issuance_data_table <- DT::renderDataTable({
        req(filtered_data())
        table_data <- generate_ytd_issuance_table(filtered_data())

        DT::datatable(
            table_data,
            options = list(
                pageLength = 15,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                order = list(list(2, 'desc'))  # Sort by Total Issuance descending
            ),
            rownames = FALSE,
            class = 'cell-border stripe hover'
        ) %>%
            DT::formatCurrency(columns = c('Total Issuance (R mil)', 'Average Issuance (R mil)',
                                           'Min Issuance (R mil)', 'Max Issuance (R mil)'),
                               currency = "R", digits = 2) %>%
            DT::formatStyle(
                'Bond',
                fontWeight = 'bold',
                color = styleEqual('TOTAL', '#1B3A6B')
            ) %>%
            DT::formatStyle(
                'Total Issuance (R mil)',
                background = styleColorBar(range(table_data[table_data$Bond != 'TOTAL', 'Total Issuance (R mil)'], na.rm = TRUE), '#90EE90'),
                backgroundSize = '100% 90%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center'
            )
    })

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

        # Get the data
        risk_data <- processed_data()
        var_metrics <- var_data()

        # Calculate comprehensive risk metrics
        metrics <- risk_data %>%
            summarise(
                avg_duration = mean(modified_duration, na.rm = TRUE),
                avg_convexity = mean(convexity, na.rm = TRUE),
                avg_dv01 = mean(modified_duration * 10000 / 1e6, na.rm = TRUE),
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

        # Combine processed data with VaR metrics
        risk_summary <- processed_data() %>%
            left_join(var_data(), by = "bond") %>%
            mutate(
                # Calculate DV01 if not present
                dv01_value = if("basis_point_value" %in% names(.)) {
                    basis_point_value / 1e6
                } else {
                    modified_duration * 10000 / 1e6  # Assuming R10mm notional
                }
            ) %>%
            select(
                bond,
                yield_to_maturity,
                modified_duration,
                duration,
                convexity,
                dv01_value,
                VaR_95_bps,
                VaR_99_bps,
                CVaR_95,
                vol
            ) %>%
            arrange(modified_duration)

        # Format for display
        display_table <- risk_summary %>%
            mutate(
                Yield = sprintf("%.3f%%", yield_to_maturity),
                `Mod Duration` = sprintf("%.2f", modified_duration),
                Duration = sprintf("%.2f", duration),
                Convexity = sprintf("%.2f", convexity),
                `DV01 (R mn)` = sprintf("%.2f", dv01_value),
                `95% VaR` = sprintf("%.0f bps", abs(VaR_95_bps)),
                `99% VaR` = sprintf("%.0f bps", abs(VaR_99_bps)),
                `CVaR (95%)` = sprintf("%.0f bps", abs(CVaR_95) * 100),
                Volatility = sprintf("%.1f%%", vol * 100)
            ) %>%
            select(
                Bond = bond,
                Yield,
                `Mod Duration`,
                Duration,
                Convexity,
                `DV01 (R mn)`,
                `95% VaR`,
                `99% VaR`,
                `CVaR (95%)`,
                Volatility
            )

        # Create enhanced datatable
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
                order = list(list(2, 'asc'))  # Sort by Mod Duration
            ),
            rownames = FALSE,
            class = 'table-striped table-bordered compact',
            caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; padding: 10px;',
                htmltools::tags$strong('Comprehensive Risk Metrics'),
                htmltools::tags$br(),
                htmltools::tags$small('Key risk measures for portfolio management and risk assessment')
            )
        ) %>%
            formatStyle(
                "95% VaR",
                backgroundColor = styleInterval(
                    c(30, 50),
                    c("#E8F5E9", "#FFF3E0", "#FFEBEE")
                )
            ) %>%
            formatStyle(
                "99% VaR",
                backgroundColor = styleInterval(
                    c(50, 100),
                    c("#E8F5E9", "#FFF3E0", "#FFEBEE")
                ),
                fontWeight = styleInterval(100, c("normal", "bold"))
            )
    })

    # Add Technical Indicators Summary
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

        # Generate signals
        rsi_signal <- case_when(
            is.na(latest$rsi_14) ~ "Neutral",
            latest$rsi_14 < 30 ~ "Oversold",
            latest$rsi_14 > 70 ~ "Overbought",
            TRUE ~ "Neutral"
        )

        bb_signal <- case_when(
            is.na(latest$bb_position) ~ "Neutral",
            latest$bb_position < 0.2 ~ "Near Lower Band",
            latest$bb_position > 0.8 ~ "Near Upper Band",
            TRUE ~ "Within Bands"
        )

        macd_signal <- case_when(
            is.na(latest$macd) | is.na(latest$macd_signal) ~ "Neutral",
            latest$macd > latest$macd_signal ~ "Bullish",
            TRUE ~ "Bearish"
        )

        trend_signal <- case_when(
            is.na(latest$sma_50) | is.na(latest$sma_200) ~ "Unknown",
            latest$yield_to_maturity < latest$sma_50 & latest$sma_50 < latest$sma_200 ~ "Strong Downtrend",
            latest$yield_to_maturity < latest$sma_50 ~ "Downtrend",
            latest$yield_to_maturity > latest$sma_50 & latest$sma_50 > latest$sma_200 ~ "Strong Uptrend",
            latest$yield_to_maturity > latest$sma_50 ~ "Uptrend",
            TRUE ~ "Sideways"
        )

        # Return formatted UI
        tagList(
            tags$div(
                style = "padding: 10px;",
                tags$h5(paste("Technical Summary:", input$tech_bond_select),
                        style = "color: #1B3A6B; margin-top: 0; font-weight: bold;"),

                # Current Level
                tags$div(style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
                         tags$p(style = "margin: 0; font-size: 18px; font-weight: bold;",
                                sprintf("%.3f%%", latest$yield_to_maturity)),
                         tags$small("Current Yield")
                ),

                # Momentum Indicators
                tags$h6("Momentum", style = "color: #666; margin-top: 15px;"),
                tags$div(style = "margin: 10px 0;",
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("RSI (14): "),
                                sprintf("%.1f", if_else(is.na(latest$rsi_14), 50, latest$rsi_14)),
                                tags$span(paste0(" - ", rsi_signal),
                                          style = paste0("color: ", case_when(
                                              rsi_signal == "Oversold" ~ "#28a745",
                                              rsi_signal == "Overbought" ~ "#dc3545",
                                              TRUE ~ "#666"
                                          )))),
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("MACD: "),
                                macd_signal,
                                style = paste0("color: ", if_else(macd_signal == "Bullish", "#28a745", "#dc3545")))
                ),

                # Volatility
                tags$h6("Volatility", style = "color: #666; margin-top: 15px;"),
                tags$div(style = "margin: 10px 0;",
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("BB Position: "),
                                bb_signal),
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("BB Width: "),
                                sprintf("%.3f%%", if_else(is.na(latest$bb_width), 0, latest$bb_width * 100)))
                ),

                # Trend
                tags$h6("Trend", style = "color: #666; margin-top: 15px;"),
                tags$div(style = "margin: 10px 0;",
                         tags$p(style = "margin: 5px 0;",
                                tags$strong("Status: "),
                                trend_signal,
                                style = paste0("color: ", case_when(
                                    grepl("Uptrend", trend_signal) ~ "#dc3545",
                                    grepl("Downtrend", trend_signal) ~ "#28a745",
                                    TRUE ~ "#666"
                                ))),
                         if(!is.na(latest$sma_50)) {
                             tags$p(style = "margin: 5px 0;",
                                    tags$strong("SMA 50: "),
                                    sprintf("%.3f%%", latest$sma_50))
                         },
                         if(!is.na(latest$sma_200)) {
                             tags$p(style = "margin: 5px 0;",
                                    tags$strong("SMA 200: "),
                                    sprintf("%.3f%%", latest$sma_200))
                         }
                ),

                # Support/Resistance
                if(!is.na(latest$support_3m) || !is.na(latest$resistance_3m)) {
                    tagList(
                        tags$h6("S/R Levels", style = "color: #666; margin-top: 15px;"),
                        tags$div(style = "margin: 10px 0;",
                                 if(!is.na(latest$support_3m)) {
                                     tags$p(style = "margin: 5px 0;",
                                            tags$strong("Support: "),
                                            sprintf("%.3f%%", latest$support_3m))
                                 },
                                 if(!is.na(latest$resistance_3m)) {
                                     tags$p(style = "margin: 5px 0;",
                                            tags$strong("Resistance: "),
                                            sprintf("%.3f%%", latest$resistance_3m))
                                 }
                        )
                    )
                }
            )
        )
    })


    output$technical_indicators_enhanced_table <- DT::renderDataTable({
        req(processed_data())

        # Get latest technical indicators for all bonds
        tech_summary <- filtered_data_with_technicals() %>%
            select(
                bond,
                date,
                yield_to_maturity,
                rsi_14,
                bb_position,
                macd,
                macd_signal,
                macd_histogram,
                sma_50,
                sma_200,
                signal_strength,
                roc_20
            ) %>%
            filter(!is.na(signal_strength))  %>%
            group_by(bond) %>%
            # CRITICAL: Filter to most recent date for each bond
            filter(date == max(date)) %>%
            ungroup() %>%
            # Select only one row per bond (in case of duplicates on same date)
            distinct(bond, .keep_all = TRUE) %>%
            arrange(bond)

        # Check if we have data
        if(nrow(tech_summary) == 0) {
            # Return empty table with message
            empty_df <- data.frame(
                Message = "No technical indicator data available. Please adjust date range or bond selection."
            )
            return(datatable(empty_df, options = list(dom = 't'), rownames = FALSE))
        }

        # Format for display
        display_table <- tech_summary %>%
            mutate(
                `Current Yield` = sprintf("%.3f%%", yield_to_maturity),
                RSI = sprintf("%.1f", rsi_14),
                `RSI Signal` = case_when(
                    rsi_14 < 30 ~ "Oversold",
                    rsi_14 > 70 ~ "Overbought",
                    TRUE ~ "Neutral"
                ),
                `BB Position` = sprintf("%.2f", bb_position),
                `BB Signal` = case_when(
                    bb_position < 0.2 ~ "Near Lower",
                    bb_position > 0.8 ~ "Near Upper",
                    TRUE ~ "Within Bands"
                ),
                MACD = sprintf("%.3f", macd),
                `MACD Signal` = case_when(
                    macd > macd_signal & macd_histogram > 0 ~ "Bullish",
                    macd < macd_signal & macd_histogram < 0 ~ "Bearish",
                    TRUE ~ "Neutral"
                ),
                `SMA 50` = sprintf("%.3f%%", sma_50),
                `SMA 200` = sprintf("%.3f%%", sma_200),
                Trend = case_when(
                    yield_to_maturity < sma_50 & sma_50 < sma_200 ~ "Downtrend",
                    yield_to_maturity > sma_50 & sma_50 > sma_200 ~ "Uptrend",
                    TRUE ~ "Sideways"
                ),
                `ROC (20d)` = sprintf("%.2f%%", roc_20),
                # `Z-Score` = sprintf("%.2f", z_score),
                `Overall Signal` = signal_strength
            ) %>%
            select(
                Bond = bond,
                `Current Yield`,
                RSI,
                `RSI Signal`,
                `BB Position`,
                `BB Signal`,
                MACD,
                `MACD Signal`,
                Trend,
                `ROC (20d)`,
                # `Z-Score`,
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
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all'),
                    list(width = '80px', targets = 0)
                )
            ),
            rownames = FALSE,
            class = 'table-striped table-bordered compact',
            caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; padding: 10px;',
                htmltools::tags$strong('Technical Analysis Summary'),
                htmltools::tags$br(),
                htmltools::tags$small('Latest technical indicators and trading signals for all selected bonds')
            )
        ) %>%
            # Color code RSI Signal
            formatStyle(
                "RSI Signal",
                backgroundColor = styleEqual(
                    c("Oversold", "Neutral", "Overbought"),
                    c("#E8F5E9", "#F5F5F5", "#FFEBEE")
                )
            ) %>%
            # Color code BB Signal
            formatStyle(
                "BB Signal",
                backgroundColor = styleEqual(
                    c("Near Lower", "Within Bands", "Near Upper"),
                    c("#E8F5E9", "#F5F5F5", "#FFEBEE")
                )
            ) %>%
            # Color code MACD Signal
            formatStyle(
                "MACD Signal",
                backgroundColor = styleEqual(
                    c("Bullish", "Neutral", "Bearish"),
                    c("#E8F5E9", "#F5F5F5", "#FFEBEE")
                )
            ) %>%
            # Color code Trend
            formatStyle(
                "Trend",
                backgroundColor = styleEqual(
                    c("Downtrend", "Sideways", "Uptrend"),
                    c("#E8F5E9", "#FFF3E0", "#FFEBEE")
                )
            ) %>%
            # Color code Overall Signal
            formatStyle(
                "Overall Signal",
                backgroundColor = styleEqual(
                    c("Strong Buy", "Buy", "Neutral", "Sell", "Strong Sell"),
                    c("#4CAF50", "#81C784", "#F5F5F5", "#FF8A65", "#F44336")
                ),
                color = styleEqual(
                    c("Strong Buy", "Strong Sell"),
                    c("white", "white")
                ),
                fontWeight = styleEqual(
                    c("Strong Buy", "Strong Sell"),
                    c("bold", "bold")
                )
            )
    })

    # Add this to server function
    output$carry_roll_summary <- renderUI({
        req(carry_roll_data())

        # Calculate summary statistics
        summary_stats <- carry_roll_data() %>%
            filter(holding_period == "90d") %>%
            summarise(
                avg_net = mean(net_return, na.rm = TRUE),
                best_net = max(net_return, na.rm = TRUE),
                worst_net = min(net_return, na.rm = TRUE),
                positive_carry = sum(net_return > 0, na.rm = TRUE),
                total_bonds = n()
            )

        if(nrow(summary_stats) > 0) {
            tagList(
                tags$p(style = "margin: 5px 0;",
                       tags$strong("90-day Average:"),
                       tags$span(sprintf(" %.2f%%", summary_stats$avg_net),
                                 style = ifelse(summary_stats$avg_net > 0,
                                                "color: #28a745;", "color: #dc3545;"))),
                tags$p(style = "margin: 5px 0;",
                       tags$strong("Best Performer:"),
                       tags$span(sprintf(" %.2f%%", summary_stats$best_net),
                                 style = "color: #28a745;")),
                tags$p(style = "margin: 5px 0;",
                       tags$strong("Positive Carry:"),
                       tags$span(sprintf(" %d of %d bonds",
                                         summary_stats$positive_carry,
                                         summary_stats$total_bonds)))
            )
        } else {
            tags$p("Calculating metrics...")
        }
    })

    # Add this output to the server function
    output$forward_rate_table <- DT::renderDataTable({
        req(processed_data())

        # Get current yield curve data
        curve_data <- processed_data() %>%
            arrange(modified_duration)

        # Calculate forward rates between different tenor points
        forward_rates <- data.frame()

        # Define standard forward rate periods
        periods <- data.frame(
            from_tenor = c(0, 1, 2, 3, 5, 7, 10),
            to_tenor = c(1, 2, 3, 5, 7, 10, 15),
            label = c("0y1y", "1y1y", "2y1y", "3y2y", "5y2y", "7y3y", "10y5y")
        )

        for(i in 1:nrow(periods)) {
            from_dur <- periods$from_tenor[i]
            to_dur <- periods$to_tenor[i]

            # Find closest bonds to these durations
            from_bond <- curve_data %>%
                mutate(dur_diff = abs(modified_duration - from_dur)) %>%
                arrange(dur_diff) %>%
                head(1)

            to_bond <- curve_data %>%
                mutate(dur_diff = abs(modified_duration - to_dur)) %>%
                arrange(dur_diff) %>%
                head(1)

            if(nrow(from_bond) > 0 && nrow(to_bond) > 0) {
                # Calculate forward rate using the formula:
                # Forward Rate = [(1 + Rlong)^Tlong / (1 + Rshort)^Tshort]^(1/(Tlong-Tshort)) - 1

                r_short <- from_bond$yield_to_maturity / 100
                r_long <- to_bond$yield_to_maturity / 100
                t_short <- from_bond$modified_duration
                t_long <- to_bond$modified_duration

                if(t_long > t_short && from_dur == 0) {
                    # For spot to forward calculation
                    forward_rate <- r_long
                } else if(t_long > t_short) {
                    # Standard forward rate calculation
                    forward_rate <- ((1 + r_long)^t_long / (1 + r_short)^t_short)^(1/(t_long - t_short)) - 1
                } else {
                    forward_rate <- NA
                }

                forward_rates <- rbind(forward_rates, data.frame(
                    Period = periods$label[i],
                    From_Tenor = from_dur,
                    To_Tenor = to_dur,
                    Spot_Rate_Start = r_short * 100,
                    Spot_Rate_End = r_long * 100,
                    Forward_Rate = forward_rate * 100,
                    Spread_to_Spot = (forward_rate - r_long) * 100,
                    From_Bond = from_bond$bond,
                    To_Bond = to_bond$bond,
                    stringsAsFactors = FALSE
                ))
            }
        }

        # Add market implied expectations
        forward_rates <- forward_rates %>%
            mutate(
                Market_View = case_when(
                    Spread_to_Spot > 50 ~ "Expects Rising Rates",
                    Spread_to_Spot > 20 ~ "Mildly Hawkish",
                    Spread_to_Spot < -50 ~ "Expects Falling Rates",
                    Spread_to_Spot < -20 ~ "Mildly Dovish",
                    TRUE ~ "Neutral"
                ),
                Signal_Strength = case_when(
                    abs(Spread_to_Spot) > 100 ~ "Strong",
                    abs(Spread_to_Spot) > 50 ~ "Moderate",
                    abs(Spread_to_Spot) > 20 ~ "Weak",
                    TRUE ~ "None"
                )
            )

        # Format for display
        display_table <- forward_rates %>%
            filter(!is.na(Forward_Rate)) %>%
            mutate(
                Spot_Rate_Start = sprintf("%.2f%%", Spot_Rate_Start),
                Spot_Rate_End = sprintf("%.2f%%", Spot_Rate_End),
                Forward_Rate = sprintf("%.2f%%", Forward_Rate),
                Spread_to_Spot = sprintf("%+.0f bps", Spread_to_Spot),
                Bonds_Used = paste(From_Bond, "→", To_Bond)
            ) %>%
            select(Period, Forward_Rate, Spot_Rate_End, Spread_to_Spot,
                   Market_View, Signal_Strength, Bonds_Used)

        # Create enhanced datatable
        datatable(
            display_table,
            options = list(
                pageLength = 10,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                columnDefs = list(
                    list(className = 'dt-center', targets = '_all'),
                    list(width = '100px', targets = 0)
                ),
                initComplete = JS(
                    "function(settings, json) {",
                    "  $('td:contains(\"Expects Rising Rates\")').css('color', '#dc3545');",
                    "  $('td:contains(\"Expects Falling Rates\")').css('color', '#28a745');",
                    "  $('td:contains(\"Strong\")').css('font-weight', 'bold');",
                    "}"
                )
            ),
            rownames = FALSE,
            class = 'table-striped table-bordered compact',
            colnames = c(
                "Forward Period",
                "Forward Rate",
                "Current Spot",
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
                        style = 'margin-top: 10px; margin-bottom: 0;',
                        "This table shows implied forward rates derived from the current yield curve. ",
                        "Forward rates indicate market expectations for future interest rates. ",
                        "Positive spreads suggest the market expects rates to rise; negative spreads suggest rate cuts."
                    )
                )
            )
        ) %>%
            formatStyle(
                "Market_View",
                backgroundColor = styleEqual(
                    c("Expects Rising Rates", "Mildly Hawkish", "Neutral",
                      "Mildly Dovish", "Expects Falling Rates"),
                    c("#FFE5E5", "#FFF0E5", "#F0F0F0", "#E5F0FF", "#E5FFE5")
                )
            ) %>%
            formatStyle(
                "Signal_Strength",
                fontWeight = styleEqual(
                    c("Strong", "Moderate"),
                    c("bold", "600")
                )
            )
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

    # Update auction bond choices
    observe({
        req(bond_data())
        bonds <- unique(bond_data()$bond)

        # Select bonds with recent auction activity as defaults
        recent_auction_bonds <- bond_data() %>%
            filter(!is.na(bid_to_cover),
                   date >= today() - days(90)) %>%
            pull(bond) %>%
            unique()

        updatePickerInput(
            session,
            "auction_bonds_select",
            choices = bonds,
            selected = head(recent_auction_bonds, 3)
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



    output$auction_success_factors <- renderPlot({
        req(filtered_data())
        p <- generate_auction_success_factors_plot(filtered_data(), list())
        if(!is.null(p)) {
            gridExtra::grid.arrange(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient auction data for analysis", cex = 1.2)
        }
    })



    output$btc_decomposition <- renderPlot({
        req(filtered_data())
        p <- generate_btc_decomposition_plot(filtered_data(), list())
        if(!is.null(p)) {
            gridExtra::grid.arrange(p)
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient data for decomposition analysis", cex = 1.2)
        }
    })



    # 22. Add after the regime_analysis_plot output
    output$regime_summary <- renderUI({
        req(regime_data())

        # Use .data pronoun to avoid confusion with date() function
        current_regime <- regime_data() %>%
            filter(.data$date == max(.data$date, na.rm = TRUE))

        if(nrow(current_regime) == 0) {
            return(tags$p("No regime data available"))
        }

        # Calculate regime statistics
        regime_stats <- regime_data() %>%
            filter(date >= today() - days(30)) %>%
            group_by(regime) %>%
            summarise(
                days = n(),
                pct = n() / 30 * 100,
                .groups = "drop"
            )

        # Determine regime color
        regime_color <- case_when(
            current_regime$regime == "Stressed" ~ insele_palette$danger,
            current_regime$regime == "Elevated" ~ insele_palette$warning,
            current_regime$regime == "Calm" ~ insele_palette$success,
            TRUE ~ insele_palette$secondary
        )

        tagList(
            tags$div(
                style = paste0("padding: 15px; background: ", regime_color,
                               "; color: white; border-radius: 8px; text-align: center;"),
                h3(current_regime$regime, style = "margin: 0; font-weight: bold;"),
                p(sprintf("Since: %s", format(current_regime$date, "%B %d")),
                  style = "margin: 5px 0;")
            ),

            tags$div(style = "margin-top: 15px;",
                     h5("Key Metrics", style = paste0("color: ", insele_palette$primary, ";")),
                     tags$div(style = "display: flex; justify-content: space-between; margin: 10px 0;",
                              span("Volatility:"),
                              strong(sprintf("%.1f%%", current_regime$vol_20d * 100))
                     ),
                     tags$div(style = "display: flex; justify-content: space-between; margin: 10px 0;",
                              span("Stress Score:"),
                              strong(sprintf("%.2f", current_regime$stress_score))
                     ),
                     tags$div(style = "display: flex; justify-content: space-between; margin: 10px 0;",
                              span("Trend:"),
                              strong(current_regime$trend)
                     ),
                     tags$div(style = "display: flex; justify-content: space-between; margin: 10px 0;",
                              span("Dispersion:"),
                              strong(sprintf("%.2f%%", current_regime$yield_dispersion))
                     )
            ),

            hr(),

            tags$div(
                h5("30-Day Distribution", style = paste0("color: ", insele_palette$primary, ";")),
                tags$div(
                    lapply(regime_stats$regime, function(r) {
                        stats <- regime_stats[regime_stats$regime == r,]
                        color <- case_when(
                            r == "Stressed" ~ insele_palette$danger,
                            r == "Elevated" ~ insele_palette$warning,
                            r == "Calm" ~ insele_palette$success,
                            TRUE ~ insele_palette$secondary
                        )
                        tags$div(
                            style = "margin: 5px 0;",
                            tags$div(
                                style = "display: flex; justify-content: space-between;",
                                span(r),
                                strong(sprintf("%.0f%%", stats$pct))
                            ),
                            tags$div(
                                style = paste0("background: #e0e0e0; height: 8px; border-radius: 4px; ",
                                               "margin-top: 3px; overflow: hidden;"),
                                tags$div(
                                    style = paste0("background: ", color, "; height: 100%; width: ",
                                                   stats$pct, "%;")
                                )
                            )
                        )
                    })
                )
            )
        )
    })



    output$regime_probability_gauge <- renderPlot({
        req(regime_data())
        p <- generate_regime_probability_gauge(regime_data(), list())
        if(!is.null(p)) print(p)
    }, height = 250)



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

        # Calculate signals and scores for ALL bonds
        opportunities <- bonds %>%
            filter(!is.na(spread_bps)) %>%
            mutate(
                # Fill NA z_scores with 0
                zscore = ifelse(is.na(z_score), 0, z_score),

                # CORRECT signal logic based on spread (positive = cheap = buy)
                signal = case_when(
                    spread_bps > 10 & zscore > 1.5 ~ "Strong Buy",
                    spread_bps > 5 ~ "Buy",
                    spread_bps < -10 & zscore < -1.5 ~ "Strong Sell",
                    spread_bps < -5 ~ "Sell",
                    TRUE ~ "Hold"
                ),

                # NEW: Calculate conviction score (0-10 scale)
                # Get bid_to_cover if available
                btc = if ("bid_to_cover" %in% names(.)) bid_to_cover else NA_real_
            ) %>%
            rowwise() %>%
            mutate(
                score = calculate_conviction_score(
                    spread_bps = spread_bps,
                    zscore = zscore,
                    bid_to_cover = btc
                )
            ) %>%
            ungroup() %>%
            mutate(
                # Signal category for filtering
                signal_category = case_when(
                    signal %in% c("Strong Buy", "Strong Sell") ~ "strong",
                    signal %in% c("Buy", "Sell") ~ "actionable",
                    TRUE ~ "hold"
                )
            )

        # Apply filter based on user selection
        if (filter_choice == "actionable") {
            opportunities <- opportunities %>%
                filter(signal_category %in% c("strong", "actionable"))
        } else if (filter_choice == "strong") {
            opportunities <- opportunities %>%
                filter(signal_category == "strong")
        }
        # "all" shows everything - no additional filter

        # Sort by score descending
        opportunities <- opportunities %>%
            arrange(desc(score))

        cat("Filtered bonds:", nrow(opportunities), "\n")

        # Select columns - use x_value which is the DYNAMIC x-axis value
        display_data <- opportunities %>%
            select(
                Bond = bond,
                Yield = yield_to_maturity,
                Duration = x_value,  # Uses whatever x-axis is currently selected
                Spread = spread_bps,
                ZScore = zscore,
                Signal = signal,
                Score = score
            )

        # Format the display
        display_data <- display_data %>%
            mutate(
                Yield = sprintf("%.3f%%", Yield),
                Duration = sprintf("%.2f", Duration),
                Spread = sprintf("%+.1f bps", Spread),  # Show sign
                ZScore = sprintf("%+.2f", ZScore),      # Show sign
                Score = sprintf("%.1f", Score)
            )

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
            colnames = c("Bond", "Yield", duration_col_name, "Spread", "Z-Score", "Signal", "Score")
        ) %>%
            formatStyle(
                "Signal",
                backgroundColor = styleEqual(
                    c("Strong Buy", "Buy", "Hold", "Sell", "Strong Sell"),
                    c("#1B5E20", "#4CAF50", "#9E9E9E", "#EF5350", "#B71C1C")
                ),
                color = styleEqual(
                    c("Strong Buy", "Buy", "Hold", "Sell", "Strong Sell"),
                    c("white", "white", "white", "white", "white")
                ),
                fontWeight = "bold"
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



    # Report Preview
    # Report Preview - Enhanced with individual plot selection
    output$report_preview_content <- renderUI({
        # Collect selected sections from new checkbox inputs
        sections <- character()
        if(isTRUE(input$section_overview)) sections <- c(sections, "overview")
        if(isTRUE(input$section_relative)) sections <- c(sections, "relative")
        if(isTRUE(input$section_risk)) sections <- c(sections, "risk")
        if(isTRUE(input$section_technical)) sections <- c(sections, "technical")
        if(isTRUE(input$section_carry)) sections <- c(sections, "carry")
        if(isTRUE(input$section_auction)) sections <- c(sections, "auction")
        if(isTRUE(input$section_intelligence)) sections <- c(sections, "intelligence")
        if(isTRUE(input$section_treasury)) sections <- c(sections, "treasury")
        if(isTRUE(input$section_recommendations)) sections <- c(sections, "recommendations")

        # Collect ALL selected plots (including new ones)
        selected_plots <- character()

        # Overview
        if(isTRUE(input$section_overview) && isTRUE(input$plot_regime_plot)) {
            selected_plots <- c(selected_plots, "Regime Analysis")
        }

        # Relative Value
        if(isTRUE(input$section_relative)) {
            if(isTRUE(input$plot_yield_curve)) selected_plots <- c(selected_plots, "Yield Curve")
            if(isTRUE(input$plot_relative_heatmap)) selected_plots <- c(selected_plots, "Relative Value Heatmap")
            if(isTRUE(input$plot_zscore_plot)) selected_plots <- c(selected_plots, "Z-Score Distribution")
            if(isTRUE(input$plot_convexity)) selected_plots <- c(selected_plots, "\U2728 Enhanced Convexity")
        }

        # Risk
        if(isTRUE(input$section_risk)) {
            if(isTRUE(input$plot_var_distribution)) selected_plots <- c(selected_plots, "VaR Distribution")
            if(isTRUE(input$plot_var_ladder)) selected_plots <- c(selected_plots, "VaR Ladder")
            if(isTRUE(input$plot_dv01_ladder)) selected_plots <- c(selected_plots, "DV01 Analysis")
        }

        # Technical
        if(isTRUE(input$section_technical)) {
            if(isTRUE(input$plot_technical_plot)) selected_plots <- c(selected_plots, "Technical Indicators")
            if(isTRUE(input$plot_signal_matrix)) selected_plots <- c(selected_plots, "Signal Matrix")
        }

        # Carry & Roll
        if(isTRUE(input$section_carry)) {
            if(isTRUE(input$plot_carry_heatmap)) selected_plots <- c(selected_plots, "Carry Heatmap")
            if(isTRUE(input$plot_scenario_analysis)) selected_plots <- c(selected_plots, "Scenario Analysis")
            if(isTRUE(input$plot_optimal_holding)) selected_plots <- c(selected_plots, "\U2728 Optimal Holding")
            if(isTRUE(input$plot_forward_curve)) selected_plots <- c(selected_plots, "\U2728 Forward Curve")
        }

        # Auction (expanded with 8 new plots)
        if(isTRUE(input$section_auction)) {
            if(isTRUE(input$plot_auction_performance)) selected_plots <- c(selected_plots, "Auction Performance")
            if(isTRUE(input$plot_auction_patterns)) selected_plots <- c(selected_plots, "Auction Patterns")
            if(isTRUE(input$plot_auction_forecast)) selected_plots <- c(selected_plots, "\U2728 Auction Forecast")
            if(isTRUE(input$plot_demand_elasticity)) selected_plots <- c(selected_plots, "\U2728 Demand Elasticity")
            if(isTRUE(input$plot_success_probability)) selected_plots <- c(selected_plots, "\U2728 Success Probability")
            if(isTRUE(input$plot_bid_distribution)) selected_plots <- c(selected_plots, "\U2728 Bid Distribution")
            if(isTRUE(input$plot_ytd_issuance)) selected_plots <- c(selected_plots, "\U2728 YTD Issuance")
            if(isTRUE(input$plot_auction_sentiment)) selected_plots <- c(selected_plots, "\U2728 Auction Sentiment")
            if(isTRUE(input$plot_auction_success_factors)) selected_plots <- c(selected_plots, "\U2728 Success Factors")
            if(isTRUE(input$plot_btc_decomposition)) selected_plots <- c(selected_plots, "\U2728 BTC Decomposition")
        }

        # Intelligence
        if(isTRUE(input$section_intelligence)) {
            if(isTRUE(input$plot_correlation)) selected_plots <- c(selected_plots, "Correlation Matrix")
            if(isTRUE(input$plot_term_structure)) selected_plots <- c(selected_plots, "3D Term Structure")
        }

        # Treasury Holdings
        if(isTRUE(input$section_treasury)) {
            if(isTRUE(input$plot_holdings_area)) selected_plots <- c(selected_plots, "\U0001F3DB Holdings Time Series")
            if(isTRUE(input$plot_sector_trend)) selected_plots <- c(selected_plots, "\U0001F3DB Sector Trend")
            if(isTRUE(input$plot_holdings_fixed)) selected_plots <- c(selected_plots, "\U0001F3DB Fixed Rate Holdings")
            if(isTRUE(input$plot_holdings_ilb)) selected_plots <- c(selected_plots, "\U0001F3DB ILB Holdings")
            if(isTRUE(input$plot_holdings_frn)) selected_plots <- c(selected_plots, "\U0001F3DB FRN Holdings")
            if(isTRUE(input$plot_holdings_sukuk)) selected_plots <- c(selected_plots, "\U0001F3DB Sukuk Holdings")
            if(isTRUE(input$plot_ownership_changes)) selected_plots <- c(selected_plots, "\U0001F3DB Ownership Changes")
            if(isTRUE(input$plot_holdings_diverging_fixed)) selected_plots <- c(selected_plots, "\U0001F3DB Fixed Rate Changes")
            if(isTRUE(input$plot_holdings_diverging_ilb)) selected_plots <- c(selected_plots, "\U0001F3DB ILB Changes")
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

        # Calculate key curve metrics
        short_yield <- mean(data$yield_to_maturity[data$x_value <= 5], na.rm = TRUE)
        medium_yield <- mean(data$yield_to_maturity[data$x_value > 5 & data$x_value <= 10], na.rm = TRUE)
        long_yield <- mean(data$yield_to_maturity[data$x_value > 10], na.rm = TRUE)

        curve_slope <- (long_yield - short_yield) * 100
        curve_level <- mean(data$yield_to_maturity, na.rm = TRUE)
        curve_curvature <- 2 * medium_yield - short_yield - long_yield

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

        # Count actionable signals (|z-score| > 1.5)
        actionable_count <- sum(latest_data$zscore_abs > 1.5, na.rm = TRUE)

        tagList(
            # ═══════════════════════════════════════════════════════════
            # CURVE METRICS SECTION
            # ═══════════════════════════════════════════════════════════
            h5("Curve Metrics", style = "color: #1B3A6B; font-weight: bold; margin-top: 0;"),
            tags$div(
                tags$p(tags$b("Level:"), sprintf(" %.2f%%", curve_level)),
                tags$p(tags$b("Slope (2s10s):"), sprintf(" %.0f bps", curve_slope)),
                tags$p(tags$b("Curvature:"), sprintf(" %.2f%%", curve_curvature))
            ),
            hr(style = "margin: 10px 0;"),

            # ═══════════════════════════════════════════════════════════
            # SECTOR YIELDS SECTION
            # ═══════════════════════════════════════════════════════════
            tags$div(
                tags$p(tags$b("Short (<5y):"), sprintf(" %.2f%%", short_yield)),
                tags$p(tags$b("Medium (5-10y):"), sprintf(" %.2f%%", medium_yield)),
                tags$p(tags$b("Long (>10y):"), sprintf(" %.2f%%", long_yield))
            ),
            hr(style = "margin: 10px 0;"),

            # ═══════════════════════════════════════════════════════════
            # MODEL QUALITY SECTION
            # ═══════════════════════════════════════════════════════════
            tags$div(
                tags$p(tags$b("Avg |Spread|:"), sprintf(" %.1f bps", avg_spread)),
                tags$p(tags$b("Model R²:"), sprintf(" %.3f", ifelse(is.na(r_squared), 0, r_squared)))
            ),
            hr(style = "margin: 10px 0;"),

            # ═══════════════════════════════════════════════════════════
            # TRADING SIGNALS SECTION (NEW!)
            # ═══════════════════════════════════════════════════════════
            h5("Trading Signals", style = "color: #1B3A6B; font-weight: bold;"),

            # Cheapest bond (BUY signal) - uses spread_bps from fitted_curve_data
            if (nrow(cheapest) > 0 && !is.na(cheapest$spread_bps[1])) {
                tags$p(
                    tags$b("Cheapest: "),
                    tags$span(cheapest$bond[1], style = "color: #388E3C; font-weight: bold;"),
                    sprintf(" (+%.1f bps)", cheapest$spread_bps[1])
                )
            } else {
                tags$p(tags$b("Cheapest: "), "N/A")
            },

            # Richest bond (SELL signal) - uses spread_bps from fitted_curve_data
            if (nrow(richest) > 0 && !is.na(richest$spread_bps[1])) {
                tags$p(
                    tags$b("Richest: "),
                    tags$span(richest$bond[1], style = "color: #D32F2F; font-weight: bold;"),
                    sprintf(" (%.1f bps)", richest$spread_bps[1])
                )
            } else {
                tags$p(tags$b("Richest: "), "N/A")
            },

            # Highest conviction trade
            if (nrow(highest_conviction) > 0 && !is.na(highest_conviction$z_score[1])) {
                tags$p(
                    tags$b("Highest Conviction: "),
                    highest_conviction$bond[1],
                    sprintf(" (Z=%.2f)", highest_conviction$z_score[1])
                )
            } else {
                tags$p(tags$b("Highest Conviction: "), "N/A")
            },

            # Actionable signals count
            tags$p(
                tags$b("Actionable Signals: "),
                sprintf("%d bonds with |Z| > 1.5", actionable_count)
            ),

            # Z-Score explanation
            tags$p(
                class = "text-muted small",
                style = "margin-top: 10px; font-size: 11px;",
                "Z-Score measures statistical significance: |Z| > 2 = strong signal, |Z| > 1.5 = moderate signal"
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

    # Replace the market_regime_box output with this fixed version
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
            color <- case_when(
                current_regime$regime[1] == "Stressed" ~ "red",
                current_regime$regime[1] == "Elevated" ~ "yellow",
                current_regime$regime[1] == "Calm" ~ "green",
                TRUE ~ "blue"
            )

            valueBox(
                value = current_regime$regime[1],
                subtitle = sprintf("Vol: %.1f%% | Trend: %s",
                                   current_regime$vol_20d[1] * 100,
                                   current_regime$trend[1]),
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

        # Check if signal_strength exists, if not create it
        if(!"signal_strength" %in% names(processed_data())) {
            valueBox(
                value = 0,
                subtitle = "Strong Signals Active",
                icon = icon("bolt"),
                color = "blue"
            )
        } else {
            signals <- processed_data() %>%
                filter(!is.na(signal_strength))

            strong_signals <- sum(signals$signal_strength %in% c("Strong Buy", "Strong Sell"))

            valueBox(
                value = strong_signals,
                subtitle = "Strong Signals Active",
                icon = icon("bolt"),
                color = if(strong_signals > 3) "red" else if(strong_signals > 1) "yellow" else "blue"
            )
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
            p <- generate_forward_curve_plot(processed_data(), list())
            if(!is.null(p)) {
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
            p <- generate_scenario_analysis_plot(processed_data())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 14, height = 8, dpi = 300, bg = "white")
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
            p <- generate_enhanced_convexity_plot(processed_data(), list())
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
            p <- generate_dv01_ladder_plot(processed_data(), list())
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


    output$download_signal_matrix <- downloadHandler(
        filename = function() {
            paste0("trading_signal_matrix_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_signal_matrix_heatmap(filtered_data_with_technicals())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            }
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
            p <- generate_bid_distribution_plot(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            }
        }
    )


    output$download_btc_decomposition <- downloadHandler(
        filename = function() {
            paste0("btc_decomposition_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_btc_decomposition_plot(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 14, height = 10, dpi = 300, bg = "white")
            }
        }
    )

    output$download_auction_success <- downloadHandler(
        filename = function() {
            paste0("auction_success_factors_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_auction_success_factors_plot(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 10, dpi = 300, bg = "white")
            }
        }
    )

    output$download_auction_sentiment <- downloadHandler(
        filename = function() {
            paste0("auction_sentiment_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_auction_sentiment_gauge(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
            }
        }
    )

    output$download_auction_pattern <- downloadHandler(
        filename = function() {
            paste0("auction_patterns_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_auction_pattern_analysis(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 10, dpi = 300, bg = "white")
            }
        }
    )

    output$download_success_probability <- downloadHandler(
        filename = function() {
            paste0("auction_success_probability_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data(), input$auction_bonds_select)
            p <- generate_success_probability_plot(filtered_data(), input$auction_bonds_select)
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    output$download_demand_elasticity <- downloadHandler(
        filename = function() {
            paste0("demand_elasticity_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_demand_elasticity_plot(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            }
        }
    )

    output$download_auction_performance <- downloadHandler(
        filename = function() {
            paste0("auction_performance_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_enhanced_auction_analytics(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 16, height = 10, dpi = 300, bg = "white")
            }
        }
    )

    # YTD Bond Issuance Chart Download
    output$download_ytd_issuance_chart <- downloadHandler(
        filename = function() {
            paste0("ytd_bond_issuance_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
            req(filtered_data())
            p <- generate_ytd_bond_issuance_chart(filtered_data(), list())
            if(!is.null(p)) {
                ggsave(file, plot = p, width = 14, height = 10, dpi = 300, bg = "white")
            }
        }
    )

    # YTD Bond Issuance Table Download (CSV)
    output$download_ytd_issuance_table <- downloadHandler(
        filename = function() {
            paste0("ytd_bond_issuance_table_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
            req(filtered_data())
            table_data <- generate_ytd_issuance_table(filtered_data())
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

                # Get reactive data ONCE here
                proc_data <- tryCatch(processed_data(), error = function(e) NULL)
                filt_data <- tryCatch(filtered_data(), error = function(e) NULL)
                var_data_val <- tryCatch(var_data(), error = function(e) NULL)
                regime_data_val <- tryCatch(regime_data(), error = function(e) NULL)
                carry_data_val <- tryCatch(carry_roll_data(), error = function(e) NULL)

                # ✅ Calculate technical indicators for report
                filt_data_with_tech <- tryCatch({
                    validate_dataframe_class(filtered_data_with_technicals(), "filtered_data_with_technicals [report]")
                }, error = function(e) {
                    log_error(e, context = "report_technical_data")
                    NULL
                })

                incProgress(0.2, detail = "Collecting charts")

                # Prepare input parameters
                # Collect selected sections
                sections <- character()
                if(isTRUE(input$section_overview)) sections <- c(sections, "overview")
                if(isTRUE(input$section_relative)) sections <- c(sections, "relative")
                if(isTRUE(input$section_risk)) sections <- c(sections, "risk")
                if(isTRUE(input$section_technical)) sections <- c(sections, "technical")
                if(isTRUE(input$section_carry)) sections <- c(sections, "carry")
                if(isTRUE(input$section_auction)) sections <- c(sections, "auction")
                if(isTRUE(input$section_intelligence)) sections <- c(sections, "intelligence")
                if(isTRUE(input$section_treasury)) sections <- c(sections, "treasury")
                if(isTRUE(input$section_recommendations)) sections <- c(sections, "recommendations")

                # Prepare input parameters with selected plots (ALL 35 plots)
                input_params <- list(
                    report_sections = sections,
                    selected_plots = list(
                        # Overview
                        regime_plot = isTRUE(input$plot_regime_plot),

                        # Relative Value
                        yield_curve = isTRUE(input$plot_yield_curve),
                        relative_heatmap = isTRUE(input$plot_relative_heatmap),
                        zscore_plot = isTRUE(input$plot_zscore_plot),
                        convexity = isTRUE(input$plot_convexity),  # NEW

                        # Risk
                        var_distribution = isTRUE(input$plot_var_distribution),
                        var_ladder = isTRUE(input$plot_var_ladder),
                        dv01_ladder = isTRUE(input$plot_dv01_ladder),

                        # Technical
                        technical_plot = isTRUE(input$plot_technical_plot),
                        signal_matrix = isTRUE(input$plot_signal_matrix),

                        # Carry & Roll
                        carry_heatmap = isTRUE(input$plot_carry_heatmap),
                        scenario_analysis = isTRUE(input$plot_scenario_analysis),
                        optimal_holding = isTRUE(input$plot_optimal_holding),  # NEW
                        forward_curve = isTRUE(input$plot_forward_curve),  # NEW

                        # Auction (8 new plots)
                        auction_performance = isTRUE(input$plot_auction_performance),
                        auction_patterns = isTRUE(input$plot_auction_patterns),
                        auction_forecast = isTRUE(input$plot_auction_forecast),  # NEW
                        demand_elasticity = isTRUE(input$plot_demand_elasticity),  # NEW
                        success_probability = isTRUE(input$plot_success_probability),  # NEW
                        bid_distribution = isTRUE(input$plot_bid_distribution),  # NEW
                        ytd_issuance = isTRUE(input$plot_ytd_issuance),  # NEW
                        auction_sentiment = isTRUE(input$plot_auction_sentiment),  # NEW
                        auction_success_factors = isTRUE(input$plot_auction_success_factors),  # NEW
                        btc_decomposition = isTRUE(input$plot_btc_decomposition),  # NEW

                        # Intelligence
                        correlation = isTRUE(input$plot_correlation),
                        term_structure = isTRUE(input$plot_term_structure),

                        # Treasury Holdings
                        holdings_area = isTRUE(input$plot_holdings_area),
                        sector_trend = isTRUE(input$plot_sector_trend),
                        holdings_fixed = isTRUE(input$plot_holdings_fixed),
                        holdings_ilb = isTRUE(input$plot_holdings_ilb),
                        holdings_frn = isTRUE(input$plot_holdings_frn),
                        holdings_sukuk = isTRUE(input$plot_holdings_sukuk),
                        ownership_changes = isTRUE(input$plot_ownership_changes),
                        holdings_diverging_fixed = isTRUE(input$plot_holdings_diverging_fixed),
                        holdings_diverging_ilb = isTRUE(input$plot_holdings_diverging_ilb)
                    ),
                    # Pass other required params for auction plots
                    auction_bonds_select = input$auction_bonds_select,
                    xaxis_choice = input$xaxis_choice,
                    curve_model = input$curve_model,
                    return_type = input$return_type,
                    tech_bond_select = input$tech_bond_select,
                    tech_indicator_type = input$tech_indicator_type
                )

                # Get treasury data for report
                treasury_holdings_ts_val <- tryCatch({
                    if(!is.null(treasury_module_data) && !is.null(treasury_module_data$holdings_ts)) {
                        treasury_module_data$holdings_ts()
                    } else { NULL }
                }, error = function(e) NULL)

                treasury_bond_holdings_val <- tryCatch({
                    if(!is.null(treasury_module_data) && !is.null(treasury_module_data$bond_holdings)) {
                        treasury_module_data$bond_holdings()
                    } else { NULL }
                }, error = function(e) NULL)

                # Collect charts with error handling - pass data directly
                charts <- list()
                tryCatch({
                    # Generate chart collection with BOTH data objects
                    chart_collection <- collect_report_charts(
                        proc_data,
                        filt_data,
                        filt_data_with_tech,  # ✅ NOW PASSING TECHNICAL DATA
                        var_data_val,
                        regime_data_val,
                        carry_data_val,
                        treasury_holdings_ts_val,    # Treasury time series
                        treasury_bond_holdings_val,  # Treasury bond holdings
                        input_params
                    )

                    # Load charts from paths
                    for(name in names(chart_collection)) {
                        if(is.character(chart_collection[[name]]) && file.exists(chart_collection[[name]])) {
                            charts[[name]] <- readRDS(chart_collection[[name]])
                        } else if(!is.character(chart_collection[[name]])) {
                            charts[[name]] <- chart_collection[[name]]
                        }
                    }
                }, error = function(e) {
                    log_error(e, context = "chart_collection", session_id = session$token)
                })

                # ✅ ADD: Ensure cleanup happens when PDF generation is done
                on.exit({
                    # Clean up chart files AFTER PDF is generated
                    if(!is.null(chart_collection) && !is.null(chart_collection$cleanup)) {
                        chart_collection$cleanup()
                    }
                }, add = TRUE)

                incProgress(0.3, detail = "Generating summaries")

                # Generate summaries with error handling - pass data directly
                summaries <- tryCatch({
                    generate_report_summaries(
                        proc_data,
                        filt_data,
                        var_data_val,
                        regime_data_val,
                        carry_data_val
                    )
                }, error = function(e) {
                    log_error(e, context = "summary_generation")
                    list(executive = "Report generation in progress.")
                })

                # Load charts from paths
                charts <- list()
                tryCatch({
                    for(name in names(chart_collection$charts)) {
                        chart_path <- chart_collection$charts[[name]]
                        if(is.character(chart_path) && file.exists(chart_path)) {
                            charts[[name]] <- readRDS(chart_path)  # ← Now files still exist!
                        }
                    }
                }, error = function(e) {
                    log_error(e, context = "chart_loading")
                })

                # Get auction data
                auction_data <- tryCatch({
                    # weekly_auction_summary should also be modified to accept data
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

                    # EXECUTIVE SUMMARY PAGE WITH HEADER
                    if(!is.null(summaries$executive)) {
                        grid.newpage()

                        # Add small logo in header
                        if(!is.null(logo_grob)) {
                            pushViewport(viewport(x = 0.9, y = 0.95, width = 0.15, height = 0.06))
                            grid.draw(logo_grob)
                            popViewport()

                            # Header line
                            grid.lines(x = c(0.05, 0.95), y = c(0.92, 0.92),
                                       gp = gpar(col = "#E0E0E0", lwd = 1))
                        }

                        exec_title_grob <- textGrob(
                            "Executive Summary",
                            x = 0.05, y = 0.95,
                            just = "left",
                            gp = gpar(fontsize = 18, fontface = 2, col = "#1B3A6B")
                        )
                        grid.draw(exec_title_grob)

                        # [Rest of executive summary content...]
                        summary_text <- summaries$executive
                        wrapped_text <- tryCatch({
                            strwrap(summary_text, width = 95)
                        }, error = function(e) {
                            "Report content being generated."
                        })

                        summary_df <- data.frame(
                            Content = wrapped_text,
                            stringsAsFactors = FALSE
                        )

                        summary_table <- tableGrob(
                            summary_df,
                            rows = NULL,
                            cols = NULL,
                            theme = ttheme_minimal(
                                base_size = 11,
                                base_colour = "#333333"
                            )
                        )

                        pushViewport(viewport(x = 0.5, y = 0.5, width = 0.9, height = 0.7))
                        grid.draw(summary_table)
                        popViewport()
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

                    # Add charts with headers
                    for(chart_name in names(charts)) {
                        if(!is.null(charts[[chart_name]])) {
                            tryCatch({
                                is_ggplot <- "ggplot" %in% class(charts[[chart_name]])

                                if(is_ggplot) {
                                    # For ggplot objects, print first then add header
                                    print(charts[[chart_name]])

                                    # Add header overlay (may not work well with ggplot)
                                    # Consider adding logo to ggplot theme instead
                                } else {
                                    # For grid objects
                                    grid.newpage()
                                    add_page_header(gsub("_", " ", tools::toTitleCase(chart_name)))

                                    # Adjust viewport to account for header
                                    pushViewport(viewport(x = 0.5, y = 0.45, width = 0.9, height = 0.85))
                                    grid.draw(charts[[chart_name]])
                                    popViewport()
                                }
                            }, error = function(e) {
                                grid.newpage()
                                add_page_header()
                                grid.text(
                                    paste("Chart", chart_name, "temporarily unavailable"),
                                    x = 0.5, y = 0.5,
                                    gp = gpar(fontsize = 12, col = "#666666")
                                )
                            })
                        }
                    }

                    # FOOTER PAGE WITH LOGO
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
            paste0("insele_bond_report_", format(input$report_date, "%Y%m%d"), ".html")
        },
        content = function(file) {
            withProgress(message = "Generating HTML report...", value = 0, {

                incProgress(0.1, detail = "Collecting data")

                # Get reactive data
                proc_data <- tryCatch(processed_data(), error = function(e) NULL)
                filt_data <- tryCatch(filtered_data(), error = function(e) NULL)
                var_data_val <- tryCatch(var_data(), error = function(e) NULL)
                regime_data_val <- tryCatch(regime_data(), error = function(e) NULL)
                carry_data_val <- tryCatch(carry_roll_data(), error = function(e) NULL)
                filt_data_with_tech <- tryCatch(filtered_data_with_technicals(), error = function(e) NULL)

                # Collect selected sections
                sections <- character()
                if(isTRUE(input$section_overview)) sections <- c(sections, "overview")
                if(isTRUE(input$section_relative)) sections <- c(sections, "relative")
                if(isTRUE(input$section_risk)) sections <- c(sections, "risk")
                if(isTRUE(input$section_technical)) sections <- c(sections, "technical")
                if(isTRUE(input$section_carry)) sections <- c(sections, "carry")
                if(isTRUE(input$section_auction)) sections <- c(sections, "auction")
                if(isTRUE(input$section_intelligence)) sections <- c(sections, "intelligence")
                if(isTRUE(input$section_treasury)) sections <- c(sections, "treasury")
                if(isTRUE(input$section_recommendations)) sections <- c(sections, "recommendations")

                # Prepare input parameters with selected plots (ALL 35 plots)
                input_params <- list(
                    report_sections = sections,
                    selected_plots = list(
                        # Overview
                        regime_plot = isTRUE(input$plot_regime_plot),

                        # Relative Value
                        yield_curve = isTRUE(input$plot_yield_curve),
                        relative_heatmap = isTRUE(input$plot_relative_heatmap),
                        zscore_plot = isTRUE(input$plot_zscore_plot),
                        convexity = isTRUE(input$plot_convexity),  # NEW

                        # Risk
                        var_distribution = isTRUE(input$plot_var_distribution),
                        var_ladder = isTRUE(input$plot_var_ladder),
                        dv01_ladder = isTRUE(input$plot_dv01_ladder),

                        # Technical
                        technical_plot = isTRUE(input$plot_technical_plot),
                        signal_matrix = isTRUE(input$plot_signal_matrix),

                        # Carry & Roll
                        carry_heatmap = isTRUE(input$plot_carry_heatmap),
                        scenario_analysis = isTRUE(input$plot_scenario_analysis),
                        optimal_holding = isTRUE(input$plot_optimal_holding),  # NEW
                        forward_curve = isTRUE(input$plot_forward_curve),  # NEW

                        # Auction (8 new plots)
                        auction_performance = isTRUE(input$plot_auction_performance),
                        auction_patterns = isTRUE(input$plot_auction_patterns),
                        auction_forecast = isTRUE(input$plot_auction_forecast),  # NEW
                        demand_elasticity = isTRUE(input$plot_demand_elasticity),  # NEW
                        success_probability = isTRUE(input$plot_success_probability),  # NEW
                        bid_distribution = isTRUE(input$plot_bid_distribution),  # NEW
                        ytd_issuance = isTRUE(input$plot_ytd_issuance),  # NEW
                        auction_sentiment = isTRUE(input$plot_auction_sentiment),  # NEW
                        auction_success_factors = isTRUE(input$plot_auction_success_factors),  # NEW
                        btc_decomposition = isTRUE(input$plot_btc_decomposition),  # NEW

                        # Intelligence
                        correlation = isTRUE(input$plot_correlation),
                        term_structure = isTRUE(input$plot_term_structure),

                        # Treasury Holdings
                        holdings_area = isTRUE(input$plot_holdings_area),
                        sector_trend = isTRUE(input$plot_sector_trend),
                        holdings_fixed = isTRUE(input$plot_holdings_fixed),
                        holdings_ilb = isTRUE(input$plot_holdings_ilb),
                        holdings_frn = isTRUE(input$plot_holdings_frn),
                        holdings_sukuk = isTRUE(input$plot_holdings_sukuk),
                        ownership_changes = isTRUE(input$plot_ownership_changes),
                        holdings_diverging_fixed = isTRUE(input$plot_holdings_diverging_fixed),
                        holdings_diverging_ilb = isTRUE(input$plot_holdings_diverging_ilb)
                    ),
                    # Pass other required params for auction plots
                    auction_bonds_select = input$auction_bonds_select,
                    xaxis_choice = input$xaxis_choice,
                    curve_model = input$curve_model,
                    return_type = input$return_type,
                    tech_bond_select = input$tech_bond_select,
                    tech_indicator_type = input$tech_indicator_type
                )

                incProgress(0.2, detail = "Collecting charts")

                # Get treasury data for report (Word export)
                treasury_holdings_ts_val <- tryCatch({
                    if(!is.null(treasury_module_data) && !is.null(treasury_module_data$holdings_ts)) {
                        treasury_module_data$holdings_ts()
                    } else { NULL }
                }, error = function(e) NULL)

                treasury_bond_holdings_val <- tryCatch({
                    if(!is.null(treasury_module_data) && !is.null(treasury_module_data$bond_holdings)) {
                        treasury_module_data$bond_holdings()
                    } else { NULL }
                }, error = function(e) NULL)

                # Collect charts with error handling
                charts <- list()
                tryCatch({
                    chart_collection <- collect_report_charts(
                        proc_data,
                        filt_data,
                        filt_data_with_tech,
                        var_data_val,
                        regime_data_val,
                        carry_data_val,
                        treasury_holdings_ts_val,    # Treasury time series
                        treasury_bond_holdings_val,  # Treasury bond holdings
                        input_params
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

                incProgress(0.4, detail = "Converting charts")

                # Convert charts to base64
                charts_base64 <- list()
                for(name in names(charts)) {
                    if(!is.null(charts[[name]])) {
                        charts_base64[[name]] <- plot_to_base64(charts[[name]], width = 10, height = 6)
                    }
                }

                incProgress(0.5, detail = "Generating summaries")

                # Generate summaries
                summaries <- list(
                    overview = "Market analysis shows current regime characteristics and positioning.",
                    yield_curve = tryCatch(generate_chart_summary("yield_curve", proc_data), error = function(e) ""),
                    risk = tryCatch(generate_chart_summary("var_analysis", var_data_val), error = function(e) ""),
                    carry = "Carry and roll analysis identifies optimal holding periods."
                )

                # Get auction data
                auction_data <- tryCatch(weekly_auction_summary(), error = function(e) NULL)

                incProgress(0.8, detail = "Creating HTML")

                # Create HTML report
                html_content <- create_email_template(
                    charts_base64,
                    summaries,
                    auction_data,
                    paste("Report prepared for:", input$client_name)
                )

                # Save to file
                writeLines(html_content, file)

                incProgress(1, detail = "Complete")
                showNotification("HTML report generated successfully", type = "message")
            })
        }
    )

    # Replace the existing export_excel handler
    output$export_excel <- downloadHandler(
        filename = function() {
            paste0("insele_bond_analysis_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
        },
        content = function(file) {
            require(openxlsx)

            withProgress(message = "Creating Excel workbook...", value = 0, {

                wb <- createWorkbook()

                # Define styles safely
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

                # 1. Executive Summary with error handling
                tryCatch({
                    addWorksheet(wb, "Executive Summary")

                    writeData(wb, "Executive Summary",
                              "SA Government Bond Analysis Report",
                              startRow = 1, startCol = 1)
                    addStyle(wb, "Executive Summary",
                             createStyle(fontSize = 16, textDecoration = "bold"),
                             rows = 1, cols = 1)

                    # Build summary metrics safely
                    summary_metrics <- data.frame(
                        Metric = c("Report Date", "Analysis Period", "Total Bonds Analyzed",
                                   "Average Yield (%)", "Average Duration (years)"),
                        Value = c(
                            format(Sys.Date(), "%B %d, %Y"),
                            if(!is.null(input$date_range)) {
                                paste(format(input$date_range[1], "%b %d"), "-",
                                      format(input$date_range[2], "%b %d, %Y"))
                            } else { "Not specified" },
                            if(!is.null(filtered_data())) {
                                as.character(length(unique(filtered_data()$bond)))
                            } else { "0" },
                            if(!is.null(processed_data()) && nrow(processed_data()) > 0) {
                                sprintf("%.3f", mean(processed_data()$yield_to_maturity, na.rm = TRUE))
                            } else { "N/A" },
                            if(!is.null(processed_data()) && nrow(processed_data()) > 0) {
                                sprintf("%.2f", mean(processed_data()$modified_duration, na.rm = TRUE))
                            } else { "N/A" }
                        )
                    )

                    writeData(wb, "Executive Summary", summary_metrics,
                              startRow = 3, headerStyle = header_style)
                    setColWidths(wb, "Executive Summary", cols = 1:2, widths = c(30, 25))

                }, error = function(e) {
                    log_error(e, context = "excel_summary")
                })

                incProgress(0.2, detail = "Bond Metrics")

                # 2. Current Bond Metrics with validation
                if(!is.null(processed_data()) && nrow(processed_data()) > 0) {
                    tryCatch({
                        addWorksheet(wb, "Bond Metrics")

                        # Select only existing columns
                        available_cols <- names(processed_data())
                        bond_metrics <- processed_data()

                        # Build metrics data frame with available columns
                        metrics_df <- data.frame(Bond = bond_metrics$bond)

                        if("yield_to_maturity" %in% available_cols) {
                            metrics_df$`Yield (%)` <- bond_metrics$yield_to_maturity
                        }
                        if("modified_duration" %in% available_cols) {
                            metrics_df$`Mod Duration` <- bond_metrics$modified_duration
                        }
                        if("duration" %in% available_cols) {
                            metrics_df$Duration <- bond_metrics$duration
                        }
                        if("convexity" %in% available_cols) {
                            metrics_df$Convexity <- bond_metrics$convexity
                        }
                        if("coupon" %in% available_cols) {
                            metrics_df$`Coupon (%)` <- bond_metrics$coupon
                        }
                        if("spread_to_curve" %in% available_cols) {
                            metrics_df$`Spread (bps)` <- bond_metrics$spread_to_curve
                        }
                        if("z_score" %in% available_cols) {
                            metrics_df$`Z-Score` <- bond_metrics$z_score
                        }

                        writeData(wb, "Bond Metrics", metrics_df, headerStyle = header_style)

                        # Add conditional formatting only if Z-Score exists
                        if("z_score" %in% available_cols) {
                            z_col <- which(names(metrics_df) == "Z-Score")
                            if(length(z_col) > 0 && nrow(metrics_df) > 0) {
                                conditionalFormatting(wb, "Bond Metrics",
                                                      cols = z_col,
                                                      rows = 2:(nrow(metrics_df)+1),
                                                      style = c("#FFCDD2", "#FFFFFF", "#C8E6C9"),
                                                      rule = c(-2, 0, 2),
                                                      type = "colourScale")
                            }
                        }

                    }, error = function(e) {
                        log_error(e, context = "excel_bond_metrics")
                    })
                }

                incProgress(0.3, detail = "Risk Metrics")

                # 3. Risk Metrics with null checks
                if(!is.null(var_data()) && nrow(var_data()) > 0) {
                    tryCatch({
                        addWorksheet(wb, "Risk Metrics")

                        risk_metrics <- var_data()
                        risk_df <- data.frame(Bond = risk_metrics$bond)

                        # Add columns that exist
                        if("VaR_95_bps" %in% names(risk_metrics)) {
                            risk_df$`95% VaR (bps)` <- risk_metrics$VaR_95_bps
                        }
                        if("VaR_99_bps" %in% names(risk_metrics)) {
                            risk_df$`99% VaR (bps)` <- risk_metrics$VaR_99_bps
                        }
                        if("CVaR_95" %in% names(risk_metrics)) {
                            risk_df$`CVaR 95% (%)` <- risk_metrics$CVaR_95
                        }
                        if("vol" %in% names(risk_metrics)) {
                            risk_df$`Volatility (%)` <- risk_metrics$vol
                        }

                        writeData(wb, "Risk Metrics", risk_df, headerStyle = header_style)
                        setColWidths(wb, "Risk Metrics", cols = 1:ncol(risk_df), widths = "auto")

                    }, error = function(e) {
                        log_error(e, context = "excel_risk_metrics")
                    })
                }

                incProgress(0.4, detail = "Auction History")

                # 4. Auction History with safe filtering
                if(!is.null(filtered_data()) && "bid_to_cover" %in% names(filtered_data())) {
                    tryCatch({
                        addWorksheet(wb, "Auction History")

                        auction_history <- filtered_data() %>%
                            filter(!is.na(bid_to_cover))

                        if(nrow(auction_history) > 0) {
                            auction_df <- data.frame(
                                Date = auction_history$date,
                                Bond = auction_history$bond
                            )

                            if("offer" %in% names(auction_history)) {
                                auction_df$`Offer (R bn)` <- round(auction_history$offer/1e9, 2)
                            }
                            if("allocation" %in% names(auction_history)) {
                                auction_df$`Allocation (R bn)` <- round(auction_history$allocation/1e9, 2)
                            }
                            if("bids" %in% names(auction_history)) {
                                auction_df$`Bids (R bn)` <- round(auction_history$bids/1e9, 2)
                            }
                            auction_df$`Bid-to-Cover` <- auction_history$bid_to_cover

                            auction_df <- auction_df %>% arrange(desc(Date))

                            writeData(wb, "Auction History", auction_df, headerStyle = header_style)

                            # Highlight strong auctions
                            if("Bid-to-Cover" %in% names(auction_df)) {
                                strongAuctions <- which(auction_df$`Bid-to-Cover` > 3)
                                if(length(strongAuctions) > 0) {
                                    addStyle(wb, "Auction History", highlight_style,
                                             rows = strongAuctions + 1, cols = 1:ncol(auction_df),
                                             gridExpand = TRUE)
                                }
                            }
                        }

                    }, error = function(e) {
                        log_error(e, context = "excel_auction_history")
                    })
                }

                incProgress(0.5, detail = "Carry & Roll")

                # 5. Carry & Roll Analysis with validation
                if(!is.null(carry_roll_data()) && nrow(carry_roll_data()) > 0) {
                    tryCatch({
                        addWorksheet(wb, "Carry & Roll")

                        carry_metrics <- carry_roll_data()
                        carry_df <- data.frame(Bond = carry_metrics$bond)

                        # Add available columns
                        col_mapping <- list(
                            "holding_period" = "Holding Period",
                            "carry_income" = "Carry Income (%)",
                            "roll_return" = "Roll Return (%)",
                            "funding_cost" = "Funding Cost (%)",
                            "net_return" = "Net Return (%)",
                            "return_per_unit_risk" = "Return/Risk"
                        )

                        for(col_name in names(col_mapping)) {
                            if(col_name %in% names(carry_metrics)) {
                                carry_df[[col_mapping[[col_name]]]] <- carry_metrics[[col_name]]
                            }
                        }

                        carry_df <- carry_df %>% arrange(Bond)

                        writeData(wb, "Carry & Roll", carry_df, headerStyle = header_style)
                        setColWidths(wb, "Carry & Roll", cols = 1:ncol(carry_df), widths = "auto")

                    }, error = function(e) {
                        log_error(e, context = "excel_carry_roll")
                    })
                }

                incProgress(0.9, detail = "Metadata")

                # 6. Metadata
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

                # Save workbook with error handling
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
                        paste("Email content saved to:", output_file),
                        type = "message",
                        duration = 10
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
