# ════════════════════════════════════════════════════════════════════════
# CRITICAL PIPELINE UTILITIES - ADD TO ui_helpers.R
# ════════════════════════════════════════════════════════════════════════

#' @export
#' @title Ensure Date Columns
#' @description Ensure specified columns are Date objects (fixes character date issues)
#' @param data Data frame to check
#' @param date_cols Vector of column names that should be Date objects
#' @return Data frame with Date columns properly typed
ensure_date_columns <- function(data, date_cols = c("date", "offer_date", "announce_date", "sett_date", "mat_date", "maturity_date")) {
    if(is.null(data) || nrow(data) == 0) {
        return(data)
    }

    for(col_name in date_cols) {
        if(col_name %in% names(data)) {
            if(!inherits(data[[col_name]], "Date")) {
                tryCatch({
                    data[[col_name]] <- as.Date(data[[col_name]])
                }, error = function(e) {
                    warning(sprintf("Could not convert %s to Date: %s", col_name, e$message))
                })
            }
        }
    }

    return(data)
}

#' @export
#' @title Sanitize Pipeline Data
#' @description Nuclear option: Strip ALL attributes, force plain data.frame, and filter NA bonds
#' @param data Data to sanitize
#' @param context String for logging where sanitization occurs
#' @return Guaranteed plain data.frame with zero hidden attributes and no NA bond rows
sanitize_pipeline_data <- function(data, context = "unknown") {
    if(is.null(data)) {
        message(sprintf("[%s] Data is NULL - returning NULL", context))
        return(NULL)
    }

    if(nrow(data) == 0) {
        message(sprintf("[%s] Data has 0 rows - returning empty data.frame", context))
        return(data.frame())
    }

    # ════════════════════════════════════════════════════════════════════════
    # CRITICAL FIX: Filter out NA bond names at EARLIEST possible point
    # ════════════════════════════════════════════════════════════════════════
    # This prevents NA bonds from appearing anywhere in the pipeline
    # Must happen BEFORE any grouping operations to prevent NA groups

    if("bond" %in% names(data)) {
        n_before <- nrow(data)
        data <- data[!is.na(data$bond) &
                    data$bond != "" &
                    data$bond != "NA" &
                    as.character(data$bond) != "NA" &
                    nchar(trimws(as.character(data$bond))) > 0, ]
        n_after <- nrow(data)

        if(n_before != n_after) {
            message(sprintf("[%s] Filtered %d NA/invalid bond rows (%d → %d)",
                           context, n_before - n_after, n_before, n_after))
        }

        if(nrow(data) == 0) {
            message(sprintf("[%s] All rows filtered - no valid bond data", context))
            return(data.frame())
        }
    }

    # STEP 0: Detect Date columns BEFORE conversion (CRITICAL FIX)
    date_columns <- sapply(data, function(col) inherits(col, "Date"))
    date_col_names <- names(date_columns)[date_columns]

    # STEP 1: Force conversion to plain data.frame
    result <- as.data.frame(data, stringsAsFactors = FALSE, row.names = NULL)

    # STEP 1.5: Restore Date columns that were converted to character (CRITICAL FIX)
    for(col_name in date_col_names) {
        if(col_name %in% names(result)) {
            if(!inherits(result[[col_name]], "Date")) {
                # Date was converted to character - restore it
                result[[col_name]] <- as.Date(result[[col_name]])
                message(sprintf("[%s] ✓ Restored Date class for column: %s", context, col_name))
            }
        }
    }

    # STEP 1.6: Also check for common date column names and ensure they're Date objects
    common_date_cols <- c("date", "offer_date", "announce_date", "sett_date", "mat_date", "maturity_date")
    for(col_name in common_date_cols) {
        if(col_name %in% names(result) && !inherits(result[[col_name]], "Date")) {
            # Try to convert to Date if it looks like a date
            tryCatch({
                result[[col_name]] <- as.Date(result[[col_name]])
                message(sprintf("[%s] ✓ Converted %s to Date class", context, col_name))
            }, error = function(e) {
                warning(sprintf("[%s] Could not convert %s to Date: %s", context, col_name, e$message))
            })
        }
    }

    # STEP 2: Nuke ALL possible grouping/tibble/data.table attributes
    attrs_to_remove <- c(
        "groups", "vars", "drop", "indices", "group_sizes",
        "biggest_group_size", "labels", "class_attr",
        ".internal.selfref", "sorted", "index",
        ".data", ".drop", ".rows"
    )

    for(attr_name in attrs_to_remove) {
        attr(result, attr_name) <- NULL
    }

    # STEP 3: Force class to be ONLY "data.frame" (no tibble, no tbl_df, no grouped_df)
    class(result) <- "data.frame"

    # STEP 4: Verify row names are simple integers
    rownames(result) <- NULL

    # STEP 5: Log for debugging
    message(sprintf(
        "[%s] ✓ Sanitized: %d rows × %d cols | class=%s",
        context,
        nrow(result),
        ncol(result),
        paste(class(result), collapse = ", ")
    ))

    return(result)
}


#' @export
#' @title Standardize Column Names
#' @description Map common aliases to standard bond dashboard column names
#' @param data Data frame with potentially non-standard column names
#' @return Data frame with standardized column names
standardize_column_names <- function(data) {
    if(is.null(data) || nrow(data) == 0) return(data)

    original_names <- names(data)

    # First, clean up column names - trim whitespace and standardize
    names(data) <- trimws(names(data))

    # Define mapping: old_name -> standard_name (case-insensitive)
    column_mapping <- list(
        # Auction columns (try multiple variations)
        offer = "offer_amount",
        offers = "offer_amount",
        "offer amount" = "offer_amount",
        "offer_amt" = "offer_amount",

        bids = "bids_received",
        "bids received" = "bids_received",
        "bids_recv" = "bids_received",
        "total bids" = "bids_received",

        # Risk columns
        dv01 = "basis_point_value",
        mod_dur = "modified_duration",
        ytm = "yield_to_maturity",

        # Price columns
        dirty_price = "full_price",
        price = "clean_price"
    )

    # Apply mappings (case-insensitive)
    changes_made <- 0
    for(old_name in names(column_mapping)) {
        new_name <- column_mapping[[old_name]]

        # Find column index with case-insensitive match
        col_idx <- which(tolower(names(data)) == tolower(old_name))

        # Only rename if old exists and new doesn't exist
        if(length(col_idx) > 0 && !new_name %in% names(data)) {
            old_actual_name <- names(data)[col_idx[1]]
            names(data)[col_idx[1]] <- new_name
            message(sprintf("✓ Mapped column: '%s' → '%s'", old_actual_name, new_name))
            changes_made <- changes_made + 1
        }
    }

    # Verify critical columns exist
    required_base_cols <- c("bond", "date", "yield_to_maturity")
    missing_critical <- setdiff(required_base_cols, names(data))

    if(length(missing_critical) > 0) {
        stop(sprintf(
            "CRITICAL: Missing required base columns: %s\nAvailable columns: %s",
            paste(missing_critical, collapse = ", "),
            paste(names(data), collapse = ", ")
        ))
    }

    if(changes_made > 0) {
        message(sprintf("✓ Column standardization complete: %d changes", changes_made))
    }

    return(data)
}

#' @export
#' @title Validate Bond Data for Plotting
#' @description Check if data meets minimum requirements for visualization
#' @param data Data frame to validate
#' @param required_bonds Minimum number of unique bonds needed
#' @param required_rows_per_bond Minimum observations per bond
#' @param required_cols Optional vector of required column names
#' @return List with 'valid' (logical), 'message' (string), 'details' (list)
validate_bond_data_for_plot <- function(data,
                                        required_bonds = 1,
                                        required_rows_per_bond = 1,
                                        required_cols = NULL) {
    # NULL check
    if(is.null(data)) {
        return(list(
            valid = FALSE,
            message = "Data is NULL",
            details = list(bonds = 0, total_rows = 0)
        ))
    }

    # Empty check
    if(nrow(data) == 0) {
        return(list(
            valid = FALSE,
            message = "Data has 0 rows",
            details = list(bonds = 0, total_rows = 0)
        ))
    }

    # Bond column check
    if(!"bond" %in% names(data)) {
        return(list(
            valid = FALSE,
            message = "'bond' column is missing from data",
            details = list(
                available_cols = paste(head(names(data), 10), collapse = ", "),
                total_rows = nrow(data)
            )
        ))
    }

    # Count unique bonds
    n_bonds <- n_distinct(data$bond)

    if(n_bonds < required_bonds) {
        return(list(
            valid = FALSE,
            message = sprintf(
                "Insufficient bonds: need %d, have %d",
                required_bonds, n_bonds
            ),
            details = list(
                bonds = n_bonds,
                bond_names = unique(data$bond),
                total_rows = nrow(data)
            )
        ))
    }

    # Check rows per bond
    rows_per_bond <- data %>%
        count(bond) %>%
        pull(n)

    insufficient_bonds <- sum(rows_per_bond < required_rows_per_bond)

    if(insufficient_bonds > 0) {
        bond_details <- data %>%
            count(bond) %>%
            filter(n < required_rows_per_bond) %>%
            arrange(n)

        return(list(
            valid = FALSE,
            message = sprintf(
                "%d bond(s) have insufficient data (<% d rows per bond)",
                insufficient_bonds, required_rows_per_bond
            ),
            details = list(
                bonds = n_bonds,
                insufficient_bonds = insufficient_bonds,
                insufficient_details = bond_details,
                total_rows = nrow(data)
            )
        ))
    }

    # Optional: Check required columns
    if(!is.null(required_cols)) {
        missing_cols <- setdiff(required_cols, names(data))

        if(length(missing_cols) > 0) {
            return(list(
                valid = FALSE,
                message = sprintf(
                    "Missing required columns: %s",
                    paste(missing_cols, collapse = ", ")
                ),
                details = list(
                    bonds = n_bonds,
                    total_rows = nrow(data),
                    missing_columns = missing_cols,
                    available_columns = names(data)
                )
            ))
        }
    }

    # All checks passed!
    return(list(
        valid = TRUE,
        message = "Data validation passed",
        details = list(
            bonds = n_bonds,
            total_rows = nrow(data),
            avg_rows_per_bond = round(mean(rows_per_bond), 1),
            min_rows_per_bond = min(rows_per_bond),
            max_rows_per_bond = max(rows_per_bond)
        )
    ))
}


# ════════════════════════════════════════════════════════════════════════
# DATA CLEANING UTILITIES (Updated to use sanitize_pipeline_data)
# ════════════════════════════════════════════════════════════════════════

#' @export
#' @title Force Plain DataFrame
#' @description Remove all dplyr/tibble/data.table attributes
#' @param data Data object to convert
#' @return Plain data.frame with no hidden attributes
#' @note DEPRECATED: This now wraps sanitize_pipeline_data(). Consider using that directly.
force_plain_dataframe <- function(data) {
    # Wrapper for backward compatibility
    return(sanitize_pipeline_data(data, context = "force_plain_dataframe [wrapper]"))
}

#' @export
#' @title Validate Data Frame Class
#' @description Ensure data is a plain data.frame, warn if not
#' @param data Data to validate
#' @param context String describing where validation occurs (for logging)
#' @return Validated plain data.frame
#' @note DEPRECATED: This now wraps sanitize_pipeline_data(). Consider using that directly.
validate_dataframe_class <- function(data, context = "unknown") {
    # Wrapper for backward compatibility
    return(sanitize_pipeline_data(data, context = paste0("validate_dataframe_class [", context, "]")))
}


#' @export
#' @title Validate Required Columns
#' @description Check that required columns exist in data
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param context String for error messages
#' @return TRUE if valid, stops with error if not
#' @note KEEP THIS - Still useful and not superseded
validate_required_columns <- function(data, required_cols, context = "function") {
    if(is.null(data)) {
        stop(sprintf("[%s] Data is NULL", context))
    }

    missing_cols <- setdiff(required_cols, names(data))

    if(length(missing_cols) > 0) {
        stop(sprintf(
            "[%s] Missing required columns: %s\nAvailable columns: %s",
            context,
            paste(missing_cols, collapse = ", "),
            paste(names(data), collapse = ", ")
        ))
    }

    return(TRUE)
}



# ════════════════════════════════════════════════════════════════════════════════
# SMART LABEL FUNCTION - Intelligent label selection for crowded plots
#
# FIX: Improved algorithm to reduce label overlap in crowded areas:
# 1. Always include highest-priority signals (highest |z-score|)
# 2. Always include extremes (cheapest/richest)
# 3. Use spatial distribution to avoid clustering labels
# 4. Support for z-score based actionable thresholds
# ════════════════════════════════════════════════════════════════════════════════
smart_label <- function(data, label_col, priority_col = NULL, max_labels = 12,
                        x_col = "modified_duration", y_col = "yield_to_maturity",
                        min_spacing = 0.5) {
    if (is.null(data) || nrow(data) == 0) {
        return(data)
    }

    if (nrow(data) <= max_labels) {
        return(data)
    }

    # Initialize selection with all FALSE
    selected <- rep(FALSE, nrow(data))

    # PRIORITY 1: Always include bonds with strong signals (|z-score| > 2.0)
    if (!is.null(priority_col) && priority_col %in% names(data)) {
        strong_signals <- abs(data[[priority_col]]) > 2.0
        selected <- selected | strong_signals
    }

    # PRIORITY 2: Always include extremes (highest/lowest in y-axis)
    if (y_col %in% names(data) && sum(!is.na(data[[y_col]])) > 0) {
        max_idx <- which.max(data[[y_col]])
        min_idx <- which.min(data[[y_col]])
        selected[max_idx] <- TRUE
        selected[min_idx] <- TRUE
    }

    # PRIORITY 3: Include extremes in x-axis (shortest/longest duration)
    if (x_col %in% names(data) && sum(!is.na(data[[x_col]])) > 0) {
        max_x_idx <- which.max(data[[x_col]])
        min_x_idx <- which.min(data[[x_col]])
        selected[max_x_idx] <- TRUE
        selected[min_x_idx] <- TRUE
    }

    # PRIORITY 4: Fill remaining slots with highest priority bonds
    # that are spatially distributed (not too close to already selected)
    remaining_slots <- max_labels - sum(selected)

    if (remaining_slots > 0 && !is.null(priority_col) && priority_col %in% names(data)) {
        # Get candidates not yet selected
        candidates <- which(!selected)

        if (length(candidates) > 0) {
            # Sort candidates by priority (descending |value|)
            candidate_priorities <- abs(data[[priority_col]][candidates])
            sorted_candidates <- candidates[order(candidate_priorities, decreasing = TRUE)]

            # Add candidates with spatial distribution check
            for (cand in sorted_candidates) {
                if (sum(selected) >= max_labels) break

                # Check if this candidate is far enough from already selected points
                if (x_col %in% names(data) && y_col %in% names(data)) {
                    already_selected <- which(selected)
                    if (length(already_selected) > 0) {
                        # Calculate distances to selected points (in normalized space)
                        x_range <- diff(range(data[[x_col]], na.rm = TRUE))
                        y_range <- diff(range(data[[y_col]], na.rm = TRUE))

                        if (x_range > 0 && y_range > 0) {
                            x_norm <- (data[[x_col]][cand] - data[[x_col]][already_selected]) / x_range
                            y_norm <- (data[[y_col]][cand] - data[[y_col]][already_selected]) / y_range
                            min_dist <- min(sqrt(x_norm^2 + y_norm^2))

                            # Only add if far enough from existing labels
                            if (min_dist > min_spacing / 10) {
                                selected[cand] <- TRUE
                            }
                        } else {
                            selected[cand] <- TRUE
                        }
                    } else {
                        selected[cand] <- TRUE
                    }
                } else {
                    selected[cand] <- TRUE
                }
            }
        }
    }

    # If still haven't filled max_labels, add evenly spaced from remaining
    remaining_slots <- max_labels - sum(selected)
    if (remaining_slots > 0) {
        remaining_indices <- which(!selected)
        if (length(remaining_indices) > 0) {
            n_to_add <- min(remaining_slots, length(remaining_indices))
            evenly_spaced <- remaining_indices[round(seq(1, length(remaining_indices), length.out = n_to_add))]
            selected[evenly_spaced] <- TRUE
        }
    }

    return(data[selected, ])
}

#' @export
# Initialize logging
init_logging <- function() {
    log_dir <- file.path(getwd(), "logs")
    if (!dir.exists(log_dir)) dir.create(log_dir)

    log_file <- file.path(log_dir, paste0("report_", format(Sys.Date(), "%Y%m%d"), ".log"))

    list(
        dir = log_dir,
        file = log_file
    )
}

#' @export
# Enhanced error logging function
log_error <- function(error, context = "", session_id = NULL) {
    log_info <- init_logging()

    error_entry <- paste0(
        "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
        if(!is.null(session_id)) paste0("Session: ", session_id, " | "),
        "Context: ", context, " | ",
        "Error: ", as.character(error$message), "\n",
        "Call: ", paste(deparse(error$call), collapse = " "), "\n"
    )

    cat(error_entry, file = log_info$file, append = TRUE)

    # Also log to console in development
    if (getOption("shiny.testmode", FALSE)) {
        cat(error_entry)
    }
}

#' @export
# Safe execution wrapper with enhanced error handling
safe_execute <- function(expr, default = NULL, context = "", show_notification = TRUE, session = NULL) {
    tryCatch({
        expr
    }, error = function(e) {
        log_error(e, context = context, session_id = session$token)

        if (show_notification && !is.null(session)) {
            showNotification(
                paste("Error in", context, ":", e$message),
                type = "error",
                duration = 5,
                session = session
            )
        }

        return(list(success = FALSE, error = e$message, data = default))
    })
}


#' @export
# Helper function to load and convert logo
load_logo_for_pdf <- function(logo_path = "bond-dashboard/www/Insele_logo_2.webp") {
    tryCatch({
        # Check if file exists
        if(!file.exists(logo_path)) {
            # Try alternative paths
            alt_paths <- c(
                "www/Insele_logo_2.webp",
                "Modules/report_generators/www/Insele_logo_2.webp",
                file.path(getwd(), "bond-dashboard/Modules/report_generators/www/Insele_logo_2.webp")
            )

            for(path in alt_paths) {
                if(file.exists(path)) {
                    logo_path <- path
                    break
                }
            }
        }

        if(!file.exists(logo_path)) {
            return(NULL)
        }

        # Convert WebP to PNG for PDF compatibility
        temp_png <- tempfile(fileext = ".png")

        # Option 1: Using magick package (recommended)
        if(requireNamespace("magick", quietly = TRUE)) {
            img <- magick::image_read(logo_path)
            magick::image_write(img, path = temp_png, format = "png")
        }
        # Option 2: Using webp package
        else if(requireNamespace("webp", quietly = TRUE)) {
            webp::read_webp(logo_path, output = temp_png)
        }
        # Option 3: Using png package with conversion
        else {
            # This requires the image to already be in a compatible format
            return(NULL)
        }

        return(temp_png)
    }, error = function(e) {
        log_error(e, context = "logo_loading")
        return(NULL)
    })
}


# Function to embed image as base64
embed_logo <- function() {
    logo_paths <- c(
        "bond-dashboard/www/Insele_logo_2.webp",
        "bond-dashboard/www/insele_logo.webp",
        "www/Insele_logo_2.webp"
    )

    for(path in logo_paths) {
        if(file.exists(path)) {
            img_base64 <- base64encode(path)
            return(paste0("data:image/webp;base64,", img_base64))
        }
    }
    return(NULL)
}