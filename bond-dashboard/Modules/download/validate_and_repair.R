# ============================================================================
# SA Government Bond Holdings - File Validation and Repair Utility
# ============================================================================
#
# Use this script to:
# 1. Identify corrupted or invalid Excel files
# 2. Re-download problematic files
# 3. Clean up invalid files
#
# ============================================================================

require(readxl)
require(dplyr)

# ============================================================================
# Main Validation Function
# ============================================================================

validate_bond_files <- function(folder = "bond_holdings",
                                verbose = TRUE,
                                min_size = 10000) {  # 10KB minimum

    # Get all Excel files (both .xls and .xlsx formats)
    excel_files <- list.files(folder,
                              pattern = "Historical government bond holdings.*\\.(xls|xlsx)$",
                              full.names = TRUE)

    if (length(excel_files) == 0) {
        cat("No Excel files found in folder:", folder, "\n")
        return(invisible(NULL))
    }

    cat(strrep("=", 70), "\n")
    cat("VALIDATING BOND HOLDINGS FILES\n")
    cat(strrep("=", 70), "\n\n")
    cat(sprintf("Checking %d files in: %s\n\n", length(excel_files), folder))

    # Storage for results
    validation_results <- data.frame(
        filename = character(),
        filepath = character(),
        file_size = numeric(),
        exists = logical(),
        size_ok = logical(),
        can_open = logical(),
        has_sheets = logical(),
        sheet_count = integer(),
        status = character(),
        stringsAsFactors = FALSE
    )

    # Check each file
    for (file_path in excel_files) {
        filename <- basename(file_path)

        # Initialize result row
        result <- list(
            filename = filename,
            filepath = file_path,
            file_size = 0,
            exists = FALSE,
            size_ok = FALSE,
            can_open = FALSE,
            has_sheets = FALSE,
            sheet_count = 0,
            status = "Unknown"
        )

        # Check 1: File exists
        if (file.exists(file_path)) {
            result$exists <- TRUE
            result$file_size <- file.size(file_path)

            # Check 2: File size
            if (result$file_size >= min_size) {
                result$size_ok <- TRUE

                # Check 3: Can open file
                tryCatch({
                    sheets <- excel_sheets(file_path)
                    result$can_open <- TRUE
                    result$sheet_count <- length(sheets)

                    # Check 4: Has sheets
                    if (length(sheets) > 0) {
                        result$has_sheets <- TRUE
                        result$status <- "Valid"
                        if (verbose) {
                            cat(sprintf("✓ %s (%d sheets, %.1f KB)\n",
                                        filename,
                                        length(sheets),
                                        result$file_size / 1024))
                        }
                    } else {
                        result$status <- "No sheets"
                        cat(sprintf("✗ %s - No sheets found\n", filename))
                    }
                }, error = function(e) {
                    result$status <<- paste("Cannot open:", e$message)
                    cat(sprintf("✗ %s - Cannot open: %s\n", filename, e$message))
                })
            } else {
                result$status <- sprintf("Too small (%d bytes)", result$file_size)
                cat(sprintf("✗ %s - File too small (%d bytes)\n", filename, result$file_size))
            }
        } else {
            result$status <- "File not found"
            cat(sprintf("✗ %s - File not found\n", filename))
        }

        validation_results <- rbind(validation_results, as.data.frame(result))
    }

    # Summary
    cat("\n")
    cat(strrep("=", 70), "\n")
    cat("VALIDATION SUMMARY\n")
    cat(strrep("=", 70), "\n\n")

    n_valid <- sum(validation_results$status == "Valid")
    n_invalid <- nrow(validation_results) - n_valid

    cat(sprintf("Total files: %d\n", nrow(validation_results)))
    cat(sprintf("Valid files: %d (%.1f%%)\n", n_valid, 100 * n_valid / nrow(validation_results)))
    cat(sprintf("Invalid files: %d (%.1f%%)\n", n_invalid, 100 * n_invalid / nrow(validation_results)))

    if (n_invalid > 0) {
        cat("\n⚠ Invalid files found:\n")
        invalid_files <- validation_results %>%
            filter(status != "Valid") %>%
            select(filename, file_size, status)

        print(invalid_files, row.names = FALSE)
    }

    return(invisible(validation_results))
}

# ============================================================================
# Clean Up Corrupted Files
# ============================================================================

remove_corrupted_files <- function(folder = "bond_holdings",
                                   backup_folder = "bond_holdings_backup",
                                   dry_run = TRUE) {

    # Validate files first
    validation <- validate_bond_files(folder, verbose = FALSE)

    corrupted_files <- validation %>%
        filter(status != "Valid")

    if (nrow(corrupted_files) == 0) {
        cat("\n✓ No corrupted files found. All files are valid!\n")
        return(invisible(NULL))
    }

    cat("\n")
    cat(strrep("=", 70), "\n")
    cat("CORRUPTED FILE CLEANUP\n")
    cat(strrep("=", 70), "\n\n")

    cat(sprintf("Found %d corrupted file(s) to remove:\n\n", nrow(corrupted_files)))

    for (i in 1:nrow(corrupted_files)) {
        cat(sprintf("%d. %s (%s)\n",
                    i,
                    corrupted_files$filename[i],
                    corrupted_files$status[i]))
    }

    if (dry_run) {
        cat("\n⚠ DRY RUN MODE - No files will be deleted.\n")
        cat("Set dry_run = FALSE to actually remove files.\n")
        return(invisible(corrupted_files))
    }

    # Create backup folder if needed
    if (!dir.exists(backup_folder)) {
        dir.create(backup_folder, recursive = TRUE)
        cat(sprintf("\nCreated backup folder: %s\n", backup_folder))
    }

    # Move corrupted files to backup
    cat("\nMoving corrupted files to backup folder...\n")

    for (i in 1:nrow(corrupted_files)) {
        old_path <- corrupted_files$filepath[i]
        new_path <- file.path(backup_folder, corrupted_files$filename[i])

        tryCatch({
            file.copy(old_path, new_path, overwrite = TRUE)
            file.remove(old_path)
            cat(sprintf("  ✓ Moved: %s\n", corrupted_files$filename[i]))
        }, error = function(e) {
            cat(sprintf("  ✗ Failed to move: %s (%s)\n",
                        corrupted_files$filename[i], e$message))
        })
    }

    cat("\n✓ Cleanup complete. Corrupted files moved to:", backup_folder, "\n")

    return(invisible(corrupted_files))
}

# ============================================================================
# Re-download Corrupted Files
# ============================================================================

redownload_corrupted_files <- function(folder = "bond_holdings",
                                       sleep_time = 1) {

    require(httr)

    # Validate files first
    validation <- validate_bond_files(folder, verbose = FALSE)

    corrupted_files <- validation %>%
        filter(status != "Valid")

    if (nrow(corrupted_files) == 0) {
        cat("\n✓ No corrupted files found. All files are valid!\n")
        return(invisible(NULL))
    }

    cat("\n")
    cat(strrep("=", 70), "\n")
    cat("RE-DOWNLOADING CORRUPTED FILES\n")
    cat(strrep("=", 70), "\n\n")

    cat(sprintf("Will attempt to re-download %d file(s):\n\n", nrow(corrupted_files)))

    # Base URL
    base_url <- "https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Holdings%20of%20Domestic%20Bonds/"

    results <- data.frame(
        filename = character(),
        status = character(),
        stringsAsFactors = FALSE
    )

    for (i in 1:nrow(corrupted_files)) {
        filename <- corrupted_files$filename[i]
        dest_file <- corrupted_files$filepath[i]

        cat(sprintf("%d. Re-downloading: %s...", i, filename))

        # Construct URL
        encoded_filename <- URLencode(filename, reserved = TRUE)
        url <- paste0(base_url, encoded_filename)

        # Delete old corrupted file
        if (file.exists(dest_file)) {
            file.remove(dest_file)
        }

        # Download
        tryCatch({
            response <- GET(url, write_disk(dest_file, overwrite = TRUE))

            if (status_code(response) == 200) {
                # Verify the new download
                if (file.size(dest_file) > 10000) {
                    test_sheets <- excel_sheets(dest_file)
                    if (length(test_sheets) > 0) {
                        cat(" SUCCESS ✓\n")
                        results <- rbind(results, data.frame(
                            filename = filename,
                            status = "Success"
                        ))
                    } else {
                        cat(" FAILED - File has no sheets\n")
                        results <- rbind(results, data.frame(
                            filename = filename,
                            status = "Failed - No sheets"
                        ))
                    }
                } else {
                    cat(" FAILED - File too small\n")
                    results <- rbind(results, data.frame(
                        filename = filename,
                        status = "Failed - Too small"
                    ))
                }
            } else {
                cat(sprintf(" FAILED - HTTP %d\n", status_code(response)))
                results <- rbind(results, data.frame(
                    filename = filename,
                    status = paste0("Failed - HTTP ", status_code(response))
                ))
            }
        }, error = function(e) {
            cat(sprintf(" ERROR: %s\n", e$message))
            results <<- rbind(results, data.frame(
                filename = filename,
                status = paste0("Error: ", e$message)
            ))
        })

        Sys.sleep(sleep_time)
    }

    # Summary
    cat("\n")
    cat(strrep("=", 70), "\n")
    cat("RE-DOWNLOAD SUMMARY\n")
    cat(strrep("=", 70), "\n\n")

    n_success <- sum(results$status == "Success")
    cat(sprintf("Successful: %d/%d\n", n_success, nrow(results)))

    if (n_success < nrow(results)) {
        cat("\nFailed downloads:\n")
        failed <- results %>% filter(status != "Success")
        print(failed, row.names = FALSE)
    }

    return(invisible(results))
}

# ============================================================================
# Complete Repair Workflow
# ============================================================================

repair_bond_files <- function(folder = "bond_holdings",
                              backup_folder = "bond_holdings_backup",
                              sleep_time = 1) {

    cat(strrep("=", 70), "\n")
    cat("BOND HOLDINGS FILE REPAIR WORKFLOW\n")
    cat(strrep("=", 70), "\n\n")

    # Step 1: Validate
    cat("STEP 1: Validating files...\n")
    cat(strrep("-", 70), "\n")
    validation <- validate_bond_files(folder, verbose = TRUE)

    corrupted <- validation %>% filter(status != "Valid")

    if (nrow(corrupted) == 0) {
        cat("\n✓ All files are valid! No repairs needed.\n")
        return(invisible(validation))
    }

    # Step 2: Backup corrupted files
    cat("\n\n")
    cat("STEP 2: Backing up corrupted files...\n")
    cat(strrep("-", 70), "\n")
    removed <- remove_corrupted_files(folder, backup_folder, dry_run = FALSE)

    # Step 3: Re-download
    cat("\n\n")
    cat("STEP 3: Re-downloading files...\n")
    cat(strrep("-", 70), "\n")
    download_results <- redownload_corrupted_files(folder, sleep_time)

    # Step 4: Final validation
    cat("\n\n")
    cat("STEP 4: Final validation...\n")
    cat(strrep("-", 70), "\n")
    final_validation <- validate_bond_files(folder, verbose = FALSE)

    cat("\n")
    cat(strrep("=", 70), "\n")
    cat("REPAIR COMPLETE\n")
    cat(strrep("=", 70), "\n\n")

    n_valid <- sum(final_validation$status == "Valid")
    n_total <- nrow(final_validation)

    cat(sprintf("Final status: %d/%d files valid (%.1f%%)\n",
                n_valid, n_total, 100 * n_valid / n_total))

    if (n_valid < n_total) {
        cat("\n⚠ Some files are still invalid. You may need to:\n")
        cat("  1. Check your internet connection\n")
        cat("  2. Try again later (files may not be available on Treasury site)\n")
        cat("  3. Manually download problematic files\n")
    }

    return(invisible(list(
        validation = validation,
        download_results = download_results,
        final_validation = final_validation
    )))
}

# ============================================================================
# Quick Fix Function
# ============================================================================

quick_fix <- function() {
    # One-command solution for most issues
    repair_bond_files()
}

# ============================================================================
# Usage Examples
# ============================================================================

# Note: Load messages are commented out to avoid noise when sourcing
# cat("\n")
# cat(strrep("=", 70), "\n")
# cat("FILE VALIDATION UTILITY LOADED\n")
# cat(strrep("=", 70), "\n")
# cat("\nAvailable functions:\n\n")
# cat("  validate_bond_files()         - Check all files for corruption\n")
# cat("  remove_corrupted_files()      - Move corrupted files to backup\n")
# cat("  redownload_corrupted_files()  - Re-download problematic files\n")
# cat("  repair_bond_files()           - Complete repair workflow (RECOMMENDED)\n")
# cat("  quick_fix()                   - Same as repair_bond_files()\n")
# cat("\nQuick start:\n")
# cat("  repair_bond_files()  # Fix everything automatically\n")
# cat(strrep("=", 70), "\n\n")