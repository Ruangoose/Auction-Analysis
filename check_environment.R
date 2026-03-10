# ================================================================================
# INSELE CAPITAL PARTNERS - ENVIRONMENT VALIDATION SCRIPT
# Run this script on a fresh deployment to verify everything is in place
#
# Usage:
#   Rscript check_environment.R
#   -- or from R console --
#   source("check_environment.R")
# ================================================================================

cat("\n")
cat("================================================================\n")
cat("  INSELE BOND ANALYTICS - ENVIRONMENT CHECK\n")
cat("================================================================\n\n")

pass_count <- 0
warn_count <- 0
fail_count <- 0

check_pass <- function(msg) {
    pass_count <<- pass_count + 1
    cat(sprintf("  [PASS] %s\n", msg))
}
check_warn <- function(msg) {
    warn_count <<- warn_count + 1
    cat(sprintf("  [WARN] %s\n", msg))
}
check_fail <- function(msg) {
    fail_count <<- fail_count + 1
    cat(sprintf("  [FAIL] %s\n", msg))
}

# ---------------------------------------------------------------------------
# 1. Working Directory Check
# ---------------------------------------------------------------------------
cat("--- Working Directory ---\n")
cat(sprintf("  Current: %s\n", getwd()))

if (file.exists("bond-dashboard/enhanced_bond_server.R")) {
    check_pass("Working directory is correct (project root)")
} else if (file.exists("enhanced_bond_server.R")) {
    check_warn("Working directory is bond-dashboard/ (should be project root)")
    cat("         Fix: cd to the parent directory or adjust your launcher\n")
} else {
    check_fail("Cannot find enhanced_bond_server.R - wrong working directory")
}

# ---------------------------------------------------------------------------
# 2. Required Files Check
# ---------------------------------------------------------------------------
cat("\n--- Required Files ---\n")

required_files <- c(
    "bond-dashboard/enhanced_bond_server.R",
    "bond-dashboard/enhanced_bond_ui.R",
    "bond-dashboard/run_fast.R",
    "bond-dashboard/Modules/data_loader.R",
    "bond-dashboard/Modules/archive_loader.R",
    "bond-dashboard/Modules/theme_config.R",
    "bond-dashboard/Modules/ui_helpers.R",
    "bond-dashboard/Modules/data_processors.R",
    "bond-dashboard/Modules/plot_generators.R"
)

for (f in required_files) {
    if (file.exists(f)) {
        check_pass(f)
    } else {
        check_fail(sprintf("MISSING: %s", f))
    }
}

# ---------------------------------------------------------------------------
# 3. Data Files Check
# ---------------------------------------------------------------------------
cat("\n--- Data Files ---\n")

# Check for Excel data file (any of the known names)
excel_files <- c(
    "data/Insele_Bonds_Data_File.xlsm",
    "data/Insele Bonds Data File.xlsx",
    "data/Siyanda Bonds.xlsx"
)

excel_found <- FALSE
for (f in excel_files) {
    if (file.exists(f)) {
        check_pass(sprintf("Excel data file: %s (%.1f MB)", f, file.size(f) / 1e6))
        excel_found <- TRUE
        break
    }
}
if (!excel_found) {
    check_fail("No Excel data file found in data/ directory")
    cat("         Expected one of:\n")
    for (f in excel_files) cat(sprintf("           - %s\n", f))
}

# Check data directory
if (dir.exists("data")) {
    check_pass("data/ directory exists")
} else {
    check_fail("data/ directory missing")
}

# Check www directory (Shiny assets)
if (dir.exists("bond-dashboard/www")) {
    logo_files <- list.files("bond-dashboard/www", pattern = "\\.(png|webp)$")
    if (length(logo_files) > 0) {
        check_pass(sprintf("www/ assets: %s", paste(logo_files, collapse = ", ")))
    } else {
        check_warn("www/ directory exists but no logo files found")
    }
} else {
    check_warn("bond-dashboard/www/ directory missing (logo will not display)")
}

# Check bond holdings data (optional)
if (dir.exists("bond_holdings_rds")) {
    rds_files <- list.files("bond_holdings_rds", pattern = "\\.rds$")
    check_pass(sprintf("bond_holdings_rds/ has %d RDS files", length(rds_files)))
} else {
    check_warn("bond_holdings_rds/ not found (Treasury Holdings tab will be empty until data is downloaded)")
}

# ---------------------------------------------------------------------------
# 4. Required R Packages Check
# ---------------------------------------------------------------------------
cat("\n--- Required R Packages ---\n")

required_packages <- c(
    "shiny", "shinydashboard", "shinyWidgets", "shinyBS", "shinycssloaders",
    "tidyverse", "ggplot2", "lubridate", "scales", "readxl",
    "splines", "zoo", "DT", "rmarkdown", "knitr",
    "TTR", "forecast", "memoise", "data.table",
    "RColorBrewer", "viridis", "gridExtra", "corrplot",
    "quantmod", "PerformanceAnalytics", "xts", "tseries",
    "randomForest", "glmnet", "shinyjqui", "waiter",
    "htmltools", "blastula", "openxlsx", "base64enc",
    "webshot2", "htmlwidgets", "stringr", "officer", "flextable",
    "future", "promises", "magick", "shinyjs", "jsonlite",
    "ggrepel", "ggridges", "extrafont", "R6", "pdftools", "png"
)

missing_pkgs <- c()
for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        # only report passes for critical packages to keep output clean
    } else {
        missing_pkgs <- c(missing_pkgs, pkg)
        check_fail(sprintf("Package not installed: %s", pkg))
    }
}

if (length(missing_pkgs) == 0) {
    check_pass(sprintf("All %d required packages installed", length(required_packages)))
} else {
    cat(sprintf("\n  To install missing packages, run:\n"))
    cat(sprintf("    install.packages(c(%s))\n\n",
                paste(sprintf('"%s"', missing_pkgs), collapse = ", ")))
}

# ---------------------------------------------------------------------------
# 5. Font Availability Check
# ---------------------------------------------------------------------------
cat("\n--- Font Configuration ---\n")

if (requireNamespace("extrafont", quietly = TRUE)) {
    tryCatch({
        fonts <- extrafont::fonts()
        if (length(fonts) > 0) {
            check_pass(sprintf("extrafont: %d fonts registered", length(fonts)))
        } else {
            check_warn("extrafont loaded but no fonts registered (run extrafont::font_import() once)")
        }
    }, error = function(e) {
        check_warn(sprintf("extrafont error: %s (app will use default fonts)", e$message))
    })
} else {
    check_warn("extrafont not available (app will use default system fonts)")
}

# ---------------------------------------------------------------------------
# 6. R Version Check
# ---------------------------------------------------------------------------
cat("\n--- R Version ---\n")

r_ver <- getRversion()
cat(sprintf("  R version: %s\n", r_ver))
if (r_ver >= "4.0.0") {
    check_pass("R version >= 4.0.0")
} else {
    check_warn(sprintf("R version %s may be too old; recommend >= 4.0.0", r_ver))
}

cat(sprintf("  Platform:  %s\n", R.version$platform))
cat(sprintf("  OS:        %s\n", Sys.info()[["sysname"]]))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("\n================================================================\n")
cat(sprintf("  RESULTS: %d passed, %d warnings, %d failed\n", pass_count, warn_count, fail_count))
cat("================================================================\n")

if (fail_count > 0) {
    cat("\n  ACTION REQUIRED: Fix the FAIL items above before running the app.\n")
} else if (warn_count > 0) {
    cat("\n  App should work, but review WARN items for best experience.\n")
} else {
    cat("\n  All checks passed! Ready to launch the app.\n")
    cat("  Run:  Rscript -e \"source('bond-dashboard/run_fast.R')\"\n")
    cat("  Or double-click: run_app.bat\n")
}
cat("\n")
