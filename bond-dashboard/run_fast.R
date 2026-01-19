# ================================================================================
# FAST STARTUP SCRIPT - Suppresses verbose console output
# ================================================================================
# Usage:
#   From R console:     source("bond-dashboard/run_fast.R")
#   From command line:  Rscript bond-dashboard/run_fast.R
#   From RStudio:       Click "Source" button on this file
# ================================================================================

# Set quiet mode BEFORE loading anything
options(
  warn = -1,                    # Suppress warnings
  shiny.launch.browser = TRUE,  # Auto-open browser
  shiny.autoreload = FALSE      # Disable auto-reload for speed
)

# Suppress all package startup messages
suppressPackageStartupMessages({
  suppressMessages({
    suppressWarnings({

      # Change to project root if needed
      if (file.exists("bond-dashboard/enhanced_bond_server.R")) {
        setwd(getwd())  # Already at root
      } else if (file.exists("enhanced_bond_server.R")) {
        setwd("..")  # In bond-dashboard folder
      }

      # Source the main app with all messages suppressed
      source("bond-dashboard/enhanced_bond_server.R", echo = FALSE, verbose = FALSE)

    })
  })
})

# Reset warning level to normal for the app
options(warn = 0)

# Launch the app
cat("\n🚀 Starting Bond Analytics Dashboard...\n\n")
shiny::runApp(
  shinyApp(ui = ui, server = server),
  port = 3838,
  host = "0.0.0.0",
  launch.browser = TRUE,
  quiet = TRUE  # Suppresses Shiny's own startup messages
)
