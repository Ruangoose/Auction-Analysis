#!/bin/bash
# ================================================================================
# Fast Bond Analytics Dashboard Launcher
# ================================================================================
# Usage:
#   ./run_app.sh          - Run in quiet mode (fast)
#   ./run_app.sh verbose  - Run with full output (debugging)
# ================================================================================

cd "$(dirname "$0")"

if [ "$1" = "verbose" ]; then
    echo "Starting in verbose mode..."
    Rscript -e "source('bond-dashboard/enhanced_bond_server.R'); shiny::runApp(shinyApp(ui=ui, server=server), port=3838, host='0.0.0.0')"
else
    echo "🚀 Starting Bond Analytics Dashboard (quiet mode)..."
    Rscript -e "suppressMessages(suppressWarnings(source('bond-dashboard/run_fast.R')))" 2>/dev/null
fi
