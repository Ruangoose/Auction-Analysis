@echo off
REM ================================================================================
REM  Insele Capital Partners - Bond Analytics Dashboard Launcher
REM  Portable launcher: works regardless of where the project folder is placed
REM ================================================================================

REM Set working directory to the folder where this .bat file lives
REM This works whether double-clicked, run from a shortcut, or run from cmd
cd /d "%~dp0"

echo ========================================
echo  Insele Bond Analytics Dashboard
echo ========================================
echo.
echo  Project folder: %cd%
echo.

REM ============================================================================
REM  Auto-detect R installation
REM  Scans standard locations for the latest R version
REM ============================================================================

set "RSCRIPT="

REM Strategy 1: Check if Rscript is already on the system PATH
where Rscript >nul 2>&1
if %ERRORLEVEL%==0 (
    for /f "tokens=*" %%i in ('where Rscript') do (
        set "RSCRIPT=%%i"
        goto :found_r
    )
)

REM Strategy 2: Scan Program Files for R installations (newest first)
for /f "tokens=*" %%d in ('dir /b /o-n "C:\Program Files\R\R-*" 2^>nul') do (
    if exist "C:\Program Files\R\%%d\bin\Rscript.exe" (
        set "RSCRIPT=C:\Program Files\R\%%d\bin\Rscript.exe"
        goto :found_r
    )
)

REM Strategy 3: Scan Program Files (x86) for 32-bit R
for /f "tokens=*" %%d in ('dir /b /o-n "C:\Program Files (x86)\R\R-*" 2^>nul') do (
    if exist "C:\Program Files (x86)\R\%%d\bin\Rscript.exe" (
        set "RSCRIPT=C:\Program Files (x86)\R\%%d\bin\Rscript.exe"
        goto :found_r
    )
)

REM R not found
echo  ERROR: R installation not found!
echo.
echo  Please install R from: https://cran.r-project.org/bin/windows/base/
echo  Expected location: C:\Program Files\R\R-x.x.x\
echo.
pause
exit /b 1

:found_r
echo  R found: %RSCRIPT%
echo.

REM ============================================================================
REM  Pre-flight checks
REM ============================================================================

if not exist "bond-dashboard\enhanced_bond_server.R" (
    echo  ERROR: Cannot find bond-dashboard\enhanced_bond_server.R
    echo  Make sure this .bat file is in the project root folder.
    echo.
    pause
    exit /b 1
)

if not exist "data" (
    echo  WARNING: data\ folder not found. The app may not load correctly.
    echo.
)

REM ============================================================================
REM  Launch the Shiny application
REM ============================================================================

echo  Starting Bond Analytics Dashboard...
echo  The app will open in your default web browser.
echo  To stop the app, close this window or press Ctrl+C.
echo.
echo ========================================
echo.

"%RSCRIPT%" -e "source('bond-dashboard/run_fast.R')"

if %ERRORLEVEL% neq 0 (
    echo.
    echo  The application exited with an error.
    echo  Check the output above for details.
    echo.
    pause
)
