@echo off
setlocal

:: Search for R.exe in the standard Windows installation path
for /d %%i in ("%ProgramFiles%\R\R-*") do (
    if exist "%%i\bin\R.exe" set R_PATH=%%i\bin\R.exe
)

if not defined R_PATH (
    echo Error: R was not found in %ProgramFiles%\R.
    echo Please ensure R is installed.
    pause
    exit /b
)

echo Starting CloudFlux...
"%R_PATH%" -e "CloudFlux::run_app()"

pause
