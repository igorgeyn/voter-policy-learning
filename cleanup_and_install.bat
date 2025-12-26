@echo off
REM ==============================================================================
REM cleanup_and_install.bat
REM ==============================================================================
REM Purpose: Clean up the policy_learning code directory and install new pipeline
REM 
REM This script will:
REM   1. Create a timestamped archive folder
REM   2. Move ALL existing prod\ contents to archive (including subfolders)
REM   3. Move dev\, dep\, and root-level scripts to archive
REM   4. Install the new clean pipeline into prod\
REM   5. Flatten prod\ so there are no subfolders
REM
REM Usage: 
REM   Double-click this file, or run from Command Prompt
REM
REM ==============================================================================

setlocal EnableDelayedExpansion

REM ==============================================================================
REM CONFIGURATION
REM ==============================================================================

REM Base project directory (UPDATE THIS IF USING DIFFERENT PATH)
set "PROJECT_ROOT=G:\My Drive\Grad School\Research\policy_learning"

REM Alternative path if using C: drive
REM set "PROJECT_ROOT=C:\Users\igorgeyn\Google Drive\Grad School\Research\policy_learning"

REM Code directories
set "CODE_DIR=%PROJECT_ROOT%\code"
set "PROD_DIR=%CODE_DIR%\prod"
set "DEV_DIR=%CODE_DIR%\dev"
set "DEP_DIR=%CODE_DIR%\dep"

REM Archive location
set "ARCHIVE_BASE=%CODE_DIR%\archive"

REM Timestamp for this archive
for /f "tokens=2 delims==" %%I in ('wmic os get localdatetime /value') do set datetime=%%I
set "TIMESTAMP=%datetime:~0,8%_%datetime:~8,6%"
set "ARCHIVE_DIR=%ARCHIVE_BASE%\archive_%TIMESTAMP%"

REM New pipeline source (same directory as this script)
set "NEW_PIPELINE_DIR=%~dp0"

REM ==============================================================================
REM HEADER
REM ==============================================================================

echo.
echo ========================================================================
echo          POLICY LEARNING CODE CLEANUP ^& INSTALLATION SCRIPT
echo ========================================================================
echo.
echo Starting cleanup at %date% %time%
echo Project root: %PROJECT_ROOT%
echo.

REM ==============================================================================
REM PRE-FLIGHT CHECKS
REM ==============================================================================

if not exist "%PROJECT_ROOT%" (
    echo [ERROR] Project directory not found: %PROJECT_ROOT%
    echo [ERROR] Please update PROJECT_ROOT in this script.
    pause
    exit /b 1
)

if not exist "%CODE_DIR%" (
    echo [ERROR] Code directory not found: %CODE_DIR%
    pause
    exit /b 1
)

REM ==============================================================================
REM CONFIRMATION
REM ==============================================================================

echo This script will:
echo   1. Create archive at: %ARCHIVE_DIR%
echo   2. Move ALL files from prod\ to archive
echo   3. Move dev\ and dep\ folders to archive
echo   4. Move root-level scripts from code\ to archive
echo   5. Install new pipeline files to prod\
echo.
echo WARNING: This is a significant reorganization!
echo.
set /p CONFIRM="Do you want to proceed? (Y/N): "
if /i not "%CONFIRM%"=="Y" (
    echo Aborted by user.
    pause
    exit /b 0
)

echo.

REM ==============================================================================
REM STEP 1: CREATE ARCHIVE STRUCTURE
REM ==============================================================================

echo [INFO] Step 1: Creating archive directory structure...

mkdir "%ARCHIVE_DIR%\prod_old" 2>nul
mkdir "%ARCHIVE_DIR%\prod_old\subfolders" 2>nul
mkdir "%ARCHIVE_DIR%\dev" 2>nul
mkdir "%ARCHIVE_DIR%\dep" 2>nul
mkdir "%ARCHIVE_DIR%\code_root_scripts" 2>nul

echo [SUCCESS] Archive directory created: %ARCHIVE_DIR%

REM ==============================================================================
REM STEP 2: ARCHIVE PROD\ CONTENTS
REM ==============================================================================

echo [INFO] Step 2: Archiving prod\ contents...

if exist "%PROD_DIR%" (
    REM Move files (not directories)
    for %%F in ("%PROD_DIR%\*.*") do (
        move "%%F" "%ARCHIVE_DIR%\prod_old\" >nul 2>&1
        echo   Archived: %%~nxF
    )
    
    REM Move subdirectories
    for /d %%D in ("%PROD_DIR%\*") do (
        move "%%D" "%ARCHIVE_DIR%\prod_old\subfolders\" >nul 2>&1
        echo   Archived subfolder: %%~nxD
    )
    
    echo [SUCCESS] Archived all prod\ contents
) else (
    echo [WARNING] prod\ directory not found, creating it...
    mkdir "%PROD_DIR%" 2>nul
)

REM ==============================================================================
REM STEP 3: ARCHIVE DEV\ FOLDER
REM ==============================================================================

echo [INFO] Step 3: Archiving dev\ folder...

if exist "%DEV_DIR%" (
    xcopy "%DEV_DIR%\*" "%ARCHIVE_DIR%\dev\" /E /I /Q >nul 2>&1
    rmdir /S /Q "%DEV_DIR%" 2>nul
    echo [SUCCESS] Archived dev\ folder
) else (
    echo [WARNING] dev\ folder not found, skipping...
)

REM ==============================================================================
REM STEP 4: ARCHIVE DEP\ FOLDER
REM ==============================================================================

echo [INFO] Step 4: Archiving dep\ folder...

if exist "%DEP_DIR%" (
    xcopy "%DEP_DIR%\*" "%ARCHIVE_DIR%\dep\" /E /I /Q >nul 2>&1
    rmdir /S /Q "%DEP_DIR%" 2>nul
    echo [SUCCESS] Archived dep\ folder
) else (
    echo [WARNING] dep\ folder not found, skipping...
)

REM ==============================================================================
REM STEP 5: ARCHIVE ROOT-LEVEL SCRIPTS
REM ==============================================================================

echo [INFO] Step 5: Archiving root-level scripts from code\...

REM Move all .R files from code root
for %%F in ("%CODE_DIR%\*.R") do (
    move "%%F" "%ARCHIVE_DIR%\code_root_scripts\" >nul 2>&1
    echo   Archived: %%~nxF
)

REM Move captions_dump.json if it exists
if exist "%CODE_DIR%\captions_dump.json" (
    move "%CODE_DIR%\captions_dump.json" "%ARCHIVE_DIR%\code_root_scripts\" >nul 2>&1
    echo   Archived: captions_dump.json
)

echo [SUCCESS] Archived root-level scripts

REM ==============================================================================
REM STEP 6: INSTALL NEW PIPELINE
REM ==============================================================================

echo [INFO] Step 6: Installing new pipeline to prod\...

set "FILES_INSTALLED=0"

for %%F in (00_config.R 00_run_all.R 01_setup_data.R 02_main_analysis.R 03_extensions.R 04_generate_outputs.R README.md LATEX_TABLE_UPDATES.tex) do (
    if exist "%NEW_PIPELINE_DIR%\%%F" (
        copy "%NEW_PIPELINE_DIR%\%%F" "%PROD_DIR%\" >nul 2>&1
        echo   Installed: %%F
        set /a FILES_INSTALLED+=1
    ) else (
        echo   [WARNING] Not found: %%F
    )
)

echo [SUCCESS] Installed %FILES_INSTALLED% pipeline files to prod\

REM ==============================================================================
REM STEP 7: CREATE ARCHIVE MANIFEST
REM ==============================================================================

echo [INFO] Step 7: Creating archive manifest...

(
echo ================================================================================
echo POLICY LEARNING CODE ARCHIVE
echo ================================================================================
echo Created: %date% %time%
echo Archive ID: archive_%TIMESTAMP%
echo.
echo This archive contains the previous code structure before the consolidation
echo to a clean 6-file pipeline.
echo.
echo CONTENTS:
echo ---------
echo.
echo prod_old\
echo   - All .R scripts that were in the prod\ folder
echo   - subfolders\
echo     - for_dep\     : Files staged for deprecation
echo     - hold\        : Files on hold
echo     - validation\  : Validation scripts
echo     - voter_knowledge_paper\ : Related paper scripts
echo.
echo dev\
echo   - Development and exploratory scripts
echo.
echo dep\
echo   - Deprecated scripts
echo.
echo code_root_scripts\
echo   - Scripts that were in the code\ root directory
echo.
echo WHAT WAS INSTALLED:
echo -------------------
echo   - 00_config.R
echo   - 00_run_all.R
echo   - 01_setup_data.R
echo   - 02_main_analysis.R
echo   - 03_extensions.R
echo   - 04_generate_outputs.R
echo   - README.md
echo   - LATEX_TABLE_UPDATES.tex
echo.
echo ================================================================================
) > "%ARCHIVE_DIR%\MANIFEST.txt"

echo [SUCCESS] Created manifest

REM ==============================================================================
REM STEP 8: VERIFY INSTALLATION
REM ==============================================================================

echo.
echo [INFO] Step 8: Verifying installation...
echo.
echo Contents of prod\:
echo -------------------
dir /b "%PROD_DIR%"
echo.
echo Contents of archive\:
echo ----------------------
dir /b "%ARCHIVE_DIR%"

REM ==============================================================================
REM COMPLETION
REM ==============================================================================

echo.
echo ========================================================================
echo                         CLEANUP COMPLETE!
echo ========================================================================
echo.
echo [SUCCESS] New pipeline installed to: %PROD_DIR%
echo [SUCCESS] Old files archived to: %ARCHIVE_DIR%
echo.
echo Next steps:
echo   1. Open R and set working directory to prod\
echo   2. Run: source('00_run_all.R')
echo   3. Update your LaTeX file using LATEX_TABLE_UPDATES.tex
echo.
echo Completed at %date% %time%
echo.
pause
