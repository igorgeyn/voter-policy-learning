#!/bin/bash
# ==============================================================================
# cleanup_and_install.sh
# ==============================================================================
# Purpose: Clean up the policy_learning code directory and install new pipeline
# 
# This script will:
#   1. Create a timestamped archive folder
#   2. Move ALL existing prod/ contents to archive (including subfolders)
#   3. Move dev/, dep/, and root-level scripts to archive
#   4. Install the new clean pipeline into prod/
#   5. Flatten prod/ so there are no subfolders
#
# Usage: 
#   chmod +x cleanup_and_install.sh
#   ./cleanup_and_install.sh
#
# ==============================================================================

set -e  # Exit on error

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Base project directory
PROJECT_ROOT="/Users/igorgeyn/Library/CloudStorage/GoogleDrive-igorgeyn@gmail.com/My Drive/Grad School/Research/policy_learning"

# Code directories
CODE_DIR="$PROJECT_ROOT/code"
PROD_DIR="$CODE_DIR/prod"
DEV_DIR="$CODE_DIR/dev"
DEP_DIR="$CODE_DIR/dep"

# Archive location (inside code directory)
ARCHIVE_BASE="$CODE_DIR/archive"

# Timestamp for this archive
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
ARCHIVE_DIR="$ARCHIVE_BASE/archive_$TIMESTAMP"

# New pipeline source (update this path to where you downloaded the zip)
# If running from the same directory as the extracted pipeline:
NEW_PIPELINE_DIR="$(dirname "$0")"

# ==============================================================================
# COLORS FOR OUTPUT
# ==============================================================================

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# ==============================================================================
# PRE-FLIGHT CHECKS
# ==============================================================================

echo ""
echo "╔══════════════════════════════════════════════════════════════════════════╗"
echo "║          POLICY LEARNING CODE CLEANUP & INSTALLATION SCRIPT              ║"
echo "╚══════════════════════════════════════════════════════════════════════════╝"
echo ""

log_info "Starting cleanup at $(date)"
log_info "Project root: $PROJECT_ROOT"
echo ""

# Check if project directory exists
if [ ! -d "$PROJECT_ROOT" ]; then
    log_error "Project directory not found: $PROJECT_ROOT"
    log_error "Please update PROJECT_ROOT in this script."
    exit 1
fi

# Check if code directory exists
if [ ! -d "$CODE_DIR" ]; then
    log_error "Code directory not found: $CODE_DIR"
    exit 1
fi

# Check if new pipeline files exist
if [ ! -f "$NEW_PIPELINE_DIR/00_config.R" ]; then
    log_warn "New pipeline files not found in: $NEW_PIPELINE_DIR"
    log_warn "Please ensure this script is in the same directory as the new pipeline files,"
    log_warn "or update NEW_PIPELINE_DIR in this script."
    echo ""
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# ==============================================================================
# CONFIRMATION
# ==============================================================================

echo "This script will:"
echo "  1. Create archive at: $ARCHIVE_DIR"
echo "  2. Move ALL files from prod/ to archive"
echo "  3. Move dev/ and dep/ folders to archive"
echo "  4. Move root-level scripts from code/ to archive"
echo "  5. Install new pipeline files to prod/"
echo ""
echo -e "${YELLOW}This is a significant reorganization!${NC}"
echo ""
read -p "Do you want to proceed? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    log_info "Aborted by user."
    exit 0
fi

echo ""

# ==============================================================================
# STEP 1: CREATE ARCHIVE STRUCTURE
# ==============================================================================

log_info "Step 1: Creating archive directory structure..."

mkdir -p "$ARCHIVE_DIR/prod_old"
mkdir -p "$ARCHIVE_DIR/prod_old/subfolders"
mkdir -p "$ARCHIVE_DIR/dev"
mkdir -p "$ARCHIVE_DIR/dep"
mkdir -p "$ARCHIVE_DIR/code_root_scripts"

log_success "Archive directory created: $ARCHIVE_DIR"

# ==============================================================================
# STEP 2: ARCHIVE PROD/ CONTENTS
# ==============================================================================

log_info "Step 2: Archiving prod/ contents..."

if [ -d "$PROD_DIR" ]; then
    # Count items
    file_count=$(find "$PROD_DIR" -maxdepth 1 -type f | wc -l | tr -d ' ')
    folder_count=$(find "$PROD_DIR" -maxdepth 1 -type d ! -path "$PROD_DIR" | wc -l | tr -d ' ')
    
    log_info "  Found $file_count files and $folder_count subfolders in prod/"
    
    # Move files (not directories) to archive
    for file in "$PROD_DIR"/*; do
        if [ -f "$file" ]; then
            mv "$file" "$ARCHIVE_DIR/prod_old/"
        fi
    done
    
    # Move subdirectories to archive/prod_old/subfolders/
    for dir in "$PROD_DIR"/*/; do
        if [ -d "$dir" ]; then
            dirname=$(basename "$dir")
            mv "$dir" "$ARCHIVE_DIR/prod_old/subfolders/"
            log_info "  Archived subfolder: $dirname"
        fi
    done
    
    log_success "Archived all prod/ contents"
else
    log_warn "prod/ directory not found, creating it..."
    mkdir -p "$PROD_DIR"
fi

# ==============================================================================
# STEP 3: ARCHIVE DEV/ FOLDER
# ==============================================================================

log_info "Step 3: Archiving dev/ folder..."

if [ -d "$DEV_DIR" ]; then
    # Count items
    dev_count=$(find "$DEV_DIR" -type f | wc -l | tr -d ' ')
    log_info "  Found $dev_count files in dev/"
    
    mv "$DEV_DIR"/* "$ARCHIVE_DIR/dev/" 2>/dev/null || true
    rmdir "$DEV_DIR" 2>/dev/null || rm -rf "$DEV_DIR"
    
    log_success "Archived dev/ folder"
else
    log_warn "dev/ folder not found, skipping..."
fi

# ==============================================================================
# STEP 4: ARCHIVE DEP/ FOLDER
# ==============================================================================

log_info "Step 4: Archiving dep/ folder..."

if [ -d "$DEP_DIR" ]; then
    dep_count=$(find "$DEP_DIR" -type f | wc -l | tr -d ' ')
    log_info "  Found $dep_count files in dep/"
    
    mv "$DEP_DIR"/* "$ARCHIVE_DIR/dep/" 2>/dev/null || true
    rmdir "$DEP_DIR" 2>/dev/null || rm -rf "$DEP_DIR"
    
    log_success "Archived dep/ folder"
else
    log_warn "dep/ folder not found, skipping..."
fi

# ==============================================================================
# STEP 5: ARCHIVE ROOT-LEVEL SCRIPTS
# ==============================================================================

log_info "Step 5: Archiving root-level scripts from code/..."

# List of known root-level scripts to archive
root_scripts=(
    "analysis_did_using_cces.R"
    "analysis_knowledge_measures_cces.R"
    "analysis_prep.R"
    "ballot_measures_explore.R"
    "cces_read_merge_analyze.R"
    "did_robustness.R"
    "captions_dump.json"
)

archived_count=0
for script in "${root_scripts[@]}"; do
    if [ -f "$CODE_DIR/$script" ]; then
        mv "$CODE_DIR/$script" "$ARCHIVE_DIR/code_root_scripts/"
        log_info "  Archived: $script"
        ((archived_count++))
    fi
done

# Also move any other .R files in code/ root
for file in "$CODE_DIR"/*.R; do
    if [ -f "$file" ]; then
        filename=$(basename "$file")
        mv "$file" "$ARCHIVE_DIR/code_root_scripts/"
        log_info "  Archived: $filename"
        ((archived_count++))
    fi
done

log_success "Archived $archived_count root-level scripts"

# ==============================================================================
# STEP 6: INSTALL NEW PIPELINE
# ==============================================================================

log_info "Step 6: Installing new pipeline to prod/..."

# List of new pipeline files
new_files=(
    "00_config.R"
    "00_run_all.R"
    "01_setup_data.R"
    "02_main_analysis.R"
    "03_extensions.R"
    "04_generate_outputs.R"
    "README.md"
    "LATEX_TABLE_UPDATES.tex"
)

installed_count=0
for file in "${new_files[@]}"; do
    if [ -f "$NEW_PIPELINE_DIR/$file" ]; then
        cp "$NEW_PIPELINE_DIR/$file" "$PROD_DIR/"
        log_info "  Installed: $file"
        ((installed_count++))
    else
        log_warn "  Not found: $file"
    fi
done

log_success "Installed $installed_count pipeline files to prod/"

# ==============================================================================
# STEP 7: CREATE ARCHIVE MANIFEST
# ==============================================================================

log_info "Step 7: Creating archive manifest..."

manifest_file="$ARCHIVE_DIR/MANIFEST.txt"

cat > "$manifest_file" << EOF
================================================================================
POLICY LEARNING CODE ARCHIVE
================================================================================
Created: $(date)
Archive ID: archive_$TIMESTAMP

This archive contains the previous code structure before the consolidation
to a clean 6-file pipeline.

CONTENTS:
---------

prod_old/
  - All .R scripts that were in the prod/ folder
  - subfolders/
    - for_dep/     : Files staged for deprecation
    - hold/        : Files on hold
    - validation/  : Validation scripts
    - voter_knowledge_paper/ : Related paper scripts

dev/
  - Development and exploratory scripts
  - Includes: redivis_l2_data/, scrape/, wholesale_attempt_Aug2025/

dep/
  - Deprecated scripts

code_root_scripts/
  - Scripts that were in the code/ root directory

WHAT WAS INSTALLED:
-------------------
The following files were installed to prod/:
$(for f in "${new_files[@]}"; do echo "  - $f"; done)

TO RESTORE:
-----------
If you need to restore the old structure:
  cp -r "$ARCHIVE_DIR/prod_old/"* "$PROD_DIR/"
  cp -r "$ARCHIVE_DIR/dev" "$CODE_DIR/"
  cp -r "$ARCHIVE_DIR/dep" "$CODE_DIR/"
  cp "$ARCHIVE_DIR/code_root_scripts/"* "$CODE_DIR/"

================================================================================
EOF

log_success "Created manifest at: $manifest_file"

# ==============================================================================
# STEP 8: VERIFY INSTALLATION
# ==============================================================================

log_info "Step 8: Verifying installation..."

echo ""
echo "Contents of prod/:"
echo "-------------------"
ls -la "$PROD_DIR"

echo ""
echo "Contents of archive:"
echo "--------------------"
ls -la "$ARCHIVE_DIR"

# ==============================================================================
# COMPLETION
# ==============================================================================

echo ""
echo "╔══════════════════════════════════════════════════════════════════════════╗"
echo "║                         CLEANUP COMPLETE!                                ║"
echo "╚══════════════════════════════════════════════════════════════════════════╝"
echo ""
log_success "New pipeline installed to: $PROD_DIR"
log_success "Old files archived to: $ARCHIVE_DIR"
echo ""
echo "Next steps:"
echo "  1. cd \"$PROD_DIR\""
echo "  2. Open R and run: source('00_run_all.R')"
echo "  3. Update your LaTeX file using LATEX_TABLE_UPDATES.tex"
echo ""
log_info "Completed at $(date)"
