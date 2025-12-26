#!/usr/bin/env Rscript
# ==============================================================================
# 00_RUN_ALL.R - Master Script for Policy Learning Analysis Pipeline
# ==============================================================================
# Purpose: Execute the complete analysis pipeline from data prep to outputs
# Usage: Rscript 00_run_all.R [--skip-setup] [--skip-analysis] [--outputs-only]
# ==============================================================================

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
SKIP_SETUP <- "--skip-setup" %in% args
SKIP_ANALYSIS <- "--skip-analysis" %in% args
OUTPUTS_ONLY <- "--outputs-only" %in% args

if (OUTPUTS_ONLY) {
  SKIP_SETUP <- TRUE
  SKIP_ANALYSIS <- TRUE
}

# ==============================================================================
# HEADER
# ==============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                          ║\n")
cat("║   POLICY LEARNING ANALYSIS PIPELINE                                      ║\n")
cat("║   Do Direct Democracy Provisions Inform Voters?                          ║\n")
cat("║                                                                          ║\n")
cat("╚══════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("\n")

# Track overall timing
pipeline_start <- Sys.time()

# ==============================================================================
# STEP 0: LOAD CONFIGURATION
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("STEP 0: Loading configuration...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

source("00_config.R")

cat("  Project root:", PROJECT_ROOT, "\n")
cat("  Analysis period:", PARAMS$start_year, "-", PARAMS$end_year, "\n")
cat("  Random seed:", PARAMS$seed, "\n")
cat("\n")

# Create output directories
create_output_dirs()

# ==============================================================================
# STEP 1: DATA PREPARATION
# ==============================================================================

if (!SKIP_SETUP) {
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("STEP 1: Data Preparation\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  
  step1_start <- Sys.time()
  
  tryCatch({
    source("01_setup_data.R")
    step1_time <- difftime(Sys.time(), step1_start, units = "mins")
    cat("\n✓ Step 1 complete (", round(step1_time, 2), " minutes)\n\n")
  }, error = function(e) {
    cat("\n✗ Step 1 FAILED:", e$message, "\n")
    stop("Pipeline halted at Step 1")
  })
  
} else {
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("STEP 1: Data Preparation [SKIPPED]\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  
  # Verify data exists
  data_path <- file.path(PATHS$data_processed, OUTPUT_FILES$data$analysis_ready)
  if (!file.exists(data_path)) {
    stop("Cannot skip setup: ", OUTPUT_FILES$data$analysis_ready, " not found")
  }
  cat("  Using existing:", OUTPUT_FILES$data$analysis_ready, "\n\n")
}

# ==============================================================================
# STEP 2: MAIN ANALYSIS
# ==============================================================================

if (!SKIP_ANALYSIS) {
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("STEP 2: Main Difference-in-Differences Analysis\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  
  step2_start <- Sys.time()
  
  tryCatch({
    source("02_main_analysis.R")
    step2_time <- difftime(Sys.time(), step2_start, units = "mins")
    cat("\n✓ Step 2 complete (", round(step2_time, 2), " minutes)\n\n")
  }, error = function(e) {
    cat("\n✗ Step 2 FAILED:", e$message, "\n")
    stop("Pipeline halted at Step 2")
  })
  
} else {
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("STEP 2: Main Analysis [SKIPPED]\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  
  # Verify results exist
  results_path <- file.path(PATHS$data_processed, OUTPUT_FILES$data$all_results)
  if (!file.exists(results_path)) {
    stop("Cannot skip analysis: ", OUTPUT_FILES$data$all_results, " not found")
  }
  cat("  Using existing:", OUTPUT_FILES$data$all_results, "\n\n")
}

# ==============================================================================
# STEP 3: EXTENSIONS
# ==============================================================================

if (!SKIP_ANALYSIS) {
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("STEP 3: Extension Analyses\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  
  step3_start <- Sys.time()
  
  tryCatch({
    source("03_extensions.R")
    step3_time <- difftime(Sys.time(), step3_start, units = "mins")
    cat("\n✓ Step 3 complete (", round(step3_time, 2), " minutes)\n\n")
  }, error = function(e) {
    cat("\n✗ Step 3 FAILED:", e$message, "\n")
    cat("  Continuing to output generation...\n\n")
  })
  
} else {
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("STEP 3: Extensions [SKIPPED]\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n\n")
}

# ==============================================================================
# STEP 4: GENERATE OUTPUTS
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("STEP 4: Generate Publication Outputs\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

step4_start <- Sys.time()

tryCatch({
  source("04_generate_outputs.R")
  step4_time <- difftime(Sys.time(), step4_start, units = "mins")
  cat("\n✓ Step 4 complete (", round(step4_time, 2), " minutes)\n\n")
}, error = function(e) {
  cat("\n✗ Step 4 FAILED:", e$message, "\n")
  cat("  Some outputs may not have been generated.\n\n")
})

# ==============================================================================
# PIPELINE SUMMARY
# ==============================================================================

pipeline_time <- difftime(Sys.time(), pipeline_start, units = "mins")

cat("╔══════════════════════════════════════════════════════════════════════════╗\n")
cat("║                         PIPELINE COMPLETE                                ║\n")
cat("╚══════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("Total execution time:", round(pipeline_time, 2), "minutes\n")
cat("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("\n")

# ==============================================================================
# OUTPUT VERIFICATION
# ==============================================================================

cat("Output Verification:\n")
cat("───────────────────────────────────────────────────────────────────────────\n")

# Check data files
cat("\nData files:\n")
data_files <- c(
  OUTPUT_FILES$data$analysis_ready,
  OUTPUT_FILES$data$ballot_treatments,
  OUTPUT_FILES$data$all_results,
  OUTPUT_FILES$data$extension_results
)

for (f in data_files) {
  path <- file.path(PATHS$data_processed, f)
  status <- if (file.exists(path)) "✓" else "✗"
  cat("  ", status, f, "\n")
}

# Check main body tables
cat("\nMain body tables:\n")
for (f in OUTPUT_FILES$main_tables) {
  path <- file.path(PATHS$tables_main, f)
  status <- if (file.exists(path)) "✓" else "✗"
  cat("  ", status, f, "\n")
}

# Check figures
cat("\nFigures:\n")
for (f in OUTPUT_FILES$figures) {
  # Check multiple possible locations
  found <- FALSE
  for (dir in c(PATHS$figures_main, PATHS$extensions)) {
    if (file.exists(file.path(dir, f))) {
      found <- TRUE
      break
    }
  }
  status <- if (found) "✓" else "✗"
  cat("  ", status, f, "\n")
}

# Check appendix tables
cat("\nAppendix tables:\n")
for (f in OUTPUT_FILES$appendix_tables) {
  path <- file.path(PATHS$tables_appendix, f)
  status <- if (file.exists(path)) "✓" else "✗"
  cat("  ", status, f, "\n")
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("Ready to compile LaTeX document!\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
