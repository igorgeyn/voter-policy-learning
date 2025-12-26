#!/usr/bin/env Rscript
# ==============================================================================
# 00_CONFIG.R - Centralized Configuration for Policy Learning Analysis
# ==============================================================================
# Purpose: Define all paths, parameters, and settings for the analysis pipeline
# Author: Igor Geyn
# Last Modified: 2025-12-26
# ==============================================================================

# ==============================================================================
# PATH CONFIGURATION
# ==============================================================================

# Detect platform and set base path
setup_project_paths <- function() {
  # Check for different possible base locations
  possible_bases <- c(
    # macOS Google Drive
    "~/Library/CloudStorage/GoogleDrive-igorgeyn@gmail.com/My Drive/Grad School/Research/policy_learning",
    # Windows Google Drive (G: drive)
    "G:/My Drive/Grad School/Research/policy_learning",
    # Alternative Windows path
    "C:/Users/igorgeyn/Google Drive/Grad School/Research/policy_learning",
    # Fallback to current directory
    getwd()
  )
  
  for (base in possible_bases) {
    expanded <- path.expand(base)
    if (dir.exists(expanded)) {
      return(normalizePath(expanded))
    }
  }
  
  stop("Could not find project directory. Please set PROJECT_ROOT manually.")
}

# Set project root
PROJECT_ROOT <- setup_project_paths()

# Define all paths as a list for easy reference
PATHS <- list(
  # Root
  root = PROJECT_ROOT,
  

  # Data directories
  data = file.path(PROJECT_ROOT, "data"),
  data_raw = file.path(PROJECT_ROOT, "data"),
  data_processed = file.path(PROJECT_ROOT, "output", "data"),
  
  # Specific data files
  ces_cumulative = file.path(PROJECT_ROOT, "data", "cumulative_2006-2024.rds"),
  ballot_measures = file.path(PROJECT_ROOT, "data", "cleaned", "ballot_measures_combined.csv"),
  
  # Output directories
  output = file.path(PROJECT_ROOT, "output"),
  tables = file.path(PROJECT_ROOT, "output", "tables"),
  tables_main = file.path(PROJECT_ROOT, "output", "tables", "all_years"),
  tables_appendix = file.path(PROJECT_ROOT, "output", "tables", "appendix"),
  figures = file.path(PROJECT_ROOT, "output", "figures"),
  figures_main = file.path(PROJECT_ROOT, "output", "figures", "all_years"),
  extensions = file.path(PROJECT_ROOT, "output", "extensions", "morality_politics"),
  
  # Code directory
  code = file.path(PROJECT_ROOT, "code")
)

# Create directories if they don't exist
create_output_dirs <- function() {
  dirs_to_create <- c(
    PATHS$data_processed,
    PATHS$tables,
    PATHS$tables_main,
    PATHS$tables_appendix,
    PATHS$figures,
    PATHS$figures_main,
    PATHS$extensions
  )
  
  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      message("Created directory: ", dir)
    }
  }
}

# ==============================================================================
# ANALYSIS PARAMETERS
# ==============================================================================

PARAMS <- list(
  # Time period
  start_year = 2006,
  end_year = 2020,
  
  # States to exclude
  excluded_states = c("Alaska", "Hawaii"),  # Non-continental
  
  # Morality politics keywords for classification
  morality_keywords = list(
    marijuana = c("marijuana", "cannabis", "marihuana", "pot ", "weed"),
    gambling = c("gambling", "casino", "lottery", "slots", "gaming"),
    abortion = c("abortion", "pro-life", "pro-choice", "unborn", "fetus", "fetal"),
    marriage = c("marriage", "same-sex", "gay marriage", "civil union", "domestic partner")
  ),
  
  # Control variables available in CES
  control_vars = c("age", "age_squared", "female", "college", 
                   "democrat", "republican", "independent"),
  
  # Clustering
  cluster_var = "state",
  
  # Significance levels
  alpha = 0.05,
  alpha_marginal = 0.10,
  
  # Bootstrap replications
  n_bootstrap = 999,
  n_placebo = 1000,
  
  # Random seed for reproducibility
  seed = 20251226
)

# ==============================================================================
# OUTPUT FILE SPECIFICATIONS
# ==============================================================================

# Main figures required by LaTeX
OUTPUT_FILES <- list(
  figures = list(
    knowledge_comparison = "knowledge_vs_interest_comparison.png",
    parallel_trends_cohort = "parallel_trends_by_cohort.pdf",
    parallel_trends_event = "parallel_trends_event_study.pdf",
    event_study_morality = "event_study_morality.pdf",
    heterogeneity_demographics = "heterogeneity_demographics_twfe.pdf",
    awareness_effects = "awareness_effects_morality.pdf"
  ),
  
  # Main body tables (THESE REPLACE HARDCODED LATEX!)
  main_tables = list(
    treatment_comparison = "table1_treatment_comparison.tex",
    estimator_comparison = "table2_estimator_comparison.tex",
    bacon_decomposition = "table3_bacon_decomposition.tex",
    timing_heterogeneity = "table4_timing_heterogeneity.tex",
    topic_effects = "table5_topic_effects.tex",
    robustness = "table6_robustness.tex"
  ),
  
  # Appendix tables
  appendix_tables = list(
    a1_full_specs = "table_a1_full_specifications.tex",
    a2_sun_abraham = "table_a2_sun_abraham.tex",
    a3_parallel_trends = "table_a3_parallel_trends.tex",
    a5_heterogeneity = "table_a5_heterogeneity.tex",
    a6_leave_one_out = "table_a6_leave_one_out.tex",
    a7_topics = "table_a7_topics.tex",
    a8_summary_stats = "table_a8_summary_stats.tex",
    a9_sample_selection = "table_a9_sample_selection.tex",
    a10_variables = "table_a10_variables.tex",
    a11_bootstrap = "table_a11_bootstrap.tex",
    a12_timing = "table_a12_timing.tex"
  ),
  
  # Intermediate data files
  data = list(
    analysis_ready = "analysis_ready_all_years.rds",
    ballot_treatments = "ballot_treatments_all_years.rds",
    all_results = "all_results_morality_politics.rds",
    extension_results = "extension_results_morality_complete.rds"
  )
)

# ==============================================================================
# PACKAGE MANAGEMENT
# ==============================================================================

# Required packages
REQUIRED_PACKAGES <- c(
  # Data manipulation
  "tidyverse", "data.table", "haven",
  # Fixed effects and DiD

  "fixest", "did", "bacondecomp",
  # Tables
  "modelsummary", "kableExtra", "xtable",
  # Visualization
  "ggplot2", "patchwork", "viridis", "scales",
  # Utilities
  "broom", "sandwich", "lmtest"
)

# Function to load all packages
load_packages <- function(packages = REQUIRED_PACKAGES) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing package: ", pkg)
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  message("All packages loaded successfully")
}

# ==============================================================================
# PLOTTING THEME
# ==============================================================================

# Publication-quality theme
theme_publication <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      # Text
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle = element_text(color = "gray40", size = base_size, hjust = 0),
      plot.caption = element_text(hjust = 0, size = base_size - 2, color = "gray50"),
      
      # Axes
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, color = "black"),
      
      # Grid
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      
      # Legend
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = base_size - 1),
      legend.text = element_text(size = base_size - 1),
      
      # Facets
      strip.text = element_text(face = "bold", size = base_size),
      
      # Margins
      plot.margin = margin(10, 10, 10, 10)
    )
}

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# Logging function
log_msg <- function(..., level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste0("[", timestamp, "] [", level, "] ", paste(..., collapse = " "))
  message(msg)
}

# Formatted number display
fmt_num <- function(x, digits = 4) {
  format(round(x, digits), nsmall = digits)
}

# Formatted p-value
fmt_pval <- function(p, digits = 3) {
  if (p < 0.001) return("< 0.001")
  format(round(p, digits), nsmall = digits)
}

# Calculate significance stars
sig_stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

# Save figure helper
save_figure <- function(plot, filename, path = PATHS$figures_main, 
                        width = 8, height = 6, device = NULL) {
  full_path <- file.path(path, filename)
  
  # Determine device from extension if not specified
  if (is.null(device)) {
    ext <- tools::file_ext(filename)
    device <- if (ext == "pdf") "pdf" else if (ext %in% c("png", "jpg")) ext else "png"
  }
  
  ggsave(full_path, plot, width = width, height = height, device = device, dpi = 300)
  log_msg("Saved figure:", full_path)
}

# ==============================================================================
# INITIALIZATION
# ==============================================================================

# Print configuration summary when sourced
print_config_summary <- function() {
  cat("\n")
  cat("==============================================================================\n")
  cat("POLICY LEARNING ANALYSIS - Configuration Loaded\n")
  cat("==============================================================================\n")
  cat("Project root:", PROJECT_ROOT, "\n")
  cat("Analysis period:", PARAMS$start_year, "-", PARAMS$end_year, "\n")
  cat("Random seed:", PARAMS$seed, "\n")
  cat("==============================================================================\n")
  cat("\n")
}

# Run initialization
if (interactive()) {
  print_config_summary()
}
