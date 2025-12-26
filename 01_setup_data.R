#!/usr/bin/env Rscript
# ==============================================================================
# 01_SETUP_DATA.R - Data Preparation for Policy Learning Analysis
# ==============================================================================
# Purpose: Load CES and ballot measure data, create analysis-ready dataset
# Input: CES cumulative file, NCSL/CEDA ballot measures
# Output: analysis_ready_all_years.rds, ballot_treatments_all_years.rds
# ==============================================================================

# Source configuration
source("00_config.R")

# Load packages
load_packages()

# Set seed
set.seed(PARAMS$seed)

log_msg("=" |> rep(70) |> paste(collapse = ""))
log_msg("STEP 1: DATA PREPARATION")
log_msg("=" |> rep(70) |> paste(collapse = ""))

# ==============================================================================
# SECTION 1: LOAD CES DATA
# ==============================================================================

log_msg("Loading CES cumulative data...")

# Check if CES file exists
if (!file.exists(PATHS$ces_cumulative)) {
  stop("CES data file not found at: ", PATHS$ces_cumulative)
}

ces_raw <- readRDS(PATHS$ces_cumulative)

log_msg("  Raw CES observations:", format(nrow(ces_raw), big.mark = ","))
log_msg("  Years in data:", paste(range(ces_raw$year, na.rm = TRUE), collapse = "-"))

# Track sample selection
sample_tracker <- data.frame(
  Step = character(),
  N = integer(),
  Dropped = integer(),
  stringsAsFactors = FALSE
)

add_sample_step <- function(tracker, step_name, current_n, prev_n = NA) {
  dropped <- if (is.na(prev_n)) 0 else prev_n - current_n
  rbind(tracker, data.frame(Step = step_name, N = current_n, Dropped = dropped))
}

sample_tracker <- add_sample_step(sample_tracker, "Raw CES data", nrow(ces_raw))

# ==============================================================================
# SECTION 2: CLEAN CES DATA
# ==============================================================================

log_msg("Cleaning CES data...")

ces <- ces_raw %>%
  # Filter to study period
  filter(year >= PARAMS$start_year, year <= PARAMS$end_year) %>%
  # Remove excluded states
  filter(!state %in% PARAMS$excluded_states) %>%
  filter(!is.na(state))

prev_n <- nrow(ces_raw)
sample_tracker <- add_sample_step(sample_tracker, "Filter to 2006-2020, continental US", nrow(ces), prev_n)

# Standardize state names
ces <- ces %>%
  mutate(
    state = case_when(
      # Handle any state name inconsistencies
      state == "District of Columbia" ~ NA_character_,
      TRUE ~ state
    )
  ) %>%
  filter(!is.na(state))

prev_n <- sample_tracker$N[nrow(sample_tracker)]
sample_tracker <- add_sample_step(sample_tracker, "Remove DC", nrow(ces), prev_n)

# Create outcome variable: news interest
# CES variable typically named 'newsint' or similar
ces <- ces %>%
  mutate(
    # Try to find news interest variable (naming varies by year)
    news_interest_score = case_when(
      "newsint" %in% names(.) ~ newsint,
      "CC316" %in% names(.) ~ CC316,
      "news_interest" %in% names(.) ~ news_interest,
      TRUE ~ NA_real_
    )
  ) %>%
  # Ensure 1-4 scale
  mutate(
    news_interest_score = case_when(
      news_interest_score %in% 1:4 ~ as.numeric(news_interest_score),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(news_interest_score))

prev_n <- sample_tracker$N[nrow(sample_tracker)]
sample_tracker <- add_sample_step(sample_tracker, "Non-missing news interest", nrow(ces), prev_n)

# Create demographic control variables
ces <- ces %>%
  mutate(
    # Age
    age = case_when(
      "age" %in% names(.) ~ as.numeric(age),
      "birthyr" %in% names(.) ~ year - birthyr,
      TRUE ~ NA_real_
    ),
    age = ifelse(age >= 18 & age <= 100, age, NA),
    age_squared = age^2,
    
    # Gender
    female = case_when(
      "gender" %in% names(.) ~ as.numeric(gender == 2),
      "female" %in% names(.) ~ as.numeric(female),
      TRUE ~ NA_real_
    ),
    
    # Education (college indicator)
    college = case_when(
      "educ" %in% names(.) ~ as.numeric(educ >= 5),  # Typically 5 = 4-year degree
      "education" %in% names(.) ~ as.numeric(education >= 5),
      TRUE ~ NA_real_
    ),
    
    # Party ID
    democrat = case_when(
      "pid3" %in% names(.) ~ as.numeric(pid3 == 1),
      "pid7" %in% names(.) ~ as.numeric(pid7 %in% 1:3),
      TRUE ~ NA_real_
    ),
    republican = case_when(
      "pid3" %in% names(.) ~ as.numeric(pid3 == 2),
      "pid7" %in% names(.) ~ as.numeric(pid7 %in% 5:7),
      TRUE ~ NA_real_
    ),
    independent = case_when(
      "pid3" %in% names(.) ~ as.numeric(pid3 == 3),
      "pid7" %in% names(.) ~ as.numeric(pid7 == 4),
      TRUE ~ NA_real_
    ),
    
    # Age groups for heterogeneity
    age_group = case_when(
      age < 35 ~ "young",
      age >= 35 & age < 65 ~ "middle",
      age >= 65 ~ "old",
      TRUE ~ NA_character_
    ),
    young = as.numeric(age_group == "young"),
    old = as.numeric(age_group == "old")
  )

log_msg("  CES after cleaning:", format(nrow(ces), big.mark = ","), "observations")

# ==============================================================================
# SECTION 3: LOAD BALLOT MEASURE DATA
# ==============================================================================

log_msg("Loading ballot measure data...")

if (!file.exists(PATHS$ballot_measures)) {
  stop("Ballot measure data not found at: ", PATHS$ballot_measures)
}

ballot_raw <- read_csv(PATHS$ballot_measures, show_col_types = FALSE)

log_msg("  Raw ballot measures:", format(nrow(ballot_raw), big.mark = ","))

# Filter to study period
ballot <- ballot_raw %>%
  filter(year >= PARAMS$start_year, year <= PARAMS$end_year)

log_msg("  Ballot measures in study period:", format(nrow(ballot), big.mark = ","))

# ==============================================================================
# SECTION 4: CLASSIFY MORALITY POLITICS MEASURES
# ==============================================================================

log_msg("Classifying morality politics measures...")

# Function to detect keywords in text
detect_topic <- function(text, keywords) {
  if (is.na(text)) return(FALSE)
  text_lower <- tolower(text)
  any(sapply(keywords, function(k) grepl(k, text_lower, fixed = FALSE)))
}

# Apply classification
ballot <- ballot %>%
  rowwise() %>%
  mutate(
    # Get text field (may be named differently)
    measure_text = coalesce(
      if ("ballot_title" %in% names(.)) ballot_title else NA_character_,
      if ("description" %in% names(.)) description else NA_character_,
      if ("measure_text" %in% names(.)) measure_text else NA_character_,
      ""
    ),
    
    # Classify by topic
    is_marijuana = detect_topic(measure_text, PARAMS$morality_keywords$marijuana),
    is_gambling = detect_topic(measure_text, PARAMS$morality_keywords$gambling),
    is_abortion = detect_topic(measure_text, PARAMS$morality_keywords$abortion),
    is_marriage = detect_topic(measure_text, PARAMS$morality_keywords$marriage),
    
    # Overall morality politics indicator
    is_morality = is_marijuana | is_gambling | is_abortion | is_marriage
  ) %>%
  ungroup()

# Summary of classification
morality_summary <- ballot %>%
  summarise(
    total = n(),
    marijuana = sum(is_marijuana),
    gambling = sum(is_gambling),
    abortion = sum(is_abortion),
    marriage = sum(is_marriage),
    any_morality = sum(is_morality)
  )

log_msg("  Morality politics measures:")
log_msg("    Marijuana:", morality_summary$marijuana)
log_msg("    Gambling:", morality_summary$gambling)
log_msg("    Abortion:", morality_summary$abortion)
log_msg("    Marriage:", morality_summary$marriage)
log_msg("    Total morality:", morality_summary$any_morality)

# ==============================================================================
# SECTION 5: CREATE STATE-YEAR TREATMENT INDICATORS
# ==============================================================================

log_msg("Creating treatment indicators...")
# Aggregate to state-year level
state_year_treatments <- ballot %>%
  group_by(state, year) %>%
  summarise(
    # Count of measures
    n_measures = n(),
    n_morality = sum(is_morality),
    n_marijuana = sum(is_marijuana),
    n_gambling = sum(is_gambling),
    n_abortion = sum(is_abortion),
    n_marriage = sum(is_marriage),
    
    # Binary indicators
    has_any_measure = n_measures > 0,
    has_morality = n_morality > 0,
    has_marijuana = n_marijuana > 0,
    has_gambling = n_gambling > 0,
    has_abortion = n_abortion > 0,
    has_marriage = n_marriage > 0,
    
    .groups = "drop"
  )

# Fill in state-years with no measures
all_state_years <- expand.grid(
  state = unique(ces$state),
  year = PARAMS$start_year:PARAMS$end_year,
  stringsAsFactors = FALSE
)

state_year_treatments <- all_state_years %>%
  left_join(state_year_treatments, by = c("state", "year")) %>%
  mutate(across(starts_with("n_"), ~replace_na(., 0))) %>%
  mutate(across(starts_with("has_"), ~replace_na(., FALSE)))

# Calculate treatment timing for each state
treatment_timing <- state_year_treatments %>%
  group_by(state) %>%
  summarise(
    # First year of any treatment
    first_any_measure = if (any(has_any_measure)) min(year[has_any_measure]) else Inf,
    first_morality = if (any(has_morality)) min(year[has_morality]) else Inf,
    first_marijuana = if (any(has_marijuana)) min(year[has_marijuana]) else Inf,
    first_gambling = if (any(has_gambling)) min(year[has_gambling]) else Inf,
    first_abortion = if (any(has_abortion)) min(year[has_abortion]) else Inf,
    first_marriage = if (any(has_marriage)) min(year[has_marriage]) else Inf,
    
    # Ever treated indicators
    ever_any_measure = any(has_any_measure),
    ever_morality = any(has_morality),
    ever_marijuana = any(has_marijuana),
    .groups = "drop"
  )

# Add timing to state-year data
state_year_treatments <- state_year_treatments %>%
  left_join(treatment_timing, by = "state") %>%
  mutate(
    # DiD treatment indicators (post-treatment × ever-treated)
    did_any = has_any_measure & (year >= first_any_measure),
    did_morality = has_morality | (ever_morality & year >= first_morality),
    did_marijuana = has_marijuana | (ever_marijuana & year >= first_marijuana),
    
    # Intensity measures
    did_intensity_any = ifelse(year >= first_any_measure, n_measures, 0),
    did_intensity_morality = ifelse(year >= first_morality, n_morality, 0),
    
    # Relative time to treatment (for event study)
    rel_time_morality = ifelse(is.finite(first_morality), year - first_morality, NA),
    
    # Cohort (for CS/SA estimators)
    cohort_morality = ifelse(is.finite(first_morality), first_morality, 0)
  )

# Count never-treated states
never_treated_any <- sum(!treatment_timing$ever_any_measure)
never_treated_morality <- sum(!treatment_timing$ever_morality)

log_msg("  Never-treated states (any measure):", never_treated_any)
log_msg("  Never-treated states (morality):", never_treated_morality)

# ==============================================================================
# SECTION 6: MERGE CES WITH TREATMENTS
# ==============================================================================

log_msg("Merging CES with treatment data...")

analysis_df <- ces %>%
  left_join(state_year_treatments, by = c("state", "year"))

log_msg("  Merged dataset:", format(nrow(analysis_df), big.mark = ","), "observations")

# Create final treatment variables with clear names
analysis_df <- analysis_df %>%
  mutate(
    # Primary treatment: morality politics
    did_treatment = as.numeric(did_morality),
    treatment_group = as.numeric(ever_morality),
    post_election = as.numeric(year >= first_morality),
    
    # For TWFE
    state_first_treat = first_morality,
    
    # Topic-specific treatments
    did_marijuana = as.numeric(did_marijuana),
    did_gambling = as.numeric(has_gambling),
    did_abortion = as.numeric(has_abortion),
    did_marriage = as.numeric(has_marriage),
    
    # Intensity
    did_intensity = did_intensity_morality,
    
    # High salience indicator (multiple morality measures)
    did_high_salience = as.numeric(n_morality >= 2),
    did_very_high_salience = as.numeric(n_morality >= 3)
  )

prev_n <- sample_tracker$N[nrow(sample_tracker)]
sample_tracker <- add_sample_step(sample_tracker, "Final analysis sample", nrow(analysis_df), prev_n)

# ==============================================================================
# SECTION 7: SUMMARY STATISTICS
# ==============================================================================

log_msg("Generating summary statistics...")

# Overall summary
summary_stats <- analysis_df %>%
  summarise(
    n_obs = n(),
    n_states = n_distinct(state),
    n_years = n_distinct(year),
    
    # Outcome
    mean_news_interest = mean(news_interest_score, na.rm = TRUE),
    sd_news_interest = sd(news_interest_score, na.rm = TRUE),
    
    # Treatment
    pct_treated = mean(did_treatment, na.rm = TRUE) * 100,
    n_treated_obs = sum(did_treatment, na.rm = TRUE),
    
    # Demographics
    mean_age = mean(age, na.rm = TRUE),
    pct_female = mean(female, na.rm = TRUE) * 100,
    pct_college = mean(college, na.rm = TRUE) * 100,
    pct_democrat = mean(democrat, na.rm = TRUE) * 100,
    pct_republican = mean(republican, na.rm = TRUE) * 100
  )

log_msg("Summary Statistics:")
log_msg("  Observations:", format(summary_stats$n_obs, big.mark = ","))
log_msg("  States:", summary_stats$n_states)
log_msg("  Years:", summary_stats$n_years)
log_msg("  Mean news interest:", round(summary_stats$mean_news_interest, 3))
log_msg("  Treated observations:", round(summary_stats$pct_treated, 1), "%")

# ==============================================================================
# SECTION 8: SAVE OUTPUTS
# ==============================================================================

log_msg("Saving outputs...")

# Create output directory if needed
create_output_dirs()

# Save analysis-ready dataset
saveRDS(
  analysis_df, 
  file.path(PATHS$data_processed, OUTPUT_FILES$data$analysis_ready)
)
log_msg("  Saved:", OUTPUT_FILES$data$analysis_ready)

# Save treatment data separately (useful for diagnostics)
saveRDS(
  state_year_treatments,
  file.path(PATHS$data_processed, OUTPUT_FILES$data$ballot_treatments)
)
log_msg("  Saved:", OUTPUT_FILES$data$ballot_treatments)

# Save sample selection table for appendix
write_csv(
  sample_tracker,
  file.path(PATHS$tables_appendix, "sample_selection_steps.csv")
)

# Save data dictionary
data_dictionary <- data.frame(
  Variable = c(
    "news_interest_score", "did_treatment", "treatment_group", "post_election",
    "did_marijuana", "did_gambling", "did_abortion", "did_marriage",
    "did_intensity", "state_first_treat", "cohort_morality",
    "age", "female", "college", "democrat", "republican"
  ),
  Description = c(
    "CES news interest (1-4 scale, higher = more interest)",
    "DiD treatment indicator (1 if state-year has/had morality measure)",
    "Ever-treated indicator (1 if state ever has morality measure)",
    "Post-treatment period indicator",
    "Treatment indicator for marijuana measures",
    "Treatment indicator for gambling measures", 
    "Treatment indicator for abortion measures",
    "Treatment indicator for marriage measures",
    "Count of morality measures (intensity)",
    "First year state received morality treatment",
    "Treatment cohort for CS/SA estimators (0 = never treated)",
    "Respondent age in years",
    "Female indicator (1 = female)",
    "College degree indicator (1 = 4-year degree or higher)",
    "Democrat identifier (1 = Democrat)",
    "Republican identifier (1 = Republican)"
  ),
  stringsAsFactors = FALSE
)

write_csv(
  data_dictionary,
  file.path(PATHS$data_processed, "data_dictionary_all_years.csv")
)
log_msg("  Saved: data_dictionary_all_years.csv")

# ==============================================================================
# COMPLETION
# ==============================================================================

log_msg("=" |> rep(70) |> paste(collapse = ""))
log_msg("DATA PREPARATION COMPLETE")
log_msg("=" |> rep(70) |> paste(collapse = ""))
log_msg("Output files saved to:", PATHS$data_processed)
log_msg("Analysis dataset:", format(nrow(analysis_df), big.mark = ","), "observations")
log_msg("Ready for 02_main_analysis.R")
