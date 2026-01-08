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
# CES variable typically named 'newsint' or similar - naming varies by year
# First, detect which column exists
news_var <- NULL
possible_news_vars <- c("newsint", "CC316", "news_interest", "newsinterest")
for (v in possible_news_vars) {
  if (v %in% names(ces)) {
    news_var <- v
    log_msg("  Found news interest variable:", v)
    break
  }
}

if (is.null(news_var)) {
  # List available columns for debugging
  log_msg("  Available columns:", paste(head(names(ces), 20), collapse = ", "), "...")
  stop("Could not find news interest variable. Check CES column names.")
}

# Create the standardized outcome variable
# IMPORTANT: CES codes newsint as 1=most, 4=hardly at all
# We REVERSE this so higher values = more interest (matches paper)
ces <- ces %>%
  mutate(
    news_interest_score = case_when(
      .data[[news_var]] == 1 ~ 4,  # Most of the time -> 4
      .data[[news_var]] == 2 ~ 3,  # Some of the time -> 3
      .data[[news_var]] == 3 ~ 2,  # Only now and then -> 2
      .data[[news_var]] == 4 ~ 1,  # Hardly at all -> 1
      TRUE ~ NA_real_
    ),
    # Binary indicator for high interest
    high_news_interest = as.numeric(news_interest_score >= 3)
  ) %>%
  filter(!is.na(news_interest_score))

prev_n <- sample_tracker$N[nrow(sample_tracker)]
sample_tracker <- add_sample_step(sample_tracker, "Non-missing news interest", nrow(ces), prev_n)

# Create demographic control variables
# First, detect which columns exist
log_msg("Creating demographic variables...")

# Helper function to get column if it exists
get_col_if_exists <- function(df, possible_names) {
  for (v in possible_names) {
    if (v %in% names(df)) return(v)
  }
  return(NULL)
}

# Detect available columns
age_var <- get_col_if_exists(ces, c("age", "birthyr"))
gender_var <- get_col_if_exists(ces, c("gender", "female", "sex"))
educ_var <- get_col_if_exists(ces, c("educ", "education", "edu"))
pid_var <- get_col_if_exists(ces, c("pid3", "pid7", "party", "partyid"))

log_msg("  Age variable:", ifelse(is.null(age_var), "not found", age_var))
log_msg("  Gender variable:", ifelse(is.null(gender_var), "not found", gender_var))
log_msg("  Education variable:", ifelse(is.null(educ_var), "not found", educ_var))
log_msg("  Party ID variable:", ifelse(is.null(pid_var), "not found", pid_var))

# Build age variable
if (!is.null(age_var)) {
  if (age_var == "age") {
    ces$age <- as.numeric(ces$age)
  } else if (age_var == "birthyr") {
    ces$age <- ces$year - as.numeric(ces$birthyr)
  }
  ces$age <- ifelse(ces$age >= 18 & ces$age <= 100, ces$age, NA)
} else {
  ces$age <- NA_real_
}
ces$age_squared <- ces$age^2

# Build gender variable (female indicator)
if (!is.null(gender_var)) {
  if (gender_var == "gender") {
    ces$female <- as.numeric(ces$gender == 2)
  } else if (gender_var == "female") {
    ces$female <- as.numeric(ces$female == 1)
  } else if (gender_var == "sex") {
    ces$female <- as.numeric(ces$sex == 2)
  }
} else {
  ces$female <- NA_real_
}

# Build education variable (college indicator)
# Original uses educ >= 4 (some college or higher)
if (!is.null(educ_var)) {
  ces$college <- as.numeric(ces[[educ_var]] >= 4)  # 4+ = some college or higher
} else {
  ces$college <- NA_real_
}

# Build party ID variables
# IMPORTANT: In CES pid3: 1=Democrat, 2=Republican, 3=Independent/Other
if (!is.null(pid_var)) {
  if (pid_var == "pid3") {
    ces$democrat <- as.numeric(ces$pid3 == 1)
    ces$republican <- as.numeric(ces$pid3 == 3)  # Note: 3 in pid3, not 2
    ces$independent <- as.numeric(ces$pid3 == 2)  # Note: 2 in pid3, not 3
  } else if (pid_var == "pid7") {
    ces$democrat <- as.numeric(ces$pid7 %in% 1:3)
    ces$republican <- as.numeric(ces$pid7 %in% 5:7)
    ces$independent <- as.numeric(ces$pid7 == 4)
  }
} else {
  ces$democrat <- NA_real_
  ces$republican <- NA_real_
  ces$independent <- NA_real_
}

# Build age groups for heterogeneity
ces <- ces %>%
  mutate(
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

# The original script uses pre-existing topic columns from ballot data
# Check required columns exist
required_cols <- c("drug", "gambling_lottery", "abort")
has_required_cols <- all(required_cols %in% names(ballot))

if (!has_required_cols) {
  log_msg("  Missing required columns:", 
          paste(setdiff(required_cols, names(ballot)), collapse = ", "), level = "WARN")
  stop("Ballot data missing required topic classification columns")
}

log_msg("  Using pre-classified topic columns from ballot data...")

# Classification matching original 01_setup_all_years.R exactly:
ballot <- ballot %>%
  mutate(
    # Marijuana: uses 'drug' column
    # Note: drug column has 113 measures, paper reports 140
    # Could supplement with keyword search if needed
    has_marijuana_measure = as.numeric(drug == 1),
    
    # Gambling: uses 'gambling_lottery' column (66 measures - matches paper)
    has_gambling = as.numeric(gambling_lottery == 1),
    
    # Abortion: uses 'abort' column (23 measures - matches paper)
    has_abortion_measure = as.numeric(abort == 1),
    
    # Marriage: ANY mention of "marriage" in name or description
    # In 2006-2020, all marriage ballot measures are about same-sex marriage
    # (either pro or con). The descriptions are often just "Marriage" with no
    # additional keywords, so we use simple detection.
    # This captures 27 of ~29 measures from the paper.
    has_marriage_measure = as.numeric(
      grepl("marriage", ballotname, ignore.case = TRUE) |
        grepl("marriage", ballotdescrip, ignore.case = TRUE)
    )
  )

# Now create the aggregate morality indicator
ballot <- ballot %>%
  mutate(
    is_marijuana = has_marijuana_measure,
    is_gambling = has_gambling,
    is_abortion = has_abortion_measure,
    is_marriage = has_marriage_measure,
    is_morality = as.numeric(
      has_marijuana_measure == 1 | 
        has_gambling == 1 | 
        has_abortion_measure == 1 | 
        has_marriage_measure == 1
    )
  )

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

# Aggregate to state-year level (matching original exactly)
state_year_treatments <- ballot %>%
  group_by(state, year) %>%
  summarise(
    # Basic counts
    has_any_measure = 1,
    n_measures = n(),
    n_passed = sum(passed, na.rm = TRUE),
    pass_rate = mean(passed, na.rm = TRUE),
    
    # Topic indicators (aggregate to state-year)
    has_marijuana_measure = max(has_marijuana_measure, na.rm = TRUE),
    has_gambling = max(has_gambling, na.rm = TRUE),
    has_abortion_measure = max(has_abortion_measure, na.rm = TRUE),
    has_marriage_measure = max(has_marriage_measure, na.rm = TRUE),
    
    # PRIMARY: Morality Politics Treatment (matches original)
    has_morality_measure = max(
      has_marijuana_measure,
      has_gambling,
      has_abortion_measure,
      has_marriage_measure,
      na.rm = TRUE
    ),
    
    # Count of morality measures for intensity
    n_morality_measures = sum(
      has_marijuana_measure,
      has_gambling,
      has_abortion_measure,
      has_marriage_measure,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  # Replace -Inf with 0 (from max of empty/all-NA)
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), 0, .)))

# Fill in state-years with no measures
all_state_years <- expand.grid(
  state = unique(ces$state),
  year = PARAMS$start_year:PARAMS$end_year,
  stringsAsFactors = FALSE
)

state_year_treatments <- all_state_years %>%
  left_join(state_year_treatments, by = c("state", "year")) %>%
  mutate(across(everything(), ~replace_na(., 0)))

# Calculate treatment timing for staggered DiD
treatment_timing <- state_year_treatments %>%
  group_by(state) %>%
  summarise(
    # First year of morality treatment
    state_first_treat = ifelse(
      any(has_morality_measure == 1),
      min(year[has_morality_measure == 1]),
      10000  # Never treated
    ),
    ever_treated = as.numeric(any(has_morality_measure == 1)),
    
    # Also track old definition
    state_first_treat_old = ifelse(
      any(has_any_measure == 1),
      min(year[has_any_measure == 1]),
      10000
    ),
    ever_treated_old = as.numeric(any(has_any_measure == 1)),
    .groups = "drop"
  )

# Join timing back to state-year data
state_year_treatments <- state_year_treatments %>%
  left_join(treatment_timing, by = "state")

# Count never-treated states
never_treated_states <- treatment_timing %>%
  filter(ever_treated == 0) %>%
  pull(state)

never_treated_any <- sum(treatment_timing$ever_treated_old == 0)
never_treated_morality <- sum(treatment_timing$ever_treated == 0)

log_msg("  Never-treated states (any measure):", never_treated_any)
log_msg("  Never-treated states (morality):", never_treated_morality)
log_msg("  Never-treated states:", paste(never_treated_states, collapse = ", "))

# ==============================================================================
# SECTION 6: MERGE CES WITH TREATMENTS
# ==============================================================================

log_msg("Merging CES with treatment data...")

analysis_df <- ces %>%
  left_join(state_year_treatments, by = c("state", "year"))

# Fill NAs with zeros for treatment variables
treatment_vars <- c("has_any_measure", "n_measures", "n_passed", "pass_rate",
                    "has_morality_measure", "n_morality_measures",
                    "has_marijuana_measure", "has_gambling", 
                    "has_abortion_measure", "has_marriage_measure",
                    "state_first_treat", "ever_treated",
                    "state_first_treat_old", "ever_treated_old")

for (var in treatment_vars) {
  if (var %in% names(analysis_df)) {
    analysis_df[[var]][is.na(analysis_df[[var]])] <- 0
  }
}

log_msg("  Merged dataset:", format(nrow(analysis_df), big.mark = ","), "observations")

# ==============================================================================
# SECTION 7: CREATE POST-ELECTION INDICATOR
# ==============================================================================

log_msg("Creating post-election indicator...")

# Check for timing variables (matching original logic)
if ("starttime" %in% names(analysis_df)) {
  # Use survey start time - post-election if November or later
  analysis_df$post_election <- as.numeric(
    lubridate::month(analysis_df$starttime) >= 11
  )
  log_msg("  Using starttime for post-election indicator")
} else if ("tookpost" %in% names(analysis_df)) {
  analysis_df$post_election <- as.numeric(analysis_df$tookpost == 1)
  log_msg("  Using tookpost for post-election indicator")
} else if ("wave" %in% names(analysis_df)) {
  analysis_df$post_election <- as.numeric(analysis_df$wave == 2)
  log_msg("  Using wave for post-election indicator")
} else {
  log_msg("  WARNING: No timing variable found - using synthetic indicator", level = "WARN")
  set.seed(PARAMS$seed)
  analysis_df <- analysis_df %>%
    group_by(year) %>%
    mutate(post_election = as.numeric(row_number() > n()/2)) %>%
    ungroup()
}

log_msg("  Post-election observations:", sum(analysis_df$post_election, na.rm = TRUE))

# ==============================================================================
# SECTION 8: CREATE DID VARIABLES (MATCHING ORIGINAL EXACTLY)
# ==============================================================================

log_msg("Creating DiD variables...")

analysis_df <- analysis_df %>%
  mutate(
    # PRIMARY: Morality Politics Treatment (matches original)
    treatment_group = as.numeric(has_morality_measure == 1),
    did_treatment = treatment_group * post_election,
    
    # OLD definition for comparison
    treatment_group_old = as.numeric(has_any_measure == 1),
    did_treatment_old = treatment_group_old * post_election,
    
    # Intensity treatments
    did_intensity = n_morality_measures * post_election,
    did_intensity_old = n_measures * post_election,
    
    # Topic-specific treatments
    did_marijuana = has_marijuana_measure * post_election,
    did_gambling = has_gambling * post_election,
    did_abortion = has_abortion_measure * post_election,
    did_marriage = has_marriage_measure * post_election,
    
    # For staggered DiD methods
    rel_time_morality = ifelse(
      state_first_treat < 10000,
      year - state_first_treat,
      NA_real_
    ),
    
    # Cohort for CS/SA estimators (0 = never treated)
    cohort_morality = ifelse(state_first_treat < 10000, state_first_treat, 0)
  )

log_msg("  Treated obs (morality × post):", sum(analysis_df$did_treatment, na.rm = TRUE))
log_msg("  Treated obs (any × post):", sum(analysis_df$did_treatment_old, na.rm = TRUE))

# Update sample tracker
prev_n <- sample_tracker$N[nrow(sample_tracker)]
sample_tracker <- add_sample_step(sample_tracker, "Final analysis sample", nrow(analysis_df), prev_n)

# ==============================================================================
# SECTION 9: SUMMARY STATISTICS
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
# SECTION 10: SAVE OUTPUTS
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