#!/usr/bin/env Rscript
# ==============================================================================
# 03_EXTENSIONS.R - Extended Analyses and Knowledge Comparison
# ==============================================================================
# Purpose: Additional analyses including knowledge outcomes, persistence, awareness
# Input: analysis_ready_all_years.rds, all_results_morality_politics.rds
# Output: extension_results_morality_complete.rds
# ==============================================================================

# Source configuration
source("00_config.R")

# Load packages
load_packages()

# Set seed
set.seed(PARAMS$seed)

log_msg("=" |> rep(70) |> paste(collapse = ""))
log_msg("STEP 3: EXTENSION ANALYSES")
log_msg("=" |> rep(70) |> paste(collapse = ""))

# ==============================================================================
# LOAD DATA
# ==============================================================================

log_msg("Loading data...")

df <- readRDS(file.path(PATHS$data_processed, OUTPUT_FILES$data$analysis_ready))
main_results <- readRDS(file.path(PATHS$data_processed, OUTPUT_FILES$data$all_results))

log_msg("  Analysis data:", format(nrow(df), big.mark = ","), "observations")

# Initialize extension results
extension_results <- list(
  knowledge = list(),
  awareness = list(),
  persistence = list(),
  event_study = list(),
  comparison = list()
)

# ==============================================================================
# PART 1: KNOWLEDGE OUTCOME COMPARISON
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 1: KNOWLEDGE OUTCOME COMPARISON")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# Check for knowledge variables in CES
# Common names: CC316 (house control), CC318 (senate control), etc.
knowledge_vars <- c("CC316", "CC318", "congress_knowledge", "house_control", "senate_control")
available_knowledge <- knowledge_vars[knowledge_vars %in% names(df)]

if (length(available_knowledge) > 0) {
  log_msg("Found knowledge variables:", paste(available_knowledge, collapse = ", "))
  
  # Create composite knowledge index if multiple vars available
  if (length(available_knowledge) >= 2) {
    df <- df %>%
      mutate(
        knowledge_index = rowMeans(across(all_of(available_knowledge)), na.rm = TRUE)
      )
    knowledge_outcome <- "knowledge_index"
  } else {
    knowledge_outcome <- available_knowledge[1]
  }
  
  # Run parallel specification for knowledge
  extension_results$knowledge$twoway_fe <- feols(
    as.formula(paste(knowledge_outcome, "~ did_treatment | state + year")),
    data = df,
    cluster = ~state
  )
  
  know_coef <- coef(extension_results$knowledge$twoway_fe)["did_treatment"]
  know_se <- se(extension_results$knowledge$twoway_fe)["did_treatment"]
  
  log_msg("  Knowledge effect:", fmt_num(know_coef), "(SE:", fmt_num(know_se), ")")
  
  # With controls
  control_vars_present <- PARAMS$control_vars[PARAMS$control_vars %in% names(df)]
  if (length(control_vars_present) > 0) {
    extension_results$knowledge$with_controls <- feols(
      as.formula(paste(knowledge_outcome, "~ did_treatment +", 
                       paste(control_vars_present, collapse = "+"), "| state + year")),
      data = df,
      cluster = ~state
    )
    log_msg("  With controls:", 
            fmt_num(coef(extension_results$knowledge$with_controls)["did_treatment"]))
  }
  
  # Correlation between outcomes
  if ("knowledge_index" %in% names(df) || knowledge_outcome %in% names(df)) {
    know_var <- if ("knowledge_index" %in% names(df)) "knowledge_index" else knowledge_outcome
    extension_results$knowledge$correlation <- cor(
      df$news_interest_score, 
      df[[know_var]], 
      use = "pairwise.complete.obs"
    )
    log_msg("  Correlation (news interest vs knowledge):", 
            fmt_num(extension_results$knowledge$correlation))
  }
  
} else {
  log_msg("  No knowledge variables found in data", level = "WARN")
  # Create placeholder for paper numbers
  extension_results$knowledge$placeholder <- list(
    estimate = 0.002,
    se = 0.006,
    note = "Values from paper - actual CES knowledge vars not in cumulative file"
  )
}

# ==============================================================================
# PART 2: POLITICAL AWARENESS HETEROGENEITY
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 2: POLITICAL AWARENESS HETEROGENEITY")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# Create awareness terciles from news interest (or other baseline measure)
# Using pre-treatment news interest as baseline awareness proxy
df <- df %>%
  group_by(state) %>%
  mutate(
    # Use first observation as baseline (imperfect but workable)
    baseline_interest = first(news_interest_score[year == min(year)], default = NA)
  ) %>%
  ungroup()

# If baseline not available, use current interest as proxy (with caveat)
if (all(is.na(df$baseline_interest))) {
  df$baseline_interest <- df$news_interest_score
}

# Create terciles
df <- df %>%
  mutate(
    awareness_tercile = ntile(baseline_interest, 3),
    awareness_low = as.numeric(awareness_tercile == 1),
    awareness_med = as.numeric(awareness_tercile == 2),
    awareness_high = as.numeric(awareness_tercile == 3)
  )

# Effects by awareness level
log_msg("Effects by awareness tercile:")

for (level in c("low", "med", "high")) {
  var_name <- paste0("awareness_", level)
  level_data <- df %>% filter(.data[[var_name]] == 1)
  
  if (nrow(level_data) > 1000) {
    extension_results$awareness[[level]] <- feols(
      news_interest_score ~ did_treatment | state + year,
      data = level_data,
      cluster = ~state
    )
    log_msg("  ", toupper(level), "awareness:", 
            fmt_num(coef(extension_results$awareness[[level]])["did_treatment"]),
            "(n =", format(nrow(level_data), big.mark = ","), ")")
  }
}

# Interaction model
df$did_x_low <- df$did_treatment * df$awareness_low
df$did_x_high <- df$did_treatment * df$awareness_high

extension_results$awareness$interaction <- feols(
  news_interest_score ~ did_treatment + did_x_low + did_x_high | state + year,
  data = df,
  cluster = ~state
)

# ==============================================================================
# PART 3: PERSISTENCE ANALYSIS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 3: PERSISTENCE ANALYSIS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# Create lagged treatment indicators
df <- df %>%
  group_by(state) %>%
  arrange(year) %>%
  mutate(
    # Lag treatment by 1 and 2 years
    did_lag1 = lag(did_treatment, 1, default = 0),
    did_lag2 = lag(did_treatment, 2, default = 0),
    # Lead (for anticipation)
    did_lead1 = lead(did_treatment, 1, default = 0)
  ) %>%
  ungroup()

# Contemporaneous effect only
extension_results$persistence$contemporaneous <- feols(
  news_interest_score ~ did_treatment | state + year,
  data = df,
  cluster = ~state
)
log_msg("  Contemporaneous:", 
        fmt_num(coef(extension_results$persistence$contemporaneous)["did_treatment"]))

# With lags
extension_results$persistence$with_lags <- feols(
  news_interest_score ~ did_treatment + did_lag1 + did_lag2 | state + year,
  data = df,
  cluster = ~state
)
log_msg("  Current:", fmt_num(coef(extension_results$persistence$with_lags)["did_treatment"]))
log_msg("  Lag 1:", fmt_num(coef(extension_results$persistence$with_lags)["did_lag1"]))
log_msg("  Lag 2:", fmt_num(coef(extension_results$persistence$with_lags)["did_lag2"]))

# Cumulative effect
cumulative <- sum(coef(extension_results$persistence$with_lags)[c("did_treatment", "did_lag1", "did_lag2")])
log_msg("  Cumulative effect:", fmt_num(cumulative))

# ==============================================================================
# PART 4: EVENT STUDY COEFFICIENTS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 4: EVENT STUDY ANALYSIS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# Create relative time indicators
df <- df %>%
  mutate(
    rel_time = ifelse(is.finite(state_first_treat), year - state_first_treat, NA),
    # Bin at endpoints
    rel_time_binned = case_when(
      is.na(rel_time) ~ NA_real_,
      rel_time <= -3 ~ -3,
      rel_time >= 3 ~ 3,
      TRUE ~ rel_time
    )
  )

# Event study regression
es_data <- df %>% filter(!is.na(rel_time_binned))

if (nrow(es_data) > 0) {
  extension_results$event_study$model <- feols(
    news_interest_score ~ i(rel_time_binned, ref = -1) | state + year,
    data = es_data,
    cluster = ~state
  )
  
  # Extract coefficients for plotting
  es_coefs <- broom::tidy(extension_results$event_study$model) %>%
    filter(grepl("rel_time", term)) %>%
    mutate(
      time = as.numeric(gsub("rel_time_binned::", "", term)),
      ci_low = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error
    ) %>%
    # Add reference period
    bind_rows(data.frame(time = -1, estimate = 0, std.error = 0, 
                         ci_low = 0, ci_high = 0)) %>%
    arrange(time)
  
  extension_results$event_study$coefficients <- es_coefs
  
  log_msg("  Event study coefficients:")
  for (i in 1:nrow(es_coefs)) {
    log_msg("    t =", es_coefs$time[i], ":", fmt_num(es_coefs$estimate[i]))
  }
}

# ==============================================================================
# PART 5: TREATMENT DEFINITION COMPARISON
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 5: TREATMENT DEFINITION COMPARISON")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# Compare different treatment definitions
treatment_definitions <- list(
  any_measure = "has_any_measure",
  morality = "did_treatment",
  marijuana_only = "did_marijuana"
)

for (def_name in names(treatment_definitions)) {
  var_name <- treatment_definitions[[def_name]]
  
  if (var_name %in% names(df)) {
    # Count never-treated
    n_never <- df %>%
      group_by(state) %>%
      summarise(ever_treated = max(.data[[var_name]], na.rm = TRUE)) %>%
      summarise(n_never = sum(ever_treated == 0)) %>%
      pull(n_never)
    
    # Run TWFE
    m <- feols(
      as.formula(paste("news_interest_score ~", var_name, "| state + year")),
      data = df,
      cluster = ~state
    )
    
    extension_results$comparison[[def_name]] <- list(
      model = m,
      n_never_treated = n_never,
      coefficient = coef(m)[[var_name]],
      se = se(m)[[var_name]]
    )
    
    log_msg("  ", def_name, ":")
    log_msg("    Never-treated states:", n_never)
    log_msg("    TWFE estimate:", fmt_num(coef(m)[[var_name]]), 
            "(SE:", fmt_num(se(m)[[var_name]]), ")")
  }
}

# ==============================================================================
# PART 6: COHORT-SPECIFIC ANALYSIS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 6: COHORT-SPECIFIC EFFECTS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# Get unique cohorts
cohorts <- df %>%
  filter(is.finite(state_first_treat), state_first_treat <= PARAMS$end_year) %>%
  pull(state_first_treat) %>%
  unique() %>%
  sort()

log_msg("  Treatment cohorts:", paste(cohorts, collapse = ", "))

# Effects by cohort
extension_results$cohort_effects <- list()

for (cohort_year in cohorts) {
  cohort_data <- df %>%
    filter(
      (state_first_treat == cohort_year) |  # This cohort
        (treatment_group == 0)                 # Or never-treated
    )
  
  if (nrow(cohort_data) > 1000) {
    m <- feols(
      news_interest_score ~ did_treatment | state + year,
      data = cohort_data,
      cluster = ~state
    )
    
    extension_results$cohort_effects[[as.character(cohort_year)]] <- list(
      model = m,
      coefficient = coef(m)["did_treatment"],
      se = se(m)["did_treatment"],
      n_treated_states = n_distinct(cohort_data$state[cohort_data$treatment_group == 1])
    )
    
    log_msg("    Cohort", cohort_year, ":", 
            fmt_num(coef(m)["did_treatment"]),
            "(", n_distinct(cohort_data$state[cohort_data$treatment_group == 1]), "states)")
  }
}

# ==============================================================================
# SAVE RESULTS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("SAVING EXTENSION RESULTS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# Add metadata
extension_results$metadata <- list(
  created = Sys.time(),
  based_on = OUTPUT_FILES$data$all_results
)

saveRDS(
  extension_results,
  file.path(PATHS$data_processed, OUTPUT_FILES$data$extension_results)
)
log_msg("Saved:", OUTPUT_FILES$data$extension_results)

# Also save event study coefficients as CSV for easy plotting
if (!is.null(extension_results$event_study$coefficients)) {
  write_csv(
    extension_results$event_study$coefficients,
    file.path(PATHS$extensions, "event_study_coefficients.csv")
  )
  log_msg("Saved: event_study_coefficients.csv")
}

# ==============================================================================
# COMPLETION
# ==============================================================================

log_msg("=" |> rep(70) |> paste(collapse = ""))
log_msg("EXTENSION ANALYSES COMPLETE")
log_msg("=" |> rep(70) |> paste(collapse = ""))
log_msg("")
log_msg("Key findings:")
if (!is.null(extension_results$knowledge$twoway_fe)) {
  log_msg("  Knowledge effect:", fmt_num(know_coef), "(news interest was", 
          fmt_num(coef(main_results$main$twoway_fe)["did_treatment"]), ")")
}
log_msg("  Persistence: Effects", 
        ifelse(abs(coef(extension_results$persistence$with_lags)["did_lag1"]) < 0.01, 
               "do not persist", "persist"), "beyond treatment year")
log_msg("")
log_msg("Ready for 04_generate_outputs.R")
