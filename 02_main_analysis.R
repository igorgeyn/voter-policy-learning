#!/usr/bin/env Rscript
# ==============================================================================
# 02_MAIN_ANALYSIS.R - Core Difference-in-Differences Analysis
# ==============================================================================
# Purpose: Run all DiD specifications and modern estimators
# Input: analysis_ready_all_years.rds
# Output: all_results_morality_politics.rds
# ==============================================================================

# Source configuration
source("00_config.R")

# Load packages
load_packages()

# Set seed
set.seed(PARAMS$seed)

log_msg("=" |> rep(70) |> paste(collapse = ""))
log_msg("STEP 2: MAIN DIFFERENCE-IN-DIFFERENCES ANALYSIS")
log_msg("=" |> rep(70) |> paste(collapse = ""))

# ==============================================================================
# LOAD DATA
# ==============================================================================

log_msg("Loading analysis data...")

df <- readRDS(file.path(PATHS$data_processed, OUTPUT_FILES$data$analysis_ready))

log_msg("  Observations:", format(nrow(df), big.mark = ","))
log_msg("  States:", n_distinct(df$state))
log_msg("  Years:", paste(range(df$year), collapse = "-"))
log_msg("  Treated observations:", sum(df$did_treatment, na.rm = TRUE))

# Initialize results storage
results <- list(
  main = list(),
  robustness = list(),
  heterogeneity = list(),
  diagnostics = list(),
  modern_estimators = list(),
  topic_specific = list()
)

# ==============================================================================
# PART 1: BASELINE DID SPECIFICATIONS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 1: BASELINE DID SPECIFICATIONS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# 1.1 Simple 2x2 DiD (no fixed effects)
log_msg("1.1 Simple DiD...")
results$main$simple_did <- lm(
  news_interest_score ~ did_treatment + treatment_group + post_election,
  data = df
)
log_msg("    Effect:", fmt_num(coef(results$main$simple_did)["did_treatment"]))

# 1.2 State fixed effects only
log_msg("1.2 State FE...")
results$main$state_fe <- feols(
  news_interest_score ~ did_treatment | state,
  data = df,
  cluster = ~state
)
log_msg("    Effect:", fmt_num(coef(results$main$state_fe)["did_treatment"]))

# 1.3 Two-way fixed effects (MAIN SPECIFICATION)
log_msg("1.3 Two-way FE (MAIN)...")
results$main$twoway_fe <- feols(
  news_interest_score ~ did_treatment | state + year,
  data = df,
  cluster = ~state
)
main_coef <- coef(results$main$twoway_fe)["did_treatment"]
main_se <- se(results$main$twoway_fe)["did_treatment"]
main_p <- 2 * pnorm(-abs(main_coef / main_se))
log_msg("    Effect:", fmt_num(main_coef), "(SE:", fmt_num(main_se), ")")
log_msg("    P-value:", fmt_pval(main_p))

# 1.4 With individual controls
log_msg("1.4 With controls...")
control_vars_present <- PARAMS$control_vars[PARAMS$control_vars %in% names(df)]
if (length(control_vars_present) > 0) {
  control_formula <- paste(control_vars_present, collapse = " + ")
  results$main$with_controls <- feols(
    as.formula(paste("news_interest_score ~ did_treatment +", control_formula, "| state + year")),
    data = df,
    cluster = ~state
  )
  log_msg("    Effect:", fmt_num(coef(results$main$with_controls)["did_treatment"]))
  log_msg("    Controls:", paste(control_vars_present, collapse = ", "))
}

# 1.5 Intensity specification
log_msg("1.5 Intensity (continuous)...")
results$main$intensity <- feols(
  news_interest_score ~ did_intensity | state + year,
  data = df,
  cluster = ~state
)
log_msg("    Effect per measure:", fmt_num(coef(results$main$intensity)["did_intensity"]))

# 1.6 State-specific trends
log_msg("1.6 State-specific trends...")
results$main$state_trends <- feols(
  news_interest_score ~ did_treatment | state + year + state[year],
  data = df,
  cluster = ~state
)
log_msg("    Effect:", fmt_num(coef(results$main$state_trends)["did_treatment"]))

# ==============================================================================
# PART 2: MODERN DID ESTIMATORS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 2: MODERN DID ESTIMATORS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# 2.1 Sun-Abraham (2021) estimator
log_msg("2.1 Sun-Abraham estimator...")
tryCatch({
  # Prepare data for Sun-Abraham
  sa_data <- df %>%
    filter(!is.na(state_first_treat)) %>%
    mutate(
      # Cohort variable (0 for never-treated, first treatment year otherwise)
      cohort = ifelse(is.finite(state_first_treat), state_first_treat, 10000),
      # Never treated indicator
      never_treated = !is.finite(state_first_treat) | state_first_treat > PARAMS$end_year
    )
  
  # Run Sun-Abraham using fixest's sunab()
  results$modern_estimators$sun_abraham <- feols(
    news_interest_score ~ sunab(cohort, year, ref.p = -1) | state + year,
    data = sa_data %>% filter(cohort <= PARAMS$end_year | never_treated),
    cluster = ~state
  )
  
  # Extract overall ATT
  sa_coefs <- coef(results$modern_estimators$sun_abraham)
  sa_att <- mean(sa_coefs[grepl("year::", names(sa_coefs)) & 
                           as.numeric(gsub(".*::", "", names(sa_coefs))) >= 0], na.rm = TRUE)
  results$modern_estimators$sun_abraham_att <- sa_att
  
  log_msg("    Sun-Abraham ATT:", fmt_num(sa_att))
  
}, error = function(e) {
  log_msg("    Sun-Abraham failed:", e$message, level = "WARN")
  results$modern_estimators$sun_abraham <- NULL
  results$modern_estimators$sun_abraham_att <- NA
})

# 2.2 Callaway-Sant'Anna (2021) estimator
log_msg("2.2 Callaway-Sant'Anna estimator...")
tryCatch({
  # Prepare panel data at state-year level
  cs_panel <- df %>%
    group_by(state, year) %>%
    summarise(
      news_interest = mean(news_interest_score, na.rm = TRUE),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    left_join(
      df %>% 
        select(state, cohort_morality, ever_morality = treatment_group) %>% 
        distinct(),
      by = "state"
    ) %>%
    mutate(
      state_id = as.numeric(factor(state)),
      # CS requires 0 for never-treated
      g = ifelse(cohort_morality == 0 | is.na(cohort_morality) | cohort_morality > PARAMS$end_year, 
                 0, cohort_morality)
    ) %>%
    filter(!is.na(news_interest))
  
  # Count never-treated
  n_never_treated <- length(unique(cs_panel$state[cs_panel$g == 0]))
  log_msg("    Never-treated states:", n_never_treated)
  
  if (n_never_treated >= 5) {
    results$modern_estimators$callaway_santanna <- att_gt(
      yname = "news_interest",
      tname = "year",
      idname = "state_id", 
      gname = "g",
      data = as.data.frame(cs_panel),
      control_group = "nevertreated",
      anticipation = 0,
      base_period = "universal"
    )
    
    # Aggregate to overall ATT
    cs_agg <- aggte(results$modern_estimators$callaway_santanna, type = "simple")
    results$modern_estimators$cs_overall <- cs_agg
    
    log_msg("    CS ATT:", fmt_num(cs_agg$overall.att), 
            "(SE:", fmt_num(cs_agg$overall.se), ")")
  } else {
    log_msg("    Insufficient never-treated units for CS estimator", level = "WARN")
  }
  
}, error = function(e) {
  log_msg("    Callaway-Sant'Anna failed:", e$message, level = "WARN")
  results$modern_estimators$callaway_santanna <- NULL
})

# ==============================================================================
# PART 3: DIAGNOSTIC TESTS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 3: DIAGNOSTIC TESTS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# 3.1 Bacon Decomposition
log_msg("3.1 Bacon decomposition...")
tryCatch({
  # Create state-year panel for Bacon
  bacon_panel <- df %>%
    group_by(state, year) %>%
    summarise(
      news_interest = mean(news_interest_score, na.rm = TRUE),
      treated = as.numeric(any(did_treatment == 1)),
      .groups = "drop"
    )
  
  results$diagnostics$bacon <- bacon(
    news_interest ~ treated,
    data = bacon_panel,
    id_var = "state",
    time_var = "year"
  )
  
  # Summarize by comparison type
  bacon_summary <- results$diagnostics$bacon %>%
    group_by(type) %>%
    summarise(
      weight = sum(weight),
      avg_estimate = weighted.mean(estimate, weight),
      .groups = "drop"
    )
  
  log_msg("    Bacon decomposition complete:")
  for (i in 1:nrow(bacon_summary)) {
    log_msg("      ", bacon_summary$type[i], ": weight =", 
            round(bacon_summary$weight[i] * 100, 1), "%")
  }
  
  results$diagnostics$bacon_summary <- bacon_summary
  
}, error = function(e) {
  log_msg("    Bacon decomposition failed:", e$message, level = "WARN")
})

# 3.2 Parallel Trends Test
log_msg("3.2 Parallel trends test...")
tryCatch({
  # Pre-treatment data only
  pre_data <- df %>%
    filter(post_election == 0 | treatment_group == 0)
  
  # Test: interaction of treatment group with time trend
  results$diagnostics$parallel_trends <- feols(
    news_interest_score ~ treatment_group * year | state,
    data = pre_data,
    cluster = ~state
  )
  
  trend_coef <- coef(results$diagnostics$parallel_trends)["treatment_group:year"]
  trend_se <- se(results$diagnostics$parallel_trends)["treatment_group:year"]
  trend_p <- 2 * pnorm(-abs(trend_coef / trend_se))
  
  results$diagnostics$parallel_trends_p <- trend_p
  
  log_msg("    Pre-trend coefficient:", fmt_num(trend_coef))
  log_msg("    P-value:", fmt_pval(trend_p))
  
  if (trend_p < 0.05) {
    log_msg("    WARNING: Parallel trends assumption violated (p < 0.05)", level = "WARN")
  }
  
}, error = function(e) {
  log_msg("    Parallel trends test failed:", e$message, level = "WARN")
})

# 3.3 Placebo Test
log_msg("3.3 Placebo test (", PARAMS$n_placebo, " iterations)...")
tryCatch({
  # Run placebo iterations
  actual_effect <- coef(results$main$twoway_fe)["did_treatment"]
  
  placebo_effects <- replicate(PARAMS$n_placebo, {
    # Randomly reassign treatment within states
    df_placebo <- df %>%
      group_by(state) %>%
      mutate(placebo_treat = sample(did_treatment)) %>%
      ungroup()
    
    m <- feols(news_interest_score ~ placebo_treat | state + year, 
               data = df_placebo, cluster = ~state)
    coef(m)["placebo_treat"]
  })
  
  # Calculate placebo p-value
  placebo_p <- mean(abs(placebo_effects) >= abs(actual_effect))
  placebo_percentile <- mean(placebo_effects <= actual_effect) * 100
  
  results$diagnostics$placebo_effects <- placebo_effects
  results$diagnostics$placebo_p <- placebo_p
  results$diagnostics$placebo_percentile <- placebo_percentile
  
  log_msg("    Actual effect percentile:", round(placebo_percentile, 1), "%")
  log_msg("    Placebo p-value:", fmt_pval(placebo_p))
  
}, error = function(e) {
  log_msg("    Placebo test failed:", e$message, level = "WARN")
})

# ==============================================================================
# PART 4: HETEROGENEITY ANALYSIS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 4: HETEROGENEITY ANALYSIS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# 4.1 By education
log_msg("4.1 By education...")
if ("college" %in% names(df)) {
  df$did_x_college <- df$did_treatment * df$college
  results$heterogeneity$education <- feols(
    news_interest_score ~ did_treatment + did_x_college + college | state + year,
    data = df,
    cluster = ~state
  )
  log_msg("    Non-college:", fmt_num(coef(results$heterogeneity$education)["did_treatment"]))
  log_msg("    College diff:", fmt_num(coef(results$heterogeneity$education)["did_x_college"]))
}

# 4.2 By party
log_msg("4.2 By party...")
if (all(c("democrat", "republican") %in% names(df))) {
  df$did_x_democrat <- df$did_treatment * df$democrat
  df$did_x_republican <- df$did_treatment * df$republican
  
  results$heterogeneity$party <- feols(
    news_interest_score ~ did_treatment + did_x_democrat + did_x_republican | state + year,
    data = df,
    cluster = ~state
  )
  log_msg("    Independent:", fmt_num(coef(results$heterogeneity$party)["did_treatment"]))
  log_msg("    Democrat diff:", fmt_num(coef(results$heterogeneity$party)["did_x_democrat"]))
  log_msg("    Republican diff:", fmt_num(coef(results$heterogeneity$party)["did_x_republican"]))
}

# 4.3 By age
log_msg("4.3 By age...")
if (all(c("young", "old") %in% names(df))) {
  df$did_x_young <- df$did_treatment * df$young
  df$did_x_old <- df$did_treatment * df$old
  
  results$heterogeneity$age <- feols(
    news_interest_score ~ did_treatment + did_x_young + did_x_old | state + year,
    data = df,
    cluster = ~state
  )
  log_msg("    Middle-aged:", fmt_num(coef(results$heterogeneity$age)["did_treatment"]))
  log_msg("    Young diff:", fmt_num(coef(results$heterogeneity$age)["did_x_young"]))
  log_msg("    Old diff:", fmt_num(coef(results$heterogeneity$age)["did_x_old"]))
}

# 4.4 By adoption timing (early vs late)
log_msg("4.4 By adoption timing...")
df <- df %>%
  mutate(early_adopter = as.numeric(state_first_treat <= 2010 & is.finite(state_first_treat)))

results$heterogeneity$timing_early <- feols(
  news_interest_score ~ did_treatment | state + year,
  data = df %>% filter(early_adopter == 1 | treatment_group == 0),
  cluster = ~state
)

results$heterogeneity$timing_late <- feols(
  news_interest_score ~ did_treatment | state + year,
  data = df %>% filter(early_adopter == 0),
  cluster = ~state
)

log_msg("    Early adopters:", fmt_num(coef(results$heterogeneity$timing_early)["did_treatment"]))
log_msg("    Late adopters:", fmt_num(coef(results$heterogeneity$timing_late)["did_treatment"]))

# ==============================================================================
# PART 5: TOPIC-SPECIFIC ANALYSIS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 5: TOPIC-SPECIFIC ANALYSIS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

topics <- c("marijuana", "gambling", "abortion", "marriage")

for (topic in topics) {
  var_name <- paste0("did_", topic)
  if (var_name %in% names(df)) {
    log_msg("  ", topic, "...")
    
    results$topic_specific[[topic]] <- feols(
      as.formula(paste("news_interest_score ~", var_name, "| state + year")),
      data = df,
      cluster = ~state
    )
    
    coef_val <- coef(results$topic_specific[[topic]])[[var_name]]
    se_val <- se(results$topic_specific[[topic]])[[var_name]]
    n_treated <- sum(df[[var_name]], na.rm = TRUE)
    
    log_msg("    Effect:", fmt_num(coef_val), "(SE:", fmt_num(se_val), ")")
    log_msg("    N treated:", format(n_treated, big.mark = ","))
  }
}

# ==============================================================================
# PART 6: ROBUSTNESS CHECKS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("PART 6: ROBUSTNESS CHECKS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# 6.1 Alternative standard errors
log_msg("6.1 Alternative standard errors...")
results$robustness$se_state_year <- feols(
  news_interest_score ~ did_treatment | state + year,
  data = df,
  cluster = ~state + year
)

results$robustness$se_robust <- feols(
  news_interest_score ~ did_treatment | state + year,
  data = df,
  vcov = "HC1"
)

log_msg("    State-year clustered SE:", fmt_num(se(results$robustness$se_state_year)["did_treatment"]))
log_msg("    Robust HC1 SE:", fmt_num(se(results$robustness$se_robust)["did_treatment"]))

# 6.2 Drop high-intensity states
log_msg("6.2 Excluding high-intensity states...")
high_intensity_states <- df %>%
  group_by(state) %>%
  summarise(total_measures = sum(n_morality, na.rm = TRUE)) %>%
  arrange(desc(total_measures)) %>%
  slice_head(n = 3) %>%
  pull(state)

results$robustness$drop_high_intensity <- feols(
  news_interest_score ~ did_treatment | state + year,
  data = df %>% filter(!state %in% high_intensity_states),
  cluster = ~state
)
log_msg("    Dropped:", paste(high_intensity_states, collapse = ", "))
log_msg("    Effect:", fmt_num(coef(results$robustness$drop_high_intensity)["did_treatment"]))

# 6.3 Exclude 2020 (COVID)
log_msg("6.3 Excluding 2020...")
results$robustness$no_2020 <- feols(
  news_interest_score ~ did_treatment | state + year,
  data = df %>% filter(year != 2020),
  cluster = ~state
)
log_msg("    Effect:", fmt_num(coef(results$robustness$no_2020)["did_treatment"]))

# 6.4 Leave-one-state-out
log_msg("6.4 Leave-one-state-out analysis...")
treated_states <- unique(df$state[df$treatment_group == 1])
loo_results <- map_df(treated_states, function(s) {
  m <- feols(news_interest_score ~ did_treatment | state + year,
             data = df %>% filter(state != s),
             cluster = ~state)
  data.frame(
    excluded_state = s,
    coefficient = coef(m)["did_treatment"],
    se = se(m)["did_treatment"]
  )
})
results$robustness$leave_one_out <- loo_results
log_msg("    Coefficient range:", fmt_num(min(loo_results$coefficient)), "to", 
        fmt_num(max(loo_results$coefficient)))

# ==============================================================================
# SAVE RESULTS
# ==============================================================================

log_msg("-" |> rep(70) |> paste(collapse = ""))
log_msg("SAVING RESULTS")
log_msg("-" |> rep(70) |> paste(collapse = ""))

# Add metadata
results$metadata <- list(
  created = Sys.time(),
  n_observations = nrow(df),
  n_states = n_distinct(df$state),
  years = range(df$year),
  seed = PARAMS$seed
)

# Save comprehensive results
saveRDS(
  results,
  file.path(PATHS$data_processed, OUTPUT_FILES$data$all_results)
)
log_msg("Saved:", OUTPUT_FILES$data$all_results)

# ==============================================================================
# SUMMARY
# ==============================================================================

log_msg("=" |> rep(70) |> paste(collapse = ""))
log_msg("ANALYSIS COMPLETE - KEY FINDINGS")
log_msg("=" |> rep(70) |> paste(collapse = ""))

log_msg("")
log_msg("MAIN TWFE RESULT:")
log_msg("  Coefficient:", fmt_num(main_coef))
log_msg("  Std. Error:", fmt_num(main_se))
log_msg("  P-value:", fmt_pval(main_p))
log_msg("  95% CI: [", fmt_num(main_coef - 1.96*main_se), ",", 
        fmt_num(main_coef + 1.96*main_se), "]")

log_msg("")
log_msg("MODERN ESTIMATORS:")
if (!is.null(results$modern_estimators$sun_abraham_att)) {
  log_msg("  Sun-Abraham ATT:", fmt_num(results$modern_estimators$sun_abraham_att))
}
if (!is.null(results$modern_estimators$cs_overall)) {
  log_msg("  Callaway-Sant'Anna ATT:", fmt_num(results$modern_estimators$cs_overall$overall.att))
}

log_msg("")
log_msg("DIAGNOSTICS:")
if (!is.null(results$diagnostics$parallel_trends_p)) {
  log_msg("  Parallel trends p-value:", fmt_pval(results$diagnostics$parallel_trends_p))
}
if (!is.null(results$diagnostics$placebo_percentile)) {
  log_msg("  Placebo percentile:", round(results$diagnostics$placebo_percentile, 1), "%")
}

log_msg("")
log_msg("Ready for 03_extensions.R and 04_generate_outputs.R")
