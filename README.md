# Policy Learning Analysis Pipeline

**Do Direct Democracy Provisions Inform Voters? Evidence from State-Level Ballot Measures in the U.S.**

Author: Igor Geyn  
Institution: UCLA Political Science

---

## Overview

This pipeline analyzes whether ballot measures enhance civic engagement by increasing voters' knowledge and awareness of policy-relevant information. It uses:

- **CES (Cooperative Election Study)** data from 2006-2020
- **NCSL/CEDA ballot measure data** merged and classified
- Modern difference-in-differences methods (Sun-Abraham, Callaway-Sant'Anna)

## Key Findings

- Traditional TWFE suggests positive effects (0.025 points, p < 0.05)
- Modern DiD estimators find **null effects** (-0.0001 to -0.0014)
- Parallel trends assumption is violated (p = 0.016)
- Only marijuana measures show marginally robust effects under TWFE

---

## Pipeline Structure

```
policy_learning_pipeline/
├── 00_config.R           # Configuration: paths, parameters, utilities
├── 00_run_all.R          # Master script: runs entire pipeline
├── 01_setup_data.R       # Data preparation: load CES, classify measures
├── 02_main_analysis.R    # Core DiD: TWFE, Sun-Abraham, Callaway-Sant'Anna
├── 03_extensions.R       # Extended analyses: knowledge, heterogeneity
├── 04_generate_outputs.R # Publication outputs: figures and tables
└── README.md             # This file
```

---

## ⚠️ IMPORTANT: Update Your LaTeX File

Your current `policy-learning.tex` has **hardcoded tables** with manually typed numbers. This is bad practice! After running the pipeline, update your LaTeX to use `\input{}`:

```latex
% OLD (BAD - hardcoded numbers):
\begin{tabular}{lrll}
  \toprule
Treatment & Never Treated States & TWFE Estimate & Std. Error \\ 
  \midrule
Any Ballot Measure (Broad) & 1 & 0.0250** & (0.0103) \\ 
...

% NEW (GOOD - generated from code):
\begin{table}[H]
\centering
\caption{Comparison of Treatment Definitions}
\label{tab:treatment_comparison}
\input{../output/tables/all_years/table1_treatment_comparison.tex}
\end{table}
```

See `LATEX_TABLE_UPDATES.tex` for complete replacement instructions for all 6 main body tables.

---

## Quick Start

### Option 1: Run Everything
```r
source("00_run_all.R")
```

### Option 2: Step by Step
```r
source("00_config.R")      # Load configuration
source("01_setup_data.R")  # Prepare data
source("02_main_analysis.R") # Run main DiD
source("03_extensions.R")   # Extensions
source("04_generate_outputs.R") # Generate outputs
```

### Option 3: Regenerate Outputs Only
```bash
Rscript 00_run_all.R --outputs-only
```

---

## Required Data

Place these files in your data directory:

1. **CES Cumulative File**: `data/cumulative_2006-2024.rds`
   - Cooperative Election Study cumulative dataset
   - Must contain: state, year, newsint (news interest)

2. **Ballot Measures**: `data/cleaned/ballot_measures_combined.csv`
   - Merged NCSL/CEDA ballot measure data
   - Must contain: state, year, ballot_title/description

---

## Outputs

### Main Body Tables (NEW - replaces hardcoded LaTeX!)
| File | Description |
|------|-------------|
| `table1_treatment_comparison.tex` | Treatment definition comparison |
| `table2_estimator_comparison.tex` | TWFE vs modern estimators |
| `table3_bacon_decomposition.tex` | Bacon decomposition weights |
| `table4_timing_heterogeneity.tex` | Early vs late adopter effects |
| `table5_topic_effects.tex` | Effects by morality topic |
| `table6_robustness.tex` | Comprehensive robustness checks |

### Figures (for main text)
| File | Description |
|------|-------------|
| `knowledge_vs_interest_comparison.png` | News interest vs knowledge outcomes |
| `parallel_trends_by_cohort.pdf` | Pre-trends by adoption cohort |
| `parallel_trends_event_study.pdf` | Event study pre-trends |
| `event_study_morality.pdf` | Main event study figure |
| `heterogeneity_demographics_twfe.pdf` | Effects by demographics |
| `awareness_effects_morality.pdf` | Effects by political awareness |

### Appendix Tables
| File | Description |
|------|-------------|
| `table_a1_full_specifications.tex` | Complete model specifications |
| `table_a2_sun_abraham.tex` | Sun-Abraham event study estimates |
| `table_a3_parallel_trends.tex` | Parallel trends diagnostics |
| `table_a5_heterogeneity.tex` | Heterogeneous effects |
| `table_a6_leave_one_out.tex` | Leave-one-state-out sensitivity |
| `table_a7_topics.tex` | Topic-specific effects |
| `table_a8_summary_stats.tex` | Summary statistics by year |
| `table_a9_sample_selection.tex` | Sample construction |
| `table_a10_variables.tex` | Variable definitions |
| `table_a11_bootstrap.tex` | Alternative inference |
| `table_a12_timing.tex` | Treatment timing by state |

### Intermediate Data
| File | Description |
|------|-------------|
| `analysis_ready_all_years.rds` | Analysis-ready individual-level data |
| `ballot_treatments_all_years.rds` | State-year treatment indicators |
| `all_results_morality_politics.rds` | All model results |
| `extension_results_morality_complete.rds` | Extension analysis results |

---

## Key Parameters (in `00_config.R`)

```r
PARAMS <- list(
  start_year = 2006,
  end_year = 2020,
  excluded_states = c("Alaska", "Hawaii"),
  morality_keywords = list(
    marijuana = c("marijuana", "cannabis", ...),
    gambling = c("gambling", "casino", ...),
    abortion = c("abortion", "pro-life", ...),
    marriage = c("marriage", "same-sex", ...)
  ),
  seed = 20251226,
  n_bootstrap = 999,
  n_placebo = 1000
)
```

---

## Dependencies

Required R packages (installed automatically):
- **Data**: tidyverse, data.table, haven
- **DiD**: fixest, did, bacondecomp
- **Tables**: modelsummary, kableExtra, xtable
- **Plots**: ggplot2, patchwork, viridis, scales
- **Utilities**: broom, sandwich, lmtest

---

## Cross-Platform Compatibility

The pipeline automatically detects your operating system and finds the project directory:

- **macOS**: `~/Library/CloudStorage/GoogleDrive-.../My Drive/...`
- **Windows**: `G:/My Drive/...` or `C:/Users/.../Google Drive/...`

If detection fails, manually set `PROJECT_ROOT` in `00_config.R`.

---

## Troubleshooting

### "CES data file not found"
Ensure `cumulative_2006-2024.rds` is in the `data/` directory.

### "Ballot measure data not found"
Ensure `ballot_measures_combined.csv` is in `data/cleaned/`.

### "Callaway-Sant'Anna failed"
This requires ≥5 never-treated states. With morality politics treatment, you should have 19.

### Parallel trends test returns NA
Pre-treatment data may be insufficient. Check `post_election` coding.

---

## Citation

If you use this code, please cite:

```
Geyn, Igor. "Do Direct Democracy Provisions Inform Voters? 
Evidence from State-Level Ballot Measures in the U.S." 
UCLA Dissertation Chapter, 2025.
```

---

## Version History

- **v1.0** (2025-12-26): Initial consolidated pipeline
  - Unified 90+ scattered scripts into 5-file pipeline
  - Added modern DiD estimators (Sun-Abraham, Callaway-Sant'Anna)
  - Standardized output generation
  - Cross-platform path handling

---

## Contact

Igor Geyn  
PhD Candidate, Political Science  
University of California, Los Angeles  
igorgeyn@ucla.edu
