#!/usr/bin/env Rscript
# ==============================================================================
# DIAGNOSTIC: Examine ballot measure classifications
# Run this to understand what's in your ballot data
# ==============================================================================

library(tidyverse)
library(data.table)

# Load data
project_dir <- "/Users/igorgeyn/Library/CloudStorage/GoogleDrive-igorgeyn@gmail.com/My Drive/Grad School/Research/policy_learning"
ballot <- fread(file.path(project_dir, "data/cleaned/ballot_measures_combined.csv"))

# Filter to study period
ballot <- ballot %>% filter(year >= 2006, year <= 2020)

cat("\n=== BALLOT DATA OVERVIEW ===\n")
cat("Total measures:", nrow(ballot), "\n")
cat("Years:", paste(range(ballot$year), collapse = "-"), "\n")
cat("\nColumns available:\n")
cat(paste(names(ballot), collapse = ", "), "\n")

cat("\n=== PRE-CLASSIFIED TOPIC COLUMNS ===\n")
cat("drug (marijuana proxy):", sum(ballot$drug == 1, na.rm = TRUE), "\n")
cat("gambling_lottery:", sum(ballot$gambling_lottery == 1, na.rm = TRUE), "\n")
cat("abort:", sum(ballot$abort == 1, na.rm = TRUE), "\n")

cat("\n=== MARRIAGE MEASURES (let's see what they look like) ===\n")

# What does "marriage" appear in?
marriage_mentions <- ballot %>%
  filter(grepl("marriage", ballotdescrip, ignore.case = TRUE) |
           grepl("marriage", ballotname, ignore.case = TRUE))

cat("\nBallot measures mentioning 'marriage':", nrow(marriage_mentions), "\n")

if (nrow(marriage_mentions) > 0) {
  cat("\nSample of marriage-related measures:\n")
  cat("----------------------------------------\n")
  for (i in 1:min(10, nrow(marriage_mentions))) {
    cat("\n[", i, "] Year:", marriage_mentions$year[i], 
        "| State:", marriage_mentions$st[i], "\n")
    cat("Name:", substr(marriage_mentions$ballotname[i], 1, 80), "...\n")
    cat("Descrip:", substr(marriage_mentions$ballotdescrip[i], 1, 200), "...\n")
  }
}

cat("\n\n=== SAME-SEX / GAY KEYWORDS ===\n")
same_sex <- ballot %>%
  filter(grepl("same.?sex|gay|homosexual", ballotdescrip, ignore.case = TRUE) |
           grepl("same.?sex|gay|homosexual", ballotname, ignore.case = TRUE))
cat("Measures with 'same-sex', 'gay', or 'homosexual':", nrow(same_sex), "\n")

cat("\n=== DEFENSE OF MARRIAGE KEYWORDS ===\n")
defense <- ballot %>%
  filter(grepl("man and.{0,5}woman|one man|defense of marriage|traditional marriage", 
               ballotdescrip, ignore.case = TRUE) |
           grepl("man and.{0,5}woman|one man|defense of marriage|traditional marriage", 
                 ballotname, ignore.case = TRUE))
cat("Measures with defense-of-marriage language:", nrow(defense), "\n")

if (nrow(defense) > 0) {
  cat("\nSample:\n")
  for (i in 1:min(5, nrow(defense))) {
    cat("[", defense$year[i], "-", defense$st[i], "]", 
        substr(defense$ballotname[i], 1, 60), "\n")
  }
}

cat("\n=== COMPARISON WITH PAPER TARGETS ===\n")
cat("Your data vs Paper:\n")
cat("  Marijuana: ", sum(ballot$drug == 1, na.rm = TRUE), " vs 140\n")
cat("  Gambling:  ", sum(ballot$gambling_lottery == 1, na.rm = TRUE), " vs 66\n")
cat("  Abortion:  ", sum(ballot$abort == 1, na.rm = TRUE), " vs 23\n")
cat("  Marriage:  ", nrow(marriage_mentions), " mentions, need ~29 classified\n")