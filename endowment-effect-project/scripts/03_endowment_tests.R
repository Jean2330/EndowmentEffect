# =============================================================================
# SCRIPT 03: HYPOTHESIS TESTS
# =============================================================================
# Project: Endowment Effect
# All comparisons are within-subjects (Wilcoxon signed-rank test).
# =============================================================================

rm(list = ls())
cat("\014")

library(dplyr)
library(tidyr)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

data_wide <- readRDS("/Users/jeanmarroquin/Documents/Economia/asistente/dotacion/nueva_version/data/clean/data_wide.rds")

cat("Participants:", nrow(data_wide), "\n")

# Helper function: runs a Wilcoxon signed-rank test and prints a clean report
run_wilcoxon <- function(x, y, label_x, label_y, hypothesis) {
  # Keep only pairs where both are non-missing
  valid <- !is.na(x) & !is.na(y)
  x <- x[valid]
  y <- y[valid]
  
  n    <- length(x)
  test <- wilcox.test(x, y, paired = TRUE, alternative = "greater",
                      exact = FALSE, conf.int = TRUE)
  
  cat("\n", strrep("-", 70), "\n")
  cat(hypothesis, "\n")
  cat(strrep("-", 70), "\n")
  cat(sprintf("  H0: Switching point(%s) = Switching point(%s)\n",
              label_x, label_y))
  cat(sprintf("  H1: Switching point(%s) > Switching point(%s)\n",
              label_x, label_y))
  cat(sprintf("  n (complete pairs): %d\n", n))
  cat(sprintf("  Mean %s: %.1f  |  Mean %s: %.1f  |  Difference: %.1f\n",
              label_x, mean(x), label_y, mean(y), mean(x) - mean(y)))
  cat(sprintf("  Median %s: %.1f  |  Median %s: %.1f\n",
              label_x, median(x), label_y, median(y)))
  cat(sprintf("  W statistic: %.1f\n", test$statistic))
  cat(sprintf("  p-value: %.4f\n", test$p.value))
  cat(sprintf("  95%% CI for location shift: [%.1f, %.1f]\n",
              test$conf.int[1], test$conf.int[2]))
  if (test$p.value < 0.001) cat("  Significance: ***\n")
  else if (test$p.value < 0.01) cat("  Significance: **\n")
  else if (test$p.value < 0.05) cat("  Significance: *\n")
  else if (test$p.value < 0.10) cat("  Significance: . (marginal)\n")
  else cat("  Significance: not significant\n")
}

# Two-sided version for comparisons where direction is not obvious a priori
run_wilcoxon_2sided <- function(x, y, label_x, label_y, hypothesis) {
  valid <- !is.na(x) & !is.na(y)
  x <- x[valid]
  y <- y[valid]
  
  n    <- length(x)
  test <- wilcox.test(x, y, paired = TRUE, alternative = "two.sided",
                      exact = FALSE, conf.int = TRUE)
  
  cat("\n", strrep("-", 70), "\n")
  cat(hypothesis, "\n")
  cat(strrep("-", 70), "\n")
  cat(sprintf("  H0: Switching point(%s) = Switching point(%s)\n",
              label_x, label_y))
  cat(sprintf("  H1: Switching point(%s) != Switching point(%s)\n",
              label_x, label_y))
  cat(sprintf("  n (complete pairs): %d\n", n))
  cat(sprintf("  Mean %s: %.1f  |  Mean %s: %.1f  |  Difference: %.1f\n",
              label_x, mean(x), label_y, mean(y), mean(x) - mean(y)))
  cat(sprintf("  Median %s: %.1f  |  Median %s: %.1f\n",
              label_x, median(x), label_y, median(y)))
  cat(sprintf("  W statistic: %.1f\n", test$statistic))
  cat(sprintf("  p-value: %.4f\n", test$p.value))
  cat(sprintf("  95%% CI for location shift: [%.1f, %.1f]\n",
              test$conf.int[1], test$conf.int[2]))
  if (test$p.value < 0.001) cat("  Significance: ***\n")
  else if (test$p.value < 0.01) cat("  Significance: **\n")
  else if (test$p.value < 0.05) cat("  Significance: *\n")
  else if (test$p.value < 0.10) cat("  Significance: . (marginal)\n")
  else cat("  Significance: not significant\n")
}

# =============================================================================
# 2. RESEARCH QUESTION 1: ENDOWMENT EFFECT
# =============================================================================
# Does ownership increase valuation?
# S1.1: personal seller (buyer has no mug) - owner valuation
# B1.1: personal buyer (no mug owned)      - non-owner valuation
#
# Theory predicts sellers demand more than buyers are willing to pay.
# H0: SP(S1.1) = SP(B1.1)  [no endowment effect]
# H1: SP(S1.1) > SP(B1.1)  [endowment effect exists]
# =============================================================================

cat("\n\n")
cat(strrep("=", 70), "\n")
cat("RESEARCH QUESTION 1: ENDOWMENT EFFECT\n")
cat(strrep("=", 70), "\n")

run_wilcoxon(
  x = data_wide$sp_S1_1,
  y = data_wide$sp_B1_1,
  label_x    = "S1.1",
  label_y    = "B1.1",
  hypothesis = "Does ownership increase valuation? (S1.1 vs B1.1)"
)

# =============================================================================
# SECOND MUG
# =============================================================================
# 3a. Does the seller adjust valuation when the buyer already has a mug?
#     S1.2: seller, buyer already has a mug
#     S1.1: seller, buyer has no mug
#     No clear directional prediction - two-sided test
#
# 3b. Does owning a mug reduce willingness to pay for a second one?
#     B1.2: buyer who already owns a mug (second mug purchase)
#     B1.1: buyer with no mug
#     Theory (diminishing marginal utility) predicts B1.1 > B1.2
#     H0: SP(B1.1) = SP(B1.2)
#     H1: SP(B1.1) > SP(B1.2)
# =============================================================================

cat("\n\n")
cat(strrep("=", 70), "\n")
cat("RESEARCH QUESTION 2: SECOND MUG\n")
cat(strrep("=", 70), "\n")

run_wilcoxon_2sided(
  x = data_wide$sp_S1_1,
  y = data_wide$sp_S1_2,
  label_x    = "S1.1",
  label_y    = "S1.2",
  hypothesis = "Does buyer ownership status affect seller valuation? (S1.1 vs S1.2)"
)

run_wilcoxon(
  x = data_wide$sp_B1_1,
  y = data_wide$sp_B1_2,
  label_x    = "B1.1",
  label_y    = "B1.2",
  hypothesis = "Does owning a mug reduce WTP for a second one? (B1.1 vs B1.2)"
)

# =============================================================================
# 4. RESEARCH QUESTION 3: BROKER EFFECT
# =============================================================================
# Does deciding for a client reduce the endowment effect?
#
# 4a. Broker sellers vs personal sellers
#     S2.1: broker seller, owns a mug
#     S2.2: broker seller, does not own a mug
#     S1.1: personal seller (benchmark)
#     Theory predicts brokers show smaller endowment effect than personal sellers
#     H0: SP(S1.1) = SP(S2.1)  /  SP(S1.1) = SP(S2.2)
#     H1: SP(S1.1) > SP(S2.1)  /  SP(S1.1) > SP(S2.2)
#
# 4b. Broker buyers vs personal buyers
#     B2.1: broker buyer, owns a mug
#     B2.2: broker buyer, does not own a mug
#     B1.1: personal buyer (benchmark)
#     Theory predicts broker buyers show higher WTP than personal buyers
#     H0: SP(B2.1) = SP(B1.1)  /  SP(B2.2) = SP(B1.1)
#     H1: SP(B2.1) > SP(B1.1)  /  SP(B2.2) > SP(B1.1)
#
# 4c. Within broker: does broker's own ownership status matter?
#     S2.1 vs S2.2  /  B2.1 vs B2.2
#     No clear directional prediction -- two-sided tests
# =============================================================================

cat("\n\n")
cat(strrep("=", 70), "\n")
cat("RESEARCH QUESTION 3: BROKER EFFECT\n")
cat(strrep("=", 70), "\n")

cat("\n--- 4a. Broker sellers vs personal seller benchmark ---\n")

run_wilcoxon(
  x = data_wide$sp_S1_1,
  y = data_wide$sp_S2_1,
  label_x    = "S1.1",
  label_y    = "S2.1",
  hypothesis = "Does being a broker reduce seller valuation? (S1.1 vs S2.1)"
)

run_wilcoxon(
  x = data_wide$sp_S1_1,
  y = data_wide$sp_S2_2,
  label_x    = "S1.1",
  label_y    = "S2.2",
  hypothesis = "Does being a broker reduce seller valuation? (S1.1 vs S2.2)"
)

cat("\n--- 4b. Broker buyers vs personal buyer benchmark ---\n")

run_wilcoxon(
  x = data_wide$sp_B2_1,
  y = data_wide$sp_B1_1,
  label_x    = "B2.1",
  label_y    = "B1.1",
  hypothesis = "Does being a broker increase buyer valuation? (B2.1 vs B1.1)"
)

run_wilcoxon(
  x = data_wide$sp_B2_2,
  y = data_wide$sp_B1_1,
  label_x    = "B2.2",
  label_y    = "B1.1",
  hypothesis = "Does being a broker increase buyer valuation? (B2.2 vs B1.1)"
)

cat("\n--- 4c. Within broker: does broker ownership status matter? ---\n")

run_wilcoxon_2sided(
  x = data_wide$sp_S2_1,
  y = data_wide$sp_S2_2,
  label_x    = "S2.1",
  label_y    = "S2.2",
  hypothesis = "Does broker ownership affect selling decisions? (S2.1 vs S2.2)"
)

run_wilcoxon_2sided(
  x = data_wide$sp_B2_1,
  y = data_wide$sp_B2_2,
  label_x    = "B2.1",
  label_y    = "B2.2",
  hypothesis = "Does broker ownership affect buying decisions? (B2.1 vs B2.2)"
)