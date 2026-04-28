# =============================================================================
# SCRIPT 02: RISK AVERSION ANALYSIS
# =============================================================================
# Holt-Laury design:
#   Option A (safe):  $200 with p  /  $160 with (1-p)
#   Option B (risky): $385 with p  /  $10  with (1-p)
#   p decreases from 100% to 10% in steps of 10pp across 10 items
#
# A risk-neutral participant switches from A to B at item 5 (p = 60%),
# where EV(A) = EV(B). Switching earlier = risk-seeking; later = risk-averse.
# =============================================================================

rm(list = ls())
cat("\014")

library(dplyr)
library(tidyr)
library(ggplot2)

theme_base <- theme_minimal() +
  theme(
    plot.title       = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = 11, hjust = 0.5),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

# =============================================================================
# 1. LOAD DATA
# =============================================================================

data_wide <- readRDS("/Users/jeanmarroquin/Documents/Economia/asistente/dotacion/nueva_version/data/clean/data_wide.rds")
data_raw  <- readxl::read_excel("/Users/jeanmarroquin/Documents/Economia/asistente/dotacion/nueva_version/data/raw/Encuesta.xlsx")

cols_aversion <- paste0("R_Aversion_", 1:10)

# Probabilities for each item (probability of high payoff)
# Item 1: p=1.0, Item 2: p=0.9, ..., Item 10: p=0.1
probs_A <- seq(1.0, 0.1, by = -0.1)

# Expected values by item
EV_A <- 200 * probs_A + 160 * (1 - probs_A)
EV_B <- 385 * probs_A + 10  * (1 - probs_A)

cat("Expected values by item:\n")
ev_table <- data.frame(
  item   = 1:10,
  prob_A = probs_A,
  EV_A   = round(EV_A, 1),
  EV_B   = round(EV_B, 1),
  EV_diff = round(EV_A - EV_B, 1)
)
print(ev_table)
cat("\nA risk-neutral participant switches at item 5 (p = 60%), where EV(A) ~ EV(B)\n")

# =============================================================================
# 2. RISK TYPE CLASSIFICATION
# =============================================================================
# Classification follows Holt-Laury (2002) Table 3
# Score = number of times participant chose option A (safe)

data_wide <- data_wide %>%
  mutate(
    risk_type = case_when(
      aversion_score <= 1  ~ "Highly risk-seeking",
      aversion_score <= 3  ~ "Risk-seeking",
      aversion_score == 4  ~ "Risk-neutral",
      aversion_score <= 6  ~ "Slightly risk-averse",
      aversion_score <= 8  ~ "Risk-averse",
      aversion_score <= 10 ~ "Highly risk-averse",
      TRUE                 ~ NA_character_
    ),
    risk_type = factor(risk_type, levels = c(
      "Highly risk-seeking", "Risk-seeking", "Risk-neutral",
      "Slightly risk-averse", "Risk-averse", "Highly risk-averse"
    ))
  )

cat("\nRisk type classification (Holt-Laury 2002):\n")
risk_tab <- data_wide %>%
  filter(!is.na(risk_type)) %>%
  count(risk_type) %>%
  mutate(pct = round(100 * n / sum(n), 1))
print(risk_tab)

p_risktype <- ggplot(risk_tab, aes(x = risk_type, y = n)) +
  geom_col(fill = "#2c7bb6", alpha = 0.8) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 3.5) +
  labs(
    title    = "Risk Preference Classification",
    subtitle = "Holt-Laury (2002) categories",
    x = NULL, y = "Frequency"
  ) +
  theme_base +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

print(p_risktype)

# =============================================================================
# 3. CHOICE PATTERNS ACROSS ITEMS
# =============================================================================
# For each item, what fraction of participants chose A (safe option)?
# A well-behaved sample should show declining A choices as p decreases.

aversion_long <- data_raw %>%
  dplyr::select(id = Id_D_UsSocioeconomico, all_of(cols_aversion)) %>%
  pivot_longer(
    cols      = all_of(cols_aversion),
    names_to  = "item",
    values_to = "choice"
  ) %>%
  mutate(
    item_num = as.integer(gsub("R_Aversion_", "", item)),
    chose_A  = case_when(
      choice == "A" ~ 1L,
      choice == "B" ~ 0L,
      TRUE          ~ NA_integer_
    ),
    prob_high = seq(1.0, 0.1, by = -0.1)[item_num]
  )

pct_A_by_item <- aversion_long %>%
  filter(!is.na(chose_A)) %>%
  group_by(item_num, prob_high) %>%
  summarise(
    n       = n(),
    pct_A   = round(100 * mean(chose_A), 1),
    .groups = "drop"
  )

cat("\nFraction choosing option A (safe) by item:\n")
print(pct_A_by_item)

p_choices <- ggplot(pct_A_by_item, aes(x = factor(item_num), y = pct_A)) +
  geom_col(fill = "#2c7bb6", alpha = 0.8) +
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "#d7191c", linewidth = 0.7) +
  geom_text(aes(label = paste0(pct_A, "%")), vjust = -0.4, size = 3.2) +
  scale_x_discrete(
    labels = paste0("Item ", 1:10, "\n(p=", probs_A * 100, "%)")
  ) +
  labs(
    title    = "Fraction Choosing Safe Option (A) by Item",
    subtitle = "Dashed line at 50%. Declining pattern indicates risk-averse sample.",
    x = "Item (probability of high payoff)", y = "% choosing A"
  ) +
  ylim(0, 105) +
  theme_base

print(p_choices)

# =============================================================================
# 4. INCONSISTENT RESPONSES
# =============================================================================
# A consistent participant switches from A to B exactly once and never goes back.
# Inconsistent = switches more than once (e.g., A, B, A, B...).

aversion_wide_raw <- data_raw %>%
  dplyr::select(id = Id_D_UsSocioeconomico, all_of(cols_aversion))

inconsistent <- aversion_wide_raw %>%
  rowwise() %>%
  mutate(
    responses   = list(as.character(c_across(all_of(cols_aversion)))),
    has_zero    = any(responses == "0"),
    n_switches  = if_else(
      !has_zero,
      sum(diff(as.integer(c_across(all_of(cols_aversion)) == "A")) != 0),
      NA_integer_
    ),
    inconsistent = n_switches > 1
  ) %>%
  ungroup()

cat("\nNumber of choice switches per participant (consistent = 0 or 1):\n")
print(table(inconsistent$n_switches, useNA = "always"))

cat("\nInconsistent participants (more than one switch):",
    sum(inconsistent$inconsistent, na.rm = TRUE), "\n")
cat("Consistent participants:",
    sum(!inconsistent$inconsistent & !inconsistent$has_zero, na.rm = TRUE), "\n")

# =============================================================================
# 5. RISK AVERSION AND ENDOWMENT EFFECT
# =============================================================================
# Does risk aversion predict a stronger endowment effect?
# Endowment effect strength = SP(S1.1) - SP(B1.1) for each participant

data_wide <- data_wide %>%
  mutate(
    endowment_gap = sp_S1_1 - sp_B1_1
  )

cat("\nEndowment gap (S1.1 - B1.1) by risk type:\n")
gap_by_risk <- data_wide %>%
  filter(!is.na(risk_type), !is.na(endowment_gap)) %>%
  group_by(risk_type) %>%
  summarise(
    n      = n(),
    mean   = round(mean(endowment_gap), 1),
    median = median(endowment_gap),
    sd     = round(sd(endowment_gap), 1),
    .groups = "drop"
  )
print(gap_by_risk)

p_gap_risk <- ggplot(
  data_wide %>% filter(!is.na(risk_type), !is.na(endowment_gap)),
  aes(x = risk_type, y = endowment_gap)
) +
  geom_boxplot(fill = "#2c7bb6", alpha = 0.7, outlier.shape = 1) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "#d7191c", linewidth = 0.7) +
  labs(
    title    = "Endowment Effect Strength by Risk Type",
    subtitle = "Gap = SP(S1.1) - SP(B1.1). Positive values indicate endowment effect.",
    x = NULL, y = "Switching point gap (pesos)"
  ) +
  theme_base +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

print(p_gap_risk)

# Correlation between aversion score and endowment gap
cor_test <- cor.test(data_wide$aversion_score, data_wide$endowment_gap,
                     method = "spearman", use = "complete.obs")

cat("\nSpearman correlation: aversion score vs endowment gap\n")
cat(sprintf("  rho = %.3f\n", cor_test$estimate))
cat(sprintf("  p-value = %.4f\n", cor_test$p.value))
if (cor_test$p.value < 0.05) {
  cat("  Significant: higher risk aversion is associated with a larger endowment effect\n")
} else {
  cat("  Not significant: no clear relationship between risk aversion and endowment effect\n")
}
