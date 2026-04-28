# =============================================================================
# SCRIPT 01: DEMOGRAPHICS AND DESCRIPTIVE STATISTICS
# =============================================================================

cat("\014")
rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

# Base theme for plots
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
data_long <- readRDS("/Users/jeanmarroquin/Documents/Economia/asistente/dotacion/nueva_version/data/clean/data_long.rds")

cat("Participants:", nrow(data_wide), "\n")
cat("Observations (long):", nrow(data_long), "\n")

# =============================================================================
# 2. DEMOGRAPHICS
# =============================================================================

# --- Age ---
age_stats <- data_wide %>%
  summarise(
    n      = n(),
    mean   = round(mean(age, na.rm = TRUE), 1),
    median = median(age, na.rm = TRUE),
    sd     = round(sd(age, na.rm = TRUE), 1),
    min    = min(age, na.rm = TRUE),
    max    = max(age, na.rm = TRUE)
  )

cat("\nAge:\n")
print(age_stats)

p_age <- ggplot(data_wide, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "#2c7bb6", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(data_wide$age, na.rm = TRUE),
             color = "#d7191c", linetype = "dashed", linewidth = 0.8) +
  labs(
    title    = "Age Distribution",
    subtitle = paste0("Mean: ", round(mean(data_wide$age, na.rm = TRUE), 1), " years"),
    x = "Age", y = "Frequency"
  ) +
  theme_base

print(p_age)

# --- Gender ---
cat("\nGender:\n")
gender_tab <- data_wide %>%
  count(gender) %>%
  mutate(pct = round(100 * n / sum(n), 1))
print(gender_tab)

# --- Education ---
cat("\nEducation level:\n")
education_tab <- data_wide %>%
  count(education) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))
print(education_tab)

# --- Number of cars (SES proxy) ---
cat("\nNumber of cars (SES proxy):\n")
cars_tab <- data_wide %>%
  count(num_cars) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(num_cars)
print(cars_tab)

# =============================================================================
# 3. RISK AVERSION
# =============================================================================

cat("\nRisk aversion score (0 = most risk-seeking, 10 = most risk-averse):\n")
aversion_stats <- data_wide %>%
  filter(!is.na(aversion_score)) %>%
  summarise(
    n      = n(),
    mean   = round(mean(aversion_score), 2),
    median = median(aversion_score),
    sd     = round(sd(aversion_score), 2),
    min    = min(aversion_score),
    max    = max(aversion_score)
  )
print(aversion_stats)

cat("\nScore distribution:\n")
print(table(data_wide$aversion_score, useNA = "always"))

p_aversion <- ggplot(data_wide %>% filter(!is.na(aversion_score)),
                     aes(x = factor(aversion_score))) +
  geom_bar(fill = "#2c7bb6", alpha = 0.8) +
  labs(
    title    = "Risk Aversion Score Distribution",
    subtitle = "Holt-Laury: number of times participant chose the safe option (A)",
    x = "Score (0-10)", y = "Frequency"
  ) +
  theme_base

print(p_aversion)

# =============================================================================
# 4. SWITCHING POINTS BY TREATMENT
# =============================================================================

treatment_order  <- c("S1.1", "S1.2", "B1.1", "B1.2", "S2.1", "S2.2", "B2.1", "B2.2")

treatment_labels <- c(
  "S1.1" = "S1.1\nSeller\n(buyer has no mug)",
  "S1.2" = "S1.2\nSeller\n(buyer has mug)",
  "B1.1" = "B1.1\nBuyer\n(no mug owned)",
  "B1.2" = "B1.2\nBuyer\n(second mug)",
  "S2.1" = "S2.1\nBroker seller\n(owns mug)",
  "S2.2" = "S2.2\nBroker seller\n(no mug)",
  "B2.1" = "B2.1\nBroker buyer\n(owns mug)",
  "B2.2" = "B2.2\nBroker buyer\n(no mug)"
)

role_colors <- c(
  "S1.1" = "#2c7bb6", "S1.2" = "#2c7bb6",
  "B1.1" = "#d7191c", "B1.2" = "#d7191c",
  "S2.1" = "#74add1", "S2.2" = "#74add1",
  "B2.1" = "#f46d43", "B2.2" = "#f46d43"
)

data_long_plot <- data_long %>%
  filter(!is.na(switching_point)) %>%
  mutate(treatment = factor(treatment, levels = treatment_order))

# --- Summary statistics by treatment ---
cat("\nSwitching point statistics by treatment:\n")
sp_stats <- data_long_plot %>%
  group_by(treatment) %>%
  summarise(
    n            = n(),
    n_censored   = sum(switching_point == 275),
    mean         = round(mean(switching_point), 1),
    median       = median(switching_point),
    sd           = round(sd(switching_point), 1),
    min          = min(switching_point),
    max          = max(switching_point),
    .groups      = "drop"
  ) %>%
  mutate(pct_censored = round(100 * n_censored / n, 1))
print(sp_stats)

# --- Boxplot by treatment ---
p_box <- ggplot(data_long_plot,
                aes(x = treatment, y = switching_point, fill = treatment)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 1, outlier.size = 1.5) +
  geom_hline(yintercept = 275, linetype = "dotted",
             color = "gray40", linewidth = 0.6) +
  scale_fill_manual(values = role_colors) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(
    breaks = c(25, 75, 125, 175, 225, 275),
    labels = c("25", "75", "125", "175", "225", "275+")
  ) +
  labs(
    title    = "Switching Point Distribution by Treatment",
    subtitle = "275 = censored valuation (participant never accepted the transaction)",
    x = NULL, y = "Switching price (pesos)"
  ) +
  theme_base +
  theme(legend.position = "none")

print(p_box)

# --- Mean switching points with 95% CI ---
sp_ci <- data_long_plot %>%
  group_by(treatment) %>%
  summarise(
    mean = mean(switching_point),
    se   = sd(switching_point) / sqrt(n()),
    ci_low  = mean - 1.96 * se,
    ci_high = mean + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(treatment = factor(treatment, levels = treatment_order))

p_means <- ggplot(sp_ci, aes(x = treatment, y = mean, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.2, linewidth = 0.8) +
  geom_hline(yintercept = 275, linetype = "dotted",
             color = "gray40", linewidth = 0.6) +
  scale_color_manual(values = role_colors) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(
    breaks = c(25, 75, 125, 175, 225, 275),
    labels = c("25", "75", "125", "175", "225", "275+")
  ) +
  labs(
    title    = "Mean Switching Points by Treatment",
    subtitle = "95% confidence intervals",
    x = NULL, y = "Switching price (pesos)"
  ) +
  theme_base +
  theme(legend.position = "none")

print(p_means)

# --- Censored fraction by treatment ---
cat("\nFraction of participants with censored valuation (switching point = 275):\n")
print(sp_stats %>% dplyr::select(treatment, n, n_censored, pct_censored))
