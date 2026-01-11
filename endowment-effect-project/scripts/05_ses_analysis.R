# =============================================================================
# SOCIOECONOMIC STATUS (SES) ANALYSIS - ENDOWMENT EFFECT
# Combined analysis of SES index creation, descriptive statistics, 
# interaction plots, and probit regression models
# =============================================================================

# Clean environment
rm(list = ls())
cat("\014")

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)
library(psych)
library(ggcorrplot)
library(MASS)           # For probit models
library(margins)        # For marginal effects
library(modelsummary)   # For regression tables

# Read data
data <- read_excel("Base_completa.xlsx")

# =============================================================================
# PART 1: CONSTRUCT SES INDEX
# =============================================================================

cat("\n=== CONSTRUCTING SES INDEX ===\n\n")

cat("SES Index Components:\n")
cat("  1. Number of cars owned (0-3 points)\n")
cat("  2. Primary transportation method (0-2 points)\n")
cat("  3. Number of streaming platforms (0-3 points)\n")
cat("  Total range: 0-8 points\n\n")

# Check platform columns
platform_cols <- names(data)[grepl("TV|Netflix|Amazon|Disney|HBO", names(data))]
cat("Streaming platform columns identified:", length(platform_cols), "\n")
print(platform_cols)
cat("\n")

# Create SES index
data_ses <- data %>%
  mutate(
    # 1. CARS (0-3 points)
    ses_cars = case_when(
      is.na(AutosPosee) ~ NA_real_,
      AutosPosee == 0 ~ 0,
      AutosPosee == 1 ~ 1,
      AutosPosee == 2 ~ 2,
      AutosPosee >= 3 ~ 3,
      TRUE ~ NA_real_
    ),
    
    # 2. TRANSPORTATION (0-2 points)
    ses_transport = case_when(
      is.na(TransporteDescripcion) ~ NA_real_,
      TransporteDescripcion == "Automovil" ~ 2,
      TransporteDescripcion == "Taxi" ~ 1,
      TransporteDescripcion %in% c("Camión", "Metro", "Bicicleta", "Caminando") ~ 0,
      TRUE ~ 0
    ),
    
    # 3. STREAMING PLATFORMS (0-3 points)
    num_platforms = rowSums(across(all_of(platform_cols)) == "TRUE", na.rm = TRUE),
    
    ses_streaming = case_when(
      num_platforms >= 5 ~ 3,
      num_platforms >= 3 ~ 2,
      num_platforms >= 1 ~ 1,
      TRUE ~ 0
    ),
    
    # TOTAL INDEX (0-8 points)
    ses_index_raw = ses_cars + ses_transport + ses_streaming
  )

# Handle missing values and create categories
data_ses <- data_ses %>%
  mutate(
    # Impute missing with median
    ses_index = ifelse(is.na(ses_index_raw), 
                       median(ses_index_raw, na.rm = TRUE), 
                       ses_index_raw),
    
    # SES categories (3 groups)
    ses_level = case_when(
      ses_index <= 2 ~ "Low SES",
      ses_index <= 5 ~ "Medium SES",
      ses_index >= 6 ~ "High SES",
      TRUE ~ NA_character_
    ),
    
    # Factor with proper ordering
    ses_level = factor(ses_level, levels = c("Low SES", "Medium SES", "High SES")),
    
    # Binary versions
    high_ses = as.numeric(ses_index >= 6),
    low_ses = as.numeric(ses_index <= 2),
    
    # Centered version for regression
    ses_index_c = ses_index - mean(ses_index, na.rm = TRUE)
  )

# Display distribution
cat("SES Index Distribution (raw):\n")
print(summary(data_ses$ses_index_raw))
cat("\n")

cat("SES Level Distribution:\n")
table_ses <- data_ses %>%
  count(ses_level) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))
print(table_ses)
cat("\n")

# =============================================================================
# PART 2: VALIDATE SES INDEX
# =============================================================================

cat("=== SES INDEX VALIDATION ===\n\n")

# Correlation with education
data_ses <- data_ses %>%
  mutate(
    has_masters = as.numeric(grepl("Maestría|Maestria|Master", DescEstudio, ignore.case = TRUE)),
    has_phd = as.numeric(grepl("Doctorado|PhD", DescEstudio, ignore.case = TRUE)),
    advanced_education = has_masters + has_phd
  )

# Spearman correlation test
cor_education <- cor.test(data_ses$ses_index, data_ses$advanced_education, 
                         method = "spearman", use = "complete.obs")

cat("Validation: Correlation between SES Index and Education Level\n")
cat("Spearman's rho =", round(cor_education$estimate, 3), "\n")
cat("p-value =", format.pval(cor_education$p.value, digits = 3), "\n\n")

# Cross-tabulation: SES × Education
cat("Cross-tabulation: SES Level × Education\n")
table_ses_edu <- data_ses %>%
  mutate(
    Education = case_when(
      has_phd == 1 ~ "PhD",
      has_masters == 1 ~ "Master's",
      TRUE ~ "Other"
    )
  ) %>%
  count(ses_level, Education) %>%
  pivot_wider(names_from = Education, values_from = n, values_fill = 0)

print(table_ses_edu)
cat("\n")

# SES components by level
cat("SES Components by Level:\n")
components_ses <- data_ses %>%
  group_by(ses_level) %>%
  summarise(
    Mean_Cars = round(mean(ses_cars, na.rm = TRUE), 2),
    Mean_Transport = round(mean(ses_transport, na.rm = TRUE), 2),
    Mean_Platforms = round(mean(ses_streaming, na.rm = TRUE), 2),
    Mean_Total = round(mean(ses_index, na.rm = TRUE), 2),
    N = n(),
    .groups = 'drop'
  )

print(components_ses)
cat("\n")

# =============================================================================
# PART 3: VISUALIZATION - SES DISTRIBUTION
# =============================================================================

cat("=== GENERATING SES DISTRIBUTION PLOTS ===\n\n")

# SES index histogram
p_ses_dist <- ggplot(data_ses, aes(x = ses_index)) +
  geom_histogram(binwidth = 1, fill = "#3498db", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = median(ses_index, na.rm = TRUE)), 
             color = "#e74c3c", linetype = "dashed", size = 1) +
  labs(
    title = "SES Index Distribution",
    subtitle = paste("Median:", round(median(data_ses$ses_index, na.rm = TRUE), 1)),
    x = "SES Index (0-8)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11)
  )

print(p_ses_dist)

# SES components bar chart
p_components <- data_ses %>%
  filter(!is.na(ses_level)) %>%
  select(ses_level, ses_cars, ses_transport, ses_streaming) %>%
  pivot_longer(cols = c(ses_cars, ses_transport, ses_streaming),
               names_to = "Component",
               values_to = "Score") %>%
  mutate(Component = case_when(
    Component == "ses_cars" ~ "Cars",
    Component == "ses_transport" ~ "Transport",
    Component == "ses_streaming" ~ "Streaming",
    TRUE ~ Component
  )) %>%
  group_by(ses_level, Component) %>%
  summarise(Mean_Score = mean(Score, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = Component, y = Mean_Score, fill = ses_level)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_viridis_d(name = "SES Level") +
  labs(
    title = "SES Index Components by Level",
    x = "Component",
    y = "Average Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

print(p_components)

# =============================================================================
# PART 4: CALCULATE SWITCHING POINTS BY SES
# =============================================================================

cat("\n=== CALCULATING SWITCHING POINTS BY SES ===\n\n")

# Price vector
prices <- seq(20, 500, 20)

# Switching point function
find_switching_point <- function(responses, agent_type) {
  valid_responses <- responses[!is.na(responses) & responses != 0]
  if (length(valid_responses) == 0) return(NA)
  
  # All same response
  if (all(valid_responses == valid_responses[1])) {
    if (agent_type %in% c("A.1", "A.1.2", "A", "Alpha")) {
      return(ifelse(valid_responses[1] == 1, 500, 20))
    } else {
      return(ifelse(valid_responses[1] == 1, 20, 500))
    }
  }
  
  # Find transition point
  for (i in 1:(length(responses) - 1)) {
    if (!is.na(responses[i]) && !is.na(responses[i + 1]) && 
        responses[i] == 1 && responses[i + 1] == 2) {
      return(prices[i + 1])
    }
  }
  
  # Midpoint estimation
  first_2 <- which(responses == 2)[1]
  last_1 <- max(which(responses == 1), na.rm = TRUE)
  
  if (!is.na(first_2) && !is.na(last_1) && first_2 > last_1) {
    return((prices[last_1] + prices[first_2]) / 2)
  }
  
  return(NA)
}

# Function to add SES to switching points
add_ses <- function(switching_df, data_ses, filter_col, agent_type) {
  # Get original indices
  indices <- data_ses %>%
    mutate(row_id = row_number()) %>%
    filter(!!sym(filter_col) == agent_type) %>%
    pull(row_id)
  
  # Add SES variables
  switching_df %>%
    mutate(row_id = indices[row_number()]) %>%
    left_join(
      data_ses %>% 
        mutate(row_id = row_number()) %>%
        select(row_id, ses_index, ses_index_c, ses_level, high_ses, low_ses),
      by = "row_id"
    )
}

# Calculate switching points for all experiments

# EXPERIMENT 1.1
switching_a1_ses <- data_ses %>%
  filter(Formato == "Agente Tipo A.1") %>%
  select(IdRes1:IdRes25) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(IdRes1:IdRes25), "A.1")) %>%
  ungroup() %>%
  filter(!is.na(switching_point)) %>%
  add_ses(data_ses, "Formato", "Agente Tipo A.1") %>%
  mutate(role = "owner-seller", experiment = "Exp1.1")

switching_b1_ses <- data_ses %>%
  filter(Descrip9 == "Agente Tipo B.1") %>%
  select(Id9Res1:Id9Res25) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id9Res1:Id9Res25), "B.1")) %>%
  ungroup() %>%
  filter(!is.na(switching_point)) %>%
  add_ses(data_ses, "Descrip9", "Agente Tipo B.1") %>%
  mutate(role = "nonowner-buyer", experiment = "Exp1.1")

# EXPERIMENT 1.2
switching_a12_ses <- data_ses %>%
  filter(Descrip10 == "Agente Tipo A.1") %>%
  select(Id10Res1:Id10Res25) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id10Res1:Id10Res25), "A.1.2")) %>%
  ungroup() %>%
  filter(!is.na(switching_point)) %>%
  add_ses(data_ses, "Descrip10", "Agente Tipo A.1") %>%
  mutate(role = "owner-buyer", experiment = "Exp1.2")

switching_b12_ses <- data_ses %>%
  filter(Descrip11 == "Agente Tipo B.1") %>%
  select(Id11Res1:Id11Res25) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id11Res1:Id11Res25), "B.1.2")) %>%
  ungroup() %>%
  filter(!is.na(switching_point)) %>%
  add_ses(data_ses, "Descrip11", "Agente Tipo B.1") %>%
  mutate(role = "nonowner-buyer-package", experiment = "Exp1.2")

# EXPERIMENT 2
switching_a_ses <- data_ses %>%
  filter(Descrip18 == "Agente Tipo A") %>%
  select(Id18Res1:Id18Res25) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id18Res1:Id18Res25), "A")) %>%
  ungroup() %>%
  filter(!is.na(switching_point)) %>%
  add_ses(data_ses, "Descrip18", "Agente Tipo A") %>%
  mutate(role = "owner-seller-broker", experiment = "Exp2")

switching_alpha_ses <- data_ses %>%
  filter(Descrip13 == "Agente Tipo Alpha") %>%
  select(Id13Res1:Id13Res25) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id13Res1:Id13Res25), "Alpha")) %>%
  ungroup() %>%
  filter(!is.na(switching_point)) %>%
  add_ses(data_ses, "Descrip13", "Agente Tipo Alpha") %>%
  mutate(role = "nonowner-seller-broker", experiment = "Exp2")

switching_b_ses <- data_ses %>%
  filter(Descrip20 == "Agente Tipo B") %>%
  select(Id20Res1:Id20Res25) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id20Res1:Id20Res25), "B")) %>%
  ungroup() %>%
  filter(!is.na(switching_point)) %>%
  add_ses(data_ses, "Descrip20", "Agente Tipo B") %>%
  mutate(role = "owner-buyer-broker", experiment = "Exp2")

switching_beta_ses <- data_ses %>%
  filter(Descrip17 == "Agente Tipo Beta") %>%
  select(Id17Res1:Id17Res25) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id17Res1:Id17Res25), "Beta")) %>%
  ungroup() %>%
  filter(!is.na(switching_point)) %>%
  add_ses(data_ses, "Descrip17", "Agente Tipo Beta") %>%
  mutate(role = "nonowner-buyer-broker", experiment = "Exp2")

cat("Switching points calculated successfully for all experiments\n\n")

# =============================================================================
# PART 5: DESCRIPTIVE STATISTICS BY SES
# =============================================================================

cat("=== DESCRIPTIVE STATISTICS BY SES LEVEL ===\n\n")

# Experiment 1.1
cat("EXPERIMENT 1.1: Statistics by SES Level\n")
stats_11_ses <- bind_rows(switching_a1_ses, switching_b1_ses) %>%
  filter(!is.na(ses_level)) %>%
  group_by(role, ses_level) %>%
  summarise(
    Mean = round(mean(switching_point, na.rm = TRUE), 1),
    SD = round(sd(switching_point, na.rm = TRUE), 1),
    Median = round(median(switching_point, na.rm = TRUE), 1),
    N = n(),
    .groups = 'drop'
  )
print(stats_11_ses)
cat("\n")

# Experiment 1.2
cat("EXPERIMENT 1.2: Statistics by SES Level\n")
stats_12_ses <- bind_rows(switching_a12_ses, switching_b12_ses) %>%
  filter(!is.na(ses_level)) %>%
  group_by(role, ses_level) %>%
  summarise(
    Mean = round(mean(switching_point, na.rm = TRUE), 1),
    SD = round(sd(switching_point, na.rm = TRUE), 1),
    Median = round(median(switching_point, na.rm = TRUE), 1),
    N = n(),
    .groups = 'drop'
  )
print(stats_12_ses)
cat("\n")

# Experiment 2
cat("EXPERIMENT 2: Statistics by SES Level\n")
stats_2_ses <- bind_rows(switching_a_ses, switching_alpha_ses, 
                         switching_b_ses, switching_beta_ses) %>%
  filter(!is.na(ses_level)) %>%
  group_by(role, ses_level) %>%
  summarise(
    Mean = round(mean(switching_point, na.rm = TRUE), 1),
    SD = round(sd(switching_point, na.rm = TRUE), 1),
    Median = round(median(switching_point, na.rm = TRUE), 1),
    N = n(),
    .groups = 'drop'
  )
print(stats_2_ses)
cat("\n")

# =============================================================================
# PART 6: SES × ROLE INTERACTION PLOTS
# =============================================================================

cat("=== GENERATING SES × ROLE INTERACTION PLOTS ===\n\n")

# Experiment 1.1
data_11_ses <- bind_rows(switching_a1_ses, switching_b1_ses) %>%
  filter(!is.na(ses_level))

stats_11_plot <- data_11_ses %>%
  group_by(role, ses_level) %>%
  summarise(
    mean = mean(switching_point, na.rm = TRUE),
    se = sd(switching_point, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

p1_ses_interact <- ggplot(stats_11_plot, aes(x = role, y = mean, 
                                              color = ses_level, group = ses_level)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_color_manual(
    values = c("Low SES" = "#E74C3C", "Medium SES" = "#F39C12", "High SES" = "#27AE60"),
    name = "SES Level"
  ) +
  labs(
    title = "Experiment 1.1: SES × Role Interaction",
    subtitle = "Mean Switching Point by Role and SES Level",
    x = "Role",
    y = "Switching Point (mean ± SE)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

print(p1_ses_interact)

# Experiment 1.2
data_12_ses <- bind_rows(switching_a12_ses, switching_b12_ses) %>%
  filter(!is.na(ses_level))

stats_12_plot <- data_12_ses %>%
  group_by(role, ses_level) %>%
  summarise(
    mean = mean(switching_point, na.rm = TRUE),
    se = sd(switching_point, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

p2_ses_interact <- ggplot(stats_12_plot, aes(x = role, y = mean, 
                                              color = ses_level, group = ses_level)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_color_manual(
    values = c("Low SES" = "#E74C3C", "Medium SES" = "#F39C12", "High SES" = "#27AE60"),
    name = "SES Level"
  ) +
  labs(
    title = "Experiment 1.2: SES × Role Interaction",
    subtitle = "Mean Switching Point by Role and SES Level",
    x = "Role",
    y = "Switching Point (mean ± SE)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

print(p2_ses_interact)

# Experiment 2
data_2_ses <- bind_rows(switching_a_ses, switching_alpha_ses, 
                        switching_b_ses, switching_beta_ses) %>%
  filter(!is.na(ses_level))

stats_2_plot <- data_2_ses %>%
  group_by(role, ses_level) %>%
  summarise(
    mean = mean(switching_point, na.rm = TRUE),
    se = sd(switching_point, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

p3_ses_interact <- ggplot(stats_2_plot, aes(x = role, y = mean, 
                                            color = ses_level, group = ses_level)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_color_manual(
    values = c("Low SES" = "#E74C3C", "Medium SES" = "#F39C12", "High SES" = "#27AE60"),
    name = "SES Level"
  ) +
  scale_x_discrete(labels = function(x) gsub("-", "-\n", x)) +
  labs(
    title = "Experiment 2: SES × Role Interaction (Brokers)",
    subtitle = "Mean Switching Point by Role and SES Level",
    x = "Role",
    y = "Switching Point (mean ± SE)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

print(p3_ses_interact)

# =============================================================================
# PART 7: STATISTICAL TESTS BY SES
# =============================================================================

cat("\n=== STATISTICAL TESTS BY SES LEVEL ===\n\n")

# Function for tests by SES level
test_by_ses <- function(data_owner, data_nonowner, ses_levels) {
  results <- data.frame()
  
  for(ses in ses_levels) {
    owner_data <- data_owner %>% filter(ses_level == ses) %>% pull(switching_point)
    nonowner_data <- data_nonowner %>% filter(ses_level == ses) %>% pull(switching_point)
    
    if(length(owner_data) >= 5 & length(nonowner_data) >= 5) {
      test <- wilcox.test(owner_data, nonowner_data, alternative = "greater")
      
      results <- bind_rows(results, data.frame(
        SES_Level = ses,
        N_Owner = length(owner_data),
        N_Nonowner = length(nonowner_data),
        W_statistic = round(test$statistic, 1),
        p_value = round(test$p.value, 4),
        Significant = ifelse(test$p.value < 0.05, "***", 
                            ifelse(test$p.value < 0.10, "*", "ns"))
      ))
    }
  }
  return(results)
}

# Test Experiment 1.1
cat("EXPERIMENT 1.1: Wilcoxon Tests by SES Level\n")
cat("H1: owner-seller > nonowner-buyer\n\n")

test_11_ses <- test_by_ses(switching_a1_ses, switching_b1_ses, 
                           c("Low SES", "Medium SES", "High SES"))
print(test_11_ses)
cat("\n")

# Test Experiment 1.2
cat("EXPERIMENT 1.2: Wilcoxon Tests by SES Level\n")
cat("H1: nonowner-buyer-package > owner-buyer\n\n")

test_12_ses <- test_by_ses(switching_b12_ses, switching_a12_ses, 
                           c("Low SES", "Medium SES", "High SES"))
print(test_12_ses)
cat("\n")

# Two-way ANOVA: Role × SES
cat("TWO-WAY ANOVA: Role × SES Interaction\n\n")

cat("Experiment 1.1:\n")
anova_11 <- aov(switching_point ~ role * ses_level, data = data_11_ses)
print(summary(anova_11))
cat("\n")

cat("Experiment 1.2:\n")
anova_12 <- aov(switching_point ~ role * ses_level, data = data_12_ses)
print(summary(anova_12))
cat("\n")

# =============================================================================
# PART 8: BOXPLOTS BY SES
# =============================================================================

cat("=== GENERATING BOXPLOTS BY SES ===\n\n")

# Experiment 1.1
p_box_11 <- ggplot(data_11_ses, aes(x = ses_level, y = switching_point, fill = role)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(
    values = c("owner-seller" = "#3498DB", "nonowner-buyer" = "#FF69B4"),
    name = "Role"
  ) +
  labs(
    title = "Experiment 1.1: Distribution by SES Level",
    x = "SES Level",
    y = "Switching Point (Pesos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom"
  )

print(p_box_11)

# Experiment 1.2
p_box_12 <- ggplot(data_12_ses, aes(x = ses_level, y = switching_point, fill = role)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(
    values = c("owner-buyer" = "#3498DB", "nonowner-buyer-package" = "#FF69B4"),
    name = "Role"
  ) +
  labs(
    title = "Experiment 1.2: Distribution by SES Level",
    x = "SES Level",
    y = "Switching Point (Pesos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom"
  )

print(p_box_12)

# =============================================================================
# PART 9: PREPARE DATA FOR PROBIT MODELS
# =============================================================================

cat("\n=== PREPARING DATA FOR PROBIT REGRESSION ===\n\n")

# Function to prepare long format data for probit
prepare_probit_data <- function(data_wide, id_cols, response_cols, agent_type, experiment) {
  data_long <- data_wide %>%
    select(all_of(c(id_cols, response_cols))) %>%
    mutate(participant = row_number()) %>%
    pivot_longer(
      cols = all_of(response_cols),
      names_to = "question",
      values_to = "response"
    ) %>%
    mutate(
      price = rep(prices, length.out = n()),
      # Indicator: 1 = chooses good/mug, 0 = chooses money
      chooses_mug = case_when(
        agent_type %in% c("A.1", "A.1.2", "A", "Alpha") ~ as.numeric(response == 1),
        agent_type %in% c("B.1", "B.1.2", "B", "Beta") ~ as.numeric(response == 1),
        TRUE ~ NA_real_
      ),
      agent_type = agent_type,
      experiment = experiment
    ) %>%
    filter(!is.na(response), response != 0, !is.na(chooses_mug))
  
  return(data_long)
}

# EXPERIMENT 1.1 - Long format data

# A.1 (Owner - Sells)
data_a1_wide <- data_ses %>%
  filter(Formato == "Agente Tipo A.1") %>%
  select(IdRes1:IdRes25, Edad, DescGenero, DescEstudio, 
         ses_index, ses_index_c, high_ses, low_ses)

data_a1_long <- prepare_probit_data(
  data_a1_wide,
  id_cols = c("Edad", "DescGenero", "DescEstudio", "ses_index", "ses_index_c", "high_ses", "low_ses"),
  response_cols = paste0("IdRes", 1:25),
  agent_type = "A.1",
  experiment = "Exp1.1"
)

# B.1 (Nonowner - Buyer)
data_b1_wide <- data_ses %>%
  filter(Descrip9 == "Agente Tipo B.1") %>%
  select(Id9Res1:Id9Res25, Edad, DescGenero, DescEstudio, 
         ses_index, ses_index_c, high_ses, low_ses)

data_b1_long <- prepare_probit_data(
  data_b1_wide,
  id_cols = c("Edad", "DescGenero", "DescEstudio", "ses_index", "ses_index_c", "high_ses", "low_ses"),
  response_cols = paste0("Id9Res", 1:25),
  agent_type = "B.1",
  experiment = "Exp1.1"
)

# Combine and prepare
data_exp11_probit <- bind_rows(data_a1_long, data_b1_long) %>%
  mutate(
    owner = as.numeric(agent_type == "A.1"),
    female = as.numeric(DescGenero == "Femenino"),
    masters = as.numeric(grepl("Master|Maestr", DescEstudio, ignore.case = TRUE)),
    age_c = as.numeric(Edad) - mean(as.numeric(Edad), na.rm = TRUE)
  )

# EXPERIMENT 1.2 - Long format data

# A.1.2 (Owner - Buys 2nd)
data_a12_wide <- data_ses %>%
  filter(Descrip10 == "Agente Tipo A.1") %>%
  select(Id10Res1:Id10Res25, Edad, DescGenero, DescEstudio, 
         ses_index, ses_index_c, high_ses, low_ses)

data_a12_long <- prepare_probit_data(
  data_a12_wide,
  id_cols = c("Edad", "DescGenero", "DescEstudio", "ses_index", "ses_index_c", "high_ses", "low_ses"),
  response_cols = paste0("Id10Res", 1:25),
  agent_type = "A.1.2",
  experiment = "Exp1.2"
)

# B.1.2 (Nonowner - Buys package)
data_b12_wide <- data_ses %>%
  filter(Descrip11 == "Agente Tipo B.1") %>%
  select(Id11Res1:Id11Res25, Edad, DescGenero, DescEstudio, 
         ses_index, ses_index_c, high_ses, low_ses)

data_b12_long <- prepare_probit_data(
  data_b12_wide,
  id_cols = c("Edad", "DescGenero", "DescEstudio", "ses_index", "ses_index_c", "high_ses", "low_ses"),
  response_cols = paste0("Id11Res", 1:25),
  agent_type = "B.1.2",
  experiment = "Exp1.2"
)

# Combine
data_exp12_probit <- bind_rows(data_a12_long, data_b12_long) %>%
  mutate(
    owner = as.numeric(agent_type == "A.1.2"),
    female = as.numeric(DescGenero == "Femenino"),
    masters = as.numeric(grepl("Master|Maestr", DescEstudio, ignore.case = TRUE)),
    age_c = as.numeric(Edad) - mean(as.numeric(Edad), na.rm = TRUE)
  )

# EXPERIMENT 2 - Long format data

# A (Seller Broker w/ Cup)
data_a_wide <- data_ses %>%
  filter(Descrip18 == "Agente Tipo A") %>%
  select(Id18Res1:Id18Res25, Edad, DescGenero, DescEstudio, 
         ses_index, ses_index_c, high_ses, low_ses)

data_a_long <- prepare_probit_data(
  data_a_wide,
  id_cols = c("Edad", "DescGenero", "DescEstudio", "ses_index", "ses_index_c", "high_ses", "low_ses"),
  response_cols = paste0("Id18Res", 1:25),
  agent_type = "A",
  experiment = "Exp2"
)

# B (Buyer Broker w/ Cup)
data_b_wide <- data_ses %>%
  filter(Descrip20 == "Agente Tipo B") %>%
  select(Id20Res1:Id20Res25, Edad, DescGenero, DescEstudio, 
         ses_index, ses_index_c, high_ses, low_ses)

data_b_long <- prepare_probit_data(
  data_b_wide,
  id_cols = c("Edad", "DescGenero", "DescEstudio", "ses_index", "ses_index_c", "high_ses", "low_ses"),
  response_cols = paste0("Id20Res", 1:25),
  agent_type = "B",
  experiment = "Exp2"
)

# Alpha (Seller Broker w/o Cup)
data_alpha_wide <- data_ses %>%
  filter(Descrip13 == "Agente Tipo Alpha") %>%
  select(Id13Res1:Id13Res25, Edad, DescGenero, DescEstudio, 
         ses_index, ses_index_c, high_ses, low_ses)

data_alpha_long <- prepare_probit_data(
  data_alpha_wide,
  id_cols = c("Edad", "DescGenero", "DescEstudio", "ses_index", "ses_index_c", "high_ses", "low_ses"),
  response_cols = paste0("Id13Res", 1:25),
  agent_type = "Alpha",
  experiment = "Exp2"
)

# Beta (Buyer Broker w/o Cup)
data_beta_wide <- data_ses %>%
  filter(Descrip17 == "Agente Tipo Beta") %>%
  select(Id17Res1:Id17Res25, Edad, DescGenero, DescEstudio, 
         ses_index, ses_index_c, high_ses, low_ses)

data_beta_long <- prepare_probit_data(
  data_beta_wide,
  id_cols = c("Edad", "DescGenero", "DescEstudio", "ses_index", "ses_index_c", "high_ses", "low_ses"),
  response_cols = paste0("Id17Res", 1:25),
  agent_type = "Beta",
  experiment = "Exp2"
)

# Combine
data_exp2_probit <- bind_rows(data_a_long, data_b_long, data_alpha_long, data_beta_long) %>%
  mutate(
    broker_owner = as.numeric(agent_type %in% c("A", "B")),
    seller_broker = as.numeric(agent_type %in% c("A", "Alpha")),
    female = as.numeric(DescGenero == "Femenino"),
    masters = as.numeric(grepl("Master|Maestr", DescEstudio, ignore.case = TRUE)),
    age_c = as.numeric(Edad) - mean(as.numeric(Edad), na.rm = TRUE)
  )

cat("Probit data prepared successfully\n")
cat("  Exp 1.1:", nrow(data_exp11_probit), "observations\n")
cat("  Exp 1.2:", nrow(data_exp12_probit), "observations\n")
cat("  Exp 2:  ", nrow(data_exp2_probit), "observations\n\n")

# =============================================================================
# PART 10: PROBIT MODELS - EXPERIMENT 1.1
# =============================================================================

cat("=== FITTING PROBIT MODELS - EXPERIMENT 1.1 ===\n\n")

# Model 1: Baseline (price + owner only)
probit_11_m1 <- glm(chooses_mug ~ price + owner, 
                    family = binomial(link = "probit"), 
                    data = data_exp11_probit)

# Model 2: Add demographic controls (no SES)
probit_11_m2 <- glm(chooses_mug ~ price + owner + female + masters + age_c, 
                    family = binomial(link = "probit"), 
                    data = data_exp11_probit)

# Model 3: Add SES (continuous, centered)
probit_11_m3 <- glm(chooses_mug ~ price + owner + female + masters + age_c + ses_index_c, 
                    family = binomial(link = "probit"), 
                    data = data_exp11_probit)

# Model 4: SES binary (High SES)
probit_11_m4 <- glm(chooses_mug ~ price + owner + female + masters + age_c + high_ses, 
                    family = binomial(link = "probit"), 
                    data = data_exp11_probit)

# Model 5: Owner × SES interaction
probit_11_m5 <- glm(chooses_mug ~ price + owner + female + masters + age_c + ses_index_c + owner:ses_index_c, 
                    family = binomial(link = "probit"), 
                    data = data_exp11_probit)

# Regression table
cat("REGRESSION RESULTS - EXPERIMENT 1.1\n\n")

modelsummary(
  list("M1" = probit_11_m1, 
       "M2" = probit_11_m2, 
       "M3" = probit_11_m3, 
       "M4" = probit_11_m4, 
       "M5" = probit_11_m5),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_rename = c(
    "price" = "Price",
    "owner" = "Owner",
    "female" = "Female",
    "masters" = "Master's Degree",
    "age_c" = "Age (centered)",
    "ses_index_c" = "SES Index (centered)",
    "high_ses" = "High SES",
    "owner:ses_index_c" = "Owner × SES"
  ),
  gof_map = c("nobs", "logLik", "AIC"),
  title = "Probit Models - Experiment 1.1 with SES Controls"
)

# =============================================================================
# PART 11: PROBIT MODELS - EXPERIMENT 1.2
# =============================================================================

cat("\n=== FITTING PROBIT MODELS - EXPERIMENT 1.2 ===\n\n")

# Model 1: Baseline
probit_12_m1 <- glm(chooses_mug ~ price + owner, 
                    family = binomial(link = "probit"), 
                    data = data_exp12_probit)

# Model 2: Demographic controls
probit_12_m2 <- glm(chooses_mug ~ price + owner + female + masters + age_c, 
                    family = binomial(link = "probit"), 
                    data = data_exp12_probit)

# Model 3: With SES continuous
probit_12_m3 <- glm(chooses_mug ~ price + owner + female + masters + age_c + ses_index_c, 
                    family = binomial(link = "probit"), 
                    data = data_exp12_probit)

# Model 4: SES binary
probit_12_m4 <- glm(chooses_mug ~ price + owner + female + masters + age_c + high_ses, 
                    family = binomial(link = "probit"), 
                    data = data_exp12_probit)

# Model 5: Owner × SES interaction
probit_12_m5 <- glm(chooses_mug ~ price + owner + female + masters + age_c + ses_index_c + owner:ses_index_c, 
                    family = binomial(link = "probit"), 
                    data = data_exp12_probit)

# Regression table
cat("REGRESSION RESULTS - EXPERIMENT 1.2\n\n")

modelsummary(
  list("M1" = probit_12_m1, 
       "M2" = probit_12_m2, 
       "M3" = probit_12_m3, 
       "M4" = probit_12_m4, 
       "M5" = probit_12_m5),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_rename = c(
    "price" = "Price",
    "owner" = "Owner",
    "female" = "Female",
    "masters" = "Master's Degree",
    "age_c" = "Age (centered)",
    "ses_index_c" = "SES Index (centered)",
    "high_ses" = "High SES",
    "owner:ses_index_c" = "Owner × SES"
  ),
  gof_map = c("nobs", "logLik", "AIC"),
  title = "Probit Models - Experiment 1.2 with SES Controls"
)

# =============================================================================
# PART 12: PROBIT MODELS - EXPERIMENT 2
# =============================================================================

cat("\n=== FITTING PROBIT MODELS - EXPERIMENT 2 ===\n\n")

# Model 1: Baseline
probit_2_m1 <- glm(chooses_mug ~ price + broker_owner + seller_broker, 
                   family = binomial(link = "probit"), 
                   data = data_exp2_probit)

# Model 2: Demographic controls
probit_2_m2 <- glm(chooses_mug ~ price + broker_owner + seller_broker + female + masters + age_c, 
                   family = binomial(link = "probit"), 
                   data = data_exp2_probit)

# Model 3: With SES continuous
probit_2_m3 <- glm(chooses_mug ~ price + broker_owner + seller_broker + female + masters + age_c + ses_index_c, 
                   family = binomial(link = "probit"), 
                   data = data_exp2_probit)

# Model 4: SES binary
probit_2_m4 <- glm(chooses_mug ~ price + broker_owner + seller_broker + female + masters + age_c + high_ses, 
                   family = binomial(link = "probit"), 
                   data = data_exp2_probit)

# Model 5: Broker_Owner × SES interaction
probit_2_m5 <- glm(chooses_mug ~ price + broker_owner + seller_broker + female + masters + age_c + ses_index_c + broker_owner:ses_index_c, 
                   family = binomial(link = "probit"), 
                   data = data_exp2_probit)

# Regression table
cat("REGRESSION RESULTS - EXPERIMENT 2\n\n")

modelsummary(
  list("M1" = probit_2_m1, 
       "M2" = probit_2_m2, 
       "M3" = probit_2_m3, 
       "M4" = probit_2_m4, 
       "M5" = probit_2_m5),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_rename = c(
    "price" = "Price",
    "broker_owner" = "Broker Owns Cup",
    "seller_broker" = "Seller Broker",
    "female" = "Female",
    "masters" = "Master's Degree",
    "age_c" = "Age (centered)",
    "ses_index_c" = "SES Index (centered)",
    "high_ses" = "High SES",
    "broker_owner:ses_index_c" = "Broker Owns × SES"
  ),
  gof_map = c("nobs", "logLik", "AIC"),
  title = "Probit Models - Experiment 2 with SES Controls (Brokers)"
)

# =============================================================================
# PART 13: MARGINAL EFFECTS
# =============================================================================

cat("\n=== MARGINAL EFFECTS ===\n\n")

cat("Experiment 1.1 - Model 3 (with SES)\n")
mfx_11 <- margins(probit_11_m3)
print(summary(mfx_11))
cat("\n")

cat("Experiment 1.2 - Model 3 (with SES)\n")
mfx_12 <- margins(probit_12_m3)
print(summary(mfx_12))
cat("\n")

cat("Experiment 2 - Model 3 (with SES)\n")
mfx_2 <- margins(probit_2_m3)
print(summary(mfx_2))
cat("\n")

# =============================================================================
# PART 14: PREDICTED PROBABILITIES BY SES
# =============================================================================

cat("=== GENERATING PREDICTED PROBABILITY CURVES BY SES ===\n\n")

# Function to create predictions by SES
create_pred_ses <- function(model, data, var_owner, ses_values = c(1, 4, 7), 
                            include_seller_broker = FALSE) {
  pred_data <- expand.grid(
    price = seq(20, 500, 20),
    owner_val = c(0, 1),
    ses_val = ses_values
  )
  
  # Add control variables at means
  pred_data$female <- mean(data$female, na.rm = TRUE)
  pred_data$masters <- mean(data$masters, na.rm = TRUE)
  pred_data$age_c <- 0  # Already centered
  pred_data$ses_index_c <- pred_data$ses_val - mean(data$ses_index, na.rm = TRUE)
  
  # If Experiment 2, add seller_broker
  if(include_seller_broker) {
    pred_data$seller_broker <- mean(data$seller_broker, na.rm = TRUE)
  }
  
  # Rename owner variable
  names(pred_data)[2] <- var_owner
  
  # Predictions
  pred_data$prob <- predict(model, newdata = pred_data, type = "response")
  
  # SES labels
  pred_data$ses_label <- factor(pred_data$ses_val,
                                levels = c(1, 4, 7),
                                labels = c("Low SES", "Medium SES", "High SES"))
  
  return(pred_data)
}

# Experiment 1.1
cat("Generating predictions for Experiment 1.1...\n")

pred_11_ses <- create_pred_ses(probit_11_m3, data_exp11_probit, "owner")

p1_pred_ses <- ggplot(pred_11_ses, aes(x = price, y = prob, 
                                        color = factor(owner), 
                                        linetype = ses_label,
                                        group = interaction(owner, ses_label))) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("0" = "#E74C3C", "1" = "#3498DB"),
    labels = c("Non-owner", "Owner"),
    name = "Role"
  ) +
  scale_linetype_manual(
    values = c("Low SES" = "dashed", "Medium SES" = "solid", "High SES" = "dotted"),
    name = "SES Level"
  ) +
  labs(
    title = "Experiment 1.1: Predicted Probabilities by SES",
    subtitle = "Probability of Choosing Mug vs Price",
    x = "Price (Pesos)",
    y = "P(Choose Mug)"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  )

print(p1_pred_ses)

# Experiment 1.2
cat("Generating predictions for Experiment 1.2...\n")

pred_12_ses <- create_pred_ses(probit_12_m3, data_exp12_probit, "owner")

p2_pred_ses <- ggplot(pred_12_ses, aes(x = price, y = prob, 
                                        color = factor(owner), 
                                        linetype = ses_label,
                                        group = interaction(owner, ses_label))) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("0" = "#E74C3C", "1" = "#3498DB"),
    labels = c("Non-owner", "Owner"),
    name = "Role"
  ) +
  scale_linetype_manual(
    values = c("Low SES" = "dashed", "Medium SES" = "solid", "High SES" = "dotted"),
    name = "SES Level"
  ) +
  labs(
    title = "Experiment 1.2: Predicted Probabilities by SES",
    subtitle = "Probability of Choosing Mug vs Price",
    x = "Price (Pesos)",
    y = "P(Choose Mug)"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  )

print(p2_pred_ses)

# Experiment 2 - Brokers
cat("Generating predictions for Experiment 2 (Brokers)...\n")

pred_2_ses <- create_pred_ses(probit_2_m3, data_exp2_probit, "broker_owner", 
                              include_seller_broker = TRUE)

p3_pred_ses <- ggplot(pred_2_ses, aes(x = price, y = prob, 
                                       color = factor(broker_owner), 
                                       linetype = ses_label,
                                       group = interaction(broker_owner, ses_label))) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("0" = "#E74C3C", "1" = "#3498DB"),
    labels = c("Broker without Cup", "Broker with Cup"),
    name = "Role"
  ) +
  scale_linetype_manual(
    values = c("Low SES" = "dashed", "Medium SES" = "solid", "High SES" = "dotted"),
    name = "SES Level"
  ) +
  labs(
    title = "Experiment 2: Predicted Probabilities by SES (Brokers)",
    subtitle = "Probability of Choosing Mug vs Price",
    x = "Price (Pesos)",
    y = "P(Choose Mug)"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  )

print(p3_pred_ses)
