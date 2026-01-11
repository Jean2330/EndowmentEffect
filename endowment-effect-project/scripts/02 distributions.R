# =============================================================================
# SWITCHING POINTS ANALYSIS: 
# Calculation, Normality Tests, and Group Comparisons
# =============================================================================

rm(list = ls())
cat("\014")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(car)
library(nortest)
library(moments)

# Load data (Change as neccesary)
data <- read_excel("/Users/jeanmarroquin/Documents/Economia/asistente/dotacion/endowment-effect-project/data/raw/data.xlsx")
prices <- seq(20, 500, 20)

# =============================================================================
# AUXILIARY FUNCTIONS
# =============================================================================

find_switching_point <- function(responses, agent_type) {
  valid_responses <- responses[!is.na(responses) & responses != 0]
  if (length(valid_responses) == 0) return(NA)
  
  if (all(valid_responses == valid_responses[1])) {
    if (agent_type %in% c("A.1", "A.1.2")) {
      return(ifelse(valid_responses[1] == 1, 500, 20))
    } else {
      return(ifelse(valid_responses[1] == 1, 20, 500))
    }
  }
  
  for (i in 1:(length(responses) - 1)) {
    if (!is.na(responses[i]) && !is.na(responses[i + 1]) && 
        responses[i] == 1 && responses[i + 1] == 2) {
      return(prices[i + 1])
    }
  }
  
  first_2 <- which(responses == 2)[1]
  last_1 <- max(which(responses == 1), na.rm = TRUE)
  if (!is.na(first_2) && !is.na(last_1) && first_2 > last_1) {
    return((prices[last_1] + prices[first_2]) / 2)
  }
  return(NA)
}

test_normality <- function(data, group_name, verbose = FALSE) {
  if (verbose) cat("### ", group_name, " (N = ", length(data), ") ###\n", sep="")
  
  sw <- shapiro.test(data)
  ad <- ad.test(data)
  
  if (verbose) {
    cat("  Shapiro-Wilk: W =", round(sw$statistic, 4), ", p =", round(sw$p.value, 4), "\n")
    cat("  Anderson-Darling: A =", round(ad$statistic, 4), ", p =", round(ad$p.value, 4), "\n")
  }
  
  return(sw$p.value > 0.05 && ad$p.value > 0.05)
}

decide_test <- function(normality_groups, homogeneity_p) {
  all_normal <- all(unlist(normality_groups))
  homogeneous_var <- homogeneity_p > 0.05
  
  if (all_normal && homogeneous_var) {
    return("ANOVA")
  } else if (all_normal && !homogeneous_var) {
    return("Welch")
  } else {
    return("Kruskal-Wallis")
  }
}

# =============================================================================
# SWITCHING POINTS CALCULATION
# =============================================================================

switching_a1 <- data %>%
  filter(Formato == "Agente Tipo A.1") %>%
  dplyr::select(IdRes1:IdRes25) %>%
  mutate(participant = row_number()) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(IdRes1:IdRes25), "A.1")) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point) %>%
  mutate(role = "Owner-Seller", experiment = "Exp1.1")

switching_b1 <- data %>%
  filter(Descrip9 == "Agente Tipo B.1") %>%
  dplyr::select(Id9Res1:Id9Res25) %>%
  mutate(participant = row_number()) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id9Res1:Id9Res25), "B.1")) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point) %>%
  mutate(role = "Nonowner-Buyer", experiment = "Exp1.1")

switching_a1_2 <- data %>%
  filter(Descrip10 == "Agente Tipo A.1") %>%
  dplyr::select(Id10Res1:Id10Res25) %>%
  mutate(participant = row_number()) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id10Res1:Id10Res25), "A.1.2")) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point) %>%
  mutate(role = "Owner-Buyer-2nd", experiment = "Exp1.2")

switching_b1_2 <- data %>%
  filter(Descrip11 == "Agente Tipo B.1") %>%
  dplyr::select(Id11Res1:Id11Res25) %>%
  mutate(participant = row_number()) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id11Res1:Id11Res25), "B.1.2")) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point) %>%
  mutate(role = "Nonowner-Buyer-Pkg", experiment = "Exp1.2")

switching_a <- data %>%
  filter(Descrip18 == "Agente Tipo A") %>%
  dplyr::select(Id18Res1:Id18Res25) %>%
  mutate(participant = row_number()) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id18Res1:Id18Res25), "A")) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point) %>%
  mutate(role = "Owner-Seller", experiment = "Exp2")

switching_b <- data %>%
  filter(Descrip20 == "Agente Tipo B") %>%
  dplyr::select(Id20Res1:Id20Res25) %>%
  mutate(participant = row_number()) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id20Res1:Id20Res25), "B")) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point) %>%
  mutate(role = "Owner-Buyer", experiment = "Exp2")

switching_alpha <- data %>%
  filter(Descrip13 == "Agente Tipo Alpha") %>%
  dplyr::select(Id13Res1:Id13Res25) %>%
  mutate(participant = row_number()) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id13Res1:Id13Res25), "Alpha")) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point) %>%
  mutate(role = "Nonowner-Seller", experiment = "Exp2")

switching_beta <- data %>%
  filter(Descrip17 == "Agente Tipo Beta") %>%
  dplyr::select(Id17Res1:Id17Res25) %>%
  mutate(participant = row_number()) %>%
  rowwise() %>%
  mutate(switching_point = find_switching_point(c_across(Id17Res1:Id17Res25), "Beta")) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point) %>%
  mutate(role = "Nonowner-Buyer", experiment = "Exp2")

exp11 <- bind_rows(switching_a1, switching_b1)
exp12 <- bind_rows(switching_a1_2, switching_b1_2)
exp2 <- bind_rows(switching_a, switching_b, switching_alpha, switching_beta)
switching_all <- bind_rows(exp11, exp12, exp2)

# =============================================================================
# DESCRIPTIVE STATISTICS
# =============================================================================

descriptive_stats <- switching_all %>%
  group_by(experiment, role) %>%
  summarise(
    n = n(),
    mean = round(mean(switching_point), 1),
    sd = round(sd(switching_point), 1),
    median = round(median(switching_point), 0),
    min = min(switching_point),
    max = max(switching_point),
    .groups = 'drop'
  )

cat("\n=== Descriptive Statistics ===\n")
print(descriptive_stats)

# =============================================================================
# NORMALITY TESTS
# =============================================================================

norm_11_owner <- test_normality(
  exp11$switching_point[exp11$role == "Owner-Seller"],
  "Exp1.1: Owner-Seller",
  verbose = TRUE
)

norm_11_nonowner <- test_normality(
  exp11$switching_point[exp11$role == "Nonowner-Buyer"],
  "Exp1.1: Nonowner-Buyer",
  verbose = TRUE
)

norm_12_owner <- test_normality(
  exp12$switching_point[exp12$role == "Owner-Buyer-2nd"],
  "Exp1.2: Owner-Buyer (2nd)",
  verbose = TRUE
)

norm_12_nonowner <- test_normality(
  exp12$switching_point[exp12$role == "Nonowner-Buyer-Pkg"],
  "Exp1.2: Nonowner-Buyer (package)",
  verbose = TRUE
)

norm_2_list <- list()
for (role_group in unique(exp2$role)) {
  norm_2_list[[role_group]] <- test_normality(
    exp2$switching_point[exp2$role == role_group],
    paste0("Exp2: ", role_group),
    verbose = TRUE
  )
}

# =============================================================================
# VISUALIZATION: Q-Q PLOTS AND HISTOGRAMS
# =============================================================================

create_qq_plot <- function(data, title) {
  ggplot(data.frame(y = data), aes(sample = y)) +
    stat_qq(color = "#3498DB", alpha = 0.6) +
    stat_qq_line(color = "#E74C3C", linetype = "dashed", size = 1) +
    labs(title = title, x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, face = "bold"))
}

create_hist_normal <- function(data, title) {
  m <- mean(data, na.rm = TRUE)
  s <- sd(data, na.rm = TRUE)
  
  ggplot(data.frame(x = data), aes(x = x)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "#3498DB", alpha = 0.6, color = "white") +
    stat_function(fun = dnorm, args = list(mean = m, sd = s), color = "#E74C3C", size = 1, linetype = "dashed") +
    labs(title = title, x = "Switching Point", y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, face = "bold"))
}

# =============================================================================
# COMPLETE VISUALIZATIONS: EXPERIMENT 1.1
# =============================================================================

p_hist_11_owner <- create_hist_normal(
  exp11$switching_point[exp11$role == "Owner-Seller"], 
  "Exp1.1: Owner-Seller"
)
p_hist_11_nonowner <- create_hist_normal(
  exp11$switching_point[exp11$role == "Nonowner-Buyer"], 
  "Exp1.1: Nonowner-Buyer"
)

grid.arrange(p_hist_11_owner, p_hist_11_nonowner, ncol = 2, 
             top = "Experiment 1.1: Histograms with Normal Curve")

p_qq_11_owner <- create_qq_plot(
  exp11$switching_point[exp11$role == "Owner-Seller"], 
  "Exp1.1: Owner-Seller"
)
p_qq_11_nonowner <- create_qq_plot(
  exp11$switching_point[exp11$role == "Nonowner-Buyer"], 
  "Exp1.1: Nonowner-Buyer"
)

grid.arrange(p_qq_11_owner, p_qq_11_nonowner, ncol = 2, 
             top = "Experiment 1.1: Q-Q Plots")

# =============================================================================
# COMPLETE VISUALIZATIONS: EXPERIMENT 1.2
# =============================================================================

p_hist_12_owner <- create_hist_normal(
  exp12$switching_point[exp12$role == "Owner-Buyer-2nd"], 
  "Exp1.2: Owner-Buyer (2nd)"
)
p_hist_12_nonowner <- create_hist_normal(
  exp12$switching_point[exp12$role == "Nonowner-Buyer-Pkg"], 
  "Exp1.2: Nonowner-Buyer (package)"
)

grid.arrange(p_hist_12_owner, p_hist_12_nonowner, ncol = 2, 
             top = "Experiment 1.2: Histograms with Normal Curve")

p_qq_12_owner <- create_qq_plot(
  exp12$switching_point[exp12$role == "Owner-Buyer-2nd"], 
  "Exp1.2: Owner-Buyer (2nd)"
)
p_qq_12_nonowner <- create_qq_plot(
  exp12$switching_point[exp12$role == "Nonowner-Buyer-Pkg"], 
  "Exp1.2: Nonowner-Buyer (package)"
)

grid.arrange(p_qq_12_owner, p_qq_12_nonowner, ncol = 2, 
             top = "Experiment 1.2: Q-Q Plots")

# =============================================================================
# COMPLETE VISUALIZATIONS: EXPERIMENT 2
# =============================================================================

p_hist_2_owner_seller <- create_hist_normal(
  exp2$switching_point[exp2$role == "Owner-Seller"], 
  "Exp2: Owner-Seller"
)
p_hist_2_owner_buyer <- create_hist_normal(
  exp2$switching_point[exp2$role == "Owner-Buyer"], 
  "Exp2: Owner-Buyer"
)
p_hist_2_nonowner_seller <- create_hist_normal(
  exp2$switching_point[exp2$role == "Nonowner-Seller"], 
  "Exp2: Nonowner-Seller"
)
p_hist_2_nonowner_buyer <- create_hist_normal(
  exp2$switching_point[exp2$role == "Nonowner-Buyer"], 
  "Exp2: Nonowner-Buyer"
)

grid.arrange(p_hist_2_owner_seller, p_hist_2_owner_buyer,
             p_hist_2_nonowner_seller, p_hist_2_nonowner_buyer,
             ncol = 2, nrow = 2,
             top = "Experiment 2: Histograms with Normal Curve")

p_qq_2_owner_seller <- create_qq_plot(
  exp2$switching_point[exp2$role == "Owner-Seller"], 
  "Exp2: Owner-Seller"
)
p_qq_2_owner_buyer <- create_qq_plot(
  exp2$switching_point[exp2$role == "Owner-Buyer"], 
  "Exp2: Owner-Buyer"
)
p_qq_2_nonowner_seller <- create_qq_plot(
  exp2$switching_point[exp2$role == "Nonowner-Seller"], 
  "Exp2: Nonowner-Seller"
)
p_qq_2_nonowner_buyer <- create_qq_plot(
  exp2$switching_point[exp2$role == "Nonowner-Buyer"], 
  "Exp2: Nonowner-Buyer"
)

grid.arrange(p_qq_2_owner_seller, p_qq_2_owner_buyer,
             p_qq_2_nonowner_seller, p_qq_2_nonowner_buyer,
             ncol = 2, nrow = 2,
             top = "Experiment 2: Q-Q Plots")

# =============================================================================
# HOMOGENEITY OF VARIANCES (LEVENE TEST)
# =============================================================================

levene_11 <- leveneTest(switching_point ~ role, data = exp11)
levene_12 <- leveneTest(switching_point ~ role, data = exp12)
levene_2 <- leveneTest(switching_point ~ role, data = exp2)

cat("Exp1.1: F =", round(levene_11$`F value`[1], 3), ", p =", round(levene_11$`Pr(>F)`[1], 4), "\n")
cat("Exp1.2: F =", round(levene_12$`F value`[1], 3), ", p =", round(levene_12$`Pr(>F)`[1], 4), "\n")
cat("Exp2:   F =", round(levene_2$`F value`[1], 3), ", p =", round(levene_2$`Pr(>F)`[1], 4), "\n")

# =============================================================================
# DECISION AND TEST EXECUTION
# =============================================================================

test_11 <- decide_test(list(norm_11_owner, norm_11_nonowner), levene_11$`Pr(>F)`[1])
test_12 <- decide_test(list(norm_12_owner, norm_12_nonowner), levene_12$`Pr(>F)`[1])
test_2 <- decide_test(norm_2_list, levene_2$`Pr(>F)`[1])

cat("\n=== Selected Tests ===\n")
cat("Exp1.1:", test_11, "\n")
cat("Exp1.2:", test_12, "\n")
cat("Exp2:  ", test_2, "\n")

cat("\n=== Experiment 1.1 Results ===\n")
if (test_11 == "ANOVA") {
  anova_11 <- aov(switching_point ~ role, data = exp11)
  print(summary(anova_11))
  if (summary(anova_11)[[1]]$`Pr(>F)`[1] < 0.05) {
    cat("\nPost-hoc (Tukey HSD):\n")
    print(TukeyHSD(anova_11))
  }
} else if (test_11 == "Welch") {
  welch_11 <- oneway.test(switching_point ~ role, data = exp11, var.equal = FALSE)
  print(welch_11)
} else {
  kw_11 <- kruskal.test(switching_point ~ role, data = exp11)
  print(kw_11)
  if (kw_11$p.value < 0.05) {
    pw_11 <- pairwise.wilcox.test(exp11$switching_point, exp11$role, p.adjust.method = "bonferroni")
    cat("\nPost-hoc (Pairwise Wilcoxon):\n")
    print(pw_11)
  }
}

cat("\n=== Experiment 1.2 Results ===\n")
if (test_12 == "ANOVA") {
  anova_12 <- aov(switching_point ~ role, data = exp12)
  print(summary(anova_12))
  if (summary(anova_12)[[1]]$`Pr(>F)`[1] < 0.05) {
    print(TukeyHSD(anova_12))
  }
} else if (test_12 == "Welch") {
  welch_12 <- oneway.test(switching_point ~ role, data = exp12, var.equal = FALSE)
  print(welch_12)
} else {
  kw_12 <- kruskal.test(switching_point ~ role, data = exp12)
  print(kw_12)
  if (kw_12$p.value < 0.05) {
    pw_12 <- pairwise.wilcox.test(exp12$switching_point, exp12$role, p.adjust.method = "bonferroni")
    cat("\nPost-hoc (Pairwise Wilcoxon):\n")
    print(pw_12)
  }
}

cat("\n=== Experiment 2 Results ===\n")
if (test_2 == "ANOVA") {
  anova_2 <- aov(switching_point ~ role, data = exp2)
  print(summary(anova_2))
  if (summary(anova_2)[[1]]$`Pr(>F)`[1] < 0.05) {
    print(TukeyHSD(anova_2))
  }
} else if (test_2 == "Welch") {
  welch_2 <- oneway.test(switching_point ~ role, data = exp2, var.equal = FALSE)
  print(welch_2)
} else {
  kw_2 <- kruskal.test(switching_point ~ role, data = exp2)
  print(kw_2)
  if (kw_2$p.value < 0.05) {
    pw_2 <- pairwise.wilcox.test(exp2$switching_point, exp2$role, p.adjust.method = "bonferroni")
    cat("\nPost-hoc (Pairwise Wilcoxon):\n")
    print(pw_2)
  }
}

# =============================================================================
# FINAL SUMMARY TABLE
# =============================================================================

cat("\n=== Summary Table ===\n")

summary_table <- data.frame(
  Experiment = c("Exp1.1", "Exp1.1", "Exp1.2", "Exp1.2"),
  Role = c("Owner-Seller", "Nonowner-Buyer", "Owner-Buyer", "Nonowner-Buyer"),
  N = c(
    nrow(switching_a1),
    nrow(switching_b1),
    nrow(switching_a1_2),
    nrow(switching_b1_2)
  ),
  Mean = c(
    round(mean(switching_a1$switching_point), 1),
    round(mean(switching_b1$switching_point), 1),
    round(mean(switching_a1_2$switching_point), 1),
    round(mean(switching_b1_2$switching_point), 1)
  ),
  SD = c(
    round(sd(switching_a1$switching_point), 1),
    round(sd(switching_b1$switching_point), 1),
    round(sd(switching_a1_2$switching_point), 1),
    round(sd(switching_b1_2$switching_point), 1)
  ),
  Test = c(test_11, test_11, test_12, test_12)
)

print(summary_table)