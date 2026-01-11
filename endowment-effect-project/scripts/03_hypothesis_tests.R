# =============================================================================
# ENDOWMENT EFFECT ANALYSIS
# Hypothesis Testing across Experiments
# =============================================================================

rm(list = ls())
cat("\014")

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
library(knitr)
library(kableExtra)

# Load data (Change as neccesary)
data <- read_excel("/Users/jeanmarroquin/Documents/Economia/asistente/dotacion/endowment-effect-project/data/raw/data.xlsx")
prices <- seq(20, 500, 20)

# =============================================================================
# FUNCTION TO FIND SWITCHING POINTS
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

# =============================================================================
# EXTRACT DATA - EXPERIMENT 1.1
# =============================================================================

# A.1 (Owner - Sells)
data_a1 <- data %>%
  filter(Formato == "Agente Tipo A.1") %>%
  dplyr::select(IdRes1:IdRes25, Edad, DescGenero, DescEstudio) %>%
  mutate(participant = row_number())

switching_a1 <- data_a1 %>%
  rowwise() %>%
  mutate(
    switching_point = find_switching_point(c_across(IdRes1:IdRes25), "A.1"),
    role = "owner-seller",
    experiment = "Exp1.1",
    gender_en = case_when(
      DescGenero == "Masculino" ~ "Male",
      DescGenero == "Femenino" ~ "Female",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point, role, experiment, Edad, DescGenero, DescEstudio, gender_en)

# B.1 (Nonowner - Buyer)
data_b1 <- data %>%
  filter(Descrip9 == "Agente Tipo B.1") %>%
  dplyr::select(Id9Res1:Id9Res25, Edad, DescGenero, DescEstudio) %>%
  mutate(participant = row_number())

switching_b1 <- data_b1 %>%
  rowwise() %>%
  mutate(
    switching_point = find_switching_point(c_across(Id9Res1:Id9Res25), "B.1"),
    role = "nonowner-buyer",
    experiment = "Exp1.1",
    gender_en = case_when(
      DescGenero == "Masculino" ~ "Male",
      DescGenero == "Femenino" ~ "Female",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point, role, experiment, Edad, DescGenero, DescEstudio, gender_en)

# =============================================================================
# EXTRACT DATA - EXPERIMENT 1.2
# =============================================================================

# A.1.2 (Owner - Buys 2nd)
data_a12 <- data %>%
  filter(Descrip10 == "Agente Tipo A.1") %>%
  dplyr::select(Id10Res1:Id10Res25, Edad, DescGenero, DescEstudio) %>%
  mutate(participant = row_number())

switching_a12 <- data_a12 %>%
  rowwise() %>%
  mutate(
    switching_point = find_switching_point(c_across(Id10Res1:Id10Res25), "A.1.2"),
    role = "owner-buyer",
    experiment = "Exp1.2",
    gender_en = case_when(
      DescGenero == "Masculino" ~ "Male",
      DescGenero == "Femenino" ~ "Female",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point, role, experiment, Edad, DescGenero, DescEstudio, gender_en)

# B.1.2 (Nonowner - Buys package)
data_b12 <- data %>%
  filter(Descrip11 == "Agente Tipo B.1") %>%
  dplyr::select(Id11Res1:Id11Res25, Edad, DescGenero, DescEstudio) %>%
  mutate(participant = row_number())

switching_b12 <- data_b12 %>%
  rowwise() %>%
  mutate(
    switching_point = find_switching_point(c_across(Id11Res1:Id11Res25), "B.1.2"),
    role = "nonowner-buyer-package",
    experiment = "Exp1.2",
    gender_en = case_when(
      DescGenero == "Masculino" ~ "Male",
      DescGenero == "Femenino" ~ "Female",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point, role, experiment, Edad, DescGenero, DescEstudio, gender_en)

# =============================================================================
# EXTRACT DATA - EXPERIMENT 2 (BROKERS)
# =============================================================================

# A (Seller Broker w/ Cup)
data_a <- data %>%
  filter(Descrip18 == "Agente Tipo A") %>%
  dplyr::select(Id18Res1:Id18Res25, Edad, DescGenero, DescEstudio) %>%
  mutate(participant = row_number())

switching_a <- data_a %>%
  rowwise() %>%
  mutate(
    switching_point = find_switching_point(c_across(Id18Res1:Id18Res25), "A"),
    role = "owner-seller-broker",
    experiment = "Exp2",
    gender_en = case_when(
      DescGenero == "Masculino" ~ "Male",
      DescGenero == "Femenino" ~ "Female",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point, role, experiment, Edad, DescGenero, DescEstudio, gender_en)

# Alpha (Seller Broker w/o Cup)
data_alpha <- data %>%
  filter(Descrip13 == "Agente Tipo Alpha") %>%
  dplyr::select(Id13Res1:Id13Res25, Edad, DescGenero, DescEstudio) %>%
  mutate(participant = row_number())

switching_alpha <- data_alpha %>%
  rowwise() %>%
  mutate(
    switching_point = find_switching_point(c_across(Id13Res1:Id13Res25), "Alpha"),
    role = "nonowner-seller-broker",
    experiment = "Exp2",
    gender_en = case_when(
      DescGenero == "Masculino" ~ "Male",
      DescGenero == "Femenino" ~ "Female",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point, role, experiment, Edad, DescGenero, DescEstudio, gender_en)

# B (Buyer Broker w/ Cup)
data_b <- data %>%
  filter(Descrip20 == "Agente Tipo B") %>%
  dplyr::select(Id20Res1:Id20Res25, Edad, DescGenero, DescEstudio) %>%
  mutate(participant = row_number())

switching_b <- data_b %>%
  rowwise() %>%
  mutate(
    switching_point = find_switching_point(c_across(Id20Res1:Id20Res25), "B"),
    role = "owner-buyer-broker",
    experiment = "Exp2",
    gender_en = case_when(
      DescGenero == "Masculino" ~ "Male",
      DescGenero == "Femenino" ~ "Female",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point, role, experiment, Edad, DescGenero, DescEstudio, gender_en)

# Beta (Buyer Broker w/o Cup)
data_beta <- data %>%
  filter(Descrip17 == "Agente Tipo Beta") %>%
  dplyr::select(Id17Res1:Id17Res25, Edad, DescGenero, DescEstudio) %>%
  mutate(participant = row_number())

switching_beta <- data_beta %>%
  rowwise() %>%
  mutate(
    switching_point = find_switching_point(c_across(Id17Res1:Id17Res25), "Beta"),
    role = "nonowner-buyer-broker",
    experiment = "Exp2",
    gender_en = case_when(
      DescGenero == "Masculino" ~ "Male",
      DescGenero == "Femenino" ~ "Female",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(switching_point)) %>%
  dplyr::select(participant, switching_point, role, experiment, Edad, DescGenero, DescEstudio, gender_en)

# =============================================================================
# COMBINE ALL DATA
# =============================================================================

all_data <- bind_rows(
  switching_a1, switching_b1,
  switching_a12, switching_b12,
  switching_a, switching_alpha, switching_b, switching_beta
)

# =============================================================================
# DESCRIPTIVE STATISTICS TABLE
# =============================================================================

cat("\n=== DESCRIPTIVE STATISTICS ===\n\n")

descriptive_stats <- all_data %>%
  group_by(experiment, role) %>%
  summarise(
    mean = round(mean(switching_point, na.rm = TRUE), 0),
    `std. dev.` = round(sd(switching_point, na.rm = TRUE), 0),
    obs. = n(),
    .groups = 'drop'
  )

print(kable(descriptive_stats, format = "simple", align = "llrrr"))

# =============================================================================
# STATISTICAL TESTS - EXPERIMENT 1.1
# =============================================================================

exp11_owner <- switching_a1$switching_point
exp11_nonowner <- switching_b1$switching_point

anova_11 <- aov(switching_point ~ role, data = bind_rows(switching_a1, switching_b1))
anova_11_summary <- summary(anova_11)
f_stat_11 <- anova_11_summary[[1]]$`F value`[1]
p_anova_11 <- anova_11_summary[[1]]$`Pr(>F)`[1]

welch_11 <- oneway.test(switching_point ~ role, data = bind_rows(switching_a1, switching_b1), var.equal = FALSE)
f_welch_11 <- welch_11$statistic
p_welch_11 <- welch_11$p.value

wilcox_11 <- wilcox.test(exp11_owner, exp11_nonowner, alternative = "greater")
w_stat_11 <- wilcox_11$statistic
p_wilcox_11 <- wilcox_11$p.value

kruskal_11 <- kruskal.test(switching_point ~ role, data = bind_rows(switching_a1, switching_b1))
h_stat_11 <- kruskal_11$statistic
p_kruskal_11 <- kruskal_11$p.value

median_diff_11 <- abs(median(exp11_owner) - median(exp11_nonowner))

# =============================================================================
# STATISTICAL TESTS - EXPERIMENT 1.2
# =============================================================================

exp12_owner <- switching_a12$switching_point
exp12_nonowner <- switching_b12$switching_point

anova_12 <- aov(switching_point ~ role, data = bind_rows(switching_a12, switching_b12))
anova_12_summary <- summary(anova_12)
f_stat_12 <- anova_12_summary[[1]]$`F value`[1]
p_anova_12 <- anova_12_summary[[1]]$`Pr(>F)`[1]

welch_12 <- oneway.test(switching_point ~ role, data = bind_rows(switching_a12, switching_b12), var.equal = FALSE)
f_welch_12 <- welch_12$statistic
p_welch_12 <- welch_12$p.value

wilcox_12 <- wilcox.test(exp12_nonowner, exp12_owner, alternative = "greater")
w_stat_12 <- wilcox_12$statistic
p_wilcox_12 <- wilcox_12$p.value

kruskal_12 <- kruskal.test(switching_point ~ role, data = bind_rows(switching_a12, switching_b12))
h_stat_12 <- kruskal_12$statistic
p_kruskal_12 <- kruskal_12$p.value

median_diff_12 <- abs(median(exp12_owner) - median(exp12_nonowner))

# =============================================================================
# STATISTICAL TESTS - EXPERIMENT 2
# =============================================================================

# Loss Aversion - Owners
exp2_sellers_owners <- bind_rows(switching_a)
exp2_buyers_owners <- bind_rows(switching_b)

anova_loss_owners <- aov(switching_point ~ role, data = bind_rows(exp2_sellers_owners, exp2_buyers_owners))
f_loss_owners <- summary(anova_loss_owners)[[1]]$`F value`[1]
p_anova_loss_owners <- summary(anova_loss_owners)[[1]]$`Pr(>F)`[1]

welch_loss_owners <- oneway.test(switching_point ~ role, data = bind_rows(exp2_sellers_owners, exp2_buyers_owners), var.equal = FALSE)
p_welch_loss_owners <- welch_loss_owners$p.value

wilcox_loss_owners <- wilcox.test(exp2_sellers_owners$switching_point, exp2_buyers_owners$switching_point, alternative = "greater")
w_loss_owners <- wilcox_loss_owners$statistic
p_wilcox_loss_owners <- wilcox_loss_owners$p.value

# Loss Aversion - Nonowners
exp2_sellers_nonowners <- bind_rows(switching_alpha)
exp2_buyers_nonowners <- bind_rows(switching_beta)

anova_loss_nonowners <- aov(switching_point ~ role, data = bind_rows(exp2_sellers_nonowners, exp2_buyers_nonowners))
f_loss_nonowners <- summary(anova_loss_nonowners)[[1]]$`F value`[1]
p_anova_loss_nonowners <- summary(anova_loss_nonowners)[[1]]$`Pr(>F)`[1]

welch_loss_nonowners <- oneway.test(switching_point ~ role, data = bind_rows(exp2_sellers_nonowners, exp2_buyers_nonowners), var.equal = FALSE)
p_welch_loss_nonowners <- welch_loss_nonowners$p.value

wilcox_loss_nonowners <- wilcox.test(exp2_sellers_nonowners$switching_point, exp2_buyers_nonowners$switching_point, alternative = "greater")
w_loss_nonowners <- wilcox_loss_nonowners$statistic
p_wilcox_loss_nonowners <- wilcox_loss_nonowners$p.value

# Ownership - Sellers
exp2_owner_sellers <- bind_rows(switching_a)
exp2_nonowner_sellers <- bind_rows(switching_alpha)

anova_own_sellers <- aov(switching_point ~ role, data = bind_rows(exp2_owner_sellers, exp2_nonowner_sellers))
f_own_sellers <- summary(anova_own_sellers)[[1]]$`F value`[1]
p_anova_own_sellers <- summary(anova_own_sellers)[[1]]$`Pr(>F)`[1]

welch_own_sellers <- oneway.test(switching_point ~ role, data = bind_rows(exp2_owner_sellers, exp2_nonowner_sellers), var.equal = FALSE)
p_welch_own_sellers <- welch_own_sellers$p.value

wilcox_own_sellers <- wilcox.test(exp2_owner_sellers$switching_point, exp2_nonowner_sellers$switching_point, alternative = "two.sided")
w_own_sellers <- wilcox_own_sellers$statistic
p_wilcox_own_sellers <- wilcox_own_sellers$p.value

# Ownership - Buyers
exp2_owner_buyers <- bind_rows(switching_b)
exp2_nonowner_buyers <- bind_rows(switching_beta)

anova_own_buyers <- aov(switching_point ~ role, data = bind_rows(exp2_owner_buyers, exp2_nonowner_buyers))
f_own_buyers <- summary(anova_own_buyers)[[1]]$`F value`[1]
p_anova_own_buyers <- summary(anova_own_buyers)[[1]]$`Pr(>F)`[1]

welch_own_buyers <- oneway.test(switching_point ~ role, data = bind_rows(exp2_owner_buyers, exp2_nonowner_buyers), var.equal = FALSE)
p_welch_own_buyers <- welch_own_buyers$p.value

wilcox_own_buyers <- wilcox.test(exp2_owner_buyers$switching_point, exp2_nonowner_buyers$switching_point, alternative = "two.sided")
w_own_buyers <- wilcox_own_buyers$statistic
p_wilcox_own_buyers <- wilcox_own_buyers$p.value

# =============================================================================
# BAR CHARTS: MEANS AND MEDIANS
# =============================================================================

# Experiment 1.1
stats_11 <- bind_rows(switching_a1, switching_b1) %>%
  group_by(role) %>%
  summarise(
    mean = mean(switching_point),
    sd = sd(switching_point),
    median = median(switching_point),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se
  )

# Create labels with line breaks
stats_11$role_label <- factor(stats_11$role, 
                              levels = c("nonowner-buyer", "owner-seller"),
                              labels = c("nonowner-\nbuyer", "owner-\nseller"))

plot_11 <- ggplot(stats_11, aes(x = role_label, y = mean, fill = role)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 0.8) +
  geom_point(aes(y = median), shape = 23, size = 5, fill = "black") +
  scale_fill_manual(values = c("nonowner-buyer" = "#E91E63", "owner-seller" = "#64B5F6")) +
  labs(
    title = "Experiment 1.1: Means and Medians",
    y = "Quantity Change (mean ± 95% CI)",
    x = "Role"
  ) +
  annotate("text", x = Inf, y = Inf, label = "Black Diamond = Median", 
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.title.x = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(plot_11)

# Experiment 1.2
stats_12 <- bind_rows(switching_a12, switching_b12) %>%
  group_by(role) %>%
  summarise(
    mean = mean(switching_point),
    sd = sd(switching_point),
    median = median(switching_point),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se
  )

# Create labels with line breaks
stats_12$role_label <- factor(stats_12$role, 
                              levels = c("nonowner-buyer-package", "owner-buyer"),
                              labels = c("nonowner-buyer-\npackage", "owner-\nbuyer"))

plot_12 <- ggplot(stats_12, aes(x = role_label, y = mean, fill = role)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 0.8) +
  geom_point(aes(y = median), shape = 23, size = 5, fill = "black") +
  scale_fill_manual(values = c("owner-buyer" = "#64B5F6", "nonowner-buyer-package" = "#E91E63")) +
  labs(
    title = "Experiment 1.2: Means and Medians",
    y = "Quantity Change (mean ± 95% CI)",
    x = "Role"
  ) +
  annotate("text", x = Inf, y = Inf, label = "Black Diamond = Median", 
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.title.x = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(plot_12)

# Experiment 2
stats_2 <- bind_rows(switching_a, switching_alpha, switching_b, switching_beta) %>%
  group_by(role) %>%
  summarise(
    mean = mean(switching_point),
    sd = sd(switching_point),
    median = median(switching_point),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se
  )

# Create labels with line breaks
stats_2$role_label <- factor(stats_2$role, 
                             levels = c("nonowner-buyer-broker", "nonowner-seller-broker", 
                                        "owner-buyer-broker", "owner-seller-broker"),
                             labels = c("nonowner-buyer-\nbroker", "nonowner-seller-\nbroker", 
                                        "owner-buyer-\nbroker", "owner-seller-\nbroker"))

plot_2 <- ggplot(stats_2, aes(x = role_label, y = mean, fill = role)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 0.8) +
  geom_point(aes(y = median), shape = 23, size = 5, fill = "black") +
  scale_fill_manual(values = c(
    "nonowner-buyer-broker" = "#9C27B0",
    "nonowner-seller-broker" = "#FFA726",
    "owner-buyer-broker" = "#E91E63",
    "owner-seller-broker" = "#26C6DA"
  )) +
  labs(
    title = "Experiment 2: Means and Medians (Brokers)",
    y = "Quantity Change (mean ± 95% CI)",
    x = "Role"
  ) +
  annotate("text", x = Inf, y = Inf, label = "Black Diamond = Median", 
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.title.x = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(plot_2)

# =============================================================================
# GENDER INTERACTION ANALYSIS
# =============================================================================

cat("\n=== GENDER × ROLE INTERACTION ANALYSIS ===\n\n")

# -----------------------------------------------------------------------------
# Experiment 1.1: Gender × Role Interaction
# -----------------------------------------------------------------------------

data_11_gender <- bind_rows(switching_a1, switching_b1) %>%
  filter(gender_en %in% c("Male", "Female"))

# Two-way ANOVA
anova_gender_11 <- aov(switching_point ~ gender_en * role, data = data_11_gender)
summary_gender_11 <- summary(anova_gender_11)

cat("\n--- Experiment 1.1: Two-way ANOVA (Gender × Role) ---\n")
print(summary_gender_11)

# Statistics for plotting
stats_gender_11 <- data_11_gender %>%
  group_by(gender_en, role) %>%
  summarise(
    mean = mean(switching_point),
    sd = sd(switching_point),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se
  )

# Interaction plot
plot_gender_11 <- ggplot(stats_gender_11, aes(x = role, y = mean, color = gender_en, group = gender_en)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, size = 0.8) +
  scale_color_manual(values = c("Female" = "#66BB6A", "Male" = "#5E35B1"),
                     name = "Gender") +
  labs(
    title = "Experiment 1.1: Basic Endowment Effect",
    subtitle = "Interaction between Gender and Role",
    y = "Quantity Change (mean ± 95% CI)",
    x = "Role"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.title.x = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

print(plot_gender_11)

# -----------------------------------------------------------------------------
# Experiment 1.2: Gender × Role Interaction
# -----------------------------------------------------------------------------

data_12_gender <- bind_rows(switching_a12, switching_b12) %>%
  filter(gender_en %in% c("Male", "Female"))

# Two-way ANOVA
anova_gender_12 <- aov(switching_point ~ gender_en * role, data = data_12_gender)
summary_gender_12 <- summary(anova_gender_12)

cat("\n--- Experiment 1.2: Two-way ANOVA (Gender × Role) ---\n")
print(summary_gender_12)

# Statistics for plotting
stats_gender_12 <- data_12_gender %>%
  group_by(gender_en, role) %>%
  summarise(
    mean = mean(switching_point),
    sd = sd(switching_point),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se
  )

# Interaction plot
plot_gender_12 <- ggplot(stats_gender_12, aes(x = role, y = mean, color = gender_en, group = gender_en)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, size = 0.8) +
  scale_color_manual(values = c("Female" = "#66BB6A", "Male" = "#5E35B1"),
                     name = "Gender") +
  labs(
    title = "Experiment 1.2: Second Unit and Package",
    subtitle = "Interaction between Gender and Role",
    y = "Quantity Change (mean ± 95% CI)",
    x = "Role"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.title.x = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

print(plot_gender_12)

# -----------------------------------------------------------------------------
# Experiment 2: Gender × Role Interaction (All Roles)
# -----------------------------------------------------------------------------

data_2_gender <- bind_rows(switching_a, switching_alpha, switching_b, switching_beta) %>%
  filter(gender_en %in% c("Male", "Female"))

# Two-way ANOVA
anova_gender_2 <- aov(switching_point ~ gender_en * role, data = data_2_gender)
summary_gender_2 <- summary(anova_gender_2)

cat("\n--- Experiment 2: Two-way ANOVA (Gender × Role) ---\n")
print(summary_gender_2)

# Statistics for plotting
stats_gender_2 <- data_2_gender %>%
  group_by(gender_en, role) %>%
  summarise(
    mean = mean(switching_point),
    sd = sd(switching_point),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se
  )

# Create shorter labels for x-axis
stats_gender_2$role_short <- factor(stats_gender_2$role,
                                    levels = c("nonowner-buyer-broker", "nonowner-seller-broker",
                                               "owner-buyer-broker", "owner-seller-broker"),
                                    labels = c("nonowner-buyer-\nbroker", "nonowner-seller-\nbroker",
                                               "owner-buyer-\nbroker", "owner-seller-\nbroker"))

# Interaction plot
plot_gender_2 <- ggplot(stats_gender_2, aes(x = role_short, y = mean, color = gender_en, group = gender_en)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, size = 0.8) +
  scale_color_manual(values = c("Female" = "#66BB6A", "Male" = "#5E35B1"),
                     name = "Gender") +
  labs(
    title = "Experiment 2: Loss Aversion and Ownership",
    subtitle = "Interaction between Gender and Role (All Roles)",
    y = "Quantity Change (mean ± 95% CI)",
    x = "Role"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.title.x = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

print(plot_gender_2)

# =============================================================================
# COMPLETE RESULTS TABLE - ANOVA
# =============================================================================

cat("\n=== ANOVA RESULTS ===\n\n")

table_anova <- data.frame(
  Test = c(
    "Endowment Effect",
    "",
    "",
    "",
    "Loss Aversion",
    "",
    "",
    "Ownership",
    ""
  ),
  Comparison = c(
    "V(owner-seller 1.1) > V(nonowner-buyer 1.1)",
    "V(nonowner-buyer 1.1) < V(owner-buyer 2nd-unit 1.2)",
    "V(owner-seller 1.1) = V(owner-buyer 2nd-unit 1.2)",
    "V(nonowner-buyer 1.1) = V(nonowner-buyer package 1.2)",
    "V(seller brokers 2) > V(buyer brokers 2) - owners",
    "V(seller brokers 2) > V(buyer brokers 2) - nonowners",
    "",
    "V(owner-seller brokers 2) > V(nonowner-seller brokers 2)",
    "V(owner-buyer brokers 2) > V(nonowner-buyer brokers 2)"
  ),
  `F-statistic` = c(
    round(f_stat_11, 2),
    round(f_stat_12, 2),
    NA,
    NA,
    round(f_loss_owners, 2),
    round(f_loss_nonowners, 2),
    NA,
    round(f_own_sellers, 2),
    round(f_own_buyers, 2)
  ),
  `p-value` = c(
    round(p_anova_11, 4),
    round(p_anova_12, 4),
    NA,
    NA,
    round(p_anova_loss_owners, 4),
    round(p_anova_loss_nonowners, 4),
    NA,
    round(p_anova_own_sellers, 4),
    round(p_anova_own_buyers, 4)
  )
)

print(kable(table_anova, format = "simple", align = "llrr"))

# =============================================================================
# WELCH ANOVA TABLE
# =============================================================================

cat("\n=== WELCH ANOVA RESULTS ===\n\n")

table_welch <- data.frame(
  Test = c(
    "Endowment Effect",
    "",
    "",
    "",
    "Loss Aversion",
    "",
    "",
    "Ownership",
    ""
  ),
  Comparison = c(
    "V(owner-seller 1.1) > V(nonowner-buyer 1.1)",
    "V(nonowner-buyer 1.1) < V(owner-buyer 2nd-unit 1.2)",
    "V(owner-seller 1.1) = V(owner-buyer 2nd-unit 1.2)",
    "V(nonowner-buyer 1.1) = V(nonowner-buyer package 1.2)",
    "V(seller brokers 2) > V(buyer brokers 2) - owners",
    "V(seller brokers 2) > V(buyer brokers 2) - nonowners",
    "",
    "V(owner-seller brokers 2) > V(nonowner-seller brokers 2)",
    "V(owner-buyer brokers 2) > V(nonowner-buyer brokers 2)"
  ),
  `F-statistic` = c(
    round(f_welch_11, 2),
    round(f_welch_12, 2),
    NA,
    NA,
    round(welch_loss_owners$statistic, 2),
    round(welch_loss_nonowners$statistic, 2),
    NA,
    round(welch_own_sellers$statistic, 2),
    round(welch_own_buyers$statistic, 2)
  ),
  `p-value` = c(
    round(p_welch_11, 4),
    round(p_welch_12, 4),
    NA,
    NA,
    round(p_welch_loss_owners, 4),
    round(p_welch_loss_nonowners, 4),
    NA,
    round(p_welch_own_sellers, 4),
    round(p_welch_own_buyers, 4)
  )
)

print(kable(table_welch, format = "simple", align = "llrr"))

# =============================================================================
# WILCOXON TEST TABLE
# =============================================================================

cat("\n=== WILCOXON TEST RESULTS ===\n\n")

table_wilcox <- data.frame(
  Test = c(
    "Endowment Effect",
    "",
    "",
    "",
    "Loss Aversion",
    "",
    "",
    "Ownership",
    ""
  ),
  Comparison = c(
    "V(owner-seller 1.1) > V(nonowner-buyer 1.1)",
    "V(nonowner-buyer 1.1) < V(owner-buyer 2nd-unit 1.2)",
    "V(owner-seller 1.1) = V(owner-buyer 2nd-unit 1.2)",
    "V(nonowner-buyer 1.1) = V(nonowner-buyer package 1.2)",
    "V(seller brokers 2) > V(buyer brokers 2) - owners",
    "V(seller brokers 2) > V(buyer brokers 2) - nonowners",
    "",
    "V(owner-seller brokers 2) > V(nonowner-seller brokers 2)",
    "V(owner-buyer brokers 2) > V(nonowner-buyer brokers 2)"
  ),
  `W-statistic` = c(
    round(w_stat_11, 1),
    round(w_stat_12, 1),
    NA,
    NA,
    round(w_loss_owners, 1),
    round(w_loss_nonowners, 1),
    NA,
    round(w_own_sellers, 1),
    round(w_own_buyers, 1)
  ),
  `p-value` = c(
    round(p_wilcox_11, 4),
    round(p_wilcox_12, 4),
    NA,
    NA,
    round(p_wilcox_loss_owners, 4),
    round(p_wilcox_loss_nonowners, 4),
    NA,
    round(p_wilcox_own_sellers, 4),
    round(p_wilcox_own_buyers, 4)
  )
)

print(kable(table_wilcox, format = "simple", align = "llrr"))

# =============================================================================
# KRUSKAL-WALLIS TEST TABLE
# =============================================================================

cat("\n=== KRUSKAL-WALLIS TEST RESULTS ===\n\n")

table_kruskal <- data.frame(
  Test = c(
    "Endowment Effect",
    "",
    "",
    "",
    "Loss Aversion",
    "",
    "",
    "Ownership",
    ""
  ),
  Comparison = c(
    "V(owner-seller 1.1) > V(nonowner-buyer 1.1)",
    "V(nonowner-buyer 1.1) < V(owner-buyer 2nd-unit 1.2)",
    "V(owner-seller 1.1) = V(owner-buyer 2nd-unit 1.2)",
    "V(nonowner-buyer 1.1) = V(nonowner-buyer package 1.2)",
    "V(seller brokers 2) > V(buyer brokers 2) - owners",
    "V(seller brokers 2) > V(buyer brokers 2) - nonowners",
    "",
    "V(owner-seller brokers 2) > V(nonowner-seller brokers 2)",
    "V(owner-buyer brokers 2) > V(nonowner-buyer brokers 2)"
  ),
  `|Median diff.|` = c(
    round(median_diff_11, 0),
    round(median_diff_12, 0),
    round(abs(median(exp11_owner) - median(exp12_owner)), 0),
    round(abs(median(exp11_nonowner) - median(exp12_nonowner)), 0),
    round(abs(median(exp2_sellers_owners$switching_point) - median(exp2_buyers_owners$switching_point)), 0),
    round(abs(median(exp2_sellers_nonowners$switching_point) - median(exp2_buyers_nonowners$switching_point)), 0),
    NA,
    round(abs(median(exp2_owner_sellers$switching_point) - median(exp2_nonowner_sellers$switching_point)), 0),
    round(abs(median(exp2_owner_buyers$switching_point) - median(exp2_nonowner_buyers$switching_point)), 0)
  ),
  `p-value (95%)` = c(
    round(p_kruskal_11, 4),
    round(p_kruskal_12, 4),
    round(kruskal.test(switching_point ~ role, data = bind_rows(switching_a1 %>% mutate(role = "owner-seller"), switching_a12 %>% mutate(role = "owner-buyer")))$p.value, 4),
    round(kruskal.test(switching_point ~ role, data = bind_rows(switching_b1 %>% mutate(role = "nonowner-buyer"), switching_b12 %>% mutate(role = "nonowner-package")))$p.value, 4),
    round(kruskal.test(switching_point ~ role, data = bind_rows(exp2_sellers_owners, exp2_buyers_owners))$p.value, 4),
    round(kruskal.test(switching_point ~ role, data = bind_rows(exp2_sellers_nonowners, exp2_buyers_nonowners))$p.value, 4),
    NA,
    round(kruskal.test(switching_point ~ role, data = bind_rows(exp2_owner_sellers, exp2_nonowner_sellers))$p.value, 4),
    round(kruskal.test(switching_point ~ role, data = bind_rows(exp2_owner_buyers, exp2_nonowner_buyers))$p.value, 4)
  )
)

print(kable(table_kruskal, format = "simple", align = "llrr"))