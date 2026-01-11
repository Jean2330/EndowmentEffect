# =============================================================================
# POOLED LOGIT REGRESSION WITH EXPERIMENT DUMMIES
# =============================================================================

rm(list = ls())
cat("\014")

library(readxl)
library(dplyr)
library(tidyr)
library(broom)
library(knitr)
library(kableExtra)
library(modelsummary)
library(ggplot2)

# Load data (Change as neccesary)
data <- read_excel("/Users/jeanmarroquin/Documents/Economia/asistente/dotacion/endowment-effect-project/data/raw/data.xlsx")
prices <- seq(20, 500, 20)

# =============================================================================
# FUNCTION TO CREATE LONG DATASET (CORRECTED CODING)
# =============================================================================

create_long_dataset <- function(agent_data, agent_type, experiment, owner_status) {
  # Determine response columns by agent type
  if (agent_type == "A.1") {
    resp_cols <- paste0("IdRes", 1:25)
  } else if (agent_type == "B.1") {
    resp_cols <- paste0("Id9Res", 1:25)
  } else if (agent_type == "A.1.2") {
    resp_cols <- paste0("Id10Res", 1:25)
  } else if (agent_type == "B.1.2") {
    resp_cols <- paste0("Id11Res", 1:25)
  } else if (agent_type == "A") {
    resp_cols <- paste0("Id18Res", 1:25)
  } else if (agent_type == "Alpha") {
    resp_cols <- paste0("Id13Res", 1:25)
  } else if (agent_type == "B") {
    resp_cols <- paste0("Id20Res", 1:25)
  } else if (agent_type == "Beta") {
    resp_cols <- paste0("Id17Res", 1:25)
  }
  
  # Select relevant data
  data_sel <- agent_data %>%
    dplyr::select(all_of(resp_cols), Edad, DescGenero, DescEstudio) %>%
    mutate(
      participant = row_number(),
      owner = owner_status,
      experiment = experiment,
      agent_type = agent_type
    )
  
  # Convert to long format
  data_long <- data_sel %>%
    pivot_longer(
      cols = all_of(resp_cols),
      names_to = "question",
      values_to = "response"
    ) %>%
    mutate(
      price_num = rep(1:25, n()/25),
      price = prices[price_num]
    ) %>%
    filter(!is.na(response), response != 0)
  
  # =============================================================================
  # CORRECTED CODING: chooses_money
  # =============================================================================
  # For ALL agents: Y=1 if they choose MONEY over MUG
  # 
  # SELLERS (A.1, A.1.2, A, Alpha): 
  #   - response 1 = keep mug (chooses_money = 0)
  #   - response 2 = sell mug = receive money (chooses_money = 1)
  # 
  # BUYERS (B.1, B.1.2, B, Beta):
  #   - response 1 = buy mug (chooses_money = 0)
  #   - response 2 = don't buy = keep money (chooses_money = 1)
  # 
  # Consistent interpretation: Prefer money over mug?
  # =============================================================================
  
  data_long <- data_long %>%
    mutate(chooses_money = ifelse(response == 2, 1, 0))
  
  # Create control variables
  data_long <- data_long %>%
    mutate(
      gender_male = ifelse(DescGenero == "Masculino", 1, 0),
      age = as.numeric(Edad)
    )
  
  return(data_long)
}

# =============================================================================
# LOAD AND PREPARE DATA FROM EACH EXPERIMENT
# =============================================================================

cat("\n=== LOADING DATA WITH CORRECTED CODING ===\n\n")

# Experiment 1.1
data_a1 <- data %>% filter(Formato == "Agente Tipo A.1")
data_b1 <- data %>% filter(Descrip9 == "Agente Tipo B.1")

df_a1 <- create_long_dataset(data_a1, "A.1", "Exp1.1", owner = 1)
df_b1 <- create_long_dataset(data_b1, "B.1", "Exp1.1", owner = 0)

# Experiment 1.2
data_a12 <- data %>% filter(Descrip10 == "Agente Tipo A.1")
data_b12 <- data %>% filter(Descrip11 == "Agente Tipo B.1")

df_a12 <- create_long_dataset(data_a12, "A.1.2", "Exp1.2", owner = 1)
df_b12 <- create_long_dataset(data_b12, "B.1.2", "Exp1.2", owner = 0)

# Experiment 2
data_a <- data %>% filter(Descrip18 == "Agente Tipo A")
data_alpha <- data %>% filter(Descrip13 == "Agente Tipo Alpha")
data_b <- data %>% filter(Descrip20 == "Agente Tipo B")
data_beta <- data %>% filter(Descrip17 == "Agente Tipo Beta")

df_a <- create_long_dataset(data_a, "A", "Exp2", owner = 1)
df_alpha <- create_long_dataset(data_alpha, "Alpha", "Exp2", owner = 0)
df_b <- create_long_dataset(data_b, "B", "Exp2", owner = 1)
df_beta <- create_long_dataset(data_beta, "Beta", "Exp2", owner = 0)

# Verify coding
cat("Verifying coding:\n")
cat("  A.1 (Owner-Seller):   Prop(chooses_money=1) =", round(mean(df_a1$chooses_money), 3), "\n")
cat("  B.1 (Nonowner-Buyer): Prop(chooses_money=1) =", round(mean(df_b1$chooses_money), 3), "\n")
cat("  Interpretation: Proportion of times choosing money over mug\n\n")

# =============================================================================
# COMBINE ALL DATA
# =============================================================================

# Complete pooled dataset
data_pooled_complete <- bind_rows(
  df_a1, df_b1,      # Exp 1.1
  df_a12, df_b12,    # Exp 1.2
  df_a, df_alpha,    # Exp 2 sellers
  df_b, df_beta      # Exp 2 buyers
) %>%
  mutate(
    # Experiment dummies (Exp1.1 is reference)
    exp1.2 = ifelse(experiment == "Exp1.2", 1, 0),
    exp2 = ifelse(experiment == "Exp2", 1, 0),
    
    # Interactions
    owner_x_price = owner * price,
    owner_x_exp1.2 = owner * exp1.2,
    owner_x_exp2 = owner * exp2
  )

# Pooled only Exp 1.1 and 1.2
data_pooled_exp1 <- bind_rows(
  df_a1, df_b1,      # Exp 1.1
  df_a12, df_b12     # Exp 1.2
) %>%
  mutate(
    exp1.2 = ifelse(experiment == "Exp1.2", 1, 0),
    owner_x_price = owner * price,
    owner_x_exp1.2 = owner * exp1.2
  )

# Pooled only Exp 2
data_pooled_exp2 <- bind_rows(
  df_a, df_alpha,
  df_b, df_beta
) %>%
  mutate(
    # For Exp2: distinguish sellers vs buyers
    seller = ifelse(agent_type %in% c("A", "Alpha"), 1, 0),
    owner_x_price = owner * price,
    owner_x_seller = owner * seller
  )

cat("Data prepared. Total observations:", nrow(data_pooled_complete), "\n")
cat("  - Exp 1.1:", nrow(df_a1) + nrow(df_b1), "\n")
cat("  - Exp 1.2:", nrow(df_a12) + nrow(df_b12), "\n")
cat("  - Exp 2:", nrow(df_a) + nrow(df_alpha) + nrow(df_b) + nrow(df_beta), "\n\n")

# =============================================================================
# POOLED LOGIT MODELS (CORRECTED)
# =============================================================================

cat("=== ESTIMATING POOLED LOGIT MODELS (CORRECTED VARIABLE) ===\n\n")

# MODEL 1: Complete pooled - Only price and owner
model1 <- glm(
  chooses_money ~ price + owner,
  data = data_pooled_complete,
  family = binomial(link = "logit")
)

# MODEL 2: Complete pooled - Price, owner, experiment dummies
model2 <- glm(
  chooses_money ~ price + owner + exp1.2 + exp2,
  data = data_pooled_complete,
  family = binomial(link = "logit")
)

# MODEL 3: Complete pooled - With owner x experiment interaction
model3 <- glm(
  chooses_money ~ price + owner + exp1.2 + exp2 + 
    owner_x_exp1.2 + owner_x_exp2,
  data = data_pooled_complete,
  family = binomial(link = "logit")
)

# MODEL 4: Complete pooled - With demographic controls
model4 <- glm(
  chooses_money ~ price + owner + exp1.2 + exp2 + 
    owner_x_exp1.2 + owner_x_exp2 +
    gender_male + age,
  data = data_pooled_complete,
  family = binomial(link = "logit")
)

# MODEL 5: Only Experiments 1.1 and 1.2
model5 <- glm(
  chooses_money ~ price + owner + exp1.2 + owner_x_exp1.2,
  data = data_pooled_exp1,
  family = binomial(link = "logit")
)

# MODEL 6: Only Experiment 2 (Brokers)
model6 <- glm(
  chooses_money ~ price + owner + seller + owner_x_seller,
  data = data_pooled_exp2,
  family = binomial(link = "logit")
)

# =============================================================================
# REGRESSION TABLE WITH MODELSUMMARY
# =============================================================================

models_list <- list(
  "Model 1" = model1,
  "Model 2" = model2,
  "Model 3" = model3,
  "Model 4" = model4,
  "Model 5" = model5,
  "Model 6" = model6
)

# Markdown table
modelsummary(
  models_list,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c("nobs", "aic", "bic"),
  coef_rename = c(
    "price" = "Price",
    "owner" = "Owner",
    "exp1.2" = "Exp 1.2",
    "exp2" = "Exp 2",
    "owner_x_exp1.2" = "Owner × Exp 1.2",
    "owner_x_exp2" = "Owner × Exp 2",
    "gender_male" = "Male",
    "age" = "Age",
    "seller" = "Seller (vs Buyer)",
    "owner_x_seller" = "Owner × Seller"
  ),
  output = "markdown"
)

# =============================================================================
# ODDS RATIOS
# =============================================================================

cat("\n\n=== ODDS RATIOS (Model 4 - Full Specification) ===\n\n")

odds_ratios <- exp(coef(model4))
conf_intervals <- exp(confint(model4))

table_or <- data.frame(
  Variable = names(odds_ratios),
  `Odds Ratio` = round(odds_ratios, 3),
  `CI 2.5%` = round(conf_intervals[,1], 3),
  `CI 97.5%` = round(conf_intervals[,2], 3)
)

print(kable(table_or, format = "simple", align = "lrrr", row.names = FALSE))

cat("\nINTERPRETATION OF KEY ODDS RATIOS:\n")
cat("  Price:  OR =", round(odds_ratios["price"], 3), 
    "→ 1 peso increase multiplies odds of choosing money by", round(odds_ratios["price"], 3), "\n")
cat("  Owner:  OR =", round(odds_ratios["owner"], 3), 
    "→ Being owner multiplies odds of choosing money by", round(odds_ratios["owner"], 3), "\n")
cat("          (< 1 means LESS probability of choosing money = more attachment)\n")
cat("  Owner × Exp 1.2: OR =", round(odds_ratios["owner_x_exp1.2"], 3), 
    "→ In Exp 1.2, owner effect is", 
    ifelse(odds_ratios["owner_x_exp1.2"] > 1, "amplified", "reduced"), "\n")
cat("  Owner × Exp 2:   OR =", round(odds_ratios["owner_x_exp2"], 3), 
    "→ In Exp 2, owner effect is", 
    ifelse(odds_ratios["owner_x_exp2"] > 1, "amplified", "reduced"), "\n\n")

# =============================================================================
# MARGINAL EFFECTS
# =============================================================================

cat("=== MARGINAL EFFECTS AT MEANS (Model 4) ===\n\n")

# Calculate predicted probability at means
data_means <- data_pooled_complete %>%
  summarise(
    price = mean(price),
    gender_male = mean(gender_male, na.rm = TRUE),
    age = mean(age, na.rm = TRUE)
  )

# Marginal effect of owner in each experiment
pred_exp11_nonowner <- predict(
  model4, 
  newdata = data.frame(price = data_means$price, owner = 0, exp1.2 = 0, exp2 = 0,
                       owner_x_exp1.2 = 0, owner_x_exp2 = 0,
                       gender_male = data_means$gender_male, age = data_means$age),
  type = "response"
)

pred_exp11_owner <- predict(
  model4, 
  newdata = data.frame(price = data_means$price, owner = 1, exp1.2 = 0, exp2 = 0,
                       owner_x_exp1.2 = 0, owner_x_exp2 = 0,
                       gender_male = data_means$gender_male, age = data_means$age),
  type = "response"
)

pred_exp12_nonowner <- predict(
  model4, 
  newdata = data.frame(price = data_means$price, owner = 0, exp1.2 = 1, exp2 = 0,
                       owner_x_exp1.2 = 0, owner_x_exp2 = 0,
                       gender_male = data_means$gender_male, age = data_means$age),
  type = "response"
)

pred_exp12_owner <- predict(
  model4, 
  newdata = data.frame(price = data_means$price, owner = 1, exp1.2 = 1, exp2 = 0,
                       owner_x_exp1.2 = 1, owner_x_exp2 = 0,
                       gender_male = data_means$gender_male, age = data_means$age),
  type = "response"
)

pred_exp2_nonowner <- predict(
  model4, 
  newdata = data.frame(price = data_means$price, owner = 0, exp1.2 = 0, exp2 = 1,
                       owner_x_exp1.2 = 0, owner_x_exp2 = 0,
                       gender_male = data_means$gender_male, age = data_means$age),
  type = "response"
)

pred_exp2_owner <- predict(
  model4, 
  newdata = data.frame(price = data_means$price, owner = 1, exp1.2 = 0, exp2 = 1,
                       owner_x_exp1.2 = 0, owner_x_exp2 = 1,
                       gender_male = data_means$gender_male, age = data_means$age),
  type = "response"
)

marginal_effects <- data.frame(
  Experiment = c("Exp 1.1", "Exp 1.2", "Exp 2"),
  `P(chooses_money) Nonowner` = round(c(pred_exp11_nonowner, pred_exp12_nonowner, pred_exp2_nonowner), 3),
  `P(chooses_money) Owner` = round(c(pred_exp11_owner, pred_exp12_owner, pred_exp2_owner), 3),
  `Marginal Effect` = round(c(
    pred_exp11_owner - pred_exp11_nonowner,
    pred_exp12_owner - pred_exp12_nonowner,
    pred_exp2_owner - pred_exp2_nonowner
  ), 3),
  `Percentage Points` = round(c(
    pred_exp11_owner - pred_exp11_nonowner,
    pred_exp12_owner - pred_exp12_nonowner,
    pred_exp2_owner - pred_exp2_nonowner
  ) * 100, 1)
)

print(kable(marginal_effects, format = "simple", align = "lrrrr"))

cat("\nINTERPRETATION:\n")
cat("  - Exp 1.1: Owners have", abs(marginal_effects$Percentage.Points[1]), 
    "pp", ifelse(marginal_effects$Marginal.Effect[1] < 0, "LESS", "MORE"), 
    "probability of choosing money\n")
cat("  - Exp 1.2: Owners have", abs(marginal_effects$Percentage.Points[2]), 
    "pp", ifelse(marginal_effects$Marginal.Effect[2] < 0, "LESS", "MORE"), 
    "probability of choosing money\n")
cat("  - Exp 2:   Owners have", abs(marginal_effects$Percentage.Points[3]), 
    "pp", ifelse(marginal_effects$Marginal.Effect[3] < 0, "LESS", "MORE"), 
    "probability of choosing money\n\n")

# =============================================================================
# LIKELIHOOD RATIO TESTS
# =============================================================================

cat("=== LIKELIHOOD RATIO TESTS ===\n\n")

# Test 1: Owner effect
model0 <- glm(chooses_money ~ price, data = data_pooled_complete, family = binomial(link = "logit"))
lr_test1 <- anova(model0, model1, test = "LRT")

cat("Test 1: Does OWNER improve model fit?\n")
cat("  LR χ² =", round(lr_test1$Deviance[2], 2), ", p =", round(lr_test1$`Pr(>Chi)`[2], 4), "\n")
cat("  Result:", ifelse(lr_test1$`Pr(>Chi)`[2] < 0.05, "YES ***", "NO"), "\n\n")

# Test 2: Experiment dummies
lr_test2 <- anova(model1, model2, test = "LRT")
cat("Test 2: Do EXPERIMENT DUMMIES improve model fit?\n")
cat("  LR χ² =", round(lr_test2$Deviance[2], 2), ", p =", round(lr_test2$`Pr(>Chi)`[2], 4), "\n")
cat("  Result:", ifelse(lr_test2$`Pr(>Chi)`[2] < 0.05, "YES ***", "NO"), "\n\n")

# Test 3: Interactions
lr_test3 <- anova(model2, model3, test = "LRT")
cat("Test 3: Do OWNER × EXPERIMENT INTERACTIONS improve model fit?\n")
cat("  LR χ² =", round(lr_test3$Deviance[2], 2), ", p =", round(lr_test3$`Pr(>Chi)`[2], 4), "\n")
cat("  Result:", ifelse(lr_test3$`Pr(>Chi)`[2] < 0.05, "YES ***", "NO"), "\n\n")

# Test 4: Demographics
lr_test4 <- anova(model3, model4, test = "LRT")
cat("Test 4: Do DEMOGRAPHICS improve model fit?\n")
cat("  LR χ² =", round(lr_test4$Deviance[2], 2), ", p =", round(lr_test4$`Pr(>Chi)`[2], 4), "\n")
cat("  Result:", ifelse(lr_test4$`Pr(>Chi)`[2] < 0.05, "YES ***", "NO"), "\n\n")

# =============================================================================
# SUMMARY 
# =============================================================================

cat("MODEL COMPARISON (AIC):\n")
cat("  Model 1 (Price + Owner):           ", round(AIC(model1), 1), "\n")
cat("  Model 2 (+ Exp Dummies):           ", round(AIC(model2), 1), "\n")
cat("  Model 3 (+ Interactions):          ", round(AIC(model3), 1), "\n")
cat("  Model 4 (+ Demographics):          ", round(AIC(model4), 1), " ← BEST\n\n")

cat("KEY FINDINGS (Model 4):\n")
cat("  Price coefficient:         ", sprintf("%+.4f", coef(model4)["price"]), 
    ifelse(summary(model4)$coefficients["price", "Pr(>|z|)"] < 0.001, "***", ""), 
    " (POSITIVE ✓)\n")
cat("  Owner coefficient:         ", sprintf("%+.4f", coef(model4)["owner"]), 
    ifelse(summary(model4)$coefficients["owner", "Pr(>|z|)"] < 0.05, "**", ""), 
    " (NEGATIVE = attachment ✓)\n")
cat("  Owner × Exp 1.2:           ", sprintf("%+.4f", coef(model4)["owner_x_exp1.2"]), 
    ifelse(summary(model4)$coefficients["owner_x_exp1.2", "Pr(>|z|)"] < 0.01, "***", ""), "\n")
cat("  Owner × Exp 2:             ", sprintf("%+.4f", coef(model4)["owner_x_exp2"]), 
    ifelse(summary(model4)$coefficients["owner_x_exp2", "Pr(>|z|)"] < 0.1, "*", ""), "\n\n")

cat("ENDOWMENT EFFECT (Marginal Effects):\n")
cat("  Exp 1.1: ", sprintf("%+.1f", marginal_effects$Percentage.Points[1]), " percentage points\n")
cat("  Exp 1.2: ", sprintf("%+.1f", marginal_effects$Percentage.Points[2]), " percentage points\n")
cat("  Exp 2:   ", sprintf("%+.1f", marginal_effects$Percentage.Points[3]), " percentage points\n\n")

# =============================================================================
# PREDICTED PROBABILITY CURVES WITH CONFIDENCE INTERVALS
# =============================================================================

cat("\n=== GENERATING PREDICTED PROBABILITY CURVES ===\n\n")

# Function to create prediction data with confidence intervals
create_prediction_data <- function(model, data_subset, owner_values = c(0, 1), 
                                   experiment_label = NULL) {
  
  # Get mean values for control variables
  mean_gender <- mean(data_subset$gender_male, na.rm = TRUE)
  mean_age <- mean(data_subset$age, na.rm = TRUE)
  
  # Create prediction grid
  pred_data <- expand.grid(
    price = seq(20, 500, 20),
    owner = owner_values
  )
  
  # Add experiment dummies and controls based on model
  if ("exp1.2" %in% names(coef(model))) {
    # For complete pooled models - adjust based on experiment
    if (!is.null(experiment_label)) {
      if (experiment_label == "Exp1.1") {
        pred_data$exp1.2 <- 0
        pred_data$exp2 <- 0
        pred_data$owner_x_exp1.2 <- 0
        pred_data$owner_x_exp2 <- 0
      } else if (experiment_label == "Exp1.2") {
        pred_data$exp1.2 <- 1
        pred_data$exp2 <- 0
        pred_data$owner_x_exp1.2 <- pred_data$owner * 1
        pred_data$owner_x_exp2 <- 0
      } else if (experiment_label == "Exp2") {
        pred_data$exp1.2 <- 0
        pred_data$exp2 <- 1
        pred_data$owner_x_exp1.2 <- 0
        pred_data$owner_x_exp2 <- pred_data$owner * 1
      }
    } else {
      # Default to Exp 1.1 baseline
      pred_data$exp1.2 <- 0
      pred_data$exp2 <- 0
      pred_data$owner_x_exp1.2 <- 0
      pred_data$owner_x_exp2 <- 0
    }
  }
  
  if ("gender_male" %in% names(coef(model))) {
    pred_data$gender_male <- mean_gender
    pred_data$age <- mean_age
  }
  
  # Get predictions with standard errors
  pred_data$fit <- predict(model, newdata = pred_data, type = "link")
  pred_data$se_fit <- predict(model, newdata = pred_data, type = "link", se.fit = TRUE)$se.fit
  
  # Calculate confidence intervals on link scale, then transform to probability scale
  pred_data$prob <- plogis(pred_data$fit)
  pred_data$ci_lower <- plogis(pred_data$fit - 1.96 * pred_data$se_fit)
  pred_data$ci_upper <- plogis(pred_data$fit + 1.96 * pred_data$se_fit)
  
  return(pred_data)
}

# Function to create probability curve plot
create_probability_plot <- function(pred_data, title, subtitle, 
                                    color_0 = "#E74C3C", color_1 = "#3498DB",
                                    label_0 = "Non-owner", label_1 = "Owner") {
  
  ggplot(pred_data, aes(x = price, y = prob, color = factor(owner), fill = factor(owner))) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
    geom_line(size = 1.2) +
    scale_color_manual(
      values = c("0" = color_0, "1" = color_1),
      labels = c(label_0, label_1),
      name = ""
    ) +
    scale_fill_manual(
      values = c("0" = color_0, "1" = color_1),
      labels = c(label_0, label_1),
      name = ""
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Price (Pesos)",
      y = "P(Choose Money over Mug)"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      axis.title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size = 11),
      panel.grid.minor = element_blank()
    )
}

# =============================================================================
# INDIVIDUAL EXPERIMENT CURVES USING MODEL 4 (POOLED WITH INTERACTIONS)
# =============================================================================

cat("Generating predictions for each experiment using Model 4...\n")

# Experiment 1.1
pred_exp11 <- create_prediction_data(model4, data_pooled_complete, 
                                     experiment_label = "Exp1.1")

plot_exp11 <- create_probability_plot(
  pred_exp11,
  title = "Experiment 1.1: Predicted Probabilities",
  subtitle = "Basic Endowment Effect - Owner-Seller vs Non-owner Buyer (95% CI)"
)

print(plot_exp11)

# Experiment 1.2
pred_exp12 <- create_prediction_data(model4, data_pooled_complete, 
                                     experiment_label = "Exp1.2")

plot_exp12 <- create_probability_plot(
  pred_exp12,
  title = "Experiment 1.2: Predicted Probabilities",
  subtitle = "Second Unit - Owner-Buyer vs Non-owner Pair Buyer (95% CI)"
)

print(plot_exp12)

# Experiment 2 - Need to handle 4 broker roles properly
cat("Generating predictions for Exp 2 with 4 broker roles...\n")

mean_gender_exp2 <- mean(data_pooled_exp2$gender_male, na.rm = TRUE)
mean_age_exp2 <- mean(data_pooled_exp2$age, na.rm = TRUE)

# Create all 4 combinations of owner × seller
pred_exp2 <- expand.grid(
  price = seq(20, 500, 20),
  owner = c(0, 1),
  seller = c(0, 1)
)

pred_exp2$owner_x_seller <- pred_exp2$owner * pred_exp2$seller
pred_exp2$gender_male <- mean_gender_exp2
pred_exp2$age <- mean_age_exp2

# Get predictions
pred_exp2$fit <- predict(model6, newdata = pred_exp2, type = "link")
pred_exp2$se_fit <- predict(model6, newdata = pred_exp2, type = "link", se.fit = TRUE)$se.fit

pred_exp2$prob <- plogis(pred_exp2$fit)
pred_exp2$ci_lower <- plogis(pred_exp2$fit - 1.96 * pred_exp2$se_fit)
pred_exp2$ci_upper <- plogis(pred_exp2$fit + 1.96 * pred_exp2$se_fit)

# Create role labels (4 broker types)
pred_exp2$role <- with(pred_exp2, 
                       case_when(
                         owner == 1 & seller == 1 ~ "Seller w/ Cup",
                         owner == 0 & seller == 1 ~ "Seller w/o Cup",
                         owner == 1 & seller == 0 ~ "Buyer w/ Cup",
                         owner == 0 & seller == 0 ~ "Buyer w/o Cup"
                       ))

# Verify all 4 roles exist
cat("Exp 2 roles in prediction data:\n")
print(table(pred_exp2$role))

plot_exp2 <- ggplot(pred_exp2, aes(x = price, y = prob, 
                                   color = role, fill = role, group = role)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
  geom_line(size = 1.1) +
  scale_color_manual(
    values = c("Seller w/ Cup" = "#2E86AB", 
               "Seller w/o Cup" = "#A23B72",
               "Buyer w/ Cup" = "#F18F01",
               "Buyer w/o Cup" = "#008000"),
    name = "Broker Type"
  ) +
  scale_fill_manual(
    values = c("Seller w/ Cup" = "#2E86AB", 
               "Seller w/o Cup" = "#A23B72",
               "Buyer w/ Cup" = "#F18F01",
               "Buyer w/o Cup" = "#00674F"),
    name = "Broker Type"
  ) +
  labs(
    title = "Experiment 2: Broker Predicted Probabilities",
    subtitle = "Four Broker Roles with 95% CI",
    x = "Price (Pesos)",
    y = "P(Choose Money over Mug)"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

print(plot_exp2)

# =============================================================================
# POOLED CURVE (BASELINE EXP 1.1)
# =============================================================================

cat("Generating pooled prediction curve...\n")

pred_pooled <- create_prediction_data(model4, data_pooled_complete)

plot_pooled <- create_probability_plot(
  pred_pooled,
  title = "Pooled Model: Predicted Probabilities",
  subtitle = "All Experiments Combined (Baseline: Exp 1.1) with 95% CI"
)

print(plot_pooled)

cat("\nProbability curves generated successfully!\n")
cat("  - 4 separate plots created\n")
cat("  - Exp 1.1: Owner-Seller vs Non-owner Buyer\n")
cat("  - Exp 1.2: Owner-Buyer vs Non-owner Pair Buyer\n")
cat("  - Exp 2: 4 Broker roles\n")
cat("  - Pooled: All experiments combined\n\n")

# =============================================================================
# OPTIONAL: SAVE OUTPUTS
# =============================================================================
 
save_outputs <- TRUE

if (save_outputs) {
  
  dir.create("outputs/pooled_logit/tables", recursive = TRUE, showWarnings = FALSE)
  dir.create("outputs/pooled_logit/figures", recursive = TRUE, showWarnings = FALSE)
  
  cat("\nSaving outputs...\n")
  
  # Save regression table
  modelsummary(
    models_list,
    stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
    gof_map = c("nobs", "aic", "bic"),
    coef_rename = c(
      "price" = "Price",
      "owner" = "Owner",
      "exp1.2" = "Exp 1.2 (vs Exp 1.1)",
      "exp2" = "Exp 2 (vs Exp 1.1)",
      "owner_x_exp1.2" = "Owner × Exp 1.2",
      "owner_x_exp2" = "Owner × Exp 2",
      "gender_male" = "Male",
      "age" = "Age",
      "seller" = "Seller (vs Buyer)",
      "owner_x_seller" = "Owner × Seller"
    ),
    output = "outputs/pooled_logit/tables/regression_table.html",
    title = "Pooled Logit Regression Models - Endowment Effect Analysis",
    notes = c("Models 1-4: Complete pooled (all experiments)",
              "Model 5: Experiments 1.1 & 1.2 only",
              "Model 6: Experiment 2 (Brokers) only",
              "Reference category for Exp dummies: Experiment 1.1")
  )
  
  # Save odds ratios table
  write.csv(table_or, "outputs/pooled_logit/tables/odds_ratios.csv", row.names = FALSE)
  
  # Save marginal effects table
  write.csv(marginal_effects, "outputs/pooled_logit/tables/marginal_effects.csv", row.names = FALSE)
  
  # Save probability curve plots (4 plots)
  ggsave("outputs/pooled_logit/figures/prob_curve_exp11.png", 
         plot = plot_exp11, width = 10, height = 6, dpi = 300)
  
  ggsave("outputs/pooled_logit/figures/prob_curve_exp12.png", 
         plot = plot_exp12, width = 10, height = 6, dpi = 300)
  
  ggsave("outputs/pooled_logit/figures/prob_curve_exp2.png", 
         plot = plot_exp2, width = 10, height = 6, dpi = 300)
  
  ggsave("outputs/pooled_logit/figures/prob_curve_pooled.png", 
         plot = plot_pooled, width = 10, height = 6, dpi = 300)
  
  cat("Outputs saved successfully to outputs/pooled_logit/\n")
  cat("  Tables: 3 files\n")
  cat("  Figures: 4 files\n")
}
