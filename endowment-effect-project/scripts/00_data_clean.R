# =============================================================================
# SCRIPT 00: DATA CLEANING AND PREPARATION
# =============================================================================

cat("\014")
rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

data_raw <- read_excel("/Users/jeanmarroquin/Documents/Economia/asistente/dotacion/nueva_version/data/raw/Encuesta.xlsx")

cat("Participants loaded:", nrow(data_raw), "\n")
cat("Variables:", ncol(data_raw), "\n")

# =============================================================================
# 2. STRUCTURE DEFINITION
# =============================================================================

# Prices in order for the 10 items in each treatment
prices <- c(25, 50, 75, 100, 125, 150, 175, 200, 225, 250)

# Treatments and their column names in the dataset
treatments <- list(
  S1.1 = paste0("R_S11_", 1:10),
  S1.2 = paste0("R_S12_", 1:10),
  B1.1 = paste0("R_B11_", 1:10),
  B1.2 = paste0("R_B12_", 1:10),
  S2.1 = paste0("R_S21_", 1:10),
  S2.2 = paste0("R_S22_", 1:10),
  B2.1 = paste0("R_B21_", 1:10),
  B2.2 = paste0("R_B22_", 1:10)
)

# Risk aversion columns in experimental order
cols_aversion <- paste0("R_Aversion_", 1:10)

# =============================================================================
# 3. FUNCTION: SWITCHING POINT CALCULATION
# =============================================================================
# Convention: B = accepts the transaction (sell or buy depending on role)
# Switching point = first price at which the response is B
# If any response in the treatment is 0, the participant is excluded
# If participant never responds B, switching point = 275 (censored above)

calc_switching_point <- function(responses, prices) {
  # If any response is 0, exclude
  if (any(responses == "0")) return(NA_real_)
  
  # First price at which response is B
  idx_b <- which(responses == "B")
  
  # Never accepted: valuation censored above, assign 275
  if (length(idx_b) == 0) return(275)
  
  return(prices[min(idx_b)])
}

# =============================================================================
# 4. SOCIOECONOMIC AND DEMOGRAPHIC VARIABLES
# =============================================================================

data_demo <- data_raw %>%
  dplyr::select(
    id               = Id_D_UsSocioeconomico,
    date             = Fecha_Registro,
    age              = Edad,
    gender           = Genero,
    marital_status   = EstadoCivil,
    education        = NivelEstudio,
    field_of_study   = Desc_AreaEstudio,
    transport        = DescTransporte,
    num_cars         = NoAutos,
    entertainment1   = Entretenimiento1,
    entertainment2   = Entretenimiento2,
    entertainment3   = Entretenimiento3,
    entertainment4   = Entretenimiento4,
    entertainment5   = Entretenimiento5,
    entertainment6   = Entretenimiento6,
    state            = NombreEstado
  )

# =============================================================================
# 5. RISK AVERSION
# =============================================================================
# Holt-Laury convention: A = safe option, B = risky option
# Aversion score = number of times participant chooses A
# Higher score = higher risk aversion

aversion_raw <- data_raw %>%
  dplyr::select(id = Id_D_UsSocioeconomico, all_of(cols_aversion))

aversion_scores <- aversion_raw %>%
  rowwise() %>%
  mutate(
    aversion_valid = !any(c_across(all_of(cols_aversion)) == "0"),
    aversion_score = if_else(
      aversion_valid,
      sum(c_across(all_of(cols_aversion)) == "A"),
      NA_integer_
    )
  ) %>%
  ungroup() %>%
  dplyr::select(id, aversion_valid, aversion_score)

cat("\nRisk aversion score distribution:\n")
print(table(aversion_scores$aversion_score, useNA = "always"))

# =============================================================================
# 6. SWITCHING POINTS BY TREATMENT
# =============================================================================

switching_wide <- data_raw %>%
  dplyr::select(id = Id_D_UsSocioeconomico) %>%
  mutate(id = as.integer(id))

for (trt in names(treatments)) {
  cols   <- treatments[[trt]]
  sp_col <- paste0("sp_", gsub("\\.", "_", trt))
  
  switching_wide[[sp_col]] <- apply(
    data_raw[, cols], 1,
    function(row) calc_switching_point(as.character(row), prices)
  )
}

# Diagnostic: zeros vs never accepted
cat("\nDiagnostic of missing switching points by treatment:\n")
for (trt in names(treatments)) {
  cols <- treatments[[trt]]
  
  has_zero <- apply(data_raw[, cols], 1,
                    function(row) any(as.character(row) == "0"))
  never_b  <- apply(data_raw[, cols], 1,
                    function(row) all(as.character(row) != "B") &
                      !any(as.character(row) == "0"))
  
  cat(sprintf("  %-6s: %d with zero, %d never accepted\n",
              trt, sum(has_zero), sum(never_b)))
}

# =============================================================================
# 7. MISSING VALUE REPORT
# =============================================================================

sp_cols <- names(switching_wide)[names(switching_wide) != "id"]

cat("\nParticipants with valid switching point by treatment:\n")
for (col in sp_cols) {
  n_valid   <- sum(!is.na(switching_wide[[col]]))
  n_excluded <- sum(is.na(switching_wide[[col]]))
  cat(sprintf("  %-10s: %d valid, %d excluded\n",
              gsub("sp_", "", col), n_valid, n_excluded))
}

complete_cases <- switching_wide %>%
  filter(if_all(all_of(sp_cols), ~ !is.na(.)))
cat("\nParticipants complete across all treatments:", nrow(complete_cases), "\n")

# =============================================================================
# 8. WIDE DATASET: one row per participant
# =============================================================================

data_wide <- data_demo %>%
  left_join(aversion_scores, by = "id") %>%
  left_join(switching_wide, by = "id")

cat("\nWide dataset dimensions:", nrow(data_wide), "x", ncol(data_wide), "\n")

# =============================================================================
# 9. LONG DATASET: one row per participant-treatment
# =============================================================================

data_long <- switching_wide %>%
  pivot_longer(
    cols      = all_of(sp_cols),
    names_to  = "treatment",
    values_to = "switching_point"
  ) %>%
  mutate(
    treatment = gsub("sp_", "", treatment),
    treatment = gsub("_", ".", treatment),
    role = case_when(
      grepl("^S", treatment) ~ "seller",
      grepl("^B", treatment) ~ "buyer"
    ),
    level = case_when(
      grepl("^.1", treatment) ~ "personal",
      grepl("^.2", treatment) ~ "broker"
    ),
    condition = case_when(
      treatment == "S1.1" ~ "owner_buyer_no_mug",
      treatment == "S1.2" ~ "owner_buyer_has_mug",
      treatment == "B1.1" ~ "non_owner",
      treatment == "B1.2" ~ "owner_second_mug",
      treatment == "S2.1" ~ "broker_seller_owner",
      treatment == "S2.2" ~ "broker_seller_non_owner",
      treatment == "B2.1" ~ "broker_buyer_owner",
      treatment == "B2.2" ~ "broker_buyer_non_owner"
    )
  ) %>%
  left_join(
    data_demo %>% left_join(aversion_scores, by = "id"),
    by = "id"
  )

cat("Long dataset dimensions:", nrow(data_long), "x", ncol(data_long), "\n")
