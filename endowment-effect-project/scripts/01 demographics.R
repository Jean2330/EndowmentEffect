# =============================================================================
# DEMOGRAPHIC ANALYSIS
# =============================================================================
# Project: Endowment Effect Beyond the Lab
# Description: Descriptive analysis of participant demographics
# =============================================================================

# Clean environment
rm(list = ls())
cat("\014")

# Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(kableExtra)
library(treemapify)

# Configure custom theme for plots
theme_custom <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# Load data
datos <- read_excel("/Users/jeanmarroquin/Documents/Economia/asistente/dotacion/endowment-effect-project/data/raw/data.xlsx")

# Verify dimensions
cat("Total participants:", nrow(datos), "\n")
cat("Total variables:", ncol(datos), "\n")

# =============================================================================
# 1. AGE
# =============================================================================

# Descriptive statistics for age
edad_stats <- datos %>%
  summarise(
    n = n(),
    mean = round(mean(Edad, na.rm = TRUE), 2),
    median = median(Edad, na.rm = TRUE),
    std_dev = round(sd(Edad, na.rm = TRUE), 2),
    minimum = min(Edad, na.rm = TRUE),
    maximum = max(Edad, na.rm = TRUE),
    q25 = quantile(Edad, 0.25, na.rm = TRUE),
    q75 = quantile(Edad, 0.75, na.rm = TRUE)
  )

print(edad_stats)

# Age distribution plot
grafico_edad <- ggplot(datos, aes(x = Edad)) +
  geom_histogram(binwidth = 2, fill = "#3498db", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = mean(Edad, na.rm = TRUE)), 
             color = "#e74c3c", linetype = "dashed", size = 1) +
  labs(
    title = "Age Distribution of Participants",
    subtitle = paste("Mean:", round(mean(datos$Edad, na.rm = TRUE), 1), "years"),
    x = "Age (years)",
    y = "Frequency"
  ) +
  theme_custom

print(grafico_edad)

# Age groups
datos$grupo_edad <- cut(datos$Edad, 
                        breaks = c(0, 25, 35, 45, 55, 100),
                        labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
                        right = FALSE)

tabla_edad <- table(datos$grupo_edad, useNA = "ifany")
cat("\nAge group distribution:\n")
print(tabla_edad)

# =============================================================================
# 2. GENDER
# =============================================================================

# Translate gender to English
datos$Gender_EN <- case_when(
  datos$DescGenero == "Masculino" ~ "Male",
  datos$DescGenero == "Femenino" ~ "Female",
  TRUE ~ "Other"
)

# Gender frequency table
tabla_genero <- datos %>%
  count(Gender_EN, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", percentage, "%)")
  )

print(tabla_genero)

# Gender plot
tabla_genero2 <- tabla_genero %>%
  mutate(prop = n / sum(n),
         label_prop = scales::percent(prop, accuracy = 1))

grafico_genero <- ggplot(tabla_genero2, 
                         aes(x = "", y = n, fill = Gender_EN)) +
  geom_col(width = 1, color = "white", alpha = 0.8) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = label_prop),
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  scale_fill_manual(values = c("Female" = "#7CB342", "Male" = "#5E35B1", "Other" = "#FF5C00")) +
  labs(title = "Gender Distribution",
       fill = "Gender",
       x = NULL,
       y = NULL) +
  theme_void() +
  theme_custom + 
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

print(grafico_genero)

# =============================================================================
# 3. EDUCATION LEVEL
# =============================================================================

# Translate education levels to English
datos$Education_EN <- case_when(
  datos$DescEstudio == "Primaria" ~ "Primary School",
  datos$DescEstudio == "Secundaria" ~ "Secondary School",
  datos$DescEstudio == "Preparatoria" ~ "High School",
  datos$DescEstudio == "Licenciatura" ~ "Bachelor's Degree",
  datos$DescEstudio == "Maestría" ~ "Master's Degree",
  datos$DescEstudio == "Doctorado" ~ "Doctorate (PhD)",
  datos$DescEstudio == "Técnico" ~ "Technician",
  datos$DescEstudio == "Ingeniería" ~ "Engineering Degree",
  datos$DescEstudio == "No Aplica" ~ "NA",
  TRUE ~ datos$DescEstudio
)

# Education table
tabla_estudios <- datos %>%
  count(Education_EN, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(Education_EN, "\n", percentage, "%")
  ) %>%
  arrange(desc(n))

print(tabla_estudios)

# Define specific colors for each level
colores_educacion <- c(
  "Engineering Degree" = "#4CAF50",
  "Bachelor's Degree" = "#E53935",
  "Master's Degree" = "#FF9800",
  "Doctorate (PhD)" = "#1E88E5"
)

# Treemap plot
grafico_estudios <- ggplot(tabla_estudios, aes(area = n, fill = Education_EN, label = label)) +
  geom_treemap(color = "white", size = 2) +
  geom_treemap_text(color = "white", place = "centre", size = 30, fontface = "bold") +
  scale_fill_manual(values = colores_educacion) +
  labs(title = "Education Level Distribution") +
  theme_custom +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0, size = 24, face = "plain")
  )

print(grafico_estudios)

# =============================================================================
# 3.5 FIELD OF STUDY
# =============================================================================

# Translate fields of study to English
datos$Area_EN <- case_when(
  datos$DescAreaE == "Ciencias Políticas y Relaciones Internacionales" ~ "Political Science and International Relations",
  datos$DescAreaE == "Derecho" ~ "Law",
  datos$DescAreaE == "Economía" ~ "Economics",
  datos$DescAreaE == "Economía Agrícola" ~ "Agricultural Economics",
  datos$DescAreaE == "Matemáticas" ~ "Mathematics",
  datos$DescAreaE == "Política Pública" ~ "Public Policy",
  datos$DescAreaE == "Desarrollo" ~ "Development",
  datos$DescAreaE == "No Aplica" ~ "NA",
  TRUE ~ datos$DescAreaE
)

# Field of study table
tabla_area <- datos %>%
  count(Area_EN, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", percentage, "%)")
  ) %>%
  arrange(desc(n))

print(tabla_area) 
colores_area <- c("#E53935", "#4CAF50", "#FF9800", "#1E88E5", "#9C27B0", "#00BCD4", "#FFC107", "#8BC34A")

# Field of study plot
grafico_area <- ggplot(tabla_area, aes(x = reorder(Area_EN, n), y = n, fill = Area_EN)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = colores_area) +
  coord_flip() +
  labs(
    title = "Discipline Distribution",
    x = "Discipline",
    y = "Number of Participants"
  ) +
  theme_custom +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(grafico_area)

# =============================================================================
# 4. STATE OF ORIGIN
# =============================================================================

# State table (keeping original Spanish names)
tabla_estados <- datos %>%
  count(EntDescripcion, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", percentage, "%)")
  ) %>%
  arrange(desc(n))

print(tabla_estados)

# State plot
grafico_estados <- ggplot(tabla_estados, aes(x = reorder(EntDescripcion, n), y = n, fill = EntDescripcion)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_flip() +
  labs(
    title = "Distribution by State of Origin",
    x = "State",
    y = "Number of Participants"
  ) +
  theme_custom +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(grafico_estados)

# =============================================================================
# 5. TRANSPORTATION
# =============================================================================

# Translate transportation methods to English
datos$Transport_EN <- case_when(
  datos$TransporteDescripcion == "Camión" ~ "Bus",
  datos$TransporteDescripcion == "Metro" ~ "Subway",
  datos$TransporteDescripcion == "Automovil" ~ "Own Car",
  datos$TransporteDescripcion == "Taxi" ~ "Ride-sharing/Taxi",
  datos$TransporteDescripcion == "Bicicleta" ~ "Bicycle",
  datos$TransporteDescripcion == "Caminando" ~ "Walking",
  TRUE ~ datos$TransporteDescripcion
)

# Transportation table
tabla_transporte <- datos %>%
  count(Transport_EN, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", percentage, "%)")
  ) %>%
  arrange(desc(n))

print(tabla_transporte)

# Transportation plot
grafico_transporte <- ggplot(tabla_transporte, aes(x = reorder(Transport_EN, n), y = n, fill = Transport_EN)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
  scale_fill_brewer(type = "qual", palette = "Spectral") +
  coord_flip() +
  labs(
    title = "Primary Transportation Method Distribution",
    x = "Transportation Method",
    y = "Number of Participants"
  ) +
  theme_custom +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(grafico_transporte)

# =============================================================================
# 6. CAR OWNERSHIP
# =============================================================================

# Car ownership table
tabla_autos <- datos %>%
  count(`AutosPosee`, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", percentage, "%)")
  ) %>%
  arrange(`AutosPosee`)

print(tabla_autos)

# Car ownership plot
grafico_autos <- ggplot(tabla_autos, aes(x = as.factor(`AutosPosee`), y = n, fill = as.factor(`AutosPosee`))) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 4) +
  scale_fill_brewer(type = "seq", palette = "Blues") +
  labs(
    title = "Number of Cars per Household",
    x = "Number of Cars",
    y = "Number of Participants"
  ) +
  theme_custom +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

print(grafico_autos)

# =============================================================================
# 7. MEDIA CONSUMPTION ANALYSIS
# =============================================================================

# Find exact column names for media
columnas_medios <- names(datos)[grepl("TV|Netflix|Amazon|Disney|HBO|Ninguna", names(datos))]
print(columnas_medios)

# Check values in these columns
print(unique(datos$TVAbierta))

# Create media consumption table with English names
consumo_medios <- data.frame(
  platform = c("Broadcast TV", "Cable TV", "Netflix", "Amazon Prime", "Disney+", "HBO", "None"),
  users = c(
    sum(datos[[columnas_medios[1]]] == "TRUE", na.rm = TRUE),
    sum(datos[[columnas_medios[2]]] == "TRUE", na.rm = TRUE),
    sum(datos[[columnas_medios[3]]] == "TRUE", na.rm = TRUE),
    sum(datos[[columnas_medios[4]]] == "TRUE", na.rm = TRUE),
    sum(datos[[columnas_medios[5]]] == "TRUE", na.rm = TRUE),
    sum(datos[[columnas_medios[6]]] == "TRUE", na.rm = TRUE),
    sum(datos[[columnas_medios[7]]] == "TRUE", na.rm = TRUE)
  )
) %>%
  mutate(
    percentage = round(users / nrow(datos) * 100, 1),
    label = paste0(users, " (", percentage, "%)")
  ) %>%
  arrange(desc(users))

print(consumo_medios)

# Media consumption plot
grafico_medios <- ggplot(consumo_medios, aes(x = reorder(platform, users), y = users, fill = platform)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  coord_flip() +
  labs(
    title = "Entertainment Media Consumption",
    subtitle = "Number of users per platform",
    x = "Platform/Media",
    y = "Number of Users"
  ) +
  theme_custom +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(grafico_medios)

# =============================================================================
# 8. AGE AND GENDER INTERACTION
# =============================================================================

# Age-gender crosstab
tabla_edad_genero <- datos %>%
  filter(!is.na(grupo_edad) & !is.na(Gender_EN)) %>%
  count(grupo_edad, Gender_EN) %>%
  pivot_wider(names_from = Gender_EN, values_from = n, values_fill = 0)

print(tabla_edad_genero)

# Age by gender plot
grafico_edad_genero <- datos %>%
  filter(!is.na(Edad) & !is.na(Gender_EN)) %>%
  ggplot(aes(x = Gender_EN, y = Edad, fill = Gender_EN)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(
    title = "Age Distribution by Gender",
    x = "Gender",
    y = "Age (years)"
  ) +
  theme_custom +
  theme(legend.position = "none")

print(grafico_edad_genero)

# =============================================================================
# 9. DEMOGRAPHIC SUMMARY
# =============================================================================

resumen_demografico <- data.frame(
  Variable = c("Total participants", 
               "Average age", 
               "Most frequent gender",
               "Most common education level",
               "Most represented state",
               "Most used transportation",
               "Average cars per household",
               "Most popular platform"),
  Value = c(nrow(datos),
            paste(round(mean(datos$Edad, na.rm = TRUE), 1), "years"),
            tabla_genero$Gender_EN[which.max(tabla_genero$n)],
            tabla_estudios$Education_EN[1],
            tabla_estados$EntDescripcion[1],
            tabla_transporte$Transport_EN[1],
            paste(round(mean(datos$`AutosPosee`, na.rm = TRUE), 1), "cars"),
            consumo_medios$platform[1])
)

print(resumen_demografico)