# =========================================================================
# 02_data_cleaning.R
# 
# Este script realiza la limpieza y preparación de los datos de la GEIH 2018
# para el análisis de ingresos. Asegura compatibilidad con 03_age_wage_profile.R
# y 04_gender_gap.R.
# =========================================================================

# Cargar paquetes necesarios
if(!require(pacman)) install.packages("pacman")
pacman::p_load(rio, tidyverse, skimr, visdat, car, lmtest, boot, stargazer, ggplot2)

# =========================================================================
# 1. Carga de datos
# =========================================================================
message("Cargando datos...")
geih_raw <- import("stores/raw/geih_2018_raw.rds") %>% as_tibble()

# Filtrar personas ocupadas mayores de 18 años
geih_filtered <- geih_raw %>%
  filter(age > 18, ocu == 1)

# =========================================================================
# 1.1 Exploración de valores faltantes
# =========================================================================
message("Generando visualización de valores faltantes...")
missing_values_plot <- visdat::vis_miss(geih_raw %>% slice_sample(n = 5000))
ggsave("views/figures/missing_values.png", plot = missing_values_plot, width = 8, height = 6)

# =========================================================================
# 2. Creación de variables clave
# =========================================================================
message("Creando variables clave...")

datos_procesados <- geih_filtered %>%
  mutate(
    y_ingLab_m = replace_na(y_ingLab_m, 0),
    ingtot = replace_na(ingtot, 0),
    ingtot = ifelse(ingtot == 0, median(ingtot[ingtot > 0], na.rm = TRUE), ingtot),
    totalHoursWorked = replace_na(totalHoursWorked, median(totalHoursWorked, na.rm = TRUE)),
    hourly_wage = ingtot / (totalHoursWorked * 4.345),
    female = as.factor(ifelse(sex == 0, 1, 0)),
    maxEducLevel = as.factor(replace_na(maxEducLevel, median(maxEducLevel, na.rm = TRUE))),
    log_ingtot = log(ingtot + 1),
    log_hourly_wage = log(hourly_wage + 1),
    age_squared = age^2,
    age_cubed = age^3,
    female_age = as.numeric(female) * age
  ) %>%
  filter(ingtot > 0, totalHoursWorked > 0) %>%
  as_tibble()  # Convertir explícitamente a tibble

# =========================================================================
# 3. Manejo de outliers
# =========================================================================
datos_procesados <- datos_procesados %>%
  mutate(z_score_wage = (hourly_wage - mean(hourly_wage, na.rm = TRUE)) / 
           sd(hourly_wage, na.rm = TRUE)) %>%
  filter(abs(z_score_wage) <= 3)

# =========================================================================
# 4. Guardado de datos procesados
# =========================================================================
message("Guardando datos procesados...")
if(!dir.exists("stores/processed")) dir.create("stores/processed", recursive = TRUE)

saveRDS(datos_procesados, "stores/processed/geih_2018_clean.rds")
write.csv(datos_procesados, "stores/processed/geih_2018_clean.csv", row.names = FALSE)

# =========================================================================
# 5. Generar datasets específicos para análisis
# =========================================================================
message("Creando datasets para análisis...")

# Convertir explícitamente a tibble antes de seleccionar columnas
datos_procesados <- as_tibble(datos_procesados)
columnas_disponibles <- names(datos_procesados)

if(all(c("age", "age_squared", "age_cubed", "log_hourly_wage") %in% columnas_disponibles)) {
  age_wage_data <- datos_procesados %>% dplyr::select(age, age_squared, age_cubed, log_hourly_wage)
  saveRDS(age_wage_data, "stores/processed/age_wage_data.rds")
} else {
  message("Advertencia: Variables necesarias para age_wage_data no están presentes. Verifique la transformación de datos.")
}

if(all(c("log_hourly_wage", "age", "female", "maxEducLevel", "totalHoursWorked") %in% columnas_disponibles)) {
  gender_gap_data <- datos_procesados %>% dplyr::select(log_hourly_wage, age, female, maxEducLevel, totalHoursWorked)
  saveRDS(gender_gap_data, "stores/processed/gender_gap_data.rds")
} else {
  message("Advertencia: Variables necesarias para gender_gap_data no están presentes. Verifique la transformación de datos.")
}

# =========================================================================
# 6. Generación de estadísticas descriptivas
# =========================================================================
message("Generando estadísticas descriptivas...")

datos_desc <- as_tibble(datos_procesados) %>%
  dplyr::select(any_of(c("hourly_wage", "log_hourly_wage", "age", "female"))) %>%
  drop_na()

if(nrow(datos_desc) > 0) {
  stargazer(
    datos_desc,
    type = "text",
    title = "Estadísticas Descriptivas",
    digits = 2,
    summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max")
  )
} else {
  message("Advertencia: No hay datos suficientes para generar estadísticas descriptivas.")
}

message("Limpieza y procesamiento completados exitosamente.")
