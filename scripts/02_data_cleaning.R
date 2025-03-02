# =========================================================================
# 02_data_cleaning.R
# 
# Este script realiza la limpieza y preparación de los datos de la GEIH 2018
# para el análisis de ingresos.
# =========================================================================

# Cargar paquetes necesarios
if(!require(pacman)) install.packages("pacman")
pacman::p_load(rio,      # importar/exportar datos
               tidyverse, # manipulación de datos
               skimr,     # resumen de datos
               corrplot,  # gráficos de correlación
               stargazer, # tablas de resultados
               visdat,    # visualización de valores faltantes
               ggplot2    # generación de gráficos
)

# =========================================================================
# 1. Carga de datos
# =========================================================================
# Cargar los datos obtenidos por web scraping
message("Cargando datos...")
geih_raw <- import("stores/raw/geih_2018_raw.rds")
geih_raw <- as_tibble(geih_raw) # convertir a tibble

# Dimensiones del conjunto de datos
message(paste("Dimensiones originales:", dim(geih_raw)[1], "filas y", 
              dim(geih_raw)[2], "columnas"))

# =========================================================================
# 1.1 Exploración de valores faltantes
# =========================================================================
message("Resumen de valores faltantes:")
skimr::skim(geih_raw) # Resumen de valores faltantes

message("Visualización de valores faltantes:")
missing_values_plot <- visdat::vis_miss(geih_raw %>% slice_sample(n = 5000))
ggsave("views/figures/missing_values.png", plot = missing_values_plot, width = 8, height = 6)

# =========================================================================
# 1.2 Correlación entre valores faltantes
# =========================================================================
message("Visualización de correlación entre valores faltantes:")
na_matrix <- geih_raw %>% is.na() %>% cor(use = "pairwise.complete.obs")
png("views/figures/missing_corr.png", width = 800, height = 600)
corrplot(na_matrix, method = "color", type = "upper", tl.cex = 0.7, na.label = " ")
dev.off()

# =========================================================================
# 2. Filtrar individuos empleados mayores de 18 años
# =========================================================================
geih_clean <- geih_raw %>%
  filter(age > 18, ocu == 1)

message(paste("Después del filtrado por edad y ocupación:", 
              dim(geih_clean)[1], "filas"))

# =========================================================================
# 3. Creación de variables para el análisis
# =========================================================================
# Crear variables clave para el análisis
geih_processed <- geih_clean %>%
  mutate(
    # Variable dependiente: salario por hora
    hourly_wage = y_ingLab_m / (totalHoursWorked * 4.345),
    
    # Variable para brecha de género
    female = ifelse(sex == 0, 1, 0),
    
    # Variables para análisis de perfil edad-salario
    age_squared = age^2,
    
    # Variable para categoría educativa
    educ_level = factor(maxEducLevel),
    
    # Variable para formalidad laboral
    formal_work = formal,
    
    # Log de salario
    log_hourly_wage = log(hourly_wage)
  ) %>%
  # Filtrar solo valores positivos de salario y horas trabajadas
  filter(!is.na(hourly_wage), hourly_wage > 0, totalHoursWorked > 0)

message(paste("Después de la creación de variables:", 
              dim(geih_processed)[1], "filas"))

# =========================================================================
# 4. Detección y manejo de outliers en salarios
# =========================================================================
# Calcular Z-score para detectar outliers
geih_processed <- geih_processed %>%
  mutate(
    # Z-score para salario por hora
    z_score_wage = (hourly_wage - mean(hourly_wage, na.rm = TRUE)) / 
      sd(hourly_wage, na.rm = TRUE),
    
    # Identificar outliers (|z| > 3)
    is_outlier_wage = abs(z_score_wage) > 3
  )

# Número de outliers identificados
n_outliers <- sum(geih_processed$is_outlier_wage, na.rm = TRUE)
message(paste("Número de outliers identificados en salarios:", n_outliers))

# Dataset sin outliers
geih_filtered <- geih_processed %>%
  filter(!is_outlier_wage)

message(paste("Después de filtrar outliers:", dim(geih_filtered)[1], "filas"))

# =========================================================================
# 5. Estadísticas descriptivas
# =========================================================================
# Verificar y limpiar valores NA antes de generar la tabla de estadísticas descriptivas
geih_filtered_clean <- geih_filtered %>% 
  select(hourly_wage, log_hourly_wage, age, female) %>%
  drop_na()

if (nrow(geih_filtered_clean) > 0) {
  stargazer(
    geih_filtered_clean,
    type = "latex",
    title = "Estadísticas Descriptivas",
    digits = 2,
    out = "views/tables/descriptive_stats.tex",
    label = "tab:descriptives",
    summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max")
  )
} else {
  message("Advertencia: No hay datos suficientes para generar estadísticas descriptivas.")
}

# =========================================================================
# 6. Guardar datos procesados
# =========================================================================
# Guardar dataset procesado para análisis posteriores
saveRDS(geih_filtered, "stores/processed/geih_2018_clean.rds")

message("Limpieza de datos completada exitosamente.")
