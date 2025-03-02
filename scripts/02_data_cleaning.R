# =========================================================================
# 02_data_cleaning.R
# 
# Este script realiza la limpieza y preparación de los datos de la GEIH 2018
# obtenidos mediante webscraping. Se enfoca en individuos empleados mayores de 18 años
# y en la creación de variables clave para el análisis de ingresos.
#
# =========================================================================

# Establecer el directorio de trabajo automáticamente en base a la ubicación del script
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# Cargar librerías necesarias
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse, janitor, naniar, skimr, lubridate, scales, ggthemes, patchwork, kableExtra, mice, corrplot
)

# Configuración inicial
raw_data_dir <- file.path(getwd(), "stores/raw")
processed_data_dir <- file.path(getwd(), "stores/processed")
figures_dir <- file.path(getwd(), "views/figures")
tables_dir <- file.path(getwd(), "views/tables")

# Crear directorios si no existen
for (dir in c(processed_data_dir, figures_dir, tables_dir)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message(glue::glue("Directorio '{dir}' creado."))
  }
}

# Cargar datos crudos
message("Cargando datos crudos...")
geih_raw <- readRDS(file.path(raw_data_dir, "geih_2018_raw.rds"))
message(glue::glue("Dimensiones de los datos crudos: {nrow(geih_raw)} filas y {ncol(geih_raw)} columnas"))

# Limpieza de nombres de variables
geih_clean <- geih_raw %>%
  janitor::clean_names() %>%
  rename_with(~ str_replace_all(.x, "p_", ""), starts_with("p_"))

# Verificar nombres de columnas
print(names(geih_clean))

# Ajustar nombres si 'edad' no está presente correctamente
edad_col <- "age"
message(glue::glue("Columna de edad identificada: {edad_col}"))

# Filtrar individuos empleados mayores de 18 años
geih_filtered <- geih_clean %>%
  filter(!!sym(edad_col) > 18, ocupados == 1)

# Visualización de valores faltantes
vis_miss(geih_filtered)

# Matriz de correlación de valores faltantes
db2 <- geih_filtered %>% mutate_all(~ifelse(!is.na(.), 1, 0))
db2 <- db2 %>% select(which(apply(db2, 2, sd) > 0))
M <- cor(db2)
corrplot(M, method = "circle", type = "lower", tl.cex = 0.7)

# Imputación de valores faltantes usando MICE
geih_imputed <- mice(geih_filtered, m = 5, method = "pmm", maxit = 5, seed = 123)
geih_filtered <- complete(geih_imputed)

# Transformaciones de variables
geih_transformed <- geih_filtered %>%
  mutate(
    hourly_wage = inglabo / (horstot * 4.345),
    log_hourly_wage = log(hourly_wage),
    sex = factor(sexo, labels = c("Male", "Female")),
    age_squared = !!sym(edad_col)^2,
    education_level = factor(case_when(
      nivel_educativo <= 3 ~ "Primary or less",
      nivel_educativo == 4 ~ "Secondary",
      nivel_educativo == 5 ~ "High school",
      nivel_educativo == 6 ~ "Technical/Technological",
      nivel_educativo > 6 ~ "University or higher",
      TRUE ~ NA_character_
    )),
    experience = pmax(0, !!sym(edad_col) - nivel_educativo - 6),
    experience_squared = experience^2,
    formal = if_else(cotpen == 1, 1, 0),
    married = if_else(est_civil %in% c(2, 3), 1, 0)
  )

# Manejo de outliers en salarios usando Z-score
geih_transformed <- geih_transformed %>%
  mutate(z_score_wage = (hourly_wage - mean(hourly_wage, na.rm = TRUE)) / sd(hourly_wage, na.rm = TRUE))

gheih_final <- geih_transformed %>%
  filter(abs(z_score_wage) < 3)

# Guardar datos procesados
saveRDS(geih_final, file.path(processed_data_dir, "geih_2018_clean.rds"))
write_csv(geih_final, file.path(processed_data_dir, "geih_2018_clean.csv"))

message(glue::glue("Datos procesados guardados en {processed_data_dir}"))

# FIN DEL SCRIPT
