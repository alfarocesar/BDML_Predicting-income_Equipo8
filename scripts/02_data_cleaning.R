# =========================================================================
# 02_data_cleaning.R
#
# Este script realiza la limpieza y preparación de los datos de la GEIH 2018
# obtenidos mediante webscraping.
#
# Preparamos los datos para el análisis de ingresos, enfocándonos en individuos 
# empleados mayores de 18 años, y creando las variables necesarias para los 
# modelos de predicción de ingresos.
#
# =========================================================================

# Establecer el directorio de trabajo automáticamente en base a la ubicación del script
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# Cargar librerías necesarias
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Para manipulación de datos
  janitor,      # Para limpieza de nombres de variables
  naniar,       # Para manejo de datos faltantes
  skimr,        # Para estadísticas descriptivas
  lubridate,    # Para manejo de fechas
  scales,       # Para formateo de escalas en gráficos
  ggthemes,     # Temas adicionales para ggplot
  patchwork,    # Para combinar múltiples gráficos
  kableExtra    # Para formateo de tablas
)

# Establecer semilla para reproducibilidad
set.seed(20250301)

# Configuración inicial
# ---------------------
raw_data_dir <- file.path(getwd(), "stores/raw")
processed_data_dir <- file.path(getwd(), "stores/processed")
figures_dir <- file.path(getwd(), "views/figures")
tables_dir <- file.path(getwd(), "views/tables")

# Verificar si los directorios existen y crearlos si es necesario
for (dir in c(processed_data_dir, figures_dir, tables_dir)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message(glue::glue("Directorio '{dir}' creado."))
  }
}

# 1. Cargar datos -----------------------------------------------------------

# Cargar los datos crudos (utilizando RDS por eficiencia)
message("Cargando datos crudos...")
geih_raw <- readRDS(file.path(raw_data_dir, "geih_2018_raw.rds"))

# Inspección inicial de los datos
message(glue::glue("Dimensiones de los datos crudos: {nrow(geih_raw)} filas y {ncol(geih_raw)} columnas"))
message("Primeras columnas: ", paste(head(names(geih_raw), 10), collapse = ", "), "...")

# 2. Limpieza básica --------------------------------------------------------

# Limpiar nombres de variables para consistencia
message("Limpiando nombres de variables...")
geih_clean <- geih_raw %>%
  janitor::clean_names() %>%
  rename_with(~ str_replace_all(.x, "p_", ""), starts_with("p_"))

# 3. Filtrado según requisitos del proyecto ---------------------------------

# Filtrar solo individuos empleados mayores de 18 años
# Según instrucciones: "focus only on employed individuals older than eighteen (18) years old"
message("Filtrando por edad > 18 y estado de ocupación...")
geih_filtered <- geih_clean %>%
  filter(
    # Filtro por edad > 18
    edad > 18,
    # Filtro por estado de ocupación (empleados)
    ocupados == 1
  )

message(glue::glue("Dimensiones después de filtrar por edad > 18 y empleados: {nrow(geih_filtered)} filas y {ncol(geih_filtered)} columnas"))

# 4. Manejo de datos faltantes y outliers -----------------------------------

# Identificar columnas con valores faltantes relevantes para nuestro análisis
message("Analizando valores faltantes...")
missing_cols <- geih_filtered %>%
  select(edad, sexo, nivel_educativo, inglabo, horstot) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

# Mostrar resumen de datos faltantes
if (nrow(missing_cols) > 0) {
  message("Columnas clave con valores faltantes:")
  print(missing_cols)
} else {
  message("No hay valores faltantes en las columnas clave seleccionadas.")
}

# Eliminar observaciones con salarios faltantes o cero
# "Keep in mind that in the data, there are many observations with missing data or 0 wages"
message("Filtrando observaciones con salarios válidos...")
geih_wages <- geih_filtered %>%
  filter(!is.na(inglabo), inglabo > 0)

message(glue::glue("Dimensiones después de filtrar salarios faltantes o cero: {nrow(geih_wages)} filas y {ncol(geih_wages)} columnas"))

# 5. Transformación de variables --------------------------------------------

message("Transformando variables...")
geih_transformed <- geih_wages %>%
  mutate(
    # Crear variable de salario por hora (w) - variable principal según instrucciones
    hourly_wage = inglabo / (horstot * 4.345), # Asumiendo horas mensuales = horas semanales * 4.345
    
    # Logaritmo del salario por hora (para modelos)
    log_hourly_wage = log(hourly_wage),
    
    # Transformar variables categóricas en factores
    sex = factor(sexo, labels = c("Male", "Female")),
    female = as.integer(sexo == 1), # 1 para mujer según documentación
    
    # Crear variables para modelos de edad
    age_squared = edad^2,
    
    # Nivel educativo como factor ordenado
    education_level = case_when(
      nivel_educativo <= 3 ~ "Primary or less",
      nivel_educativo == 4 ~ "Secondary",
      nivel_educativo == 5 ~ "High school",
      nivel_educativo == 6 ~ "Technical/Technological",
      nivel_educativo > 6 ~ "University or higher",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Primary or less", "Secondary", "High school", 
                            "Technical/Technological", "University or higher")),
    
    # Sector económico simplificado (basado en rama2d_r4)
    sector = case_when(
      rama2d_r4 %in% 1:3 ~ "Primary",
      rama2d_r4 %in% 4:9 ~ "Secondary",
      rama2d_r4 %in% 10:21 ~ "Tertiary",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    # Experiencia potencial (edad - años de educación - 6)
    years_education = case_when(
      nivel_educativo == 1 ~ 0,
      nivel_educativo == 2 ~ 3,
      nivel_educativo == 3 ~ 5,
      nivel_educativo == 4 ~ 9,
      nivel_educativo == 5 ~ 11,
      nivel_educativo == 6 ~ 13,
      nivel_educativo == 7 ~ 16,
      nivel_educativo == 8 ~ 18,
      nivel_educativo == 9 ~ 21,
      TRUE ~ NA_real_
    ),
    experience = pmax(0, edad - years_education - 6),
    experience_squared = experience^2,
    
    # Tipo de empleo (formal vs informal)
    formal = if_else(cotpen == 1, 1, 0),
    
    # Estado civil
    married = if_else(est_civil %in% c(2, 3), 1, 0) # 2=casado, 3=unión libre
  )

# 6. Manejo de outliers en salarios -----------------------------------------

# Identificar outliers en salarios usando método IQR
message("Identificando y tratando outliers en salarios...")
q1_wage <- quantile(geih_transformed$hourly_wage, 0.25, na.rm = TRUE)
q3_wage <- quantile(geih_transformed$hourly_wage, 0.75, na.rm = TRUE)
iqr_wage <- q3_wage - q1_wage
lower_bound <- q1_wage - 3 * iqr_wage  # Uso 3*IQR para ser menos restrictivo
upper_bound <- q3_wage + 3 * iqr_wage

# Recortar valores extremos de salarios
geih_final <- geih_transformed %>%
  filter(
    hourly_wage >= lower_bound,
    hourly_wage <= upper_bound
  )

message(glue::glue("Dimensiones después de recortar outliers de salarios: {nrow(geih_final)} filas y {ncol(geih_final)} columnas"))
message(glue::glue("Se eliminaron {nrow(geih_transformed) - nrow(geih_final)} observaciones por salarios extremos"))

# 7. Estadísticas descriptivas ----------------------------------------------

message("Generando estadísticas descriptivas...")
# Variables numéricas clave
numeric_vars <- c("edad", "horstot", "hourly_wage", "log_hourly_wage", "inglabo", "experience")

# Generar estadísticas descriptivas para variables numéricas
wage_stats <- geih_final %>%
  select(all_of(numeric_vars)) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  )

message("Estadísticas descriptivas de variables numéricas clave:")
print(wage_stats)

# Estadísticas por género
gender_stats <- geih_final %>%
  group_by(sex) %>%
  summarise(
    mean_hourly_wage = mean(hourly_wage),
    median_hourly_wage = median(hourly_wage),
    count = n(),
    proportion = n() / nrow(geih_final)
  )

message("Estadísticas por género:")
print(gender_stats)

# 8. Guardar datos procesados ----------------------------------------------

message("Guardando datos procesados...")
# Guardar datos procesados
saveRDS(geih_final, file.path(processed_data_dir, "geih_2018_clean.rds"))
write_csv(geih_final, file.path(processed_data_dir, "geih_2018_clean.csv"))

message(glue::glue("Datos procesados guardados en {processed_data_dir}"))

# 9. Generar visualizaciones exploratorias ---------------------------------

message("Generando visualizaciones exploratorias...")

# Histograma de salarios por hora
p1 <- ggplot(geih_final, aes(x = hourly_wage)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribución de salarios por hora",
       x = "Salario por hora (COP)",
       y = "Frecuencia") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)

# Transformación logarítmica
p2 <- ggplot(geih_final, aes(x = log_hourly_wage)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribución del logaritmo de salarios por hora",
       x = "Log(Salario por hora)",
       y = "Frecuencia") +
  theme_minimal()

# Distribución de salarios por género
p3 <- ggplot(geih_final, aes(x = hourly_wage, fill = sex)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de salarios por género",
       x = "Salario por hora (COP)",
       y = "Densidad",
       fill = "Género") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(labels = scales::comma, limits = c(0, quantile(geih_final$hourly_wage, 0.99)))

# Salarios por nivel educativo
p4 <- ggplot(geih_final, aes(x = education_level, y = hourly_wage)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Salarios por nivel educativo",
       x = "Nivel educativo",
       y = "Salario por hora (COP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, quantile(geih_final$hourly_wage, 0.99)))

# Relación edad-salario
p5 <- ggplot(geih_final, aes(x = edad, y = hourly_wage)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  labs(title = "Relación edad-salario",
       x = "Edad",
       y = "Salario por hora (COP)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, limits = c(0, quantile(geih_final$hourly_wage, 0.99)))

# Relación edad-salario por género
p6 <- ggplot(geih_final, aes(x = edad, y = hourly_wage, color = sex)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Relación edad-salario por género",
       x = "Edad",
       y = "Salario por hora (COP)",
       color = "Género") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::comma, limits = c(0, quantile(geih_final$hourly_wage, 0.99)))

# Combinar gráficos
exploratory_plots1 <- (p1 + p2) / (p3 + p4)
exploratory_plots2 <- (p5 + p6)

# Guardar los gráficos combinados
ggsave(file.path(figures_dir, "exploratory_analysis1.png"), 
       exploratory_plots1, 
       width = 12, 
       height = 10)

ggsave(file.path(figures_dir, "exploratory_analysis2.png"), 
       exploratory_plots2, 
       width = 12, 
       height = 6)

message(glue::glue("Gráficos exploratorios guardados en {figures_dir}"))

# 10. Exportar tablas de resumen -------------------------------------------

message("Exportando tablas de resumen...")

# Estadísticas descriptivas de variables numéricas
numeric_summary <- geih_final %>%
  select(edad, horstot, hourly_wage, log_hourly_wage, inglabo, experience) %>%
  summary() %>%
  as.data.frame() %>%
  rownames_to_column(var = "statistic")

# Frecuencias de variables categóricas
categorical_summary <- geih_final %>%
  select(sex, formal, education_level, sector, married) %>%
  summarise(across(everything(), 
                   list(
                     count = ~n_distinct(., na.rm = TRUE),
                     na = ~sum(is.na(.))
                   )))

# Variables categóricas - proporción
cat_props <- geih_final %>%
  select(sex, formal, education_level, sector, married) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "category") %>%
  group_by(variable, category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = count / sum(count),
         percentage = proportion * 100)

# Exportar tablas en formato CSV
write_csv(wage_stats, file.path(tables_dir, "numeric_variables_summary.csv"))
write_csv(cat_props, file.path(tables_dir, "categorical_variables_summary.csv"))
write_csv(gender_stats, file.path(tables_dir, "gender_wage_summary.csv"))

# Crear tablas más formateadas para el informe usando kableExtra
# Estas tablas se pueden importar directamente en el documento LaTeX

# Tabla de estadísticas numéricas
numeric_table <- wage_stats %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  kable("latex", 
        caption = "Estadísticas descriptivas de variables numéricas",
        booktabs = TRUE,
        label = "tab:numeric_summary") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Tabla de diferencias por género
gender_table <- gender_stats %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  kable("latex", 
        caption = "Diferencias salariales por género",
        booktabs = TRUE,
        label = "tab:gender_summary") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Guardar tablas en formato .tex para importar en LaTeX
writeLines(numeric_table, file.path(tables_dir, "numeric_summary.tex"))
writeLines(gender_table, file.path(tables_dir, "gender_summary.tex"))

message(glue::glue("Tablas de resumen guardadas en {tables_dir}"))

# Mensaje final
message("\nProceso de limpieza y procesamiento completado con éxito.")
message(glue::glue("Datos finales: {nrow(geih_final)} observaciones y {ncol(geih_final)} variables."))

# FIN DEL SCRIPT ------------------------------------------------------------