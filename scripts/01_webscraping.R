# =========================================================================
# 01_webscraping.R
# 
# Este script extrae los datos de la Gran Encuesta Integrada de Hogares (GEIH) 2018
# para Bogotá desde el sitio web: https://ignaciomsarmiento.github.io/GEIH2018_sample/
# 
# Los datos están divididos en 10 archivos HTML que se descargan y combinan
# en un único dataset.
#
# =========================================================================

# Establecer el directorio de trabajo automáticamente en base a la ubicación del script
if (!require(rstudioapi)) install.packages("rstudioapi")  # Instalar si es necesario
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# Cargar librerías necesarias
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  httr,       # Para solicitudes HTTP
  rvest,      # Para web scraping
  readr,      # Para lectura/escritura de archivos
  dplyr,      # Para manipulación de datos
  glue,       # Para interpolación de cadenas
  purrr       # Para programación funcional
)

# Configuración inicial
# ---------------------
raw_data_dir <- file.path(getwd(), "stores/raw")

# Verificar si el directorio de datos existe y crearlo si es necesario
if (!dir.exists(raw_data_dir)) {
  dir.create(raw_data_dir, recursive = TRUE)
  message(glue("Directorio '{raw_data_dir}' creado."))
}

# Definir URLs de los archivos con los datos
base_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
urls <- paste0(base_url, 1:10, ".html")

# Función para descargar y procesar los datos de cada URL
download_data <- function(url) {
  message(glue("Descargando datos de {url}"))
  
  # Realizar la solicitud HTTP
  response <- GET(url)
  
  # Verificar si la solicitud fue exitosa
  if (http_status(response)$category == "Success") {
    # Obtener el contenido HTML
    html_content <- content(response, "text", encoding = "UTF-8")
    
    # Parsear el HTML y extraer tablas
    html_doc <- read_html(html_content)
    tables <- html_table(html_doc, fill = TRUE)
    
    # Si hay tablas, usar la más grande
    if (length(tables) > 0) {
      # Encontrar la tabla más grande (por número de filas)
      table_sizes <- sapply(tables, nrow)
      largest_table_index <- which.max(table_sizes)
      
      data <- tables[[largest_table_index]]
      message(glue("Tabla extraída con {nrow(data)} filas y {ncol(data)} columnas"))
      return(data)
    } else {
      message("No se encontraron tablas en el HTML")
      
      # Guardar el HTML para inspección manual
      html_file <- file.path(raw_data_dir, paste0("chunk_", basename(url)))
      writeLines(html_content, html_file)
      message(glue("HTML guardado en {html_file} para inspección manual"))
      return(NULL)
    }
  } else {
    message(glue("Error: {http_status(response)$message}"))
    return(NULL)
  }
}

# Descargar los datos de cada URL
message("Iniciando descarga de datos...")
all_data <- list()

for (i in seq_along(urls)) {
  url <- urls[i]
  tryCatch({
    data_chunk <- download_data(url)
    if (!is.null(data_chunk)) {
      all_data[[i]] <- data_chunk
    }
  }, error = function(e) {
    message(glue("Error al procesar {url}: {e$message}"))
  })
  
  # Pequeña pausa para no sobrecargar el servidor
  Sys.sleep(1)
}

# Extraer resultados válidos
valid_data <- all_data[!sapply(all_data, is.null)]

# Verificar si se obtuvieron datos
if (length(valid_data) > 0) {
  # Intentar combinar todos los chunks
  tryCatch({
    geih_data <- bind_rows(valid_data)
    
    # Guardar los datos combinados
    saveRDS(geih_data, file.path(raw_data_dir, "geih_2018_raw.rds"))
    write_csv(geih_data, file.path(raw_data_dir, "geih_2018_raw.csv"))
    
    message(glue("Datos combinados y guardados exitosamente en '{raw_data_dir}'"))
    message(glue("Total de observaciones: {nrow(geih_data)}"))
    message(glue("Total de variables: {ncol(geih_data)}"))
    
    # Mostrar muestra de los datos
    message("Muestra de los primeros datos:")
    print(head(geih_data[, 1:min(5, ncol(geih_data))]))
  }, error = function(e) {
    message(glue("Error al combinar los datos: {e$message}"))
    
    # Guardar los chunks individuales en caso de error
    for (i in seq_along(valid_data)) {
      chunk_file <- file.path(raw_data_dir, paste0("geih_2018_chunk_", i, ".rds"))
      saveRDS(valid_data[[i]], chunk_file)
      message(glue("Chunk {i} guardado en {chunk_file}"))
    }
    
    message("Se han guardado los chunks individuales en el directorio de datos.")
  })
} else {
  message("No se pudieron obtener datos de ninguna URL.")
  message("Revise manualmente el contenido de las URLs para determinar la estructura correcta.")
}

message("Proceso de webscraping finalizado.")
