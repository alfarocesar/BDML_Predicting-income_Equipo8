# Datos del Proyecto GEIH 2018

## Fuente de Datos
Los datos provienen de la Gran Encuesta Integrada de Hogares (GEIH) 2018 para Bogotá, disponible en:
https://ignaciomsarmiento.github.io/GEIH2018_sample/

## Estructura del Directorio
stores/
├── raw/          # Datos crudos descargados mediante web scraping
├── processed/    # Datos procesados y listos para análisis
└── README.md     # Este archivo

## Variables Principales
Los datos incluyen información sobre:
- Características demográficas
- Información laboral
- Ingresos
- Educación
- Otras variables socioeconómicas

La descripción detallada de las variables se documentará una vez completado el web scraping.

## Procesamiento de Datos
El procesamiento se realiza en dos etapas:
1. Web scraping (scripts/01_webscraping.R)
2. Limpieza y preparación (scripts/02_data_cleaning.R)

## Notas
- Los archivos de datos crudos no se incluyen en el repositorio debido a su tamaño
- Los scripts de R incluyen el código para obtener y procesar los datos
- Documentación detallada del proceso de limpieza se incluirá en el documento final