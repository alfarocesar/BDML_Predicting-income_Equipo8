#---
# title: "Punto 5"
# author: "Jose_D_Cuervo
# date: "2025-03-04"
# output: html_document
# ---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### PUNTO DESARROLAR

```{r}
# Fijar semilla para reproducibilidad
set.seed(10101)

# Dividir datos en entrenamiento (70%) y prueba (30%)
trainIndex <- createDataPartition(geih_filtered_clean$log_hourly_wage, p = 0.7, list = FALSE)
train_data <- geih_filtered_clean[trainIndex, ]
test_data <- geih_filtered_clean[-trainIndex, ]

# Modelos a estimar
models <- list()

# Modelo 1: Regresión lineal básica
models$lm1 <- lm(log_hourly_wage ~ age + I(age^2), data = train_data)

# Modelo 2: Regresión con más predictores
models$lm2 <- lm(log_hourly_wage ~ age + I(age^2) + female + totalHoursWorked + formal_work + educ_level, data = train_data)

# Modelo 3: Regresión con interacciones
models$lm3 <- lm(log_hourly_wage ~ age * educ_level + female * formal_work, data = train_data)

# Modelo 4: Random Forest
models$rf <- randomForest(log_hourly_wage ~ age + I(age^2) + female + totalHoursWorked + formal_work + educ_level, data = train_data)

# Modelo 5: Red Neuronal (nnet)
models$nn <- nnet(log_hourly_wage ~ age + I(age^2) + female + totalHoursWorked + formal_work + educ_level, data = train_data, size = 5, linout = TRUE)

# Evaluar modelos en conjunto de prueba
rmse_results <- sapply(models, function(model) {
  preds <- predict(model, newdata = test_data)
  RMSE(preds, test_data$log_hourly_wage)
})

# Seleccionar el mejor modelo (menor RMSE)
best_model <- names(which.min(rmse_results))

# Análisis de errores de predicción
best_preds <- predict(models[[best_model]], newdata = test_data)
pred_errors <- test_data$log_hourly_wage - best_preds

# Análisis de observaciones con mayores errores
high_error_obs <- test_data[order(abs(pred_errors), decreasing = TRUE), ][1:10, ]

# Validación cruzada LOOCV para los dos mejores modelos
loocv_rmse <- function(model_formula, data) {
  cv_results <- cv.glm(data, glm(model_formula, data = data), K = nrow(data))
  return(sqrt(cv_results$delta[1]))  # RMSE
}

# Aplicar LOOCV a los dos mejores modelos
loocv_results <- sapply(names(sort(rmse_results)[1:2]), function(model_name) {
  loocv_rmse(formula(models[[model_name]]), train_data)
})

# Comparar LOOCV con error en conjunto de prueba
comparison <- data.frame(Model = names(sort(rmse_results)[1:2]), 
                         RMSE_Test = rmse_results[names(sort(rmse_results)[1:2])],
                         RMSE_LOOCV = loocv_results)

# Mostrar resultados
print(rmse_results)
print(best_model)
print(comparison)
print(high_error_obs)

```

