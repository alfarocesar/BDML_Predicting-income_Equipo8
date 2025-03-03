#Paquetes

require(pacman)

p_load(tidyverse, # contiene las librerías ggplot, dplyr...
       rvest,# web-scraping
       httr, #Permite simular la llamada 
       jsonlite# Para manejar formato JSON
) 

url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

links <- read_html(url) %>% html_nodes(xpath="/html/body/div/div/div[2]/ul") %>%
  html_nodes("a") %>%
  html_attr("href")

links_pags <- paste0(url, links)

l1  <- data.frame(links_pags)

url_pag <- c()

for (i in 1:10){
  urli <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i , ".html")
  url_pag <- c(url_pag,urli)
}

GEIH <- c()

for (i in 1:10){
  gyh <- url_pag[i] %>%
    read_html() %>% 
    html_table()
  
  GEIH[[i]] <- gyh[[1]]  
}

# Concatenar todas las tablas en un solo dataframe
GEIH_df <- bind_rows(GEIH)



dff <- data.frame(GEIH)
write.csv(GEIH_df , "datos_taller1.csv", row.names = FALSE, sep = ";")


###############################################################################  
#-------------------------------punto 4---------------------------------------#

####################### PASO # 1 FILTAR LOS DATOS ##############################

# Instalar y cargar paquetes necesarios
install.packages("tidyverse")
library(tidyverse)

# Filtrar datos: personas empleadas mayores de 18 años

##filtrar sin criterios de NA
GEIH_df_filtro2 <- GEIH_df %>%
  filter(age > 18, y_ingLab_m > 0, !is.na(y_ingLab_m))

## FILTRO CONCRITERIOS DE NA
GEIH_df_filtro <- GEIH_df %>%
  filter(age > 18, y_ingLab_m > 0) %>%
  drop_na(age, sex,fex_c, p6100, p6426, relab)  

###################### PASO 2  crear variables log #############################

# Crear variable logarítmica del salario
GEIH_df_filtro <- GEIH_df_filtro %>%
  mutate(log_salario = log(y_ingLab_m))

# Verificar cambios
summary(GEIH_df_filtro$log_salario)
summary(GEIH_df_filtro$y_ingLab_m)

################### PASO 3  70% entrenamiento 30% prueba #######################

library(caret)

# Fijar semilla para reproducibilidad
set.seed(10101)

# Crear índices para dividir la muestra
trainIndex <- createDataPartition(GEIH_df_filtro$log_salario, p = 0.7, list = FALSE)

# Dividir datos
train_data <- GEIH_df_filtro[trainIndex, ]
test_data <- GEIH_df_filtro[-trainIndex, ]

# Verificar tamaños
dim(train_data)
dim(test_data)

############################### PASO 4 Modelar  ################################

# Ajustar un modelo de regresión lineal múltiple
modelo_lm <- lm(log_salario ~ age + I(age^2) + sex + p6100 + fex_c + p6426 + relab, data = train_data)

# Resumen del modelo
summary(modelo_lm)

# Predecir en el conjunto de prueba
predicciones <- predict(modelo_lm, newdata = test_data)

# Calcular RMSE
rmse_lm <- sqrt(mean((test_data$log_salario - predicciones)^2))
print(paste("RMSE del modelo lineal:", rmse_lm))

########################## PASO 5 Regresión Ridge y Lasso  #####################

install.packages("glmnet")
library(glmnet)

# Preparar datos para Ridge y Lasso

# Seleccionar variables predictoras
X_train <- model.matrix(log_salario ~ age + I(age^2) + sex + p6100 + fex_c + p6426 + relab, data = train_data)[, -1]
X_test <- model.matrix(log_salario ~ age + I(age^2) + sex + p6100 + fex_c + p6426 + relab, data = test_data)[, -1]

# Variable dependiente
y_train <- train_data$log_salario
y_test <- test_data$log_salario


### Regresión Ridge

# Ajustar modelo Ridge (alpha = 0)
ridge_model <- cv.glmnet(X_train, y_train, alpha = 0)

# Predecir en el conjunto de prueba
ridge_pred <- predict(ridge_model, s = ridge_model$lambda.min, newx = X_test)

# Calcular RMSE
rmse_ridge <- sqrt(mean((y_test - ridge_pred)^2))
print(paste("RMSE Ridge:", rmse_ridge))

### Regresión Lasso

# Ajustar modelo Lasso (alpha = 1)
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)

# Predecir en el conjunto de prueba
lasso_pred <- predict(lasso_model, s = lasso_model$lambda.min, newx = X_test)

# Calcular RMSE
rmse_lasso <- sqrt(mean((y_test - lasso_pred)^2))
print(paste("RMSE Lasso:", rmse_lasso))


################### PASO 6 Árboles de Decisión y Random Forest  ################

# librerias
install.packages("rpart")
install.packages("randomForest")
library(rpart)
library(randomForest)

### Árbol de Decisión

# Ajustar modelo de árbol de decisión
tree_model <- rpart(log_salario ~ age + sex + p6100 + fex_c + p6426 + relab, data = train_data, method = "anova")

# Predecir en el conjunto de prueba
tree_pred <- predict(tree_model, newdata = test_data)

# Calcular RMSE
rmse_tree <- sqrt(mean((y_test - tree_pred)^2))
print(paste("RMSE Árbol de Decisión:", rmse_tree))

### Random Forest

# Ajustar modelo Random Forest
rf_model <- randomForest(log_salario ~ age + sex + p6100 + fex_c + p6426 + relab, data = train_data, ntree = 500)

# Predecir en el conjunto de prueba
rf_pred <- predict(rf_model, newdata = test_data)

# Calcular RMSE
rmse_rf <- sqrt(mean((y_test - rf_pred)^2))
print(paste("RMSE Random Forest:", rmse_rf))


############################## PASO 7 Resultados  ###############################
# Comparar resultados

rmse_results <- data.frame(
  Modelo = c("Regresión Lineal", "Ridge", "Lasso", "Árbol de Decisión", "Random Forest"),
  RMSE = c(rmse_lm, rmse_ridge, rmse_lasso, rmse_tree, rmse_rf)
)
print(rmse_results)


############################## PASO 8 Bootstrap  ###############################

library(boot)
# Definir la función para el bootstrap
boot_fn <- function(data, index) {
  # Crear un subconjunto de datos con el índice proporcionado por el bootstrap
  sample_data <- data[index, ]
  
  # Ajustar el modelo de regresión con las variables especificadas
  model <- lm(log_salario ~ age + I(age^2) + sex + p6100 + fex_c + p6426 + relab, data = sample_data)
  
  # Devolver los coeficientes del modelo
  return(coef(model))
}

# Realizar el bootstrap
boot_results <- boot(GEIH_df_filtro, boot_fn, R = 5000)  # R = número de réplicas bootstrap

# Mostrar los resultados del bootstrap
print(boot_results)

# Crear una matriz para almacenar los intervalos de confianza
conf_intervals <- matrix(NA, nrow = length(boot_results$t0), ncol = 2)

# Calcular los intervalos de confianza para cada coeficiente
for (i in 1:length(boot_results$t0)) {
  ci <- boot.ci(boot_results, type = "perc", index = i)
  if (!is.null(ci)) {
    conf_intervals[i, ] <- ci$percent[4:5]  # Almacenar los límites inferior y superior
  }
}

# Asignar nombres de filas a los intervalos de confianza
rownames(conf_intervals) <- names(boot_results$t0)
colnames(conf_intervals) <- c("Lower", "Upper")

# Mostrar los intervalos de confianza
print(conf_intervals)


############################## PASO 9 Graficos  ################################
# Cargar ggplot2
library(ggplot2)


#1.Visualización de los Intervalos de Confianza

# Crear un dataframe para el gráfico
conf_intervals_df <- data.frame(
  Coeficiente = rownames(conf_intervals),
  Lower = conf_intervals[, "Lower"],
  Upper = conf_intervals[, "Upper"]
)

# Gráfico de barras con intervalos de confianza
ggplot(conf_intervals_df, aes(x = Coeficiente, y = (Lower + Upper) / 2)) +
  geom_point(size = 3) +  # Punto para el valor medio del intervalo
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +  # Barras de error
  labs(title = "Intervalos de Confianza para los Coeficientes",
       x = "Coeficiente",
       y = "Valor del Coeficiente") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x



#2.Gráfico de la Brecha Salarial Incondicional

# Gráfico de boxplot para la brecha salarial incondicional
ggplot(GEIH_df_filtro, aes(x = factor(sex), y = log_salario, fill = factor(sex))) +
  geom_boxplot() +
  labs(title = "Brecha Salarial Incondicional por Género",
       x = "Género (0 = Hombre, 1 = Mujer)",
       y = "Log(Salario)",
       fill = "Género") +
  theme_minimal()

#3.Gráfico de la Brecha Salarial Condicional

# Ajustar el modelo condicional
model_cond <- lm(log_salario ~ sex + age + I(age^2) + p6100 + fex_c + p6426 + relab, data = GEIH_df_filtro)

# Crear un dataframe con los coeficientes de "sex" y sus intervalos de confianza
conf_int_sex <- confint(model_cond)["sex", ]
coef_sex <- coef(model_cond)["sex"]

# Gráfico de barras para la brecha salarial condicional
ggplot(data.frame(Coeficiente = "sex", Valor = coef_sex, Lower = conf_int_sex[1], Upper = conf_int_sex[2]), 
       aes(x = Coeficiente, y = Valor)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(title = "Brecha Salarial Condicional por Género",
       x = "Coeficiente de Género",
       y = "Valor del Coeficiente") +
  theme_minimal()


#4. Gráfico del Perfil Edad-Salario por Género

# Ajustar modelos separados para hombres y mujeres
model_male <- lm(log_salario ~ age + I(age^2), data = GEIH_df_filtro %>% filter(sex == 0))
model_female <- lm(log_salario ~ age + I(age^2), data = GEIH_df_filtro %>% filter(sex == 1))

# Crear datos para predecir
age_range <- seq(min(GEIH_df_filtro$age), max(GEIH_df_filtro$age), length.out = 100)
pred_male <- predict(model_male, newdata = data.frame(age = age_range), interval = "confidence")
pred_female <- predict(model_female, newdata = data.frame(age = age_range), interval = "confidence")

# Crear un dataframe para el gráfico
plot_data <- data.frame(
  age = rep(age_range, 2),
  log_salario = c(pred_male[, "fit"], pred_female[, "fit"]),
  lower = c(pred_male[, "lwr"], pred_female[, "lwr"]),
  upper = c(pred_male[, "upr"], pred_female[, "upr"]),
  gender = rep(c("Hombre", "Mujer"), each = 100)
)

#5. Gráfico de líneas con intervalos de confianza
ggplot(plot_data, aes(x = age, y = log_salario, color = gender)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender), alpha = 0.2) +
  labs(title = "Perfil Edad-Salario por Género",
       x = "Edad",
       y = "Log(Salario)",
       color = "Género",
       fill = "Género") +
  theme_minimal()


#6. Gráfico de Dispersión con Predicciones

# Predecir salarios con el modelo condicional
GEIH_df_filtro$pred_wage <- predict(model_cond, newdata = GEIH_df_filtro)

# Gráfico de dispersión
ggplot(GEIH_df_filtro, aes(x = log_salario, y = pred_wage, color = factor(sex))) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicciones del Modelo Condicional",
       x = "Log(Salario Observado)",
       y = "Log(Salario Predicho)",
       color = "Género (0 = Hombre, 1 = Mujer)") +
  theme_minimal()


#7.Gráfico de la Distribución de Errores de Predicción por Género

# Calcular errores de predicción
GEIH_df_filtro$pred_error <- GEIH_df_filtro$log_salario - GEIH_df_filtro$pred_wage

# Gráfico de densidad
ggplot(GEIH_df_filtro, aes(x = pred_error, fill = factor(sex))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de Errores de Predicción por Género",
       x = "Error de Predicción",
       y = "Densidad",
       fill = "Género (0 = Hombre, 1 = Mujer)") +
  theme_minimal()