# ========================================================================
# 03_age_wage_profile.R
# Limpieza del entorno antes de ejecutar
# ========================================================================

rm(list = ls())  # Elimina todos los objetos del entorno de trabajo para evitar conflictos
message("Entorno limpio. Ejecutando el script...")

# Cargar paquetes necesarios
if(!require(pacman)) install.packages("pacman")
pacman::p_load(rio, tidyverse, lmtest, car, boot, stargazer, gridExtra, ggplot2)

# ========================================================================
# 1. Cargar datos limpios
# ========================================================================
message("Cargando datos procesados...")
geih_clean <- readRDS("stores/processed/geih_2018_clean.rds")

# ========================================================================
# 2. Estimación del perfil edad-salario
# ========================================================================
message("Estimando el perfil edad-salario...")

age_wage_model <- lm(log(ingtot) ~ age + I(age^2), data = geih_clean, na.action = na.exclude)

# Guardar resultados para su uso en el informe
saveRDS(age_wage_model, "stores/processed/age_wage_model.rds")

# Exportar tabla de resultados con formato AER 
stargazer(age_wage_model, 
          type = "latex",
          title = "Regresión Edad-Salario",
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("ser", "adj.rsq", "f"),
          dep.var.labels.include = FALSE,
          no.space = TRUE,
          notes = "",
          notes.append = FALSE,
          out = "views/tables/age_wage_regression.tex")

# ========================================================================
# 3. Prueba de heterocedasticidad
# ========================================================================
message("Realizando prueba de heterocedasticidad...")
heteroskedasticity_test <- bptest(age_wage_model, 
                                  ~ age + I(age^2) + fitted(age_wage_model)^2, 
                                  data = geih_clean)
print(heteroskedasticity_test)

# ========================================================================
# 4. Bootstrap para intervalo de confianza de la edad máxima
# ========================================================================
message("Realizando bootstrap para intervalos de confianza...")

boot_fn <- function(data, index) {
  sample_data <- data[index, ]
  model <- lm(log(ingtot) ~ age + I(age^2), data = sample_data)
  return(coef(model))
}

set.seed(3589)
boot_results <- boot(geih_clean, boot_fn, R = 1000)

beta1 <- boot_results$t[, 2]  # Coeficiente de age
beta2 <- boot_results$t[, 3]  # Coeficiente de age^2
edad_maxima_boot <- -beta1 / (2 * beta2)
edad_maxima_ic <- quantile(edad_maxima_boot, probs = c(0.025, 0.975))
edad_maxima_media <- mean(edad_maxima_boot)

bootstrap_results <- list(
  boot_object = boot_results,
  max_age_mean = edad_maxima_media,
  max_age_ci = edad_maxima_ic
)

saveRDS(bootstrap_results, "stores/processed/age_wage_bootstrap.rds")

message(paste("Edad máxima estimada:", round(edad_maxima_media, 2), 
              "años (IC 95%: [", round(edad_maxima_ic[1], 2), 
              ",", round(edad_maxima_ic[2], 2), "])"))

# ========================================================================
# 5. Visualización del perfil edad-salario
# ========================================================================
message("Creando visualización del perfil edad-salario...")

edad_seq <- seq(min(geih_clean$age, na.rm = TRUE), 
                max(geih_clean$age, na.rm = TRUE), 
                length.out = 100)

nuevo_df <- data.frame(age = edad_seq)
predicciones <- predict(age_wage_model, newdata = nuevo_df, interval = "confidence")

pred_df <- data.frame(
  age = edad_seq, 
  fit = predicciones[, "fit"], 
  lwr = predicciones[, "lwr"], 
  upr = predicciones[, "upr"]
)

age_wage_plot <- ggplot(data = geih_clean, aes(x = age, y = log(ingtot))) +
  geom_point(size = 0.8, alpha = 0.3, color = "grey50") +
  geom_line(data = pred_df, aes(x = age, y = fit), color = "black", size = 1) +
  geom_ribbon(data = pred_df, aes(x = age, ymin = lwr, ymax = upr), 
              fill = "grey30", alpha = 0.2) +
  labs(x = "Edad", 
       y = "Log(Salario por Hora)",
       title = "Perfil Edad-Salario") +
  theme_classic()

# Guardar gráfico
ggsave("views/figures/age_wage_profile.png", plot = age_wage_plot, width = 8, height = 6, dpi = 300)

# ========================================================================
# 6. Cálculo formal de la edad de máximo salario
# ========================================================================
beta1_hat <- coef(age_wage_model)[2]  # Coeficiente de age
beta2_hat <- coef(age_wage_model)[3]  # Coeficiente de age^2

max_age <- -beta1_hat / (2 * beta2_hat)

message("Análisis del perfil edad-salario completado exitosamente.")
message(paste("La edad de máximo salario estimada es:", round(max_age, 2), "años."))