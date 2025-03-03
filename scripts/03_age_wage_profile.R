# Regresión para estimar el perfil

reg3 <- lm(log(ingtot) ~ age + I(age^2), data = GEIH_seleccionado, a.action = na.exclude )

# Exportar resultados con formato AER
stargazer(reg3, type = "text", title = "Regresión Edad-Salario",
          star.cutoffs = NA,
          omit.stat = c("ser", "adj.rsq", "f"),  
          dep.var.labels.include = FALSE, 
          no.space = TRUE, 
          notes = "",
          notes.append = FALSE)  


# Prueba de Heterocedasticidad
bptest(reg3, ~ age + I(age^2) + fitted(reg3)^2, data = GEIH_seleccionado)

# Bootstrap para intervalo de confianza

boot_fn <- function(data, index) {
  sample_data <- data[index, ]  # Tomamos la muestra bootstrap
  model <- lm(log(ingtot) ~ age + I(age^2), data = sample_data)  # Ajustamos el modelo
  return(coef(model))  # Retornamos los coeficientes
}

# Aplicar bootstrap con 10000 repeticiones
set.seed(3589)
boot_results <- boot(GEIH_seleccionado, boot_fn, R = 1000)

# Extraer coeficientes bootstrap
beta1 <- boot_results$t[, 2]  # Coeficiente de age
beta2 <- boot_results$t[, 3]  # Coeficiente de age^2

# Calcular la edad máxima en cada iteración bootstrap
edad_maxima_boot <- -beta1 / (2 * beta2)

# Calcular IC del 95% para la edad máxima
edad_maxima_ic <- quantile(edad_maxima_boot, probs = c(0.025, 0.975))

# Crear datos para la curva estimada
edad_seq <- seq(min(GEIH_seleccionado$age, na.rm = TRUE), 
                max(GEIH_seleccionado$age, na.rm = TRUE), length.out = 90)

# Crear nuevo data.frame para predicción
nuevo_df <- data.frame(age = edad_seq)

# Realizar predicciones con IC
predicciones <- predict(reg3, newdata = nuevo_df, interval = "confidence")

# Convertir predicciones en data.frame
pred_df <- data.frame(age = edad_seq, 
                      fit = predicciones[, "fit"], 
                      lwr = predicciones[, "lwr"], 
                      upr = predicciones[, "upr"])

pred_df$ingtot <- exp(pred_df$fit)

# Convertir los límites del intervalo de confianza a la escala original
pred_df$lwr_ingtot <- exp(pred_df$lwr)
pred_df$upr_ingtot <- exp(pred_df$upr)

# Crear gráfico con ggplot2 usando formato AER
ggplot(data = GEIH_seleccionado, aes(x = age, y = log(ingtot))) +
  geom_point(size = 1.5, alpha = 0.7, color = "grey") +  
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed") +  
  geom_line(data = data.frame(age = edad_seq, fit = predicciones[, "fit"]),
            aes(x = age, y = fit), color = "black", size = 1) +  
  geom_ribbon(data = pred_df, aes(x = age, ymin = lwr, ymax = upr), fill = "grey", alpha = 0.2) +  
  geom_vline(xintercept = mean(edad_maxima_boot), linetype = "dashed", color = "black", size = 1) +  
  annotate("text", x = mean(edad_maxima_boot) + 2, y = max(predicciones[, "fit"]),
           label = paste0("Edad Máx: ", round(mean(edad_maxima_boot), 1), " años\nIC: [",
                          round(edad_maxima_ic[1], 1), ", ", round(edad_maxima_ic[2], 1), "]"),
           color = "black", size = 4, hjust = 0) +  
  labs(x = "Edad", y = "Log(Ingresos Totales)") +  
  theme_classic() +  
  theme(
    text = element_text(size = 14), 
    axis.title = element_text(face = "bold"),  
    axis.text = element_text(color = "black"),  
    plot.caption = element_text(hjust = 0, face = "italic")  
  )