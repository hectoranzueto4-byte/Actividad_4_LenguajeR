# 1. Cargar la librería necesaria
library(ggplot2)

# 2. Simular datos de dos grupos (ej. Rendimiento de dos cultivos)
set.seed(123)
grupo_control   <- rnorm(30, mean = 15, sd = 2) # Media = 15
grupo_tratamiento <- rnorm(30, mean = 18, sd = 2) # Media = 18

# Combinar en un único DataFrame
datos <- data.frame(
  Rendimiento = c(grupo_control, grupo_tratamiento),
  Grupo = rep(c("Control", "Tratamiento"), each = 30)
)

# 3. Ejecutar la prueba paramétrica (Prueba t de Student)
resultado_ttest <- t.test(Rendimiento ~ Grupo, data = datos, var.equal = TRUE)
print(resultado_ttest)

# 4. Visualización Gráfica en R
# Gráfico combinando Boxplot (diagrama de caja) y puntos para ver la distribución
ggplot(datos, aes(x = Grupo, y = Rendimiento, fill = Grupo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 2, aes(color = Grupo)) +
  labs(
    title = "Comparación de Rendimiento entre Grupos",
    subtitle = paste("Prueba t, p-value =", round(resultado_ttest$p.value, 5)),
    x = "Grupo de Estudio",
    y = "Rendimiento (unidades)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
