# 1. Cargar la librería necesaria para visualización
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 2. Definir parámetros del problema M/M/1
lambda <- 15  # Tasa media de llegadas por hora
mu     <- 20  # Tasa media de servicio por hora

# 3. Calcular métricas operacionales básicas
rho <- lambda / mu                      # Factor de utilización del sistema
L   <- lambda / (mu - lambda)           # Número promedio de clientes en el sistema
Lq  <- (lambda^2) / (mu * (mu - lambda)) # Número promedio de clientes en la cola
W   <- 1 / (mu - lambda)                # Tiempo promedio en el sistema (horas)
Wq  <- lambda / (mu * (mu - lambda))    # Tiempo promedio en la cola (horas)

# Mostrar métricas en consola
cat("--- MÉTRICAS DEL SISTEMA DE ESPERA M/M/1 ---\n")
cat(sprintf("Utilización del Servidor (rho): %.2f%%\n", rho * 100))
cat(sprintf("Clientes promedio en el Sistema (L): %.2f clientes\n", L))
cat(sprintf("Clientes promedio en la Cola (Lq): %.2f clientes\n", Lq))
cat(sprintf("Tiempo promedio de espera en Sistema (W): %.2f minutos\n", W * 60))
cat(sprintf("Tiempo promedio de espera en Cola (Wq): %.2f minutos\n", Wq * 60))

# 4. Preparar datos para la visualización del estado del sistema
# Calculamos la probabilidad de tener exactamente 'n' clientes en el sistema: Pn = (1-rho) * rho^n
n_clientes <- 0:10
probabilidades <- (1 - rho) * (rho ^ n_clientes)

df_colas <- data.frame(
  Clientes = n_clientes,
  Probabilidad = probabilidades
)

# 5. Crear el gráfico interactivo/visual elegante con ggplot2
ggplot(df_colas, aes(x = factor(Clientes), y = Probabilidad)) +
  geom_bar(stat = "identity", fill = "#3182bd", color = "#08519c", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Probabilidad * 100)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Probabilidad del Número de Clientes en el Sistema (M/M/1)",
    subtitle = paste0("Tasa de llegada (lambda) = ", lambda, "/hr | Tasa de servicio (mu) = ", mu, "/hr"),
    x = "Número de Clientes Exactos en el Sistema (Cola + Atención)",
    y = "Probabilidad de Ocurrencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    panel.grid.minor = element_blank()
  )
