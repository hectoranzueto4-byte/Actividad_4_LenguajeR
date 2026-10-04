# 1. Instalar y cargar librerías necesarias
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")

library(ggplot2)
library(tidyr)

# 2. Configuración de parámetros del sistema
set.seed(42)  # Semilla para reproducibilidad
dias <- 30
punto_reorden <- 15  # Cuando el inventario baja de 15, se pide más
cantidad_pedido <- 50 # Tamaño del lote de reabastecimiento (Q)
tiempo_entrega <- 3   # Días que tarda el proveedor en entregar

# Vectores para almacenar el estado diario
inventario_disponible <- numeric(dias)
espera_clientes <- numeric(dias)
balance_neto <- numeric(dias)

# Estado inicial (Día 1)
inventario_actual <- 40
clientes_en_espera <- 0
dia_entrega_pendiente <- NA

# 3. Ciclo de simulación diaria
for (t in 1:dias) {
  # Generar una demanda aleatoria para el día (Distribución de Poisson)
  demanda_dia <- rpois(1, lambda = 12) 
  
  # Procesar llegada de reabastecimiento si corresponde
  if (!is.na(dia_entrega_pendiente) && t == dia_entrega_pendiente) {
    inventario_actual <- inventario_actual + cantidad_pedido
    dia_entrega_pendiente <- NA
  }
  
  # Afectar el inventario neto con la demanda del día
  balance_previo <- inventario_actual - clientes_en_espera - demanda_dia
  
  if (balance_previo >= 0) {
    inventario_actual <- balance_previo
    clientes_en_espera <- 0
  } else {
    inventario_actual <- 0
    clientes_en_espera <- abs(balance_previo) # Clientes que entran en lista de espera
  }
  
  # Guardar los estados del día t
  inventario_disponible[t] <- inventario_actual
  espera_clientes[t] <- clientes_en_espera
  balance_neto[t] <- inventario_actual - clientes_en_espera
  
  # Evaluar si se debe colocar una nueva orden de reorden
  if (balance_neto[t] <= punto_reorden && is.na(dia_entrega_pendiente)) {
    dia_entrega_pendiente <- t + tiempo_entrega
  }
}

# 4. Estructurar los datos para graficar
df_simulacion <- data.frame(
  Dia = 1:dias,
  Inventario_Fisico = inventario_disponible,
  Clientes_Espera = espera_clientes,
  Balance_Neto = balance_neto
)

df_long <- pivot_longer(df_simulacion, 
                        cols = c("Inventario_Fisico", "Clientes_Espera", "Balance_Neto"), 
                        names_to = "Metrica", values_to = "Cantidad")

# 5. Generar la visualización gráfica
ggplot(df_long, aes(x = Dia, y = Cantidad, color = Metrica, group = Metrica)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = punto_reorden, linetype = "dotted", color = "orange", size = 1) +
  scale_color_manual(values = c("Balance_Neto" = "purple", 
                                "Clientes_Espera" = "red", 
                                "Inventario_Fisico" = "darkgreen"),
                     labels = c("Balance Neto (Fisico - Espera)", 
                                "Clientes en Lista de Espera", 
                                "Inventario Físico Real")) +
  labs(title = "Simulación Dinámica de Inventario con Clientes en Espera",
       subtitle = "Efecto de la demanda aleatoria y retrasos de entrega",
       x = "Días de Operación",
       y = "Cantidad de Unidades",
       color = "Métrica del Sistema") +
  theme_minimal() +
  theme(legend.position = "bottom")
