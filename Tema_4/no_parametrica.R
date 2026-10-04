# Crear datos de ejemplo (dos grupos independientes)
grupo_control <- c(12, 14, 15, 10, 18, 13, 16, 11)
grupo_tratamiento <- c(22, 25, 20, 24, 28, 23, 26, 21)

# Juntar en un data frame
datos <- data.frame(
  valor = c(grupo_control, grupo_tratamiento),
  grupo = rep(c("Control", "Tratamiento"), each = 8)
)

# 1. Visualización gráfica (Diagrama de caja)
boxplot(valor ~ grupo, data = datos,
        main = "Comparación de Medianas entre Grupos",
        ylab = "Valores",
        xlab = "Grupos",
        col = c("lightblue", "lightgreen"))

# 2. Aplicar la prueba U de Mann-Whitney (Wilcoxon rank-sum)
resultado <- wilcox.test(valor ~ grupo, data = datos, exact = FALSE)

# Mostrar resultados
print(resultado)
