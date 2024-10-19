# Librerías necesarias
library(ggplot2)
library(reshape2)

# Leer el archivo de resultados del test de Wilcoxon
archivo <- "resultados_wilcoxon.txt"
resultados <- readLines(archivo)

# Extraer las comparaciones y los p-valores
comparaciones <- grep("Comparación de promedios entre", resultados, value = TRUE)
valores_p <- grep("Resultado:", resultados, value = TRUE)

# Crear un data frame con las comparaciones y los p-valores
df_comparaciones <- data.frame(
  metodo1 = sub(".*entre (.*?) y (.*?) Resultado.*", "\\1", comparaciones),
  metodo2 = sub(".*entre (.*?) y (.*?) Resultado.*", "\\2", comparaciones),
  p_valor = as.numeric(sub(".*Resultado: (.*)", "\\1", valores_p)),
  stringsAsFactors = FALSE
)

# Crear una matriz vacía con todos los métodos
metodos <- unique(c(df_comparaciones$metodo1, df_comparaciones$metodo2))
matriz_p_valores <- matrix(NA, nrow = length(metodos), ncol = length(metodos),
                           dimnames = list(metodos, metodos))

# Rellenar la matriz con los p-valores correspondientes
for (i in 1:nrow(df_comparaciones)) {
  m1 <- df_comparaciones$metodo1[i]
  m2 <- df_comparaciones$metodo2[i]
  matriz_p_valores[m1, m2] <- df_comparaciones$p_valor[i]
  matriz_p_valores[m2, m1] <- df_comparaciones$p_valor[i]
}

# Llenar la diagonal con 1 (sin diferencia entre el mismo método)
diag(matriz_p_valores) <- 1

# Convertir la matriz a formato "long" para ggplot2
df_matriz <- melt(matriz_p_valores, na.rm = TRUE)

# Crear el mapa de calor
ggplot(df_matriz, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "black", na.value = "grey50", name = "p-valor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Método 1", y = "Método 2", title = "Mapa de Calor de Comparaciones Wilcoxon")