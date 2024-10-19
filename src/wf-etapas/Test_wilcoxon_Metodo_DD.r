# Cargar las librerías
rm(list = ls())
gc(full = TRUE)

library(data.table)
library(readxl)
library(dplyr)

# Establecer el directorio de trabajo
script_path <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "")
setwd(script_path)

# Valores de envíos a filtrar
valores <- c(9000, 9500, 10000, 10500, 11000, 11500, 12000, 12500, 13000)

# Función para leer y filtrar los archivos
leer_y_filtrar <- function(archivo) {
  # Especifica que las columnas son numéricas
  data <- read_excel(archivo, col_types = "numeric") %>%
    filter(envios %in% valores)
  return(data)
}

# Lista de archivos Excel
archivos <- c("ganancias_deflacion.xlsx", "ganancias_dolar_blue.xlsx", "ganancias_dolar_oficial.xlsx",
              "ganancias_estandarizar.xlsx", "ganancias_mediana.xlsx", "ganancias_ninguno.xlsx",
              "ganancias_rank_cero_fijo.xlsx", "ganancias_rank_simple.xlsx", "ganancias_uva.xlsx")

# Leer y filtrar todos los archivos
lista_datos <- lapply(archivos, leer_y_filtrar)

# Archivo de salida para los resultados
output_file <- "resultados_wilcoxon.txt"
writeLines(c("Resultados del Test de Wilcoxon\n"), output_file)

# Calcular los promedios de cada archivo
lista_promedios <- lapply(lista_datos, function(data) colMeans(data %>% select(starts_with("m")), na.rm = TRUE))

# Comparar los promedios entre archivos
cat("\nComparaciones de promedios entre archivos\n", file = output_file, append = TRUE)

for (i in 1:(length(lista_promedios) - 1)) {
  for (j in (i + 1):length(lista_promedios)) {
    nombre_1 <- archivos[i]
    nombre_2 <- archivos[j]
    
    promedios_1 <- lista_promedios[[i]]
    promedios_2 <- lista_promedios[[j]]
    
    # Asegurarse de que los promedios tengan la misma longitud
    if (length(promedios_1) == length(promedios_2)) {
      
      # Filtrar promedios constantes o con longitud insuficiente
      if (length(na.omit(promedios_1)) > 1 && length(na.omit(promedios_2)) > 1 && 
          length(unique(promedios_1)) > 1 && length(unique(promedios_2)) > 1) {
        
        resultado <- suppressWarnings(
          wilcox.test(as.numeric(promedios_1), as.numeric(promedios_2), exact = FALSE)
        )
        
        p_value <- resultado$p.value
        cat(paste("Comparación de promedios entre", nombre_1, "y", nombre_2, 
                  "Resultado:", p_value), "\n", file = output_file, append = TRUE)
        
        # Evaluar cuál de los métodos tiene un promedio mayor
        if (p_value < 0.05) {
          if (mean(promedios_1, na.rm = TRUE) > mean(promedios_2, na.rm = TRUE)) {
            cat(paste(nombre_1, "es significativamente mejor que", nombre_2, "\n"), file = output_file, append = TRUE)
          } else {
            cat(paste(nombre_2, "es significativamente mejor que", nombre_1, "\n"), file = output_file, append = TRUE)
          }
        } else {
          cat(paste("No hay una diferencia significativa entre", nombre_1, "y", nombre_2, "\n"), file = output_file, append = TRUE)
        }
        
      } else {
        cat(paste("Advertencia: Datos constantes o insuficientes para la comparación de promedios entre", 
                  nombre_1, "y", nombre_2, "\n"), file = output_file, append = TRUE)
      }
    } else {
      cat(paste("Advertencia: Diferentes tamaños de promedios entre", 
                nombre_1, "y", nombre_2, "\n"), file = output_file, append = TRUE)
    }
  }
}
