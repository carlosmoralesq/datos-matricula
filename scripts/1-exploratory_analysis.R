
# Preparamos espacio de trabajo ---------------------------------------------------------------

# Instalamos paquetes (si no están en el sistema)
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("gtsummary")

# Cargamos paquetes
library(data.table)
library(ggplot2)

# Importamos los datos
data <- readRDS(file = "data/data.RDS")


# Estadísticos descriptivos -------------------------------------------------------------------

summary(data) # Descriptivos básicos por variable

# # Con la función 'summary' obtenemos estadísticos descriptivos basados posición (cuartiles), así como la media aritmética (sólo cuando el primer argumento es un data.frame).

plot(data) # Graficamos los datos con 'base R'

# # Con la función 'plot' generamos una matriz de gráficos de dispersión. Ideal para la exploración rápida de data.frames 


# Exploración gráfica con ggplot2 -------------------------------------------------------------

# 1. Distribución de la edad de los alumnos agrupado por región
ggplot(data, aes(x = region, y = edad_alumnos)) +
  # Etiquetas de los ejes
  labs(x = "Región", y = "Edad alumnos (años)", title = "Edad de alumnos agrupado por región") + 
  # Gráfico de violín - primera capa
  geom_violin(aes(fill = region), show.legend = F, alpha = .3) + 
  # Gráfico de cajas - segunda capa
  geom_boxplot(aes(fill = region), show.legend = F, width = .3, notch = TRUE) +
  # Jitter (puntos) - tercera capa
  geom_jitter(cex = .1, width = 0.1) +
  # Barras de error usando intervalo de confianza de la media mediante Bootstrap - cuarta capa
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, col = "white", width = .2) +
  # Punto representando la media aritmética - quinta capa
  stat_summary(geom = "point", fun = mean, col = "white") +
  # Tema clásico (formato publicación)
  theme_classic()
