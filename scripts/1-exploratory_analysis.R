
# Preparamos espacio de trabajo ---------------------------------------------------------------

# Instalamos paquetes (si no están en el sistema)
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("GGally")
# install.packages("dplyr")
# install.packages("correlation")

# Cargamos paquetes
library(data.table)
library(ggplot2)

# Importamos los datos
data <- readRDS(file = "data/data.RDS")


# Estadísticos descriptivos -------------------------------------------------------------------

resultados <- summary(data) # Descriptivos básicos por variable

# # Con la función 'summary' obtenemos estadísticos descriptivos basados posición (cuartiles), así como la media aritmética (sólo cuando el primer argumento es un data.frame).

plot(data) # Graficamos los datos con 'base R'

# # Con la función 'plot' generamos una matriz de gráficos de dispersión. Ideal para la exploración rápida de data.frames 


# Exploración gráfica con ggplot2 -------------------------------------------------------------

temp <- melt(data, measure.vars = c("matricula_coanil", "matricula_pie")
             )[j = variable := `levels<-`(variable, c("Coanil", "Pie"))][]

# 1. Distribución de la cantidad de matrícula de los alumnos agrupado por región
ggplot(temp, aes(x = año, y = value, fill = año)) +
  facet_wrap(~variable, nrow = 2, scales = "free_y", ) +
  # Etiquetas de los ejes
  labs(x = "Año", y = "Matrículas", fill = "Año") + 
  # Gráfico de violín - primera capa
  geom_violin(alpha = .3) + 
  # Gráfico de cajas - segunda capa
  geom_boxplot(width = .1, outlier.colour = NA, notch = TRUE) +
  # Jitter (puntos) - tercera capa
  geom_jitter(cex = .05, width = .03) +
  # Barras de error usando intervalo de confianza de la media mediante Bootstrap - cuarta capa
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, col = "white", width = .1) +
  # Punto representando la media aritmética - quinta capa
  stat_summary(geom = "point", fun = mean, col = "white") +
  # Tema clásico (formato publicación)
  theme_classic()

# 2. Matriz de correlación
GGally::ggpairs(data, cardinality_threshold = 16)

# # Matriz de correlación de ejemplo
GGally::ggpairs(iris, mapping = aes(fill = Species), alpha = .5)


# Correlaciones -------------------------------------------------------------------------------

# Usando sintaxis de data.table
data[j = correlation::correlation(.SD), keyby = "region"][p < 0.05, .(paste(Parameter1, "y", Parameter2), r), "region"]
# Pros: Más rápido, más conciso
# Contras: Código más complejo


# Usando dplyr - tidyverse
library(dplyr)

data %>% 
  group_by(region) %>%
  correlation::correlation(method = "spearman") %>%
  filter(p > 0.05) 
# Pros: Fácil entender (more 'verbose') por su naturaleza secuencial
# Contra: Lento en casos de datos muy grandes (1 Millon de filas)