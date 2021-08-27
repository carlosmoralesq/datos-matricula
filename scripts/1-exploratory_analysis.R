
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

# Correlaciones -------------------------------------------------------------------------------


# Si no se ha hecho, primero instalar paquetes de Correlación
# install.packages("GGally")
# install.packages("correlation")
# 2. Matriz de correlación
GGally::ggpairs(data, cardinality_threshold = 16)

# # Matriz de correlación de ejemplo
GGally::ggpairs(iris, mapping = aes(col = Species))

# Usando sintaxis de data.table

names(data) <- c("Año", "region", "Edad alumnos", "Matrícula Coanil", "Matrícula Pie")

datos_tabla <- data[
  # Realizamos correlación por pares de variables numéricas usando...
  j = correlation::correlation(.SD), 
  # Un 'subset of data' agrupado por región
  keyby = "region"
  # Luego filtramos por el valor de prueba de hipótesis
  ][i = p < 0.05, 
    j = .(Variable = paste(Parameter1, "y", Parameter2), Correlación = round(r, 3)), 
    keyby = .(Región = region)]


# Exportar resultados a formato tabla ---------------------------------------------------------

# Instalamos paquetes necesarios
# install.packages("knitr")
# install.packages("kableExtra")
# install.packages("dplyr")

library(dplyr) # Para poder usar el 'pipe operator' (%>%)

datos_tabla %>%
  knitr::kable() %>%
  kableExtra::kable_paper() %>%
  kableExtra::kable_styling(full_width = FALSE, 
                            bootstrap_options = "condensed")
