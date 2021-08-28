
# Preparamos espacio de trabajo ---------------------------------------------------------------

pkgs <- function(installed, needed) {
  if (any(!needed %in% installed)) {
    missing_pkgs <- needed[!needed %in% installed]
    message("Faltan los siguientes paquetes: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, verbose = FALSE, quiet = TRUE)
    message("Todo listo!")
  } else {
    message("No falta ningún paquete")
  }
}
pkg_instalados <- .packages(T) 
pkg_necesarios <- c("data.table",  # Computación rápdia
                    "ggplot2",     # Exploración gráfica
                    "GGally",      # Matriz de correlaciones
                    "magrittr",    # Pipe operator 
                    "knitr",       # Para la fabricación de tablas
                    "kableExtra",  # Para la personalización de tablas
                    "webshot",     # Para transformar archivos HTML en PDF
                    "correlation") # Correlaciones en formato largo

# Instalamos paquetes (si no están en el sistema)
pkgs(installed = pkg_instalados, needed = pkg_necesarios)

# Cargamos paquetes
library(data.table)
library(ggplot2)
library(GGally)
library(correlation)
library(magrittr)
library(knitr)
library(kableExtra)
library(webshot)

# Importamos los datos
data <- readRDS(file = "data/data.RDS")


# Estadísticos descriptivos -------------------------------------------------------------------

descriptivos <- summary(data) # Descriptivos básicos por variable

# # Con la función 'summary' obtenemos estadísticos descriptivos basados posición (cuartiles), así como la media aritmética (sólo cuando el primer argumento es un data.frame).

plot(data) # Graficamos los datos con 'base R'

pdf("output/figura-1.pdf"); plot(data); dev.off()

# # Con la función 'plot' generamos una matriz de gráficos de dispersión. Ideal para la exploración rápida de data.frames 


# Exploración gráfica con ggplot2 -------------------------------------------------------------

temp <- melt(data, measure.vars = c("matricula_coanil", "matricula_pie")
             )[j = variable := `levels<-`(variable, c("Coanil", "Pie"))][]

# 1. Distribución de la cantidad de matrícula de los alumnos agrupado por región
figure.2 <- ggplot(temp, aes(x = año, y = value, fill = año)) +
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

pdf("output/figura-2.pdf", width = 10); print(figure.2); dev.off()

# 1. Distribución de la cantidad de matrícula de los alumnos agrupado por región
figure.4 <- ggplot(temp, aes(x = edad_alumnos, y = value, col = region)) +
  facet_grid(cols = vars(año), rows = vars(variable), scales = "free") +
  geom_line() +
  theme_classic()

pdf("output/figura-4.pdf", width = 10); print(figure.4); dev.off()

# Correlaciones -------------------------------------------------------------------------------

# 2. Matriz de correlación
GGally::ggpairs(data, cardinality_threshold = 16)

pdf("output/figura-3.pdf",width = 40, height = 40); GGally::ggpairs(data, cardinality_threshold = 16); dev.off()

# Usando sintaxis de data.table

names(data) <- c("Año", "region", "Edad alumnos", "Matrícula Coanil", "Matrícula Pie")

datos_tabla <- data[
  # Realizamos correlación por pares de variables numéricas usando...
  j = correlation::correlation(.SD), 
  # Un 'subset of data' agrupado por región
  keyby = "region"
  # Luego filtramos por el valor de prueba de hipótesis
  ][i = p < 0.05, 
    j = .(Variable = paste(Parameter1, "y", Parameter2), "Correlación (r)" = round(r, 3)), 
    keyby = .(Región = region)]

# Exportar resultados a formato tabla ---------------------------------------------------------

datos_tabla %>%
  knitr::kable("html") %>%
  kableExtra::kable_styling(
  html_font = "Times",
  bootstrap_options = "condensed", 
  full_width = FALSE) %>%
  kableExtra::save_kable("output/tabla-1.html")

webshot::webshot(url = "output/tabla-1.html", 
  file = "output/tabla-1.pdf", 
  zoom = 1)