
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
                    "magrittr")    # Pipe operator 
                    
# Instalamos paquetes (si no están en el sistema)
pkgs(installed = pkg_instalados, needed = pkg_necesarios)

# Cargamos paquetes - necesarios
library(data.table)
library(ggplot2)
library(magrittr)

# Opcional cargarlos - llamamos funciones mediante `Namespacing` (i.e. paquete::funcion)
# library(GGally)

# Importamos los datos
data <- readRDS(file = "data/data.RDS")

# Exploración gráfica -------------------------------------------------------------------------

# Figura 1. Gráficos exploratorios.
pdf(file = "output/figura-1.pdf"); plot(data); dev.off()

# Figura 2. Matriz de correlación de los datos
pdf(file = "output/figura-2.pdf", width = 30, height = 30); print(GGally::ggpairs(data, cardinality_threshold = 16)); dev.off()

temp <- melt(data, measure.vars = c("matricula_coanil", "matricula_pie")
             )[j = variable := `levels<-`(variable, c("Coanil", "Pie"))][]

# Figura 3. Distribución de la cantidad de matrícula de alumnos agrupado por año y tipo de matrícula.
fig3 <- ggplot(temp, aes(x = año, y = value, fill = año)) +
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
pdf(file = "output/figura-3.pdf", width = 10); print(fig3); dev.off()

# Figura 4. Distribución de la cantidad de matrícula de los alumnos agrupado por región y año.
fig4 <- ggplot(temp, aes(x = edad_alumnos, y = value, col = region)) +
  labs(y = "Matrículas", col = "Región", x = "Edad (años)") +
  facet_grid(cols = vars(año), rows = vars(variable), scales = "free_y") +
  geom_line() +
  theme_classic()
pdf(file = "output/figura-4.pdf",width = 10); print(fig4); dev.off()

# Figura 5. Suma de cantidad de matrículas por año agrupado por tipo de matricula.
fig5 <- temp[j = list(Matrícula = sum(value)), by = .(variable, año)] %>%
ggplot(aes(x = año, y = Matrícula)) +
  facet_grid(rows = vars(variable), scales = "free_y") +
  geom_point() +
  geom_line(aes(group = 1)) +
  geom_smooth(aes(group = 1), method = "lm", lwd = 0.5) +
  theme_classic()
pdf(file = "output/figura-5.pdf"); print(fig5); dev.off()
