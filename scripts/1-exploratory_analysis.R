
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
                    "gtsummary",   # Fabricación de tablas con descriptivos
                    "knitr",       # Para la fabricación de tablas
                    "kableExtra",  # Para la personalización de tablas
                    "webshot",     # Para transformar archivos HTML en PDF
                    "remotes",
                    "correlation") # Correlaciones en formato largo

# Instalamos paquetes (si no están en el sistema)
pkgs(installed = pkg_instalados, needed = pkg_necesarios)

# Paquete necesario para estadísticos descriptivos
if(!"writR" %in% pkg_instalados) remotes::install_github("matcasti/writR")

# Cargamos paquetes - necesarios
library(data.table)
library(ggplot2)
library(magrittr)
library(gtsummary)

# Opcional cargarlos - llamamos funciones mediante `Namespacing` (i.e. paquete::funcion)
# library(GGally)
# library(correlation)
# library(knitr)
# library(kableExtra)
# library(webshot)

# Importamos los datos
data <- readRDS(file = "data/data.RDS")

# Estadísticos descriptivos -------------------------------------------------------------------

summary(data, maxsum = 16) # Descriptivos básicos por variable

# # Con la función 'summary' obtenemos estadísticos descriptivos basados posición (cuartiles), 
# # así como la media aritmética (sólo cuando el primer argumento es un data.frame).

# Tablas --------------------------------------------------------------------------------------

# Configuración: lenguaje de tablas gtsummary en español (inglés viene por default)
gtsummary::theme_gtsummary_language("es")

## Tabla 1. Descriptivos generales usando el paquete gtsummary agrupado por año ----

# Usando los datos, excluyento región...
data[j = -c("region")] %>%  
  # Hacemos una tabla resumen
  gtsummary::tbl_summary(
    # Agrupando por año
    by = "año",
    label = list(
      # (Etiquetas)
      edad_alumnos ~ "Edad alumnos (años)",
      matricula_coanil ~ "Matrícula Coanil",
      matricula_pie ~ "Matrícula Pie"
    ),
    # Expresando por fila, debajo de cada variable
    type = all_continuous() ~ "continuous2",
    # Los siguientew estadísticos
    statistic = all_continuous() ~ c(
      "{mean} ± {sd}", "{median} ({p25}, {p75})", "[{min}, {max}]"
    )
  )

## Tabla 2. Correlaciones significativas (p < 0.05) por pares de variables agrupadas por región ----

`names<-`(data, c("Año", "region", "Edad alumnos", "Matrícula Coanil", "Matrícula Pie"))[
  # Realizamos correlación por pares de variables numéricas usando...
  j = correlation::correlation(.SD), 
  # Un 'subset of data' agrupado por región
  keyby = "region"
  # Luego filtramos por el valor de prueba de hipótesis
  ][i = p < 0.05, 
    j = list(
      Variable = paste(Parameter1, "y", Parameter2), 
      "Correlación (r)" = round(r, 3)
    ), 
    keyby = .(Región = region)]

## Tabla 3. Variaciones porcentuales de matricula Pie y Coanil agrupados por región entre 2017 al 2020. ----

# Datos de la tabla
data[
  # Calcular la media de cada matrícula
  j = list(pie = mean(matricula_pie), coanil = mean(matricula_coanil)),
  # Agrupar por region y año
  keyby = list(region, año)][
    # Asignar una columna a cada año
    j = dcast(.SD, region ~ año, value.var = c("pie", "coanil"))][
       # Porcentaje de variación entre pares de años consecutivos
       j = list(
         `2017 a 2018` = paste0(round((pie_2018/pie_2017 - 1)*100, 1), "%")
         , `2018 a 2019` = paste0(round((pie_2019/pie_2018 - 1)*100, 1), "%")
         , `2019 a 2020` = paste0(round((pie_2020/pie_2019 - 1)*100, 1), "%")
         , `2017 a 2018` = paste0(round((coanil_2018/coanil_2017 - 1)*100, 1), "%")
         , `2018 a 2019` = paste0(round((coanil_2019/coanil_2018 - 1)*100, 1), "%")
         , `2019 a 2020` = paste0(round((coanil_2020/coanil_2019 - 1)*100, 1), "%")
         ),
       # Agrupar nuevamente por región
       keyby = list(Región = region)]

## Tabla 4. Estadísticos de estimación de centralidad y dispersión de las variaciones porcentuales de matrícula Pie y Coanil agrupados por región entre 2017 al 2020. ----

data[
  # Calcular la media de cada matrícula
  j = list(pie = sum(matricula_pie), coanil = sum(matricula_coanil)),
  # Agrupar por region y año
  keyby = .(año, region)
  ][# Asignar una columna a cada año
    j = dcast(.SD, region ~ año, value.var = c("pie", "coanil"))
     ][# Porcentaje de variación entre pares de años consecutivos
       j = list(
         `pie_2018_2017` = round((pie_2018/pie_2017 - 1)*100, 1)
         , `pie_2019_2018` = round((pie_2019/pie_2018 - 1)*100, 1)
         , `pie_2020_2019` = round((pie_2020/pie_2019 - 1)*100, 1)
         , `coanil_2018_2017` = round((coanil_2018/coanil_2017 - 1)*100, 1)
         , `coanil_2019_2018` = round((coanil_2019/coanil_2018 - 1)*100, 1)
         , `coanil_2020_2019` = round((coanil_2020/coanil_2019 - 1)*100, 1)
         ),
       # Agrupar nuevamente por región
       keyby = list(Región = region)
     ][j = melt(.SD, id.var = "Región")
       ][j = list(
         `Media ± DE` = writR::cent_disp(value, str.a = "{mean} ± {sd}"),
         `Mediana (RIQ)` = writR::cent_disp(value, str.a = "{median} ({IQR})"),
         `[Rango]` = writR::cent_disp(value, str.a = "[{min}, {max}]")
       ), by = list(variable)
       ][j = transpose(.SD, keep.names = "Estadísticos", make.names = "variable")]
