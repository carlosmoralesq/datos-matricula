
# Preparamos el espacio de trabajo ------------------------------------------------------------

# Instalamos paquetes (si no están en el sistema)
# install.packages("data.table")

# Cargamos paquetes
library(data.table)


# Cargamos los datos --------------------------------------------------------------------------

data <- fread(
  # Ubicación del archivo
  input = "data/datos_matriculas.csv",
  # Nombre de las columnas
  col.names = c("año", "region", "edad_alumnos", "matricula_coanil", "matricula_pie")
)


# Transformación / Manipulación ---------------------------------------------------------------

data[, region := factor(region)][]

# Guardamos los datos procesados --------------------------------------------------------------

saveRDS(data, file = "data/data.RDS")
