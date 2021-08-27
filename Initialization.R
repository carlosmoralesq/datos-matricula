# Para la preparación del espacio de trabajo se realizará la siguiente secuencia de comandos:

# Integrar GitHub con RStudio
## 1. Descargar Git
### http://git-scm.com/downloads
## 2. Tools -> Global Options -> Git/SVN -> Some magic
## 3. Copiar y pegar en la terminal
### git config --global user.email "example@mail.com"
### git config --global user.name "username"
## 4. Haz alguna modificación de prueba -> guardar
## 5. commit -> autenticación del navegador -> Todo listo

# Instalar Rtools
install.packages("pkgbuild")
pkgbuild::check_build_tools()

## Esto descargará Rtools desde la web, comenzará a instalarse y una vez terminada la instalación se volverá a correr:

pkgbuild::check_build_tools()

## Luego se instalarán paquetes de utilidad

### Manipulación de variables
install.packages("fastverse")
install.packages("tidyverse")

### Análisis estadístico
install.packages("rstatix")
install.packages("MASS")
install.packages("afex")
install.packages("correlation")
install.packages("parameters")
install.packages("car")
install.packages("lme4")
install.packages("lmerTest")

### Desarrollo e importación de paquetes
install.packages("devtools")
install.packages("remotes")
install.packages("usethis")

### Gráficos y complementos de ggplot2
install.packages("ggstatsplot")
install.packages("ggpubr")
install.packages("GGally")
install.packages("ggExtra")
install.packages("ggridges")
install.packages("ggbeeswarm")
install.packages("dabestr")

# Este no puede faltar por supuesto
remotes::install_github("matcasti/writR")
