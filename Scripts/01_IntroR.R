##########################################################################
############            Introducción al Lenguaje R            ############
##########################################################################

# Configuraciones iniciales

# Eliminación de la notación científica
5/1230000
options(scipen = 999)

# Fijación del número de decimales
3.467/5.973
options(digits = 5)

# Instalación y carga de librerías
install.packages("data.table") # No se instalan las dependencias, lo cual puede causar errores en ciertos casos por la ausencia de funciones auxiliares
install.packages("tidyverse", dependencies = TRUE)

library(data.table)
library(tidyverse)

# Se puede fijar que un conjunto de paquetes se carguen por defecto cuanto arranca la sesión de R
# Se verifica la existencia el archivo Rprofile
if(!file.exists("~/.Rprofile")){
  file.create("~/.Rprofile") 
} else {
  file.edit("~/.Rprofile")
}





