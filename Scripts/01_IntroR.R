##########################################################################
############            Introducción al Lenguaje R            ############
##########################################################################

# Configuraciones iniciales ----

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

# Creación de objetos ----
# En el Lenguaje R existen varios tipos de objetos que permiten almacenar información importante para posteriores análisis

# Vectores
c(2,5,8,12)
sum(c(2,5,8,12)) # Suma de los elementos del vector
mean(c(2,5,8,12)) # Valor Promedio de los elementos almacenados en el vector

x <- c(2,5,8,12) # asignación de un nombre para el vector
sum(x)
mean(x) # Se realiza el llamado al vector por su nombre o alias con ello se evita reescribir el vector

x + 2 # Suma de un vector con un escalar, se aplica el reciclaje de elementos











