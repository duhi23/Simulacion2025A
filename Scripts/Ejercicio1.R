# Ejercicio 1

# Tasa de ingreso de llamadas 
lambda <- function(t){
  return(20/(1+exp(-(t-8))))
}

plot(seq(0,16, by = 0.05), lambda(seq(0,16, by = 0.05)), type = "l",
     xlab = "Tiempo de servicio", ylab = "Tasa de ingreso de llamadas")

# Simulación de Ingreso de llamadas
intervalos <- seq(0,16, length.out = 6)

tllamadas <- poisson_no_homogeneo(lambda_t = lambda, subintervalos = intervalos)
plot(tllamadas$Tiempos, 1:length(tllamadas$Tiempos), type = "s",
     xlab = "Tiempos de arribo", ylab = "Llamadas recibidas")
# Número de llamadas en toda la jornada
length(tllamadas$Tiempos)
# Total de llamadas por franja horaria
sum(tllamadas$Tiempos <= 4) # [0,4]
sum(tllamadas$Tiempos > 4 & tllamadas$Tiempos <= 12) # (4,12]
sum(tllamadas$Tiempos > 12) # (12,16]

# Tabla con la información de las llamadas
library(data.table)
TT <- data.table(Llamada = 1:length(tllamadas$Tiempos),
                 Llegada = tllamadas$Tiempos,
                 Tiempo = rexp(length(tllamadas$Tiempos), rate = 5))
TT[, TiempoMin := ceiling(Tiempo*60)]
TT[, Finalizacion := Llegada + Tiempo]
# Suponga que los trabajadores que superan las 16 horas de trabajo
# reciben un compensación salarial (15 usd)
TT[,Marca_Comp := ifelse(Finalizacion > 16, 1, 0)]
TT[,list(sum(Marca_Comp))]
TT[, LLamadasEnAtencion := c(0, sapply(2:nrow(TT), function(k){
  sum(TT[k,Llegada] < TT[1:(k-1), Finalizacion])
}))]
TT[, OperadoresRequeridos := LLamadasEnAtencion + 1]
# Estadísticas
TT[,list(Operadores = ifelse(max(OperadoresRequeridos) > 10, 1, 0),
         Finalizacion = ifelse(max(Finalizacion) <= 16, 1, 0),
         Operadores = ifelse(max(OperadoresRequeridos*ifelse(max(Finalizacion) > 16,1,0)) > 3, 1, 0),
         Llamadas = ifelse(sum(ifelse(Llegada <= 5, 1, 0)) <= 2, 1, 0))] # más de 10 operadores


# Simule el proceso en un periodo de 30 días
# a) Número de días en los que se requirieron más de 10 operadores
# b) Número de días en los cuales la jornada finalizo antes de las 16 horas
# c) Número de días en los que se requirieron más de 3 operadores fueran del horario de las 16 horas
# para completar la atención
# d) Número de días en los cuales en las primeras 5 horas no se recibieron más de 2 llamadas




