# Solución Primer Examen - Semestre 2025A

# EJERCICIO 1

# n: número de afiliados
siniestros <- function(num){
      return(rpois(n=num, lambda = 1.4))
}

siniestros(4)

atencion <- function(){
      return(sample(c(rnorm(n=1, mean = 60, sd = 12), rgamma(n=1, shape = 2.8, scale = 410), rlnorm(n=1, meanlog = 8, sdlog = 0.8)), 
             size = 1, prob = c(0.60, 0.30, 0.10)))
}
atencion()

total_atenciones <- function(n){
      if(n == 0){
            return(0)
      } else {
            return(round(sum(sapply(1:n, function(x){atencion()})),2))
      }
}

total_atenciones(1)

suma_asegurada <- function(n){
      return(sample(c(120000, 80000, 50000, 30000), size = n, replace = TRUE, prob = c(0.05, 0.15, 0.35, 0.45)))
}

suma_asegurada(5)

prima_anual <- function(val){
      if(val == 120000){
            return(500)
      } else if(val == 80000){
            return(370)
      } else if(val == 50000){
            return(210)
      } else {
            return(140)
      }
}

# Simulación de escenarios
simu <- function(n, deducible = 120, coef_suma = 1){
      res <- sapply(1:n, function(x){
            Tabla <- function(n){
                  res <- data.table(Poliza = 1:n,
                                    Siniestros = siniestros(n),
                                    Deducible = deducible,
                                    Suma_Asegurada = coef_suma*suma_asegurada(n))
                  return(res)
            }
            T1 <- Tabla(1400) # Afiliados
            T1[, Total_Gastos := total_atenciones(Siniestros), by = Poliza] 
            T1[, Prima := prima_anual(Suma_Asegurada), by = Poliza]
            T1[, Monto_Cubierto := ifelse(Total_Gastos > Deducible, Total_Gastos - Deducible, 0)]
            T1[, Ganancia_Perdida := Prima - Monto_Cubierto]
            estadisticas <- T1[,list(Ganancia_Perdida = sum(Ganancia_Perdida), 
                                     Perdidas = sum(ifelse(Ganancia_Perdida < 0, 1, 0)),
                                     Alcanza_Tope = sum(ifelse(Monto_Cubierto > Suma_Asegurada, 1, 0)),
                                     No_Deducible = sum(ifelse(Monto_Cubierto < Deducible, 1, 0)))]
            estadisticas
      })
      return(t(res))
}

# Simulación de 1000 escenarios
E1 <- simu(1000)

# Formato data.table
E2 <- data.table(Ganancia_Perdida = unlist(E1[,"Ganancia_Perdida"]))
E2[, Prob_Perdidas := unlist(E1[,"Perdidas"])/1400]
E2[, Prob_Tope := unlist(E1[,"Alcanza_Tope"])/1400]
E2[, Prob_Deducible := unlist(E1[,"No_Deducible"])/1400]

# Visualización de resultados
E2 %>% ggplot(aes(x= Ganancia_Perdida)) + geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = "Distribución de Ganancia/Pérdida", x = "Ganancia/Pérdida", y = "Frecuencia") +
      theme_minimal()

E2 %>% ggplot(aes(x = Prob_Perdidas)) + geom_density(fill = "red", alpha = 0.5) +
      labs(title = "Densidad de Probabilidad de Pérdidas", x = "Probabilidad de Pérdidas", y = "Densidad") +
      theme_minimal()
summary(E2$Prob_Perdidas)
summary(E2$Prob_Tope)
summary(E2$Prob_Deducible)


# Simulación con escenarios en los cuales el deducible se duplica
E3 <- simu(1000, deducible = 240)
E4 <- data.table(Ganancia_Perdida = unlist(E3[,"Ganancia_Perdida"]))
E4 %>% ggplot(aes(x= Ganancia_Perdida)) + geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = "Distribución de Ganancia/Pérdida", x = "Ganancia/Pérdida", y = "Frecuencia") +
      theme_minimal()
summary(E4$Ganancia_Perdida)


# Simulación con escenarios en los cuales el deducible se reduce a la mitad
E5 <- simu(1000, deducible = 60)
E6 <- data.table(Ganancia_Perdida = unlist(E5[,"Ganancia_Perdida"]))
E6 %>% ggplot(aes(x= Ganancia_Perdida)) + geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = "Distribución de Ganancia/Pérdida", x = "Ganancia/Pérdida", y = "Frecuencia") +
      theme_minimal()
summary(E6$Ganancia_Perdida)

