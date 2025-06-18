# Solución Primera Prueba - Semestre 2025A

# EJERCICIO 1
# Implementación del Método de los Cuadrados Medios ----
medios <- function(x0, k){ # Función que recibe una semilla y la cantidad de dígitos (par) a considerar
      return(floor((x0^2 - floor(x0^2/10^(2*k - k/2))*10^(2*k - k/2))/10^(k/2)))
}

c_medios <- function(x0, k, n){ # Función que recibe una semilla, la cantidad de dígitos (par) a considerar y la cantidad de números pseudolaeatorios
      x <- numeric(n)
      for(i in 1:n){
            if (i == 1) {
                  x[i] <- medios(x0, k)
            } else {
                  x[i] <- medios(x[i - 1], k)
            }
      }
      return(x/10^k)
}

medios(1223, 4)
c_medios(1223, 4, 50)

# Aproximación de la integral empleando el método de MonteCarlo ----
fx <- function(x){ # Función Original
      return(3*x*(1+x^2)^(-2))
}

plot(seq(pi,10, by=1), fx(seq(pi,10, by=1)), type = "l", col = "red") # La función tiende a cero a medida que x aumenta

hy <- function(y){ # Función transformada
      a <- pi
      b <- 10
      return((b-a)*fx(a + (b-a)*y))
}

mean(hy(c_medios(1223, 4, 100))) # Evaluación de la función transformada
integrate(f = fx, lower = pi, upper = 100) # Evaluación de la integral original

# Aplicación del Método de la Transformada Inversa ----
f_inv <- function(x){
      return(sqrt(2*x + 0.25) - 0.5)
}

hist(f_inv(c_medios(1223, 4, 100)), freq = FALSE)
curve(f_inv, add = TRUE, col = "red", lwd = 2)


# Método de aceptación y rechazo ----
f_ar <- function(x){
      return((2/sqrt(pi))*sqrt(x)*exp(-x))
}

g_ar <- function(x, lambda){
      return(lambda*exp(-lambda*x))
}

h_ar <- function(x, lambda){
      return(ifelse(g_ar(x, lambda) == 0, Inf, f_ar(x)/g_ar(x, lambda)))
}


# Optimización parámetro lambda
f_lambda <- function(vlambda){
      c <- optimize(h_ar, interval = c(0, 10), lambda = vlambda, maximum = TRUE)$objective
      Y <- (-1/vlambda)*log(runif(1000))
      U <- runif(1000)
      X <- ifelse(U <= f_ar(Y)/(c*g_ar(Y, vlambda)), Y, NA)
      return(sum(!is.na(X))/1000) # Porcentaje de aceptación
}

plot(seq(0.01, 1, by=0.01), sapply(seq(0.01, 1, by=0.01), function(x){f_lambda(x)}), type = "l", col = "blue")

f_var <- function(n){
      vlambda <- 2/3
      c <- optimize(h_ar, interval = c(0, 10), lambda = vlambda, maximum = TRUE)$objective
      Y <- (-1/vlambda)*log(runif(n))
      U <- runif(n)
      X <- ifelse(U <= f_ar(Y)/(c*g_ar(Y, vlambda)), Y, NA)
      return(X)
}

f_var(100)

# EJERCICIO 2

# n: número de pólizas
monto <- function(n=1){
      return(sample(c(500, 1000, 2500, 5000), size = n, replace = TRUE, prob = c(0.35, 0.30, 0.25, 0.10)))
}

prima <- function(n){
      return(sample(c(200, 400, 600, 800, 1000), size = n, replace = TRUE, prob = c(0.04, 0.42, 0.29, 0.18, 0.07)))
}
siniestro <- function(n, casos){
      return(ifelse(1:n %in% sample(1:n, replace = FALSE, size = casos), monto() + 180, 0))
}

siniestro(100, 10)

# Generación de tabla de resultados por mes
Tabla <- function(n, vlambda){
      Siniestros_mensuales <- rpois(n=12, lambda = vlambda)
      res <- data.table(Poliza = 1:n,
                 Prima = prima(n),
                 Sin_Enero = siniestro(n, Siniestros_mensuales[1]),
                 Sin_Febrero = siniestro(n, Siniestros_mensuales[2]),
                 Sin_Marzo = siniestro(n, Siniestros_mensuales[3]),
                 Sin_Abril = siniestro(n, Siniestros_mensuales[4]),
                 Sin_Mayo = siniestro(n, Siniestros_mensuales[5]),
                 Sin_Junio = siniestro(n, Siniestros_mensuales[6]),
                 Sin_Julio = siniestro(n, Siniestros_mensuales[7]),
                 Sin_Agosto = siniestro(n, Siniestros_mensuales[8]),
                 Sin_Septiembre = siniestro(n, Siniestros_mensuales[9]),
                 Sin_Octubre = siniestro(n, Siniestros_mensuales[10]),
                 Sin_Noviembre = siniestro(n, Siniestros_mensuales[11]),
                 Sin_Diciembre = siniestro(n, Siniestros_mensuales[12]))
      return(res)
}

T1 <- Tabla(2800, vlambda = 2.8)
T1[, Total_Siniestros := rowSums(.SD), .SDcols = patterns("^Sin_")]
T1[, Perdida_Ganancia := Prima - Total_Siniestros]
# Resultado económico 2800 pólizas
T1[,list(Perdida_Ganancia = sum(Perdida_Ganancia))]

# Porcentaje de asegurados que generan pérdida
T1[, Marca := ifelse(Perdida_Ganancia < 0, 1, 0)]
T1[,list(Porcentaje = mean(Marca))]


# Ahora se incrementa la tasa de siniestralidad
T2 <- Tabla(2800, vlambda = 5.6)
T2[, Total_Siniestros := rowSums(.SD), .SDcols = patterns("^Sin_")]
T2[, Perdida_Ganancia := Prima - Total_Siniestros]
# Resultado económico 2800 pólizas
T2[,list(Perdida_Ganancia = sum(Perdida_Ganancia))]

# Porcentaje de asegurados que generan pérdida
T2[, Marca := ifelse(Perdida_Ganancia < 0, 1, 0)]
T2[,list(Porcentaje = mean(Marca))]





