# Procesos de Poisson

# Ejercicio 22

TT <- 1

fun_poisson <- function(lambda, TT){
  t <- 0
  I <- 0
  S <- 0
  aficionados <- c()
  llegadas <- c()
  while(t < TT){
    Xi <- (-1/lambda)*log(runif(1)) # Tiempo de arribo de cada autobús
    Ai <- sample(20:40, size = 1) # Número de aficionados por autobús
    t <- t + Xi
    if(t > TT){
      break
    }
    llegadas <- c(llegadas, Xi)
    aficionados <- c(aficionados, Ai)
    I <- I + 1
    S <- t
  }
  return(list(NAutobuses = I, Tiempos = llegadas, Aficionado = aficionados))
}

R1 <- system.time(fun_poisson(lambda = 5, TT = 2400))

plot(cumsum(R1$Tiempos), 0:(length(R1$Tiempos)-1), type = "s", col = "red",
     xlab = "Tiempos de arribo", ylab = "Número de Autobuses")

plot(c(0,cumsum(R1$Tiempos)), c(0,cumsum(R1$Aficionado)), type = "s", col = "red",
     xlab = "Tiempos de arribo", ylab = "Número de Aficionads")



fun_poisson2 <- function(vlambda, TT){
  NT <- rpois(1, lambda = vlambda*TT)
  Ai <- sample(20:40, size = NT, replace = TRUE) # Número de aficionados por autobús
  U <- runif(NT)
  Ti <- sort(U*TT)
  return(list(NAutobuses = NT, Tiempos = diff(c(0,Ti)), Aficionado = Ai))
}

R2 <- system.time(fun_poisson2(vlambda = 5, TT=2400))
plot(cumsum(R2$Tiempos), 0:(length(R2$Tiempos)-1), type = "s", col = "red",
     xlab = "Tiempos de arribo", ylab = "Número de Autobuses")

plot(c(0,cumsum(R2$Tiempos)), c(0,cumsum(R2$Aficionado)), type = "s", col = "red",
     xlab = "Tiempos de arribo", ylab = "Número de Aficionads")


# Proceso Poisson No Homogéneo
lambda_t <- function(t){
  return(3 + 4/(t+1))
}

plot(seq(0,10, by = 0.01), lambda_t(seq(0,10, by = 0.01)), type = "l", col = "red")
abline(h=7, col="blue")
abline(v=1, col="orange")
abline(h=5, col="orange")
abline(v=3, col="purple")
abline(h=4, col="purple")

fun_poisson3 <- function(TT){
  lambda <- 7
  t <- 0
  I <- 0
  S <- 0
  R <- 0
  tiempos <- c()
  while(t < TT){
    Xi <- (-1/lambda)*log(runif(1)) # Tiempo de arribo
    t <- t + Xi
    if(t > TT){
      break
    }
    if(runif(1) <= lambda_t(t)/lambda){
      I <- I + 1
      S <- t
      tiempos <- c(tiempos, S)
    } else {
      R <- R + 1
    }
  }
  return(list(Eventos = I, Tiempos = tiempos, Rechazos = R))
}

R4 <- fun_poisson3(TT=1000)
# Porcentaje de rechazo
3994/(length(R4$Tiempos) + 3994)

plot(R4$Tiempos, 0:(length(R4$Tiempos)-1), type = "s", col = "red",
     xlab = "Tiempos de arribo", ylab = "Número de Eventos")


# Proceso de Poisson no homogéneo

subintervalos <- seq(0,100, by = 0.5)#c(0,1,3,10)

poisson_no_homogeneo <- function(lambda_t, subintervalos){ # parámetros: TT tiempo de exposición, lambda_t la tasa de crecimiento
  TT <- max(subintervalos)
  nvar <- 0
  t <- 0
  J <- 1
  I <- 0
  S <- c()
  k <- length(subintervalos)-1 # Número de subintervalos
  lambdas <- numeric(k)
  for(i in 1:k){
    lambdas[i] <- ceiling(optimise(f=lambda_t, interval = c(subintervalos[i], subintervalos[i+1]), maximum = TRUE)$objective)
  }
  repeat{
    nvar <- nvar +1
    # Paso 2
    X <- -log(runif(1))/lambdas[J]
    # Paso 3
    if(t + X > TT){break}
    # Paso 4
    t <- t + X
    # Paso 5 y 6
    if(runif(1) <= lambda_t(t)/lambdas[J]){
      I <- I + 1
      S[I] <- t
    }
    # Paso 7 : volver al paso 2
    repeat{
      if(t + X <= subintervalos[J+1] || J == k){break} # aún dentro del subintervalo
      
      # Paso 8: si J = k (último subintervalo), terminar
      if(J == k) return(S)
      
      # Paso 9: avanzar al siguiente subintervalo
      X <- (X - (subintervalos[J+1] -t))*lambdas[J]/lambdas[J+1]
      t <- subintervalos[J+1]
      J <- J + 1
    }
     # Paso 10: ir al paso 3 (dentro del bucle)
  }
  cat("Número de variables generadas: ", nvar, "\n")
  return(S)
}

res <- poisson_no_homogeneo(lambda_t, subintervalos)
length(res)
# Porcentaje de rechazo
1- 220/276

plot(res, 1:length(res), type = "s", col = "red", xlab = "Tiempos de arribo",
     ylab = "Eventos")
abline(v=50, col = "blue")
vline(h=)














