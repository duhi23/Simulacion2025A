library(shiny)
library(kableExtra)
library(data.table)

setwd("/cloud/project/Aleatorios")
source("fun_auxiliares.R")

# Funciòn que genera numeros aleatorios bajo el método congruencial mutiplicativo
random_cong <- function(a, m, x0, n){
  res <- numeric(n+1)
  res[1] <- x0
  for(k in 2:length(res)){
    res[k] <- (a*res[k-1]) %% m
  }
  return(res[-1]/m)
}

# Funciòn que genera numeros aleatorios bajo el método congruencial mixto
random_mixt <- function(a, c, m, x0, n){
  res <- numeric(n+1)
  res[1] <- x0
  for(k in 2:length(res)){
    res[k] <- (a*res[k-1] + c) %% m
  }
  return(round((res[-1])/m,6))
}

conv_matrix <- function(vector, cols=10){
  res <- rep(NA_real_, ceiling(length(vector)/cols)*cols)
  res[1:length(vector)] <- vector
  res <- as.data.frame(matrix(res, nrow = ceiling(length(vector)/cols), ncol = cols, byrow = TRUE))
  colnames(res) <- paste0("Cols", 1:cols)
  return(res)
}

conv_matrix(c(1,2,3,4,5,6,7,8), cols = 5)


# Define server logic required to draw a histogram
function(input, output, session) {
  
  coords <- eventReactive(input$calcular, {
    x_vals <- seq(input$lim_inf, input$lim_sup, length.out = 100)
    y_vals <- sapply(x_vals, function(x){eval(parse(text = input$funcion))})
    coords <- data.frame(x = x_vals, y = y_vals)
    })

    output$tabla <- function(){
      
      res <- conv_matrix(random_cong(a = input$constante, m = input$divisor, x0 = input$semilla, n = input$num))
      
      kbl(res, booktabs = TRUE, escape = FALSE) %>% 
        kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
        row_spec(0, background = "#1D3889", color = "#ffffff") %>% scroll_box(width = "100%", height = "200px")
    }
    
    output$graf_fun01 <- renderPlot({
      f <- function(x) eval(parse(text = input$funcion))
      datos <- coords()
      delta_x <- (input$lim_sup - input$lim_inf) / (length(datos$x) - 1)
      area <- (delta_x / 2) * (f(input$lim_inf) + 2 * sum(f(datos$x[2:(length(datos$x) - 1)])) + f(input$lim_sup))
      
      datos %>% ggplot(aes(x = x, y = y)) + geom_line(color = "blue", linewidth = 1) +
        geom_area(mapping = aes(x = x, y = y), fill = "lightblue", alpha = 0.3) +
        geom_vline(xintercept = input$lim_inf, linetype = "dashed", color = "red") +
        geom_vline(xintercept = input$lim_sup, linetype = "dashed", color = "red") +
        labs(title = paste("Área bajo la curva de f(x) =", input$funcion),
             subtitle = paste("Intervalo:", input$lim_inf, "a", input$lim_sup),
             x = "x",
             y = "f(x)",
             caption = paste("Área aproximada:", round(area, 4))
        ) + theme_minimal()
    })
    
    output$graf_aprox01 <- renderPlot({
      secuencia <- seq(100, 10000, by = 250)
      f <- function(x) eval(parse(text = input$funcion))
      
      if(input$lim_inf == 0 & input$lim_sup == 1){
        data.table(Aproximacion = sapply(secuencia, function(k){
          res <- random_cong(a = 7^5, m = 2^31 - 1, x0 = as.numeric(now()), n = k)
          mean(sapply(res, function(x){eval(parse(text = input$funcion))}))
        }), 
        Teorico = integrate(f, lower=0, upper = 1)$value
        ) %>% gather(key = "Etiqueta", value = "Valor") %>% mutate(Aleatorios = c(secuencia, secuencia)) %>% 
          ggplot(aes(x=Aleatorios, y = Valor, group = Etiqueta, , colour = Etiqueta)) + geom_line()
      } else if(input$lim_inf !=0 | input$lim_sup !=1){
        data.table(Aproximacion = sapply(secuencia, function(k){
          res <- input$lim_inf + (input$lim_sup - input$lim_inf)*random_cong(a = 7^5, m = 2^31 - 1, x0 = as.numeric(now()), n = k)
          mean(sapply(res, function(x){(input$lim_sup - input$lim_inf)*eval(parse(text = input$funcion))}))
        }), 
        Teorico = integrate(f, lower=input$lim_inf, upper = input$lim_sup)$value
        ) %>% gather(key = "Etiqueta", value = "Valor") %>% mutate(Aleatorios = c(secuencia, secuencia)) %>% 
          ggplot(aes(x=Aleatorios, y = Valor, group = Etiqueta, , colour = Etiqueta)) + geom_line()
      }
      
      
    })

}
