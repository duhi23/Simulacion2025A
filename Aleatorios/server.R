library(shiny)
library(kableExtra)
library(data.table)

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

    output$tabla <- function(){
      
      res <- conv_matrix(random_cong(a = input$constante, m = input$divisor, x0 = input$semilla, n = input$num))
      
      kbl(res, booktabs = TRUE, escape = FALSE) %>% 
        kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
        row_spec(0, background = "#1D3889", color = "#ffffff") %>% scroll_box(width = "100%", height = "200px")
    }
    
    output$graf_aprox01 <- renderPlot({
      secuencia <- seq(100, 10000, by = 200)
      f <- function(x) eval(parse(text = input$funcion))
      
      data.table(Aproximacion = sapply(secuencia, function(k){
        res <- random_cong(a = 7^5, m = 2^31 - 1, x0 = as.numeric(now()), n = k)
        mean(sapply(res, function(x){eval(parse(text = input$funcion))}))
      }), 
      Teorico = integrate(f, lower=0, upper = 1)$value
      ) %>% gather(key = "Etiqueta", value = "Valor") %>% mutate(Aleatorios = c(secuencia, secuencia)) %>% 
        ggplot(aes(x=Aleatorios, y = Valor, group = Etiqueta, , colour = Etiqueta)) + geom_line()
      
    })

}
