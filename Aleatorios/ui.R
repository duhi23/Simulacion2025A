
library(shiny)
library(kableExtra)

# Define UI for application that draws a histogram
fluidPage(
    # Application title
    titlePanel("Generación de números aleatorios - Método congruencial"),
    
    tabsetPanel(
      tabPanel("Nùmeros Aleatorios",
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("semilla",
                        "Ingrese un valor inicial:",
                        min = 1,
                        max = 500,
                        value = 30),
            sliderInput("divisor",
                        "Ingrese el valor de m:",
                        min = 1,
                        max = 500,
                        value = 37),
            sliderInput("constante",
                        "Ingrese el valor de a:",
                        min = 1,
                        max = 500,
                        value = 123),
            sliderInput("num",
                        "Cantidad de números a generar:",
                        min = 1,
                        max = 200,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h4("Tabla de resultados - Método Congruencial Multiplicativo:"),
          br(),
            tableOutput("tabla"),
          br(),
          h4("Tabla de resultados - Método Congruencial Mixto:"),
          br(),
          br(), # Aqui va la segunda tabla
          br(),
          h4("Gráfico:"),
          fluidRow(
            column(width = 2,
                   numericInput("barras", "Número de barras:", value = 10, min = 2, max = 20),
                   ),
            column(width = 5,
                   h5("Método 1")
                   ),
            column(width = 5,
                   h5("Método 2")
                   )
          )
          
        )
    )
    ),
    tabPanel("Resultados",
             h1("Aqui se muestran los resultados")
             ),
    tabPanel("Integrales",
             sidebarLayout(
               sidebarPanel(
                 textInput("funcion", "Ingrese la funciòn a integrar:", value = "1-x"),
                 numericInput("lim_inf", "Límite inferior del intervalo:", value = 0),
                 numericInput("lim_sup", "Límite superior del intervalo:", value = 1),
                 radioButtons("metodo", "Seleccione el mètodo para generar los nùmeros aleatorios:", 
                              c("Congruencial Multiplicativo", "Congruencial Mixto"))
               ),
               mainPanel(
                 h4("Gráfica de la funcion a integrar:"),
                 plotOutput("graf_fun01"),
                 h4("Aproximaciòn:"),
                 plotOutput("graf_aprox01")
               )
             )
             )
    )
)
