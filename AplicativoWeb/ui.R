library(shiny)

# Define UI for application that draws a histogram
fluidPage(
    tags$style("h1 {color: #9A9A9A; font-size:35px}"),
    tags$style("h2 {color: #A569BD; font-size:25px}"),
  fluidRow(column(width = 3, tags$img(src="EPN_logo.png", width = "60px", height = "60px")),
           column(width = 9, h1("Primer aplicativo Simulación", style = "text-align:center"))),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("tipo", "Seleccione el tipo de variables a analizar:", 
                         choiceNames = c("Cuantitativas", "Cualitativas"),
                         choiceValues = c("numeric", "character"), selected = "numeric"),
            selectInput("var", "Variable seleccionada",
                        choices = tvar$Variable, selected = tvar$Variable[2])
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h2("Resumen:"),
            verbatimTextOutput("resumen"),
          h2("Gráfico"),
            plotOutput("grafico", width = "350px")
        )
    )
)
