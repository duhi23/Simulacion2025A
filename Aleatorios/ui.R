
library(shiny)
library(kableExtra)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Generación de números aleatorios - Método congruencial"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("semilla",
                        "Ingrese un valor inicial:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("divisor",
                        "Ingrese el valor de m:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("constante",
                        "Ingrese el valor de a:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("num",
                        "Cantidad de números a generar:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("tabla")
        )
    )
)
