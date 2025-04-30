library(shiny)
library(tidyverse)

# Carga de datos
load("./data/Info.RData")

# Ajuste de formato fecha
datos[, fecha_corte:= ymd(fecha_corte)]
datos[, fecha_nacimiento := ymd(fecha_nacimiento)]

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Filtro de variables por su tipo
  tvar <- datos |> map_chr(class) 
  tvar <- data.table(Variable = names(tvar) , Tipo = unname(tvar))
  
  observe({
    updateSelectInput(session, "var", choices = tvar[Tipo == input$tipo])
  })

  output$resumen <- renderPrint({
    datos %>% dplyr::select(input$var) %>% summary(.)
  })
  
  output$grafico <- renderPlot({
    if(input$tipo == "numeric"){
      datos %>% dplyr::select(input$var) %>% pull(.) %>% hist(.)
    } else {
      datos %>% dplyr::select(input$var) %>% table(.) %>% barplot(.)
    }
  })

}
