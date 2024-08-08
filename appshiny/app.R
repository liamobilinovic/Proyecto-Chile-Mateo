library(shiny)
library(leaflet)

# Define la interfaz de usuario
ui <- fluidPage(
  includeCSS("/Users/liam/Desktop/Proyecto DARA/paleta.css"),
  titlePanel("Promedios Santiago 2018-2023"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Seleccione el año: ",
        choices = 2018:2023,
        selected = 2018
      )
    ),
    mainPanel(
      uiOutput("map_title"),
      leafletOutput("mapa1")  # Contenedor del mapa
    )
  )
)


server <- function(input, output, session) {
  
  
  output$map_title <- renderUI({
    titulo <- switch(as.character(input$year),
                     "2018" = "Promedio por comuna 2018",
                     "2019" = "Promedio por comuna 2019",
                     "2020" = "Promedio por comuna 2020",
                     "2021" = "Promedio por comuna 2021", 
                     "2022" = "Promedio por comuna 2022",
                     "2023" = "Promedio por comuna 2023")
    h3(titulo)
  })
  

  output$mapa1 <- renderLeaflet({
    mapa_santiago2018  
  })
  
  # Observa cambios en el slider de años y actualiza el mapa
  observe({
    # Selecciona el mapa adecuado según el año elegido
    mapa <- switch(as.character(input$year),
                   "2018" = mapa_santiago2018,
                   "2019" = mapa_santiago2019,
                   "2020" = mapa_santiago2020,
                   "2021" = mapa_santiago2021,
                   "2022" = mapa_santiago2022,
                   "2023" = mapa_santiago2023)
    
    # Renderiza de nuevo el mapa completo cada vez que se cambia el año
    output$mapa1 <- renderLeaflet({
      mapa  # Renderiza el mapa seleccionado
    })
  })
  
  # Detiene la aplicación cuando la sesión termina
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Lanza la aplicación
shinyApp(ui, server)
