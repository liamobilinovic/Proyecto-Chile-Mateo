library(shiny)
library(leaflet)
library(bslib)


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap"),
    tags$style(HTML("
      body {
        background: rgb(254,78,124);
        background: linear-gradient(90deg, rgba(254,78,124,1) 19%, rgba(82,92,208,1) 100%);
        color: white;
        font-family: 'Poppins', sans-serif;
      }
      .well {
        background-color: #2C11DD; /* Fondo del panel lateral */
        padding: 10px;
        border-radius: 10px;
        color: white;
      }
      .leaflet-container {
        background: rgba(255, 255, 255, 0.1); /* Fondo del contenedor del mapa con opacidad */
        border-radius: 10px;
      }
      .map-title {
        color: white;
        font-family: 'Poppins', sans-serif;
        text-align: center;
        margin-top: 20px;
      }
      .value-box {
        color: white;
        font-family: 'Poppins', sans-serif;
      }
    ")),
  ),
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
      leafletOutput("mapa1")
        )
      ),
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
    h3(titulo, class = "map-title")
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
