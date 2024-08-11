

## Mapas ##



ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap"),
    tags$style(
      HTML(
        "
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
        .card {
          background-color: rgba(0, 0, 0, 0.5);
          border-radius: 10px;
          padding: 15px;
          margin-top: 20px;
          color: white;
          font-family: 'Poppins', sans-serif;
          text-align: center;
        }
        "
      )
    )
  ),
  titlePanel("Promedios Santiago 2018-2023"),
  fluidRow(column(
    6,
    selectInput(
      "year",
      "Seleccione el año: ",
      choices = 2018:2023,
      selected = 2018
    )
  )),
  fluidRow(column(
    12,
    uiOutput("map_title"),
    leafletOutput("mapa1", height = "590", width = "100%")
  )),
  fluidRow(
    column(4,
           div(
             class = "card",
             h4("Comuna seleccionada:", class = "card-title"),
             htmlOutput("selected_comuna")
           )
    ),
    column(4,
           div(
             class = "card",
             h4("Promedio de la comuna:", class = "card-title"),
             htmlOutput("average_comuna")
           )
    ),
    column(4,
           div(
             class = "card",
             h4("Número de estudiantes:", class = "card-title"),
             htmlOutput("student_count")
           )
    )
  )
)

server <- function(input, output, session) {
  # Variables reactivas para almacenar la comuna y los datos asociados
  selected_comuna <- reactiveVal(NULL)
  selected_promedio <- reactiveVal(NULL)
  selected_n_estudiantes <- reactiveVal(NULL)
  
  # Actualiza el título del mapa basado en el año seleccionado
  output$map_title <- renderUI({
    titulo <- switch(
      as.character(input$year),
      "2018" = "Promedio por comuna 2018",
      "2019" = "Promedio por comuna 2019",
      "2020" = "Promedio por comuna 2020",
      "2021" = "Promedio por comuna 2021",
      "2022" = "Promedio por comuna 2022",
      "2023" = "Promedio por comuna 2023"
    )
    h3(titulo, class = "map-title")
  })
  
  # Renderiza el mapa inicial
  output$mapa1 <- renderLeaflet({
    mapa_santiago2018
  })
  
  # Observa cambios en el slider de años y actualiza el mapa
  observe({
    mapa <- switch(
      as.character(input$year),
      "2018" = mapa_santiago2018,
      "2019" = mapa_santiago2019,
      "2020" = mapa_santiago2020,
      "2021" = mapa_santiago2021,
      "2022" = mapa_santiago2022,
      "2023" = mapa_santiago2023
    )
    
    output$mapa1 <- renderLeaflet({
      mapa
    })
  })
  
  # Observa los clics en el mapa y actualiza el contenido de las tarjetas
  observeEvent(input$mapa1_shape_click, {
    click <- input$mapa1_shape_click
    if (!is.null(click)) {
      comuna <- click$id  # Obtiene el nombre de la comuna del `layerId`
      
      # Encuentra el promedio de la comuna seleccionada según el año
      promedio <- switch(
        as.character(input$year),
        "2018" = comunas_santiago2018$promedio[comunas_santiago2018$nombre_comuna == comuna],
        "2019" = comunas_santiago2019$promedio[comunas_santiago2019$nombre_comuna == comuna],
        "2020" = comunas_santiago2020$promedio[comunas_santiago2020$nombre_comuna == comuna],
        "2021" = comunas_santiago2021$promedio[comunas_santiago2021$nombre_comuna == comuna],
        "2022" = comunas_santiago2022$promedio[comunas_santiago2022$nombre_comuna == comuna],
        "2023" = comunas_santiago2023$promedio[comunas_santiago2023$nombre_comuna == comuna]
      )
      
      n_estudiantes <- switch(
        as.character(input$year),
        "2018" = comunas_santiago2018$n_estudiantes[comunas_santiago2018$nombre_comuna == comuna],
        "2019" = comunas_santiago2019$n_estudiantes[comunas_santiago2019$nombre_comuna == comuna],
        "2020" = comunas_santiago2020$n_estudiantes[comunas_santiago2020$nombre_comuna == comuna],
        "2021" = comunas_santiago2021$n_estudiantes[comunas_santiago2021$nombre_comuna == comuna],
        "2022" = comunas_santiago2022$n_estudiantes[comunas_santiago2022$nombre_comuna == comuna],
        "2023" = comunas_santiago2023$n_estudiantes[comunas_santiago2023$nombre_comuna == comuna]
      )
      
      # Actualiza los valores reactivos
      selected_comuna(comuna)
      selected_promedio(promedio)
      selected_n_estudiantes(n_estudiantes)
    }
  })
  
  # Renderiza el contenido de las tarjetas basado en los valores reactivos
  output$selected_comuna <- renderText({
    paste("Comuna: ", selected_comuna())
  })
  
  output$average_comuna <- renderText({
    paste("Promedio: ", selected_promedio())
  })
  
  output$student_count <- renderText({
    paste("Número de estudiantes: ", selected_n_estudiantes())
  })
  
  # Detiene la aplicación cuando la sesión termina
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
