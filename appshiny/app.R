##App de shiny, este script es el que se ejecuta para correr la aplicación##
#################

if (!require("summarytools")) install.packages("summarytools")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dplyr")) install.packages("kableExtra")
if (!require("tidyr")) install.packages("tidyr")
if (!require("janitor")) install.packages("janitor")
if (!require("sjmisc")) install.packages("sjmisc")
if (!require("sjPlot")) install.packages("sjPlot")
if (!require("httpgd")) install.packages("httpgd")
if (!require("devtools")) install.packages("devtools")
if (!require("shiny")) install.packages("shiny")
if (!require("bslib")) install.packages("bslib")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggThemeAssist")) install.packages("ggThemeAssist")
if (!require("leaflet")) install.packages("leaflet")
if (!require("stringr")) install.packages("stringr")
if (!require("sf")) install.packages("sf")
if (!require("chilemapas")) install.packages("chilemapas")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("plotly")) install.packages("plotly")


library(summarytools)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(tidyr)
library(janitor)
library(sjmisc)
library(sjPlot)
library(httpgd)
library(devtools)
library(shiny)
library(bslib)
library(ggplot2)
library(ggThemeAssist)
library(stringr)
library(leaflet)
library(sf)
library(chilemapas)
library(shinydashboard)
library(plotly)

###Particularidad.r####



#######

establecimiento2018 <- read_csv("establecimiento2018.csv")
establecimiento2019 <- read_csv("establecimiento2019.csv")
establecimiento2020 <- read_csv("establecimiento2020.csv")
establecimiento2021 <- read_csv("establecimiento2021.csv")
establecimiento2022 <- read_csv("establecimiento2022.csv")
establecimiento2023 <- read_csv("establecimiento2023.csv")

establecimientos_total <- list(establecimiento2018, establecimiento2019, establecimiento2020, establecimiento2021, establecimiento2022, establecimiento2023)

###Función para hacer que las comunas estén bien#####

capitalize <- function(text) {
  # Divide el texto en palabras
  words <- unlist(strsplit(text, " "))
  
  # Capitaliza la primera letra de cada palabra y une las palabras
  capitalize <- paste(toupper(substring(words, 1, 1)), 
                      tolower(substring(words, 2)), 
                      sep = "", 
                      collapse = " ")
  
  return(capitalize)
}




#Función para hacer graficos interactivos##

comunas <- unique(establecimientos_total[1]$codigo_comuna)

grafico_interactivo <- function(comuna, year_index){
  
  df_comuna <- establecimientos_total[[year_index]]
  
  df_comuna <- df_comuna %>% 
    filter(codigo_comuna == comuna)
  
  total_estudiantes <- sum(df_comuna$n_estudiantes)
  
  df_comuna <- df_comuna %>%
    mutate(porcentaje = (n_estudiantes / total_estudiantes) * 100)
  
  
  nombre_comuna <- unique(df_comuna$nombre_comuna)
  
  nombre_comuna <- capitalize(nombre_comuna)
  
  plot_ly(df_comuna, labels = ~TipoEstablecimiento, values = ~n_estudiantes, type = 'pie', hole = 0.6, width = 410, height = 260, # Aumentar tamaño para mayor control
          hoverinfo = 'label+text',
          text = ~paste("Promedio de notas:", as.integer(round(promedio, 1))),
          textinfo = 'percent',
          marker = list(colors = c("#C70319", "#2C0DBB", "#6F0976", "#B10530", "#85085E"))
  ) %>%
    layout(
      showlegend = FALSE, # Mostrar la leyenda
      paper_bgcolor = 'rgba(0,0,0,0)', # Fondo del área del gráfico
      plot_bgcolor = 'rgba(0,0,0,0)',  # Fondo del gráfico
      font = list(color = "white"),
      autosize = TRUE,
      margin = list(l = 50, r = 50, t = 20, b = 70), # Ajustar márgenes para centrar el gráfico
      legend = list(
        x = 0.5,           # Centrar la leyenda horizontalmente
        y = 0.5,           # Centrar la leyenda verticalmente
        xanchor = 'center',# Fijar la posición horizontal de la leyenda
        yanchor = 'middle',
        font = list(size = 12, color = "white") # Tamaño y color de la leyenda
      )
    )
  
  
}
#Aqui solo hay que poner el codigo de la comuna



#######


comunas_santiago2018 <- read_sf("comunas_santiago2018.gpkg")
comunas_santiago2019 <- read_sf("comunas_santiago2019.gpkg")
comunas_santiago2020 <- read_sf("comunas_santiago2020.gpkg")
comunas_santiago2021 <- read_sf("comunas_santiago2021.gpkg")
comunas_santiago2022 <- read_sf("comunas_santiago2022.gpkg")
comunas_santiago2023 <- read_sf("comunas_santiago2023.gpkg")





bins <- c(40, 45, 50, 52, 54, 56, 58, 60, 62, 64, 65)

paleta <- c(
  "#DD0202", 
  "#C70319", 
  "#B10530", 
  "#9B0647", 
  "#85085E", 
  "#6F0976", 
  "#580A8D",
  "#420CA4", 
  "#2C0DBB", 
  "#160FD2", 
  "#0010E9")



pal <- colorBin(
  palette = paleta,
  domain = c(40, 65),
  bins = bins)



###2018###


mapa_santiago2018 <- leaflet(comunas_santiago2018) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2018$promedio),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    layerId = ~nombre_comuna,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2018$nombre_comuna, "", "Promedio: ", comunas_santiago2018$promedio)
  )


###2019###

mapa_santiago2019 <- leaflet(comunas_santiago2019) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2019$promedio),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    layerId = ~nombre_comuna,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2019$nombre_comuna, "", "Promedio: ", comunas_santiago2019$promedio)
  )

###2020###

mapa_santiago2020 <- leaflet(comunas_santiago2020) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2020$promedio),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    layerId = ~nombre_comuna,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2020$nombre_comuna, "", "Promedio: ", comunas_santiago2020$promedio)
  )

###2021###

mapa_santiago2021 <- leaflet(comunas_santiago2021) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2021$promedio),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    layerId = ~nombre_comuna,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2021$nombre_comuna, "", "Promedio: ", comunas_santiago2021$promedio)
  )

###2022###


mapa_santiago2022 <- leaflet(comunas_santiago2022) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2022$promedio),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    layerId = ~nombre_comuna,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2022$nombre_comuna, "", "Promedio: ", comunas_santiago2022$promedio)
  )

###2023###

mapa_santiago2023 <- leaflet(comunas_santiago2023) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2023$promedio),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    layerId = ~nombre_comuna,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "white",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2023$nombre_comuna, "", "Promedio: ", comunas_santiago2023$promedio)
  )




######################################


###app shiny más abajo###



ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap"),
    tags$style(
      HTML(
        "
        body {
          background: rgb(221,2,2);
          background: linear-gradient(125deg, rgba(221,2,2,1) 7%, rgba(0,16,233,1) 76%);
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
        .card2 {
          background-color: rgba(243, 198, 221, 0.8);
          border-radius: 10px;
          padding: 15px;
          margin-top: 20px;
          color: white;
          font-family: 'Poppins', sans-serif;
          text-align: center;
        }
        .highlighted-subtitle {
          color: white;
          background-color: rgba(0, 0, 0, 0.5); 
          padding: 5px 10px;
          border-radius: 5px;
          display: inline-block;
          font-size: 14px;
          font-weight: 400;
          border-radius: 10px;
        }
        .title {
          color: white;
          height: 40px;
          background-color: rgba(0, 0, 0, 0.5);
          font-family: 'Poppins', sans-serif;
          font-size: 24px;
          font-weight: 400;
          position: relative;
          line-height: 40px;
          margin-top: 30px;
          border-radius: 10px;
          padding: 0 10px;
        }
        .text {
          color: white;
          font-family: 'Poppins', sans-serif;
          text-align: center;
          font-size: 16px;
          font-weight: 300;
          background-color: rgba(0, 0, 0, 0.5);
          margin-top: 10px;
          border-radius: 10px;
        }
        .text2 {
        font-family: 'Poppins', sans-serif;
        text-align: center;
        }
        .plot-container {
          text-align: center;
        }
        .sidebarpanel {
        width: 200px;
        }
        .custom-selector {
          align-items: center;
          width: 150px;
          margin: 0 auto; 
        }
        "
      )
    )
  ),
  titlePanel((div(class = "title", "Promedios Gran Santiago 2018-2023"))
  ),
  column(12,
         p(div(class = "highlighted-subtitle",
               "Los siguientes mapas muestran el promedio final de estudiantes entre primer básico y quinto básico del Gran Santiago entre los años 2018 y 2023. Fuente: Sistema de Información General de Estudiantes (SIGE)"))),
  sidebarPanel(width = 2,
      fluidRow(div(class = "plot-container", div(class = "custom-selector", selectInput(
                              "year",
                              "Seleccione el año: ",
                              choices = 2018:2023,
                              selected = 2018
                     )))),
      div(class = "text2",
                          p("Haga clic en el selector de años para seleccionar el año de interés"),
                          p(""),
                          br(),
                          br(),
                          p("Haga clic en una comuna para ver los datos sobre el rendimiento académico promedio")
                   )),
  card(
    fluidRow(
      column(4,
             div(
               class = "card",
               h4("Comuna", class = "card-title"),
               htmlOutput("selected_comuna")
             )
      ),
      column(4,
             div(
               class = "card",
               h4("Promedio", class = "card-title"),
               htmlOutput("average_comuna")
             )
      ),
      column(4,
             div(
               class = "card",
               h4("Estudiantes", class = "card-title"),
               htmlOutput("student_count")
             )
      ),
      column(8,
             uiOutput("map_title"),
             leafletOutput("mapa1", height = "500", width = "100%")),
      column(4,
             div(
               class = "card",
               h4("Tipos de establecimientos", class = "card-title"),
               plotlyOutput("grafico_interactivo", height = "200px", width = "100%"))))))
  



server <- function(input, output, session) {
  # Variables reactivas para almacenar la comuna y los datos asociados
  selected_comuna <- reactiveVal(NULL)
  selected_promedio <- reactiveVal(NULL)
  selected_n_estudiantes <- reactiveVal(NULL)
  selected_codigo_comuna <- reactiveVal(NULL)
  
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
      comuna <- click$id  
      
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
      
      codigo_comuna <- switch(
        as.character(input$year),
        "2018" = comunas_santiago2018$codigo_comuna[comunas_santiago2018$nombre_comuna == comuna],
        "2019" = comunas_santiago2019$codigo_comuna[comunas_santiago2019$nombre_comuna == comuna],
        "2020" = comunas_santiago2020$codigo_comuna[comunas_santiago2020$nombre_comuna == comuna],
        "2021" = comunas_santiago2021$codigo_comuna[comunas_santiago2021$nombre_comuna == comuna],
        "2022" = comunas_santiago2022$codigo_comuna[comunas_santiago2022$nombre_comuna == comuna],
        "2023" = comunas_santiago2023$codigo_comuna[comunas_santiago2023$nombre_comuna == comuna]
      )
      
      # Actualiza los valores reactivos
      selected_comuna(comuna)
      selected_promedio(promedio)
      selected_n_estudiantes(n_estudiantes)
      selected_codigo_comuna(codigo_comuna)
    }
  })
  

  output$grafico_interactivo <- renderPlotly({
    req(selected_codigo_comuna())
    year_index <- as.numeric(input$year) - 2017
    grafico_interactivo(selected_codigo_comuna(), year_index)
  })
  
  # Renderiza el contenido de las tarjetas basado en los valores reactivos
  output$selected_comuna <- renderText({
    paste(div(class = "text", selected_comuna()))
  })
  
  output$average_comuna <- renderText({
    paste(div(class = "text", selected_promedio()))
  })
  
  output$student_count <- renderText({
    paste(div(class = "text", selected_n_estudiantes()))
  })
  
  # Detiene la aplicación cuando la sesión termina
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)

 
      