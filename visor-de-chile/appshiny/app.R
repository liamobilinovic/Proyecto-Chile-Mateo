### Visor de Chile###


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
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("scales")) install.packages("scales")
if (!require("extrafont")) install.packages("extrafont")
if (!require("showtext")) install.packages("showtext")


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
library(shinyWidgets)
library(scales)
library(extrafont)
library(showtext)
library(shinycssloaders)



###Miscelaneos###


nombres_regiones <- c(
  "Región de Arica y Parinacota" = "15",
  "Región de Tarapacá" = "01",
  "Región de Antofagasta" = "02",
  "Región de Atacama" = "03",
  "Región de Coquimbo" = "04",
  "Región de Valparaíso" = "05",
  "Región Metropolitana de Santiago" = "13",
  "Región del Libertador General Bernardo O'Higgins" = "06",
  "Región del Maule" = "07",
  "Región de Ñuble" = "16",
  "Región del Biobío" = "08",
  "Región de La Araucanía" = "09",
  "Región de Los Ríos" = "14",
  "Región de Los Lagos" = "10",
  "Región de Aysén del General Carlos Ibáñez del Campo" = "11",
  "Región de Magallanes y de la Antártica Chilena" = "12"
)

paleta <- c("#EC5A25", "#D7522D", "#C14936", "#AC413E", "#963846", "#81304F", "#6B2757", "#551E5F", "#401667", "#2B0E70", "#150578")

paleta_g <- c("#EC5A25", "#2B0E70", "#6B2757", "#AC413E", "#150578", "#C70319", "#2C0DBB", "#6F0976")

capitalize <- function(text) {
  # Divide el texto en palabras
  words <- unlist(strsplit(text, " "))
  
  # Capitaliza la primera letra de cada palabra y une las palabras
  capitalize <- paste(toupper(substring(words, 1, 1)),
                      tolower(substring(words, 2)),
                      sep = "",
                      collapse = " "
  )
  
  return(capitalize)
}

bins <- c(4.0, 4.5, 5.0, 5.2, 5.4, 5.6, 5.8, 6.0, 6.2, 6.4, 6.5)


paleta <- c("#EC5A25", "#D7522D", "#C14936", "#AC413E", "#963846", "#81304F", "#6B2757", "#551E5F", "#401667", "#2B0E70", "#150578")

pal <- colorBin(
  palette = paleta,
  domain = c(4.0, 6.5),
  bins = bins
)

######### 
#Funcion de mapas###


comunas_chile2018 <- read_sf("Datos-proyecto/comunas_chile2018.gpkg")
comunas_chile2019 <- read_sf("Datos-proyecto/comunas_chile2019.gpkg")
comunas_chile2020 <- read_sf("Datos-proyecto/comunas_chile2020.gpkg")
comunas_chile2021 <- read_sf("Datos-proyecto/comunas_chile2021.gpkg")
comunas_chile2022 <- read_sf("Datos-proyecto/comunas_chile2022.gpkg")
comunas_chile2023 <- read_sf("Datos-proyecto/comunas_chile2023.gpkg")

comunas_chile2018$codigo_comuna <- as.integer(comunas_chile2018$codigo_comuna)
comunas_chile2019$codigo_comuna <- as.integer(comunas_chile2019$codigo_comuna)
comunas_chile2020$codigo_comuna <- as.integer(comunas_chile2020$codigo_comuna)
comunas_chile2021$codigo_comuna <- as.integer(comunas_chile2021$codigo_comuna)
comunas_chile2022$codigo_comuna <- as.integer(comunas_chile2022$codigo_comuna)
comunas_chile2023$codigo_comuna <- as.integer(comunas_chile2023$codigo_comuna)


comunas_total <- list(comunas_chile2018, comunas_chile2019, comunas_chile2020, comunas_chile2021, comunas_chile2022, comunas_chile2023)


comunas <- unique(comunas_total[1]$codigo_comuna)

generar_mapa <- function(year_index, region){
  
  df_total <- comunas_total[[year_index]]
  
  df_total <- df_total %>% 
    filter(codigo_region == region)
  
  leaflet(df_total) %>%
    addTiles(
      urlTemplate = "",
      options = tileOptions(background = "white")
    ) %>%
    addPolygons(
      fillColor = ~ pal(df_total$promedio),
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
      label = ~ paste("Comuna: ", nombre_comuna, "", "Promedio: ", promedio)
    ) %>% 
    addLegend(
      pal = pal,
      values = ~ promedio,
      opacity = 0.5,
      title = "Promedio",
      position = "topright"
    )
}



############

#Particularidad Chile

################# Funcion

establecimientos_chile2018 <- read_csv("Datos-proyecto/establecimientos_chile2018.csv")
establecimientos_chile2019 <- read_csv("Datos-proyecto/establecimientos_chile2019.csv")
establecimientos_chile2020 <- read_csv("Datos-proyecto/establecimientos_chile2020.csv")
establecimientos_chile2021 <- read_csv("Datos-proyecto/establecimientos_chile2021.csv")
establecimientos_chile2022 <- read_csv("Datos-proyecto/establecimientos_chile2022.csv")
establecimientos_chile2023 <- read_csv("Datos-proyecto/establecimientos_chile2023.csv")



establecimientos_chile_total <- list(establecimientos_chile2018, establecimientos_chile2019, establecimientos_chile2020, establecimientos_chile2021, establecimientos_chile2022, establecimientos_chile2023)


capitalize <- function(text) {
  # Divide el texto en palabras
  words <- unlist(strsplit(text, " "))
  
  # Capitaliza la primera letra de cada palabra y une las palabras
  capitalize <- paste(toupper(substring(words, 1, 1)),
                      tolower(substring(words, 2)),
                      sep = "",
                      collapse = " "
  )
  
  return(capitalize)
}

comunas <- unique(establecimientos_chile_total[1]$codigo_comuna)

establecimiento_chile <- function(comuna, year_index) {
  df_comuna <- establecimientos_chile_total[[year_index]]
  
  df_comuna <- df_comuna %>%
    filter(codigo_comuna == comuna)
  
  total_estudiantes <- sum(df_comuna$n_estudiantes)
  
  df_comuna <- df_comuna %>%
    mutate(porcentaje = (n_estudiantes / total_estudiantes) * 100)
  
  
  nombre_comuna <- unique(df_comuna$nombre_comuna)
  
  nombre_comuna <- capitalize(nombre_comuna)
  
  plot_ly(df_comuna,
          labels = ~TipoEstablecimiento, values = ~n_estudiantes, type = "pie", hole = 0.6, width = 410, height = 260, # Aumentar tamaño para mayor control
          hoverinfo = "label+text",
          text = ~ paste("Promedio:", promedio, "<br>Estudiantes:", n_estudiantes),
          textinfo = "percent",
          marker = list(colors = c("#EC5A25", "#2B0E70", "#6B2757", "#AC413E", "#150578"))
  ) %>%
    layout(
      showlegend = FALSE, # Mostrar la leyenda
      paper_bgcolor = "rgba(0,0,0,0)", # Fondo del área del gráfico
      plot_bgcolor = "rgba(0,0,0,0)", # Fondo del gráfico
      font = list(color = "white"),
      autosize = TRUE,
      margin = list(l = 50, r = 50, t = 20, b = 70), # Ajustar márgenes para centrar el gráfico
      legend = list(
        x = 0.5, # Centrar la leyenda horizontalmente
        y = 0.5, # Centrar la leyenda verticalmente
        xanchor = "center", # Fijar la posición horizontal de la leyenda
        yanchor = "middle",
        font = list(size = 12, color = "white") # Tamaño y color de la leyenda
      )
    )
}




### Situacion final chile

sfinal_chile2018 <- read_csv("Datos-proyecto/sfinal_chile2018.csv")
sfinal_chile2019 <- read_csv("Datos-proyecto/sfinal_chile2019.csv")
sfinal_chile2020 <- read_csv("Datos-proyecto/sfinal_chile2020.csv")
sfinal_chile2021 <- read_csv("Datos-proyecto/sfinal_chile2021.csv")
sfinal_chile2022 <- read_csv("Datos-proyecto/sfinal_chile2022.csv")
sfinal_chile2023 <- read_csv("Datos-proyecto/sfinal_chile2023.csv")


#### Funcion generadora de graficos

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


sfinal_chile_total <- list(sfinal_chile2018, sfinal_chile2019, sfinal_chile2020, sfinal_chile2021, sfinal_chile2022, sfinal_chile2023)


comunas <- unique(sfinal_chile_total[1]$codigo_comuna)

grafico_sfinal_chile <- function(comuna, year_index){
  
  df_comuna2 <- sfinal_chile_total[[year_index]]
  
  df_comuna2 <- df_comuna2 %>% 
    filter(codigo_comuna == comuna)
  
  total_estudiantes <- sum(df_comuna2$n_estudiantes)
  
  df_comuna2 <- df_comuna2 %>%
    mutate(porcentaje = (n_estudiantes / total_estudiantes) * 100)
  
  
  nombre_comuna <- unique(df_comuna2$nombre_comuna)
  
  nombre_comuna <- capitalize(nombre_comuna)
  
  plot_ly(df_comuna2, labels = ~SituacionFinal, values = ~n_estudiantes, type = 'pie', hole = 0.6, width = 410, height = 260, # Aumentar tamaño para mayor control
          hoverinfo = 'label+text',
          text = ~paste("Estudiantes:", n_estudiantes),
          textinfo = 'percent',
          marker = list(colors = c("#EC5A25", "#2B0E70", "#6B2757", "#AC413E", "#150578"))
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



comunas_total_chile <- list(comunas_chile2018, comunas_chile2019, comunas_chile2020, comunas_chile2021, comunas_chile2022, comunas_chile2023)


nombres_anios <- c("2018", "2019", "2020", "2021", "2022", "2023")

grafico_evolucion_chile <- function(comuna, year_index) {
  
  df_total <- bind_rows(comunas_total_chile, .id = "year_index")
  
  df_total <- df_total %>%
    mutate(anio = nombres_anios[as.numeric(year_index)]) %>%
    filter(codigo_comuna == comuna)
  
  nombre_comuna <- unique(df_total$nombre_comuna)
  nombre_comuna <- capitalize(nombre_comuna)
  
  df_total <- df_total %>%
    mutate(anio = factor(anio, levels = nombres_anios))
  
  ggplot(df_total, aes(x = anio, y = promedio, group = 1)) +
    geom_line(color = "#031163", size = 2) +
    geom_point(color = "#AC413E") +
    geom_text(aes(label = scales::comma(promedio)), vjust = -0.5, color = "#CACFEC", size = 6) +
    labs(title = paste("Evolución del promedio en", nombre_comuna),
         x = "Año",
         
         y = "Promedio") +
    scale_y_continuous(limits = c(1.0, 7.0),
                       breaks = seq(1.0, 7.0, by = 1)) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#000005", color = "#000005"),
      panel.background = element_rect(fill = "#000005", color = "#000005"),
      axis.text = element_text(family = "Arial", color = "#CACFEC", size = 16),
      panel.grid.major = element_line(color = "#0E1E38"),
      panel.grid = element_blank(),
      axis.title = element_text(family = "Arial", color = "#CACFEC", size = 18),
      axis.ticks = element_blank(),
      axis.line = element_line(color = "#CACFEC"),
      plot.title = element_text(family = "Arial", color = "#CACFEC", hjust = 0.5, size = 22),
      legend.position = "none" 
    )
  
}

#####

promedio_rural2018 <- read_csv("Datos-proyecto/promedio_rural2018.csv")
promedio_rural2019 <- read_csv("Datos-proyecto/promedio_rural2019.csv")
promedio_rural2020 <- read_csv("Datos-proyecto/promedio_rural2020.csv")
promedio_rural2021 <- read_csv("Datos-proyecto/promedio_rural2021.csv")
promedio_rural2022 <- read_csv("Datos-proyecto/promedio_rural2022.csv")
promedio_rural2023 <- read_csv("Datos-proyecto/promedio_rural2023.csv")

######################
#Funcion para generar ruralidad


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

ruralidad_total <- list(promedio_rural2018, promedio_rural2019, promedio_rural2020, promedio_rural2021, promedio_rural2022, promedio_rural2023)


comunas <- unique(ruralidad_total[1]$codigo_comuna)

grafico_ruralidad <- function(comuna, year_index){
  
  df_comuna2 <- ruralidad_total[[year_index]]
  
  df_comuna2 <- df_comuna2 %>% 
    filter(codigo_comuna == comuna)
  
  total_estudiantes <- sum(df_comuna2$n_estudiantes)
  
  df_comuna2 <- df_comuna2 %>%
    mutate(porcentaje = (n_estudiantes / total_estudiantes) * 100)
  
  
  nombre_comuna <- unique(df_comuna2$nombre_comuna)
  
  nombre_comuna <- capitalize(nombre_comuna)
  
  plot_ly(df_comuna2, labels = ~`Tipo de zona`, values = ~n_estudiantes, type = 'pie', hole = 0.6, width = 410, height = 260, # Aumentar tamaño para mayor control
          hoverinfo = 'label+text',
          text = ~paste("Promedio:", promedio, "<br>Estudiantes:", n_estudiantes),
          textinfo = 'percent',
          marker = list(colors = c("#EC5A25", "#2B0E70", "#6B2757", "#AC413E", "#150578"))
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


### Denuncias ####

denuncias_chile2018 <- read_csv("Datos-proyecto/denuncias_chile2018.csv")
denuncias_chile2019 <- read_csv("Datos-proyecto/denuncias_chile2019.csv")
denuncias_chile2020 <- read_csv("Datos-proyecto/denuncias_chile2020.csv")
denuncias_chile2021 <- read_csv("Datos-proyecto/denuncias_chile2021.csv")
denuncias_chile2022 <- read_csv("Datos-proyecto/denuncias_chile2022.csv")
denuncias_chile2023 <- read_csv("Datos-proyecto/denuncias_chile2023.csv")

denuncias_chile_total <- list(denuncias_chile2018, denuncias_chile2019, denuncias_chile2020, denuncias_chile2021, denuncias_chile2022, denuncias_chile2023)



comunas <- unique(denuncias_chile_total[1]$codigo_comuna)

grafico_denunciasC <- function(comuna, year_index){
  
  df_comuna2 <- denuncias_chile_total[[year_index]]
  
  df_comuna2 <- df_comuna2 %>% 
    filter(codigo_comuna == comuna)
  
  total_estudiantes <- sum(df_comuna2$n_denuncias)
  
  df_comuna2 <- df_comuna2 %>%
    mutate(porcentaje = (n_denuncias / total_estudiantes) * 100,
           ambito = sapply(ambito, capitalize))
  
  
  nombre_comuna <- unique(df_comuna2$nombre_comuna)
  
  nombre_comuna <- capitalize(nombre_comuna)
  
  colores_asignados <- paleta_g[1:length(unique(df_comuna2$ambito))]
  
  plot_ly(df_comuna2, 
          labels = ~ambito,
          values = ~n_denuncias,
          parents = "",
          type = "treemap",
          hoverinfo = 'label+text',
          text = ~paste(n_denuncias), 
          textinfo = 'percent',
          marker = list(colors = colores_asignados)
  ) %>%
    layout(
      showlegend = FALSE, # Mostrar la leyenda
      paper_bgcolor = 'rgba(0,0,0,0)', # Fondo del área del gráfico
      plot_bgcolor = 'rgba(0,0,0,0)',  # Fondo del gráfico
      font = list(color = "white", size = 20),
      autosize = TRUE,
      margin = list(l = 50, r = 50, t = 20, b = 70),
      legend = list(
        x = 0.5,           # Centrar la leyenda horizontalmente
        y = 0.5,           # Centrar la leyenda verticalmente
        xanchor = 'center',# Fijar la posición horizontal de la leyenda
        yanchor = 'middle',
        font = list(size = 12, color = "white")
      )
    ) 
  
  
}  

#################

#Aqui empieza la app


ui <- fluidPage(
  tags$head(
    tags$title("Visualizador Chile"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap"),
    tags$style(
      HTML(
        "
        body {
          background: #000E25;
          color: white;
          font-family: 'Roboto', sans-serif;
        }
        .well {
          background-color: #061a40;
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
          font-family: 'Roboto', sans-serif;
          text-align: center;
          margin-top: 20px;
        }
        .card {
          background-color: rgba(0, 0, 0, 0.5);
          border-radius: 10px;
          padding: 15px;
          margin-top: 20px;
          color: white;
          font-family: 'Roboto', sans-serif;
          text-align: center;
        }
        .card-final {
          background-color: #1D263B;
          border-radius: 10px;
          padding: 15px;
          margin-top: 20px;
          color: white;
          font-family: 'Roboto', sans-serif;
          text-align: center;
        }
        .card2 {
          background-color: rgba(243, 198, 221, 0.8);
          border-radius: 10px;
          padding: 15px;
          margin-top: 20px;
          color: white;
          font-family: 'Roboto', sans-serif;
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
          font-family: 'Roboto', sans-serif;
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
          font-family: 'Roboto', sans-serif;
          text-align: center;
          font-size: 16px;
          font-weight: 300;
          background-color: rgba(0, 0, 0, 0.5);
          margin-top: 10px;
          border-radius: 10px;
        }
        .text2 {
        font-family: 'Roboto', sans-serif;
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
        .evolucion {
          color: white;
          font-family: 'Roboto', sans-serif;
          text-align: center;
          font-size: 16px;
          font-weight: 300;
          background-color: rgba(0, 0, 0, 0.5);
          margin-top: 10px;
          border-radius: 10px;
          overflow: hidden;
        } 
        .na_denuncias {
          height: 500px;
          width: 100%;
          color: white;
          border-radius: 10px;
          text-align: center;
          font-family: 'Roboto', sans-serif;
        }
         .help-button {
          position: absolute;
          top: -44px;
          right: -1px;
          border-radius: 50%;
          border: none;
          bottom-margin: 10px;
          color: black;
         }
        .help-text {
        color: black;  
        font-family: 'Roboto', sans-serif;
        font-size: 16px;
         text-align: left;
        }
          /* Estilos para el modal */
        .modal-content {
          background-color: rgba(0, 0, 0, 0.5); 
          border-radius: 10px; 
          color: white; 
        }
        
        .modal-title {
          color: white; 
          font-family: 'Roboto', sans-serif;
        }
        
        .modal-body {
          color: white; 
          font-family: 'Roboto', sans-serif;
        }
        
        .close {
          color: white;
        }
        "
      )
    )
  ),
  titlePanel((div(class = "title", "Visualizador de rendimiento académico en Chile 2018 - 2023"))),
  column(
    12,
    p(div(
      class = "highlighted-subtitle",
      "Los siguientes mapas muestran el rendimiento académico de jóvenes de primero medio a cuarto medio de Chile entre los años 2018 y 2023. Fuente: Sistema de Información General de Estudiantes (SIGE)"
    ))
  ),
  sidebarPanel(
    width = 2,
    fluidRow(div(class = "plot-container", div(class = "custom-selector", selectInput(
      "year",
      "Seleccione el año: ",
      choices = 2018:2023,
      selected = 2018
    )))),
    fluidRow(
      div(class = "plot-container", div(class = "custom-selector", selectInput(
        "Region",
        "Seleccione la región: ",
        choices = nombres_regiones,
        selected = "Región Metropolitana de Santiago"
      )))),
    div(
      class = "text2",
      p("Haga clic en el selector de años para seleccionar el año de interés"),
      p(""),
      br(),
      p("Haga clic en el selector de regiones para seleccionar la región de interés"),
      br(),
      p("Haga clic en una comuna para ver los datos sobre el rendimiento académico promedio")
    )
  ),
  card(
    fluidRow(
      column(
        4,
        div(
          class = "card-final",
          h4("Comuna", class = "card-title"),
          htmlOutput("selected_comuna")
        )
      ),
      column(
        4,
        div(
          class = "card-final",
          h4("Promedio", class = "card-title"),
          htmlOutput("average_comuna")
        )
      ),
      column(
        4,
        div(
          class = "card-final",
          h4("Estudiantes", class = "card-title"),
          htmlOutput("student_count")
        )
      ),
      column(
        8,
        uiOutput("map_title"),
        leafletOutput("mapa1", height = "500", width = "100%")
      ),
      column(
        4,
        div(
          class = "card-final",
          h4("Denuncias", class = "card-title"),
          withSpinner(uiOutput("grafico_denunciasC", height = "500px", width = "100%"), type = 1)
        )
      ),
      column(
        4,
        div(
          class = "card-final",
          h4("Tipos de establecimientos", class = "card-title"),
          withSpinner(plotlyOutput("grafico_interactivo_chile", height = "200px", width = "100%"))
        )
      ),
      column(
        4,
        div(
          class = "card-final",
          h4("Situación final por comuna", class = "card-title"),
          withSpinner(plotlyOutput("grafico_sfinal_chile", height = "200px", width = "100%"))
        )
      ),
      column(
        4,
        div(
          class = "card-final",
          h4("Tipo de zona", class = "card-title"),
          div(style = "position: relative;",  
              actionButton("ayuda", icon("info-circle"), 
                           title = "Ayuda",
                           class = "help-button",
                           style = "border-radius: 50%"),
              withSpinner(plotlyOutput("grafico_ruralidad", height = "200px", width = "100%"))
          )
        )
      ),
      column(
        8,
        div(
          class = "card-final",
          div(class = "evolucion" ,plotOutput("grafico_evolucion_chile", height = "500px", width = "100%")
          ))
      )
    )
  )                
)


server <- function(input, output, session) {
  # Variables reactivas para almacenar la comuna y los datos asociados
  selected_comuna <- reactiveVal(NULL)
  selected_promedio <- reactiveVal(NULL)
  selected_n_estudiantes <- reactiveVal(NULL)
  selected_codigo_comuna <- reactiveVal(NULL)
  
  # Actualiza el título del mapa basado en el año seleccionado
  output$map_title <- renderUI({
    titulo <- switch(as.character(input$year),
                     "2018" = "Mapa de comunas",
                     "2019" = "Mapa de comunas",
                     "2020" = "Mapa de comunas",
                     "2021" = "Mapa de comunas",
                     "2022" = "Mapa de comunas",
                     "2023" = "Mapa de comunas"
    )
    h3(titulo, class = "map-title")
  })
  
  # Renderiza el mapa inicial
  output$mapa1 <- renderLeaflet({
    year_index <- as.numeric(input$year) - 2017
    region <- input$Region
    
    
    # Observa cambios en el slider de años y actualiza el mapa
    
    generar_mapa(year_index, region)
    
  })  
  
  
  # Observa los clics en el mapa y actualiza el contenido de las tarjetas
  observeEvent(input$mapa1_shape_click, {
    click <- input$mapa1_shape_click
    if (!is.null(click)) {
      comuna <- click$id
      
      
      # Encuentra el promedio de la comuna seleccionada según el año
      promedio <- switch(as.character(input$year),
                         "2018" = comunas_chile2018$promedio[comunas_chile2018$nombre_comuna == comuna],
                         "2019" = comunas_chile2019$promedio[comunas_chile2019$nombre_comuna == comuna],
                         "2020" = comunas_chile2020$promedio[comunas_chile2020$nombre_comuna == comuna],
                         "2021" = comunas_chile2021$promedio[comunas_chile2021$nombre_comuna == comuna],
                         "2022" = comunas_chile2022$promedio[comunas_chile2022$nombre_comuna == comuna],
                         "2023" = comunas_chile2023$promedio[comunas_chile2023$nombre_comuna == comuna]
      )
      
      n_estudiantes <- switch(as.character(input$year),
                              "2018" = comunas_chile2018$n_estudiantes[comunas_chile2018$nombre_comuna == comuna],
                              "2019" = comunas_chile2019$n_estudiantes[comunas_chile2019$nombre_comuna == comuna],
                              "2020" = comunas_chile2020$n_estudiantes[comunas_chile2020$nombre_comuna == comuna],
                              "2021" = comunas_chile2021$n_estudiantes[comunas_chile2021$nombre_comuna == comuna],
                              "2022" = comunas_chile2022$n_estudiantes[comunas_chile2022$nombre_comuna == comuna],
                              "2023" = comunas_chile2023$n_estudiantes[comunas_chile2023$nombre_comuna == comuna]
      )
      
      codigo_comuna <- switch(as.character(input$year),
                              "2018" = comunas_chile2018$codigo_comuna[comunas_chile2018$nombre_comuna == comuna],
                              "2019" = comunas_chile2019$codigo_comuna[comunas_chile2019$nombre_comuna == comuna],
                              "2020" = comunas_chile2020$codigo_comuna[comunas_chile2020$nombre_comuna == comuna],
                              "2021" = comunas_chile2021$codigo_comuna[comunas_chile2021$nombre_comuna == comuna],
                              "2022" = comunas_chile2022$codigo_comuna[comunas_chile2022$nombre_comuna == comuna],
                              "2023" = comunas_chile2023$codigo_comuna[comunas_chile2023$nombre_comuna == comuna]
      )
      
      # Actualiza los valores reactivos
      selected_comuna(comuna)
      selected_promedio(promedio)
      selected_n_estudiantes(n_estudiantes)
      selected_codigo_comuna(codigo_comuna)
    }
  })
  
  output$grafico_interactivo_chile <- renderPlotly({
    req(selected_codigo_comuna())
    year_index <- as.numeric(input$year) - 2017
    establecimiento_chile(selected_codigo_comuna(), year_index)
  })
  
  output$grafico_sfinal_chile <- renderPlotly({
    req(selected_codigo_comuna())
    year_index <- as.numeric(input$year) - 2017
    grafico_sfinal_chile(selected_codigo_comuna(), year_index)
  })
  
  output$grafico_evolucion_chile <- renderPlot({
    req(selected_codigo_comuna())
    year_index <- as.numeric(input$year) - 2017
    grafico_evolucion_chile(selected_codigo_comuna(), year_index)
  })
  
  output$grafico_ruralidad <- renderPlotly({
    req(selected_codigo_comuna())
    year_index <- as.numeric(input$year) - 2017
    grafico_ruralidad(selected_codigo_comuna(), year_index)
  })
  
  output$grafico_denunciasC <- renderUI({
    year_index <- as.numeric(input$year) - 2017
    req(selected_codigo_comuna())  # Asegura que haya una comuna seleccionada
    
    df_comuna2 <- denuncias_chile_total[[year_index]] %>%
      filter(codigo_comuna == selected_codigo_comuna())
    
    if (nrow(df_comuna2) == 0) {
      return(h5("No hay datos disponibles para la comuna seleccionada", class = "na_denuncias"))
    } else {
      plot <- grafico_denunciasC(selected_codigo_comuna(), year_index)
      return(plotlyOutput("plot_denuncias", height = "500px", width = "100%"))
    }
  })
  
  output$plot_denuncias <- renderPlotly({
    year_index <- as.numeric(input$year) - 2017
    req(selected_codigo_comuna())
    
    grafico_denunciasC(selected_codigo_comuna(), year_index)
  })
  
  observeEvent(input$ayuda, {
    showModal(
      modalDialog(
        title = "Gráfico de tipo de zonas",
        "Este gráfico enseña los tipos de zona en los cuales se encuentran los establecimientos educacionales. Estos pueden ser de dos tipos: urbanos o rurales. Para conocer el tipo de zona de cada comuna debe reposar su cursor en el gráfico.",
        easyClose = TRUE,
        footer = NULL,
      )
    )
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


