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
          text = ~ paste("Promedio de notas:", as.integer(round(promedio, 1))),
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

establecimiento_chile(04103, 2)


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


comunas2 <- unique(sfinal_chile_total[1]$codigo_comuna)

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
          text = ~paste(n_estudiantes),
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

grafico_sfinal_chile(13103, 1)
