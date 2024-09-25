
sfinal2018 <- read_csv("Datos-proyecto/situacionfinal2018.csv")
sfinal2019 <- read_csv("Datos-proyecto/situacionfinal2019.csv")
sfinal2020 <- read_csv("Datos-proyecto/situacionfinal2020.csv")
sfinal2021 <- read_csv("Datos-proyecto/situacionfinal2021.csv")
sfinal2022 <- read_csv("Datos-proyecto/situacionfinal2022.csv")
sfinal2023 <- read_csv("Datos-proyecto/situacionfinal2023.csv")



situacion_final_total <- list(sfinal2018, sfinal2019, sfinal2020, sfinal2021, sfinal2022, sfinal2023)

capitalize <- function(text) {
  # Divide el texto en palabras
  words <- unlist(strsplit(text, " "))
  
  capitalize <- paste(toupper(substring(words, 1, 1)), 
                      tolower(substring(words, 2)), 
                      sep = "", 
                      collapse = " ")
  
  return(capitalize)
}



comunas <- unique(situacion_final_total[1]$codigo_comuna)

grafico_sfinal <- function(comuna, year_index){
  
  df_comuna2 <- situacion_final_total[[year_index]]
  
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

grafico_sfinal(comuna = 13101, year_index = 1)
