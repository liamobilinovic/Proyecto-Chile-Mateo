##Script para definir colegios particulares o no##

###Lectura de CSVs###



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
          text = ~paste("Promedio de notas:", promedio),
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

grafico_interactivo(comuna = 13103, year_index = 1)
