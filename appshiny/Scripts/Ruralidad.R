###Rural vs urbano####

###aqui veremos las diferencias de promedio entre rural y urbano###


###2018###

promedio_rural2018 <- tabla_rendimientos2018 %>%
  group_by(as.integer(COD_COM_RBD), NOM_COM_RBD, TipoRural) %>%
  summarise(n_estudiantes = n(), 
            promedio = round(mean(PROM_GRAL)) / 10,
            .groups = "drop")

promedio_rural2018 <- promedio_rural2018 %>% 
  rename("Tipo de zona" = TipoRural,
         "nombre_comuna" = NOM_COM_RBD,
         "codigo_comuna" = `as.integer(COD_COM_RBD)`)


promedio_rural2019 <- tabla_rendimientos2019 %>%
  group_by(as.integer(COD_COM_RBD), NOM_COM_RBD, TipoRural) %>%
  summarise(n_estudiantes = n(), 
            promedio = round(mean(PROM_GRAL)) / 10,
            .groups = "drop")

promedio_rural2019 <- promedio_rural2019 %>%
  rename("Tipo de zona" = TipoRural,
         "nombre_comuna" = NOM_COM_RBD,
         "codigo_comuna" = `as.integer(COD_COM_RBD)`)

promedio_rural2020 <- tabla_rendimientos2020 %>%
  group_by(as.integer(COD_COM_RBD), NOM_COM_RBD, TipoRural) %>%
  summarise(n_estudiantes = n(), 
            promedio = round(mean(PROM_GRAL)) / 10,
            .groups = "drop")

promedio_rural2020 <- promedio_rural2020 %>%
  rename("Tipo de zona" = TipoRural,
         "nombre_comuna" = NOM_COM_RBD,
         "codigo_comuna" = `as.integer(COD_COM_RBD)`)

promedio_rural2021 <- tabla_rendimientos2021 %>%
  group_by(as.integer(COD_COM_RBD), NOM_COM_RBD, TipoRural) %>%
  summarise(n_estudiantes = n(), 
            promedio = round(mean(PROM_GRAL)) / 10,
            .groups = "drop")

promedio_rural2021 <- promedio_rural2021 %>%
  rename("Tipo de zona" = TipoRural,
         "nombre_comuna" = NOM_COM_RBD,
         "codigo_comuna" = `as.integer(COD_COM_RBD)`)

promedio_rural2022 <- tabla_rendimientos2022 %>%
  group_by(as.integer(COD_COM_RBD), NOM_COM_RBD, TipoRural) %>%
  summarise(n_estudiantes = n(), 
            promedio = round(mean(PROM_GRAL)) / 10,
            .groups = "drop")

promedio_rural2022 <- promedio_rural2022 %>%
  rename("Tipo de zona" = TipoRural,
         "nombre_comuna" = NOM_COM_RBD,
         "codigo_comuna" = `as.integer(COD_COM_RBD)`)

promedio_rural2023 <- tabla_rendimientos2023 %>%
  group_by(as.integer(COD_COM_RBD), NOM_COM_RBD, TipoRural) %>%
  summarise(n_estudiantes = n(), 
            promedio = round(mean(PROM_GRAL)) / 10,
            .groups = "drop")

promedio_rural2023 <- promedio_rural2023 %>%
  rename("Tipo de zona" = TipoRural,
         "nombre_comuna" = NOM_COM_RBD,
         "codigo_comuna" = `as.integer(COD_COM_RBD)`)

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





