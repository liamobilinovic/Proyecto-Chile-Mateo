
denuncias2018 <- read_delim("denuncias2018.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

denuncias2019 <- read_delim("Denuncias/denuncias2019.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

denuncias2020 <- read_delim("Denuncias/denuncias2020.csv",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

denuncias2021 <- read_delim("Denuncias/denuncias2021.csv",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

denuncias2022 <- read_delim("Denuncias/denuncias2022.csv",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

denuncias2023 <- read_delim("Denuncias/denuncias2023.csv",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)


denuncias2018stgo <- denuncias2018 %>% 
  filter(EE_COD_PROVINCIA == 131) %>% 
  filter(AFEC_COD_ENSE2 == c(5, 7)) %>% 
  group_by(EE_COD_COMUNA, EE_NOM_COMUNA, DEN_AMBITO) %>% 
  summarise(n = n()) %>% 
  rename(codigo_comuna = EE_COD_COMUNA, nombre_comuna = EE_NOM_COMUNA, ambito = DEN_AMBITO, n_denuncias = n)

denuncias2019stgo <- denuncias2019 %>%
  filter(EE_COD_PROVINCIA == 131) %>% 
  filter(AFEC_COD_ENSE2 == c(5, 7)) %>% 
  group_by(EE_COD_COMUNA, EE_NOM_COMUNA, DEN_AMBITO) %>% 
  summarise(n = n()) %>% 
  rename(codigo_comuna = EE_COD_COMUNA, nombre_comuna = EE_NOM_COMUNA, ambito = DEN_AMBITO, n_denuncias = n)

denuncias2020stgo <- denuncias2020 %>%
  filter(EE_COD_PROVINCIA == 131) %>% 
  filter(AFEC_COD_ENSE2 == c(5, 7)) %>% 
  group_by(EE_COD_COMUNA, EE_NOM_COMUNA, DEN_AMBITO) %>% 
  summarise(n = n()) %>% 
  rename(codigo_comuna = EE_COD_COMUNA, nombre_comuna = EE_NOM_COMUNA, ambito = DEN_AMBITO, n_denuncias = n)

denuncias2021stgo <- denuncias2021 %>%
  filter(EE_COD_PROVINCIA == 131) %>% 
  filter(AFEC_COD_ENSE2 == c(5, 7)) %>% 
  group_by(EE_COD_COMUNA, EE_NOM_COMUNA, DEN_AMBITO) %>% 
  summarise(n = n()) %>% 
  rename(codigo_comuna = EE_COD_COMUNA, nombre_comuna = EE_NOM_COMUNA, ambito = DEN_AMBITO, n_denuncias = n)

denuncias2022stgo <- denuncias2022 %>%
  filter(EE_COD_PROVINCIA == 131) %>% 
  filter(AFEC_COD_ENSE2 == c(5, 7)) %>% 
  group_by(EE_COD_COMUNA, EE_NOM_COMUNA, DEN_AMBITO) %>% 
  summarise(n = n()) %>% 
  rename(codigo_comuna = EE_COD_COMUNA, nombre_comuna = EE_NOM_COMUNA, ambito = DEN_AMBITO, n_denuncias = n)

denuncias2023stgo <- denuncias2023 %>%
  filter(EE_COD_PROVINCIA == 131) %>% 
  filter(AFEC_COD_ENSE2 == c(5, 7)) %>% 
  group_by(EE_COD_COMUNA, EE_NOM_COMUNA, DEN_AMBITO) %>% 
  summarise(n = n()) %>% 
  rename(codigo_comuna = EE_COD_COMUNA, nombre_comuna = EE_NOM_COMUNA, ambito = DEN_AMBITO, n_denuncias = n)

write_csv(denuncias2018stgo, "denuncias2018stgo.csv")
write_csv(denuncias2019stgo, "denuncias2019stgo.csv")
write_csv(denuncias2020stgo, "denuncias2020stgo.csv")
write_csv(denuncias2021stgo, "denuncias2021stgo.csv")
write_csv(denuncias2022stgo, "denuncias2022stgo.csv")
write_csv(denuncias2023stgo, "denuncias2023stgo.csv")


########################### 


denuncias_stgo_total <- list(denuncias2018stgo, denuncias2019stgo, denuncias2020stgo, denuncias2021stgo, denuncias2022stgo, denuncias2023stgo)

colores_denuncias <- c(
  "ADMISIÓN" = "#EC5A25",
  "CONVIVENCIA" = "#2B0E70",
  "GESTIÓN DE RECURSOS FINANCIEROS" = "#6B2757",
  "GESTIÓN TÉCNICO-ADMINISTRATIVA" = "#AC413E",
  "GESTIÓN TÉCNICO-PEDAGÓGICA" = "#150578",
  "INFRAESTRUCTURA" = "#C70319",
  "SEGURIDAD E HIGIENE" = "#2C0DBB",
  "MOBILIARIO, EQUIPAMIENTO, MATERIAL DIDÁCTICO Y ELEMENTOS DE ENSEÑANZA" = "#6F0976")
  


comunas <- unique(denuncias_stgo_total[1]$codigo_comuna)

grafico_denunciasC <- function(comuna, year_index){
  
  df_comuna2 <- denuncias_stgo_total[[year_index]]
  
  df_comuna2 <- df_comuna2 %>% 
    filter(codigo_comuna == comuna)
  
  total_estudiantes <- sum(df_comuna2$n_denuncias)
  
  df_comuna2 <- df_comuna2 %>%
    mutate(porcentaje = (n_denuncias / total_estudiantes) * 100,
           ambito = sapply(ambito, capitalize))
  
  
  nombre_comuna <- unique(df_comuna2$nombre_comuna)
  
  nombre_comuna <- capitalize(nombre_comuna)
  
  colores_asignados <- colores_denuncias[df_comuna2$ambito]

  plot_ly(df_comuna2, labels = ~ambito, values = ~n_denuncias, type = 'pie', hole = 0.6, width = 410, height = 260, # Aumentar tamaño para mayor control
          hoverinfo = 'label+text',
          text = ~paste(n_denuncias), 
          textinfo = 'percent',
          marker = list(colors = colores_asignados)
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

grafico_denunciasC(13102, 1)
  
