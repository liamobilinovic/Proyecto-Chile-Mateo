##Script para definir colegios particulares o no##

###Santiago###

###Tablas por año####

establecimiento2018 <- tabla_rendimientos2018 %>% 
  filter(COD_PRO_RBD == 131) %>% 
  group_by(as.integer(COD_COM_RBD), TipoEstablecimiento) %>%
  summarise(n_estudiantes = n(), 
            promedio = mean(PROM_GRAL), 
            .groups = "drop")

establecimiento2018 <- establecimiento2018 %>% 
  rename("codigo_comuna" = `as.integer(COD_COM_RBD)`)

establecimiento2019 <- tabla_rendimientos2019 %>% 
  filter(COD_PRO_RBD == 131) %>% 
  group_by(as.integer(COD_COM_RBD), TipoEstablecimiento) %>%
  summarise(n_estudiantes = n(), 
            promedio = mean(PROM_GRAL), 
            .groups = "drop")

establecimiento2019 <- establecimiento2019 %>% 
  rename("codigo_comuna" = `as.integer(COD_COM_RBD)`)

establecimiento2020 <- tabla_rendimientos2020 %>% 
  filter(COD_PRO_RBD == 131) %>% 
  group_by(as.integer(COD_COM_RBD), TipoEstablecimiento) %>%
  summarise(n_estudiantes = n(), 
            promedio = mean(PROM_GRAL), 
            .groups = "drop")

establecimiento2020 <- establecimiento2020 %>%
  rename("codigo_comuna" = `as.integer(COD_COM_RBD)`)

establecimiento2021 <- tabla_rendimientos2021 %>% 
  filter(COD_PRO_RBD == 131) %>% 
  group_by(as.integer(COD_COM_RBD), TipoEstablecimiento) %>%
  summarise(n_estudiantes = n(), 
            promedio = mean(PROM_GRAL), 
            .groups = "drop")

establecimiento2021 <- establecimiento2021 %>%
  rename("codigo_comuna" = `as.integer(COD_COM_RBD)`)

establecimiento2022 <- tabla_rendimientos2022 %>% 
  filter(COD_PRO_RBD == 131) %>% 
  group_by(as.integer(COD_COM_RBD), TipoEstablecimiento) %>%
  summarise(n_estudiantes = n(), 
            promedio = mean(PROM_GRAL), 
            .groups = "drop")

establecimiento2022 <- establecimiento2022 %>%
  rename("codigo_comuna" = `as.integer(COD_COM_RBD)`)

establecimiento2023 <- tabla_rendimientos2023 %>% 
  filter(COD_PRO_RBD == 131) %>% 
  group_by(as.integer(COD_COM_RBD), TipoEstablecimiento) %>%
  summarise(n_estudiantes = n(), 
            promedio = mean(PROM_GRAL), 
            .groups = "drop")

establecimiento2023 <- establecimiento2023 %>%
  rename("codigo_comuna" = `as.integer(COD_COM_RBD)`)

establecimientos_total <- list(establecimiento2018, establecimiento2019, establecimiento2020, establecimiento2021, establecimiento2022, establecimiento2023)

#####

#Función para hacer graficos interactivos##

comunas <- unique(establecimientos_total$codigo_comuna)

grafico_interactivo <- function(comuna, year_index){
  
  df_comuna <- establecimientos_total[[year_index]]
  
  df_comuna <- df_comuna %>% 
    filter(codigo_comuna == comuna)
  
  total_estudiantes <- sum(df_comuna$n_estudiantes)
  
  df_comuna <- df_comuna %>%
    mutate(porcentaje = (n_estudiantes / total_estudiantes) * 100)
  
  plot_ly(df_comuna, labels = ~TipoEstablecimiento, values = ~n_estudiantes, type = 'pie',
          textinfo = 'label+percent',
          hoverinfo = 'label+value+text',
          text = ~paste("Promedio Nota:", round(promedio, 1)),
          marker = list(colors = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854'))
  )
}

#Aqui solo hay que poner el codigo de la comuna

grafico_interactivo(comuna = 13101, year_index = 1)

