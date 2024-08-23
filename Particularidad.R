##Script para definir colegios particulares o no##

###Santiago###

###2018###


particularidad2018santiago <- tabla_rendimientos2018 %>% 
  filter(COD_PRO_RBD == 131) %>% 
  group_by(NOM_COM_RBD, TipoEstablecimiento) %>%
  summarise(n_estudiantes = n(), 
            promedio = mean(PROM_GRAL), 
            .groups = "drop")

#Función para hacer graficos interactivos##

comunas <- unique(particularidad2018santiago$NOM_COM_RBD)

grafico_interactivo <- function(comuna){
  df_comuna <- particularidad2018santiago %>% 
    filter(NOM_COM_RBD == comuna)
  
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

#Aqui solo hay que poner el nombre.

grafico_interactivo("RECOLETA")