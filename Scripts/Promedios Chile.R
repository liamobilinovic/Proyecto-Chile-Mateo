### En este script se guardan todas las tablas referentes ###
### a los promedios de todo chile                         ###


comunas_chile2018 <- mapa_comunas %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2018)

comunas_chile2018 <- st_as_sf(comunas_chile2018)


leaflet(comunas_chile2018) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_chile2018$promedio),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_chile2018$nombre_comuna, "", "Promedio: ", comunas_chile2018$promedio)
  )

 ###2019


###2020 


### En este script se guardan todas las tablas referentes ###
### a los promedios de todo chile                         ###


comunas_chile2020 <- mapa_comunas %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2020)

comunas_chile2020 <- st_as_sf(comunas_chile2020)


leaflet(comunas_chile2020) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_chile2020$promedio),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_chile2020$nombre_comuna, "", "Promedio: ", comunas_chile2020$promedio)
  )



