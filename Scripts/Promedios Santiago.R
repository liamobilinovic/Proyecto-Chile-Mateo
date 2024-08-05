###En este script se crean las tablas de promedios de notas finales###
###en la comuna de santiago###



paleta1 <- c("#114b5f","#456990","#e4fde1","#f45b69","#6b2737")

paleta1 <- c("#6b2737","#f45b69","#e4fde1","#456990","#114b5f")

pal <- colorNumeric(
  palette = c("#6b2737","#f45b69","#e4fde1","#456990","#114b5f"),
  domain = rango_global
)


rango_global <- range(data_combinada$promedio, na.rm = TRUE)


###2018###


promedios2018 <- tabla_rendimientos2018 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2018 <- promedios2018 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2018 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2018) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")


comunas_santiago2018 <- st_as_sf(comunas_santiago2018)


leaflet(comunas_santiago2018) %>% 
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
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2018$nombre_comuna, "", "Promedio: ", comunas_santiago2018$promedio)
  )


######



###2019###

promedios2019 <- tabla_rendimientos2019 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2019 <- promedios2019 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2019 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2019) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2019 <- st_as_sf(comunas_santiago2019)

leaflet(comunas_santiago2019) %>% 
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

promedios2020 <- tabla_rendimientos2020 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2020 <- promedios2020 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2020 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2020) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2020 <- st_as_sf(comunas_santiago2020)

leaflet(comunas_santiago2020) %>% 
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

promedios2021 <- tabla_rendimientos2021 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2021 <- promedios2021 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)


comunas_santiago2021 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2021) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2021 <- st_as_sf(comunas_santiago2021)

leaflet(comunas_santiago2021) %>% 
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
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2021$nombre_comuna, "", "Promedio: ", comunas_santiago2010$promedio)
  )

###2022###

promedios2022 <- tabla_rendimientos2022 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2022 <- promedios2022 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2022 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2022) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2022 <- st_as_sf(comunas_santiago2022)

leaflet(comunas_santiago2022) %>% 
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

promedios2023 <- tabla_rendimientos2023 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2023 <- promedios2023 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2023 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2023) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2023 <- st_as_sf(comunas_santiago2023)

leaflet(comunas_santiago2023) %>% 
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
    highlightOptions = highlightOptions(
      weight = 5,
      color = "white",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2023$nombre_comuna, "", "Promedio: ", comunas_santiago2023$promedio)
  )

