###En este script se crean las tablas de promedios de notas finales###
###en la comuna de santiago###


###2018###


promedios2018 <- tabla_rendimientos2018 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2018 <- merge(promedios2018, estudiantes_santiago2018, by = "as.character(COD_COM_RBD)")

promedios2018 <- promedios2018 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)



comunas_santiago2018 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2018) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")


comunas_santiago2018 <- st_as_sf(comunas_santiago2018)




######



###2019###

promedios2019 <- tabla_rendimientos2019 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2019 <- merge(promedios2019, estudiantes_santiago2019, by = "as.character(COD_COM_RBD)")

promedios2019 <- promedios2019 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2019 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2019) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2019 <- st_as_sf(comunas_santiago2019)




###2020###

promedios2020 <- tabla_rendimientos2020 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2020 <- merge(promedios2020, estudiantes_santiago2020, by = "as.character(COD_COM_RBD)")

promedios2020 <- promedios2020 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2020 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2020) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2020 <- st_as_sf(comunas_santiago2020)




###2021###

promedios2021 <- tabla_rendimientos2021 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2021 <- merge(promedios2021, estudiantes_santiago2021, by = "as.character(COD_COM_RBD)")

promedios2021 <- promedios2021 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)


comunas_santiago2021 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2021) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2021 <- st_as_sf(comunas_santiago2021)



###2022###

promedios2022 <- tabla_rendimientos2022 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2022 <- merge(promedios2022, estudiantes_santiago2022, by = "as.character(COD_COM_RBD)")

promedios2022 <- promedios2022 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2022 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2022) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2022 <- st_as_sf(comunas_santiago2022)


###2023###

promedios2023 <- tabla_rendimientos2023 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2023 <- merge(promedios2023, estudiantes_santiago2023, by = "as.character(COD_COM_RBD)")

promedios2023 <- promedios2023 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2023 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2023) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2023 <- st_as_sf(comunas_santiago2023)




###MAPAS###

data_combinada <- rbind(promedios2018, promedios2019, promedios2020, promedios2021, promedios2022, promedios2023)

rango_global <- range(data_combinada$promedio, na.rm = TRUE)


bins <- c(40, 45, 50, 52, 54, 56, 58, 60, 62, 64, 65)

paleta <- c(
  "#DD0202", 
  "#C70319", 
  "#B10530", 
  "#9B0647", 
  "#85085E", 
  "#6F0976", 
  "#580A8D",
  "#420CA4", 
  "#2C0DBB", 
  "#160FD2", 
  "#0010E9")



pal <- colorBin(
  palette = paleta,
  domain = c(40, 65),
  bins = bins)



###2018###


mapa_santiago2018 <- leaflet(comunas_santiago2018) %>% 
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
    layerId = ~nombre_comuna,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2018$nombre_comuna, "", "Promedio: ", comunas_santiago2018$promedio)
  )


###2019###

mapa_santiago2019 <- leaflet(comunas_santiago2019) %>% 
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
    layerId = ~nombre_comuna,
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

mapa_santiago2020 <- leaflet(comunas_santiago2020) %>% 
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
    layerId = ~nombre_comuna,
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

mapa_santiago2021 <- leaflet(comunas_santiago2021) %>% 
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
    layerId = ~nombre_comuna,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2021$nombre_comuna, "", "Promedio: ", comunas_santiago2021$promedio)
  )

###2022###


mapa_santiago2022 <- leaflet(comunas_santiago2022) %>% 
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
    layerId = ~nombre_comuna,
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

mapa_santiago2023 <- leaflet(comunas_santiago2023) %>% 
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
    layerId = ~nombre_comuna,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "white",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2023$nombre_comuna, "", "Promedio: ", comunas_santiago2023$promedio)
  )

