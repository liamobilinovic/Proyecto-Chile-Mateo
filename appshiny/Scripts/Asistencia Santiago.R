###En este script se van a realizar las tablas y mapas ###
###de la asistencia en Santiago.                       ###

##Paleta

pal_asistencia <- colorNumeric(
  palette = c("#6b2737","#f45b69","#e4fde1","#456990","#114b5f"),
  domain = asistencia_santiago2020unido$asistencia_prom
)

###2018###

asistencia_santiago2018 <- tabla_rendimientos2018 %>%
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(asistencia_prom = as.integer(mean(ASISTENCIA)))

asistencia_santiago2018 <- asistencia_santiago2018 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)



asistencia_santiago2018unido <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(asistencia_santiago2018) %>% 
  group_by(nombre_comuna, codigo_comuna, asistencia_prom) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")


asistencia_santiago2018unido <- st_as_sf(asistencia_santiago2018unido)


leaflet(asistencia_santiago2018unido) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal_asistencia(asistencia_santiago2018unido$asistencia_prom),
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
    label = ~paste("Comuna: ", asistencia_santiago2018unido$nombre_comuna, "", "Promedio de Asistencia: ", asistencia_santiago2018unido$asistencia_prom)
  )


###2019###



###2020###

asistencia_santiago2020 <- tabla_rendimientos2020 %>%
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(asistencia_prom = as.integer(mean(ASISTENCIA)))

asistencia_santiago2020 <- asistencia_santiago2020 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)



asistencia_santiago2020unido <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(asistencia_santiago2020) %>% 
  group_by(nombre_comuna, codigo_comuna, asistencia_prom) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")


asistencia_santiago2020unido <- st_as_sf(asistencia_santiago2020unido)


leaflet(asistencia_santiago2020unido) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal_asistencia(asistencia_santiago2020unido$asistencia_prom),
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
    label = ~paste("Comuna: ", asistencia_santiago2020unido$nombre_comuna, "", "Promedio de Asistencia: ", asistencia_santiago2020unido$asistencia_prom)
  )

###2021###


###2022###


###2023###
