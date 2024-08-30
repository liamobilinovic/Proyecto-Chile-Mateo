### Este script contiene las funciones para generar los mapas de todas las regiones de Chile### 

##ojo, se excluye el Gran Santiago. 

##################

##2018##

promedios2018 <- tabla_rendimientos2018 %>% 
  group_by(as.character(COD_COM_RBD), COD_REG_RBD) %>% 
  summarise(promedio = round(mean(PROM_GRAL)) /10)

promedios2018 <- merge(promedios2018, estudiantes_santiago2018, by = "as.character(COD_COM_RBD)")

promedios2018 <- promedios2018 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_chile2018 <- mapa_comunas %>%
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2018) %>% 
  group_by(nombre_comuna, codigo_comuna, codigo_region, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")


comunas_chile2018 <- st_as_sf(comunas_chile2018)

comunas_chile2018 <- st_transform(comunas_chile2018, crs = 4326)

##2019##

promedios2019 <- tabla_rendimientos2019 %>% 
  group_by(as.character(COD_COM_RBD), COD_REG_RBD) %>% 
  summarise(promedio = round(mean(PROM_GRAL)) /10)

promedios2019 <- merge(promedios2019, estudiantes_santiago2019, by = "as.character(COD_COM_RBD)")

promedios2019 <- promedios2019 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_chile2019 <- mapa_comunas %>%
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2019) %>% 
  group_by(nombre_comuna, codigo_comuna, codigo_region, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_chile2019 <- st_as_sf(comunas_chile2019)

comunas_chile2019 <- st_transform(comunas_chile2019, crs = 4326)

##2020###

promedios2020 <- tabla_rendimientos2020 %>% 
  group_by(as.character(COD_COM_RBD), COD_REG_RBD) %>% 
  summarise(promedio = round(mean(PROM_GRAL)) /10)

promedios2020 <- merge(promedios2020, estudiantes_santiago2020, by = "as.character(COD_COM_RBD)")

promedios2020 <- promedios2020 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_chile2020 <- mapa_comunas %>%
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2020) %>% 
  group_by(nombre_comuna, codigo_comuna, codigo_region, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_chile2020 <- st_as_sf(comunas_chile2020)

comunas_chile2020 <- st_transform(comunas_chile2020, crs = 4326)

##2021###

promedios2021 <- tabla_rendimientos2021 %>% 
  group_by(as.character(COD_COM_RBD), COD_REG_RBD) %>% 
  summarise(promedio = round(mean(PROM_GRAL)) /10)

promedios2021 <- merge(promedios2021, estudiantes_santiago2021, by = "as.character(COD_COM_RBD)")

promedios2021 <- promedios2021 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_chile2021 <- mapa_comunas %>%
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2021) %>% 
  group_by(nombre_comuna, codigo_comuna, codigo_region, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_chile2021 <- st_as_sf(comunas_chile2021)

comunas_chile2021 <- st_transform(comunas_chile2021, crs = 4326)

##2022###

promedios2022 <- tabla_rendimientos2022 %>% 
  group_by(as.character(COD_COM_RBD), COD_REG_RBD) %>% 
  summarise(promedio = round(mean(PROM_GRAL)) /10)

promedios2022 <- merge(promedios2022, estudiantes_santiago2022, by = "as.character(COD_COM_RBD)")

promedios2022 <- promedios2022 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

promedios2022 <- promedios2022 %>% 
  rename("codigo_region" = `COD_REG_RBD`)

comunas_chile2022 <- mapa_comunas %>%
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2022) %>% 
  group_by(nombre_comuna, codigo_comuna, codigo_region, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_chile2022 <- st_as_sf(comunas_chile2022)

comunas_chile2022 <- st_transform(comunas_chile2022, crs = 4326)

##2023###

promedios2023 <- tabla_rendimientos2023 %>% 
  group_by(as.character(COD_COM_RBD), COD_REG_RBD) %>% 
  summarise(promedio = round(mean(PROM_GRAL)) /10)

promedios2023 <- merge(promedios2023, estudiantes_santiago2023, by = "as.character(COD_COM_RBD)")

promedios2023 <- promedios2023 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

promedios2023 <- promedios2023 %>% 
  rename("codigo_region" = `COD_REG_RBD`)

comunas_chile2023 <- mapa_comunas %>%
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna")) 
  ) %>% 
  left_join(promedios2023) %>% 
  group_by(nombre_comuna, codigo_comuna, codigo_region, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_chile2023 <- st_as_sf(comunas_chile2023)

comunas_chile2023 <- st_transform(comunas_chile2023, crs = 4326)


#############################
write_sf(comunas_chile2018, "comunas_chile2018.gpkg")
write_sf(comunas_chile2019, "comunas_chile2019.gpkg")
write_sf(comunas_chile2020, "comunas_chile2020.gpkg")
write_sf(comunas_chile2021, "comunas_chile2021.gpkg")
write_sf(comunas_chile2022, "comunas_chile2022.gpkg")
write_sf(comunas_chile2023, "comunas_chile2023.gpkg")



#############################

comunas_total <- list(comunas_chile2018, comunas_chile2019, comunas_chile2020, comunas_chile2021, comunas_chile2022, comunas_chile2023)


comunas <- unique(comunas_total[1]$codigo_comuna)

generar_mapa <- function(year_index, region){
  
  df_total <- comunas_total[[year_index]]
  
  df_total <- df_total %>% 
    filter(codigo_region == region)
  
  leaflet(df_total) %>%
    addTiles(
      urlTemplate = "",
      options = tileOptions(background = "white")
    ) %>%
    addPolygons(
      fillColor = ~ pal(df_total$promedio),
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
      label = ~ paste("Comuna: ", nombre_comuna, "", "Promedio: ", promedio)
    ) %>% 
    addLegend(
      pal = pal,
      values = ~ promedio,
      opacity = 0.5,
      title = "Promedio",
      position = "topright"
    )
}

generar_mapa(1, "02")
