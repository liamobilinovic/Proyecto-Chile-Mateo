###En este script se crean las tablas de promedios de notas finales###
###en la comuna de santiago###

install.packages("leaflet")
install.packages("stringr")

library(stringr)
library(leaflet)
library(sf)

promedios2018 <- tabla_rendimientos2018 %>% 
  group_by(as.character(NOM_COM_RBD)) %>% 
  summarize(promedio = as.integer(mean(PROM_GRAL))) 

Prom_stgo2018 <- STGO_BAS_2018 %>%
  group_by(ComunaEst) %>%
  summarize(promedio = mean(PROM_GRAL, na.rm = TRUE))

geourl <- "https://raw.githubusercontent.com/robsalasco/precenso_2016_geojson_chile/master/Zonas/R13.geojson"

zonas <- jsonlite::fromJSON(geourl, simplifyVector = FALSE)
zonas <- geojsonio::as.json(zonas)
zonas <- geojsonsf::geojson_sf(zonas)

install.packages("chilemapas")

poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 14) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))

promedios_stgo2018 <- data.frame(comunass, Prom_stgo2018$promedio)

###a####
comunass <- as.intec(13101,
             13102,
             13103,
             13104,
             13105,
             13106,
             13107,
             13108,
             13109,
             13110,
             13111,
             13112,
             13113,
             13114,
             13115,
             13116,
             13117,
             13118,
             13119,
             13120,
             13121,
             13122,
             13123,
             13124,
             13125,
             13126,
             13127,
             13128,
             13129,
             13130,
             13131,
             13132)

######

poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 14) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))

comunas_los_rios <- mapa_comunas %>% 
  filter(codigo_region == 14) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(poblacion_adulto_mayor_comunas)

paleta <- c("blue", "deepskyblue", "lightblue", "aquamarine3", "grey")


ggplot(comunas_los_rios) + 
  geom_sf(aes(fill = pob_adulto_mayor, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 anios y mas en la Region de los Rios") +
  theme_minimal(base_size = 13)


promedios_stgo2018 <- Prom_stgo2018 %>% 
  group_by(ComunaEst)


poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 14) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))


promedios2018v2 <- tabla_rendimientos2018v2 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2018v2 <- promedios2018v2 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

tabla_rendimientos2018v2 <- tabla_rendimientos2018 %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0"))


promedios2018 <- tabla_rendimientos2018 %>% 
  group_by(COD_COM_RBD) %>% 
  summarize(promedio = as.integer(mean(PROM_GRAL))) %>% 
  rename("codigo_comunas" = COD_COM_RBD)


comunas_santiago <- mapa_comunas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
    ) %>% 
      left_join(promedios2018v2)

ggplot(comunas_santiago) + 
  geom_sf(aes(fill = promedio, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Promedios") +
  labs(title = "Promedios en Santiago 2018") +
  theme_minimal(base_size = 13)


poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 14) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))

comunas_los_rios <- mapa_comunas %>% 
  filter(codigo_region == 14) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(poblacion_adulto_mayor_comunas)







view(mapa_comunas)

promedios2018 <- promedios2018 %>% rename("codigo_comunas" = COD_COM_RBD)
  
comunas_santiago <- mapa_comunas %>% 
    filter(codigo_provincia == 131) %>% 
    left_join(
      codigos_territoriales %>% 
        select(matches("comuna"))
    ) %>% 
    left_join(promedios2018v2)






poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 14) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))

comunas_los_rios <- mapa_comunas %>% 
  filter(codigo_region == 14) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(poblacion_adulto_mayor_comunas)

ggplot(comunas_los_rios) + 
  geom_sf(aes(fill = pob_adulto_mayor, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 anios y mas en la Region de los Rios") +
  theme_minimal(base_size = 13)


