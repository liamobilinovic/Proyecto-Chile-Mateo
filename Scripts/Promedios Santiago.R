###En este script se crean las tablas de promedios de notas finales###
###en la comuna de santiago###

paleta <- c("blue", "deepskyblue", "lightblue", "aquamarine3", "grey")


library(openxlsx)

###2018###


promedios2018 <- tabla_rendimientos2018 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2018 <- promedios2018 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2018 <- mapa_comunas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2018)

ggplot(comunas_santiago2018) + 
  geom_sf(aes(fill = promedio, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Promedios") +
  labs(title = "Promedios en Santiago 2018") +
  theme_minimal(base_size = 13)

######



###2019###

promedios2019 <- tabla_rendimientos2019 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2019 <- promedios2019 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2019 <- mapa_comunas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2019)

ggplot(comunas_santiago2019) + 
  geom_sf(aes(fill = promedio, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Promedios") +
  labs(title = "Promedios en Santiago 2019") +
  theme_minimal(base_size = 13)
 + theme(panel.grid.major = element_line(colour = NA,
    linetype = "blank"), panel.grid.minor = element_line(colour = NA,
    linetype = "blank"), plot.title = element_text(size = 15))

###2020###

promedios2020 <- tabla_rendimientos2020 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2020 <- promedios2020 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2020 <- mapa_comunas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2020)

ggplot(comunas_santiago2020) + 
  geom_sf(aes(fill = promedio, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Promedios") +
  labs(title = "Promedios en Santiago 2020") +
  theme_minimal(base_size = 13)


###2021###

promedios2021 <- tabla_rendimientos2021 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2021 <- promedios2021 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2021 <- mapa_comunas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2021)

ggplot(comunas_santiago2021) + 
  geom_sf(aes(fill = promedio, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Promedios") +
  labs(title = "Promedios en Santiago 2021") +
  theme_minimal(base_size = 13)

###2022###

promedios2022 <- tabla_rendimientos2022 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2022 <- promedios2022 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2022 <- mapa_comunas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2022)

ggplot(comunas_santiago2022) + 
  geom_sf(aes(fill = promedio, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Promedios") +
  labs(title = "Promedios en Santiago 2022") +
  theme_minimal(base_size = 13)

###2023###

promedios2023 <- tabla_rendimientos2023 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2023 <- promedios2023 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2023 <- mapa_comunas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2023)

ggplot(comunas_santiago2023) + 
  geom_sf(aes(fill = promedio, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Promedios") +
  labs(title = "Promedios en Santiago 2023") +
  theme_minimal(base_size = 13)
