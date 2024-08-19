altura <- c(180, 
            175,
            190,
            189,
            180,
            179,
            165,
            158,
            169,
            175,
            150,
            170,
            195)

peso <- c(80,
          75,
          85,
          90,
          87,
          86,
          65,
          67,
          70,
          54,
          68,
          70,
          90)

df_pesos <- data.frame(altura, peso)

ggplot(data = df_pesos, aes(x = altura, y = peso)) +
  geom_point() + geom_line()

ggplot(Nt_cursos2018, aes(x = Total, y = PROM_GRAL2)) + 
  geom_



datas <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

ggplot(datas, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


ggplot(Nt_cursos2018, aes(x="", y=Total, fill = PROM_GRAL2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0)


df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))

rural_años <- list(Nt_rural2018, Nt_rural2019, Nt_rural2020)

ggplot(data=Nt_rural2018, aes(x=PROM_GRAL2, y=Rural, group=1)) +
  geom_line()+
  geom_point()

orden <- c("Excelente (7,0)", "Satisfactorio (6,0-6,9)", "Bueno (5,0-5,9)", "Suficiente (4,0-4,9)", "Insuficiente (3,0-3,9)", "Malo (2,0-2,9)", "Muy malo (1,0-1,9)", "Total")

Nt_rural2018 <- Nt_rural2018 %>% slice(match(orden, PROM_GRAL2))

orden <- glimpse(Nt_rural2018$PROM_GRAL2)


#########################################################################


comunas1 <- mapa_comunas

comunas <- mapa_comunas %>% 
  filter(codigo_region==13)

notas_geo <- st_sf(comunas_santiago)

pal <- colorNumeric(
  palette = "YloBr",
  domain = comunas_santiago$promedio
)

mapa <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = notas_geo,
              fillColor = ~pal(comunas_santiago$promedio),
              color = "#b2aeae",
              fillOpacity = 1,
              smoothFactor = 0.2,
              weight = 0.8) %>% 
  addLegend(pal = pal,
            values = notas_geo$promedio,
            position = "bottomright",
            title = "Promedio General") %>% 
  addScaleBar(position = "topright")





