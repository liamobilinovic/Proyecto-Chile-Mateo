### este script es para sacar el numero de estudiantes de santiago por comuna ##

estudiantes <- tabla_rendimientos2018 %>%
  filter(COD_PRO_RBD == 131) %>% 
  group_by(NOM_COM_RBD) %>% 
  summarise(n_estudiantes = n())

estudiantes <- estudiantes %>% 
  rename("comuna" = NOM_COM_RBD)

comunas_santiago2018 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2018) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")
