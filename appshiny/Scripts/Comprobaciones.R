###Este es un script para comprobar que las funciones esten en orden###

comprobar1 <- tabla_promedios_2021 %>% filter(Curso == "1ero Básico")

mean(comprobar1$PROM_GRAL)

