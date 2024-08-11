###Este script ejecuta todos los scripts en el orden correspondiente##

###Si se quiere ejecutar de otra forma entonces se debe realizar de##
##forma manual          ##

source("/Users/liam/Desktop/Proyecto DARA/Scripts/script_rendimientos_2023.R")

source("/Users/liam/Desktop/Proyecto DARA/Scripts/estudiantes_santiago.R")

source("/Users/liam/Desktop/Proyecto DARA/Scripts/Promedios Santiago.R")

source("/Users/liam/Desktop/Proyecto DARA/appshiny/app.R")

comunas_santiago2018 <- st_transform(comunas_santiago2018, crs = 4326)
comunas_santiago2019 <- st_transform(comunas_santiago2019, crs = 4326)
comunas_santiago2020 <- st_transform(comunas_santiago2020, crs = 4326)
comunas_santiago2021 <- st_transform(comunas_santiago2021, crs = 4326)
comunas_santiago2022 <- st_transform(comunas_santiago2022, crs = 4326)
comunas_santiago2023 <- st_transform(comunas_santiago2023, crs = 4326)


