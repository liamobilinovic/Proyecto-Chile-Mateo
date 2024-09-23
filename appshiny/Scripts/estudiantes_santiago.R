### este script es para sacar el numero de estudiantes de santiago por comuna ##

estudiantes_santiago2018 <- tabla_rendimientos2018 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(n_estudiantes = n())




###2019###

estudiantes_santiago2019 <- tabla_rendimientos2019 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(n_estudiantes = n())



###2020###

estudiantes_santiago2020 <- tabla_rendimientos2020 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(n_estudiantes = n())



###2021###

estudiantes_santiago2021 <- tabla_rendimientos2021 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(n_estudiantes = n())



###2022###

estudiantes_santiago2022 <- tabla_rendimientos2022 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(n_estudiantes = n())



###2023###

estudiantes_santiago2023 <- tabla_rendimientos2023 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(n_estudiantes = n())

