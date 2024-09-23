### Deserción escolar ###

library(summarytools)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(tidyr)
library(janitor)
library(sjmisc)
library(sjPlot)
library(httpgd)
library(devtools)
library(shiny)
library(bslib)
library(ggplot2)
library(ggThemeAssist)
library(stringr)
library(leaflet)
library(sf)
library(chilemapas)
library(shinydashboard)
library(plotly)

####################################

####2018####

rendimientos2018_final <- read_delim("Bases de datos/Rendimiento-2018/20190220_Rendimiento_2018_20190131_PUBL.csv", 
                                 delim = ";", quote = "'", escape_double = FALSE, 
                                 trim_ws = TRUE)

rendimientos2018_final <- rendimientos2018_final %>% select(COD_REG_RBD, 
                                                      COD_PRO_RBD, 
                                                      COD_DEPE2, 
                                                      GEN_ALU, 
                                                      COD_REG_ALU,
                                                      COD_COM_ALU,
                                                      COD_COM_RBD,
                                                      NOM_COM_RBD,
                                                      LET_CUR,
                                                      COD_ENSE,
                                                      COD_ENSE2,
                                                      COD_GRADO,
                                                      RURAL_RBD,
                                                      NOM_COM_ALU,
                                                      ASISTENCIA,
                                                      PROM_GRAL,
                                                      SIT_FIN)

situacionfinal2018 <- rendimientos2018_final %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  filter(COD_ENSE2 == c(5, 7)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ro Medio",
                           COD_GRADO %in% c(2) ~ "2do Medio",
                           COD_GRADO %in% c(3) ~ "3ro Medio",
                           COD_GRADO %in% c(4) ~ "4to Medio")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(TipoEstablecimiento = case_when(COD_DEPE2 %in% c(1) ~ "Municipal",
                                         COD_DEPE2 %in% c(2) ~ "Particular Subvencionado",
                                         COD_DEPE2 %in% c(3) ~ "Particular Pagado",
                                         COD_DEPE2 %in% c(4) ~ "Corporación de Administración Delegada",
                                         COD_DEPE2 %in% c(5) ~ "Servicio Local de Educación",)) %>% 
  mutate(SituacionFinal = case_when(SIT_FIN %in% c("P") ~ "Promovido",
                                    SIT_FIN %in% c("R") ~ "Reprobado",
                                    SIT_FIN %in% c("Y") ~ "Retirado")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0")) %>% 
  mutate(COD_REG_RBD = str_pad(COD_REG_RBD, width = 2, pad = "0"))

####2019####

rendimientos2019_final <- read_delim("Bases de datos/Rendimiento-2019/20200220_Rendimiento_2019_20200131_PUBL.csv", 
                                     delim = ";", quote = "'", escape_double = FALSE, 
                                     trim_ws = TRUE)

rendimientos2019_final <- rendimientos2019_final %>% select(COD_REG_RBD, 
                                                      COD_PRO_RBD, 
                                                      COD_DEPE2, 
                                                      GEN_ALU, 
                                                      COD_REG_ALU,
                                                      COD_COM_ALU,
                                                      COD_COM_RBD,
                                                      NOM_COM_RBD,
                                                      LET_CUR,
                                                      COD_ENSE,
                                                      COD_ENSE2,
                                                      COD_GRADO,
                                                      RURAL_RBD,
                                                      NOM_COM_ALU,
                                                      ASISTENCIA,
                                                      PROM_GRAL,
                                                      SIT_FIN)

situacionfinal2019 <- rendimientos2019_final %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  filter(COD_ENSE2 == c(5, 7)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ro Medio",
                           COD_GRADO %in% c(2) ~ "2do Medio",
                           COD_GRADO %in% c(3) ~ "3ro Medio",
                           COD_GRADO %in% c(4) ~ "4to Medio")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(TipoEstablecimiento = case_when(COD_DEPE2 %in% c(1) ~ "Municipal",
                                         COD_DEPE2 %in% c(2) ~ "Particular Subvencionado",
                                         COD_DEPE2 %in% c(3) ~ "Particular Pagado",
                                         COD_DEPE2 %in% c(4) ~ "Corporación de Administración Delegada",
                                         COD_DEPE2 %in% c(5) ~ "Servicio Local de Educación",)) %>% 
  mutate(SituacionFinal = case_when(SIT_FIN %in% c("P") ~ "Promovido",
                                    SIT_FIN %in% c("R") ~ "Reprobado",
                                    SIT_FIN %in% c("Y") ~ "Retirado")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0")) %>% 
  mutate(COD_REG_RBD = str_pad(COD_REG_RBD, width = 2, pad = "0"))

####2020####

rendimientos2020_final <- read_delim("Bases de datos/Rendimiento-2020/20210223_Rendimiento_2020_20210131_WEB.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)


rendimientos2020_final <- rendimientos2020_final %>% select(COD_REG_RBD, 
                                                      COD_PRO_RBD, 
                                                      COD_DEPE2, 
                                                      GEN_ALU, 
                                                      COD_REG_ALU,
                                                      COD_COM_ALU,
                                                      COD_COM_RBD,
                                                      NOM_COM_RBD,
                                                      LET_CUR,
                                                      COD_ENSE,
                                                      COD_ENSE2,
                                                      COD_GRADO,
                                                      RURAL_RBD,
                                                      NOM_COM_ALU,
                                                      ASISTENCIA,
                                                      PROM_GRAL,
                                                      SIT_FIN)

situacionfinal2020 <- rendimientos2020_final %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  filter(COD_ENSE2 == c(5, 7)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ro Medio",
                           COD_GRADO %in% c(2) ~ "2do Medio",
                           COD_GRADO %in% c(3) ~ "3ro Medio",
                           COD_GRADO %in% c(4) ~ "4to Medio")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(TipoEstablecimiento = case_when(COD_DEPE2 %in% c(1) ~ "Municipal",
                                         COD_DEPE2 %in% c(2) ~ "Particular Subvencionado",
                                         COD_DEPE2 %in% c(3) ~ "Particular Pagado",
                                         COD_DEPE2 %in% c(4) ~ "Corporación de Administración Delegada",
                                         COD_DEPE2 %in% c(5) ~ "Servicio Local de Educación",)) %>% 
  mutate(SituacionFinal = case_when(SIT_FIN %in% c("P") ~ "Promovido",
                                    SIT_FIN %in% c("R") ~ "Reprobado",
                                    SIT_FIN %in% c("Y") ~ "Retirado")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0")) %>% 
  mutate(COD_REG_RBD = str_pad(COD_REG_RBD, width = 2, pad = "0"))

####2021####

rendimientos2021_final <- read_delim("Bases de datos/Rendimiento-2021/20220302_Rendimiento_2021_20220131_WEB.csv", 
                                     delim = ";", quote = "'", escape_double = FALSE, trim_ws = TRUE)

rendimientos2021_final <- rendimientos2021_final %>% select(COD_REG_RBD, 
                                                      COD_PRO_RBD, 
                                                      COD_DEPE2, 
                                                      GEN_ALU, 
                                                      COD_REG_ALU,
                                                      COD_COM_ALU,
                                                      COD_COM_RBD,
                                                      NOM_COM_RBD,
                                                      LET_CUR,
                                                      COD_ENSE,
                                                      COD_ENSE2,
                                                      COD_GRADO,
                                                      RURAL_RBD,
                                                      NOM_COM_ALU,
                                                      ASISTENCIA,
                                                      PROM_GRAL,
                                                      SIT_FIN)

situacionfinal2021 <- rendimientos2021_final %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  filter(COD_ENSE2 == c(5, 7)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ro Medio",
                           COD_GRADO %in% c(2) ~ "2do Medio",
                           COD_GRADO %in% c(3) ~ "3ro Medio",
                           COD_GRADO %in% c(4) ~ "4to Medio")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(TipoEstablecimiento = case_when(COD_DEPE2 %in% c(1) ~ "Municipal",
                                         COD_DEPE2 %in% c(2) ~ "Particular Subvencionado",
                                         COD_DEPE2 %in% c(3) ~ "Particular Pagado",
                                         COD_DEPE2 %in% c(4) ~ "Corporación de Administración Delegada",
                                         COD_DEPE2 %in% c(5) ~ "Servicio Local de Educación",)) %>% 
  mutate(SituacionFinal = case_when(SIT_FIN %in% c("P") ~ "Promovido",
                                    SIT_FIN %in% c("R") ~ "Reprobado",
                                    SIT_FIN %in% c("Y") ~ "Retirado")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0")) %>% 
  mutate(COD_REG_RBD = str_pad(COD_REG_RBD, width = 2, pad = "0"))

####2022####

rendimientos2022_final <- read_delim("Bases de datos/Rendimiento-2022/20230209_Rendimiento_2022_20230131_WEB.csv", 
                                     delim = ";", quote = "'", escape_double = FALSE, 
                                     trim_ws = TRUE)

rendimientos2022_final <- rendimientos2022_final %>% select(COD_REG_RBD, 
                                                      COD_PRO_RBD, 
                                                      COD_DEPE2, 
                                                      GEN_ALU, 
                                                      COD_REG_ALU,
                                                      COD_COM_ALU,
                                                      COD_COM_RBD,
                                                      NOM_COM_RBD,
                                                      LET_CUR,
                                                      COD_ENSE,
                                                      COD_ENSE2,
                                                      COD_GRADO,
                                                      RURAL_RBD,
                                                      NOM_COM_ALU,
                                                      ASISTENCIA,
                                                      PROM_GRAL,
                                                      SIT_FIN)

situacionfinal2022 <- rendimientos2022_final %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  filter(COD_ENSE2 == c(5, 7)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ro Medio",
                           COD_GRADO %in% c(2) ~ "2do Medio",
                           COD_GRADO %in% c(3) ~ "3ro Medio",
                           COD_GRADO %in% c(4) ~ "4to Medio")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(TipoEstablecimiento = case_when(COD_DEPE2 %in% c(1) ~ "Municipal",
                                         COD_DEPE2 %in% c(2) ~ "Particular Subvencionado",
                                         COD_DEPE2 %in% c(3) ~ "Particular Pagado",
                                         COD_DEPE2 %in% c(4) ~ "Corporación de Administración Delegada",
                                         COD_DEPE2 %in% c(5) ~ "Servicio Local de Educación",)) %>% 
  mutate(SituacionFinal = case_when(SIT_FIN %in% c("P") ~ "Promovido",
                                    SIT_FIN %in% c("R") ~ "Reprobado",
                                    SIT_FIN %in% c("Y") ~ "Retirado")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0")) %>% 
  mutate(COD_REG_RBD = str_pad(COD_REG_RBD, width = 2, pad = "0"))

####2023####

rendimientos2023_final <- read_delim("Bases de datos/Rendimiento-2023/20240209_Rendimiento_2023_20240131_WEB.csv", 
                                     delim = ";", quote = "'", escape_double = FALSE, 
                                     trim_ws = TRUE)

rendimientos2023_final <- rendimientos2023_final %>% select(COD_REG_RBD, 
                                                      COD_PRO_RBD, 
                                                      COD_DEPE2, 
                                                      GEN_ALU, 
                                                      COD_REG_ALU,
                                                      COD_COM_ALU,
                                                      COD_COM_RBD,
                                                      NOM_COM_RBD,
                                                      LET_CUR,
                                                      COD_ENSE,
                                                      COD_ENSE2,
                                                      COD_GRADO,
                                                      RURAL_RBD,
                                                      NOM_COM_ALU,
                                                      ASISTENCIA,
                                                      PROM_GRAL,
                                                      SIT_FIN)

situacionfinal2023 <- rendimientos2023_final %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  filter(COD_ENSE2 == c(5, 7)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ro Medio",
                           COD_GRADO %in% c(2) ~ "2do Medio",
                           COD_GRADO %in% c(3) ~ "3ro Medio",
                           COD_GRADO %in% c(4) ~ "4to Medio")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(TipoEstablecimiento = case_when(COD_DEPE2 %in% c(1) ~ "Municipal",
                                         COD_DEPE2 %in% c(2) ~ "Particular Subvencionado",
                                         COD_DEPE2 %in% c(3) ~ "Particular Pagado",
                                         COD_DEPE2 %in% c(4) ~ "Corporación de Administración Delegada",
                                         COD_DEPE2 %in% c(5) ~ "Servicio Local de Educación",)) %>% 
  mutate(SituacionFinal = case_when(SIT_FIN %in% c("P") ~ "Promovido",
                                    SIT_FIN %in% c("R") ~ "Reprobado",
                                    SIT_FIN %in% c("Y") ~ "Retirado")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0")) %>% 
  mutate(COD_REG_RBD = str_pad(COD_REG_RBD, width = 2, pad = "0"))

#########################3



#######################################


##Tablas de situación final###

sfinal2018 <- situacionfinal2018 %>% 
  filter(COD_PRO_RBD == 131) %>% 
  group_by(COD_COM_RBD, NOM_COM_RBD, SituacionFinal) %>% 
  summarise(n_estudiantes = n(), 
            .groups = "drop")

sfinal2018 <- sfinal2018 %>% 
  rename("codigo_comuna" = COD_COM_RBD) %>% 
  rename("nombre_comuna" = NOM_COM_RBD)

sfinal2019 <- situacionfinal2019 %>% 
  filter(COD_PRO_RBD == 131) %>% 
  group_by(COD_COM_RBD, NOM_COM_RBD, SituacionFinal) %>% 
  summarise(n_estudiantes = n(), 
            .groups = "drop")

sfinal2019 <- sfinal2019 %>%
  rename("codigo_comuna" = COD_COM_RBD) %>% 
  rename("nombre_comuna" = NOM_COM_RBD)

sfinal2020 <- situacionfinal2020 %>%
  filter(COD_PRO_RBD == 131) %>% 
  group_by(COD_COM_RBD, NOM_COM_RBD, SituacionFinal) %>% 
  summarise(n_estudiantes = n(), 
            .groups = "drop")

sfinal2020 <- sfinal2020 %>%
  rename("codigo_comuna" = COD_COM_RBD) %>% 
  rename("nombre_comuna" = NOM_COM_RBD)

sfinal2021 <- situacionfinal2021 %>%
  filter(COD_PRO_RBD == 131) %>% 
  group_by(COD_COM_RBD, NOM_COM_RBD, SituacionFinal) %>% 
  summarise(n_estudiantes = n(), 
            .groups = "drop")

sfinal2021 <- sfinal2021 %>%
  rename("codigo_comuna" = COD_COM_RBD) %>% 
  rename("nombre_comuna" = NOM_COM_RBD)

sfinal2022 <- situacionfinal2022 %>%
  filter(COD_PRO_RBD == 131) %>% 
  group_by(COD_COM_RBD, NOM_COM_RBD, SituacionFinal) %>% 
  summarise(n_estudiantes = n(), 
            .groups = "drop")

sfinal2022 <- sfinal2022 %>%
  rename("codigo_comuna" = COD_COM_RBD) %>% 
  rename("nombre_comuna" = NOM_COM_RBD)

sfinal2023 <- situacionfinal2023 %>%
  filter(COD_PRO_RBD == 131) %>% 
  group_by(COD_COM_RBD, NOM_COM_RBD, SituacionFinal) %>% 
  summarise(n_estudiantes = n(), 
            .groups = "drop")

sfinal2023 <- sfinal2023 %>%
  rename("codigo_comuna" = COD_COM_RBD) %>% 
  rename("nombre_comuna" = NOM_COM_RBD)


write_csv(sfinal2018, "situacionfinal2018.csv")
write_csv(sfinal2019, "situacionfinal2019.csv")
write_csv(sfinal2020, "situacionfinal2020.csv")
write_csv(sfinal2021, "situacionfinal2021.csv")
write_csv(sfinal2022, "situacionfinal2022.csv")
write_csv(sfinal2023, "situacionfinal2023.csv")





