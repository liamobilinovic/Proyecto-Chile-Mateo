###Script para app de shiny###

###Este script crea todas las tablas relacionadas al rendimmiento de los estudiantes entre####
###primero basico y quinto basico. Ademas, carga todas las bases de datos por lo que es muy####
###importante ejecutar este script al principio del proyecto###

###ESTE ES EL SCRIPT QUE EJECUTA TODO EL PROYECTO##

###Paquetes###
###Carga de paquetes###


if (!require("summarytools")) install.packages("summarytools")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dplyr")) install.packages("kableExtra")
if (!require("tidyr")) install.packages("tidyr")
if (!require("janitor")) install.packages("janitor")
if (!require("sjmisc")) install.packages("sjmisc")
if (!require("sjPlot")) install.packages("sjPlot")
if (!require("httpgd")) install.packages("httpgd")
if (!require("devtools")) install.packages("devtools")
if (!require("shiny")) install.packages("shiny")
if (!require("bslib")) install.packages("bslib")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggThemeAssist")) install.packages("ggThemeAssist")
if (!require("leaflet")) install.packages("leaflet")
if (!require("stringr")) install.packages("stringr")
if (!require("sf")) install.packages("sf")
if (!require("chilemapas")) install.packages("chilemapas")
if (!require("shinydashboard")) install.packages("shinydashboard")


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

unzip("CSVS.zip", exdir = "Bases de datos")

####2018####


###IDEA: guardar cada dataframe de promedios como una base independiente para que no pese tanto, Guardar comunas_santiago como archivo csv?#


kablevectores <- c("striped", "bordered", "responsive")



rendimientos2018 <- read_delim("Bases de datos/CSVS/20190220_Rendimiento_2018_20190131_PUBL.csv", 
                               delim = ";", quote = "'", escape_double = FALSE, 
                               trim_ws = TRUE)


tabla_rendimientos2018 <- rendimientos2018 %>% select(COD_REG_RBD, 
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
                                                      PROM_GRAL)



tabla_rendimientos2018 <- tabla_rendimientos2018 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(PROM_GRAL2 = case_when(PROM_GRAL %in% c(7, 70) ~ "Excelente (7,0)",
                                PROM_GRAL %in% c(6, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69) ~ "Satisfactorio (6,0-6,9)", 
                                PROM_GRAL %in% c(5, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59) ~ "Bueno (5,0-5,9)",
                                PROM_GRAL %in% c(4, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49) ~ "Suficiente (4,0-4,9)", 
                                PROM_GRAL %in% c(3, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39) ~ "Insuficiente (3,0-3,9)", 
                                PROM_GRAL %in% c(2, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Malo (2,0-2,9)", 
                                PROM_GRAL %in% c(1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19) ~ "Muy malo (1,0-1,9)")) %>%
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  mutate(Zona = case_when(COD_REG_RBD %in% c(1, 2, 3, 4, 15) ~ "Norte", 
                          COD_REG_RBD %in% c(5, 6, 13) ~ "Centro", 
                          COD_REG_RBD %in% c(7, 8, 9, 10, 11, 12, 14, 16) ~ "Sur")) %>% 
  filter(COD_ENSE2 ==2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0"))




###TOTAL NOTAS 2018###

Nt_cursos2018 <- tabla_rendimientos2018 %>% 
  tabyl(PROM_GRAL2, Curso) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_cursos2018) %>% 
  kable_classic(html_font = "Raleway") %>% 
  kable_styling(bootstrap_options = kablevectores)



###Estadisticos 2018###

mean(tabla_rendimientos2018$PROM_GRAL)

summary(tabla_rendimientos2018$PROM_GRAL)

####2019#######################################################################

rendimientos2019 <- read_delim("Bases de datos/CSVS/20200220_Rendimiento_2019_20200131_PUBL.csv", 
                               delim = ";", quote = "'", escape_double = FALSE, 
                               trim_ws = TRUE)



tabla_rendimientos2019 <- rendimientos2019 %>% select(COD_REG_RBD, 
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
                                                      PROM_GRAL)

tabla_rendimientos2019 <- tabla_rendimientos2019 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(PROM_GRAL2 = case_when(PROM_GRAL %in% c(7, 70) ~ "Excelente (7,0)",
                                PROM_GRAL %in% c(6, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69) ~ "Satisfactorio (6,0-6,9)", 
                                PROM_GRAL %in% c(5, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59) ~ "Bueno (5,0-5,9)",
                                PROM_GRAL %in% c(4, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49) ~ "Suficiente (4,0-4,9)", 
                                PROM_GRAL %in% c(3, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39) ~ "Insuficiente (3,0-3,9)", 
                                PROM_GRAL %in% c(2, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Malo (2,0-2,9)", 
                                PROM_GRAL %in% c(1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19) ~ "Muy malo (1,0-1,9)")) %>%
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  mutate(Zona = case_when(COD_REG_RBD %in% c(1, 2, 3, 4, 15) ~ "Norte", 
                          COD_REG_RBD %in% c(5, 6, 13) ~ "Centro", 
                          COD_REG_RBD %in% c(7, 8, 9, 10, 11, 12, 14, 16) ~ "Sur")) %>% 
  filter(COD_ENSE2 ==2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0"))

###Total notas 2020###


Nt_cursos2019 <- tabla_rendimientos2019 %>% 
  tabyl(PROM_GRAL2, Curso) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_cursos2019) %>% 
  kable_material(html_font = "Times New Roman") %>% 
  kable_styling(bootstrap_options = kablevectores)

kablevectores <- c("striped", "bordered", "responsive")




####2020#####################################################################

rendimientos2020 <- read_delim("Bases de datos/CSVS/20210223_Rendimiento_2020_20210131_WEB.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)


tabla_rendimientos2020 <- rendimientos2020 %>% select(COD_REG_RBD, 
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
                                                      PROM_GRAL)

tabla_rendimientos2020 <- tabla_rendimientos2020 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(PROM_GRAL2 = case_when(PROM_GRAL %in% c(7, 70) ~ "Excelente (7,0)",
                                PROM_GRAL %in% c(6, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69) ~ "Satisfactorio (6,0-6,9)", 
                                PROM_GRAL %in% c(5, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59) ~ "Bueno (5,0-5,9)",
                                PROM_GRAL %in% c(4, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49) ~ "Suficiente (4,0-4,9)", 
                                PROM_GRAL %in% c(3, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39) ~ "Insuficiente (3,0-3,9)", 
                                PROM_GRAL %in% c(2, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Malo (2,0-2,9)", 
                                PROM_GRAL %in% c(1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19) ~ "Muy malo (1,0-1,9)")) %>%
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  mutate(Zona = case_when(COD_REG_RBD %in% c(1, 2, 3, 4, 15) ~ "Norte", 
                          COD_REG_RBD %in% c(5, 6, 13) ~ "Centro", 
                          COD_REG_RBD %in% c(7, 8, 9, 10, 11, 12, 14, 16) ~ "Sur")) %>% 
  filter(COD_ENSE2 ==2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0"))

###Total notas 2020###

Nt_cursos2020 <- tabla_rendimientos2020 %>% 
  tabyl(PROM_GRAL2, Curso) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_cursos2020) %>% 
  kable_material(html_font = "Times New Roman") %>% 
  kable_styling(bootstrap_options = kablevectores)

###Estadisticos 2020###

mean(tabla_rendimientos2020$PROM_GRAL)

summary(tabla_rendimientos2020$PROM_GRAL)


###2021#######################################################################

rendimientos2021 <- read_delim("Bases de datos/CSVS/20220302_Rendimiento_2021_20220131_WEB.csv", 
                               delim = ";", quote = "'", escape_double = FALSE, trim_ws = TRUE)

tabla_rendimientos2021 <- rendimientos2021 %>% select(COD_REG_RBD, 
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
                                                      PROM_GRAL)

tabla_rendimientos2021 <- tabla_rendimientos2021 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(PROM_GRAL2 = case_when(PROM_GRAL %in% c(7, 70) ~ "Excelente (7,0)",
                                PROM_GRAL %in% c(6, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69) ~ "Satisfactorio (6,0-6,9)", 
                                PROM_GRAL %in% c(5, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59) ~ "Bueno (5,0-5,9)",
                                PROM_GRAL %in% c(4, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49) ~ "Suficiente (4,0-4,9)", 
                                PROM_GRAL %in% c(3, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39) ~ "Insuficiente (3,0-3,9)", 
                                PROM_GRAL %in% c(2, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Malo (2,0-2,9)", 
                                PROM_GRAL %in% c(1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19) ~ "Muy malo (1,0-1,9)")) %>%
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  mutate(Zona = case_when(COD_REG_RBD %in% c(1, 2, 3, 4, 15) ~ "Norte", 
                          COD_REG_RBD %in% c(5, 6, 13) ~ "Centro", 
                          COD_REG_RBD %in% c(7, 8, 9, 10, 11, 12, 14, 16) ~ "Sur")) %>% 
  filter(COD_ENSE2 ==2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0"))

###Total notas 2021###

Nt_cursos2021 <- tabla_rendimientos2021 %>% 
  tabyl(PROM_GRAL2, Curso) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_cursos2021) %>% 
  kable_material(html_font = "Times New Roman") %>% 
  kable_styling(bootstrap_options = kablevectores)

###Estadisticos 2021###

mean(tabla_rendimientos2021$PROM_GRAL)

summary(tabla_rendimientos2021$PROM_GRAL)

###2022#######################################################################

rendimientos2022 <- read_delim("Bases de datos/CSVS/20230209_Rendimiento_2022_20230131_WEB.csv", 
                               delim = ";", quote = "'", escape_double = FALSE, 
                               trim_ws = TRUE)

tabla_rendimientos2022 <- rendimientos2022 %>% select(COD_REG_RBD, 
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
                                                      PROM_GRAL)

tabla_rendimientos2022 <- tabla_rendimientos2022 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(PROM_GRAL2 = case_when(PROM_GRAL %in% c(7, 70) ~ "Excelente (7,0)",
                                PROM_GRAL %in% c(6, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69) ~ "Satisfactorio (6,0-6,9)", 
                                PROM_GRAL %in% c(5, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59) ~ "Bueno (5,0-5,9)",
                                PROM_GRAL %in% c(4, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49) ~ "Suficiente (4,0-4,9)", 
                                PROM_GRAL %in% c(3, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39) ~ "Insuficiente (3,0-3,9)", 
                                PROM_GRAL %in% c(2, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Malo (2,0-2,9)", 
                                PROM_GRAL %in% c(1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19) ~ "Muy malo (1,0-1,9)")) %>%
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  mutate(Zona = case_when(COD_REG_RBD %in% c(1, 2, 3, 4, 15) ~ "Norte", 
                          COD_REG_RBD %in% c(5, 6, 13) ~ "Centro", 
                          COD_REG_RBD %in% c(7, 8, 9, 10, 11, 12, 14, 16) ~ "Sur")) %>% 
  filter(COD_ENSE2 ==2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0"))



###Total notas 2022###

Nt_cursos2022 <- tabla_rendimientos2022 %>% 
  tabyl(PROM_GRAL2, Curso) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_cursos2022) %>% 
  kable_material(html_font = "Times New Roman") %>% 
  kable_styling(bootstrap_options = kablevectores)

###Estadisticos 2022###

mean(tabla_rendimientos2022$PROM_GRAL)

summary(tabla_rendimientos2022$PROM_GRAL)



####2023####

rendimientos2023 <- read_delim("Bases de datos/CSVS/20240209_Rendimiento_2023_20240131_WEB.csv", 
                               delim = ";", quote = "'", escape_double = FALSE, 
                               trim_ws = TRUE)

tabla_rendimientos2023 <- rendimientos2023 %>% select(COD_REG_RBD, 
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
                                                      PROM_GRAL)

tabla_rendimientos2023 <- tabla_rendimientos2023 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(PROM_GRAL2 = case_when(PROM_GRAL %in% c(7, 70) ~ "Excelente (7,0)",
                                PROM_GRAL %in% c(6, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69) ~ "Satisfactorio (6,0-6,9)", 
                                PROM_GRAL %in% c(5, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59) ~ "Bueno (5,0-5,9)",
                                PROM_GRAL %in% c(4, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49) ~ "Suficiente (4,0-4,9)", 
                                PROM_GRAL %in% c(3, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39) ~ "Insuficiente (3,0-3,9)", 
                                PROM_GRAL %in% c(2, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Malo (2,0-2,9)", 
                                PROM_GRAL %in% c(1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19) ~ "Muy malo (1,0-1,9)")) %>%
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>%  
  mutate(Zona = case_when(COD_REG_RBD %in% c(1, 2, 3, 4, 15) ~ "Norte", 
                          COD_REG_RBD %in% c(5, 6, 13) ~ "Centro", 
                          COD_REG_RBD %in% c(7, 8, 9, 10, 11, 12, 14, 16) ~ "Sur")) %>% 
  filter(COD_ENSE2 ==2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural")) %>% 
  mutate(COD_COM_RBD = str_pad(COD_COM_RBD, width = 5, pad = "0"))


###Total notas 2023###

Nt_cursos2023 <- tabla_rendimientos2022 %>% 
  tabyl(PROM_GRAL2, Curso) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_cursos2023) %>% 
  kable_material(html_font = "Times New Roman") %>% 
  kable_styling(bootstrap_options = kablevectores)





##############################################################################

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


################################################################## 

###En este script se crean las tablas de promedios de notas finales###
###en la comuna de santiago###


###2018###


promedios2018 <- tabla_rendimientos2018 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2018 <- merge(promedios2018, estudiantes_santiago2018, by = "as.character(COD_COM_RBD)")

promedios2018 <- promedios2018 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)



comunas_santiago2018 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2018) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")


comunas_santiago2018 <- st_as_sf(comunas_santiago2018)




######



###2019###

promedios2019 <- tabla_rendimientos2019 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2019 <- merge(promedios2019, estudiantes_santiago2019, by = "as.character(COD_COM_RBD)")

promedios2019 <- promedios2019 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2019 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2019) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2019 <- st_as_sf(comunas_santiago2019)




###2020###

promedios2020 <- tabla_rendimientos2020 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2020 <- merge(promedios2020, estudiantes_santiago2020, by = "as.character(COD_COM_RBD)")

promedios2020 <- promedios2020 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2020 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2020) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2020 <- st_as_sf(comunas_santiago2020)




###2021###

promedios2021 <- tabla_rendimientos2021 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2021 <- merge(promedios2021, estudiantes_santiago2021, by = "as.character(COD_COM_RBD)")

promedios2021 <- promedios2021 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)


comunas_santiago2021 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2021) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2021 <- st_as_sf(comunas_santiago2021)



###2022###

promedios2022 <- tabla_rendimientos2022 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2022 <- merge(promedios2022, estudiantes_santiago2022, by = "as.character(COD_COM_RBD)")

promedios2022 <- promedios2022 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2022 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2022) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2022 <- st_as_sf(comunas_santiago2022)


###2023###

promedios2023 <- tabla_rendimientos2023 %>% 
  group_by(as.character(COD_COM_RBD)) %>% 
  summarise(promedio = as.integer(mean(PROM_GRAL)))

promedios2023 <- merge(promedios2023, estudiantes_santiago2023, by = "as.character(COD_COM_RBD)")

promedios2023 <- promedios2023 %>% 
  rename("codigo_comuna" = `as.character(COD_COM_RBD)`)

comunas_santiago2023 <- mapa_zonas %>%
  filter(codigo_provincia == 131) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(promedios2023) %>% 
  group_by(nombre_comuna, codigo_comuna, promedio, n_estudiantes) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop")

comunas_santiago2023 <- st_as_sf(comunas_santiago2023)



comunas_santiago2018 <- st_transform(comunas_santiago2018, crs = 4326)
comunas_santiago2019 <- st_transform(comunas_santiago2019, crs = 4326)
comunas_santiago2020 <- st_transform(comunas_santiago2020, crs = 4326)
comunas_santiago2021 <- st_transform(comunas_santiago2021, crs = 4326)
comunas_santiago2022 <- st_transform(comunas_santiago2022, crs = 4326)
comunas_santiago2023 <- st_transform(comunas_santiago2023, crs = 4326)



###Mapas### 




bins <- c(40, 45, 50, 52, 54, 56, 58, 60, 62, 64, 65)

paleta <- c(
  "#DD0202", 
  "#C70319", 
  "#B10530", 
  "#9B0647", 
  "#85085E", 
  "#6F0976", 
  "#580A8D",
  "#420CA4", 
  "#2C0DBB", 
  "#160FD2", 
  "#0010E9")



pal <- colorBin(
  palette = paleta,
  domain = c(40, 65),
  bins = bins)



###2018###


mapa_santiago2018 <- leaflet(comunas_santiago2018) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2018$promedio),
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
    label = ~paste("Comuna: ", comunas_santiago2018$nombre_comuna, "", "Promedio: ", comunas_santiago2018$promedio)
  )


###2019###

mapa_santiago2019 <- leaflet(comunas_santiago2019) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2019$promedio),
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
    label = ~paste("Comuna: ", comunas_santiago2019$nombre_comuna, "", "Promedio: ", comunas_santiago2019$promedio)
  )

###2020###

mapa_santiago2020 <- leaflet(comunas_santiago2020) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2020$promedio),
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
    label = ~paste("Comuna: ", comunas_santiago2020$nombre_comuna, "", "Promedio: ", comunas_santiago2020$promedio)
  )

###2021###

mapa_santiago2021 <- leaflet(comunas_santiago2021) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2021$promedio),
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
    label = ~paste("Comuna: ", comunas_santiago2021$nombre_comuna, "", "Promedio: ", comunas_santiago2021$promedio)
  )

###2022###


mapa_santiago2022 <- leaflet(comunas_santiago2022) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2022$promedio),
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
    label = ~paste("Comuna: ", comunas_santiago2022$nombre_comuna, "", "Promedio: ", comunas_santiago2022$promedio)
  )

###2023###

mapa_santiago2023 <- leaflet(comunas_santiago2023) %>% 
  addTiles(
    urlTemplate = "",
    options = tileOptions(background = "white")
  ) %>% 
  addPolygons(
    fillColor = ~pal(comunas_santiago2023$promedio),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    layerId = ~nombre_comuna,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "white",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Comuna: ", comunas_santiago2023$nombre_comuna, "", "Promedio: ", comunas_santiago2023$promedio)
  )


