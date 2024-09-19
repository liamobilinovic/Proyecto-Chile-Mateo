###Provincia de Santiago###

# este solo es de tablas #





####2018####

STGO_BAS_2018 <- tabla_rendimientos2018 %>% 
  filter(COD_GRADO %in% c(1:5))

STGO_BAS_2018 <- STGO_BAS_2018 %>% 
  filter(COD_PRO_RBD == 131)

STGO_BAS_2018 <- STGO_BAS_2018 %>% 
  filter(COD_COM_RBD %in% c(13101,
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
                            13132))

STGO_BAS_2018 <- STGO_BAS_2018 %>% 
  mutate(ComunaEst = case_when(COD_COM_RBD %in% c(13101) ~ "Santiago",
                               COD_COM_RBD %in% c(13102) ~ "Cerrilos",
                               COD_COM_RBD %in% c(13103) ~ "Cerro Navia",
                               COD_COM_RBD %in% c(13104) ~ "Conchalí",
                               COD_COM_RBD %in% c(13105) ~ "El Bosque",
                               COD_COM_RBD %in% c(13106) ~ "Estación Central",
                               COD_COM_RBD %in% c(13107) ~ "Huechuraba",
                               COD_COM_RBD %in% c(13108) ~ "Independencia",
                               COD_COM_RBD %in% c(13109) ~ "La cisterna",
                               COD_COM_RBD %in% c(13110) ~ "La Florida",
                               COD_COM_RBD %in% c(13111) ~ "La Granja",
                               COD_COM_RBD %in% c(13112) ~ "La Pintana",
                               COD_COM_RBD %in% c(13113) ~ "La Reina",
                               COD_COM_RBD %in% c(13114) ~ "Las Condes",
                               COD_COM_RBD %in% c(13115) ~ "Lo Barnechea",
                               COD_COM_RBD %in% c(13116) ~ "Lo Espejo",
                               COD_COM_RBD %in% c(13117) ~ "Lo Prado",
                               COD_COM_RBD %in% c(13118) ~ "Macul",
                               COD_COM_RBD %in% c(13119) ~ "Maipú",
                               COD_COM_RBD %in% c(13120) ~ "Ñuñoa",
                               COD_COM_RBD %in% c(13121) ~ "Pedro Aguirre Cerda",
                               COD_COM_RBD %in% c(13122) ~ "Peñalolen",
                               COD_COM_RBD %in% c(13123) ~ "Providencia",
                               COD_COM_RBD %in% c(13124) ~ "Pudahuel",
                               COD_COM_RBD %in% c(13125) ~ "Quilicura",
                               COD_COM_RBD %in% c(13126) ~ "Quinta Normal",
                               COD_COM_RBD %in% c(13127) ~ "Recoleta",
                               COD_COM_RBD %in% c(13128) ~ "Renca",
                               COD_COM_RBD %in% c(13129) ~ "San Joaquin",
                               COD_COM_RBD %in% c(13130) ~ "San Miguel",
                               COD_COM_RBD %in% c(13131) ~ "San Ramon",
                               COD_COM_RBD %in% c(13132) ~ "Vitacura"))




Nt_stgo2018 <- STGO_BAS_2018 %>% 
  tabyl(ComunaEst, PROM_GRAL2) %>% 
  adorn_percentages("row") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()



Nt_stgo2018 <- Nt_stgo2018 %>% relocate(ComunaEst, `Excelente (7,0)`, 
                                        `Satisfactorio (6,0-6,9)`, 
                                        `Bueno (5,0-5,9)`,
                                        `Suficiente (4,0-4,9)`,
                                        `Insuficiente (3,0-3,9)`,
                                        `Malo (2,0-2,9)`,
                                        `Muy malo (1,0-1,9)`)

kable(Nt_stgo2018) %>% 
  kable_classic(html_font = "Raleway") %>%
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  add_header_above(c("Promedios por comuna Provincia de Santiago 2018" = 9), bold = TRUE, font_size = 22)



####2019####

STGO_BAS_2019 <- tabla_rendimientos2019 %>% 
  filter(COD_GRADO %in% c(1:5))

STGO_BAS_2019 <- STGO_BAS_2019 %>% 
  filter(COD_PRO_RBD == 131)

STGO_BAS_2019 <- STGO_BAS_2019 %>% 
  filter(COD_COM_RBD %in% c(13101,
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
                            13132))

STGO_BAS_2019 <- STGO_BAS_2019 %>% 
  mutate(ComunaEst = case_when(COD_COM_RBD %in% c(13101) ~ "Santiago",
                               COD_COM_RBD %in% c(13102) ~ "Cerrilos",
                               COD_COM_RBD %in% c(13103) ~ "Cerro Navia",
                               COD_COM_RBD %in% c(13104) ~ "Conchalí",
                               COD_COM_RBD %in% c(13105) ~ "El Bosque",
                               COD_COM_RBD %in% c(13106) ~ "Estación Central",
                               COD_COM_RBD %in% c(13107) ~ "Huechuraba",
                               COD_COM_RBD %in% c(13108) ~ "Independencia",
                               COD_COM_RBD %in% c(13109) ~ "La cisterna",
                               COD_COM_RBD %in% c(13110) ~ "La Florida",
                               COD_COM_RBD %in% c(13111) ~ "La Granja",
                               COD_COM_RBD %in% c(13112) ~ "La Pintana",
                               COD_COM_RBD %in% c(13113) ~ "La Reina",
                               COD_COM_RBD %in% c(13114) ~ "Las Condes",
                               COD_COM_RBD %in% c(13115) ~ "Lo Barnechea",
                               COD_COM_RBD %in% c(13116) ~ "Lo Espejo",
                               COD_COM_RBD %in% c(13117) ~ "Lo Prado",
                               COD_COM_RBD %in% c(13118) ~ "Macul",
                               COD_COM_RBD %in% c(13119) ~ "Maipú",
                               COD_COM_RBD %in% c(13120) ~ "Ñuñoa",
                               COD_COM_RBD %in% c(13121) ~ "Pedro Aguirre Cerda",
                               COD_COM_RBD %in% c(13122) ~ "Peñalolen",
                               COD_COM_RBD %in% c(13123) ~ "Providencia",
                               COD_COM_RBD %in% c(13124) ~ "Pudahuel",
                               COD_COM_RBD %in% c(13125) ~ "Quilicura",
                               COD_COM_RBD %in% c(13126) ~ "Quinta Normal",
                               COD_COM_RBD %in% c(13127) ~ "Recoleta",
                               COD_COM_RBD %in% c(13128) ~ "Renca",
                               COD_COM_RBD %in% c(13129) ~ "San Joaquin",
                               COD_COM_RBD %in% c(13130) ~ "San Miguel",
                               COD_COM_RBD %in% c(13131) ~ "San Ramon",
                               COD_COM_RBD %in% c(13132) ~ "Vitacura"))




Nt_stgo2019 <- STGO_BAS_2019 %>% 
  tabyl(ComunaEst, PROM_GRAL2) %>% 
  adorn_percentages("row") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

Nt_stgo2019 <- Nt_stgo2019 %>% relocate(ComunaEst, `Excelente (7,0)`, 
                         `Satisfactorio (6,0-6,9)`, 
                         `Bueno (5,0-5,9)`,
                         `Suficiente (4,0-4,9)`,
                         `Insuficiente (3,0-3,9)`,
                         `Malo (2,0-2,9)`,
                         `Muy malo (1,0-1,9)`)

kable(Nt_stgo2019) %>% 
  kable_classic(html_font = "Raleway") %>% 
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  add_header_above(c("Promedio de notas por comuna Provincia de Santiago 2019" = 9), bold = TRUE, font_size = 18)
  



####2020####

STGO_BAS_2020 <- tabla_rendimientos2020 %>% 
  filter(COD_GRADO %in% c(1:5))

STGO_BAS_2020 <- STGO_BAS_2020 %>% 
  filter(COD_PRO_RBD == 131)

STGO_BAS_2020 <- STGO_BAS_2020 %>% 
  filter(COD_COM_RBD %in% c(13101,
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
                            13132))

STGO_BAS_2020 <- STGO_BAS_2020 %>% 
  mutate(ComunaEst = case_when(COD_COM_RBD %in% c(13101) ~ "Santiago",
                               COD_COM_RBD %in% c(13102) ~ "Cerrilos",
                               COD_COM_RBD %in% c(13103) ~ "Cerro Navia",
                               COD_COM_RBD %in% c(13104) ~ "Conchalí",
                               COD_COM_RBD %in% c(13105) ~ "El Bosque",
                               COD_COM_RBD %in% c(13106) ~ "Estación Central",
                               COD_COM_RBD %in% c(13107) ~ "Huechuraba",
                               COD_COM_RBD %in% c(13108) ~ "Independencia",
                               COD_COM_RBD %in% c(13109) ~ "La cisterna",
                               COD_COM_RBD %in% c(13110) ~ "La Florida",
                               COD_COM_RBD %in% c(13111) ~ "La Granja",
                               COD_COM_RBD %in% c(13112) ~ "La Pintana",
                               COD_COM_RBD %in% c(13113) ~ "La Reina",
                               COD_COM_RBD %in% c(13114) ~ "Las Condes",
                               COD_COM_RBD %in% c(13115) ~ "Lo Barnechea",
                               COD_COM_RBD %in% c(13116) ~ "Lo Espejo",
                               COD_COM_RBD %in% c(13117) ~ "Lo Prado",
                               COD_COM_RBD %in% c(13118) ~ "Macul",
                               COD_COM_RBD %in% c(13119) ~ "Maipú",
                               COD_COM_RBD %in% c(13120) ~ "Ñuñoa",
                               COD_COM_RBD %in% c(13121) ~ "Pedro Aguirre Cerda",
                               COD_COM_RBD %in% c(13122) ~ "Peñalolen",
                               COD_COM_RBD %in% c(13123) ~ "Providencia",
                               COD_COM_RBD %in% c(13124) ~ "Pudahuel",
                               COD_COM_RBD %in% c(13125) ~ "Quilicura",
                               COD_COM_RBD %in% c(13126) ~ "Quinta Normal",
                               COD_COM_RBD %in% c(13127) ~ "Recoleta",
                               COD_COM_RBD %in% c(13128) ~ "Renca",
                               COD_COM_RBD %in% c(13129) ~ "San Joaquin",
                               COD_COM_RBD %in% c(13130) ~ "San Miguel",
                               COD_COM_RBD %in% c(13131) ~ "San Ramon",
                               COD_COM_RBD %in% c(13132) ~ "Vitacura"))




Nt_stgo2020 <- STGO_BAS_2020 %>% 
  tabyl(ComunaEst, PROM_GRAL2) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

Nt_stgo2020 <- Nt_stgo2020 %>% relocate(ComunaEst, `Excelente (7,0)`, 
                                        `Satisfactorio (6,0-6,9)`, 
                                        `Bueno (5,0-5,9)`,
                                        `Suficiente (4,0-4,9)`,
                                        `Insuficiente (3,0-3,9)`,
                                        `Malo (2,0-2,9)`,
                                        `Muy malo (1,0-1,9)`)

kable(Nt_stgo2020) %>% 
  kable_classic(html_font = "Raleway") %>% 
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  add_header_above(c("Promedios por comuna Provincia de Santiago 2020" = 9), bold = TRUE, font_size = 22)




####2021####


STGO_BAS_2021 <- tabla_rendimientos2021 %>% 
  filter(COD_GRADO %in% c(1:5))

STGO_BAS_2021 <- STGO_BAS_2021 %>% 
  filter(COD_PRO_RBD == 131)

STGO_BAS_2021 <- STGO_BAS_2021 %>% 
  filter(COD_COM_RBD %in% c(13101,
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
                            13132))

STGO_BAS_2021 <- STGO_BAS_2021 %>% 
  mutate(ComunaEst = case_when(COD_COM_RBD %in% c(13101) ~ "Santiago",
                               COD_COM_RBD %in% c(13102) ~ "Cerrilos",
                               COD_COM_RBD %in% c(13103) ~ "Cerro Navia",
                               COD_COM_RBD %in% c(13104) ~ "Conchalí",
                               COD_COM_RBD %in% c(13105) ~ "El Bosque",
                               COD_COM_RBD %in% c(13106) ~ "Estación Central",
                               COD_COM_RBD %in% c(13107) ~ "Huechuraba",
                               COD_COM_RBD %in% c(13108) ~ "Independencia",
                               COD_COM_RBD %in% c(13109) ~ "La cisterna",
                               COD_COM_RBD %in% c(13110) ~ "La Florida",
                               COD_COM_RBD %in% c(13111) ~ "La Granja",
                               COD_COM_RBD %in% c(13112) ~ "La Pintana",
                               COD_COM_RBD %in% c(13113) ~ "La Reina",
                               COD_COM_RBD %in% c(13114) ~ "Las Condes",
                               COD_COM_RBD %in% c(13115) ~ "Lo Barnechea",
                               COD_COM_RBD %in% c(13116) ~ "Lo Espejo",
                               COD_COM_RBD %in% c(13117) ~ "Lo Prado",
                               COD_COM_RBD %in% c(13118) ~ "Macul",
                               COD_COM_RBD %in% c(13119) ~ "Maipú",
                               COD_COM_RBD %in% c(13120) ~ "Ñuñoa",
                               COD_COM_RBD %in% c(13121) ~ "Pedro Aguirre Cerda",
                               COD_COM_RBD %in% c(13122) ~ "Peñalolen",
                               COD_COM_RBD %in% c(13123) ~ "Providencia",
                               COD_COM_RBD %in% c(13124) ~ "Pudahuel",
                               COD_COM_RBD %in% c(13125) ~ "Quilicura",
                               COD_COM_RBD %in% c(13126) ~ "Quinta Normal",
                               COD_COM_RBD %in% c(13127) ~ "Recoleta",
                               COD_COM_RBD %in% c(13128) ~ "Renca",
                               COD_COM_RBD %in% c(13129) ~ "San Joaquin",
                               COD_COM_RBD %in% c(13130) ~ "San Miguel",
                               COD_COM_RBD %in% c(13131) ~ "San Ramon",
                               COD_COM_RBD %in% c(13132) ~ "Vitacura"))




Nt_stgo2021 <- STGO_BAS_2021 %>% 
  tabyl(ComunaEst, PROM_GRAL2) %>% 
  adorn_percentages("row") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

Nt_stgo2021 <- Nt_stgo2021 %>% relocate(ComunaEst, `Excelente (7,0)`, 
                                        `Satisfactorio (6,0-6,9)`, 
                                        `Bueno (5,0-5,9)`,
                                        `Suficiente (4,0-4,9)`,
                                        `Insuficiente (3,0-3,9)`,
                                        `Malo (2,0-2,9)`,
                                        `Muy malo (1,0-1,9)`)

kable(Nt_stgo2021) %>% 
  kable_classic(html_font = "Tahoma") %>%
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  add_header_above(c("Promedios por comuna Provincia de Santiago" = 9), bold = TRUE, font_size = 22)



####2022####


STGO_BAS_2022 <- tabla_rendimientos2022 %>% 
  filter(COD_GRADO %in% c(1:5))

STGO_BAS_2022 <- STGO_BAS_2022 %>% 
  filter(COD_PRO_RBD == 131)

STGO_BAS_2022 <- STGO_BAS_2022 %>% 
  filter(COD_COM_RBD %in% c(13101,
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
                            13132))

STGO_BAS_2022 <- STGO_BAS_2022 %>% 
  mutate(ComunaEst = case_when(COD_COM_RBD %in% c(13101) ~ "Santiago",
                               COD_COM_RBD %in% c(13102) ~ "Cerrilos",
                               COD_COM_RBD %in% c(13103) ~ "Cerro Navia",
                               COD_COM_RBD %in% c(13104) ~ "Conchalí",
                               COD_COM_RBD %in% c(13105) ~ "El Bosque",
                               COD_COM_RBD %in% c(13106) ~ "Estación Central",
                               COD_COM_RBD %in% c(13107) ~ "Huechuraba",
                               COD_COM_RBD %in% c(13108) ~ "Independencia",
                               COD_COM_RBD %in% c(13109) ~ "La cisterna",
                               COD_COM_RBD %in% c(13110) ~ "La Florida",
                               COD_COM_RBD %in% c(13111) ~ "La Granja",
                               COD_COM_RBD %in% c(13112) ~ "La Pintana",
                               COD_COM_RBD %in% c(13113) ~ "La Reina",
                               COD_COM_RBD %in% c(13114) ~ "Las Condes",
                               COD_COM_RBD %in% c(13115) ~ "Lo Barnechea",
                               COD_COM_RBD %in% c(13116) ~ "Lo Espejo",
                               COD_COM_RBD %in% c(13117) ~ "Lo Prado",
                               COD_COM_RBD %in% c(13118) ~ "Macul",
                               COD_COM_RBD %in% c(13119) ~ "Maipú",
                               COD_COM_RBD %in% c(13120) ~ "Ñuñoa",
                               COD_COM_RBD %in% c(13121) ~ "Pedro Aguirre Cerda",
                               COD_COM_RBD %in% c(13122) ~ "Peñalolen",
                               COD_COM_RBD %in% c(13123) ~ "Providencia",
                               COD_COM_RBD %in% c(13124) ~ "Pudahuel",
                               COD_COM_RBD %in% c(13125) ~ "Quilicura",
                               COD_COM_RBD %in% c(13126) ~ "Quinta Normal",
                               COD_COM_RBD %in% c(13127) ~ "Recoleta",
                               COD_COM_RBD %in% c(13128) ~ "Renca",
                               COD_COM_RBD %in% c(13129) ~ "San Joaquin",
                               COD_COM_RBD %in% c(13130) ~ "San Miguel",
                               COD_COM_RBD %in% c(13131) ~ "San Ramon",
                               COD_COM_RBD %in% c(13132) ~ "Vitacura"))




Nt_stgo2022 <- STGO_BAS_2022 %>% 
  tabyl(ComunaEst, PROM_GRAL2) %>% 
  adorn_percentages("row") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()



Nt_stgo2022 <- Nt_stgo2022 %>% relocate(ComunaEst, `Excelente (7,0)`, 
                                        `Satisfactorio (6,0-6,9)`, 
                                        `Bueno (5,0-5,9)`,
                                        `Suficiente (4,0-4,9)`,
                                        `Insuficiente (3,0-3,9)`,
                                        `Malo (2,0-2,9)`,
                                        `Muy malo (1,0-1,9)`)


kable(Nt_stgo2022) %>% 
  kable_classic(html_font = "Tahoma") %>% 
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  add_header_above(c("Promedios por comuna Provincia de Santiago 2022" = 9), bold = TRUE, font_size = 22)


####2023####


STGO_BAS_2023 <- tabla_rendimientos2023 %>% 
  filter(COD_GRADO %in% c(1:5))

STGO_BAS_2023 <- STGO_BAS_2023 %>% 
  filter(COD_PRO_RBD == 131)

STGO_BAS_2023 <- STGO_BAS_2023 %>% 
  filter(COD_COM_RBD %in% c(13101,
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
                            13132))

STGO_BAS_2023 <- STGO_BAS_2023 %>% 
  mutate(ComunaEst = case_when(COD_COM_RBD %in% c(13101) ~ "Santiago",
                               COD_COM_RBD %in% c(13102) ~ "Cerrilos",
                               COD_COM_RBD %in% c(13103) ~ "Cerro Navia",
                               COD_COM_RBD %in% c(13104) ~ "Conchalí",
                               COD_COM_RBD %in% c(13105) ~ "El Bosque",
                               COD_COM_RBD %in% c(13106) ~ "Estación Central",
                               COD_COM_RBD %in% c(13107) ~ "Huechuraba",
                               COD_COM_RBD %in% c(13108) ~ "Independencia",
                               COD_COM_RBD %in% c(13109) ~ "La cisterna",
                               COD_COM_RBD %in% c(13110) ~ "La Florida",
                               COD_COM_RBD %in% c(13111) ~ "La Granja",
                               COD_COM_RBD %in% c(13112) ~ "La Pintana",
                               COD_COM_RBD %in% c(13113) ~ "La Reina",
                               COD_COM_RBD %in% c(13114) ~ "Las Condes",
                               COD_COM_RBD %in% c(13115) ~ "Lo Barnechea",
                               COD_COM_RBD %in% c(13116) ~ "Lo Espejo",
                               COD_COM_RBD %in% c(13117) ~ "Lo Prado",
                               COD_COM_RBD %in% c(13118) ~ "Macul",
                               COD_COM_RBD %in% c(13119) ~ "Maipú",
                               COD_COM_RBD %in% c(13120) ~ "Ñuñoa",
                               COD_COM_RBD %in% c(13121) ~ "Pedro Aguirre Cerda",
                               COD_COM_RBD %in% c(13122) ~ "Peñalolen",
                               COD_COM_RBD %in% c(13123) ~ "Providencia",
                               COD_COM_RBD %in% c(13124) ~ "Pudahuel",
                               COD_COM_RBD %in% c(13125) ~ "Quilicura",
                               COD_COM_RBD %in% c(13126) ~ "Quinta Normal",
                               COD_COM_RBD %in% c(13127) ~ "Recoleta",
                               COD_COM_RBD %in% c(13128) ~ "Renca",
                               COD_COM_RBD %in% c(13129) ~ "San Joaquin",
                               COD_COM_RBD %in% c(13130) ~ "San Miguel",
                               COD_COM_RBD %in% c(13131) ~ "San Ramon",
                               COD_COM_RBD %in% c(13132) ~ "Vitacura"))




Nt_stgo2023 <- STGO_BAS_2023 %>% 
  tabyl(ComunaEst, PROM_GRAL2) %>% 
  adorn_percentages("row") %>% 
  adorn_totals("col") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

Nt_stgo2023 <- Nt_stgo2023 %>% relocate(ComunaEst, `Excelente (7,0)`, 
                                        `Satisfactorio (6,0-6,9)`, 
                                        `Bueno (5,0-5,9)`,
                                        `Suficiente (4,0-4,9)`,
                                        `Insuficiente (3,0-3,9)`,
                                        `Malo (2,0-2,9)`,
                                        `Muy malo (1,0-1,9)`)


kable(Nt_stgo2023) %>% 
  kable_classic(html_font = "Tahoma") %>% 
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  add_header_above(c("Promedios según comuna Provincia de Santiago 2023" = 9), bold = TRUE, font_size = 22)

mean(tabla_rendimientos2023$PROM_GRAL)


  


