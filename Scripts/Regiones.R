### Aqui van las regiones### 

####2018####

CICL_BAS_2018 <- tabla_rendimientos2018 %>% 
  filter(COD_GRADO %in% c(1:5))


CICL_BAS_2018 <-CICL_BAS_2018 %>% mutate(Región = case_when(COD_REG_RBD %in% c(1) ~ "Región de Tarapacá",
                                              COD_REG_RBD %in% c(2) ~ "Región de Antofagasta",
                                              COD_REG_RBD %in% c(3) ~ "Región de Atacama",
                                              COD_REG_RBD %in% c(4) ~ "Región de Coquimbo",
                                              COD_REG_RBD %in% c(5) ~ "Región de Valparaíso",
                                              COD_REG_RBD %in% c(6) ~ "Región del Libertador Gral. Bernardo O’Higgins",
                                              COD_REG_RBD %in% c(7) ~ "Región del Maule",
                                              COD_REG_RBD %in% c(8) ~ "Región del Biobío",
                                              COD_REG_RBD %in% c(9) ~ "Región de la Araucanía",
                                              COD_REG_RBD %in% c(10) ~ "Región de Los Lagos",
                                              COD_REG_RBD %in% c(11) ~ "Región de Aysén del Gral. Carlos Ibáñez del Campo",
                                              COD_REG_RBD %in% c(12) ~ "Región de Magallanes y de la Antártica Chilena",
                                              COD_REG_RBD %in% c(13) ~ "Región Metropolitana de Santiago",
                                              COD_REG_RBD %in% c(14) ~ "Región de Los Ríos",
                                              COD_REG_RBD %in% c(15) ~ "Región de Arica y Parinacota",
                                              COD_REG_RBD %in% c(16) ~ "Región de Ñuble",
                                              ))
                           

Nt_regiones2018 <- CICL_BAS_2018 %>% 
  tabyl(PROM_GRAL2, Región) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_regiones2018) %>% 
  kable_classic(html_font = "Raleway") %>% 
  kable_styling(bootstrap_options = kablevectores)


kablevectores <- c("striped", "bordered", "responsive")

####2019####

CICL_BAS_2019 <- tabla_rendimientos2019 %>% 
  filter(COD_GRADO %in% c(1:5))

CICL_BAS_2019 <-CICL_BAS_2019 %>% mutate(Región = case_when(COD_REG_RBD %in% c(1) ~ "Región de Tarapacá",
                                                            COD_REG_RBD %in% c(2) ~ "Región de Antofagasta",
                                                            COD_REG_RBD %in% c(3) ~ "Región de Atacama",
                                                            COD_REG_RBD %in% c(4) ~ "Región de Coquimbo",
                                                            COD_REG_RBD %in% c(5) ~ "Región de Valparaíso",
                                                            COD_REG_RBD %in% c(6) ~ "Región del Libertador Gral. Bernardo O’Higgins",
                                                            COD_REG_RBD %in% c(7) ~ "Región del Maule",
                                                            COD_REG_RBD %in% c(8) ~ "Región del Biobío",
                                                            COD_REG_RBD %in% c(9) ~ "Región de la Araucanía",
                                                            COD_REG_RBD %in% c(10) ~ "Región de Los Lagos",
                                                            COD_REG_RBD %in% c(11) ~ "Región de Aysén del Gral. Carlos Ibáñez del Campo",
                                                            COD_REG_RBD %in% c(12) ~ "Región de Magallanes y de la Antártica Chilena",
                                                            COD_REG_RBD %in% c(13) ~ "Región Metropolitana de Santiago",
                                                            COD_REG_RBD %in% c(14) ~ "Región de Los Ríos",
                                                            COD_REG_RBD %in% c(15) ~ "Región de Arica y Parinacota",
                                                            COD_REG_RBD %in% c(16) ~ "Región de Ñuble",
))


Nt_regiones2019 <- CICL_BAS_2019 %>% 
  tabyl(PROM_GRAL2, Región) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_regiones2019) %>% 
  kable_classic(html_font = "Raleway") %>%
  kable_classic_2() %>% 
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  column_spec(1:17) %>% 
  row_spec(0, bold = TRUE)

####2020####

CICL_BAS_2020 <- tabla_rendimientos2020 %>% 
  filter(COD_GRADO %in% c(1:5))


CICL_BAS_2020 <-CICL_BAS_2020 %>% mutate(Región = case_when(COD_REG_RBD %in% c(1) ~ "Región de Tarapacá",
                                                            COD_REG_RBD %in% c(2) ~ "Región de Antofagasta",
                                                            COD_REG_RBD %in% c(3) ~ "Región de Atacama",
                                                            COD_REG_RBD %in% c(4) ~ "Región de Coquimbo",
                                                            COD_REG_RBD %in% c(5) ~ "Región de Valparaíso",
                                                            COD_REG_RBD %in% c(6) ~ "Región del Libertador Gral. Bernardo O’Higgins",
                                                            COD_REG_RBD %in% c(7) ~ "Región del Maule",
                                                            COD_REG_RBD %in% c(8) ~ "Región del Biobío",
                                                            COD_REG_RBD %in% c(9) ~ "Región de la Araucanía",
                                                            COD_REG_RBD %in% c(10) ~ "Región de Los Lagos",
                                                            COD_REG_RBD %in% c(11) ~ "Región de Aysén del Gral. Carlos Ibáñez del Campo",
                                                            COD_REG_RBD %in% c(12) ~ "Región de Magallanes y de la Antártica Chilena",
                                                            COD_REG_RBD %in% c(13) ~ "Región Metropolitana de Santiago",
                                                            COD_REG_RBD %in% c(14) ~ "Región de Los Ríos",
                                                            COD_REG_RBD %in% c(15) ~ "Región de Arica y Parinacota",
                                                            COD_REG_RBD %in% c(16) ~ "Región de Ñuble",
))


Nt_regiones2020 <- CICL_BAS_2020 %>% 
  tabyl(PROM_GRAL2, Región) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_regiones2020) %>% 
  kable_classic(html_font = "Raleway") %>%
  kable_paper() %>% 
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  column_spec(1:17) %>% 
  row_spec(0, bold = TRUE)


####2021####

CICL_BAS_2021 <- tabla_rendimientos2021 %>% 
  filter(COD_GRADO %in% c(1:5))


CICL_BAS_2021 <-CICL_BAS_2021 %>% mutate(Región = case_when(COD_REG_RBD %in% c(1) ~ "Región de Tarapacá",
                                                            COD_REG_RBD %in% c(2) ~ "Región de Antofagasta",
                                                            COD_REG_RBD %in% c(3) ~ "Región de Atacama",
                                                            COD_REG_RBD %in% c(4) ~ "Región de Coquimbo",
                                                            COD_REG_RBD %in% c(5) ~ "Región de Valparaíso",
                                                            COD_REG_RBD %in% c(6) ~ "Región del Libertador Gral. Bernardo O’Higgins",
                                                            COD_REG_RBD %in% c(7) ~ "Región del Maule",
                                                            COD_REG_RBD %in% c(8) ~ "Región del Biobío",
                                                            COD_REG_RBD %in% c(9) ~ "Región de la Araucanía",
                                                            COD_REG_RBD %in% c(10) ~ "Región de Los Lagos",
                                                            COD_REG_RBD %in% c(11) ~ "Región de Aysén del Gral. Carlos Ibáñez del Campo",
                                                            COD_REG_RBD %in% c(12) ~ "Región de Magallanes y de la Antártica Chilena",
                                                            COD_REG_RBD %in% c(13) ~ "Región Metropolitana de Santiago",
                                                            COD_REG_RBD %in% c(14) ~ "Región de Los Ríos",
                                                            COD_REG_RBD %in% c(15) ~ "Región de Arica y Parinacota",
                                                            COD_REG_RBD %in% c(16) ~ "Región de Ñuble",
))


Nt_regiones2021 <- CICL_BAS_2021 %>% 
  tabyl(PROM_GRAL2, Región) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_regiones2021) %>% 
  kable_classic(html_font = "Raleway") %>%
  kable_paper() %>% 
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  column_spec(1:17) %>% 
  row_spec(0, bold = TRUE)

####2022####

CICL_BAS_2022 <- tabla_rendimientos2022 %>% 
  filter(COD_GRADO %in% c(1:5))


CICL_BAS_2022 <-CICL_BAS_2022 %>% mutate(Región = case_when(COD_REG_RBD %in% c(1) ~ "Región de Tarapacá",
                                                            COD_REG_RBD %in% c(2) ~ "Región de Antofagasta",
                                                            COD_REG_RBD %in% c(3) ~ "Región de Atacama",
                                                            COD_REG_RBD %in% c(4) ~ "Región de Coquimbo",
                                                            COD_REG_RBD %in% c(5) ~ "Región de Valparaíso",
                                                            COD_REG_RBD %in% c(6) ~ "Región del Libertador Gral. Bernardo O’Higgins",
                                                            COD_REG_RBD %in% c(7) ~ "Región del Maule",
                                                            COD_REG_RBD %in% c(8) ~ "Región del Biobío",
                                                            COD_REG_RBD %in% c(9) ~ "Región de la Araucanía",
                                                            COD_REG_RBD %in% c(10) ~ "Región de Los Lagos",
                                                            COD_REG_RBD %in% c(11) ~ "Región de Aysén del Gral. Carlos Ibáñez del Campo",
                                                            COD_REG_RBD %in% c(12) ~ "Región de Magallanes y de la Antártica Chilena",
                                                            COD_REG_RBD %in% c(13) ~ "Región Metropolitana de Santiago",
                                                            COD_REG_RBD %in% c(14) ~ "Región de Los Ríos",
                                                            COD_REG_RBD %in% c(15) ~ "Región de Arica y Parinacota",
                                                            COD_REG_RBD %in% c(16) ~ "Región de Ñuble",
))


Nt_regiones2022 <- CICL_BAS_2022 %>% 
  tabyl(PROM_GRAL2, Región) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_regiones2022) %>% 
  kable_classic(html_font = "Raleway") %>%
  kable_paper() %>% 
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  column_spec(1:17) %>% 
  row_spec(0, bold = TRUE)

####2023####

CICL_BAS_2023 <- tabla_rendimientos2023 %>% 
  filter(COD_GRADO %in% c(1:5))


CICL_BAS_2023 <-CICL_BAS_2023 %>% mutate(Región = case_when(COD_REG_RBD %in% c(1) ~ "Región de Tarapacá",
                                                            COD_REG_RBD %in% c(2) ~ "Región de Antofagasta",
                                                            COD_REG_RBD %in% c(3) ~ "Región de Atacama",
                                                            COD_REG_RBD %in% c(4) ~ "Región de Coquimbo",
                                                            COD_REG_RBD %in% c(5) ~ "Región de Valparaíso",
                                                            COD_REG_RBD %in% c(6) ~ "Región del Libertador Gral. Bernardo O’Higgins",
                                                            COD_REG_RBD %in% c(7) ~ "Región del Maule",
                                                            COD_REG_RBD %in% c(8) ~ "Región del Biobío",
                                                            COD_REG_RBD %in% c(9) ~ "Región de la Araucanía",
                                                            COD_REG_RBD %in% c(10) ~ "Región de Los Lagos",
                                                            COD_REG_RBD %in% c(11) ~ "Región de Aysén del Gral. Carlos Ibáñez del Campo",
                                                            COD_REG_RBD %in% c(12) ~ "Región de Magallanes y de la Antártica Chilena",
                                                            COD_REG_RBD %in% c(13) ~ "Región Metropolitana de Santiago",
                                                            COD_REG_RBD %in% c(14) ~ "Región de Los Ríos",
                                                            COD_REG_RBD %in% c(15) ~ "Región de Arica y Parinacota",
                                                            COD_REG_RBD %in% c(16) ~ "Región de Ñuble",
))


Nt_regiones2023 <- CICL_BAS_2023 %>% 
  tabyl(PROM_GRAL2, Región) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()

kable(Nt_regiones2022) %>% 
  kable_classic(html_font = "Raleway") %>%
  kable_paper() %>% 
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  column_spec(1:17) %>% 
  row_spec(0, bold = TRUE)
