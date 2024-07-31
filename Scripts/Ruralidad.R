###Rural vs urbano####

####2018#### 

Nt_rural2018 <- tabla_rendimientos2018 %>% 
  tabyl(PROM_GRAL2, TipoRural) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting(digits = 2) %>% 
  adorn_ns()

kable(Nt_rural2018) %>% 
  kable_classic(html_font = "Tahoma") %>%
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  add_header_above(c("Promedios entre colegios rurales y urbanos 2018" = 3), bold = TRUE, font_size = 15)

####2019####

Nt_rural2019 <- tabla_rendimientos2019 %>% 
  tabyl(PROM_GRAL2, TipoRural) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting(digits = 2) %>% 
  adorn_ns()

kable(Nt_rural2019) %>% 
  kable_classic(html_font = "Tahoma") %>%
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  add_header_above(c("Promedios entre colegios rurales y urbanos 2019" = 3), bold = TRUE, font_size = 15)

####2020####

Nt_rural2020 <- tabla_rendimientos2020 %>% 
  tabyl(PROM_GRAL2, TipoRural) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting(digits = 2) %>% 
  adorn_ns()

kable(Nt_rural2020) %>% 
  kable_classic(html_font = "Tahoma") %>%
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  add_header_above(c("Promedios entre colegios rurales y urbanos 2020" = 3), bold = TRUE, font_size = 15)

