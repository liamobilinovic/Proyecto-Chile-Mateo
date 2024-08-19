###Rural vs urbano####

###aqui veremos las diferencias de promedio entre rural y urbano###


###2018###

promedio_rural2018 <- tabla_rendimientos2018 %>% 
  group_by(as.character(TipoRural)) %>% 
  summarise(promedio = mean(PROM_GRAL))


promedio_rural2018 <- promedio_rural2018 %>%
  mutate(prop = promedio / sum(promedio) * 100,
         ypos = cumsum(prop) - 0.5 * prop)

promedio_rural2018 <- promedio_rural2018 %>% 
  rename("Tipo de zona" = `as.character(TipoRural)`)



###2019##

promedio_rural2019 <- tabla_rendimientos2019 %>% 
  group_by(as.character(TipoRural)) %>% 
  summarise(promedio = mean(PROM_GRAL))

promedio_rural2019 <- promedio_rural2019 %>%
  mutate(prop = promedio / sum(promedio) * 100,
         ypos = cumsum(prop) - 0.5 * prop)

promedio_rural2019 <- promedio_rural2019 %>% 
  rename("Tipo de zona" = `as.character(TipoRural)`)

###2020###


promedio_rural2020 <- tabla_rendimientos2020 %>% 
  group_by(as.character(TipoRural)) %>% 
  summarise(promedio = mean(PROM_GRAL))

promedio_rural2020 <- promedio_rural2020 %>%
  mutate(prop = promedio / sum(promedio) * 100,
         ypos = cumsum(prop) - 0.5 * prop)

promedio_rural2020 <- promedio_rural2020 %>% 
  rename("Tipo de zona" = `as.character(TipoRural)`)

###2021##

promedio_rural2021 <- tabla_rendimientos2021 %>% 
  group_by(as.character(TipoRural)) %>% 
  summarise(promedio = mean(PROM_GRAL))

promedio_rural2021 <- promedio_rural2021 %>%
  mutate(prop = promedio / sum(promedio) * 100,
         ypos = cumsum(prop) - 0.5 * prop)

promedio_rural2021 <- promedio_rural2021 %>% 
  rename("Tipo de zona" = `as.character(TipoRural)`)

###2022##

promedio_rural2022 <- tabla_rendimientos2022 %>% 
  group_by(as.character(TipoRural)) %>% 
  summarise(promedio = mean(PROM_GRAL))

promedio_rural2022 <- promedio_rural2022 %>%
  mutate(prop = promedio / sum(promedio) * 100,
         ypos = cumsum(prop) - 0.5 * prop)

promedio_rural2022 <- promedio_rural2022 %>% 
  rename("Tipo de zona" = `as.character(TipoRural)`)

###2023##

promedio_rural2023 <- tabla_rendimientos2023 %>% 
  group_by(as.character(TipoRural)) %>% 
  summarise(promedio = mean(PROM_GRAL))

promedio_rural2023 <- promedio_rural2023 %>%
  mutate(prop = promedio / sum(promedio) * 100,
         ypos = cumsum(prop) - 0.5 * prop)

promedio_rural2023 <- promedio_rural2023 %>% 
  rename("Tipo de zona" = `as.character(TipoRural)`)









##Graficos##


###2018###

ggplot(promedio_rural2018, aes(x = "", y = prop, fill = `Tipo de zona`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Promedio por tipo de zona 2018") +
  theme(legend.position = "right",
        text = element_text(family = "poppins"),
        plot.background = element_rect(fill = "white")) +
  geom_text(aes(y = ypos, label = paste0(round(prop, 1), "%")), color = "white", size = 5, family = "poppins") +
  scale_fill_manual(values = c("Rural" = "#B80428", "Urbano" = "#250EC3"))

###2019###

ggplot(promedio_rural2019, aes(x = "", y = prop, fill = `Tipo de zona`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Promedio por tipo de zona 2019") +
  theme(legend.position = "right",
        text = element_text(family = "poppins")) +
  geom_text(aes(y = ypos, label = paste0(round(prop, 1), "%")), color = "white", size = 5, family = "poppins")

###2020###

ggplot(promedio_rural2020, aes(x = "", y = prop, fill = `Tipo de zona`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Promedio por tipo de zona 2020") +
  theme(legend.position = "right",
        text = element_text(family = "poppins")) +
  geom_text(aes(y = ypos, label = paste0(round(prop, 1), "%")), color = "white", size = 5, family = "poppins")

###2021###

ggplot(promedio_rural2021, aes(x = "", y = prop, fill = `Tipo de zona`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Promedio por tipo de zona 2021") +
  theme(legend.position = "right",
        text = element_text(family = "poppins")) +
  geom_text(aes(y = ypos, label = paste0(round(prop, 1), "%")), color = "white", size = 5, family = "poppins")

###2022###


ggplot(promedio_rural2022, aes(x = "", y = prop, fill = `Tipo de zona`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Promedio por tipo de zona 2022") +
  theme(legend.position = "right",
        text = element_text(family = "poppins")) +
  geom_text(aes(y = ypos, label = paste0(round(prop, 1), "%")), color = "white", size = 5, family = "poppins")



###2023##


ggplot(promedio_rural2023, aes(x = "", y = prop, fill = `Tipo de zona`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Promedio por tipo de zona 2023") +
  theme(legend.position = "right",
        text = element_text(family = "poppins")) +
  geom_text(aes(y = ypos, label = paste0(round(prop, 1), "%")), color = "white", size = 5, family = "poppins")


##########################

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


ggplot(Nt_rural2020, mapping = aes(x = Rural, y = PROM_GRAL2)) +
  geom_bar()

##2021##

Nt_rural2021 <- tabla_rendimientos2021 %>% 
  tabyl(PROM_GRAL2, TipoRural) %>% 
  adorn_percentages("col") %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting(digits = 2) %>% 
  adorn_ns()

kable(Nt_rural2021) %>% 
  kable_classic(html_font = "Tahoma") %>%
  kable_styling(bootstrap_options = kablevectores, full_width = FALSE) %>% 
  add_header_above(c("Promedios entre colegios rurales y urbanos 2020" = 3), bold = TRUE, font_size = 15)
