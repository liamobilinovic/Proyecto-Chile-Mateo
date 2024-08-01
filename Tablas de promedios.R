
##### Scrip de Tablas de promedios #### 

####En el siguiente script se realizarán las tablas ####
####representando el promedio (mean) del promedio general de notas ####
#### (PROM_GRAL) en todos los años a realizar #### 

###RECORDAR QUE ESTOS PROMEDIOS SON ENTRE 1ERO BASICO Y QUINTO####




tabla_promedios_2018 <- tabla_rendimientos2018 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>% 
  filter(COD_ENSE2 == 2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural"))

  
Prom_cursos2018 <- tabla_promedios_2018 %>%
  group_by(Curso) %>%
  summarize(promedio = mean(PROM_GRAL, na.rm = TRUE))

ggplot(data=Prom_cursos, aes(x=Curso, y=promedio, group=1)) +
  geom_line()+
  geom_point() + theme(plot.caption = element_text(size = 8,
    colour = "gray21", hjust = 0), axis.line = element_line(colour = "cornflowerblue",
    linetype = "solid"), panel.grid.major = element_line(linetype = "blank"),
    axis.title = element_text(family = "AvantGarde",
        face = "bold"), axis.text = element_text(family = "AvantGarde",
        size = 11, face = "bold", colour = "gray15"),
    plot.title = element_text(family = "AvantGarde",
        face = "bold", hjust = 0.5), legend.position = "inside") +labs(title = "Promedio de notas por curso año 2018",
    y = "Promedio", caption = "Elaborado a partir de los datos recogidos por el  Sistema de Información General de Estudiantes (SIGE)") +
  geom_text(aes(label = round(promedio, 2)), vjust = 1.5, size = 3)

  
  
#### 2019 ##### 

tabla_promedios_2019 <- tabla_rendimientos2019 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>% 
  filter(COD_ENSE2 == 2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural"))


Prom_cursos2019 <- tabla_promedios_2019 %>%
  group_by(Curso) %>%
  summarize(promedio = mean(PROM_GRAL, na.rm = TRUE))

ggplot(data=Prom_cursos2019, aes(x=Curso, y=promedio, group=1)) +
  geom_line()+
  geom_point()

#### 2020 ##### 

tabla_promedios_2020 <- tabla_rendimientos2020 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>% 
  filter(COD_ENSE2 == 2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural"))


Prom_cursos2020 <- tabla_promedios_2020 %>%
  group_by(Curso) %>%
  summarize(promedio = mean(PROM_GRAL, na.rm = TRUE))

ggplot(data=Prom_cursos2020, aes(x=Curso, y=promedio, group=1)) +
  geom_line()+
  geom_point()

#### 2021 ####

tabla_promedios_2021 <- tabla_rendimientos2021 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>% 
  filter(COD_ENSE2 == 2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural"))


Prom_cursos2021 <- tabla_promedios_2021 %>%
  group_by(Curso) %>%
  summarize(promedio = mean(PROM_GRAL, na.rm = TRUE))

ggplot(data=Prom_cursos2021, aes(x=Curso, y=promedio, group=1)) +
  geom_line()+
  geom_point()

#### 2022 ####

tabla_promedios_2022 <- tabla_rendimientos2022 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>% 
  filter(COD_ENSE2 == 2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural"))


Prom_cursos2022 <- tabla_promedios_2022 %>%
  group_by(Curso) %>%
  summarize(promedio = mean(PROM_GRAL, na.rm = TRUE))

ggplot(data=Prom_cursos2022, aes(x=Curso, y=promedio, group=1)) +
  geom_line()+
  geom_point()

#### 2023 ####

tabla_promedios_2023 <- tabla_rendimientos2023 %>% 
  filter(PROM_GRAL != -0) %>% 
  mutate(GEN = case_when(GEN_ALU %in% c(1) ~ "Masculino",
                         GEN_ALU %in% c(2) ~ "Femenino")) %>% 
  filter(COD_ENSE2 == 2) %>% 
  filter(COD_GRADO %in% c(1:5)) %>% 
  mutate(Curso = case_when(COD_GRADO %in% c(1) ~ "1ero Básico",
                           COD_GRADO %in% c(2) ~ "2ndo Básico", 
                           COD_GRADO %in% c(3) ~ "3ro Básico",
                           COD_GRADO %in% c(4) ~ "4to Básico", 
                           COD_GRADO %in% c(5) ~ "5to Básico")) %>% 
  mutate(TipoRural = case_when(RURAL_RBD %in% c(0) ~ "Urbano",
                               RURAL_RBD %in% c(1) ~ "Rural"))


Prom_cursos2023 <- tabla_promedios_2023 %>%
  group_by(Curso) %>%
  summarize(promedio = mean(PROM_GRAL, na.rm = TRUE))

ggplot(data=Prom_cursos2023, aes(x=Curso, y=promedio, group=1)) +
  geom_line()+
  geom_point()
