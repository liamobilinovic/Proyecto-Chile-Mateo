##Este script es para evidenciar la evolución de los promedios##



comunas_total <- list(comunas_santiago2018, comunas_santiago2019, comunas_santiago2020, comunas_santiago2021, comunas_santiago2022, comunas_santiago2023)


nombres_anios <- c("2018", "2019", "2020", "2021", "2022", "2023")

grafico_evolucion <- function(comuna, year_index) {
  
  df_total <- bind_rows(comunas_total, .id = "year_index")
  
  df_total <- df_total %>%
    mutate(anio = nombres_anios[as.numeric(year_index)]) %>%
    filter(codigo_comuna == comuna)

  nombre_comuna <- unique(df_total$nombre_comuna)
  nombre_comuna <- capitalize(nombre_comuna)
  
  df_total <- df_total %>%
    mutate(anio = factor(anio, levels = nombres_anios))
  
  ggplot(df_total, aes(x = anio, y = promedio, group = 1)) +
    geom_line(color = "#031163", size = 2) +
    geom_point(color = "#AC413E") +
    geom_text(aes(label = scales::comma(promedio)), vjust = -0.5, color = "#CACFEC") +
    labs(title = paste("Evolución del promedio en", nombre_comuna),
         x = "Año",
         y = "Promedio") +
    scale_y_continuous(limits = c(4.0, 7.0),
                       breaks = seq(4.0, 7.0, by = 0.5)) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#000005", color = "#000005"),
      panel.background = element_rect(fill = "#000005", color = "#000005"),
      axis.text = element_text(family = "Poppins", color = "#CACFEC"),
      panel.grid.major = element_line(color = "#0E1E38"),
      panel.grid = element_blank(),
      axis.title = element_text(family = "Poppins", color = "#CACFEC"),
      axis.ticks = element_blank(),
      axis.line = element_line(color = "#CACFEC"),
      plot.title = element_text(family = "Poppins", color = "#CACFEC", hjust = 0.5),
      legend.position = "none"  # Ocultar la leyenda
    )
  
}

grafico_evolucion(13103)



