

####lista####

graficos <- list(
  "2018" = ggplot(comunas_santiago2018) +
    geom_sf(aes(fill = promedio, geometry = geometry)) +
    scale_fill_gradientn(colours = rev(paleta1), name = "Promedios") +
    labs(title = "Promedios en Santiago 2018") +
    theme_minimal(base_size = 13),
  "2019" = ggplot(comunas_santiago2019) +
    geom_sf(aes(fill = promedio, geometry = geometry)) +
    scale_fill_gradientn(colours = rev(paleta1), name = "Promedios") +
    labs(title = "Promedios en Santiago 2019") +
    theme_minimal(base_size = 13),
  "2020" = ggplot(comunas_santiago2020) +
    geom_sf(aes(fill = promedio, geometry = geometry)) +
    scale_fill_gradientn(colours = rev(paleta1), name = "Promedios") +
    labs(title = "Promedios en Santiago 2020") +
    theme_minimal(base_size = 13),
  "2021" = ggplot(comunas_santiago2021) +
    geom_sf(aes(fill = promedio, geometry = geometry)) +
    scale_fill_gradientn(colours = rev(paleta1), name = "Promedios") +
    labs(title = "Promedios en Santiago 2021") +
    theme_minimal(base_size = 13),
  "2022" = ggplot(comunas_santiago2022) +
    geom_sf(aes(fill = promedio, geometry = geometry)) +
    scale_fill_gradientn(colours = rev(paleta1), name = "Promedios") +
    labs(title = "Promedios en Santiago 2022") +
    theme_minimal(base_size = 13),
  "2023" = ggplot(comunas_santiago2023) +
    geom_sf(aes(fill = promedio, geometry = geometry)) +
    scale_fill_gradientn(colours = rev(paleta1), name = "Promedios") +
    labs(title = "Promedios en Santiago 2023") +
    theme_minimal(base_size = 13)
)

#######




ui <- fluidPage(
  titlePanel("Visualizador de datos abiertos TRANSPARENTA 2024"),
  includeCSS("paleta.css"),
  tags$style(HTML("
    .irs-bar { background-color: #F02D3A; }
    .irs-bar-edge { background-color: #F02D3A; }
    .irs-line { background-color: #273043; }
    .irs-grid-text { color: #EFF6EE; }
    .irs-min, .irs-max, .irs-from, .irs-to, .irs-single { background-color: #007bff; color: #ffffff; }
  ")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_slider",
                  label = "Año:",
                  min = 2018,
                  max = 2023,
                  value = 2018,
                  step = 1,
                  animate = FALSE,
                  sep = ""),
    ),
    mainPanel(
      plotOutput("dynamic_plot"),
    )
  )
)

server <- function(input, output) {
  output$dynamic_plot <- renderPlot({
    selected_year <- as.character(input$year_slider)
    
    
    graficos[[selected_year]]
  })
}





shinyApp(ui = ui, server = server)


