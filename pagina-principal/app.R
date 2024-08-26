library(shiny)

ui <- fluidPage(
  tags$head(
    # Fuente Poppins y estilos CSS
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap"),
    tags$style(HTML("
      body {
        background: rgb(0,0,0);
        background: linear-gradient(125deg, rgba(0,0,0,1) 39%, rgba(0,3,233,1) 76%);
        color: white;
        font-family: 'Poppins', sans-serif;
        text-align: center;
        padding-top: 200px;
        overflow: hidden;
      }
      
      h1 {
        font-size: 48px;
        font-weight: 600;
        display: inline-block;
        white-space: nowrap;
        border-right: 2px solid white; /* Simula el cursor de la máquina de escribir */
        width: 0; /* Inicia con un ancho de 0 */
        animation: typing 2s steps(12) 1s forwards, blink-caret 0.75s step-end infinite; /* Define las animaciones */
        overflow: hidden;
      }
      
      /* Efecto de máquina de escribir */
      @keyframes typing {
        from { width: 0; } /* Comienza desde 0 caracteres visibles */
        to { width: 12ch; } /* Muestra progresivamente hasta 12 caracteres (ch) */
      }

      /* Parpadeo del cursor */
      @keyframes blink-caret {
        from, to { border-color: transparent; } /* Alterna la visibilidad del cursor */
        100% { border-color: white; }
      }
      
      a {
        color: white;
        font-size: 24px;
        text-decoration: underline;
        margin-top: 20px;
        display: inline-block;
      }
    "))
  ),
  
  h1("ChileMateo"),
  
  p("Bienvenido a ChileMateo, un visualizador de datos abiertos sobre el rendimiento escolar."),
  
  # Hipervínculo al visualizador principal
  p(a("Visualizador Gran Santiago", href = "https://tu-visualizador.shinyapps.io", target = "_blank"))
)

server <- function(input, output, session) {
  # Detiene la aplicación cuando se cierra la sesión
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
