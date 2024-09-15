library(shiny)

ui <- fluidPage(
  tags$head(
    # Fuente Poppins y estilos CSS
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap"),
    tags$style(HTML(
      "
      body {
        background-color: #000E25;
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
        overflow: hidden;
        border-right: 2px solid white;
        animation: typing 1s steps(12) 1s forwards, blink-caret 0.75s step-end infinite;
      }
      
      @keyframes typing {
        from { width: 0; }
        to { width: 12ch; } 
      }

      @keyframes blink-caret {
        from, to { border-color: transparent; }
        100% { border-color: white; }
      }

      h1.finished-typing {
        border-right: none;
      }
      
      a {
        color: white;
        font-size: 24px;
        text-decoration: underline;
        margin-top: 20px;
        display: inline-block;
      }
      
      .github-icon {
        margin-top: 500px;
      }
      .github-icon img {
        width: 70px; 
        height: auto;
      }
      .transparenta-icon {
        margin-top: 100px;
        margin-left: 50px;
      }
      .transparenta-icon img {
        height: auto;
      }
    "
    ))
  ),
  
  h1(id = "title", "ChileMateo"),
  
  p("Bienvenido a ChileMateo, un visualizador de datos abiertos sobre el rendimiento escolar."),
  
  # Hipervínculo al visualizador principal
  p(a("Visualizador Gran Santiago", href = "https://tu-visualizador.shinyapps.io", target = "_blank")),
  
  # Ícono de GitHub
  div(class = "github-icon",
      a(href = "https://github.com/liamobilinovic/Proyecto-Chile-Mateo", target = "_blank", 
        img(src = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png"))
  ),


  div(class = "transparenta-icon",
      a(href = "https://transparentadatos.cl/", target = "_blank", 
        img(src = "https://cienciaabierta.uc.cl/wp-content/uploads/2023/09/transparenta.jpg"))
  )
)

server <- function(input, output, session) {
  observe({
    session$sendCustomMessage(type = 'jsCode', 
                              list(code = "
        setTimeout(function() {
          document.getElementById('title').classList.add('finished-typing');
        }, 3000);
      "))
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
