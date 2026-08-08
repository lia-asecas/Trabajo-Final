# Viene de la app de apertura.
# 
# Shiny para lanzar nuevo cálculo
# Pide Año y Trimestre y llama a 02_actualizar_indice


library(shiny)
library(rmarkdown)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Nuevo Cálculo — Índice de Precarización"),
  sidebarLayout(
    sidebarPanel(
      numericInput("anio", "Año a procesar:",
                   value = as.integer(format(Sys.Date(), "%Y")),
                   min = 2017, max = as.integer(format(Sys.Date(), "%Y")), step = 1),
      selectInput("trim", "Trimestre:", choices = 1:4, selected = 4),
      actionButton("procesar", "Procesar", class = "btn-primary"),
      br(), br(),
      helpText("Descarga los microdatos EPH desde 2017 hasta el año elegido. Puede tardar varios minutos.")
    ),
    mainPanel(
      verbatimTextOutput("estado")
    )
  )
)

server <- function(input, output, session) {
  
  estado <- reactiveVal("Esperando parámetros...")
  output$estado <- renderText(estado())
  
  observeEvent(input$procesar, {
    shinyjs::disable("procesar")
    anio <- input$anio
    trim <- as.integer(input$trim)
    nombre <- paste0("Ind_", anio, "_T", trim)
    
    estado(paste0("Procesando T", trim, " ", anio, "... esto puede demorar varios minutos."))
    
    tryCatch({
      rmarkdown::render(
        input         = "../../scripts/02_actualizar_indice.Rmd",
        output_format = "all",
        output_file   = nombre,
        output_dir    = "../../resultados",
        params        = list(anio = anio, trim = trim),
        envir         = new.env()
      )
      estado(paste0("✔ Listo. Informe guardado como '", nombre, ".html' / '.docx' en /resultados."))
    }, error = function(e) {
      estado(paste0("✖ Error durante el procesamiento:\n", conditionMessage(e)))
    })
    
    shinyjs::enable("procesar")
  })
}

shinyApp(ui, server)