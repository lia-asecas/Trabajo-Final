# /scripts/app.R
#
# Pantalla de Inicio del Índice de Precarización Laboral (Observatorio de les Trabajadores)
# Equivalente a 01_dashboard.Rmd, pero como app.R puro (sin Pandoc/knitr).

library(shiny)
library(readxl)
library(dplyr)
library(scales)
library(processx)

source("Formatos.R")

www_dir <- normalizePath(file.path("..", "www"), mustWork = FALSE)
shiny::addResourcePath("assets", www_dir)

resultados_dir <- normalizePath(file.path("..", "resultados"), mustWork = FALSE)
shiny::addResourcePath("informes", resultados_dir)


# ---- UI ---------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "assets/estilos.css")
  ),
  titlePanel("Índice de Precarización Laboral"),
  fluidRow(
    column(
      width = 4,
      h4("Observatorio de les Trabajadores"),
      uiOutput("info_publicacion"),
      uiOutput("botones_accion")
    ),
    column(
      width = 8,
      h4("Último período procesado"),
      uiOutput("bloque_indicadores")
    )
  )
)


# ---- SERVER -------------------------------------------------------------

server <- function(input, output, session) {
  
  refrescar <- reactiveVal(0)
  
  datos_log <- reactive({
    refrescar()   # crea la dependencia: se recalcula cuando esto cambia
    log <- read_excel("../log/ejecucion_log.xlsx")
    log |> slice_tail(n = 1)
  })
  
  # ---- Info publicación ----
  
  output$info_publicacion <- renderUI({
    d <- datos_log()
    req(nrow(d) == 1)
    tagList(
      tags$br(),
      tags$strong("Período publicado:"), tags$br(),
      sprintf("%s° trimestre %s", d$trimestre, d$anio_ult), tags$br(), tags$br(),
      tags$strong("Actualizado:"), tags$br(),
      d$fecha, tags$br(),
      tags$em(d$user)
    )
  })
  
  # ---- Botones ----
  
  output$botones_accion <- renderUI({
    d <- datos_log()
    req(nrow(d) == 1)
    
    ruta_informe <- file.path("..", "resultados",
                              paste0("Ind_", d$anio_ult, "_T", d$trimestre, ".html"))
    href_informe <- paste0("informes/", basename(ruta_informe))
    
    tags$div(
      class = "botones-accion",
      if (file.exists(ruta_informe)) {
        tags$a(class = "boton-accion", href = href_informe, target = "_blank", "Ver Informe")
      } else {
        tags$span(class = "boton-accion deshabilitado", "Informe no disponible")
      },
      actionButton("btn_consultar",     "Consultar",     class = "boton-accion"),
      actionButton("btn_nuevo_calculo", "Nuevo Cálculo", class = "boton-accion secundario"),
      actionButton("btn_refrescar",     "Refrescar",     class = "boton-accion")
    )
  })
  
  # ---- Indicadores ----
  
  output$bloque_indicadores <- renderUI({
    d <- datos_log()
    req(nrow(d) == 1)
    HTML(paste(
      '<div class="bloque-indicadores">',
      '<div class="tarjeta tarjeta-principal">',
      sprintf('<div class="valor-indicador">%s%%</div>', fmt_decimal(d$nivel_grl)),
      '<div class="etiqueta-indicador">de ocupados/as con al menos una dimensión de precarización</div>',
      sprintf('<div class="periodo-indicador">T%s %s</div>', d$trimestre, d$anio_ult),
      '</div>',
      '<div class="tarjeta tarjeta-secundaria">',
      sprintf('<div class="valor-indicador">%s%%</div>', fmt_decimal(d$nivel_v)),
      '<div class="etiqueta-indicador">Varones</div>',
      '</div>',
      '<div class="tarjeta tarjeta-secundaria">',
      sprintf('<div class="valor-indicador">%s%%</div>', fmt_decimal(d$nivel_m)),
      '<div class="etiqueta-indicador">Mujeres</div>',
      '</div>',
      '<div class="tarjeta tarjeta-terciaria">',
      sprintf('<div class="valor-indicador">%s%%</div>', fmt_decimal(d$pre_c)),
      '<div class="etiqueta-indicador">Precario por Contrato</div>',
      '</div>',
      '<div class="tarjeta tarjeta-terciaria">',
      sprintf('<div class="valor-indicador">%s%%</div>', fmt_decimal(d$pre_j)),
      '<div class="etiqueta-indicador">Precario por Jornada</div>',
      '</div>',
      '<div class="tarjeta tarjeta-terciaria">',
      sprintf('<div class="valor-indicador">%s%%</div>', fmt_decimal(d$pre_i)),
      '<div class="etiqueta-indicador">Precario por Ingreso</div>',
      '</div>',
      '</div>',
      sep = "\n"
    ))
  })
  
  # ---- Nuevo cálculo ----
  
  rv <- reactiveValues(proceso = NULL, corriendo = FALSE, nombre_actual = NULL)
  
  modal_formulario <- function() {
    modalDialog(
      title = "Nuevo Cálculo",
      size = "s",
      numericInput("nc_anio", "Año:",
                   value = as.integer(format(Sys.Date(), "%Y")),
                   min = 2017, max = as.integer(format(Sys.Date(), "%Y")), step = 1),
      selectInput("nc_trim", "Trimestre:", choices = 1:4, selected = 1),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_procesar", "Procesar", class = "btn-primary")
      ),
      easyClose = FALSE
    )
  }
  
  modal_procesando <- function() {
    modalDialog(
      title = "Procesando...",
      size = "s",
      tags$div(class = "spinner-nuevo-calculo"),
      tags$p("Descargando microdatos y calculando el índice. Puede demorar varios minutos."),
      footer = NULL,
      easyClose = FALSE
    )
  }
  
  observeEvent(input$btn_consultar, {
    r_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
    
    processx::process$new(
      r_bin,
      c("-e", "shiny::runApp('../apps/consultar', launch.browser = TRUE)"),
      wd        = getwd(),
      stdout    = "../log/consultar_out.log",
      stderr    = "../log/consultar_err.log",
      supervise = FALSE
    )
  })
  
  observeEvent(input$btn_nuevo_calculo, {
    showModal(modal_formulario())
  })
  
  observeEvent(input$btn_procesar, {
    anio   <- input$nc_anio
    trim   <- as.integer(input$nc_trim)
    nombre <- paste0("Ind_", anio, "_T", trim)
    
    r_bin  <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
    codigo <- sprintf(
      "withr::with_dir('scripts', rmarkdown::render(input='02_actualizar_indice.Rmd', output_format='all', output_file='%s', output_dir='../resultados', params=list(anio=%d, trim=%d), envir=new.env()))",
      nombre, anio, trim
    )
    
    rv$proceso        <- processx::process$new(
      r_bin, c("--vanilla", "-e", codigo),
      wd = normalizePath(".."),
      stdout = "|", stderr = "|", supervise = FALSE
    )
    rv$corriendo      <- TRUE
    rv$nombre_actual  <- nombre
    
    showModal(modal_procesando())
  })
  
  observeEvent(input$btn_refrescar, {
    refrescar(refrescar() + 1)
    removeModal()
  })
  
  observe({
    req(rv$corriendo)
    invalidateLater(2000)
    isolate({
      if (!is.null(rv$proceso) && !rv$proceso$is_alive()) {
        rv$corriendo <- FALSE
        exit_code <- rv$proceso$get_exit_status()
        
        if (!is.null(exit_code) && exit_code == 0) {
          showModal(modalDialog(
            title = "Listo ✔",
            size = "s",
            paste0("Se generó ", rv$nombre_actual, ".html / .docx en /resultados."),
            footer = tagList(
              tags$a(
                class = "btn btn-primary", target = "_blank",
                href = paste0("informes/", rv$nombre_actual, ".html"),
                "Ver Informe"
              ),
              modalButton("Cerrar")
            )
          ))
        } else {
          showModal(modalDialog(
            title = "Error",
            size = "s",
            "Algo falló durante el procesamiento. Revisá la consola de R o los logs.",
            footer = modalButton("Cerrar")
          ))
        }
      }
    })
  })
}

shinyApp(ui, server)