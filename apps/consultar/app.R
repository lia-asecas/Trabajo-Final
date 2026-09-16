# apps/consultar/app.R
#
# Pantalla de consulta: serie histórica de % precariedad por dimensión,
# filtrable por CAT_OCUP, Ocupación (2 dígitos de PP04D_COD) y Jerarquía (3er dígito).

# apps/consultar/app.R
# Pantalla de consulta: serie histórica de % precariedad por dimensión,
# filtrable por CAT_OCUP, Ocupación (2 dígitos de PP04D_COD) y Jerarquía (3er dígito).
# Bases ya filtradas por ocupados (ESTADO == 1) en el procesamiento previo.

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(scales)
library(plotly)

source("../../scripts/Formatos.R")

www_dir <- normalizePath(file.path("..", "..", "www"), mustWork = FALSE)
shiny::addResourcePath("assets", www_dir)

bases_dir <- normalizePath(file.path("..", "..", "bases"), mustWork = FALSE)


# ---- Datos fijos del último procesamiento (encabezado) -------------------

log <- readxl::read_excel("../../log/ejecucion_log.xlsx") |> dplyr::slice_tail(n = 1)

anio_dde  <- log$anio_dde
anio_ult  <- log$anio_ult
trimestre <- log$trimestre

# archivo de referencia (el último) para armar las opciones de los filtros
ruta_ref <- file.path(bases_dir,
                      sprintf("individual_procesada_%d_T%d.csv", anio_ult, trimestre))

datos_ref <- read_csv(
  ruta_ref,
  col_select = c(CAT_OCUP, PP04D_COD),
  col_types  = cols(CAT_OCUP = col_character(), PP04D_COD = col_character())
)


# NOTA: las etiquetas de CAT_OCUP siguen la codificación estándar EPH
# (1 Patrón, 2 Cuenta propia, 3 Obrero o empleado, 4 Trabajador familiar).
cat_ocup_valores <- sort(unique(datos_ref$CAT_OCUP))
cat_ocup_labels  <- case_when(
  cat_ocup_valores == "1" ~ "1 - Patrón",
  cat_ocup_valores == "2" ~ "2 - Cuenta propia",
  cat_ocup_valores == "3" ~ "3 - Obrero o empleado",
  cat_ocup_valores == "4" ~ "4 - Trabajador familiar",
  TRUE ~ cat_ocup_valores
)
names(cat_ocup_valores) <- cat_ocup_labels

cno <- readxl::read_excel(file.path(bases_dir, "CNO.xlsx"), sheet = "CNO")

armar_choices <- function(tipo_valor, codigos_presentes) {
  d <- cno |> filter(TIPO == tipo_valor, OCU %in% codigos_presentes)
  choices <- d$OCU
  names(choices) <- paste0(d$OCU, " - ", d$DESCRIPCION)
  choices
}

ocupacion_valores <- armar_choices("Tarea",     sort(unique(substr(datos_ref$PP04D_COD, 1, 2))))
jerarquia_valores <- armar_choices("Jerarquia", sort(unique(substr(datos_ref$PP04D_COD, 3, 3))))


# ---- UI --------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "assets/estilos.css")),
  titlePanel("Consultar — Índice de Precarización Laboral"),
  
  tags$div(
    style = "display:flex; align-items:stretch; gap:20px;",
    
    # ---- Columna izquierda: filtros, altura completa ----
    tags$div(
      style = "flex: 0 0 280px;",
      wellPanel(
        style = "height:100%;",
        checkboxGroupInput("f_cat_ocup", "Categoría ocupacional:",
                           choices = cat_ocup_valores, selected = cat_ocup_valores),
        checkboxGroupInput("f_jerarquia", "Jerarquía:",
                           choices = jerarquia_valores, selected = jerarquia_valores),
        tags$div(
          style = "max-height:420px; overflow-y:auto; border:1px solid #ddd; padding:6px; border-radius:4px;",
          checkboxGroupInput("f_ocupacion", "Ocupación:",
                             choices = ocupacion_valores, selected = ocupacion_valores)
        )
      )
    ),
    
    # ---- Columna derecha: tarjetas chicas + gráfico + tabla ----
    tags$div(
      style = "flex: 1;",
      
      tags$div(
        style = "display:flex; gap:10px; margin-bottom:16px;",
        tags$div(class = "tarjeta tarjeta-principal", style = "flex:1; padding:10px;",
                 tags$div(class = "valor-indicador", style = "font-size:1.4rem;",
                          paste0(fmt_decimal(log$nivel_grl), "%")),
                 tags$div(class = "etiqueta-indicador", style = "font-size:0.75rem;", "Nivel general"),
                 tags$div(class = "periodo-indicador", style = "font-size:0.7rem;",
                          sprintf("T%s %s", trimestre, anio_ult))
        ),
        tags$div(class = "tarjeta tarjeta-terciaria", style = "flex:1; padding:10px;",
                 tags$div(class = "valor-indicador", style = "font-size:1.4rem;",
                          paste0(fmt_decimal(log$pre_c), "%")),
                 tags$div(class = "etiqueta-indicador", style = "font-size:0.75rem;", "Contrato")
        ),
        tags$div(class = "tarjeta tarjeta-terciaria", style = "flex:1; padding:10px;",
                 tags$div(class = "valor-indicador", style = "font-size:1.4rem;",
                          paste0(fmt_decimal(log$pre_j), "%")),
                 tags$div(class = "etiqueta-indicador", style = "font-size:0.75rem;", "Jornada")
        ),
        tags$div(class = "tarjeta tarjeta-terciaria", style = "flex:1; padding:10px;",
                 tags$div(class = "valor-indicador", style = "font-size:1.4rem;",
                          paste0(fmt_decimal(log$pre_i), "%")),
                 tags$div(class = "etiqueta-indicador", style = "font-size:0.75rem;", "Ingreso")
        )
      ),
      
      plotlyOutput("grafico_serie", height = "280px"),
      
      tags$p(textOutput("anios_faltantes"), style = "color:#999; font-size:0.85rem;"),
      
      hr(),
      tableOutput("tabla_resumen")
    )
  )
)


# ---- SERVER ------------------------------------------------------------

server <- function(input, output, session) {
  
  leer_anio <- function(anio) {
    ruta <- file.path(bases_dir, sprintf("individual_procesada_%d_T%d.csv", anio, trimestre))
    if (!file.exists(ruta)) return(NULL)
    read_csv(
      ruta,
      col_select = c(CAT_OCUP, PP04D_COD, PONDERA, PONDIIO,
                     precario_contrato_num, precario_jornada_num, precario_ingreso_num, num_dimensiones),
      col_types  = cols(CAT_OCUP = col_character(), PP04D_COD = col_character(),
                        .default = col_double())
    ) |>
      mutate(anio = anio)
  }
  
  datos_serie <- reactive({
    anios <- anio_dde:anio_ult
    bind_rows(lapply(anios, leer_anio))
  })
  
  datos_filtrados <- reactive({
    d <- datos_serie()
    req(nrow(d) > 0)
    
    if (length(input$f_cat_ocup) > 0) {
      d <- d |> filter(CAT_OCUP %in% input$f_cat_ocup)
    }
    if (length(input$f_ocupacion) > 0) {
      d <- d |> filter(substr(PP04D_COD, 1, 2) %in% input$f_ocupacion)
    }
    if (length(input$f_jerarquia) > 0) {
      d <- d |> filter(substr(PP04D_COD, 3, 3) %in% input$f_jerarquia)
    }
    d
  })
  
  resumen_anual <- reactive({
    datos_filtrados() |>
      group_by(anio) |>
      summarise(
        abs_contrato = sum(PONDERA[precario_contrato_num == 1], na.rm = TRUE),
        abs_jornada  = sum(PONDERA[precario_jornada_num  == 1], na.rm = TRUE),
        abs_ingreso  = sum(PONDIIO[precario_ingreso_num  == 1], na.rm = TRUE),
        abs_precario = sum(PONDERA[num_dimensiones >= 1], na.rm = TRUE),
        pct_contrato = 100 * abs_contrato / sum(PONDERA, na.rm = TRUE),
        pct_jornada  = 100 * abs_jornada  / sum(PONDERA, na.rm = TRUE),
        pct_ingreso  = 100 * abs_ingreso  / sum(PONDIIO, na.rm = TRUE),
        pct_precario = 100 * abs_precario / sum(PONDERA, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$grafico_serie <- renderPlotly({
    d <- resumen_anual()
    req(nrow(d) > 0)
    
    d_larga <- d |>
      select(anio, starts_with("pct_")) |>
      pivot_longer(cols = starts_with("pct_"), names_to = "dimension", values_to = "pct") |>
      mutate(dimension = recode(dimension,
                                pct_contrato = "Contrato",
                                pct_jornada  = "Jornada",
                                pct_ingreso  = "Ingreso",
                                pct_precario = "Precarios"))
    
    p <- suppressWarnings(
      ggplot(d_larga, aes(x = anio, y = pct, color = dimension, group = dimension)) +
      geom_line(linewidth = 1) +
      geom_point(aes(text = sprintf("%s\nAño: %d\n%.1f%%", dimension, anio, pct)), size = 2) +
      scale_x_continuous(breaks = anio_dde:anio_ult) +
      scale_color_manual(values = c("Contrato" = "#e74c3c",
                                    "Jornada"  = "#e67e22",
                                    "Ingreso"  = "#2C6E91")) +
      labs(x = NULL, y = "% precario", color = NULL,
           title = sprintf("Precarización laboral — serie T%s (%s-%s)", trimestre, anio_dde, anio_ult)) +
      theme_minimal(base_size = 10)
    )
    
    ggplotly(p, tooltip = "text") |>
      plotly::layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$tabla_resumen <- renderTable({
    d <- resumen_anual()
    req(nrow(d) > 0)
    
    d |>
      transmute(
        `Año`                    = anio,
        `Contrato (casos)`       = fmt_enteros(abs_contrato),
        `Contrato (%)`           = fmt_decimal(pct_contrato),
        `Jornada (casos)`        = fmt_enteros(abs_jornada),
        `Jornada (%)`            = fmt_decimal(pct_jornada),
        `Ingreso (casos)`        = fmt_enteros(abs_ingreso),
        `Ingreso (%)`            = fmt_decimal(pct_ingreso),
        `Precarios (casos)`      = fmt_enteros(abs_precario),
        `Precarios (%)`          = fmt_decimal(pct_precario)       
      )
  }, align = "c", striped = TRUE, spacing = "s")
  
  output$anios_faltantes <- renderText({
    d <- datos_serie()
    faltan <- setdiff(anio_dde:anio_ult, unique(d$anio))
    if (length(faltan) > 0) paste("Sin datos para:", paste(faltan, collapse = ", ")) else ""
  })
}

shinyApp(ui, server)