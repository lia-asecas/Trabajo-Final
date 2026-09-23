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

cno <- readxl::read_excel(file.path(bases_dir, "CNO.xlsx"), sheet = "CNO") |>
  distinct(TIPO, OCU, .keep_all = TRUE)
  # El Excel trae algunos códigos de Tarea duplicados (20, 30, 31, 32), lo que
  # provocaba un join many-to-many y filas repetidas en la tabla de cruce.

armar_choices <- function(tipo_valor, codigos_presentes) {
  d <- cno |> filter(TIPO == tipo_valor, OCU %in% codigos_presentes)
  choices <- d$OCU
  names(choices) <- paste0(d$OCU, " - ", d$DESCRIPCION)
  choices
}

ocupacion_valores <- armar_choices("Tarea", sort(unique(substr(datos_ref$PP04D_COD, 1, 2))))

# Jerarquía: SIEMPRE los códigos/descripciones/orden tal como están en CNO.xlsx
# (no depende de qué códigos estén presentes en la base de referencia), para que
# ambas pantallas usen exactamente el mismo universo y el mismo orden.
armar_jerarquia <- function() {
  d <- cno |> filter(TIPO == "Jerarquia") |> arrange(as.integer(OCU))
  choices <- d$OCU
  names(choices) <- paste0(d$OCU, " - ", d$DESCRIPCION)
  choices
}
jerarquia_valores <- armar_jerarquia()

# descripciones de Ocupación a 2 dígitos (para la tabla y el gráfico de cruce)
ocup2_desc <- cno |> filter(TIPO == "Tarea") |> transmute(ocup2 = OCU, desc_ocup2 = DESCRIPCION)


# ---- UI --------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "assets/estilos.css"),
    tags$style(HTML("
      .nav-tabs > li > a { font-weight: 700; font-size: 1.05rem; }
    "))
  ),
  titlePanel("Consultar — Índice de Precarización Laboral"),

  tabsetPanel(

    # ==================================================================
    # Pestaña 1: Serie histórica
    # ==================================================================
    tabPanel("Serie histórica",
      tags$div(
        style = "display:flex; align-items:stretch; gap:20px; margin-top:16px;",

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
    ),

    # ==================================================================
    # Pestaña 2: Ingresos por ocupación y sexo
    # ==================================================================
    tabPanel("Ingresos por ocupación y sexo",
      tags$div(
        style = "display:flex; align-items:stretch; gap:20px; margin-top:16px;",

        tags$div(
          style = "flex: 0 0 200px;",
          wellPanel(
            style = "height:100%;",
            selectInput("f2_anio", "Año:",
                       choices  = anio_ult:anio_dde, selected = anio_ult),
            checkboxGroupInput("f2_jerarquia", "Jerarquía:",
                               choices = jerarquia_valores, selected = jerarquia_valores)
          )
        ),

        tags$div(
          style = "flex: 1;",
          plotlyOutput("grafico_ingresos_sexo", height = "350px"),
          hr(),
          tableOutput("tabla_cruce")
        )
      )
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
      col_select = c(CAT_OCUP, PP04D_COD, PONDERA, PONDIIO, CH04, P21,
                     precario_contrato_num, precario_jornada_num, precario_ingreso_num,
                     num_dimensiones),
      col_types  = cols(CAT_OCUP = col_character(), PP04D_COD = col_character(),
                        .default = col_double())
    ) |>
      mutate(anio = anio)
  }

  datos_serie <- reactive({
    anios <- anio_dde:anio_ult
    bind_rows(lapply(anios, leer_anio))
  })

  # ---- Pestaña 1: Serie histórica ----

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
        abs_contrato  = sum(PONDERA[precario_contrato_num == 1], na.rm = TRUE),
        abs_jornada   = sum(PONDERA[precario_jornada_num  == 1], na.rm = TRUE),
        abs_ingreso   = sum(PONDIIO[precario_ingreso_num  == 1], na.rm = TRUE),
        abs_precario  = sum(PONDERA[num_dimensiones >= 1], na.rm = TRUE),
        total_pondera = sum(PONDERA, na.rm = TRUE),
        pct_contrato  = 100 * abs_contrato / sum(PONDERA, na.rm = TRUE),
        pct_jornada   = 100 * abs_jornada  / sum(PONDERA, na.rm = TRUE),
        pct_ingreso   = 100 * abs_ingreso  / sum(PONDIIO, na.rm = TRUE),
        pct_precario  = 100 * abs_precario / sum(PONDERA, na.rm = TRUE),
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
      scale_color_manual(values = c("Contrato"  = "#e74c3c",
                                    "Jornada"   = "#e67e22",
                                    "Ingreso"   = "#2C6E91",
                                    "Precarios" = "#8e44ad")) +
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
      arrange(desc(anio)) |>
      transmute(
        `Año`                    = anio,
        `Contrato (casos)`       = fmt_enteros(abs_contrato),
        `Contrato (%)`           = fmt_decimal(pct_contrato),
        `Jornada (casos)`        = fmt_enteros(abs_jornada),
        `Jornada (%)`            = fmt_decimal(pct_jornada),
        `Ingreso (casos)`        = fmt_enteros(abs_ingreso),
        `Ingreso (%)`            = fmt_decimal(pct_ingreso),
        `Precarios (casos)`      = fmt_enteros(abs_precario),
        `Precarios (%)`          = fmt_decimal(pct_precario),
        `Total (casos)`          = fmt_enteros(total_pondera)
      )
  }, align = "c", striped = TRUE, spacing = "s")

  output$anios_faltantes <- renderText({
    d <- datos_serie()
    faltan <- setdiff(anio_dde:anio_ult, unique(d$anio))
    if (length(faltan) > 0) paste("Sin datos para:", paste(faltan, collapse = ", ")) else ""
  })

  # ---- Pestaña 2: Ingresos por ocupación y sexo ----

  # Promedio ponderado que descarta observaciones sin ponderador de ingreso
  # (P21 == -9, "no responde", viene con PONDIIO vacío o en 0).
  promedio_ponderado <- function(valor, pondera) {
    pond <- ifelse(is.na(pondera) | pondera <= 0, NA_real_, pondera)
    ok   <- !is.na(pond) & !is.na(valor)
    if (!any(ok)) return(NA_real_)
    sum(valor[ok] * pond[ok]) / sum(pond[ok])
  }

  datos_cruce <- reactive({
    d <- datos_serie() |> filter(anio == as.integer(input$f2_anio))
    req(nrow(d) > 0)

    if (length(input$f2_jerarquia) > 0) {
      d <- d |> filter(substr(PP04D_COD, 3, 3) %in% input$f2_jerarquia)
    }
    d |> mutate(ocup2 = substr(PP04D_COD, 1, 2))
  })

  # Agregado por sexo (CH04: 1 Varón, 2 Mujer), a dos niveles:
  # total por Ocupación a 2 dígitos, y detalle por código completo (5 dígitos).
  cruce_ocup_sexo <- reactive({
    d <- datos_cruce()

    por_grupo <- function(data, agrupadores = character(0)) {
      base <- data |> mutate(CH04 = as.integer(CH04))
      base <- if (length(agrupadores) == 0) {
        base |> group_by(CH04)
      } else {
        base |> group_by(across(all_of(agrupadores)), CH04)
      }
      base |>
        summarise(
          cant    = sum(PONDERA, na.rm = TRUE),
          ingreso = promedio_ponderado(P21, PONDIIO),
          .groups = "drop"
        ) |>
        pivot_wider(
          names_from  = CH04,
          values_from = c(cant, ingreso),
          names_glue  = "{.value}_{CH04}",
          values_fill = list(cant = 0)
        )
    }

    general <- por_grupo(d) |>
      mutate(ocup2 = "TOTAL", PP04D_COD = NA_character_,
             desc_ocup2 = "Total general (todas las ocupaciones)")

    total   <- por_grupo(d, "ocup2") |> mutate(PP04D_COD = NA_character_)
    detalle <- por_grupo(d, c("ocup2", "PP04D_COD"))

    resto <- bind_rows(total, detalle) |>
      left_join(ocup2_desc, by = "ocup2", relationship = "many-to-one") |>
      arrange(ocup2, !is.na(PP04D_COD), PP04D_COD)

    bind_rows(general, resto)
  })

  output$tabla_cruce <- renderTable({
    d <- cruce_ocup_sexo()
    req(nrow(d) > 0)

    d |>
      transmute(
        Ocupación = dplyr::case_when(
          ocup2 == "TOTAL" ~ "<b>TOTAL GENERAL</b>",
          is.na(PP04D_COD) ~ sprintf("<b>%s - %s</b>", ocup2, desc_ocup2),
          TRUE              ~ sprintf("&nbsp;&nbsp;&nbsp;&nbsp;%s", PP04D_COD)
        ),
        `Cant. Varones`         = if_else(cant_1 == 0, "-", fmt_enteros(cant_1)),
        `Ingreso Prom. Varones` = if_else(is.na(ingreso_1), "-", fmt_pesos(ingreso_1)),
        `Cant. Mujeres`         = if_else(cant_2 == 0, "-", fmt_enteros(cant_2)),
        `Ingreso Prom. Mujeres` = if_else(is.na(ingreso_2), "-", fmt_pesos(ingreso_2)),
        `Brecha (%)`            = if_else(
          is.na(ingreso_1) | is.na(ingreso_2) | ingreso_1 == 0,
          "-",
          fmt_decimal(100 * (ingreso_1 - ingreso_2) / ingreso_1)
        )
      ) |>
      mutate(Ocupación = sprintf("<span style='font-size:0.8em;'>%s</span>", Ocupación))
  }, align = "lrrrrr", striped = TRUE, spacing = "s", sanitize.text.function = function(x) x)

  output$grafico_ingresos_sexo <- renderPlotly({
    d <- cruce_ocup_sexo() |>
      filter(is.na(PP04D_COD)) |>
      select(ocup2, desc_ocup2, ingreso_1, ingreso_2) |>
      pivot_longer(cols = c(ingreso_1, ingreso_2), names_to = "sexo", values_to = "ingreso") |>
      mutate(sexo = recode(sexo, ingreso_1 = "Varones", ingreso_2 = "Mujeres"))
    req(nrow(d) > 0)

    p <- suppressWarnings(
      ggplot(d, aes(x = ocup2, y = ingreso, fill = sexo,
                    text = sprintf("%s - %s\n%s: %s", ocup2, desc_ocup2, sexo,
                                   ifelse(is.na(ingreso), "s/d", fmt_pesos(ingreso))))) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Varones" = "#2C6E91", "Mujeres" = "#e74c3c")) +
        scale_y_continuous(labels = function(x) fmt_pesos(x)) +
        labs(x = "Ocupación (2 dígitos)", y = "Ingreso promedio", fill = NULL,
             title = sprintf("Ingreso promedio por sexo y ocupación — T%s %s", trimestre, input$f2_anio)) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 7))
    )

    ggplotly(p, tooltip = "text") |>
      plotly::layout(hoverlabel = list(bgcolor = "white"))
  })
}

shinyApp(ui, server)
