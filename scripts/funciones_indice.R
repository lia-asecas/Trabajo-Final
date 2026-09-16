# funciones_indice.R
# Funciones compartidas para trabajar con las bases reducidas del Índice de
# Precarización Laboral (bases/individual_procesada_<anio>_T<trim>.csv).
# Pensadas para poder reutilizarse desde 04_consultas.
#

# ---------------------------------------------------------------------------
# Variables derivadas (etiquetas legibles a partir de los códigos EPH)
# ---------------------------------------------------------------------------
construir_variables_derivadas <- function(data, orden_edad, orden_educ) {
  data %>%
    mutate(
      genero = case_when(
        CH04 == 1 ~ "Varon",
        CH04 == 2 ~ "Mujer",
        TRUE      ~ NA_character_
      ),
      region_nombre = case_when(
        REGION == 1  ~ "Gran Buenos Aires",
        REGION == 40 ~ "NOA",
        REGION == 41 ~ "NEA",
        REGION == 42 ~ "Cuyo",
        REGION == 43 ~ "Pampeana",
        REGION == 44 ~ "Patagonia",
        TRUE         ~ NA_character_
      ),
      grupo_etario = case_when(
        CH06 >= 14 & CH06 <= 29 ~ "14-29",
        CH06 >= 30 & CH06 <= 64 ~ "30-64",
        CH06 >= 65              ~ "65+",
        TRUE                    ~ NA_character_
      ),
      grupo_etario = factor(grupo_etario, levels = orden_edad, ordered = TRUE),
      nivel_educativo = case_when(
        NIVEL_ED %in% c(1, 2) ~ "Sin instruccion/Primaria incompleta",
        NIVEL_ED %in% c(3, 4) ~ "Primaria completa/Secundaria incompleta",
        NIVEL_ED %in% c(5, 6) ~ "Secundaria completa/Superior incompleta",
        NIVEL_ED == 7         ~ "Superior/Universitario completo",
        TRUE                  ~ NA_character_
      ),
      nivel_educativo = factor(nivel_educativo, levels = orden_educ, ordered = TRUE),
      JERARQUIA = substr(PP04D_COD, 3, 3),
      JERARQUIA = case_when(
        JERARQUIA %in% c("0", "2") ~ "Dirección o Jefes",
        JERARQUIA == "1"           ~ "Cuentapropia",
        JERARQUIA == "3"           ~ "Trabajadores Asalariados",
        TRUE                       ~ "0"
      ),
      JERARQUIA = factor(JERARQUIA, c("Dirección o Jefes", "Trabajadores Asalariados", "Cuentapropia"))
    )
}

# ---------------------------------------------------------------------------
# Tablas de índice y componentes (data.frame simples, sin formato de salida)
# ---------------------------------------------------------------------------
tabla_indice <- function(data, var_corte) {
  data %>%
    filter(!is.na({{ var_corte }}), !is.na(indice_precariedad_cat)) %>%
    group_by({{ var_corte }}, indice_precariedad_cat) %>%
    summarise(total_pond = sum(PONDERA, na.rm = TRUE), .groups = "drop") %>%
    group_by({{ var_corte }}) %>%
    mutate(pct = round(total_pond / sum(total_pond) * 100, 1)) %>%
    ungroup() %>%
    select(-total_pond) %>%
    complete(
      {{ var_corte }},
      indice_precariedad_cat = factor(c("Ninguna", "Baja", "Media", "Alta"), levels = c("Ninguna", "Baja", "Media", "Alta"), ordered = TRUE),
      fill = list(pct = 0)
    ) %>%
    pivot_wider(names_from = indice_precariedad_cat, values_from = pct, values_fill = 0) %>%
    select({{ var_corte }}, Ninguna, Baja, Media, Alta)
}

tabla_indice_abs <- function(data, var_corte) {
  data %>%
    filter(!is.na({{ var_corte }}), !is.na(indice_precariedad_cat)) %>%
    group_by({{ var_corte }}, indice_precariedad_cat) %>%
    summarise(total_pond = round(sum(PONDERA)), .groups = "drop") %>%
    complete(
      {{ var_corte }},
      indice_precariedad_cat = factor(
        c("Ninguna", "Baja", "Media", "Alta"),
        levels  = c("Ninguna", "Baja", "Media", "Alta"),
        ordered = TRUE
      ),
      fill = list(total_pond = 0)
    ) %>%
    pivot_wider(names_from = indice_precariedad_cat, values_from = total_pond, values_fill = 0) %>%
    select({{ var_corte }}, Ninguna, Baja, Media, Alta)
}

tabla_componentes <- function(data, var_corte) {
  data %>%
    filter(!is.na({{ var_corte }})) %>%
    group_by({{ var_corte }}) %>%
    summarise(
      `Ingresos (%)` = round(
        sum(PONDIIO[precario_ingreso_num == 1], na.rm = TRUE) /
          sum(PONDIIO[!is.na(precario_ingreso_num)], na.rm = TRUE) * 100, 1),
      `Jornada (%)`  = round(
        sum(precario_jornada_num  * PONDERA, na.rm = TRUE) /
          sum(PONDERA[!is.na(precario_jornada_num)]) * 100, 1),
      `Contrato (%)` = round(
        sum(precario_contrato_num * PONDERA, na.rm = TRUE) /
          sum(PONDERA[!is.na(precario_contrato_num)]) * 100, 1),
      .groups = "drop"
    )
}

calcular_evolucion_indice <- function(data) {
  data %>%
    filter(!is.na(indice_precariedad_cat)) %>%
    group_by(ANO4, indice_precariedad_cat) %>%
    summarise(total_pond = sum(PONDERA), .groups = "drop") %>%
    group_by(ANO4) %>%
    mutate(pct = round(total_pond / sum(total_pond) * 100, 1)) %>%
    ungroup()
}

calcular_evolucion_componentes <- function(data) {
  data %>%
    group_by(ANO4) %>%
    summarise(
      `Ingresos (%)`     = round(
        sum(PONDIIO[precario_ingreso_num == 1], na.rm = TRUE) /
          sum(PONDIIO[!is.na(precario_ingreso_num)], na.rm = TRUE) * 100, 1),
      `Jornada (%)`      = round(
        sum(precario_jornada_num  * PONDERA, na.rm = TRUE) /
          sum(PONDERA[!is.na(precario_jornada_num)]) * 100, 1),
      `Contrato (%)`     = round(
        sum(precario_contrato_num * PONDERA, na.rm = TRUE) /
          sum(PONDERA[!is.na(precario_contrato_num)]) * 100, 1),
      `No precarios (%)` = round(
        sum((num_dimensiones == 0) * PONDERA, na.rm = TRUE) / sum(PONDERA) * 100, 1),
      .groups = "drop"
    ) %>%
    rename(Año = ANO4)
}

nota_indice <- paste0(
  "El indice compuesto usa PONDERA. Los P21=-9 (no responden ingreso) se incluyen con ",
  "num_dimensiones = jornada + contrato (rango 0-2): nunca pueden alcanzar nivel Alta ",
  "y su Ninguna no implica informacion sobre ingresos."
)

nota_componentes <- paste0(
  "Ingresos: expansor PONDIIO (incluye redistribucion de no-respondentes P21=-9). ",
  "Jornada y Contrato: expansor PONDERA. Los porcentajes no suman 100% porque una ",
  "persona puede acumular mas de una dimension."
)

fuente_eph <- function(trim, anio) {
  paste0("Fuente: Elaboracion propia en base a EPH-INDEC T", trim, " ", anio, ".")
}

# ---------------------------------------------------------------------------
# Gráficos (ggplot2) — se pueden embeber igual en HTML o en Word
# ---------------------------------------------------------------------------
grafico_barras_indice <- function(data, var_x, titulo, subtitulo = NULL,
                                   label_x = "", reordenar = FALSE,
                                   fuente_txt = NULL) {
  df_plot <- data %>%
    filter(!is.na({{ var_x }}), !is.na(indice_precariedad_cat)) %>%
    group_by({{ var_x }}, indice_precariedad_cat) %>%
    summarise(total_pond = sum(PONDERA), .groups = "drop") %>%
    group_by({{ var_x }}) %>%
    mutate(pct = total_pond / sum(total_pond) * 100) %>%
    ungroup()

  if (reordenar) {
    orden <- df_plot %>%
      filter(indice_precariedad_cat == "Alta") %>%
      arrange(pct) %>%
      pull({{ var_x }})
    df_plot <- df_plot %>%
      mutate({{ var_x }} := factor({{ var_x }}, levels = as.character(orden)))
  }

  ggplot(df_plot, aes(x = {{ var_x }}, y = pct, fill = indice_precariedad_cat)) +
    geom_col(position = "stack", width = 0.7) +
    geom_text(
      aes(label = ifelse(pct >= 5, paste0(round(pct, 1), "%"), "")),
      position = position_stack(vjust = 0.5),
      size = 3.2, color = "white", fontface = "bold"
    ) +
    scale_fill_manual(values = PALETA_PREC, name = "Precariedad") +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    coord_flip() +
    labs(
      title    = titulo,
      subtitle = subtitulo,
      x = label_x, y = NULL,
      caption  = fuente_txt
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#434343"),
      plot.subtitle      = element_text(size = 10, color = "grey40"),
      legend.position    = "bottom",
      panel.grid.major.y = element_blank()
    )
}

grafico_evolucion_indice <- function(evolucion_indice, anios, fuente_txt = NULL,
                                      titulo = "Evolución del índice de precariedad laboral") {
  evolucion_indice %>%
    ggplot(aes(x = ANO4, y = pct, fill = indice_precariedad_cat)) +
    geom_area(position = "stack", alpha = 0.85) +
    geom_text(aes(label = ifelse(pct >= 5, paste0(pct, "%"), "")),
              position = position_stack(vjust = 0.5),
              size = 3, color = "white", fontface = "bold") +
    scale_fill_manual(values = PALETA_PREC, name = "Precariedad") +
    scale_x_continuous(breaks = anios) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    labs(
      title   = titulo,
      x = NULL, y = NULL,
      caption = fuente_txt
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#434343"),
      legend.position = "bottom"
    )
}

grafico_evolucion_componentes <- function(data, anios, fuente_txt = NULL,
                                           titulo = "Evolución de componentes de precariedad laboral") {
  data %>%
    group_by(ANO4) %>%
    summarise(
      Ingresos = round(
        sum(PONDIIO[precario_ingreso_num == 1], na.rm = TRUE) /
          sum(PONDIIO[!is.na(precario_ingreso_num)], na.rm = TRUE) * 100, 1),
      Jornada  = round(
        sum(precario_jornada_num  * PONDERA, na.rm = TRUE) /
          sum(PONDERA[!is.na(precario_jornada_num)]) * 100, 1),
      Contrato = round(
        sum(precario_contrato_num * PONDERA, na.rm = TRUE) /
          sum(PONDERA[!is.na(precario_contrato_num)]) * 100, 1),
      .groups = "drop"
    ) %>%
    pivot_longer(-ANO4, names_to = "Componente", values_to = "pct") %>%
    ggplot(aes(x = ANO4, y = pct, color = Componente, group = Componente)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(aes(label = paste0(pct, "%")),
              vjust = -0.8, size = 3, show.legend = FALSE) +
    scale_color_manual(values = PALETA_COMP) +
    scale_x_continuous(breaks = anios) +
    scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
    labs(
      title   = titulo,
      x = NULL, y = NULL,
      caption = fuente_txt
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#434343"),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
}

# ---------------------------------------------------------------------------
# Tabla con formato Word (flextable), consistente con la paleta del proyecto
# ---------------------------------------------------------------------------
tabla_word <- function(df, titulo = NULL, fuente_txt = NULL, digits = 1) {
  ft <- df %>%
    flextable::flextable() %>%
    flextable::colformat_double(digits = digits) %>%
    flextable::bg(part = "header", bg = "#2c3e50") %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::bold(part = "header") %>%
    flextable::bold(j = 1) %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(j = -1, align = "center", part = "body") %>%
    flextable::autofit()

  if (!is.null(titulo)) {
    ft <- ft %>% flextable::set_caption(caption = titulo)
  }
  if (!is.null(fuente_txt)) {
    ft <- ft %>%
      flextable::add_footer_lines(values = fuente_txt) %>%
      flextable::fontsize(size = 8, part = "footer") %>%
      flextable::color(color = "grey40", part = "footer")
  }
  ft
}
