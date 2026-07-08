# =============================================================================
# INDICE DE PRECARIEDAD LABORAL — EPH INDEC
# Basado en tres dimensiones: Ingresos, Jornada y Contrato
# Niveles: Ninguna (0 dim.) | Baja (1 dim.) | Media (2 dim.) | Alta (3 dim.)
#
# CAMBIOS RESPECTO A VERSION ANTERIOR (v7):
#
#   [PONDIIO-ING]  Para el componente de Ingresos se usa PONDIIO como expansor
#                  poblacional, siguiendo la lógica del INDEC en sus
#                  publicaciones de distribución del ingreso y pobreza.
#                  PONDIIO redistribuye el peso de los no-respondentes (P21=-9,
#                  que tienen PONDIIO=0) entre quienes sí declararon ingreso.
#                  Esto evita excluir al ~23% de la muestra que no responde,
#                  asumiendo que se distribuyen como los respondentes.
#                  PONDERA se mantiene para el índice compuesto y todos los
#                  demás análisis.
#
#   [INC-NEG9]    Los P21=-9 se incluyen en el índice compuesto con
#                  num_dimensiones = precario_jornada + precario_contrato
#                  (rango 0-2). Los P21>=0 tienen rango 0-3. Esta asimetría
#                  queda documentada en la nota metodológica.
#                  IMPORTANTE: un -9 nunca puede alcanzar nivel "Alta" (3 dim.)
#                  y su "Ninguna" significa solo que no es precario en jornada
#                  ni en contrato, sin información sobre ingresos.
#
#   [VALID-INDEC] Se agrega bloque de validación al inicio de aplicar_indice()
#                  con totales comparables a publicaciones oficiales del INDEC:
#                  ocupados totales, asalariados, no asalariados, distribución
#                  por decil de ingreso con PONDIIO.
#
# CAMBIOS RESPECTO A VERSIONES ANTERIORES (v6 → v7, se conservan):
#
#   [T4-UMBRALES]    Umbrales CBT diciembres (T4), fuente Junta Interna ATE-INDEC.
#   [T4-TRIMESTRE]   Descarga trimester = 4.
#   [FIX-P21-DIRECTO] P21 directo para clasificación individual.
#   [FIX-P21-CERO]   P21==0 es precario en ingresos.
#   [FIX-OVERFLOW-1] as.numeric() para evitar integer overflow.
#   [FIX-POND-1]     PONDERA para expansiones poblacionales (salvo ingresos).
#   [CONSERVA]       Todos los FIX anteriores.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. LIBRERIAS
# -----------------------------------------------------------------------------
library(eph)
library(data.table)
library(tidyverse)
library(purrr)
library(openxlsx)
library(janitor)
library(kableExtra)
library(ggplot2)
library(scales)
library(forcats)


# -----------------------------------------------------------------------------
# 1. CONSTANTES Y PARAMETROS
# -----------------------------------------------------------------------------

COLS_TEXTO            <- c("CALIFICACION", "caes_eph_label", "cno_label", "CODUSU")
COLS_FACTOR_PRESERVAR <- "indice_precariedad_cat"

PALETA_PREC <- c(
  "Ninguna" = "#2ecc71",
  "Baja"    = "#f1c40f",
  "Media"   = "#e67e22",
  "Alta"    = "#e74c3c"
)

PALETA_COMP <- c(
  "Ingresos"  = "#3498db",
  "Jornada"   = "#9b59b6",
  "Contrato"  = "#e74c3c"
)

# [T4-UMBRALES] CBT adulto equivalente — diciembres (T4)
# Fuente: Junta Interna ATE-INDEC
umbrales_cbt <- c(
  "2017" =   8376.86,
  "2018" =  12753.85,
  "2019" =  20026.58,
  "2020" =  27699.03,
  "2021" =  39999.35,
  "2022" =  73589.64,
  "2023" = 250068.28,
  "2024" = 500701.94,
  "2025" = 691540.45
)

anios <- 2017:2025

trim <- 4

orden_educ <- c(
  "Sin instruccion/Primaria incompleta",
  "Primaria completa/Secundaria incompleta",
  "Secundaria completa/Superior incompleta",
  "Superior/Universitario completo"
)

orden_edad <- c("14-29", "30-64", "65+")

# Números enteros (hogares, población)
fmt_enteros <- function(x) {
  number(x,
         big.mark = ".", 
         decimal.mark = ",",
         accuracy = 1)
}

# -----------------------------------------------------------------------------
# 2. DESCARGA DE MICRODATOS — T4 de cada año (2017-2025)
# -----------------------------------------------------------------------------
bases_raw <- map(anios, function(a) {
  message("Descargando T", trim, " ", a, "...")
  df <- get_microdata(year = a, trimester = trim, type = "individual", vars = "all")
  df <- organize_cno(df)
  df <- organize_caes(df)
  df
})
names(bases_raw) <- as.character(anios)


# -----------------------------------------------------------------------------
# 3. FUNCION PRINCIPAL
# -----------------------------------------------------------------------------
aplicar_indice <- function(df, umbral_ingreso, anio_label = "") {

  setDT(df)

  # -------------------------------------------------------------------------
  # [VALID-INDEC] BLOQUE DE VALIDACIÓN
  # Totales comparables con publicaciones oficiales del INDEC.
  # Verificar contra: Mercado de Trabajo (EPH), Distribución del Ingreso.
  # -------------------------------------------------------------------------
  message("\n========== VALIDACIÓN ", anio_label, " ==========")

  # Total población relevada
  message("Poblacion total expandida (PONDERA):    ", fmt_enteros(sum(df$PONDERA, na.rm = TRUE)))

  # Ocupados totales
  ocup <- df[ESTADO == 1]
  message("Ocupados totales (PONDERA):             ", fmt_enteros(sum(ocup$PONDERA, na.rm = TRUE)))

  # Por categoría ocupacional
  ocup_tb <- as_tibble(ocup)
  message("  Patrones (CAT_OCUP=1):                ",
          fmt_enteros(sum(ocup_tb$PONDERA[ocup_tb$CAT_OCUP == 1], na.rm = TRUE)))
  message("  Cuenta propia (CAT_OCUP=2):           ",
          fmt_enteros(sum(ocup_tb$PONDERA[ocup_tb$CAT_OCUP == 2], na.rm = TRUE)))
  message("  Asalariados (CAT_OCUP=3):             ",
          fmt_enteros(sum(ocup_tb$PONDERA[ocup_tb$CAT_OCUP == 3], na.rm = TRUE)))
  message("  Trabajadores s/rem (CAT_OCUP=4):      ",
          fmt_enteros(sum(ocup_tb$PONDERA[ocup_tb$CAT_OCUP == 4], na.rm = TRUE)))

  # Distribución P21 en ocupados no patrones ni FF.SS. (nuestro universo)
  nuestros <- ocup_tb %>%
    filter(CAT_OCUP != 1,
           is.na(PP04D_COD) |
             !(as.integer(substr(as.character(PP04D_COD), 1, 2)) %in% c(48, 49)))

  message("Nuestro universo (no patrones, sin FF.SS): ",
          fmt_enteros(sum(nuestros$PONDERA, na.rm = TRUE)))
  message("  P21 >= 0 (declaran ingreso, PONDIIO): ",
          fmt_enteros(sum(nuestros$PONDIIO[!is.na(nuestros$P21) & nuestros$P21 >= 0], na.rm = TRUE)))
  message("  P21 = 0 (declaran no saber su ingreso, PONDIIO): ",
          fmt_enteros(sum(nuestros$PONDIIO[!is.na(nuestros$P21) & nuestros$P21 = 0], na.rm = TRUE)))
  message("  P21 = -9 (no responde, PONDIIO=0):    ",
          fmt_enteros(sum(nuestros$PONDERA[!is.na(nuestros$P21) & nuestros$P21 == -9], na.rm = TRUE)))
  message("  Suma PONDIIO total (debe ser cercano a PONDERA total universo): ",
          fmt_enteros(sum(nuestros$PONDIIO, na.rm = TRUE)))

  # Deciles de ingreso con PONDIIO (comparable con publicación INDEC)
  message("Distribución por decil de ingreso (PONDIIO, P21>=0):")
  decil_check <- nuestros %>%
    filter(!is.na(P21), P21 >= 0, !is.na(PONDII)) %>%
    arrange(P21) %>%
    mutate(
      cum_pond = cumsum(PONDIIO),
      total_pond = sum(PONDIIO, na.rm = TRUE),
      decil = ceiling(cum_pond / total_pond * 10)
    ) %>%
    group_by(decil) %>%
    summarise(
      pob_pondiio  = fmt_enteros(sum(PONDIIO, na.rm = TRUE)),
      ingreso_medio = fmt_enteros(mean(P21, na.rm = TRUE)),
      .groups = "drop"
    )
  print(decil_check)
  message("==========================================\n")

  # -------------------------------------------------------------------------
  # FILTROS BASE
  # -------------------------------------------------------------------------
  n_total <- sum(df$PONDERA, na.rm = TRUE)
  message("\n========== DIAGNÓSTICO ", anio_label, " ==========")
  message("Poblacion expandida total (suma PONDERA):          ", fmt_enteros(n_total))

  base <- df[ESTADO == 1 & CAT_OCUP != 1]
  pond_base <- sum(base$PONDERA, na.rm = TRUE)
  message("Tras filtro ocupados no patrones:                  ",
          fmt_enteros(pond_base),
          " (perdidos: ", fmt_enteros(n_total - pond_base), ")")

  base <- as_tibble(base) %>%
    filter(
      is.na(PP04D_COD) |
        !(as.integer(substr(as.character(PP04D_COD), 1, 2)) %in% c(48, 49))
    )
  pond_base2 <- sum(base$PONDERA, na.rm = TRUE)
  message("Tras excluir FF.SS/FF.AA:                          ",
          fmt_enteros(pond_base2),
          " (perdidos: ", fmt_enteros(n_total - pond_base2), ")")

  n_base_filtrada <- pond_base2

  # -------------------------------------------------------------------------
  # COMPONENTE 1 — Ingresos
  #
  # [FIX-P21-DIRECTO] P21 directo, sin ajuste por PONDIIO en la clasificación.
  # [FIX-P21-CERO]    P21==0 es precario (0 < umbral siempre).
  # [INC-NEG9]        P21==-9 queda NA: se redistribuye via PONDIIO al calcular
  #                   proporciones, pero no tiene valor asignado individualmente.
  # -------------------------------------------------------------------------
  base <- base %>%
    mutate(
      precario_ingreso_num = case_when(
        is.na(P21) | P21 == -9           ~ NA_real_,
        as.numeric(P21) < umbral_ingreso ~ 1,
        TRUE                             ~ 0
      )
    )

  # [PONDIIO-ING] Diagnóstico con PONDIIO como expansor para ingresos
  pond_prec_ing   <- sum(base$PONDIIO[!is.na(base$precario_ingreso_num) &
                                        base$precario_ingreso_num == 1], na.rm = TRUE)
  pond_noprec_ing <- sum(base$PONDIIO[!is.na(base$precario_ingreso_num) &
                                        base$precario_ingreso_num == 0], na.rm = TRUE)
  pond_nr_ing     <- sum(base$PONDERA[!is.na(base$P21) & base$P21 == -9], na.rm = TRUE)
  pond_cero_ing   <- sum(base$PONDERA[!is.na(base$P21) & base$P21 == 0],  na.rm = TRUE)
  pond_total_diio <- sum(base$PONDIIO, na.rm = TRUE)

  message("  P21 == 0  (precario, PONDERA):                  ", fmt_enteros(pond_cero_ing),
          " (", round(pond_cero_ing / n_base_filtrada * 100, 1), "%)")
  message("  P21 == -9 (no responde, PONDERA):               ", fmt_enteros(pond_nr_ing),
          " (", round(pond_nr_ing / n_base_filtrada * 100, 1), "%)")
  message("  Precario ingresos PI=1 (PONDIIO):               ", fmt_enteros(pond_prec_ing),
          " (", round(pond_prec_ing / pond_total_diio * 100, 1), "% sobre PONDIIO)")
  message("  No precario ingresos PI=0 (PONDIIO):            ", fmt_enteros(pond_noprec_ing),
          " (", round(pond_noprec_ing / pond_total_diio * 100, 1), "% sobre PONDIIO)")
  message("  Suma PONDIIO universo (debe ~ PONDERA universo):", fmt_enteros(pond_total_diio))

  # -------------------------------------------------------------------------
  # COMPONENTE 2 — Jornada
  # -------------------------------------------------------------------------
  base <- base %>%
    mutate(
      precario_jornada_num = case_when(
        is.na(INTENSI) | is.na(PP03I) | is.na(PP03C) ~ NA_real_,
        INTENSI == 1                                   ~ 1,
        INTENSI == 3                                   ~ 1,
        INTENSI == 2 & PP03I == 1                      ~ 1,
        PP03C == 2                                     ~ 1,
        TRUE                                           ~ 0
      )
    )

  n_na_jornada <- sum(base$PONDERA[is.na(base$precario_jornada_num)], na.rm = TRUE)
  message("Pob. sin dato Jornada (PONDERA):                   ", fmt_enteros(n_na_jornada),
          " (", round(n_na_jornada / n_base_filtrada * 100, 1), "%)")

  # -------------------------------------------------------------------------
  # COMPONENTE 3 — Contrato
  # -------------------------------------------------------------------------
  base <- base %>%
    mutate(
      precario_contrato_num = case_when(
        is.na(PP07H) | is.na(PP07C) | is.na(CAT_OCUP) | is.na(ESTADO) ~ NA_real_,
        PP07H == 2 ~ 1,
        PP07H == 1 & (is.na(PP07G1) | PP07G1 == 2 |
                        is.na(PP07G2) | PP07G2 == 2 |
                        is.na(PP07G3) | PP07G3 == 2 |
                        is.na(PP07G4) | PP07G4 == 2) ~ 1,
        PP07H == 1 & PP07C == 1 ~ 1,
        CAT_OCUP == 4 ~ 1,
        CAT_OCUP == 2 & (CALIFICACION != "Profesionales" |
                           is.na(CALIFICACION)) ~ 1,
        TRUE ~ 0
      )
    )

  n_na_contrato <- sum(base$PONDERA[is.na(base$precario_contrato_num)], na.rm = TRUE)
  message("Pob. sin dato Contrato (PONDERA):                  ", fmt_enteros(n_na_contrato),
          " (", round(n_na_contrato / n_base_filtrada * 100, 1), "%)")

  # -------------------------------------------------------------------------
  # [INC-NEG9] CONSTRUCCIÓN DEL ÍNDICE
  #
  # Para quienes tienen P21 válido (>=0): num_dimensiones = PI + PJ + PC (0-3)
  # Para P21 = -9 (NA en ingresos):       num_dimensiones = PJ + PC     (0-2)
  #
  # Nota metodológica: los -9 nunca pueden alcanzar nivel "Alta" y su
  # "Ninguna" significa solo no precario en jornada ni contrato, sin
  # información sobre ingresos.
  # Solo se excluyen quienes tienen NA en jornada O contrato.
  # -------------------------------------------------------------------------
  base <- base %>%
    mutate(
      # Excluir solo si falta jornada o contrato (ingresos NA es aceptable)
      indice_incompleto = is.na(precario_jornada_num) | is.na(precario_contrato_num),

      num_dimensiones = case_when(
        indice_incompleto                    ~ NA_real_,
        !is.na(precario_ingreso_num)         ~
          precario_ingreso_num + precario_jornada_num + precario_contrato_num,
        TRUE                                 ~   # P21 = -9: solo jornada + contrato
          precario_jornada_num + precario_contrato_num
      ),

      indice_precariedad_cat = factor(
        case_when(
          is.na(num_dimensiones) ~ NA_character_,
          num_dimensiones == 0   ~ "Ninguna",
          num_dimensiones == 1   ~ "Baja",
          num_dimensiones == 2   ~ "Media",
          num_dimensiones == 3   ~ "Alta"
        ),
        levels  = c("Ninguna", "Baja", "Media", "Alta"),
        ordered = TRUE
      )
    )

  n_excluidos <- sum(base$PONDERA[base$indice_incompleto],  na.rm = TRUE)
  n_final     <- sum(base$PONDERA[!base$indice_incompleto], na.rm = TRUE)
  n_neg9_inc  <- sum(base$PONDERA[!base$indice_incompleto & is.na(base$precario_ingreso_num)], na.rm = TRUE)

  message("Excluidos por NA en jornada o contrato:            ", fmt_enteros(n_excluidos),
          " (", round(n_excluidos / n_base_filtrada * 100, 1), "%)")
  message("BASE FINAL UTILIZABLE (pob. expandida, PONDERA):   ", fmt_enteros(n_final),
          " (", round(n_final / n_base_filtrada * 100, 1), "%)")
  message("  De los cuales P21=-9 (índice 0-2):               ", fmt_enteros(n_neg9_inc),
          " (", round(n_neg9_inc / n_final * 100, 1), "% del total)")
  message("==========================================\n")

  base <- base %>% filter(!indice_incompleto)

  return(base)
}


# -----------------------------------------------------------------------------
# 4. APLICAR EL INDICE Y HOMOGENEIZAR
# -----------------------------------------------------------------------------
bases_indice <- imap(bases_raw, function(df, anio) {
  message("Calculando indice para ", anio, "...")
  umbral <- umbrales_cbt[anio]
  aplicar_indice(df, umbral_ingreso = umbral, anio_label = anio)
})

bases_indice <- map(bases_indice, function(df) {
  cols_texto_presentes  <- intersect(COLS_TEXTO, names(df))
  cols_factor_presentes <- intersect(COLS_FACTOR_PRESERVAR, names(df))

  df %>%
    mutate(
      across(
        where(is.factor) & !all_of(cols_factor_presentes),
        as.character
      ),
      across(
        where(is.character) & !all_of(cols_texto_presentes),
        ~ suppressWarnings(as.numeric(.x))
      )
    )
})

resultados_todos <- bind_rows(bases_indice, .id = "anio") %>%
  mutate(anio = as.integer(anio))


# -----------------------------------------------------------------------------
# Variables sociodemo
# -----------------------------------------------------------------------------
resultados_todos <- resultados_todos %>%
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
    nivel_educativo = factor(nivel_educativo, levels = orden_educ, ordered = TRUE)
  )


ultimo_anio   <- max(resultados_todos$ANO4)
base_ultimo   <- resultados_todos %>% filter(ANO4 == ultimo_anio)
base_ocupados <- base_ultimo %>% filter(ESTADO == 1)


# =============================================================================
# 5. FUNCIONES AUXILIARES
# =============================================================================

# [PONDIIO-ING] tabla_componentes usa PONDIIO para ingresos, PONDERA para el resto
tabla_indice <- function(data, var_corte) {
  data %>%
    filter(!is.na({{ var_corte }}), !is.na(indice_precariedad_cat)) %>%
    group_by({{ var_corte }}, indice_precariedad_cat) %>%
    summarise(total_pond = sum(PONDERA), .groups = "drop") %>%
    group_by({{ var_corte }}) %>%
    mutate(pct = round(total_pond / sum(total_pond) * 100, 1)) %>%
    ungroup() %>%
    select(-total_pond) %>%
    complete(
      {{ var_corte }},
      indice_precariedad_cat = factor(
        c("Ninguna", "Baja", "Media", "Alta"),
        levels  = c("Ninguna", "Baja", "Media", "Alta"),
        ordered = TRUE
      ),
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
      # [PONDIIO-ING] Ingresos: expansor PONDIIO, denominador = sum(PONDIIO) sobre P21>=0
      `Ingresos (%)`  = round(
        sum(PONDIIO[precario_ingreso_num == 1], na.rm = TRUE) /
          sum(PONDIIO[!is.na(precario_ingreso_num)], na.rm = TRUE) * 100, 1),
      # Jornada y Contrato: expansor PONDERA como siempre
      `Jornada (%)`   = round(
        sum(precario_jornada_num  * PONDERA, na.rm = TRUE) /
          sum(PONDERA[!is.na(precario_jornada_num)]) * 100, 1),
      `Contrato (%)`  = round(
        sum(precario_contrato_num * PONDERA, na.rm = TRUE) /
          sum(PONDERA[!is.na(precario_contrato_num)]) * 100, 1),
      .groups = "drop"
    )
}

nota_componentes <- paste0(
  "Ingresos: expansor PONDIIO (incluye redistribucion de no-respondentes P21=-9). ",
  "Jornada y Contrato: expansor PONDERA. ",
  "Los porcentajes no suman 100% porque una persona puede acumular mas de una dimension."
)

nota_indice <- paste0(
  "El indice compuesto usa PONDERA. ",
  "Los P21=-9 (no responden ingreso) se incluyen con num_dimensiones = jornada + contrato (rango 0-2): ",
  "nunca pueden alcanzar nivel Alta y su Ninguna no implica informacion sobre ingresos."
)

kable_prec <- function(df, titulo, fuente = NULL, font_size = 13) {

  if (nrow(df) == 0) {
    message("AVISO: tabla vacia para '", titulo, "' — se omite.")
    return(invisible(NULL))
  }

  k <- df %>%
    kable(
      format  = "html",
      caption = titulo,
      align   = c("l", rep("c", ncol(df) - 1))
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size  = font_size
    ) %>%
    row_spec(0, bold = TRUE, color = "white", background = "#2c3e50") %>%
    column_spec(1, bold = TRUE)

  if (!is.null(fuente)) {
    k <- k %>% footnote(general = fuente, general_title = "")
  }
  k
}

grafico_barras_indice <- function(data, var_x, titulo, subtitulo = NULL,
                                  label_x = "", reordenar = FALSE) {
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
      caption  = paste0("Fuente: Elaboracion propia en base a EPH-INDEC T", trim, " ", ultimo_anio, ".")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(size = 10, color = "grey40"),
      legend.position    = "bottom",
      panel.grid.major.y = element_blank()
    )
}

grafico_evolucion_componentes <- function(data) {
  data %>%
    group_by(ANO4) %>%
    summarise(
      # [PONDIIO-ING] Ingresos con PONDIIO
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
      title   = "Evolucion de componentes de precariedad laboral — T", trim, " 2017-2025",
      x = NULL, y = NULL,
      caption = paste0("Fuente: Elaboracion propia en base a EPH-INDEC T", trim, " ", ultimo_anio, ".")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
}


# =============================================================================
# 6. TABLAS Y GRAFICOS
# =============================================================================

# -----------------------------------------------------------------------------
# 6A. EVOLUCION TEMPORAL DEL INDICE
# -----------------------------------------------------------------------------
evolucion_indice <- resultados_todos %>%
  filter(!is.na(indice_precariedad_cat)) %>%
  group_by(ANO4, indice_precariedad_cat) %>%
  summarise(total_pond = sum(PONDERA), .groups = "drop") %>%
  group_by(ANO4) %>%
  mutate(pct = round(total_pond / sum(total_pond) * 100, 1)) %>%
  ungroup()

tabla_evolucion <- evolucion_indice %>%
  select(ANO4, indice_precariedad_cat, pct) %>%
  pivot_wider(names_from = ANO4, values_from = pct, names_sort = TRUE) %>%
  arrange(indice_precariedad_cat) %>%
  rename(`Nivel de precariedad` = indice_precariedad_cat)

kable_prec(tabla_evolucion,
           "EVOLUCION DEL INDICE DE PRECARIEDAD LABORAL — T", trim, " 2017-2025 (%)",
           fuente = nota_indice)

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
    title   = "Evolucion del indice de precariedad laboral — T", trim, " 2017-2025",
    x = NULL, y = NULL,
    caption = paste0("Fuente: Elaboracion propia en base a EPH-INDEC T", trim, " ", ultimo_anio, ".")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

# -----------------------------------------------------------------------------
# 6B. EVOLUCION DE COMPONENTES
# -----------------------------------------------------------------------------
evolucion_componentes <- resultados_todos %>%
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

kable_prec(evolucion_componentes,
           "EVOLUCION DE COMPONENTES DE PRECARIEDAD — T", trim, " 2017-2025 (%)",
           fuente = nota_componentes)

grafico_evolucion_componentes(resultados_todos)

# -----------------------------------------------------------------------------
# 6C. GENERO
# -----------------------------------------------------------------------------
kable_prec(
  tabla_indice(base_ultimo, genero) %>% rename(Genero = genero),
  paste0("INDICE DE PRECARIEDAD POR GENERO — T", trim, " ", ultimo_anio, " (%)"),
  fuente = nota_indice)
kable_prec(
  tabla_indice_abs(base_ultimo, genero) %>% rename(Genero = genero),
  paste0("INDICE DE PRECARIEDAD POR GENERO — T", trim, " ", ultimo_anio, " (pob. expandida)"))
kable_prec(
  tabla_componentes(base_ultimo, genero) %>% rename(Genero = genero),
  paste0("COMPONENTES DE PRECARIEDAD POR GENERO — T", trim, " ", ultimo_anio, " (%)"),
  fuente = nota_componentes)
grafico_barras_indice(base_ultimo, genero,
                      titulo  = paste0("Indice de precariedad por genero — T", trim, " ", ultimo_anio),
                      label_x = "Genero")

# -----------------------------------------------------------------------------
# 6D. REGION
# -----------------------------------------------------------------------------
kable_prec(
  tabla_indice(base_ultimo, region_nombre) %>% rename(Region = region_nombre),
  paste0("INDICE DE PRECARIEDAD POR REGION — T", trim, " ", ultimo_anio, " (%)"),
  fuente = nota_indice)
kable_prec(
  tabla_indice_abs(base_ultimo, region_nombre) %>% rename(Region = region_nombre),
  paste0("INDICE DE PRECARIEDAD POR REGION — T", trim, " ", ultimo_anio, " (pob. expandida)"))
kable_prec(
  tabla_componentes(base_ultimo, region_nombre) %>% rename(Region = region_nombre),
  paste0("COMPONENTES DE PRECARIEDAD POR REGION — T", trim, " ", ultimo_anio, " (%)"),
  fuente = nota_componentes)
grafico_barras_indice(base_ultimo, region_nombre,
                      titulo    = paste0("Indice de precariedad por region — T", trim, " ", ultimo_anio),
                      label_x   = "Region",
                      reordenar = TRUE)

# -----------------------------------------------------------------------------
# 6E. RAMA DE ACTIVIDAD
# -----------------------------------------------------------------------------
kable_prec(
  tabla_indice(base_ocupados, caes_eph_label) %>% rename(Rama = caes_eph_label),
  paste0("INDICE DE PRECARIEDAD POR RAMA — T", trim, " ", ultimo_anio, " (%)"),
  fuente = nota_indice, font_size = 11)
kable_prec(
  tabla_indice_abs(base_ocupados, caes_eph_label) %>% rename(Rama = caes_eph_label),
  paste0("INDICE DE PRECARIEDAD POR RAMA — T", trim, " ", ultimo_anio, " (pob. expandida)"),
  font_size = 11)
kable_prec(
  tabla_componentes(base_ocupados, caes_eph_label) %>% rename(Rama = caes_eph_label),
  paste0("COMPONENTES DE PRECARIEDAD POR RAMA — T4 ", ultimo_anio, " (%)"),
  fuente = nota_componentes, font_size = 11)
grafico_barras_indice(base_ocupados, caes_eph_label,
                      titulo    = paste0("Indice de precariedad por rama — T", trim, " ", ultimo_anio),
                      label_x   = "Rama",
                      reordenar = TRUE)

# -----------------------------------------------------------------------------
# 6F. GRUPO ETARIO
# -----------------------------------------------------------------------------
kable_prec(
  tabla_indice(base_ultimo, grupo_etario) %>%
    arrange(grupo_etario) %>% rename(`Grupo etario` = grupo_etario),
  paste0("INDICE DE PRECARIEDAD POR GRUPO ETARIO — T", trim, " ", ultimo_anio, " (%)"),
  fuente = nota_indice)
kable_prec(
  tabla_indice_abs(base_ultimo, grupo_etario) %>%
    arrange(grupo_etario) %>% rename(`Grupo etario` = grupo_etario),
  paste0("INDICE DE PRECARIEDAD POR GRUPO ETARIO — T", trim, " ", ultimo_anio, " (pob. expandida)"))
kable_prec(
  tabla_componentes(base_ultimo, grupo_etario) %>%
    arrange(grupo_etario) %>% rename(`Grupo etario` = grupo_etario),
  paste0("COMPONENTES DE PRECARIEDAD POR GRUPO ETARIO — T", trim, " ", ultimo_anio, " (%)"),
  fuente = nota_componentes)
grafico_barras_indice(base_ultimo, grupo_etario,
                      titulo  = paste0("Indice de precariedad por grupo etario — T", trim, " ", ultimo_anio),
                      label_x = "Grupo etario")

# -----------------------------------------------------------------------------
# 6G. NIVEL EDUCATIVO
# -----------------------------------------------------------------------------
kable_prec(
  tabla_indice(base_ultimo, nivel_educativo) %>%
    arrange(nivel_educativo) %>% rename(`Nivel educativo` = nivel_educativo),
  paste0("INDICE DE PRECARIEDAD POR NIVEL EDUCATIVO — T", trim, " ", ultimo_anio, " (%)"),
  fuente = nota_indice)
kable_prec(
  tabla_indice_abs(base_ultimo, nivel_educativo) %>%
    arrange(nivel_educativo) %>% rename(`Nivel educativo` = nivel_educativo),
  paste0("INDICE DE PRECARIEDAD POR NIVEL EDUCATIVO — T", trim, " ", ultimo_anio, " (pob. expandida)"))
kable_prec(
  tabla_componentes(base_ultimo, nivel_educativo) %>%
    arrange(nivel_educativo) %>% rename(`Nivel educativo` = nivel_educativo),
  paste0("COMPONENTES DE PRECARIEDAD POR NIVEL EDUCATIVO — T", trim, " ", ultimo_anio, " (%)"),
  fuente = nota_componentes)
grafico_barras_indice(base_ultimo, nivel_educativo,
                      titulo  = paste0("Indice de precariedad por nivel educativo — T", trim, " ", ultimo_anio),
                      label_x = "Nivel educativo")

# -----------------------------------------------------------------------------
# 6H. CRUCE GENERO x GRUPO ETARIO
# -----------------------------------------------------------------------------
base_ultimo %>%
  filter(!is.na(genero), !is.na(grupo_etario), !is.na(indice_precariedad_cat)) %>%
  group_by(genero, grupo_etario, indice_precariedad_cat) %>%
  summarise(total_pond = sum(PONDERA), .groups = "drop") %>%
  group_by(genero, grupo_etario) %>%
  mutate(pct = round(total_pond / sum(total_pond) * 100, 1)) %>%
  ungroup() %>%
  select(-total_pond) %>%
  complete(genero, grupo_etario,
           indice_precariedad_cat = factor(c("Ninguna","Baja","Media","Alta"),
                                           levels = c("Ninguna","Baja","Media","Alta"),
                                           ordered = TRUE),
           fill = list(pct = 0)) %>%
  pivot_wider(names_from = indice_precariedad_cat, values_from = pct, values_fill = 0) %>%
  select(genero, grupo_etario, Ninguna, Baja, Media, Alta) %>%
  arrange(genero, grupo_etario) %>%
  rename(Genero = genero, `Grupo etario` = grupo_etario) %>%
  kable_prec(paste0("INDICE DE PRECARIEDAD POR GENERO Y GRUPO ETARIO — T", trim, " ", ultimo_anio, " (%)"),
             fuente = nota_indice)

# -----------------------------------------------------------------------------
# 6I. CRUCE GENERO x NIVEL EDUCATIVO
# -----------------------------------------------------------------------------
base_ultimo %>%
  filter(!is.na(genero), !is.na(nivel_educativo), !is.na(indice_precariedad_cat)) %>%
  group_by(genero, nivel_educativo, indice_precariedad_cat) %>%
  summarise(total_pond = sum(PONDERA), .groups = "drop") %>%
  group_by(genero, nivel_educativo) %>%
  mutate(pct = round(total_pond / sum(total_pond) * 100, 1)) %>%
  ungroup() %>%
  select(-total_pond) %>%
  complete(genero, nivel_educativo,
           indice_precariedad_cat = factor(c("Ninguna","Baja","Media","Alta"),
                                           levels = c("Ninguna","Baja","Media","Alta"),
                                           ordered = TRUE),
           fill = list(pct = 0)) %>%
  pivot_wider(names_from = indice_precariedad_cat, values_from = pct, values_fill = 0) %>%
  select(genero, nivel_educativo, Ninguna, Baja, Media, Alta) %>%
  arrange(genero, nivel_educativo) %>%
  rename(Genero = genero, `Nivel educativo` = nivel_educativo) %>%
  kable_prec(paste0("INDICE DE PRECARIEDAD POR GENERO Y NIVEL EDUCATIVO — T", trim, " ", ultimo_anio, " (%)"),
             fuente = nota_indice)


# =============================================================================
# 7. EXPORTAR A EXCEL
# =============================================================================
wb_out <- createWorkbook()

sheets <- list(
  "Indice_evolucion"        = tabla_evolucion,
  "Componentes_evolucion"   = evolucion_componentes,
  "Indice_genero"           = tabla_indice(base_ultimo, genero)               %>% rename(Genero = genero),
  "Indice_genero_abs"       = tabla_indice_abs(base_ultimo, genero)           %>% rename(Genero = genero),
  "Indice_region"           = tabla_indice(base_ultimo, region_nombre)        %>% rename(Region = region_nombre),
  "Indice_region_abs"       = tabla_indice_abs(base_ultimo, region_nombre)    %>% rename(Region = region_nombre),
  "Indice_rama"             = tabla_indice(base_ocupados, caes_eph_label)     %>% rename(Rama = caes_eph_label),
  "Indice_rama_abs"         = tabla_indice_abs(base_ocupados, caes_eph_label) %>% rename(Rama = caes_eph_label),
  "Indice_edad"             = tabla_indice(base_ultimo, grupo_etario)         %>% arrange(grupo_etario) %>% rename(`Grupo etario` = grupo_etario),
  "Indice_edad_abs"         = tabla_indice_abs(base_ultimo, grupo_etario)     %>% arrange(grupo_etario) %>% rename(`Grupo etario` = grupo_etario),
  "Indice_educacion"        = tabla_indice(base_ultimo, nivel_educativo)      %>% arrange(nivel_educativo) %>% rename(`Nivel educativo` = nivel_educativo),
  "Indice_educacion_abs"    = tabla_indice_abs(base_ultimo, nivel_educativo)  %>% arrange(nivel_educativo) %>% rename(`Nivel educativo` = nivel_educativo),
  "Comp_genero"             = tabla_componentes(base_ultimo, genero)          %>% rename(Genero = genero),
  "Comp_region"             = tabla_componentes(base_ultimo, region_nombre)   %>% rename(Region = region_nombre),
  "Comp_rama"               = tabla_componentes(base_ocupados, caes_eph_label) %>% rename(Rama = caes_eph_label),
  "Comp_edad"               = tabla_componentes(base_ultimo, grupo_etario)    %>% arrange(grupo_etario) %>% rename(`Grupo etario` = grupo_etario),
  "Comp_educacion"          = tabla_componentes(base_ultimo, nivel_educativo) %>% arrange(nivel_educativo) %>% rename(`Nivel educativo` = nivel_educativo),
  "Umbrales_CBT"            = data.frame(
    Anio              = as.integer(names(umbrales_cbt)),
    CBT_ATE_INDEC_dic = unname(umbrales_cbt)
  )
)

walk2(names(sheets), sheets, function(nombre, datos) {
  addWorksheet(wb_out, nombre)
  writeData(wb_out, nombre, datos)
})

saveWorkbook(wb_out, "resultados_indice_precarizacion_v8_T4.xlsx", overwrite = TRUE)
message("Resultados exportados a: resultados_indice_precarizacion_v8_T4.xlsx")


# =============================================================================
# FIN DEL SCRIPT v8 — T4 2017-2025
# =============================================================================
