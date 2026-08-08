# Formatos generales

library(scales)

tema_bs <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2C6E91",
  base_font = "Calibri"
)


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

orden_educ <- c(
  "Sin instruccion/Primaria incompleta",
  "Primaria completa/Secundaria incompleta",
  "Secundaria completa/Superior incompleta",
  "Superior/Universitario completo"
)

orden_edad <- c("14-29", "30-64", "65+")

fmt_enteros <- function(x) {
  number(x,
         big.mark = ".",
         decimal.mark = ",",
         accuracy = 1)
}

fmt_decimal <- function(x) {
  number(x,
         big.mark = ".",
         decimal.mark = ",",
         accuracy = 0.1)
}

fmt_mixto <- function(x) {
  ifelse(x == round(x), fmt_enteros(x), fmt_decimal(x))
}