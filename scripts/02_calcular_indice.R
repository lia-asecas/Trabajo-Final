# Aplicar el Indice a un año y trimestre determinado


aplicar_indice <- function(df, umbral_ingreso, anio_label = "", trim, guardar = TRUE) {
  
  setDT(df)
  
  # Normalizar PP04D_COD a 5 dígitos si existe (preservando NA)
  if ("PP04D_COD" %in% names(df)) {
    df[, PP04D_COD := as.character(PP04D_COD)]
    df[, PP04D_COD := ifelse(is.na(PP04D_COD), NA_character_, stringr::str_pad(PP04D_COD, width = 5, side = "left", pad = "0"))]
  }
  
  validaciones <- data.frame(
    anio = character(),
    concepto = character(),
    valor = numeric(),
    stringsAsFactors = FALSE
  )
  
  poblacion_total <- if ("PONDERA" %in% names(df)) sum(df$PONDERA, na.rm = TRUE) else NA_real_
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "Población TOTAL", valor = poblacion_total))
  
  ocup <- if ("ESTADO" %in% names(df)) df[ESTADO == 1, ] else df[0]
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "Población Ocupada", valor = sum(ocup$PONDERA, na.rm = TRUE)))
  
  ocup_tb <- as_tibble(ocup)
  
  if ("CAT_OCUP" %in% names(ocup_tb)) {
    validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  . Patrones", valor = sum(ocup_tb$PONDERA[ocup_tb$CAT_OCUP == 1], na.rm = TRUE)))
    validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  . Cta.Propia", valor = sum(ocup_tb$PONDERA[ocup_tb$CAT_OCUP == 2], na.rm = TRUE)))
    validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  . Asalariados", valor = sum(ocup_tb$PONDERA[ocup_tb$CAT_OCUP == 3], na.rm = TRUE)))
    validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  . TFSR", valor = sum(ocup_tb$PONDERA[ocup_tb$CAT_OCUP == 4], na.rm = TRUE)))
  }
  
  # Prueba sin Grupo 0 - substr(PP04D_COD,1,1) == "0"
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  . Grupo 0 (no Patrones)", valor = sum(ocup_tb$PONDERA[substr(ocup_tb$PP04D_COD,1,1)=="0" & !ocup_tb$CAT_OCUP==1], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  . FF.AA.", valor = sum(ocup_tb$PONDERA[substr(ocup_tb$PP04D_COD, 1, 2) %in% c("48", "49")], na.rm = TRUE)))
  
  
  # Filtrado de nuestro universo: excluir patrones y FF.SS (48/49)
  nuestros <- ocup_tb %>%
     filter(is.na(CAT_OCUP)  | CAT_OCUP != 1) %>%
    # filter(is.na(PP04D_COD) | substr(PP04D_COD, 1, 1) != "0" ) %>%
    filter(is.na(PP04D_COD) | !(substr(PP04D_COD, 1, 2) %in% c("48", "49")))
  
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "Nuestros Ocup (PONDERA)", valor = sum(nuestros$PONDERA, na.rm = TRUE)))
  if ("PONDIIO" %in% names(nuestros)) {
    validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "Nuestros Ocup (PONDIIO)", valor = sum(nuestros$PONDIIO, na.rm = TRUE)))
  }
  
  # Construcción del índice sobre 'nuestros'
  base <- nuestros %>%
    mutate(
      precario_ingreso_num = case_when(
        is.na(P21) | P21 == -9           ~ NA_real_,
        as.numeric(P21) < umbral_ingreso ~ 1,
        TRUE                             ~ 0
      ),
      precario_jornada_num = case_when(
        is.na(INTENSI) | is.na(PP03I) | is.na(PP03C) ~ NA_real_,
        INTENSI == 1 | INTENSI == 3                   ~ 1,
        INTENSI == 2 & PP03I == 1                     ~ 1,
        PP03C == 2                                    ~ 1,
        TRUE                                          ~ 0
      ),
      precario_contrato_num = case_when(
        is.na(PP07H) | is.na(PP07C) | is.na(CAT_OCUP) | is.na(ESTADO) ~ NA_real_,
        PP07H == 2 ~ 1,
        PP07H == 1 & (is.na(PP07G1) | PP07G1 == 2 |
                        is.na(PP07G2) | PP07G2 == 2 |
                        is.na(PP07G3) | PP07G3 == 2 |
                        is.na(PP07G4) | PP07G4 == 2) ~ 1,
        PP07H == 1 & PP07C == 1 ~ 1,
        CAT_OCUP == 4 ~ 1,
        CAT_OCUP == 2 & (is.na(CALIFICACION) | CALIFICACION != "Profesionales") ~ 1,
        TRUE ~ 0
      )
    )
  
  # Calculo nivel general
  es_precario <- with(base,
                      (!is.na(precario_ingreso_num)  & precario_ingreso_num  == 1) |
                        (!is.na(precario_jornada_num)  & precario_jornada_num  == 1) |
                        (!is.na(precario_contrato_num) & precario_contrato_num == 1)
  )
  
  Por_G  <- sum(base$PONDERA[es_precario], na.rm = TRUE) / sum(base$PONDERA, na.rm = TRUE) * 100
  Por_GV <- sum(base$PONDERA[es_precario & base$CH04 == 1], na.rm = TRUE) /
    sum(base$PONDERA[base$CH04 == 1], na.rm = TRUE) * 100
  Por_GM <- sum(base$PONDERA[es_precario & base$CH04 == 2], na.rm = TRUE) /
    sum(base$PONDERA[base$CH04 == 2], na.rm = TRUE) * 100
  Pre_I  <- sum(base$PONDIIO[base$precario_ingreso_num == 1], na.rm = TRUE)
  Pre_C  <- sum(base$PONDERA[base$precario_contrato_num == 1], na.rm = TRUE)
  Pre_J  <- sum(base$PONDERA[base$precario_jornada_num == 1], na.rm = TRUE)
  Por_PI <- Pre_I / sum(base$PONDIIO, na.rm = TRUE) * 100
  Por_PJ <- Pre_J / sum(base$PONDERA, na.rm = TRUE) * 100
  Por_PC <- Pre_C / sum(base$PONDERA, na.rm = TRUE) * 100
  
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "Precarios (PONDERA)", valor = Por_G))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = " . Varones", valor = Por_GV))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = " . Mujeres", valor = Por_GM))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = " . por Ingreso (PONDIIO)", valor = Pre_I))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "   . por Ingreso %", valor = Por_PI))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = " . por Jornada (PONDERA)", valor = Pre_J))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "   . por Jornada %", valor = Por_PJ))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = " . por Contrato (PONDERA)", valor = Pre_C))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "   . por Contrato %", valor = Por_PC))
  
  base <- base %>%
    mutate(
      indice_incompleto = is.na(precario_jornada_num) | is.na(precario_contrato_num),
      num_dimensiones = case_when(
        indice_incompleto                    ~ NA_real_,
        !is.na(precario_ingreso_num)         ~ (precario_ingreso_num + precario_jornada_num + precario_contrato_num),
        TRUE                                 ~ (precario_jornada_num + precario_contrato_num)
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
  
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "Indice Completo (PONDERA)", valor = sum(base$PONDERA[!base$indice_incompleto], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "Indice incompleto (PONDERA)", valor = sum(base$PONDERA[base$indice_incompleto], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  .Ninguna ", valor = sum(base$PONDERA[base$num_dimensiones == 0], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  .Baja    ", valor = sum(base$PONDERA[base$num_dimensiones == 1], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  .Moderada", valor = sum(base$PONDERA[base$num_dimensiones == 2], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  .Alta    ", valor = sum(base$PONDERA[base$num_dimensiones == 3], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = " . Ninguna  %", valor = sum(base$PONDERA[base$num_dimensiones == 0], na.rm = TRUE)*100/sum(base$PONDERA, na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = " . Baja     %", valor = sum(base$PONDERA[base$num_dimensiones == 1], na.rm = TRUE)*100/sum(base$PONDERA, na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = " . Moderada %", valor = sum(base$PONDERA[base$num_dimensiones == 2], na.rm = TRUE)*100/sum(base$PONDERA, na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = " . Alta     %", valor = sum(base$PONDERA[base$num_dimensiones == 3], na.rm = TRUE)*100/sum(base$PONDERA, na.rm = TRUE)))
  
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "Indice Completo (PONDIIO)", valor = sum(base$PONDIIO[!base$indice_incompleto], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "Indice incompleto ", valor = sum(base$PONDIIO[base$indice_incompleto], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  _ Ninguna  ", valor = sum(base$PONDIIO[base$num_dimensiones == 0], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  _ Baja     ", valor = sum(base$PONDIIO[base$num_dimensiones == 1], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  _ Moderada ", valor = sum(base$PONDIIO[base$num_dimensiones == 2], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  _ Alta     ", valor = sum(base$PONDIIO[base$num_dimensiones == 3], na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  _ Ninguna  %", valor = sum(base$PONDIIO[base$num_dimensiones == 0], na.rm = TRUE)*100/sum(base$PONDIIO, na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  _ Baja     %", valor = sum(base$PONDIIO[base$num_dimensiones == 1], na.rm = TRUE)*100/sum(base$PONDIIO, na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  _ Moderada %", valor = sum(base$PONDIIO[base$num_dimensiones == 2], na.rm = TRUE)*100/sum(base$PONDIIO, na.rm = TRUE)))
  validaciones <- rbind(validaciones, data.frame(anio = anio_label, concepto = "  _ Alta     %", valor = sum(base$PONDIIO[base$num_dimensiones == 3], na.rm = TRUE)*100/sum(base$PONDIIO, na.rm = TRUE)))
  
  # Guardar Base reduciada. 
  # Seleccionar las columnas del CSV (evita inconsistencias de tipo)
  
  vars_guardar <- c("ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "CODUSU", "NRO_HOGAR", "COMPONENTE", "CH03",   "CH04", "CH06", "ESTADO", "CAT_OCUP", "PONDERA", "EMPLEO", "SECTOR", "PONDIIO", "P21","INTENSI", "PP03I", "PP03C", "PP07H", "PP07C", "PP07G1", "PP07G2", "PP07G3", "PP07G4", "PP04D_COD","CALIFICACION", "caes_eph_label", "NIVEL_ED"
  )
  
  if (guardar) {
    
    nombre <- file.path("../bases", paste0("individual_procesada_", anio_label, "_T", trim, ".csv"))
    
    
    cols_keep <- c( "ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "CODUSU", "NRO_HOGAR", "COMPONENTE", "CH03", "CH04", "CH06", "ESTADO", "CAT_OCUP", "P21", "precario_ingreso_num",
                    "precario_jornada_num", "precario_contrato_num", "num_dimensiones", "indice_precariedad_cat", "EMPLEO", "SECTOR", "PONDERA", "PONDIIO", "NIVEL_ED", "caes_eph_label", "CALIFICACION", "PP04D_COD"
    )
    
    base_reducida <- base %>% select(any_of(cols_keep)) %>% mutate(ANO4 = ifelse(is.na(ANO4), as.integer(anio_label), ANO4))
    
    # Armar bases reduciadas (../bases/individualaatt.csv para reutilizar
    # Completar con NA las columnas esperadas que no existan en otros años
    # (EMPLEO, SECTOR, etc solo existe desde 2024), para que no corte la ejecucion
    # y quede en blanco en la tabla
    
    
    faltantes <- setdiff(cols_keep, names(base_reducida))
    if (length(faltantes) > 0) {
      base_reducida[faltantes] <- NA
    }
    base_reducida <- base_reducida %>% select(all_of(cols_keep))
    
    # Forzar tipos coherentes para las columnas que usamos (evita bind_rows fallando)
    base_reducida <- base_reducida %>%
      mutate(
        ANO4 = as.integer(ANO4),
        TRIMESTRE = as.integer(TRIMESTRE),
        REGION = suppressWarnings(as.integer(REGION)),
        AGLOMERADO = suppressWarnings(as.integer(AGLOMERADO)),
        CODUSU = as.character(CODUSU),
        NRO_HOGAR = as.integer(NRO_HOGAR),
        COMPONENTE = as.integer(COMPONENTE),
        CH03 = as.integer(CH03),
        CH04 = suppressWarnings(as.integer(CH04)),
        CH06 = suppressWarnings(as.integer(CH06)),
        ESTADO = suppressWarnings(as.integer(ESTADO)),
        CAT_OCUP = suppressWarnings(as.integer(CAT_OCUP)),
        P21 = suppressWarnings(as.numeric(as.character(P21))),
        precario_ingreso_num = as.integer(precario_ingreso_num),
        precario_jornada_num = as.integer(precario_jornada_num),
        precario_contrato_num = as.integer(precario_contrato_num),
        num_dimensiones = as.integer(num_dimensiones),
        indice_precariedad_cat = as.factor(indice_precariedad_cat),
        EMPLEO = suppressWarnings(as.integer(EMPLEO)),
        SECTOR = suppressWarnings(as.integer(SECTOR)),
        PONDERA = as.numeric(PONDERA),
        PONDIIO = as.numeric(PONDIIO),
        NIVEL_ED = suppressWarnings(as.integer(NIVEL_ED)),
        caes_eph_label = as.character(caes_eph_label),
        CALIFICACION = as.character(CALIFICACION),
        PP04D_COD = as.character(PP04D_COD)
      )
    write.csv(base_reducida, nombre, row.names = FALSE)
  }
  
  list(
    base = base_reducida,
    validaciones = validaciones,
    resumen = list(
      nivel_gral = round(Por_G,2),
      nivel_v    = round(Por_GV,2),
      nivel_m    = round(Por_GM,2),
      pre_i      = round(Por_PI,2),
      pre_j      = round(Por_PJ,2),
      pre_c      = round(Por_PC,2)
    )
  )
}