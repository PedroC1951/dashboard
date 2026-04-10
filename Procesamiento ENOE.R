# ============================================
# LIBRERÍAS
# ============================================
pacman::p_load(data.table, haven, ggplot2, scales)


# ============================================
# FUNCIÓN AUXILIAR
# ============================================
get_prefijo <- function(anio, trim) {
  if (anio == 2020 & trim == 1) return("ENOE_")
  if (anio == 2020 & trim == 2) return(NA)      # no hay
  if (anio == 2020 & trim >= 3) return("ENOEN_")
  if (anio %in% 2021:2022) return("ENOEN_")
  if (anio >= 2023) return("ENOE_")
  return("")
}

get_periodo <- function(anio, trim){
  paste0(trim, substr(anio, 3, 4))
}

# ============================================
# FUNCIÓN PRINCIPAL
# ============================================
procesar_enoe_serie <- function(base_path = "E:/ENOEr") {

  lista <- list()
  resumen_validacion <- list()
  k <- 1
  v <- 1

  for (anio in 2005:2019) {
    for (trim in 1:4) {

      if (anio == 2020 & trim == 2) next

      prefijo <- get_prefijo(anio, trim)
      if (is.na(prefijo)) next

      periodo <- get_periodo(anio, trim)
      carpeta <- file.path(base_path, anio, paste0(anio, "-", trim))

      cat("Procesando:", anio, "T", trim, "\n")

      sdem_file <- list.files(
        carpeta,
        pattern = paste0("(?i)", prefijo, "SDEMT", periodo, ".*\\.dta"),
        full.names = TRUE
      )

      coe2_file <- list.files(
        carpeta,
        pattern = paste0("(?i)", prefijo, "COE2T", periodo, ".*\\.dta"),
        full.names = TRUE
      )

      if (length(sdem_file) == 0) next

      # ============================
      # LEER SDEM
      # ============================
      sdem <- as.data.table(read_dta(sdem_file))
      setnames(sdem, toupper(names(sdem)))

      # Edad robusta
      edad_var <- c("EDAD","EDA","EDAD_ANOS")[
        c("EDAD","EDA","EDAD_ANOS") %in% names(sdem)
      ][1]

      sdem[, edad := as.numeric(get(edad_var))]
      sdem[, sexo := as.numeric(SEX)]
      if ("ENT" %in% names(sdem)) {
        sdem[, entidad := as.numeric(ENT)]
      } else if ("CVE_ENT" %in% names(sdem)) {
        sdem[, entidad := as.numeric(CVE_ENT)]
      } else {
        sdem[, entidad := NA_real_]
      }
      sdem[, escolaridad := as.numeric(NIV_INS)]


      # Ocupado
      sdem[, ocupado := as.integer(CLASE2 == 1)]

      # Factor correcto
      if ("FAC_TRI" %in% names(sdem)) {
        sdem[, factor := as.numeric(FAC_TRI)]
      } else {
        sdem[, factor := as.numeric(FAC)]
      }

      # Universo oficial
      sdem <- sdem[edad >= 15]

      # ============================
      # TOTAL OFICIAL (SDEM)
      # ============================
      total_sdem <- sdem[
        ocupado == 1,
        sum(factor, na.rm = TRUE)
      ]

      # ============================
      # LEER COE (SI EXISTE)
      # ============================
      if (length(coe2_file) > 0) {

        coe2 <- as.data.table(read_dta(coe2_file))
        setnames(coe2, toupper(names(coe2)))

        # quitar factores de COE
        if ("FAC" %in% names(coe2)) coe2[, FAC := NULL]
        if ("FAC_TRI" %in% names(coe2)) coe2[, FAC_TRI := NULL]

        # llaves
        llaves <- c("CD_A","ENT","CON","V_SEL","N_HOG","H_MUD","N_REN")
        llaves <- llaves[llaves %in% names(sdem) & llaves %in% names(coe2)]

        base <- merge(sdem, coe2, by = llaves, all.x = TRUE)

      } else {
        base <- copy(sdem)
      }

      # ============================
      # VALIDACIÓN
      # ============================
      total_merge <- base[
        ocupado == 1,
        sum(factor, na.rm = TRUE)
      ]

      diff <- total_sdem - total_merge
      diff_rel <- abs(diff) / total_sdem

      resumen_validacion[[v]] <- data.table(
        anio = anio,
        trimestre = trim,
        sdem = total_sdem,
        merge = total_merge,
        diff = diff,
        diff_rel = diff_rel
      )
      v <- v + 1

      # ============================
      # VARIABLES ADICIONALES
      # ============================

      # INGRESO (solo INGOCUP)
      base[, ingreso := if ("INGOCUP" %in% names(base)) as.numeric(INGOCUP) else NA]

      # CUIDADOS
      base[, cuidados_h := if ("P11_H2" %in% names(base)) as.numeric(P11_H2) else NA]
      base[, cuidados_m := if ("P11_M2" %in% names(base)) as.numeric(P11_M2) else NA]
      base[, cuidados_total_horas := cuidados_h + (cuidados_m / 60)]

      # agregar tiempo
      base[, anio := anio]
      base[, trimestre := trim]

      # guardar mínimo necesario
      base <- base[, .(
        anio, trimestre,
        entidad,
        sexo,
        edad, ocupado,
        escolaridad,
        ingreso,
        cuidados_h, cuidados_m, cuidados_total_horas,
        factor
      )]

      lista[[k]] <- base
      k <- k + 1

      rm(sdem, base)
      gc()
    }
  }

  panel <- rbindlist(lista, fill = TRUE)
  validacion <- rbindlist(resumen_validacion)

  return(list(panel = panel, validacion = validacion))
}

# ============================================
# EJECUCIÓN
# ============================================
res <- procesar_enoe_serie("E:/ENOEr")

panel <- res$panel
validacion <- res$validacion

# ============================================
# VALIDACIÓN GLOBAL
# ============================================
print(validacion[order(anio, trimestre)])

# ============================================
# SERIE OCUPADOS
# ============================================
ocupados_ts <- panel[
  ocupado == 1,
  .(total = sum(factor, na.rm = TRUE)),
  by = .(anio, trimestre)
]

ocupados_ts[, fecha := as.Date(paste0(anio, "-", trimestre*3, "-01"))]




######################################## post 2020 ####

library(data.table)
library(haven)

procesar_enoe_post2020 <- function(base_path = "E:/ENOEr") {

  lista <- list()
  k <- 1

  for (anio in 2020:2025) {
    for (trim in 1:4) {

      if (anio == 2020 & trim == 2) next

      prefijo <- get_prefijo(anio, trim)
      if (is.na(prefijo)) next

      periodo <- get_periodo(anio, trim)
      carpeta <- file.path(base_path, anio, paste0(anio, "-", trim))

      cat("Procesando:", anio, "T", trim, "\n")

      sdem_file <- list.files(
        carpeta,
        pattern = paste0("(?i)", prefijo, "SDEMT", periodo, ".*\\.dta"),
        full.names = TRUE
      )

      coe2_file <- list.files(
        carpeta,
        pattern = paste0("(?i)", prefijo, "COE2T", periodo, ".*\\.dta"),
        full.names = TRUE
      )

      if (length(sdem_file) == 0) next

      # ============================
      # SDEM
      # ============================

      sdem <- as.data.table(read_dta(sdem_file))
      setnames(sdem, toupper(names(sdem)))

      # Edad robusta
      edad_var <- c("EDAD","EDA","EDAD_ANOS")[
        c("EDAD","EDA","EDAD_ANOS") %in% names(sdem)
      ][1]

      sdem[, edad := as.numeric(get(edad_var))]
      sdem[, sexo := as.numeric(SEX)]
      if ("ENT" %in% names(sdem)) {
        sdem[, entidad := as.numeric(ENT)]
      } else if ("CVE_ENT" %in% names(sdem)) {
        sdem[, entidad := as.numeric(CVE_ENT)]
      } else {
        sdem[, entidad := NA_real_]
      }
      sdem[, escolaridad := as.numeric(NIV_INS)]

      # ============================
      # UNIVERSO ENOE (CRÍTICO)
      # ============================

      sdem <- sdem[
        R_DEF == 00 &
        C_RES %in% c(1, 3) &
        edad >= 15 & edad <= 98
      ]

      # ============================
      # VARIABLE OCUPADO
      # ============================

      sdem[, ocupado := as.integer(CLASE2 == 1)]

      # ============================
      # FACTOR
      # ============================

      sdem[, factor := NA_real_]

      if ("FAC_TRI" %in% names(sdem)) {
        sdem[, factor := as.numeric(FAC_TRI)]
      }

      if ("FAC" %in% names(sdem) & all(is.na(sdem$factor))) {
        sdem[, factor := as.numeric(FAC)]
      }

      # Validación rápida
      if (mean(!is.na(sdem$factor)) < 0.9) {
        warning(paste("⚠️ Factor incompleto en", anio, "T", trim))
      }

      # ============================
      # COE2 (OPCIONAL)
      # ============================

      if (length(coe2_file) > 0) {

        coe2 <- as.data.table(read_dta(coe2_file))
        setnames(coe2, toupper(names(coe2)))

        # eliminar factores duplicados
        if ("FAC" %in% names(coe2)) coe2[, FAC := NULL]
        if ("FAC_TRI" %in% names(coe2)) coe2[, FAC_TRI := NULL]

        llaves <- c("CD_A","ENT","CON","V_SEL","N_HOG","H_MUD","N_REN")
        llaves <- llaves[llaves %in% names(sdem) & llaves %in% names(coe2)]

        # ⚠️ evitar duplicados
        coe2 <- unique(coe2, by = llaves)

        base <- merge(sdem, coe2, by = llaves, all.x = TRUE)

      } else {
        base <- copy(sdem)
      }

      # ============================
      # VARIABLES DERIVADAS
      # ============================

      base[, ingreso := if ("INGOCUP" %in% names(base)) as.numeric(INGOCUP) else NA]

      base[, cuidados_h := if ("P11_H2" %in% names(base)) as.numeric(P11_H2) else NA]
      base[, cuidados_m := if ("P11_M2" %in% names(base)) as.numeric(P11_M2) else NA]

      base[, cuidados_total_horas := cuidados_h + (cuidados_m / 60)]

      base[, anio := anio]
      base[, trimestre := trim]

      base <- base[, .(
        anio, trimestre,
        entidad,
        escolaridad,
        sexo,
        edad, ocupado,
        ingreso,
        cuidados_h, cuidados_m, cuidados_total_horas,
        factor
      )]

      lista[[k]] <- base
      k <- k + 1

      rm(sdem, base)
      gc()
    }
  }

  panel <- rbindlist(lista, fill = TRUE)
}


# ============================
# EJECUCIÓN
# ============================

panel_post2020 <- procesar_enoe_post2020("E:/ENOEr")

# ============================
# VALIDACIÓN OCUPADOS
# ============================

panel_post2020[
  , .(ocupados = sum(factor * ocupado, na.rm = TRUE)),
  by = .(anio, trimestre)
]


# ============================================
# UNIÓN CORRECTA DE PANELES
# ============================================

# ⚠️ evitar duplicación de 2020+
panel_pre2020 <- panel[anio < 2020]
panel_post <- panel_post2020

panel_total <- rbindlist(list(panel_pre2020, panel_post), fill = TRUE)

# ============================================
# TABLA TRIMESTRAL EXPANDIDA
# ============================================

tabla_ocupados <- panel_total[
  ocupado == 1,
  .(total_ocupados = sum(factor, na.rm = TRUE)),
  by = .(anio, trimestre)
][order(anio, trimestre)]

# Fecha trimestral
tabla_ocupados[, fecha := as.Date(paste0(anio, "-", trimestre*3, "-01"))]

print(tabla_ocupados)
###AQUI TODO COINCIDE CON INEGI

  
  
# ============================================
# TABLA ANUAL
# ============================================

tabla_anual <- tabla_ocupados[
  , .(ocupados_promedio = mean(total_ocupados)),
  by = anio
]

print(tabla_anual)

# ============================================
# GRÁFICO
# ============================================

ggplot(tabla_ocupados, aes(x = fecha, y = total_ocupados)) +
  
  geom_line(linewidth = 1.2) +
  
  scale_y_continuous(labels = comma) +
  
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  
  labs(
    title = "Población ocupada en México (ENOE 2005–2025)",
    subtitle = "Serie trimestral expandida",
    x = "",
    y = "Personas ocupadas"
  ) +
  
  theme_minimal(base_size = 14)

# ============================================
# EXPORTAR RESULTADOS
# ============================================

fwrite(tabla_ocupados, "E:/ENOEr/ocupados_trimestral.csv")
fwrite(tabla_anual, "E:/ENOEr/ocupados_anual.csv")
saveRDS(panel_total, "E:/ENOEr/panel_total.rds")


fwrite(panel_total, "E:/ENOEr/panel_total.csv")
write_dta(panel_total, "E:/ENOEr/panel_total.dta")


glimpse(panel_total)

