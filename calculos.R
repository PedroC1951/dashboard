# ==============================================================================
# PROYECTO: Monitor Laboral y de Cuidados (Basado en ENOE)
# DESCRIPCIÓN: Dashboard interactivo para el análisis de indicadores de ocupación,
#              brechas de género y horas de cuidado no remunerado en México.
# AUTORES: Pedro Castro Ortega, Dafne Díaz Lira, Louisevie Renois, Saúl Adonis Noguez Olvera
# FECHA DE ACTUALIZACIÓN: 2026
# ==============================================================================
#
# METADATOS DE LA FUENTE:
# -----------------------
# FUENTE: Encuesta Nacional de Ocupación y Empleo (ENOE), INEGI.
# PERIODICIDAD: Trimestral (Series desde 2005-1T hasta 2025-4T).
# COBERTURA: Nacional y Entidad Federativa (32 estados).
# VARIABLES CLAVE: 
#   - sexo (1: Hombre, 2: Mujer)
#   - ocupado (1: Ocupado, 0: No ocupado)
#   - cuidados_total_horas: Sumatoria de horas y fracciones de minutos (h + m/60).
#   - factor: Ponderador poblacional (Factor de expansión).
#
# ESTRUCTURA DEL PROYECTO:
# ------------------------
# REPOSITORIO: https://github.com/PedroC1951/dashboard
# DESPLIEGUE: https://pedrocastroortega.shinyapps.io/dashboard/
# ARCHIVOS REQUERIDOS:
#   - data/bd_resumen.parquet : Datos agregados y expandidos.
#   - data/mapa_ready.rds     : Cartografía simplificada (sf) en WGS84.
#
# LÓGICA DE PROCESAMIENTO:
# 1. Los datos originales (.dta) se agrupan por jerarquía Año/Año-Trimestre.
# 2. Se filtran casos de no respuesta en edad (97-99).
# 3. La simplificación cartográfica se mantiene al 5% para optimizar carga.
# 4. El despliegue se gestiona vía rsconnect::deployApp().
# ==============================================================================
library(dplyr)
library(sf)
library(ggplot2)
library(officer)
library(tidyverse)
library(rmapshaper)
library(arrow) 

base<-readr::read_rds("~/Downloads/Base para dashboard/panel_total.rds")|>
  mutate(CVEGEO = sprintf("%02d",entidad))

bd <- base %>%
  mutate(cuidados_total_horas = cuidados_h + (cuidados_m / 60),
grupo_edad = case_when(
    edad %in% c(97, 98, 99) ~ "No sabe / No contesta",
    edad >= 15 & edad <= 29 ~ "15 a 29 años",
    edad >= 30 & edad <= 44 ~ "30 a 44 años",
    edad >= 45 & edad <= 59 ~ "45 a 59 años",
    edad >= 60 & edad <= 96 ~ "60 años o más",    
    TRUE ~ "Otros" 
  ))



# 1. Cargar y simplificar mapa
mapa <- st_read("~/Downloads/dest22gw_c/dest22cw.shp") %>%
  ms_simplify(keep = 0.05) %>% 
  st_transform(4326) 

# 2. Resumir la base gigante (usando factor de expansión)
bd_resumen <- bd %>%
  group_by(anio, entidad, CVEGEO, sexo, grupo_edad) %>%
  summarise(
    peso_total_pob   = sum(factor, na.rm = TRUE),
    ocupados_total   = sum(factor[ocupado == 1], na.rm = TRUE),
    cuidados_total   = sum(cuidados_total_horas * factor, na.rm = TRUE),
    peso_cuidadores  = sum(factor[cuidados_total_horas > 0], na.rm = TRUE),
    
    cuidados_ocu     = sum(cuidados_total_horas[ocupado == 1] * factor[ocupado == 1], na.rm = TRUE),
    peso_cuid_ocu    = sum(factor[cuidados_total_horas > 0 & ocupado == 1], na.rm = TRUE),
    
    cuidados_no_ocu  = sum(cuidados_total_horas[ocupado == 0] * factor[ocupado == 0], na.rm = TRUE),
    peso_cuid_no_ocu = sum(factor[cuidados_total_horas > 0 & ocupado == 0], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(sexo_lab = ifelse(sexo == 1, "Hombre", "Mujer"))


bd_resumen <- bd_resumen %>%
  left_join(st_drop_geometry(mapa), by = "CVEGEO")

write_parquet(bd_resumen, "data/bd_resumen.parquet")
