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

library(rsconnect)
rsconnect::deployApp()
