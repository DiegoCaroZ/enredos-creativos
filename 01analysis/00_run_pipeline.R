############################################################
# 00_RUN_PIPELINE.R — corre el pipeline completo, en orden
#
# Uso: abre este archivo en RStudio con el .Rproj de TheatreWorkshop
# abierto (para que here::here() encuentre la raíz del proyecto) y
# ejecútalo completo, o desde la terminal:
#   Rscript 01analysis/00_run_pipeline.R
#
# Cada etapa relee su propio cache en 01analysis/.cache/, así que
# también puedes correr solo un archivo (p.ej. 03_mrqap.R) sin repetir
# las etapas anteriores, mientras no hayan cambiado sus salidas.
#
# TIEMPO ESTIMADO TOTAL: ~6 minutos (dominado por 03_mrqap.R, ~5 min
# con REPS_QAP = 5000; el resto de las etapas toma segundos).
############################################################

source(here::here("01analysis", "01_limpieza.R"))
source(here::here("01analysis", "02_redes.R"))
source(here::here("01analysis", "03_mrqap.R"))
source(here::here("01analysis", "04_visualizacion.R"))
source(here::here("01analysis", "05_visualizacion_alternativas.R"))

message("\n=== PIPELINE COMPLETO ===")
message("Revisa 02outputs/ para todas las tablas y figuras generadas.")
