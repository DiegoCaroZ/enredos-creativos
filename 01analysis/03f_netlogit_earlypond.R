############################################################
# 03f_NETLOGIT_EARLYPOND.R — Anexo de robustez (NO forma parte de 00_run_pipeline.R)
# Taller de Teatro y Creatividad Docente
#
# POR QUÉ EXISTE:
#   Completa, para el modelo de "chance de vínculo" (netlogit, ahora Tabla 4
#   oficial del artículo -- ver 03e_mrqap_netlogit.R), el mismo chequeo de
#   robustez de escala del predictor Early que 03d_mrqap_factorial.R ya hacía
#   para el modelo netlm. Corre CO_FIN/CR_FIN (binarias) ~ Early_fuerza
#   (ponderado por orden de nominación) vía netlogit, con las 3
#   especificaciones anidadas de siempre.
#
#   Junto con la celda "CO/CR_EarlyPond_FINPond" de 03d_mrqap_factorial.R
#   (netlm, ya calculada -- es la fuente de la Tabla 5 con Early ponderado),
#   esto le da al párrafo de robustez de Limitaciones el panorama completo:
#   ¿el hallazgo depende de si Early se mide con el promedio de co-presencia
#   corregido por fronteras (oficial) o con la versión ponderada por orden de
#   nominación, en CUALQUIERA de los dos modelos promovidos a Resultados
#   (Tabla 4 = netlogit, Tabla 5 = netlm ponderado)?
#
# ENTRADA:  01analysis/.cache/01_limpieza.rds
#           01analysis/.cache/02_redes.rds
# SALIDA:   01analysis/.cache/03f_netlogit_earlypond.rds
#
# TIEMPO DE EJECUCIÓN: 6 modelos netlogit (CO/CR x 3 especificaciones),
# reps=5000 -- similar a 03e_mrqap_netlogit.R (~9-10 min). Puede acercarse
# al límite de una sola corrida de herramienta de shell.
############################################################

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(sna)
})

RUTA_CACHE <- here::here("01analysis", ".cache")
REPS_QAP <- 5000

c01 <- readRDS(file.path(RUTA_CACHE, "01_limpieza.rds"))
c02 <- readRDS(file.path(RUTA_CACHE, "02_redes.rds"))
list2env(c01, envir = environment())
list2env(c02, envir = environment())

CO_FIN <- lista_matrices$CO_FIN
CR_FIN <- lista_matrices$CR_FIN

set.seed(2026)

# --- Reconstrucción idéntica de las matrices de covariables de 03_mrqap.R ---
datos_qap <- vertices_df %>%
  filter(name %in% nodos_core) %>%
  arrange(match(name, nodos_core))
imputar_media <- function(x) { x[is.na(x)] <- mean(x, na.rm = TRUE); x }
datos_qap <- datos_qap %>%
  mutate(CSE1 = imputar_media(CSE1), G1 = imputar_media(G1), FI1 = imputar_media(FI1))

mat_mismo_genero <- { m <- outer(datos_qap$genero, datos_qap$genero, "=="); diag(m) <- 0; m * 1 }
mat_ambos_leng <- { m <- outer(datos_qap$Type == "Leng", datos_qap$Type == "Leng", "&"); diag(m) <- 0; m * 1 }
mat_dist_cse <- as.matrix(dist(datos_qap$CSE1))
mat_dist_growth <- as.matrix(dist(datos_qap$G1))
mat_dist_fixed <- as.matrix(dist(datos_qap$FI1))

crear_matriz_target <- function(v, n) { m <- matrix(rep(v, each = n), nrow = n, byrow = TRUE); diag(m) <- 0; m }
n_core <- length(nodos_core)
mat_target_cse    <- crear_matriz_target(datos_qap$CSE1, n_core)
mat_target_growth <- crear_matriz_target(datos_qap$G1,   n_core)
mat_target_fixed  <- crear_matriz_target(datos_qap$FI1,  n_core)

sin_historial_vec <- asistencia$Sin_Historial_Temprano[match(nodos_core, asistencia$name)]
crear_matriz_ego <- function(v, n) { m <- matrix(rep(v, times = n), nrow = n, byrow = FALSE); diag(m) <- 0; m }
mat_ego_sin_historial <- crear_matriz_ego(sin_historial_vec, n_core)

bloques_comunes <- list(
  Demografico = list(Same_Gender = mat_mismo_genero, Ambos_Leng = mat_ambos_leng,
                     Ego_Sin_Historial_Temprano = mat_ego_sin_historial),
  Homofilia   = list(Dist_CSE = mat_dist_cse, Dist_Growth = mat_dist_growth, Dist_Fixed = mat_dist_fixed),
  Atractivo   = list(Target_CSE = mat_target_cse, Target_Growth = mat_target_growth, Target_Fixed = mat_target_fixed)
)

correr_logit_pond <- function(y, reps = REPS_QAP) {
  specs <- c(list(Estructural = list(Early_CO = CO_early_fuerza, Early_CR = CR_early_fuerza)), bloques_comunes)
  correr <- function(incluir) {
    x <- purrr::flatten(specs[incluir])
    list(modelo = netlogit(y = y, x = x, reps = reps), terminos = names(x))
  }
  list(
    Estructural              = correr("Estructural"),
    Estructural_Demografico  = correr(c("Estructural", "Demografico")),
    Completo                 = correr(c("Estructural", "Demografico", "Homofilia", "Atractivo"))
  )
}

cat("Corriendo CO_FIN ~ Early_fuerza (netlogit)...\n")
ajustes_CO <- correr_logit_pond(CO_FIN)
cat("Corriendo CR_FIN ~ Early_fuerza (netlogit)...\n")
ajustes_CR <- correr_logit_pond(CR_FIN)

saveRDS(list(ajustes_CO = ajustes_CO, ajustes_CR = ajustes_CR),
        file.path(RUTA_CACHE, "03f_netlogit_earlypond.rds"))

for (nm in names(ajustes_CO)) { cat("\n=== CO (logit, Early ponderado) ~", nm, "===\n"); print(summary(ajustes_CO[[nm]]$modelo)) }
for (nm in names(ajustes_CR)) { cat("\n=== CR (logit, Early ponderado) ~", nm, "===\n"); print(summary(ajustes_CR[[nm]]$modelo)) }

message("03f_netlogit_earlypond.R completo. Cache en .cache/03f_netlogit_earlypond.rds")
