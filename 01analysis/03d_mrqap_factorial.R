############################################################
# 03D_MRQAP_FACTORIAL.R — diseño factorial 2x2 (Early bin/pond × FIN
# bin/pond) para el MRQAP, anexo de robustez (NO forma parte de
# 00_run_pipeline.R; no reemplaza ni modifica 03_mrqap.R ni 03c)
# Taller de Teatro y Creatividad Docente
#
# POR QUÉ ESTE SCRIPT (23-sep-2026):
#   03c_mrqap_ponderado.R comparó dos puntos: (a) todo binarizado (oficial,
#   Tabla 4) vs. (b) todo ponderado por orden de nominación (predictor
#   Early Y variable dependiente FIN). Ahí encontramos que el hallazgo
#   central (EarlyA -> I(T4), negativo) se sostiene en (b), pero que el
#   efecto "apenas positivo" de EarlyA -> A(T4) deja de ser significativo.
#   Diego preguntó si se puede separar POR QUÉ cambia -- ¿es la escala del
#   predictor Early la que importa, la escala de la variable dependiente
#   FIN, o ambas? Con solo los dos extremos (a) y (b) no se puede saber:
#   un cambio pudo venir de cualquiera de las dos, o de la combinación.
#
#   Este script corre las DOS celdas que faltan para completar el 2x2:
#     - Early PONDERADO / FIN BINARIA  (¿importa solo la escala del predictor?)
#     - Early BINARIA   / FIN PONDERADA (¿importa solo la escala de la DV?)
#   y las junta con las dos que ya existen (oficial = ambas binarias, cache
#   de 03_mrqap.R; ambas ponderadas, se re-corre acá mismo para que las 4
#   celdas salgan de la MISMA secuencia de permutaciones -- con un solo
#   set.seed() al inicio -- en vez de mezclar corridas de scripts distintos).
#
# ENTRADA:  01analysis/.cache/01_limpieza.rds, 02_redes.rds, 03_mrqap.rds
#           (para reciclar los ajustes oficiales ya corridos, sin repetir
#           el trabajo)
# SALIDA:   02outputs/Resultados_MRQAP_factorial.xlsx (las 4 celdas, para
#           CO y CR, lado a lado)
############################################################

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(sna)
  library(writexl)
})

RUTA_SALIDAS <- here::here("02outputs")
RUTA_CACHE   <- here::here("01analysis", ".cache")
REPS_QAP <- 5000

c01 <- readRDS(file.path(RUTA_CACHE, "01_limpieza.rds"))
c02 <- readRDS(file.path(RUTA_CACHE, "02_redes.rds"))
c03 <- readRDS(file.path(RUTA_CACHE, "03_mrqap.rds"))  # trae ajustes_CO/ajustes_CR oficiales, ya corridos
list2env(c01, envir = environment())
list2env(c02, envir = environment())

CO_FIN <- lista_matrices$CO_FIN
CR_FIN <- lista_matrices$CR_FIN

set.seed(2026)  # una sola semilla para las 4 celdas de este script (ver nota arriba)

############################################################
# 0. Covariables no-red -- IDÉNTICAS en las 4 celdas (copiadas tal cual de
# 03_mrqap.R -- no son redes de vínculo, no tienen versión "ponderada")
############################################################
datos_qap <- vertices_df %>%
  filter(name %in% nodos_core) %>%
  arrange(match(name, nodos_core))
stopifnot(identical(datos_qap$name, nodos_core))

imputar_media <- function(x) { x[is.na(x)] <- mean(x, na.rm = TRUE); x }
datos_qap <- datos_qap %>%
  mutate(CSE1 = imputar_media(CSE1), G1 = imputar_media(G1), FI1 = imputar_media(FI1))

mat_mismo_genero <- { m <- outer(datos_qap$genero, datos_qap$genero, "=="); diag(m) <- 0; m * 1 }
mat_ambos_leng   <- { m <- outer(datos_qap$Type == "Leng", datos_qap$Type == "Leng", "&"); diag(m) <- 0; m * 1 }
mat_dist_cse     <- as.matrix(dist(datos_qap$CSE1))
mat_dist_growth  <- as.matrix(dist(datos_qap$G1))
mat_dist_fixed   <- as.matrix(dist(datos_qap$FI1))

crear_matriz_target <- function(v, n) { m <- matrix(rep(v, each = n), nrow = n, byrow = TRUE); diag(m) <- 0; m }
crear_matriz_ego    <- function(v, n) { m <- matrix(rep(v, times = n), nrow = n, byrow = FALSE); diag(m) <- 0; m }
n_core <- length(nodos_core)
mat_target_cse    <- crear_matriz_target(datos_qap$CSE1, n_core)
mat_target_growth <- crear_matriz_target(datos_qap$G1,   n_core)
mat_target_fixed  <- crear_matriz_target(datos_qap$FI1,  n_core)

sin_historial_vec <- asistencia$Sin_Historial_Temprano[match(nodos_core, asistencia$name)]
mat_ego_sin_historial <- crear_matriz_ego(sin_historial_vec, n_core)

bloques_comunes <- list(
  Demografico = list(Same_Gender = mat_mismo_genero,
                     Ambos_Leng = mat_ambos_leng,
                     Ego_Sin_Historial_Temprano = mat_ego_sin_historial),
  Homofilia   = list(Dist_CSE = mat_dist_cse, Dist_Growth = mat_dist_growth, Dist_Fixed = mat_dist_fixed),
  Atractivo   = list(Target_CSE = mat_target_cse, Target_Growth = mat_target_growth, Target_Fixed = mat_target_fixed)
)

correr_celda <- function(y, early_co, early_cr, reps = REPS_QAP) {
  specs <- c(list(Estructural = list(Early_CO = early_co, Early_CR = early_cr)), bloques_comunes)
  correr <- function(incluir) {
    x <- purrr::flatten(specs[incluir])
    list(modelo = netlm(y = y, x = x, reps = reps), terminos = names(x))
  }
  list(
    Estructural             = correr("Estructural"),
    Estructural_Demografico = correr(c("Estructural", "Demografico")),
    Completo                = correr(c("Estructural", "Demografico", "Homofilia", "Atractivo"))
  )
}

# CHECKPOINTING (agregado tras un primer intento que se quedó sin tiempo a
# mitad de camino, 5 de 6 celdas ya corridas): cada celda se guarda en su
# propio .rds apenas termina, y si ya existe de una corrida anterior se
# carga en vez de re-correrla -- así una segunda invocación de este script
# retoma donde quedó, no repite trabajo ya hecho (cada netlm con 5000 reps
# tarda ~30-40s, y con 18 modelos en total el script entero no siempre cabe
# en un solo timeout de herramienta).
RUTA_CHECKPOINT <- file.path(RUTA_CACHE, "03d_checkpoints")
dir.create(RUTA_CHECKPOINT, showWarnings = FALSE)
correr_celda_checkpoint <- function(nombre, y, early_co, early_cr) {
  archivo <- file.path(RUTA_CHECKPOINT, paste0(nombre, ".rds"))
  if (file.exists(archivo)) {
    cat("(ya calculado, cargando checkpoint)", nombre, "\n")
    return(readRDS(archivo))
  }
  cat("Calculando", nombre, "...\n")
  res <- correr_celda(y, early_co, early_cr)
  saveRDS(res, archivo)
  res
}

############################################################
# 1. LAS 4 CELDAS (para CO y para CR)
############################################################
ajustes_CO_EarlyPond_FINBin  <- correr_celda_checkpoint("CO_EarlyPond_FINBin",  CO_FIN,        CO_early_fuerza, CR_early_fuerza)
ajustes_CR_EarlyPond_FINBin  <- correr_celda_checkpoint("CR_EarlyPond_FINBin",  CR_FIN,        CO_early_fuerza, CR_early_fuerza)
ajustes_CO_EarlyBin_FINPond  <- correr_celda_checkpoint("CO_EarlyBin_FINPond",  CO_FIN_fuerza, CO_early,        CR_early)
ajustes_CR_EarlyBin_FINPond  <- correr_celda_checkpoint("CR_EarlyBin_FINPond",  CR_FIN_fuerza, CO_early,        CR_early)
ajustes_CO_EarlyPond_FINPond <- correr_celda_checkpoint("CO_EarlyPond_FINPond", CO_FIN_fuerza, CO_early_fuerza, CR_early_fuerza)
ajustes_CR_EarlyPond_FINPond <- correr_celda_checkpoint("CR_EarlyPond_FINPond", CR_FIN_fuerza, CO_early_fuerza, CR_early_fuerza)

# Celda 1/4 (Early binaria / FIN binaria) = la oficial, YA corrida en
# 03_mrqap.R -- se recicla desde el cache, no se repite el trabajo.
ajustes_CO_EarlyBin_FINBin <- c03$ajustes_CO
ajustes_CR_EarlyBin_FINBin <- c03$ajustes_CR

############################################################
# 2. FORMATEAR Y ARMAR TABLA COMPARATIVA (4 celdas x 3 modelos anidados)
############################################################
extraer_r2 <- function(s) {
  candidatos <- c("r.squared", "r2", "mult.r2", "R2")
  campo <- intersect(candidatos, names(s))
  if (length(campo) > 0) return(as.numeric(s[[campo[1]]]))
  txt <- capture.output(print(s))
  linea <- grep("Multiple R-squared", txt, value = TRUE)
  if (length(linea) == 1) return(as.numeric(regmatches(linea, regexpr("[0-9]+\\.[0-9]+", linea))[1]))
  NA_real_
}
extraer_mrqap <- function(ajuste, nombre_col) {
  # BUG CORREGIDO (30-sep-2026) -- ver nota en 03_mrqap.R::extraer_mrqap.
  # p_vals <- m[, ncol(m)] devolvía la estimación otra vez, no un p-valor.
  s <- summary(ajuste$modelo)
  estimates <- as.numeric(ajuste$modelo$coefficients)
  p_vals <- as.numeric(s$pgreqabs)
  stars <- case_when(p_vals < 0.001 ~ "***", p_vals < 0.01 ~ "**", p_vals < 0.05 ~ "*", p_vals < 0.1 ~ ".", TRUE ~ "")
  res <- tibble(Term = c("(Intercept)", ajuste$terminos), Value = paste0(round(estimates, 3), stars))
  res <- bind_rows(res, tibble(Term = "R-squared", Value = as.character(round(extraer_r2(s), 3))))
  colnames(res)[2] <- nombre_col
  res
}
formatear_celda <- function(ajustes, prefijo) {
  reduce(imap(ajustes, ~ extraer_mrqap(.x, paste0(prefijo, "_", .y))), ~ full_join(.x, .y, by = "Term"))
}

tabla_CO <- reduce(list(
  formatear_celda(ajustes_CO_EarlyBin_FINBin,   "1_Oficial_EarlyBin_FINBin"),
  formatear_celda(ajustes_CO_EarlyPond_FINBin,  "2_EarlyPond_FINBin"),
  formatear_celda(ajustes_CO_EarlyBin_FINPond,  "3_EarlyBin_FINPond"),
  formatear_celda(ajustes_CO_EarlyPond_FINPond, "4_AmbasPond")
), full_join, by = "Term")

tabla_CR <- reduce(list(
  formatear_celda(ajustes_CR_EarlyBin_FINBin,   "1_Oficial_EarlyBin_FINBin"),
  formatear_celda(ajustes_CR_EarlyPond_FINBin,  "2_EarlyPond_FINBin"),
  formatear_celda(ajustes_CR_EarlyBin_FINPond,  "3_EarlyBin_FINPond"),
  formatear_celda(ajustes_CR_EarlyPond_FINPond, "4_AmbasPond")
), full_join, by = "Term")

# Solo las filas Early_CO/Early_CR/R-squared del modelo Completo, para
# lectura rápida del patrón 2x2 (la tabla completa con las 13 filas x 12
# columnas queda en las hojas MRQAP_CO/MRQAP_CR para quien quiera el
# detalle completo).
resumen_early <- bind_rows(
  tabla_CO %>% filter(Term %in% c("Early_CO", "Early_CR", "R-squared")) %>% mutate(Red = "CO (afinidad, A(T4))"),
  tabla_CR %>% filter(Term %in% c("Early_CO", "Early_CR", "R-squared")) %>% mutate(Red = "CR (influencia creativa, I(T4))")
) %>% select(Red, everything())

write_xlsx(
  list(
    Resumen_2x2_Completo = resumen_early,
    MRQAP_CO = tabla_CO,
    MRQAP_CR = tabla_CR,
    Leyenda = tibble(columna = c(
      "1_Oficial_EarlyBin_FINBin", "2_EarlyPond_FINBin", "3_EarlyBin_FINPond", "4_AmbasPond"),
      significado = c(
        "Oficial (Tabla 4 del artículo): Early_CO/Early_CR = CO_early/CR_early (fracción de olas T1-T3 con vínculo, binario por ola); DV = CO_FIN/CR_FIN (binaria, presencia/ausencia).",
        "Solo el predictor Early ponderado por orden de nominación (CO_early_fuerza/CR_early_fuerza); DV sigue binaria (CO_FIN/CR_FIN).",
        "Predictor Early sigue binario (CO_early/CR_early); solo la DV ponderada por orden de nominación (CO_FIN_fuerza/CR_FIN_fuerza).",
        "Ambas ponderadas por orden de nominación (= la comparación de 03c_mrqap_ponderado.R, re-corrida acá con la misma semilla que las otras 3 celdas para que las 4 salgan de una sola secuencia de permutaciones)."
      ))
  ),
  path = file.path(RUTA_SALIDAS, "Resultados_MRQAP_factorial.xlsx")
)

message("03d_mrqap_factorial.R completo. Revisa 02outputs/Resultados_MRQAP_factorial.xlsx (hoja Resumen_2x2_Completo para el patrón rápido).")
