############################################################
# 03C_MRQAP_PONDERADO.R — QAP/MRQAP con redes PONDERADAS (orden de
# nominación) en vez de binarizadas, anexo de robustez (NO forma parte
# de 00_run_pipeline.R; no reemplaza ni modifica 03_mrqap.R)
# Taller de Teatro y Creatividad Docente
#
# POR QUÉ ESTE SCRIPT (23-sep-2026):
#   Diego preguntó: si Figura 1 y la sección "Actores clave en el taller"
#   ya usan la fuerza ponderada por orden de nominación (`peso_orden` /
#   `strength_in` con `w_force`, ver 02_redes.R y la Figura 1 nueva), ¿por
#   qué las tablas de QAP (Tabla 3) y MRQAP (Tabla 4) siguen corriendo
#   sobre las redes BINARIZADAS (presencia/ausencia, sin importar el
#   orden)? Es una pregunta legítima -- el Método del artículo dice
#   explícitamente "para los análisis QAP y MRQAP se utilizó su versión
#   binarizada", pero esa es una elección de diseño, no un requisito
#   técnico: sna::qaptest(FUN=gcor) y sna::netlm() aceptan matrices
#   valoradas (no binarias) sin ningún cambio de código.
#
# QUÉ HACE ESTE SCRIPT:
#   Repite EXACTAMENTE la Tabla 3 (correlación QAP entre las 8 redes) y la
#   Tabla 4 (MRQAP anidado para CO_FIN/CR_FIN) de 03_mrqap.R, pero
#   reemplazando las 8 redes binarias por sus versiones ponderadas por
#   orden de nominación (fuerza_desde_rango: 1ª mención=1, 2ª=0.67, 3ª o
#   más=0.33 -- la MISMA transformación que ya usa Figura 1 y "Actores
#   clave"). El predictor Early ponderado (CO_early_fuerza/CR_early_fuerza)
#   y las DV FIN ponderadas (CO_FIN_fuerza/CR_FIN_fuerza) YA estaban
#   calculados en 02_redes.R (se agregaron para peso_orden/Figura 1, pero
#   se documentó ahí mismo que no entraban a ningún modelo -- este script
#   es el que finalmente los usa como tal). Las covariables NO-red
#   (demográficas, distancia en creencias, atractivo del receptor) NO
#   cambian -- no son redes de vínculo, no tienen versión "binaria" que
#   ponderar.
#
# OJO -- esto es una comparación EXPLORATORIA, no un reemplazo del
# análisis oficial: cambia la pregunta de "¿qué predice el RECONOCIMIENTO
# de un vínculo?" (binario) a "¿qué predice la INTENSIDAD de un vínculo,
# dado que existe?" (ponderado) -- son preguntas relacionadas pero no
# idénticas, y la escala FIN ponderada solo tiene 4 valores posibles
# (0/0.33/0.67/1), más cerca de ordinal que de continua. Revisa el
# resultado con Diego antes de promover esto a análisis principal.
#
# ENTRADA:  01analysis/.cache/01_limpieza.rds, 02_redes.rds, 03_mrqap.rds
# SALIDA:   02outputs/Resultados_MRQAP_ponderado.xlsx (Tabla 4 ponderada +
#           comparación lado a lado con la oficial binarizada)
#           02outputs/Resultados_Correlaciones_QAP_ponderado.xlsx
############################################################

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(sna)
  library(igraph)
  library(writexl)
})

RUTA_SALIDAS <- here::here("02outputs")
RUTA_CACHE   <- here::here("01analysis", ".cache")

REPS_QAP <- 5000

c01 <- readRDS(file.path(RUTA_CACHE, "01_limpieza.rds"))
c02 <- readRDS(file.path(RUTA_CACHE, "02_redes.rds"))
c03 <- readRDS(file.path(RUTA_CACHE, "03_mrqap.rds"))
list2env(c01, envir = environment())
list2env(c02, envir = environment())

set.seed(2026)

############################################################
# 0. LAS 8 REDES PONDERADAS (mismo criterio que Figura 1 / Actores clave)
############################################################
fuerza_desde_rango <- function(m) {
  out <- matrix(0, nrow(m), ncol(m), dimnames = dimnames(m))
  out[m > 0] <- (4 - pmin(m[m > 0], 3)) / 3
  out
}
get_adj_rango <- function(g, nodes) {
  M <- as.matrix(as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  M[is.na(M)] <- 0
  M <- M[nodes, nodes, drop = FALSE]
  diag(M) <- 0
  M
}

CO_T1_f <- fuerza_desde_rango(get_adj_rango(grafos$Wg1, nodos_core))
CO_T2_f <- fuerza_desde_rango(get_adj_rango(grafos$Wg2, nodos_core))
CO_T3_f <- fuerza_desde_rango(get_adj_rango(grafos$Wg3, nodos_core))
CR_T1_f <- fuerza_desde_rango(get_adj_rango(grafos$Wg5, nodos_core))
CR_T2_f <- fuerza_desde_rango(get_adj_rango(grafos$Wg6, nodos_core))
CR_T3_f <- fuerza_desde_rango(get_adj_rango(grafos$Wg7, nodos_core))
# CO_FIN_fuerza / CR_FIN_fuerza ya vienen del cache de 02_redes.R (idéntica
# transformación, ver comentario ahí).

lista_matrices_pond <- list(
  CO_T1 = CO_T1_f, CO_T2 = CO_T2_f, CO_T3 = CO_T3_f, CO_FIN = CO_FIN_fuerza,
  CR_T1 = CR_T1_f, CR_T2 = CR_T2_f, CR_T3 = CR_T3_f, CR_FIN = CR_FIN_fuerza
)

# Chequeo de soporte: el patrón de ceros/no-ceros debe ser IDÉNTICO al de
# las redes binarias oficiales (fuerza_desde_rango no agrega ni quita
# vínculos, solo cambia cuánto pesa cada uno -- ver nota en 02_redes.R).
lista_matrices_bin <- lista_matrices  # viene de 02_redes.rds
for (nm in names(lista_matrices_pond)) {
  soporte_igual <- identical((lista_matrices_pond[[nm]] > 0) * 1, lista_matrices_bin[[nm]])
  if (!soporte_igual) warning(sprintf("Soporte distinto entre binaria y ponderada en %s -- revisar.", nm))
}
message("Chequeo de soporte binaria vs. ponderada: OK si no aparece ningún warning arriba.")


############################################################
# 1. TABLA 3 PONDERADA — CORRELACIÓN QAP ENTRE LAS 8 REDES
############################################################
n_redes  <- length(lista_matrices_pond)
nombres_r <- names(lista_matrices_pond)
mat_r <- matrix(NA, n_redes, n_redes, dimnames = list(nombres_r, nombres_r))
mat_p <- matrix(NA, n_redes, n_redes, dimnames = list(nombres_r, nombres_r))

for (i in 1:n_redes) {
  for (j in i:n_redes) {
    if (i == j) {
      mat_r[i, j] <- 1; mat_p[i, j] <- 0
    } else {
      cat("QAP (ponderado):", nombres_r[i], "vs", nombres_r[j], "...\n")
      qap_out <- sna::qaptest(list(lista_matrices_pond[[i]], lista_matrices_pond[[j]]),
                              FUN = gcor, g1 = 1, g2 = 2, reps = REPS_QAP)
      r <- qap_out$testval
      p <- mean(abs(qap_out$dist) >= abs(r))
      mat_r[i, j] <- mat_r[j, i] <- r
      mat_p[i, j] <- mat_p[j, i] <- p
    }
  }
}

tabla_qap_pond <- as.data.frame(mat_r) %>%
  mutate(Red = rownames(.)) %>%
  pivot_longer(-Red, names_to = "Red_Col", values_to = "r") %>%
  mutate(
    p = as.vector(t(mat_p)),
    signif = case_when(p <= 0.001 ~ "***", p <= 0.01 ~ "**", p <= 0.05 ~ "*", p <= 0.1 ~ ".", TRUE ~ ""),
    valor = paste0(format(round(r, 3), nsmall = 3), signif)
  ) %>%
  select(Red, Red_Col, valor) %>%
  pivot_wider(names_from = Red_Col, values_from = valor)

write_xlsx(tabla_qap_pond, path = file.path(RUTA_SALIDAS, "Resultados_Correlaciones_QAP_ponderado.xlsx"))


############################################################
# 2. TABLA 4 PONDERADA — MRQAP (netlm) para CO_FIN/CR_FIN
############################################################
# Las covariables NO-red (demográficas, distancia en creencias, atractivo)
# se reciclan TAL CUAL del script oficial -- no son redes de vínculo.
datos_qap <- vertices_df %>%
  filter(name %in% nodos_core) %>%
  arrange(match(name, nodos_core))
stopifnot(identical(datos_qap$name, nodos_core))

imputar_media <- function(x) { x[is.na(x)] <- mean(x, na.rm = TRUE); x }
datos_qap <- datos_qap %>%
  mutate(CSE1 = imputar_media(CSE1), G1 = imputar_media(G1), FI1 = imputar_media(FI1))

mat_mismo_genero <- { m <- outer(datos_qap$genero, datos_qap$genero, "=="); diag(m) <- 0; m * 1 }
mat_ambos_leng <- { m <- outer(datos_qap$Type == "Leng", datos_qap$Type == "Leng", "&"); diag(m) <- 0; m * 1 }
mat_dist_cse     <- as.matrix(dist(datos_qap$CSE1))
mat_dist_growth  <- as.matrix(dist(datos_qap$G1))
mat_dist_fixed   <- as.matrix(dist(datos_qap$FI1))

crear_matriz_target <- function(vector_atributo, n_nodos) {
  m <- matrix(rep(vector_atributo, each = n_nodos), nrow = n_nodos, byrow = TRUE)
  diag(m) <- 0
  m
}
crear_matriz_ego <- function(vector_atributo, n_nodos) {
  m <- matrix(rep(vector_atributo, times = n_nodos), nrow = n_nodos, byrow = FALSE)
  diag(m) <- 0
  m
}
n_core <- length(nodos_core)
mat_target_cse    <- crear_matriz_target(datos_qap$CSE1, n_core)
mat_target_growth <- crear_matriz_target(datos_qap$G1,   n_core)
mat_target_fixed  <- crear_matriz_target(datos_qap$FI1,  n_core)

sin_historial_vec <- asistencia$Sin_Historial_Temprano[match(nodos_core, asistencia$name)]
mat_ego_sin_historial <- crear_matriz_ego(sin_historial_vec, n_core)

# ÚNICO cambio real respecto a 03_mrqap.R: Early_CO/Early_CR ponderados
# (CO_early_fuerza/CR_early_fuerza, ya en cache) en vez de CO_early/CR_early.
specs_pond <- list(
  Estructural = list(Early_CO = CO_early_fuerza, Early_CR = CR_early_fuerza),
  Demografico = list(Same_Gender = mat_mismo_genero,
                     Ambos_Leng = mat_ambos_leng,
                     Ego_Sin_Historial_Temprano = mat_ego_sin_historial),
  Homofilia   = list(Dist_CSE = mat_dist_cse, Dist_Growth = mat_dist_growth, Dist_Fixed = mat_dist_fixed),
  Atractivo   = list(Target_CSE = mat_target_cse, Target_Growth = mat_target_growth, Target_Fixed = mat_target_fixed)
)

correr_mrqap_pond <- function(y, incluir, reps = REPS_QAP) {
  x <- purrr::flatten(specs_pond[incluir])
  list(modelo = netlm(y = y, x = x, reps = reps), terminos = names(x))
}

# DV también ponderada: CO_FIN_fuerza / CR_FIN_fuerza (no CO_FIN/CR_FIN)
ajustes_CO_pond <- list(
  Estructural              = correr_mrqap_pond(CO_FIN_fuerza, c("Estructural")),
  Estructural_Demografico  = correr_mrqap_pond(CO_FIN_fuerza, c("Estructural", "Demografico")),
  Completo                 = correr_mrqap_pond(CO_FIN_fuerza, c("Estructural", "Demografico", "Homofilia", "Atractivo"))
)
ajustes_CR_pond <- list(
  Estructural              = correr_mrqap_pond(CR_FIN_fuerza, c("Estructural")),
  Estructural_Demografico  = correr_mrqap_pond(CR_FIN_fuerza, c("Estructural", "Demografico")),
  Completo                 = correr_mrqap_pond(CR_FIN_fuerza, c("Estructural", "Demografico", "Homofilia", "Atractivo"))
)

modelos_CO_pond <- map(ajustes_CO_pond, "modelo")
modelos_CR_pond <- map(ajustes_CR_pond, "modelo")

for (nm in names(modelos_CO_pond)) { cat("\n=== CO (ponderado) ~", nm, "===\n"); print(summary(modelos_CO_pond[[nm]])) }
for (nm in names(modelos_CR_pond)) { cat("\n=== CR (ponderado) ~", nm, "===\n"); print(summary(modelos_CR_pond[[nm]])) }

############################################################
# 3. FORMATEAR Y COMPARAR CONTRA LA TABLA 4 OFICIAL (binarizada)
############################################################
extraer_r2 <- function(s) {
  candidatos <- c("r.squared", "r2", "mult.r2", "R2")
  campo <- intersect(candidatos, names(s))
  if (length(campo) > 0) return(as.numeric(s[[campo[1]]]))
  txt <- capture.output(print(s))
  linea <- grep("Multiple R-squared", txt, value = TRUE)
  if (length(linea) == 1) {
    val <- regmatches(linea, regexpr("[0-9]+\\.[0-9]+", linea))
    return(as.numeric(val[1]))
  }
  NA_real_
}

# MISMA función extraer_mrqap() de 03_mrqap.R, tal cual, para que el
# formato de columnas/estrellas de significancia sea idéntico entre la
# tabla oficial y esta -- no reinventar una versión ligeramente distinta.
extraer_mrqap <- function(ajuste, nombre_col) {
  modelo <- ajuste$modelo
  s <- summary(modelo)
  m <- as.matrix(s$coefficients)
  estimates <- m[, 1]
  p_vals    <- m[, ncol(m)]
  stars <- case_when(p_vals < 0.001 ~ "***", p_vals < 0.01 ~ "**", p_vals < 0.05 ~ "*", p_vals < 0.1 ~ ".", TRUE ~ "")
  nombres <- c("(Intercept)", ajuste$terminos)
  res <- tibble(Term = nombres, Value = paste0(round(estimates, 3), stars))
  r2 <- extraer_r2(s)
  res <- bind_rows(res, tibble(Term = "R-squared", Value = as.character(round(r2, 3))))
  colnames(res)[2] <- nombre_col
  res
}

formatear_tabla <- function(ajustes) reduce(imap(ajustes, ~ extraer_mrqap(.x, .y)), ~ full_join(.x, .y, by = "Term"))

tabla_CO_pond <- formatear_tabla(ajustes_CO_pond)
tabla_CR_pond <- formatear_tabla(ajustes_CR_pond)

# Comparación lado a lado con la Tabla 4 oficial (binarizada, ya en cache de
# 03_mrqap.R -- c03 trae los ajustes ya corridos, no hace falta re-correrlos).
tabla_CO_oficial <- formatear_tabla(c03$ajustes_CO) %>% rename_with(~paste0(.x, "_binaria"), -Term)
tabla_CR_oficial <- formatear_tabla(c03$ajustes_CR) %>% rename_with(~paste0(.x, "_binaria"), -Term)
tabla_CO_pond2   <- tabla_CO_pond %>% rename_with(~paste0(.x, "_ponderada"), -Term)
tabla_CR_pond2   <- tabla_CR_pond %>% rename_with(~paste0(.x, "_ponderada"), -Term)

comparacion_CO <- full_join(tabla_CO_oficial, tabla_CO_pond2, by = "Term")
comparacion_CR <- full_join(tabla_CR_oficial, tabla_CR_pond2, by = "Term")

write_xlsx(
  list(
    Resumen = tibble(
      nota = c(
        "Comparación EXPLORATORIA (no oficial): MRQAP con redes ponderadas por orden de nominación (fuerza_desde_rango, la misma transformación que ya usa Figura 1 y la sección 'Actores clave') vs. la Tabla 4 oficial del artículo (redes binarizadas, presencia/ausencia).",
        "DV ponderada = CO_FIN_fuerza/CR_FIN_fuerza (4 valores posibles: 0/0.33/0.67/1, no continua). Predictor Early ponderado = CO_early_fuerza/CR_early_fuerza.",
        "Covariables no-red (demográficas, distancia en creencias, atractivo del receptor) son IDÉNTICAS en ambas versiones -- no son redes de vínculo.",
        "Soporte (qué pares tienen arista) es idéntico entre binaria y ponderada -- ver chequeo al inicio del script. Solo cambia CUÁNTO pesa cada vínculo, no CUÁLES existen."
      )
    ),
    MRQAP_CO_binaria_vs_ponderada = comparacion_CO,
    MRQAP_CR_binaria_vs_ponderada = comparacion_CR,
    QAP_correlaciones_ponderado = tabla_qap_pond
  ),
  path = file.path(RUTA_SALIDAS, "Resultados_MRQAP_ponderado.xlsx")
)

message("03c_mrqap_ponderado.R completo. Revisa 02outputs/Resultados_MRQAP_ponderado.xlsx y Resultados_Correlaciones_QAP_ponderado.xlsx")
