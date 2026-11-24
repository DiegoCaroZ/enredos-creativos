############################################################
# 03b_MRQAP_ASISTENCIA_ALTER.R — anexo de robustez / trazabilidad histórica
# Taller de Teatro y Creatividad Docente
#
# QUÉ HACE (y qué NO hace):
#   Este script NO forma parte del pipeline oficial (no lo corre
#   00_run_pipeline.R) y NO sobrescribe Resultados_MRQAP.xlsx.
#
#   HISTORIA: hasta el 23-sep-2026, el predictor "Estructural" (Early_CO /
#   Early_CR) de los modelos MRQAP se calculaba promediando, para cada ego,
#   SOLO sobre las olas T1-T3 a las que ese ego asistió -- sin chequear si el
#   alter (la columna) también estaba presente esa ola. Se detectó que el
#   diseño del taller impedía que un ego nominara a un alter ausente esa
#   sesión, así que ese promedio confundía "no hubo vínculo" con "el alter no
#   podía ser nominado esa ola" (cero estructural forzado, no un dato real).
#   Magnitud verificada: 44.4% de las celdas ego-alter-ola usadas en ese
#   promedio tenían al alter ausente esa ola; 65.6% de los pares del core
#   tenían al menos una ola afectada.
#
#   Desde el 23-sep-2026, CO_early/CR_early (en 02_redes.R y por lo tanto en
#   Resultados_MRQAP.xlsx) usan la versión CORREGIDA (promedia solo sobre
#   olas donde asistieron AMBOS, ego y alter) -- ver el comentario extenso en
#   02_redes.R, sección 2, para el detalle completo. Este script hace el
#   camino inverso al que hacía antes de ese reemplazo: vuelve a correr los
#   mismos modelos MRQAP pero con la versión ANTERIOR (ego-only,
#   CO_early_ego_only/CR_early_ego_only, conservada en el cache solo como
#   referencia histórica) y arma una tabla lado a lado -- oficial (corregido)
#   vs. legado (ego-only, el que estaba en Resultados_MRQAP.xlsx antes de
#   esta fecha) -- para que la sensibilidad de los resultados a esta decisión
#   metodológica quede documentada y disponible para la sección de métodos o
#   de límites del artículo, sin tener que rehacer el análisis desde cero.
#
#   Todo lo demás (variable dependiente CO_FIN/CR_FIN, covariables
#   Demografico/Homofilia/Atractivo, número de repeticiones QAP, orden de
#   las especificaciones anidadas) es IDÉNTICO a 03_mrqap.R, para que la
#   comparación sea de manzanas con manzanas: el único cambio es la fuente
#   del predictor Estructural.
#
# ENTRADA:  01analysis/.cache/01_limpieza.rds
#           01analysis/.cache/02_redes.rds  (trae CO_early_ego_only/CR_early_ego_only)
#           01analysis/.cache/03_mrqap.rds  (resultados OFICIALES, ya corregidos, para comparar)
# SALIDA:   02outputs/Resultados_MRQAP_comparacion_asistencia.xlsx
#           (NO toca 02outputs/Resultados_MRQAP.xlsx)
#
# OJO: las matrices de covariables (Demografico/Homofilia/Atractivo) están
# copiadas literalmente de 03_mrqap.R. Si alguna vez cambian allá, hay que
# actualizarlas acá también para que la comparación siga siendo válida.
############################################################

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(sna)
  library(writexl)
})

RUTA_SALIDAS <- here::here("02outputs")
RUTA_CACHE   <- here::here("01analysis", ".cache")

REPS_QAP <- 5000  # igual que 03_mrqap.R, para que los p-valores sean comparables

c01 <- readRDS(file.path(RUTA_CACHE, "01_limpieza.rds"))
c02 <- readRDS(file.path(RUTA_CACHE, "02_redes.rds"))
c03_oficial <- readRDS(file.path(RUTA_CACHE, "03_mrqap.rds"))  # resultados oficiales YA corregidos
list2env(c01, envir = environment())
list2env(c02, envir = environment())

# CO_FIN/CR_FIN no quedan como objetos sueltos en el cache de 02_redes.R
# (solo dentro de lista_matrices) -- se extraen acá explícitamente. Esto es
# necesario para correr este script de forma standalone (sin pasar por
# 00_run_pipeline.R en la misma sesión de R); 03_mrqap.R tenía el mismo
# problema latente hasta que se corrigió ahí también el 23-sep-2026.
CO_FIN <- lista_matrices$CO_FIN
CR_FIN <- lista_matrices$CR_FIN

set.seed(2026)  # misma semilla que 03_mrqap.R, mismo criterio (justo antes del primer sorteo)


############################################################
# 0. QUÉ TAN DISTINTO ES EL PREDICTOR LEGADO (ego-only) DEL OFICIAL (corregido)
############################################################
comparar_predictor <- function(oficial, legado, nombre) {
  off_diag <- upper.tri(oficial) | lower.tri(oficial)
  v_of <- oficial[off_diag]; v_leg <- legado[off_diag]
  n_distintas <- sum(v_of != v_leg)
  cat(sprintf(
    "%s: %d de %d celdas (fuera de la diagonal) distintas entre oficial y legado | correlación: %.3f\n",
    nombre, n_distintas, length(v_of), cor(v_of, v_leg)
  ))
  tibble(
    predictor = nombre,
    celdas_distintas = n_distintas,
    celdas_totales = length(v_of),
    correlacion_oficial_legado = cor(v_of, v_leg)
  )
}
resumen_predictor <- bind_rows(
  comparar_predictor(CO_early, CO_early_ego_only, "Early_CO"),
  comparar_predictor(CR_early, CR_early_ego_only, "Early_CR")
)


############################################################
# 1. COVARIABLES (copiadas de 03_mrqap.R, sin cambios)
############################################################
datos_qap <- vertices_df %>%
  filter(name %in% nodos_core) %>%
  arrange(match(name, nodos_core))
stopifnot(identical(datos_qap$name, nodos_core))

imputar_media <- function(x) { x[is.na(x)] <- mean(x, na.rm = TRUE); x }
datos_qap <- datos_qap %>%
  mutate(CSE1 = imputar_media(CSE1), G1 = imputar_media(G1), FI1 = imputar_media(FI1))

mat_mismo_genero <- { m <- outer(datos_qap$genero, datos_qap$genero, "=="); diag(m) <- 0; m * 1 }
# Same_Type ya NO entra a los modelos (ver nota en 03_mrqap.R, 23-sep-2026:
# casi colineal con Ambos_Leng dada la composición del grupo) -- se calcula
# igual, pero no se usa en `specs`, solo para mantener el mismo código que allá.
mat_mismo_tipo   <- { m <- outer(datos_qap$Type,   datos_qap$Type,   "=="); diag(m) <- 0; m * 1 }
mat_ambos_leng   <- { m <- outer(datos_qap$Type == "Leng", datos_qap$Type == "Leng", "&"); diag(m) <- 0; m * 1 }

mat_dist_cse    <- as.matrix(dist(datos_qap$CSE1))
mat_dist_growth <- as.matrix(dist(datos_qap$G1))
mat_dist_fixed  <- as.matrix(dist(datos_qap$FI1))

crear_matriz_target <- function(vector_atributo, n_nodos) {
  m <- matrix(rep(vector_atributo, each = n_nodos), nrow = n_nodos, byrow = TRUE)
  diag(m) <- 0; m
}
crear_matriz_ego <- function(vector_atributo, n_nodos) {
  m <- matrix(rep(vector_atributo, times = n_nodos), nrow = n_nodos, byrow = FALSE)
  diag(m) <- 0; m
}
n_core <- length(nodos_core)
mat_target_cse    <- crear_matriz_target(datos_qap$CSE1, n_core)
mat_target_growth <- crear_matriz_target(datos_qap$G1,   n_core)
mat_target_fixed  <- crear_matriz_target(datos_qap$FI1,  n_core)

sin_historial_vec <- asistencia$Sin_Historial_Temprano[match(nodos_core, asistencia$name)]
mat_ego_sin_historial <- crear_matriz_ego(sin_historial_vec, n_core)


############################################################
# 2. ESPECIFICACIONES -- solo "Estructural" usa la versión LEGADO (ego-only)
############################################################
specs <- list(
  Estructural = list(Early_CO = CO_early_ego_only, Early_CR = CR_early_ego_only),  # <- el único cambio
  Demografico = list(Same_Gender = mat_mismo_genero,
                     Ambos_Leng = mat_ambos_leng,
                     Ego_Sin_Historial_Temprano = mat_ego_sin_historial),
  Homofilia   = list(Dist_CSE = mat_dist_cse, Dist_Growth = mat_dist_growth, Dist_Fixed = mat_dist_fixed),
  Atractivo   = list(Target_CSE = mat_target_cse, Target_Growth = mat_target_growth, Target_Fixed = mat_target_fixed)
)

correr_mrqap <- function(y, incluir, reps = REPS_QAP) {
  x <- purrr::flatten(specs[incluir])
  list(modelo = netlm(y = y, x = x, reps = reps), terminos = names(x))
}

cat("\nCorriendo MRQAP con el predictor Estructural LEGADO (ego-only, pre-23-sep-2026)...\n")
ajustes_CO_legado <- list(
  Estructural              = correr_mrqap(CO_FIN, c("Estructural")),
  Estructural_Demografico  = correr_mrqap(CO_FIN, c("Estructural", "Demografico")),
  Completo                 = correr_mrqap(CO_FIN, c("Estructural", "Demografico", "Homofilia", "Atractivo"))
)
ajustes_CR_legado <- list(
  Estructural              = correr_mrqap(CR_FIN, c("Estructural")),
  Estructural_Demografico  = correr_mrqap(CR_FIN, c("Estructural", "Demografico")),
  Completo                 = correr_mrqap(CR_FIN, c("Estructural", "Demografico", "Homofilia", "Atractivo"))
)


############################################################
# 3. TABLA COMPARATIVA: oficial (corregido, en Resultados_MRQAP.xlsx) vs legado (ego-only)
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

extraer_mrqap <- function(ajuste, nombre_col) {
  # BUG CORREGIDO (30-sep-2026) -- ver nota en 03_mrqap.R::extraer_mrqap.
  # p_vals <- m[, ncol(m)] devolvía la estimación otra vez, no un p-valor.
  modelo <- ajuste$modelo
  s <- summary(modelo)
  estimates <- as.numeric(modelo$coefficients)
  p_vals    <- as.numeric(s$pgreqabs)
  stars <- case_when(p_vals < 0.001 ~ "***", p_vals < 0.01 ~ "**", p_vals < 0.05 ~ "*", p_vals < 0.1 ~ ".", TRUE ~ "")
  nombres <- c("(Intercept)", ajuste$terminos)
  res <- tibble(Term = nombres, Value = paste0(round(estimates, 3), stars))
  r2 <- extraer_r2(s)
  res <- bind_rows(res, tibble(Term = "R-squared", Value = as.character(round(r2, 3))))
  colnames(res)[2] <- nombre_col
  res
}

tabla_co_legado <- reduce(imap(ajustes_CO_legado, ~ extraer_mrqap(.x, .y)), ~ full_join(.x, .y, by = "Term"))
tabla_cr_legado <- reduce(imap(ajustes_CR_legado, ~ extraer_mrqap(.x, .y)), ~ full_join(.x, .y, by = "Term"))

# --- Tabla lado a lado: oficial (de 03_mrqap.rds, ya corregido) vs legado, por especificación ---
armar_comparacion <- function(tabla_oficial, tabla_legado, red) {
  names(tabla_oficial) <- c("Term", paste0(names(tabla_oficial)[-1], "_oficial_corregido"))
  names(tabla_legado)  <- c("Term", paste0(names(tabla_legado)[-1],  "_legado_ego_only"))
  comb <- full_join(tabla_oficial, tabla_legado, by = "Term")
  specs_nombres <- c("Estructural", "Estructural_Demografico", "Completo")
  orden_cols <- c("Term", unlist(lapply(specs_nombres, function(s) paste0(s, c("_oficial_corregido", "_legado_ego_only")))))
  comb <- comb[, intersect(orden_cols, names(comb))]
  comb$Red <- red
  comb
}
comparacion_co <- armar_comparacion(c03_oficial$tabla_co, tabla_co_legado, "CO")
comparacion_cr <- armar_comparacion(c03_oficial$tabla_cr, tabla_cr_legado, "CR")

write_xlsx(
  list(
    Resumen_Predictor          = resumen_predictor,
    MRQAP_CO_oficial_vs_legado = comparacion_co,
    MRQAP_CR_oficial_vs_legado = comparacion_cr
  ),
  path = file.path(RUTA_SALIDAS, "Resultados_MRQAP_comparacion_asistencia.xlsx")
)

message("\n03b_mrqap_asistencia_alter.R completo (anexo de robustez, NO oficial).")
message("Revisa 02outputs/Resultados_MRQAP_comparacion_asistencia.xlsx")
message("Resultados_MRQAP.xlsx (el oficial, ya con el predictor corregido) NO fue modificado por este script.")
