############################################################
# 03e_MRQAP_NETLOGIT.R — Fuente de la Tabla 4 OFICIAL del artículo (29-sep-2026)
# Taller de Teatro y Creatividad Docente
#
# POR QUÉ EXISTE (y por qué YA NO es un anexo de robustez):
#   Comentario de expertos en presentación (29-sep-2026): el entonces-oficial
#   Tabla 4 usaba sna::netlm() -- regresión lineal (MCO) -- sobre CO_FIN/CR_FIN,
#   que son redes BINARIAS (presencia/ausencia de vínculo). Eso es un "modelo
#   lineal de probabilidad" (LPM): funcionalmente puede predecir fuera de
#   [0,1] y asume una relación lineal predictor->probabilidad que no siempre
#   es razonable. sna::netlogit() ajusta el análogo logístico (misma
#   inferencia por permutación QAP, coeficientes en escala de log-odds), que
#   es la opción más estándar en la literatura MRQAP para una variable
#   dependiente binaria.
#
#   Diego decidió, junto con el chequeo de pesos por orden de nominación
#   (ver 03d_mrqap_factorial.R / README), resolver ambas críticas de una vez
#   emparejando cada modelo con la variable que le corresponde: netlogit para
#   la red de cierre BINARIA (¿existe el vínculo? -- este script, ahora Tabla 4
#   oficial), y netlm para la red de cierre PONDERADA (¿qué tan fuerte es? --
#   celda "EarlyBin_FINPond" de 03d_mrqap_factorial.R, ahora Tabla 5 oficial).
#   El antiguo netlm-sobre-binaria (LPM) quedó retirado de Resultados: ver
#   sección "Tabla 4 pasa a ser netlogit..." más abajo en este README.
#
#   Este script corre netlogit() sobre la MISMA especificación de 03_mrqap.R
#   (mismas matrices, mismas covariables, mismo núcleo de nodos, mismo
#   predictor Early oficial) con sus 3 modelos anidados de siempre.
#
# ENTRADA:  01analysis/.cache/01_limpieza.rds
#           01analysis/.cache/02_redes.rds
# SALIDA:   02outputs/Resultados_MRQAP_netlogit.xlsx
#
# TIEMPO DE EJECUCIÓN: netlogit() con reps=5000 es más lento que netlm() por
# iteración (cada permutación reajusta una regresión logística por IRLS en
# vez de una OLS cerrada). Con 6 modelos (3 especificaciones x CO/CR) puede
# acercarse al límite de 580000ms del tool Bash -- si truena por timeout,
# bajar REPS_QAP temporalmente y volver a 5000 solo para la corrida final.
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
list2env(c01, envir = environment())
list2env(c02, envir = environment())

CO_FIN <- lista_matrices$CO_FIN
CR_FIN <- lista_matrices$CR_FIN
stopifnot(all(as.vector(CO_FIN) %in% c(0, 1)))
stopifnot(all(as.vector(CR_FIN) %in% c(0, 1)))

set.seed(2026)

# --- Reconstrucción IDÉNTICA de las matrices de covariables de 03_mrqap.R ---
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

specs <- list(
  Estructural = list(Early_CO = CO_early, Early_CR = CR_early),
  Demografico = list(Same_Gender = mat_mismo_genero,
                     Ambos_Leng = mat_ambos_leng,
                     Ego_Sin_Historial_Temprano = mat_ego_sin_historial),
  Homofilia   = list(Dist_CSE = mat_dist_cse, Dist_Growth = mat_dist_growth, Dist_Fixed = mat_dist_fixed),
  Atractivo   = list(Target_CSE = mat_target_cse, Target_Growth = mat_target_growth, Target_Fixed = mat_target_fixed)
)

correr_mrqap_logit <- function(y, incluir, reps = REPS_QAP) {
  x <- purrr::flatten(specs[incluir])
  list(modelo = netlogit(y = y, x = x, reps = reps), terminos = names(x))
}

cat("Corriendo netlogit para CO_FIN (3 especificaciones)...\n")
ajustes_CO_logit <- list(
  Estructural              = correr_mrqap_logit(CO_FIN, c("Estructural")),
  Estructural_Demografico  = correr_mrqap_logit(CO_FIN, c("Estructural", "Demografico")),
  Completo                 = correr_mrqap_logit(CO_FIN, c("Estructural", "Demografico", "Homofilia", "Atractivo"))
)
cat("Corriendo netlogit para CR_FIN (3 especificaciones)...\n")
ajustes_CR_logit <- list(
  Estructural              = correr_mrqap_logit(CR_FIN, c("Estructural")),
  Estructural_Demografico  = correr_mrqap_logit(CR_FIN, c("Estructural", "Demografico")),
  Completo                 = correr_mrqap_logit(CR_FIN, c("Estructural", "Demografico", "Homofilia", "Atractivo"))
)

modelos_CO <- map(ajustes_CO_logit, "modelo")
modelos_CR <- map(ajustes_CR_logit, "modelo")
for (nm in names(modelos_CO)) { cat("\n=== CO (logit) ~", nm, "===\n"); print(summary(modelos_CO[[nm]])) }
for (nm in names(modelos_CR)) { cat("\n=== CR (logit) ~", nm, "===\n"); print(summary(modelos_CR[[nm]])) }

# --- Formateo (reutiliza la misma lógica que 03_mrqap.R, adaptada a netlogit) ---
# BUG CORREGIDO (30-sep-2026): ver nota extensa en 03_mrqap.R::extraer_mrqap.
# summary.netlogit()$coefficients también es un vector simple de estimaciones,
# no una matriz con p-valor -- `m[, ncol(m)]` devolvía la estimación otra vez,
# no un p-valor. El p-valor bilateral real está en summary()$pgreqabs. Además,
# ninguno de los nombres de campo que probaba extraer_pseudo_r2() existe en
# summary.netlogit (por eso siempre devolvía NA, sin usarse en el artículo) --
# se reemplaza por el cálculo directo de McFadden, que es el que de hecho se
# usó para los números de pseudo-R² que ya están en Resultados.
extraer_pseudo_r2 <- function(s) {
  1 - s$deviance / s$null.deviance
}

extraer_mrqap_logit <- function(ajuste, nombre_col) {
  modelo <- ajuste$modelo
  s <- summary(modelo)
  estimates <- as.numeric(modelo$coefficients)
  p_vals    <- as.numeric(s$pgreqabs)
  stars <- case_when(p_vals < 0.001 ~ "***", p_vals < 0.01 ~ "**", p_vals < 0.05 ~ "*", p_vals < 0.1 ~ ".", TRUE ~ "")
  nombres <- c("(Intercept)", ajuste$terminos)
  res <- tibble(Term = nombres, Value = paste0(round(estimates, 3), stars))
  colnames(res)[2] <- nombre_col
  res
}

tabla_co_logit <- reduce(imap(ajustes_CO_logit, ~ extraer_mrqap_logit(.x, .y)), ~ full_join(.x, .y, by = "Term"))
tabla_cr_logit <- reduce(imap(ajustes_CR_logit, ~ extraer_mrqap_logit(.x, .y)), ~ full_join(.x, .y, by = "Term"))

# --- Comparación directa con el modelo oficial (netlm, desde el cache de 03_mrqap.R) ---
c03 <- readRDS(file.path(RUTA_CACHE, "03_mrqap.rds"))
extraer_mrqap_lm <- function(ajuste, nombre_col) {
  # Mismo bug que extraer_mrqap_logit -- ver nota arriba. Corregido igual.
  modelo <- ajuste$modelo
  s <- summary(modelo)
  estimates <- as.numeric(modelo$coefficients)
  p_vals    <- as.numeric(s$pgreqabs)
  stars <- case_when(p_vals < 0.001 ~ "***", p_vals < 0.01 ~ "**", p_vals < 0.05 ~ "*", p_vals < 0.1 ~ ".", TRUE ~ "")
  nombres <- c("(Intercept)", ajuste$terminos)
  res <- tibble(Term = nombres, Value = paste0(round(estimates, 3), stars))
  colnames(res)[2] <- nombre_col
  res
}
tabla_co_lm <- extraer_mrqap_lm(c03$ajustes_CO$Completo, "netlm_Completo")
tabla_cr_lm <- extraer_mrqap_lm(c03$ajustes_CR$Completo, "netlm_Completo")

comparacion_CO <- full_join(tabla_co_lm, tabla_co_logit %>% select(Term, netlogit_Completo = Completo), by = "Term")
comparacion_CR <- full_join(tabla_cr_lm, tabla_cr_logit %>% select(Term, netlogit_Completo = Completo), by = "Term")

write_xlsx(
  list(
    MRQAP_CO_netlogit = tabla_co_logit,
    MRQAP_CR_netlogit = tabla_cr_logit,
    Comparacion_CO = comparacion_CO,
    Comparacion_CR = comparacion_CR
  ),
  path = file.path(RUTA_SALIDAS, "Resultados_MRQAP_netlogit.xlsx")
)

saveRDS(
  list(ajustes_CO_logit = ajustes_CO_logit, ajustes_CR_logit = ajustes_CR_logit,
       comparacion_CO = comparacion_CO, comparacion_CR = comparacion_CR),
  file = file.path(RUTA_CACHE, "03e_mrqap_netlogit.rds")
)

message("03e_mrqap_netlogit.R completo. Revisa 02outputs/Resultados_MRQAP_netlogit.xlsx")
