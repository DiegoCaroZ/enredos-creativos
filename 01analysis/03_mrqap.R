############################################################
# 03_MRQAP.R — Etapa 3 de 4 del pipeline SNA
# Taller de Teatro y Creatividad Docente
#
# QUÉ HACE:
#   - Corre QAP (sna::qaptest) entre las 8 redes para la matriz de
#     correlaciones completa.
#   - Corre MRQAP (sna::netlm) para CO_FIN y CR_FIN con tres
#     especificaciones anidadas (Estructural / +Demográfico / Completo).
#   - Exporta ambas tablas ya formateadas con significancia.
#
# ENTRADA:  01analysis/.cache/01_limpieza.rds
#           01analysis/.cache/02_redes.rds
# SALIDA:   02outputs/Resultados_Correlaciones_QAP.xlsx
#           02outputs/Resultados_MRQAP.xlsx
#           01analysis/.cache/03_mrqap.rds  (para 05_visualizacion_alternativas.R)
#
# TIEMPO DE EJECUCIÓN: con REPS_QAP = 5000 (default), esta etapa tarda
# ~5 minutos en un notebook estándar (medido: 4m56s sobre los datos
# reales del taller, N=10 nodos core). Para iterar rápido mientras
# ajustas especificaciones, baja REPS_QAP a ~200-500 y solo vuelve a
# 5000 para la corrida final que citarás en el artículo.
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

REPS_QAP <- 5000  # sube/baja según necesites velocidad vs. precisión del p-valor

c01 <- readRDS(file.path(RUTA_CACHE, "01_limpieza.rds"))
c02 <- readRDS(file.path(RUTA_CACHE, "02_redes.rds"))
list2env(c01, envir = environment())
list2env(c02, envir = environment())

# BUGFIX 23-sep-2026: CO_FIN/CR_FIN (y el resto de las 8 matrices) solo
# quedaban DENTRO de lista_matrices en el cache, nunca como objetos sueltos.
# Si este script se corre standalone (Rscript 03_mrqap.R, sin pasar antes por
# 01_limpieza.R/02_redes.R en la MISMA sesión de R vía 00_run_pipeline.R),
# "CO_FIN"/"CR_FIN" no existían y el script fallaba en la sección de MRQAP
# (aunque la matriz de correlaciones QAP, que sí usa lista_matrices
# directamente, corría bien). El comentario del encabezado de este archivo
# dice explícitamente que se puede correr esta etapa sola -- esto lo deja
# cierto de verdad.
CO_FIN <- lista_matrices$CO_FIN
CR_FIN <- lista_matrices$CR_FIN

# La semilla se fija AQUÍ (no en 01_limpieza.R) porque es justo antes del
# primer sorteo aleatorio real del pipeline (qaptest). Si algún día se
# reordenan las etapas, mover set.seed() al inicio de la etapa que hace
# el primer sorteo, no dejarlo "flotando" en un script que ya no lo usa.
set.seed(2026)


############################################################
# 1. CORRELACIONES QAP ENTRE LAS 8 REDES (matriz completa, con loop)
############################################################
n_redes  <- length(lista_matrices)
nombres_r <- names(lista_matrices)
mat_r <- matrix(NA, n_redes, n_redes, dimnames = list(nombres_r, nombres_r))
mat_p <- matrix(NA, n_redes, n_redes, dimnames = list(nombres_r, nombres_r))

for (i in 1:n_redes) {
  for (j in i:n_redes) {
    if (i == j) {
      mat_r[i, j] <- 1; mat_p[i, j] <- 0
    } else {
      cat("QAP:", nombres_r[i], "vs", nombres_r[j], "...\n")
      qap_out <- sna::qaptest(list(lista_matrices[[i]], lista_matrices[[j]]),
                              FUN = gcor, g1 = 1, g2 = 2, reps = REPS_QAP)
      r <- qap_out$testval
      # p-valor bilateral calculado directamente desde la distribución nula por
      # permutación (qap_out$dist), en vez de summary()$pgreqabs -- ese accesor
      # cambia de nombre/forma entre versiones de sna y puede devolver un vector,
      # no un escalar, rompiendo la asignación a una sola celda.
      p <- mean(abs(qap_out$dist) >= abs(r))
      mat_r[i, j] <- mat_r[j, i] <- r
      mat_p[i, j] <- mat_p[j, i] <- p
    }
  }
}

tabla_qap <- as.data.frame(mat_r) %>%
  mutate(Red = rownames(.)) %>%
  pivot_longer(-Red, names_to = "Red_Col", values_to = "r") %>%
  mutate(
    p = as.vector(t(mat_p)),
    signif = case_when(p <= 0.001 ~ "***", p <= 0.01 ~ "**", p <= 0.05 ~ "*", p <= 0.1 ~ ".", TRUE ~ ""),
    valor = paste0(format(round(r, 3), nsmall = 3), signif)
  ) %>%
  select(Red, Red_Col, valor) %>%
  pivot_wider(names_from = Red_Col, values_from = valor)

write_xlsx(tabla_qap, path = file.path(RUTA_SALIDAS, "Resultados_Correlaciones_QAP.xlsx"))


############################################################
# 2. MRQAP (netlm) — SOLO sobre nodos_core, sin duplicar modelos
############################################################
# Atributos alineados al MISMO orden que nodos_core (crítico para netlm)
# OJO: usamos vertices_df, no atributos crudo -- vertices_df ya tiene "genero"
# y "Type" normalizados.
datos_qap <- vertices_df %>%
  filter(name %in% nodos_core) %>%
  arrange(match(name, nodos_core))

stopifnot(identical(datos_qap$name, nodos_core))  # chequeo de alineación duro

imputar_media <- function(x) { x[is.na(x)] <- mean(x, na.rm = TRUE); x }
datos_qap <- datos_qap %>%
  mutate(CSE1 = imputar_media(CSE1), G1 = imputar_media(G1), FI1 = imputar_media(FI1))

mat_mismo_genero <- { m <- outer(datos_qap$genero, datos_qap$genero, "=="); diag(m) <- 0; m * 1 }
mat_mismo_tipo   <- { m <- outer(datos_qap$Type,   datos_qap$Type,   "=="); diag(m) <- 0; m * 1 }

# Diagnóstico: ¿el efecto de Same_Type es genérico o lo carga sobre todo Leng?
# (con Art/Dif/Eng con 1-2 casos, casi toda la combinatoria "misma especialidad"
# cae en el subgrupo Leng -- separamos el efecto para no sobregeneralizar)
mat_ambos_leng <- { m <- outer(datos_qap$Type == "Leng", datos_qap$Type == "Leng", "&"); diag(m) <- 0; m * 1 }

mat_dist_cse     <- as.matrix(dist(datos_qap$CSE1))
mat_dist_growth  <- as.matrix(dist(datos_qap$G1))
mat_dist_fixed   <- as.matrix(dist(datos_qap$FI1))

crear_matriz_target <- function(vector_atributo, n_nodos) {
  # Efecto de RECEPTOR (columna j constante = atributo de j): "¿atrae vínculos?"
  m <- matrix(rep(vector_atributo, each = n_nodos), nrow = n_nodos, byrow = TRUE)
  diag(m) <- 0
  m
}
crear_matriz_ego <- function(vector_atributo, n_nodos) {
  # Efecto de EMISOR (fila i constante = atributo de i): "¿nomina distinto según su condición?"
  m <- matrix(rep(vector_atributo, times = n_nodos), nrow = n_nodos, byrow = FALSE)
  diag(m) <- 0
  m
}
n_core <- length(nodos_core)
mat_target_cse    <- crear_matriz_target(datos_qap$CSE1, n_core)
mat_target_growth <- crear_matriz_target(datos_qap$G1,   n_core)
mat_target_fixed  <- crear_matriz_target(datos_qap$FI1,  n_core)

# Sin_Historial_Temprano alineado al orden de nodos_core (viene de 01_limpieza.R)
sin_historial_vec <- asistencia$Sin_Historial_Temprano[match(nodos_core, asistencia$name)]
mat_ego_sin_historial <- crear_matriz_ego(sin_historial_vec, n_core)

# --- Especificaciones anidadas (evita repetir netlm() copiado y pegado) ---
specs <- list(
  Estructural = list(Early_CO = CO_early, Early_CR = CR_early),
  Demografico = list(Same_Gender = mat_mismo_genero, Same_Type = mat_mismo_tipo,
                     Ambos_Leng = mat_ambos_leng,
                     Ego_Sin_Historial_Temprano = mat_ego_sin_historial),
  Homofilia   = list(Dist_CSE = mat_dist_cse, Dist_Growth = mat_dist_growth, Dist_Fixed = mat_dist_fixed),
  Atractivo   = list(Target_CSE = mat_target_cse, Target_Growth = mat_target_growth, Target_Fixed = mat_target_fixed)
)

correr_mrqap <- function(y, incluir, reps = REPS_QAP) {
  x <- purrr::flatten(specs[incluir])
  # netlm() NO conserva los nombres de x=list(...): el summary siempre imprime
  # x1, x2, ... Guardamos aparte el orden real para poder etiquetar la tabla final.
  list(modelo = netlm(y = y, x = x, reps = reps), terminos = names(x))
}

ajustes_CO <- list(
  Estructural              = correr_mrqap(CO_FIN, c("Estructural")),
  Estructural_Demografico  = correr_mrqap(CO_FIN, c("Estructural", "Demografico")),
  Completo                 = correr_mrqap(CO_FIN, c("Estructural", "Demografico", "Homofilia", "Atractivo"))
)

ajustes_CR <- list(
  Estructural              = correr_mrqap(CR_FIN, c("Estructural")),
  Estructural_Demografico  = correr_mrqap(CR_FIN, c("Estructural", "Demografico")),
  Completo                 = correr_mrqap(CR_FIN, c("Estructural", "Demografico", "Homofilia", "Atractivo"))
)

modelos_CO <- map(ajustes_CO, "modelo")
modelos_CR <- map(ajustes_CR, "modelo")

for (nm in names(modelos_CO)) { cat("\n=== CO ~", nm, "===\n"); print(summary(modelos_CO[[nm]])) }
for (nm in names(modelos_CR)) { cat("\n=== CR ~", nm, "===\n"); print(summary(modelos_CR[[nm]])) }


############################################################
# 3. EXPORTAR TABLAS MRQAP YA FORMATEADAS (una función, no copy-paste)
############################################################
extraer_r2 <- function(s) {
  # El nombre exacto del campo de R^2 varía entre versiones de sna, así que
  # probamos los nombres más comunes y, si ninguno existe, lo recuperamos
  # parseando el texto que summary.netlm() de todas formas imprime siempre.
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
  modelo <- ajuste$modelo
  s <- summary(modelo)
  m <- as.matrix(s$coefficients)
  estimates <- m[, 1]
  p_vals    <- m[, ncol(m)]  # Pr(>=|b|): p-valor bilateral
  stars <- case_when(p_vals < 0.001 ~ "***", p_vals < 0.01 ~ "**", p_vals < 0.05 ~ "*", p_vals < 0.1 ~ ".", TRUE ~ "")
  nombres <- c("(Intercept)", ajuste$terminos)
  res <- tibble(Term = nombres, Value = paste0(round(estimates, 3), stars))
  r2 <- extraer_r2(s)
  res <- bind_rows(res, tibble(Term = "R-squared", Value = as.character(round(r2, 3))))
  colnames(res)[2] <- nombre_col
  res
}

tabla_co <- reduce(imap(ajustes_CO, ~ extraer_mrqap(.x, .y)), ~ full_join(.x, .y, by = "Term"))
tabla_cr <- reduce(imap(ajustes_CR, ~ extraer_mrqap(.x, .y)), ~ full_join(.x, .y, by = "Term"))

write_xlsx(
  list(MRQAP_CO = tabla_co, MRQAP_CR = tabla_cr, Nodos_Core = tibble(name = nodos_core), Excluidos = no_core),
  path = file.path(RUTA_SALIDAS, "Resultados_MRQAP.xlsx")
)


############################################################
# 4. GUARDAR CACHE (para 05_visualizacion_alternativas.R: heatmap QAP,
#    y para poder resaltar en las figuras qué predictores fueron significativos)
############################################################
saveRDS(
  list(
    mat_r = mat_r, mat_p = mat_p, tabla_qap = tabla_qap,
    ajustes_CO = ajustes_CO, ajustes_CR = ajustes_CR,
    tabla_co = tabla_co, tabla_cr = tabla_cr
  ),
  file = file.path(RUTA_CACHE, "03_mrqap.rds")
)

message("03_mrqap.R completo. Revisa 02outputs/Resultados_Correlaciones_QAP.xlsx y Resultados_MRQAP.xlsx")
message(sprintf("Modelos MRQAP corridos sobre N = %d nodos (core persistente).", n_core))


summary(modelos_CO$Completo)
summary(modelos_CR$Completo)
