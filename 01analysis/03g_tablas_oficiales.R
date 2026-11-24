############################################################
# 03g_TABLAS_OFICIALES.R — Fuente única y corregida de Tabla 4 y Tabla 5 (30-sep-2026)
# Taller de Teatro y Creatividad Docente
#
# POR QUÉ EXISTE:
#   Al revisar el pedido de Diego de ponderar también el predictor Early (no
#   solo la DV de Tabla 5 y las redes de QAP), se encontró un bug real en la
#   extracción de resultados MRQAP que viene desde que se escribió
#   03_mrqap.R: summary.netlm()/summary.netlogit() devuelven $coefficients
#   como un VECTOR simple (solo estimaciones puntuales), no una matriz con
#   columna de p-valor. El código original hacía
#     m <- as.matrix(s$coefficients); p_vals <- m[, ncol(m)]
#   que para un vector forzado a matriz de 1 columna devuelve la MISMA
#   estimación como si fuera el p-valor. Los asteriscos de significancia que
#   hasta hoy mostraban Tabla 4, Tabla 5 y los chequeos de robustez de
#   Limitaciones NO eran p-valores reales -- comparaban el coeficiente crudo
#   contra 0.001/0.01/0.05.
#
#   El p-valor real (bilateral, por permutación QAP) está en
#   summary(modelo)$pgreqabs, alineado por posición con
#   c("(Intercept)", terminos). Esto se verificó por tres vías independientes
#   antes de tocar el artículo: (1) el propio print(summary(modelo)) de sna,
#   columna "Pr(>=|b|)"; (2) la documentación de ?netlogit ("By default, the
#   z-statistic... is used"); (3) un recálculo manual desde
#   modelo$dist/modelo$tstat, línea por línea, sin usar pgreqabs en absoluto.
#   Las tres coinciden exactamente. Ver README para el detalle completo y el
#   impacto sustantivo en las conclusiones del artículo.
#
#   De paso, esta corrección se aprovecha para cerrar la pregunta de Early
#   ponderado vs. binario (la que motivó revisar esto): se usa la versión
#   ponderada por orden de nominación (Early_fuerza) como predictor OFICIAL
#   en ambas tablas, ya que no hay ningún requisito estadístico que obligue a
#   mantenerlo binario en ninguno de los dos modelos (a diferencia de la DV,
#   el predictor no está sujeto a esa restricción), y con p-valores correctos
#   la conclusión sustantiva es la misma con cualquiera de las dos versiones.
#
#   Este script NO reajusta ningún modelo -- todos los netlogit/netlm
#   relevantes ya estaban corridos con reps=5000 (03f_netlogit_earlypond.R
#   para Tabla 4, checkpoint CO/CR_EarlyPond_FINPond.rds de
#   03d_mrqap_factorial.R para Tabla 5). Solo corrige la extracción de
#   resultados desde esos objetos ya ajustados.
#
# ENTRADA:  01analysis/.cache/03f_netlogit_earlypond.rds        (Tabla 4)
#           01analysis/.cache/03d_checkpoints/{CO,CR}_EarlyPond_FINPond.rds (Tabla 5)
# SALIDA:   02outputs/Resultados_Tablas_Oficiales.xlsx
#           01analysis/.cache/03g_tablas_oficiales.rds
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

estrellas <- function(p) case_when(
  p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ "†", TRUE ~ ""
)

mcfadden <- function(mod) {
  s <- summary(mod)
  1 - s$deviance / s$null.deviance
}

r2_lm <- function(mod) {
  txt <- capture.output(print(summary(mod)))
  linea <- grep("Multiple R-squared", txt, value = TRUE)
  as.numeric(regmatches(linea, regexpr("[0-9]+\\.[0-9]+", linea))[1])
}

extraer_logit <- function(ajuste, nombre_col) {
  modelo <- ajuste$modelo
  s <- summary(modelo)
  estimates <- as.numeric(modelo$coefficients)
  p_vals    <- as.numeric(s$pgreqabs)
  nombres <- c("(Intercept)", ajuste$terminos)
  res <- tibble(Term = nombres, Value = paste0(round(estimates, 3), estrellas(p_vals)), p = round(p_vals, 4))
  res <- bind_rows(res, tibble(Term = "Pseudo-R2 (McFadden)", Value = as.character(round(mcfadden(modelo), 3)), p = NA_real_))
  colnames(res)[2] <- nombre_col
  colnames(res)[3] <- paste0("p_", nombre_col)
  res
}

extraer_lm <- function(ajuste, nombre_col) {
  modelo <- ajuste$modelo
  s <- summary(modelo)
  estimates <- as.numeric(modelo$coefficients)
  p_vals    <- as.numeric(s$pgreqabs)
  nombres <- c("(Intercept)", ajuste$terminos)
  res <- tibble(Term = nombres, Value = paste0(round(estimates, 3), estrellas(p_vals)), p = round(p_vals, 4))
  res <- bind_rows(res, tibble(Term = "R-squared", Value = as.character(round(r2_lm(modelo), 3)), p = NA_real_))
  colnames(res)[2] <- nombre_col
  colnames(res)[3] <- paste0("p_", nombre_col)
  res
}

############################################################
# TABLA 4 OFICIAL: netlogit, Early_fuerza, DV = CO_FIN/CR_FIN (binaria)
############################################################
c03f <- readRDS(file.path(RUTA_CACHE, "03f_netlogit_earlypond.rds"))

tabla4_co <- reduce(imap(c03f$ajustes_CO, ~ extraer_logit(.x, .y)), ~ full_join(.x, .y, by = "Term"))
tabla4_cr <- reduce(imap(c03f$ajustes_CR, ~ extraer_logit(.x, .y)), ~ full_join(.x, .y, by = "Term"))

############################################################
# TABLA 5 OFICIAL: netlm, Early_fuerza, DV = CO_FIN_fuerza/CR_FIN_fuerza (ponderada)
############################################################
ck_co <- readRDS(file.path(RUTA_CACHE, "03d_checkpoints", "CO_EarlyPond_FINPond.rds"))
ck_cr <- readRDS(file.path(RUTA_CACHE, "03d_checkpoints", "CR_EarlyPond_FINPond.rds"))

tabla5_co <- reduce(imap(ck_co, ~ extraer_lm(.x, .y)), ~ full_join(.x, .y, by = "Term"))
tabla5_cr <- reduce(imap(ck_cr, ~ extraer_lm(.x, .y)), ~ full_join(.x, .y, by = "Term"))

cat("\n==== TABLA 4 OFICIAL (netlogit, Early_fuerza) — CO / A(T4) ====\n"); print(tabla4_co, n = 20)
cat("\n==== TABLA 4 OFICIAL (netlogit, Early_fuerza) — CR / I(T4) ====\n"); print(tabla4_cr, n = 20)
cat("\n==== TABLA 5 OFICIAL (netlm, Early_fuerza, FIN ponderada) — CO / A(T4) ====\n"); print(tabla5_co, n = 20)
cat("\n==== TABLA 5 OFICIAL (netlm, Early_fuerza, FIN ponderada) — CR / I(T4) ====\n"); print(tabla5_cr, n = 20)

############################################################
# Tests omnibus (ajuste global del modelo Completo, cada uno)
############################################################
omnibus_logit <- function(mod) {
  s <- summary(mod)
  chi <- s$null.deviance - s$deviance
  df <- s$df.model
  c(chi = chi, df = df, p = pchisq(chi, df, lower.tail = FALSE))
}
omnibus_lm_p <- function(mod) {
  txt <- capture.output(print(summary(mod)))
  linea <- grep("F-statistic", txt, value = TRUE)
  as.numeric(regmatches(linea, regexpr("(?<=p-value: )[0-9.]+", linea, perl = TRUE)))
}

cat("\n==== OMNIBUS (modelo Completo) ====\n")
cat("netlogit CO (A(T4)):\n"); print(omnibus_logit(c03f$ajustes_CO$Completo$modelo))
cat("netlogit CR (I(T4)):\n"); print(omnibus_logit(c03f$ajustes_CR$Completo$modelo))
cat("netlm CO (A(T4)) p-omnibus:", omnibus_lm_p(ck_co$Completo$modelo), "\n")
cat("netlm CR (I(T4)) p-omnibus:", omnibus_lm_p(ck_cr$Completo$modelo), "\n")

write_xlsx(
  list(Tabla4_CO = tabla4_co, Tabla4_CR = tabla4_cr, Tabla5_CO = tabla5_co, Tabla5_CR = tabla5_cr),
  path = file.path(RUTA_SALIDAS, "Resultados_Tablas_Oficiales.xlsx")
)
saveRDS(
  list(tabla4_co = tabla4_co, tabla4_cr = tabla4_cr, tabla5_co = tabla5_co, tabla5_cr = tabla5_cr),
  file.path(RUTA_CACHE, "03g_tablas_oficiales.rds")
)

message("03g_tablas_oficiales.R completo. Revisa 02outputs/Resultados_Tablas_Oficiales.xlsx")
