############################################################
# 02_REDES.R — Etapa 2 de 4 del pipeline SNA
# Taller de Teatro y Creatividad Docente
#
# QUÉ HACE:
#   - Calcula la tabla de centralidades para las 8 redes (Wg1..Wg8).
#   - Alinea las 8 redes al CORE (definido en 01_limpieza.R) como
#     matrices de adyacencia N x N comparables.
#   - Calcula las redes "early" (promedio NA-aware de T1-T3) que se
#     usan como predictor estructural en el MRQAP.
#   - Arma g_comparacion: un objeto igraph único con TODOS los atributos
#     de nodo (demográficos, constructos T1/T2, asistencia) y las DOS
#     redes (CO y CR) en las DOS olas de comparación (Early = promedio
#     T1-T3, FIN = última medición), como aristas paralelas distinguidas
#     por `tipo` ("CO"/"CR") y `ola` ("Early"/"FIN"), con su peso en `peso`.
#
# ENTRADA:  01analysis/.cache/01_limpieza.rds
# SALIDA:   02outputs/centralidades_edge2.xlsx
#           02outputs/Red_Comparacion_Early_FIN_atributos.rds  (objeto igraph completo)
#           01analysis/.cache/02_redes.rds  (para 03_mrqap.R y las
#           etapas de visualización)
############################################################

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(igraph)
  library(writexl)
})

RUTA_SALIDAS <- here::here("02outputs")
RUTA_CACHE   <- here::here("01analysis", ".cache")

c01 <- readRDS(file.path(RUTA_CACHE, "01_limpieza.rds"))
list2env(c01, envir = environment())


############################################################
# 1. CENTRALIDADES (reemplaza el bloque repetido x8 del script viejo)
############################################################
edge_cost <- function(g) {
  w <- as.numeric(igraph::E(g)$weight)
  if (all(is.na(w))) return(NULL)
  w
}

harmonic_dir <- function(g, mode = c("out", "in"), weights = NULL) {
  mode <- match.arg(mode)
  D <- igraph::distances(g, mode = mode, weights = weights)
  diag(D) <- Inf
  rowSums(1 / D, na.rm = TRUE)
}

centrality_table <- function(g, graph_name) {
  node <- igraph::V(g)$name
  deg_all <- igraph::degree(g, mode = "all", loops = FALSE)
  deg_in  <- igraph::degree(g, mode = "in",  loops = FALSE)
  deg_out <- igraph::degree(g, mode = "out", loops = FALSE)

  w_rank  <- as.numeric(igraph::E(g)$weight)
  w_force <- 4 - pmin(w_rank, 3)  # 1ª mención (rank=1) -> fuerza 3; rank>=3 -> fuerza 1

  str_all <- igraph::strength(g, mode = "all", weights = w_force, loops = FALSE)
  str_in  <- igraph::strength(g, mode = "in",  weights = w_force, loops = FALSE)
  str_out <- igraph::strength(g, mode = "out", weights = w_force, loops = FALSE)

  cost <- edge_cost(g)
  btw     <- igraph::betweenness(g, directed = TRUE, weights = cost, normalized = TRUE)
  clo_out <- igraph::closeness(g, mode = "out", weights = cost, normalized = TRUE)
  clo_in  <- igraph::closeness(g, mode = "in",  weights = cost, normalized = TRUE)

  eig <- tryCatch(
    igraph::eigen_centrality(g, directed = TRUE, weights = NULL, scale = TRUE)$vector,
    error = function(e) rep(NA_real_, igraph::vcount(g))
  )

  fuerza_pr <- 4 - pmin(w_rank, 3)
  pr <- igraph::page_rank(g, directed = TRUE, weights = fuerza_pr)$vector

  harm_out <- harmonic_dir(g, mode = "out", weights = cost)
  harm_in  <- harmonic_dir(g, mode = "in",  weights = cost)

  tibble(
    graph = graph_name, node = node,
    degree_total = deg_all, degree_in = deg_in, degree_out = deg_out,
    strength_total = str_all, strength_in = str_in, strength_out = str_out,
    betweenness = btw, closeness_out = clo_out, closeness_in = clo_in,
    harmonic_out = harm_out, harmonic_in = harm_in,
    eigenvector = eig, pagerank = pr
  ) %>%
    arrange(desc(degree_in), desc(pagerank))
}

nombres_completos <- c(
  Wg1 = "Wg1_CO_T1", Wg2 = "Wg2_CO_T2", Wg3 = "Wg3_CO_T3", Wg4 = "Wg4_CO_FIN",
  Wg5 = "Wg5_CR_T1", Wg6 = "Wg6_CR_T2", Wg7 = "Wg7_CR_T3", Wg8 = "Wg8_CR_FIN"
)

tablas_centralidad <- imap(grafos, ~ centrality_table(.x, nombres_completos[[.y]]))
tab_all <- bind_rows(tablas_centralidad)

write_xlsx(
  c(tablas_centralidad, list(All = tab_all)),
  path = file.path(RUTA_SALIDAS, "centralidades_edge2.xlsx")
)


############################################################
# 2. MATRICES ALINEADAS AL CORE + REDES "EARLY" (T1-T3 promediadas)
############################################################
get_adj <- function(g, nodes, binary = TRUE) {
  M <- as.matrix(as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  M[is.na(M)] <- 0
  if (binary) M <- 1 * (M > 0)
  M <- M[nodes, nodes, drop = FALSE]
  diag(M) <- 0
  M
}

# Todo alineado al CORE definido en 01_limpieza.R (ya no al roster completo)
CO_T1 <- get_adj(grafos$Wg1, nodos_core); CO_T2 <- get_adj(grafos$Wg2, nodos_core)
CO_T3 <- get_adj(grafos$Wg3, nodos_core); CO_FIN <- get_adj(grafos$Wg4, nodos_core)
CR_T1 <- get_adj(grafos$Wg5, nodos_core); CR_T2 <- get_adj(grafos$Wg6, nodos_core)
CR_T3 <- get_adj(grafos$Wg7, nodos_core); CR_FIN <- get_adj(grafos$Wg8, nodos_core)

# --- Promedio "early" NA-aware, condicionado en la asistencia de AMBOS (ego Y alter) ---
# HISTORIA (importante para entender por qué hay dos funciones acá abajo):
# la primera versión de este cálculo (compute_early_avg_ego_only, más abajo)
# promediaba la fila de cada ego SOLO sobre las olas a las que asistió ESE ego,
# sin chequear si el alter (la columna) también estaba presente esa ola. El
# 23-sep-2026 se detectó que el diseño del taller impedía que un ego nominara
# a un alter que no estuviera presente esa sesión -- así que un "0" en la celda
# (i,j) de una ola podía significar dos cosas muy distintas: "i no nominó a j"
# (dato real) o "j no estaba en la sala, i no podía nominarlo" (cero estructural
# forzado por el diseño, no evidencia de ausencia de vínculo). Es un problema
# clásico de especificación de frontera de red / dato faltante estructural
# (boundary specification), no un simple error de codificación, y afectaba una
# fracción grande de las celdas: de las 162 celdas ego-alter-ola que entraban al
# promedio original, 72 (44.4%) tenían al alter ausente esa ola; 59 de los 90
# pares ordenados del core (65.6%) tenían al menos una ola afectada.
#
# compute_early_avg() (la versión oficial desde el 23-sep-2026, abajo) corrige
# esto: la celda (i,j) se promedia SOLO sobre las olas donde asistieron AMBOS,
# ego y alter. Se comparó contra la versión anterior en
# 03b_mrqap_asistencia_alter.R antes de reemplazarla: el predictor cambia de
# forma no trivial (26/90 celdas distintas en CO, correlación 0.89 con la
# versión anterior; 21/90 en CR, correlación 0.92) y el signo del coeficiente
# de Early_CO en el modelo MRQAP de CO_FIN se invierte (de negativo a positivo,
# manteniéndose significativo) -- ver Resultados_MRQAP_comparacion_asistencia.xlsx
# para el detalle completo de esa comparación.
#
# Nota sobre pares sin ninguna ventana válida: 24 de los 90 pares ordenados del
# core (todo par con Javiera, que no asistió a ninguna ola T1-T3; y pares como
# Laura->Felipe, donde Laura solo asistió a T3 y Felipe no) nunca coincidieron
# en una misma ola T1-T3 -- para esos pares no existe NINGÚN dato observable
# sobre si habría vínculo o no. Se imputan a 0 con el mismo criterio ya usado
# para "sin historial temprano" (sin dato -> no se puede afirmar vínculo), pero
# se guarda aparte qué celdas son esta imputación "sin ventana"
# (CO_early_sin_ventana / CR_early_sin_ventana) para que quede trazable y no se
# confunda con una celda donde sí hubo oportunidad de observar y el vínculo
# simplemente no se dio.
compute_early_avg <- function(mats_list, presente_df, nodes) {
  n <- length(nodes); k <- length(mats_list)
  arr <- array(NA_real_, dim = c(n, n, k))
  for (w in seq_len(k)) arr[, , w] <- mats_list[[w]]
  pres_mat <- as.matrix(presente_df[match(nodes, presente_df$name), -1])  # n x k, 1/0
  rownames(pres_mat) <- nodes

  M_avg <- matrix(NA_real_, n, n, dimnames = list(nodes, nodes))
  sin_ventana <- matrix(FALSE, n, n, dimnames = list(nodes, nodes))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) { M_avg[i, j] <- 0; next }
      waves_ij <- which(pres_mat[i, ] == 1 & pres_mat[j, ] == 1)  # AMBOS presentes
      if (length(waves_ij) == 0) {
        M_avg[i, j] <- NA_real_
        sin_ventana[i, j] <- TRUE
      } else if (length(waves_ij) == 1) {
        M_avg[i, j] <- arr[i, j, waves_ij]
      } else {
        M_avg[i, j] <- mean(vapply(waves_ij, function(w) arr[i, j, w], numeric(1)))
      }
    }
  }
  list(M = M_avg, sin_ventana = sin_ventana)
}

asistencia_core <- asistencia %>% filter(name %in% nodos_core) %>%
  select(name, Presente_T1, Presente_T2, Presente_T3)

res_CO_early <- compute_early_avg(list(CO_T1, CO_T2, CO_T3), asistencia_core, nodos_core)
res_CR_early <- compute_early_avg(list(CR_T1, CR_T2, CR_T3), asistencia_core, nodos_core)

CO_early <- res_CO_early$M
CR_early <- res_CR_early$M
CO_early_sin_ventana <- res_CO_early$sin_ventana
CR_early_sin_ventana <- res_CR_early$sin_ventana

# Imputación explícita para celdas sin ninguna ventana de co-presencia válida
# (NA -> 0), necesaria porque netlm() no acepta NA. Queda documentado, no oculto
# -- ver CO_early_sin_ventana/CR_early_sin_ventana para saber cuáles son.
CO_early[is.na(CO_early)] <- 0
CR_early[is.na(CR_early)] <- 0

message(sprintf(
  "CO_early / CR_early (predictor Estructural oficial desde 23-sep-2026, corregido por asistencia del alter): %d celdas del core sin ninguna ola de co-presencia ego-alter (imputadas a 0, ver comentario arriba).",
  sum(CO_early_sin_ventana)
))

WgEarlyCO <- set_vertex_gender_colors(graph_from_adjacency_matrix(CO_early, mode = "directed", weighted = TRUE))
WgEarlyCR <- set_vertex_gender_colors(graph_from_adjacency_matrix(CR_early, mode = "directed", weighted = TRUE))


############################################################
# 2b. VERSIÓN ANTERIOR (ego-only), CONSERVADA SOLO COMO REFERENCIA HISTÓRICA
############################################################
# Esta es la función original (previa al 23-sep-2026), que solo condiciona en
# la asistencia del EGO. Ya NO se usa para nada del pipeline oficial -- se deja
# acá, con las variables *_ego_only, únicamente para trazabilidad: es la que
# produjo los números que aparecían en Resultados_MRQAP.xlsx antes de esta
# corrección, y permite reproducir esa comparación (03b_mrqap_asistencia_alter.R)
# sin tener que reconstruirla desde cero si alguna vez hace falta revisitarla
# para la sección de métodos/limitaciones del artículo.
compute_early_avg_ego_only <- function(mats_list, presente_df, nodes) {
  n <- length(nodes); k <- length(mats_list)
  arr <- array(NA_real_, dim = c(n, n, k))
  for (w in seq_len(k)) arr[, , w] <- mats_list[[w]]
  pres_mat <- as.matrix(presente_df[match(nodes, presente_df$name), -1])  # n x k, 1/0

  M_avg <- matrix(NA_real_, n, n, dimnames = list(nodes, nodes))
  for (i in seq_len(n)) {
    waves_i <- which(pres_mat[i, ] == 1)
    if (length(waves_i) == 0) {
      M_avg[i, ] <- NA_real_
    } else if (length(waves_i) == 1) {
      M_avg[i, ] <- arr[i, , waves_i]
    } else {
      M_avg[i, ] <- rowMeans(sapply(waves_i, function(w) arr[i, , w]))
    }
  }
  diag(M_avg) <- 0
  M_avg
}

CO_early_ego_only <- compute_early_avg_ego_only(list(CO_T1, CO_T2, CO_T3), asistencia_core, nodos_core)
CR_early_ego_only <- compute_early_avg_ego_only(list(CR_T1, CR_T2, CR_T3), asistencia_core, nodos_core)
CO_early_ego_only[is.na(CO_early_ego_only)] <- 0
CR_early_ego_only[is.na(CR_early_ego_only)] <- 0


############################################################
# 3. GRAFO ATRIBUIDO COMPLETO (comparación Early vs FIN, CO + CR)
############################################################
# Un solo objeto igraph, con TODOS los atributos de nodo ya pegados
# (demográficos, constructos T1/T2, asistencia) y las dos redes (afinidad
# y creativa) EN LAS DOS OLAS DE COMPARACIÓN, como aristas paralelas
# distinguidas por `tipo` ("CO"/"CR") y `ola` ("Early"/"FIN"). Pensado
# como punto de partida único: cualquier visualización o análisis sobre
# esta comparación filtra este objeto, no reconstruye nada desde matrices
# sueltas.
#
# OJO -- `peso` NO es la misma métrica en las dos olas (queda así porque
# así están definidas Early y FIN en el resto del pipeline, incluido el
# MRQAP; no se inventó una escala nueva para la figura):
#   - ola == "Early": peso = CO_early/CR_early, el promedio NA-aware de
#     T1-T3 de la presencia BINARIA del vínculo (0-1) -- el mismo predictor
#     estructural que entra al MRQAP.
#   - ola == "FIN": peso = fuerza normalizada a partir del RANGO de
#     nominación de la edgelist FIN (1ª mención -> 1, 2ª -> 0.67, 3ª o
#     más -> 0.33) -- no es una frecuencia, es intensidad de la nominación.
# Por eso, en las figuras, cada ola lleva su propia leyenda de ancho de
# arista (no se fusionan como si fueran comparables número a número).
vertices_comparacion <- vertices_df %>%
  filter(name %in% nodos_core) %>%
  left_join(
    asistencia %>% select(name, Presente_T1, Presente_T2, Presente_T3, Presente_FIN, Sin_Historial_Temprano),
    by = "name"
  ) %>%
  mutate(genero_norm = norm_genero(genero)) %>%
  arrange(match(name, nodos_core)) %>%
  select(name, id_anon, genero, genero_norm, Type,
         D1, CSE1, FI1, G1, V1, D2, CSE2, FI2, G2, V2,
         Presente_T1, Presente_T2, Presente_T3, Presente_FIN, Sin_Historial_Temprano)

edges_from_matrix <- function(mat, nodos, tipo, ola) {
  idx <- which(mat > 0, arr.ind = TRUE)
  if (nrow(idx) == 0) return(tibble(from = character(), to = character(), tipo = character(), ola = character(), peso = numeric()))
  tibble(from = nodos[idx[, 1]], to = nodos[idx[, 2]], tipo = tipo, ola = ola, peso = mat[idx])
}

# FIN con el RANGO original (no binarizado) para poder mostrar intensidad
# de nominación en la figura -- CO_FIN/CR_FIN (sección 2, arriba) ya están
# binarizados porque así los necesita el MRQAP como variable dependiente.
CO_FIN_rango <- get_adj(grafos$Wg4, nodos_core, binary = FALSE)
CR_FIN_rango <- get_adj(grafos$Wg8, nodos_core, binary = FALSE)
fuerza_desde_rango <- function(m) {
  out <- matrix(0, nrow(m), ncol(m), dimnames = dimnames(m))
  out[m > 0] <- (4 - pmin(m[m > 0], 3)) / 3
  out
}
CO_FIN_fuerza <- fuerza_desde_rango(CO_FIN_rango)
CR_FIN_fuerza <- fuerza_desde_rango(CR_FIN_rango)

# --- peso_orden: versión de Early que SÍ usa el orden/rango de nominación ---
# `peso` en Early (arriba) es la fracción NA-aware de olas T1-T3 con vínculo
# presente -- es el predictor tal cual entra al MRQAP, pero descarta el
# orden en que cada docente nominó a otro dentro de cada ola (esa misma
# columna `weight` de Wg1-Wg3/Wg5-Wg7 SÍ trae esa información, igual que
# Wg4/Wg8 la traen para FIN). Acá se aplica la MISMA transformación que ya
# se usa para FIN (fuerza_desde_rango: 1ª mención=1, 2ª=0.67, 3ª o más=0.33)
# a cada ola T1/T2/T3 por separado, y se promedian con el mismo promedio
# NA-aware (compute_early_avg) que ya se usa para `peso`. El soporte (qué
# pares quedan con arista) es IDÉNTICO al de `peso` -- weight>0 en una ola
# es exactamente cuándo fuerza_desde_rango(weight)>0 en esa misma ola --,
# así que esto NO agrega ni quita vínculos, solo cambia cuánto "pesa" cada
# uno. Para FIN, peso_orden == peso (FIN ya era rango desde un principio).
#
# OJO: peso_orden es una variable de VISUALIZACIÓN, igual que CO_FIN_fuerza
# ya lo era -- el MRQAP (03_mrqap.R) sigue usando `peso` (Early = fracción
# binaria, el predictor reportado). No se tocó ningún modelo.
#
# Usa el MISMO compute_early_avg (corregido por asistencia del alter, ver
# sección 2 arriba) que `peso`, precisamente para que ambas variables sigan
# teniendo el mismo soporte -- si peso_orden se hubiera quedado con el
# promedio ego-only mientras peso ya usa el corregido, el chequeo de soporte
# idéntico de más abajo habría dejado de tener sentido.
CO_T1_fuerza <- fuerza_desde_rango(get_adj(grafos$Wg1, nodos_core, binary = FALSE))
CO_T2_fuerza <- fuerza_desde_rango(get_adj(grafos$Wg2, nodos_core, binary = FALSE))
CO_T3_fuerza <- fuerza_desde_rango(get_adj(grafos$Wg3, nodos_core, binary = FALSE))
CR_T1_fuerza <- fuerza_desde_rango(get_adj(grafos$Wg5, nodos_core, binary = FALSE))
CR_T2_fuerza <- fuerza_desde_rango(get_adj(grafos$Wg6, nodos_core, binary = FALSE))
CR_T3_fuerza <- fuerza_desde_rango(get_adj(grafos$Wg7, nodos_core, binary = FALSE))

CO_early_fuerza <- compute_early_avg(list(CO_T1_fuerza, CO_T2_fuerza, CO_T3_fuerza), asistencia_core, nodos_core)$M
CR_early_fuerza <- compute_early_avg(list(CR_T1_fuerza, CR_T2_fuerza, CR_T3_fuerza), asistencia_core, nodos_core)$M
CO_early_fuerza[is.na(CO_early_fuerza)] <- 0
CR_early_fuerza[is.na(CR_early_fuerza)] <- 0

edges_comparacion <- bind_rows(
  edges_from_matrix(CO_early,     nodos_core, "CO", "Early"),
  edges_from_matrix(CR_early,     nodos_core, "CR", "Early"),
  edges_from_matrix(CO_FIN_fuerza, nodos_core, "CO", "FIN"),
  edges_from_matrix(CR_FIN_fuerza, nodos_core, "CR", "FIN")
)

edges_orden <- bind_rows(
  edges_from_matrix(CO_early_fuerza, nodos_core, "CO", "Early"),
  edges_from_matrix(CR_early_fuerza, nodos_core, "CR", "Early"),
  edges_from_matrix(CO_FIN_fuerza,   nodos_core, "CO", "FIN"),
  edges_from_matrix(CR_FIN_fuerza,   nodos_core, "CR", "FIN")
) %>% rename(peso_orden = peso)

# left_join 1:1 esperado (mismo soporte, ver nota arriba) -- si algún
# peso_orden quedara en NA sería señal de que el soporte SÍ difiere y hay
# que revisar el supuesto.
edges_comparacion <- edges_comparacion %>%
  left_join(edges_orden, by = c("from", "to", "tipo", "ola"))
if (anyNA(edges_comparacion$peso_orden)) {
  warning("peso_orden con NA -- el soporte de peso y peso_orden no coincide como se esperaba, revisar.")
}

# directed = TRUE, aristas paralelas permitidas (un mismo par i->j puede
# tener a la vez una arista CO y una CR, en la misma ola o en distintas --
# eso es justo lo que se necesita para poder filtrar por `tipo` y por
# `ola` sin perder ninguna).
g_comparacion <- graph_from_data_frame(edges_comparacion, directed = TRUE, vertices = vertices_comparacion)

message(sprintf(
  "g_comparacion: %d nodos, %d aristas (Early: %d CO / %d CR; FIN: %d CO / %d CR).\nAtributos de nodo: %s.\nAtributos de arista: %s.",
  vcount(g_comparacion), ecount(g_comparacion),
  sum(E(g_comparacion)$tipo == "CO" & E(g_comparacion)$ola == "Early"),
  sum(E(g_comparacion)$tipo == "CR" & E(g_comparacion)$ola == "Early"),
  sum(E(g_comparacion)$tipo == "CO" & E(g_comparacion)$ola == "FIN"),
  sum(E(g_comparacion)$tipo == "CR" & E(g_comparacion)$ola == "FIN"),
  paste(vertex_attr_names(g_comparacion), collapse = ", "),
  paste(edge_attr_names(g_comparacion), collapse = ", ")
))

saveRDS(g_comparacion, file = file.path(RUTA_SALIDAS, "Red_Comparacion_Early_FIN_atributos.rds"))


############################################################
# 4. GUARDAR CACHE PARA LAS SIGUIENTES ETAPAS
############################################################
saveRDS(
  list(
    lista_matrices = list(
      CO_T1 = CO_T1, CO_T2 = CO_T2, CO_T3 = CO_T3, CO_FIN = CO_FIN,
      CR_T1 = CR_T1, CR_T2 = CR_T2, CR_T3 = CR_T3, CR_FIN = CR_FIN
    ),
    CO_early  = CO_early,
    CR_early  = CR_early,
    CO_early_sin_ventana = CO_early_sin_ventana,
    CR_early_sin_ventana = CR_early_sin_ventana,
    CO_early_ego_only = CO_early_ego_only,
    CR_early_ego_only = CR_early_ego_only,
    CO_early_fuerza = CO_early_fuerza,
    CR_early_fuerza = CR_early_fuerza,
    CO_FIN_fuerza = CO_FIN_fuerza,
    CR_FIN_fuerza = CR_FIN_fuerza,
    WgEarlyCO = WgEarlyCO,
    WgEarlyCR = WgEarlyCR,
    g_comparacion = g_comparacion,
    tablas_centralidad = tablas_centralidad,
    tab_all = tab_all
  ),
  file = file.path(RUTA_CACHE, "02_redes.rds")
)

message("02_redes.R completo. Cache guardado en 01analysis/.cache/02_redes.rds")
