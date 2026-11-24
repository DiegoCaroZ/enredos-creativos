############################################################
# 04_VISUALIZACION.R — Etapa 4 de 4 del pipeline SNA (figuras "clásicas")
# Taller de Teatro y Creatividad Docente
#
# QUÉ HACE:
#   Reproduce EXACTAMENTE las figuras que ya conocías (sociogramas en
#   layout circular, paneles por ola), ahora en su propio script.
#   No cambia ningún cálculo -- solo dónde vive el código.
#
# ÚNICO CAMBIO DE COMPORTAMIENTO respecto al script original:
#   Por defecto, las etiquetas de nodo usan el código anonimizado
#   (id_anon, ver 01_limpieza.R) en vez del nombre real. Con 13
#   docentes, una figura con nombres reales es fácilmente
#   reidentificable si termina en un paper o una presentación de
#   congreso. Para volver a nombres reales (solo para uso interno,
#   nunca para el artículo), cambia USAR_ANONIMOS a FALSE.
#
# ENTRADA:  01analysis/.cache/01_limpieza.rds
#           01analysis/.cache/02_redes.rds
# SALIDA:   02outputs/Plot_Early.pdf
#           02outputs/Plot_Early2.pdf
############################################################

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(igraph)
})

RUTA_SALIDAS <- here::here("02outputs")
RUTA_CACHE   <- here::here("01analysis", ".cache")

USAR_ANONIMOS <- TRUE  # <- cambia a FALSE para volver a nombres reales (solo uso interno)

c01 <- readRDS(file.path(RUTA_CACHE, "01_limpieza.rds"))
c02 <- readRDS(file.path(RUTA_CACHE, "02_redes.rds"))
list2env(c01, envir = environment())
list2env(c02, envir = environment())

etiqueta_nodo <- function(g) {
  if (!USAR_ANONIMOS) return(V(g)$name)
  vertices_df$id_anon[match(V(g)$name, vertices_df$name)]
}

coords <- layout_in_circle(grafos$Wg1)

edge_width_ranked <- function(w) {
  w <- as.numeric(w)
  case_when(w == 1 ~ 3, w == 2 ~ 2, w >= 3 ~ 1, TRUE ~ 1)
}

plot_red <- function(g, titulo, coords, vertex_size_mult = 4) {
  ideg <- igraph::degree(g, mode = "in", loops = FALSE)
  plot(g, layout = coords,
       vertex.size = pmax(3, ideg * vertex_size_mult),
       edge.width  = edge_width_ranked(E(g)$weight),
       edge.color  = "cornsilk4", edge.curved = 0.25,
       vertex.label = etiqueta_nodo(g), vertex.label.color = "grey20",
       edge.arrow.size = 0.3, vertex.label.dist = 1.7,
       vertex.label.degree = -pi / 2)
  title(titulo)
}


############################################################
# 1. Plot_Early.pdf — panel 2x3, T1-T3, CO y CR
############################################################
pdf(file.path(RUTA_SALIDAS, "Plot_Early.pdf"), width = 10, height = 8)
par(mfrow = c(2, 3), mar = c(1, 1, 1, 1))
plot_red(grafos$Wg1, "Afinidad T1", coords); plot_red(grafos$Wg2, "Afinidad T2", coords); plot_red(grafos$Wg3, "Afinidad T3", coords)
plot_red(grafos$Wg5, "Influencia Creativa T1", coords); plot_red(grafos$Wg6, "Influencia Creativa T2", coords); plot_red(grafos$Wg7, "Influencia Creativa T3", coords)
legend("bottomright", legend = names(colores_genero), fill = unname(colores_genero), bty = "n", cex = 1.1)
dev.off()


############################################################
# 2. Plot_Early2.pdf — panel 2x4, T1-T3 + Early (core), CO y CR
############################################################
coords_core <- layout_in_circle(WgEarlyCO)

pdf(file.path(RUTA_SALIDAS, "Plot_Early2.pdf"), width = 10, height = 8)
par(mfrow = c(2, 4), mar = c(1, 1, 1, 1))
plot_red(grafos$Wg1, "Afinidad T1", coords); plot_red(grafos$Wg2, "Afinidad T2", coords); plot_red(grafos$Wg3, "Afinidad T3", coords)
plot_red(WgEarlyCO, "Afinidad Early (T1-T3, core)", coords_core)
plot_red(grafos$Wg5, "Influencia Creativa T1", coords); plot_red(grafos$Wg6, "Influencia Creativa T2", coords); plot_red(grafos$Wg7, "Influencia Creativa T3", coords)
plot_red(WgEarlyCR, "Influencia Creativa Early (T1-T3, core)", coords_core)
legend("bottomright", legend = names(colores_genero), fill = unname(colores_genero), bty = "n", cex = 1.1)
dev.off()

message("04_visualizacion.R completo. Revisa 02outputs/Plot_Early.pdf y Plot_Early2.pdf")
message(sprintf("Etiquetas usadas: %s", if (USAR_ANONIMOS) "anonimizadas (id_anon)" else "nombres reales"))
