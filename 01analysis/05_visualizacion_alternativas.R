############################################################
# 05_VISUALIZACION_ALTERNATIVAS.R — Propuestas nuevas de visualización
# Taller de Teatro y Creatividad Docente
#
# POR QUÉ ESTE SCRIPT:
#   Las figuras clásicas (04_visualizacion.R) son sociogramas en layout
#   circular, coloreados solo por género, con las 8 redes tratadas como
#   paneles independientes. Eso deja fuera lo que el artículo (Art. 1,
#   cuantitativo) plantea como hallazgo central:
#
#     (a) que CO (afinidad) y CR (influencia creativa) tienen una
#         estructura muy relacionada ya en las olas tempranas -- pero
#         ninguna figura actual las pone una al lado de la otra con el
#         mismo layout para poder comparar directamente esa estructura;
#     (b) que el layout circular no es informativo -- el orden de
#         los nodos en el círculo es arbitrario (viene de layout_in_circle
#         sobre Wg1), así que no se puede "leer" la estructura de
#         subgrupos, solo el grosor de las aristas.
#
#   Este script propone 3 figuras:
#
#     Figura A: Early vs FIN, CO y CR juntos en cada panel  -> muestra (a)
#     Figura B: Evolución con layout estable                -> muestra (b)
#     Figura C: Heatmap de correlaciones QAP                -> respaldo estadístico
#
#   NOTA sobre nombres reales (a pedido de Diego, 16-sep-2026): la Figura A
#   usa el nombre real de cada docente, no el código anonimizado id_anon.
#   Si esta figura llega a un manuscrito o una presentación pública, vale
#   la pena reconsiderar eso -- con 10 docentes en el core, una figura de
#   red con nombres reales es fácilmente reidentificable. Para volver a
#   anonimizado, cambia USAR_NOMBRES_REALES a FALSE más abajo.
#
# ENTRADA:  01analysis/.cache/01_limpieza.rds
#           01analysis/.cache/02_redes.rds
#           01analysis/.cache/03_mrqap.rds
# SALIDA:   02outputs/Fig_A_Early_vs_FIN.pdf
#           02outputs/Fig_B_Evolucion_Layout_Estable.pdf
#           02outputs/Fig_C_Heatmap_QAP.pdf
############################################################

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(igraph)
  library(tidygraph)
  library(ggraph)
  library(ggplot2)
  library(patchwork)
  library(viridis)
  library(scales)
  library(ggrepel)
})

USAR_NOMBRES_REALES <- TRUE  # <- cambia a FALSE para usar id_anon en la Figura A

RUTA_SALIDAS <- here::here("02outputs")
RUTA_CACHE   <- here::here("01analysis", ".cache")

c01 <- readRDS(file.path(RUTA_CACHE, "01_limpieza.rds"))
c02 <- readRDS(file.path(RUTA_CACHE, "02_redes.rds"))
c03 <- readRDS(file.path(RUTA_CACHE, "03_mrqap.rds"))
list2env(c01, envir = environment())
list2env(c02, envir = environment())
list2env(c03, envir = environment())

set.seed(2026)  # reproducibilidad del layout force-directed

# Etiqueta anonimizada por defecto en TODAS las figuras nuevas.
# (nodos_core viene ordenado alfabéticamente por nombre real desde 01_limpieza.R;
# id_anon NO sigue ese orden, así que no reintroduce el patrón por otra vía)
lab_core <- setNames(vertices_df$id_anon[match(nodos_core, vertices_df$name)], nodos_core)
gen_core <- setNames(vertices_df$genero[match(nodos_core, vertices_df$name)] %>% norm_genero(), nodos_core)

# Género se distingue por FORMA de nodo (no por color) -- formas VACÍAS
# (solo contorno, sin relleno) para que el tamaño (centralidad/fuerza) se
# lea por el borde y no quede un disco opaco tapando las aristas que pasan
# por debajo: círculo, triángulo, cuadrado, todos sin relleno (shapes 0-14,
# donde el aes `color` pinta el contorno, no un relleno).
forma_genero <- c(
  "Femenino"        = 1,  # círculo vacío
  "Masculino"       = 2,  # triángulo vacío
  "Otro/No binario" = 0   # cuadrado vacío
)
color_nodo <- "grey25"  # color fijo del contorno de los nodos en la Figura B, ya que la forma lleva el género

theme_red <- theme_graph(base_family = "sans") +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )


############################################################
# FIGURA A — EARLY vs FIN: mismo layout, CO y CR juntos en cada panel
############################################################
# g_comparacion (armado en 02_redes.R) ya trae los nodos con todos sus
# atributos y las dos redes (CO/CR) en las dos olas de comparación, como
# aristas paralelas con `tipo` y `ola`. Acá se arman dos paneles filtrando
# por ola -- CO y CR se muestran JUNTOS en cada panel, distinguidos por
# color, nunca fusionados en una categoría "ambos": un par de docentes con
# vínculo en las dos redes simplemente tiene dos aristas (una de cada
# color), no una tercera categoría.
tg_comparacion <- as_tbl_graph(g_comparacion)

# Layout único calculado sobre la unión de TODOS los vínculos que entran a
# la figura (Early y FIN, CO y CR), para que la posición de cada docente
# sea la misma en los dos paneles.
union_bin_comp <- ((CO_early > 0) | (CR_early > 0) | (CO_FIN_fuerza > 0) | (CR_FIN_fuerza > 0)) * 1
g_union_comp <- graph_from_adjacency_matrix(union_bin_comp, mode = "directed")
xy_comp <- layout_with_fr(g_union_comp)
rownames(xy_comp) <- nodos_core
xy_comp <- xy_comp[V(g_comparacion)$name, ]  # alinear al orden de nodos de g_comparacion

etiqueta_a <- if (USAR_NOMBRES_REALES) "name" else "id_anon"
colores_tipo <- c(CO = "darkolivegreen4", CR = "darkgoldenrod4")
color_nodo_a <- "cornsilk4"  # color fijo de nodo en la Figura A (el tamaño ahora lleva la centralidad)

# --- Centralidad combinada (CO+CR juntos) por ola, para el TAMAÑO de nodo ---
# Diego preguntó si se puede calcular una centralidad para los dos tipos de
# vínculo A LA VEZ, y sí: se arma sobre el grafo YA filtrado por ola (Early o
# FIN) pero SIN filtrar por tipo, así que las aristas CO y CR cuentan juntas
# para la centralidad de cada docente -- no es sumar dos centralidades
# calculadas por separado, es UNA sola corrida sobre el grafo combinado.
#
# Elegí in-degree PONDERADO (= strength de entrada, usando `peso` como peso
# de arista) como medida por defecto:
#   - in-degree = "cuántas nominaciones de afinidad o influencia recibe cada
#     docente" -- se lee directo en términos del propio diseño del estudio
#     (las redes son de nominación), a diferencia de eigenvector o
#     betweenness que miden algo más abstracto (posición estructural).
#   - eigenvector es inestable en grafos chicos y con componentes poco
#     conectados (10 nodos): un par de aristas puede cambiar mucho el
#     resultado, y con red dirigida requiere cuidado adicional (empates,
#     convergencia).
#   - betweenness casi no discrimina acá: con 10 docentes y una red bastante
#     densa (sobre todo en FIN), la mayoría de los pares queda conectado por
#     caminos directos o casi directos, así que casi todos los betweenness
#     terminan cerca de 0 -- no es una medida informativa a esta escala.
# Si prefieres probar otra, cambia MEDIDA_CENTRALIDAD_FIG_A a "eigenvector" o
# "betweenness" (las tres quedan implementadas en centralidad_combinada()).
MEDIDA_CENTRALIDAD_FIG_A <- "indegree"

etiqueta_centralidad_base <- c(
  indegree    = "grado-entrada ponderado",
  eigenvector = "autovector",
  betweenness = "intermediación"
)
# Título de la leyenda de tamaño POR OLA (necesario aunque la medida sea la
# misma en los dos paneles: si los dos scale_size_continuous() llevaran el
# mismo título, plot_layout(guides="collect") las trataría como "la misma"
# leyenda e intentaría fusionarlas -- pero como las escalas numéricas son
# distintas entre Early y FIN, esa fusión falla en silencio y la leyenda
# desaparece. Con un título distinto por ola, quedan como dos leyendas
# separadas (mismo patrón que ya se usa para el ancho de arista).
etiqueta_centralidad <- function(medida, ola_lab) {
  sprintf("Centralidad %s\n(%s, CO+CR)", ola_lab, etiqueta_centralidad_base[[medida]])
}

centralidad_combinada <- function(tg, ola_sel, tipo = c("indegree", "eigenvector", "betweenness")) {
  tipo <- match.arg(tipo)
  g_f <- tg %>% activate(edges) %>% filter(ola == ola_sel) %>% as.igraph()

  cent <- switch(tipo,
    indegree    = strength(g_f, mode = "in", weights = E(g_f)$peso),
    eigenvector = eigen_centrality(g_f, directed = TRUE, weights = E(g_f)$peso)$vector,
    betweenness = betweenness(g_f, directed = TRUE, weights = 1 / E(g_f)$peso)
  )
  setNames(as.numeric(cent), V(g_f)$name)
}

# Cada ola tiene su propio título de leyenda de ancho Y de tamaño porque
# `peso` (y por lo tanto la centralidad que se calcula a partir de `peso`)
# mide cosas distintas en Early y en FIN (ver nota en 02_redes.R) -- por eso
# esas dos leyendas NO se fusionan con guides="collect" (si patchwork las
# viera "iguales" las fundiría, y acá no lo son). El color de tipo y la
# forma de género SÍ son la misma escala en los dos paneles, así que esas
# leyendas se comparten.
plot_ola_comparacion <- function(tg, ola_sel, titulo, nombre_ancho, xy,
                                  medida_centralidad = MEDIDA_CENTRALIDAD_FIG_A) {
  tg_f <- tg %>% activate(edges) %>% filter(ola == ola_sel)

  cent <- centralidad_combinada(tg, ola_sel, medida_centralidad)
  tg_f <- tg_f %>% activate(nodes) %>% mutate(centralidad = cent[name])

  layout_tg <- create_layout(tg_f, layout = "manual", x = xy[, 1], y = xy[, 2])

  ggraph(layout_tg) +
    geom_edge_fan(
      aes(color = tipo, width = peso), alpha = 0.75,
      arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
      end_cap = circle(3, "mm"), start_cap = circle(3, "mm"), strength = 0.6
    ) +
    scale_edge_color_manual(values = colores_tipo, name = "Tipo de vínculo",
                             labels = c(CO = "Afinidad (CO)", CR = "Influencia creativa (CR)")) +
    scale_edge_width_continuous(name = nombre_ancho, range = c(0.1, 1.2), limits = c(0, 1)) +
    geom_node_point(aes(shape = genero_norm, size = centralidad), color = color_nodo_a, stroke = 1.1) +
    scale_size_continuous(name = etiqueta_centralidad(medida_centralidad, ola_sel), range = c(3, 9)) +
    geom_node_text(aes(label = .data[[etiqueta_a]]), size = 2.9, color = "grey15",
                    repel = TRUE, nudge_y = 0.06) +
    scale_shape_manual(values = forma_genero, name = "Género") +
    labs(title = titulo) +
    theme_red
}

fig_a1 <- plot_ola_comparacion(tg_comparacion, "Early", "Early (promedio T1-T3)",
                                "Fuerza Early\n(fracción de olas)", xy_comp)
fig_a2 <- plot_ola_comparacion(tg_comparacion, "FIN", "FIN (última medición)",
                                "Fuerza FIN\n(rango de nominación)", xy_comp)

fig_a <- (fig_a1 + fig_a2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Afinidad e influencia creativa, Early vs. FIN",
    subtitle = "Mismo layout en ambos paneles; color de arista = tipo de vínculo; forma de nodo = género; ancho de arista y tamaño de nodo = fuerza y centralidad (escala propia de cada ola, no comparable entre paneles)",
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
                  plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey30"))
  ) &
  # legend.box="vertical": con 6 leyendas distintas (género, tipo, fuerza x2,
  # centralidad x2) puestas en una sola fila horizontal, las últimas quedan
  # más anchas que la página y se cortan sin avisar. En una columna vertical
  # (una leyenda por fila) todas quedan visibles completas.
  theme(legend.position = "bottom", legend.box = "vertical")

ggsave(file.path(RUTA_SALIDAS, "Fig_A_Early_vs_FIN.pdf"), fig_a, width = 13, height = 11)


############################################################
# FIGURA B — EVOLUCIÓN CON LAYOUT ESTABLE (T1 -> FIN, red CO)
############################################################
# Layout propio para esta figura (independiente del de la Figura A):
# calculado sobre la unión de CO_T1, CR_T1, CO_FIN y CR_FIN, para que la
# posición de cada docente sea la MISMA en las 4 olas. Tamaño de nodo por
# fuerza total (strength_total, ya calculada en 02_redes.R) y color por
# género. Compara directamente con Plot_Early.pdf / Plot_Early2.pdf,
# que usan un círculo sin significado estructural.
union_bin_evol <- (
  (lista_matrices$CO_T1 > 0) | (lista_matrices$CR_T1 > 0) |
  (lista_matrices$CO_FIN > 0) | (lista_matrices$CR_FIN > 0)
) * 1
g_union_evol <- graph_from_adjacency_matrix(union_bin_evol, mode = "directed")
xy <- layout_with_fr(g_union_evol)
rownames(xy) <- nodos_core
fuerza_por_ola <- tab_all %>%
  filter(node %in% nodos_core) %>%
  mutate(ola = case_when(
    grepl("_T1$",  graph) ~ "T1", grepl("_T2$",  graph) ~ "T2",
    grepl("_T3$",  graph) ~ "T3", grepl("_FIN$", graph) ~ "FIN"
  ), red = ifelse(grepl("^Wg[1-4]", graph), "CO", "CR")) %>%
  filter(red == "CO") %>%
  select(node, ola, strength_total)

olas <- c("T1" = "CO_T1", "T2" = "CO_T2", "T3" = "CO_T3", "FIN" = "CO_FIN")

# Rango GLOBAL de fuerza total (a través de las 4 olas), para que el tamaño de
# nodo sea comparable entre paneles y patchwork pueda fusionar una sola leyenda.
rango_fuerza <- range(fuerza_por_ola$strength_total, na.rm = TRUE)

plot_ola_estable <- function(ola_lab, mat_name, xy, nodos, rango) {
  mat <- lista_matrices[[mat_name]]
  edges <- which(mat > 0, arr.ind = TRUE)
  edges_df <- tibble(from = edges[, 1], to = edges[, 2])
  fz <- fuerza_por_ola %>% filter(ola == ola_lab)
  nodes_df <- tibble(name = nodos, genero = gen_core[nodos], label = lab_core[nodos]) %>%
    left_join(fz, by = c("name" = "node")) %>%
    mutate(strength_total = replace_na(strength_total, 0))

  tg <- tbl_graph(nodes = nodes_df, edges = edges_df, directed = TRUE)
  layout_tg <- create_layout(tg, layout = "manual", x = xy[, 1], y = xy[, 2])

  ggraph(layout_tg) +
    geom_edge_fan(color = "#8a8a8a", alpha = 0.6, width = 0.4,
                  arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
                  end_cap = circle(2.5, "mm"), start_cap = circle(2.5, "mm"), strength = 0.5) +
    geom_node_point(aes(shape = genero, size = strength_total), color = color_nodo, stroke = 1.1) +
    geom_node_text(aes(label = label), size = 2.2, color = "grey15", nudge_y = 0.08) +
    scale_shape_manual(values = forma_genero, name = "Género") +
    scale_size_continuous(name = "Fuerza total", range = c(2, 10), limits = rango) +
    labs(title = ola_lab) +
    theme_red
}

paneles_b <- imap(olas, ~ plot_ola_estable(.y, .x, xy, nodos_core, rango_fuerza))

fig_b <- wrap_plots(paneles_b, nrow = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Evolución de la red de afinidad (CO), layout fijo",
    subtitle = "Misma posición de nodo en las 4 olas (layout calculado sobre la unión CO+CR, T1+FIN) -- comparar con Plot_Early.pdf",
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
                  plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey30"))
  ) &
  theme(legend.position = "bottom")

ggsave(file.path(RUTA_SALIDAS, "Fig_B_Evolucion_Layout_Estable.pdf"), fig_b, width = 14, height = 5)


############################################################
# FIGURA C — HEATMAP DE CORRELACIONES QAP (respaldo de la fusión)
############################################################
# tabla mat_r / mat_p vienen cacheadas desde 03_mrqap.R (ya usan el
# mismo REPS_QAP que corriste). No se vuelve a correr QAP acá.
df_heat <- as.data.frame(mat_r) %>%
  rownames_to_column("Red_A") %>%
  pivot_longer(-Red_A, names_to = "Red_B", values_to = "r") %>%
  mutate(
    p = as.vector(mat_p),
    signif = case_when(p <= 0.001 ~ "***", p <= 0.01 ~ "**", p <= 0.05 ~ "*", p <= 0.1 ~ ".", TRUE ~ ""),
    Red_A = factor(Red_A, levels = rownames(mat_r)),
    Red_B = factor(Red_B, levels = rev(rownames(mat_r))),
    es_cruzada = xor(grepl("^CO", Red_A), grepl("^CO", Red_B))  # CO vs CR = la correlación que sustenta "fusión"
  )

fig_c <- ggplot(df_heat, aes(Red_A, Red_B, fill = r)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = paste0(round(r, 2), signif)), size = 2.9,
            color = ifelse(abs(df_heat$r) > 0.5, "white", "grey15")) +
  geom_tile(data = filter(df_heat, es_cruzada), fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_gradient2(low = "#1d7a8c", mid = "white", high = "#c9184a", midpoint = 0,
                        limits = c(-1, 1), name = "r (QAP)") +
  coord_fixed() +
  labs(
    title = "Correlaciones QAP entre las 8 redes (afinidad y creativa, 4 olas)",
    subtitle = "Recuadro negro = correlación CRUZADA (CO vs CR) -- la evidencia directa de fusión estructural  ·  *** p<.001 ** p<.01 * p<.05 . p<.1",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 8.5, color = "grey30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(file.path(RUTA_SALIDAS, "Fig_C_Heatmap_QAP.pdf"), fig_c, width = 8, height = 7)

message("05_visualizacion_alternativas.R completo. Revisa 02outputs/Fig_A_Early_vs_FIN.pdf, Fig_B_*.pdf, Fig_C_*.pdf")
