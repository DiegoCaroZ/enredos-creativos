############################################################
# 05B_FIGURA1_INDEGREE_TIPO.R — Figura 1 del artículo (oficial desde 23-sep-2026)
# Taller de Teatro y Creatividad Docente
#
# ESTADO (23-sep-2026): Diego revisó esta variante (antes guardada como
# 05b_prueba_CO_size_CR_alpha.R / Fig_A_PRUEBA_CO_size_CR_alpha.pdf) y pidió
# reportarla en el artículo -- es la que ahora se inserta como Figura 1 en
# el manuscrito. 05_visualizacion_alternativas.R / Fig_A_Early_vs_FIN.pdf
# (centralidad combinada CO+CR, sin peso_orden ni filtro de densidad) NO se
# borra ni se modifica -- queda como referencia/robustez, ya no es la
# figura que se reporta. El script de prueba original
# (05b_prueba_CO_size_CR_alpha.R) también se conserva intacto para
# trazabilidad de cómo se llegó a esta versión.
#
# POR QUÉ ESTA VARIANTE (en vez de la centralidad combinada CO+CR de la
# Figura A "oficial" anterior):
#   Diego pidió separar, en el tamaño y el color del nodo, el in-degree de
#   CADA red por separado (en vez de la centralidad combinada CO+CR que
#   usaba esa versión anterior):
#     - TAMAÑO de nodo   = in-degree ponderado de la red CO (afinidad)
#     - COLOR de RELLENO = in-degree ponderado de la red CR (influencia
#       creativa) -- más saturado (dorado intenso) = mayor in-degree CR,
#       más pálido (casi blanco) = menor in-degree CR.
#
# Primera versión (iteración anterior de esta misma prueba) usaba
# transparencia (alpha) sobre nodos sin relleno. Diego pidió dejar el
# CONTORNO fijo y mover el color al RELLENO -- para eso los nodos pasan de
# shapes sin relleno (0/1/2) a shapes CON relleno propio (21/22/24, círculo/
# cuadrado/triángulo), que separan `color` (contorno, fijo en gris) de
# `fill` (relleno, con la escala de in-degree CR). El degradé va de un
# dorado muy pálido (`#f7ecc9`, in-degree CR bajo) al mismo dorado que ya
# identifica a CR en las aristas (`colores_tipo["CR"]`, in-degree CR alto),
# así el color del nodo queda asociado a la misma red que representa.
#
# UMBRAL_PESO (agregado 23-sep): con las dos redes juntas en cada panel el
# grafo es casi completo (82 aristas en Early, 76 en FIN, para 10 nodos) y
# se vuelve un "ovillo" donde no se distingue dónde hay más vínculos de cada
# tipo. Para despejarlo, se dibujan solo las aristas con peso >= 2/3. El
# IN-DEGREE de tamaño/color de nodo NO cambia con este filtro -- se sigue
# calculando sobre TODAS las aristas (incluidas las débiles que ya no se
# dibujan), para no perder información real solo porque decidimos no
# dibujarla. El valor vive en UMBRAL_PESO, más abajo -- ponlo en 0 para
# volver a ver todas las aristas.
#
# peso_orden (agregado 23-sep, segunda vuelta): Diego notó que `peso` en
# Early es solo la fracción de olas T1-T3 con vínculo presente -- NO
# considera en qué ORDEN se nominó a cada colega dentro de cada ola, aunque
# esa información sí existe en los datos crudos (la misma columna `weight`
# que ya se usaba para calcular la fuerza de FIN). `peso_orden` -- agregado
# en 02_redes.R -- aplica la MISMA transformación rango->fuerza que ya se
# usaba solo para FIN a cada ola T1/T2/T3 por separado, y las promedia con
# el mismo promedio NA-aware que ya se usaba para `peso`. Resultado: para
# Early, peso_orden queda mucho más granular que peso (compara si presente
# en 2 de 3 olas siendo siempre 1ª nominación vs. siendo siempre 3ª
# nominación -- con `peso` esos dos casos se ven IGUAL, con `peso_orden`
# no). Para FIN, peso_orden == peso (FIN ya era orden de nominación desde
# el principio). Esta figura usa peso_orden en vez de peso -- tanto para el
# ancho/opacidad de arista como para el in-degree de tamaño/color de
# nodo -- para que todo el gráfico hable el mismo idioma. OJO: sigue siendo
# una variable de VISUALIZACIÓN -- el MRQAP reportado usa `peso` (la
# fracción binaria), no se tocó ningún modelo.
#
# UMBRAL_PESO se RECALIBRÓ de 2/3 a 0.5 al pasar a peso_orden: exigir 2/3
# de peso_orden en Early significa "consistentemente entre las 2 primeras
# nominaciones en casi todas las olas asistidas" -- un estándar mucho más
# alto que el 2/3 de `peso` (que solo pedía presencia, cualquier orden), y
# dejaba pasar solo 13 de 82 aristas (vs. 24 con `peso`). Con 0.5 quedan 31
# de 82 en Early y 39 de 76 en FIN -- densidad más parecida a la que ya
# habíamos calibrado antes. Ajusta a gusto.
#
# ENTRADA:  01analysis/.cache/01_limpieza.rds, 02_redes.rds
# SALIDA:   02outputs/Fig_1_Afinidad_Influencia_indegree_tipo.pdf (y .png,
#           para insertar en el manuscrito)
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
  library(scales)
  library(ggrepel)
})

USAR_NOMBRES_REALES <- TRUE  # los nombres de 00raw-data/Edge2.xlsx ya son seudónimos (24-nov-2026)
# Etiquetas de ola para leyendas (el código interno sigue usando "Early"/"FIN")
ETIQUETA_OLA <- c(Early = "inicial", FIN = "final")
UMBRAL_PESO <- 0.5  # solo se dibujan aristas con peso_orden >= este valor (ver nota arriba); 0 = mostrar todas

RUTA_SALIDAS <- here::here("02outputs")
RUTA_CACHE   <- here::here("01analysis", ".cache")

c01 <- readRDS(file.path(RUTA_CACHE, "01_limpieza.rds"))
c02 <- readRDS(file.path(RUTA_CACHE, "02_redes.rds"))
list2env(c01, envir = environment())
list2env(c02, envir = environment())

set.seed(2026)  # mismo seed que 05_visualizacion_alternativas.R -> mismo layout

etiqueta_a   <- if (USAR_NOMBRES_REALES) "name" else "id_anon"
# Paleta colorblind-friendly (30-sep-2026, ajustada el mismo día a pedido
# de Diego -- primero violeta/naranja sobre azul/naranja, luego invertida
# CO<->CR dentro de ese mismo par): el verde/café original
# (darkolivegreen4/darkgoldenrod4) quedaba difícil de distinguir para
# protanopía/deuteranopía -- ambos caen cerca de la misma línea de
# confusión rojo-verde. Violeta/naranja tiene separación muy por encima
# del umbral de 8 (ΔE CVD 29.5, ΔE visión normal 37.6), sin warnings de
# contraste; ver dataviz skill, scripts/validate_palette.js. CR (naranja)
# ahora coincide con el tono que ya identificaba a CR en la iteración
# azul/naranja anterior.
colores_tipo <- c(CO = "#eb6834", CR = "#4a3aa7")
color_contorno <- "grey25"  # contorno FIJO -- el color ahora va en el relleno
forma_genero <- c(
  "Femenino"        = 21,  # círculo con relleno + contorno propio
  "Masculino"       = 24,  # triángulo con relleno + contorno propio
  "Otro/No binario" = 22   # cuadrado con relleno + contorno propio
)

theme_red <- theme_graph(base_family = "sans") +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )

tg_comparacion <- as_tbl_graph(g_comparacion)

# Mismo layout compartido que la Figura A oficial (sobre la unión de todos
# los vínculos Early+FIN, CO+CR), para que las dos figuras sean comparables
# nodo a nodo.
union_bin_comp <- ((CO_early > 0) | (CR_early > 0) | (CO_FIN_fuerza > 0) | (CR_FIN_fuerza > 0)) * 1
g_union_comp <- graph_from_adjacency_matrix(union_bin_comp, mode = "directed")
xy_comp <- layout_with_fr(g_union_comp)
rownames(xy_comp) <- nodos_core
xy_comp <- xy_comp[V(g_comparacion)$name, ]

# In-degree ponderado de UN SOLO tipo de vínculo (CO o CR), sobre el grafo
# filtrado por ola -- a diferencia de centralidad_combinada() en
# 05_visualizacion_alternativas.R (que junta CO+CR), acá se filtra también
# por `tipo`, así que CO y CR quedan como dos medidas independientes. Usa
# `peso_orden` (no `peso`) para que el tamaño/color de nodo hable el mismo
# idioma que el ancho/opacidad de arista (ver nota arriba).
indegree_por_tipo <- function(tg, ola_sel, tipo_sel) {
  g_f <- tg %>% activate(edges) %>% filter(ola == ola_sel, tipo == tipo_sel) %>% as.igraph()
  setNames(strength(g_f, mode = "in", weights = E(g_f)$peso_orden), V(g_f)$name)
}

# Título de leyenda con la ola incluida (mismo motivo que en el script
# principal: si Early y FIN compartieran título, guides="collect" trataría
# de fusionar dos escalas numéricas distintas y la leyenda desaparecería).
plot_ola_prueba <- function(tg, ola_sel, titulo, nombre_ancho, xy) {
  # El in-degree (tamaño/color de nodo) se calcula ANTES de este filtro,
  # sobre `tg` completo -- así que declutter de aristas no cambia esa
  # medida, solo qué aristas se dibujan. Se filtra por peso_orden (no
  # peso): son iguales en FIN, pero en Early peso_orden es la versión que
  # de verdad se dibuja.
  tg_f <- tg %>% activate(edges) %>% filter(ola == ola_sel, peso_orden >= UMBRAL_PESO)

  co_ind <- indegree_por_tipo(tg, ola_sel, "CO")
  cr_ind <- indegree_por_tipo(tg, ola_sel, "CR")
  tg_f <- tg_f %>% activate(nodes) %>%
    mutate(co_indegree = co_ind[name], cr_indegree = cr_ind[name])

  layout_tg <- create_layout(tg_f, layout = "manual", x = xy[, 1], y = xy[, 2])

  ggraph(layout_tg) +
    geom_edge_fan(
      # alpha también sigue a peso_orden (antes era fijo en 0.55): entre
      # las aristas que sobreviven el umbral, las más fuertes quedan más
      # opacas -- refuerza el mismo mensaje que el ancho, en vez de competir
      # con él.
      aes(color = tipo, width = peso_orden, alpha = peso_orden),
      arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
      end_cap = circle(7, "mm"), start_cap = circle(7, "mm"), strength = 0.6
    ) +
    scale_edge_color_manual(values = colores_tipo, name = "Tipo de vínculo",
                             labels = c(CO = "Afinidad (CO)", CR = "Reconocimiento creativo (CR)")) +
    # `limits` = c(UMBRAL_PESO, 1) por la misma razón de la iteración
    # anterior: como no se dibuja nada por debajo del umbral, dejar el 0 en
    # la escala desperdicia rango visual. A diferencia de la iteración
    # anterior (que uso `peso`, con solo 2-3 valores posibles), peso_orden
    # SÍ es lo bastante granular en Early como para que una escala continua
    # tenga sentido -- ya no hace falta discretizarla a mano.
    scale_edge_width_continuous(name = nombre_ancho, range = c(0.3, 1.8), limits = c(UMBRAL_PESO, 1)) +
    scale_edge_alpha_continuous(range = c(0.4, 0.95), limits = c(UMBRAL_PESO, 1), guide = "none") +
    geom_node_point(
      aes(shape = genero_norm, size = co_indegree, fill = cr_indegree),
      color = color_contorno, stroke = 0.8
    ) +
    # scale_radius() en vez de scale_size_continuous(): scale_size_continuous
    # mapea el valor al ÁREA del punto (raíz cuadrada del valor -> percepción
    # "correcta" pero diferencias chicas casi no se notan). scale_radius()
    # mapea linealmente al RADIO, lo que amplifica visualmente el contraste
    # entre in-degree alto y bajo (a costa de exagerar un poco el área
    # relativa) -- justo lo que pediste. El rango también se ensanchó
    # (1-13 en vez de 2-10) para acentuar más la diferencia.
    scale_radius(name = paste0("In-degree CO (", ETIQUETA_OLA[[ola_sel]], ")\ntamaño de nodo"), range = c(1, 13)) +
    scale_fill_gradient(name = paste0("In-degree CR (", ETIQUETA_OLA[[ola_sel]], ")\ncolor de relleno"),
                         low = "#e8e6f5", high = colores_tipo[["CR"]]) +
    geom_node_text(aes(label = .data[[etiqueta_a]]), size = 2.9, color = "grey15",
                    repel = TRUE, nudge_y = 0.06) +
    scale_shape_manual(values = forma_genero, name = "Género") +
    labs(title = titulo) +
    theme_red
}

fig_1a <- plot_ola_prueba(tg_comparacion, "Early", "Red inicial (T1-T3)",
                           "Fuerza red inicial\n(orden de nominación incluido)", xy_comp)
fig_1b <- plot_ola_prueba(tg_comparacion, "FIN", "Red final (T4)",
                           "Fuerza red final\n(orden de nominación)", xy_comp)

fig_1 <- (fig_1a + fig_1b) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Afinidad y reconocimiento creativo: in-degree por tipo de vínculo, red inicial vs. red final",
    subtitle = "Mismo layout en ambos paneles; forma de nodo = género; tamaño de nodo = in-degree CO, color de relleno = in-degree CR; ancho/opacidad de arista = fuerza ponderada por orden de nominación (peso_orden)",
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
                  plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey30"))
  ) &
  theme(legend.position = "bottom", legend.box = "vertical")

ggsave(file.path(RUTA_SALIDAS, "Fig_1_Afinidad_Influencia_indegree_tipo.pdf"), fig_1, width = 13, height = 13)

# Versión SIN el título/subtítulo de plot_annotation() (paneles + leyenda
# solamente) -- es la que se inserta en el manuscrito, porque el artículo ya
# trae su propio pie de figura ("Figura 1. ...") justo debajo; iba a quedar
# duplicado si además arrastraba un título grande dentro de la imagen (así
# se dejó también la versión anterior de esta figura que ya estaba en el
# manuscrito). NO usa ggsave a PNG directo -- el device PNG de este entorno
# rompe los acentos/eñes del texto (tildes se ven como ".."); se exporta a
# PDF y se rasteriza aparte con pdftoppm, que sí preserva el texto bien.
fig_1_sin_titulo <- (fig_1a + fig_1b) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "vertical")
ggsave(file.path(RUTA_SALIDAS, "Fig_1_Afinidad_Influencia_indegree_tipo_sin_titulo.pdf"),
       fig_1_sin_titulo, width = 13, height = 12)

message("05b_Figura1_indegree_tipo.R completo. Revisa 02outputs/Fig_1_Afinidad_Influencia_indegree_tipo.pdf (Figura 1 del artículo, con título) y la versión _sin_titulo.pdf (para insertar en el manuscrito; rasterizar con pdftoppm, no con ggsave a PNG).")
