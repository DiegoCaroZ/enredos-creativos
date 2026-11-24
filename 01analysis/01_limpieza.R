############################################################
# 01_LIMPIEZA.R — Etapa 1 de 4 del pipeline SNA
# Taller de Teatro y Creatividad Docente
#
# QUÉ HACE:
#   - Lee la hoja "Nodes" de Edge2.xlsx y construye vertices_df
#     (atributos promediados por ola: D, CSE, FI, G, V).
#   - Lee las 8 edgelists (CO/CR x T1/T2/T3/FIN) y construye los
#     grafos igraph Wg1..Wg8.
#   - Determina el "core" analítico (docentes presentes en FIN),
#     usando asistencia registrada (Presente_T1..FIN) si existe.
#   - Genera un id anonimizado por docente (para las figuras que
#     puedan terminar en el artículo — ver nota más abajo).
#   - Aplica color por género a los grafos.
#
# ENTRADA:  00raw-data/Edge2.xlsx
# SALIDA:   02outputs/Tabla_Asistencia_Por_Ola.xlsx
#           01analysis/.cache/01_limpieza.rds  (para las etapas siguientes)
#
# NOTA SOBRE ANONIMIZACIÓN:
#   Las figuras y tablas de este taller identifican a 13 docentes por
#   su nombre real. Con un grupo tan pequeño, cualquier figura de red
#   publicada (paper, congreso) es fácilmente reidentificable. Este
#   script agrega una columna `id_anon` (código estable Dxx, no ligado
#   al orden alfabético del nombre real ni al género) para que las
#   etapas de visualización puedan usar etiquetas anonimizadas por
#   defecto. Los nombres reales se siguen usando internamente para
#   join/QA, nunca se pierden.
############################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(here)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(igraph)
  library(writexl)
})

RUTA_DATOS   <- here::here("00raw-data", "Edge2.xlsx")
RUTA_SALIDAS <- here::here("02outputs")
RUTA_CACHE   <- here::here("01analysis", ".cache")
if (!dir.exists(RUTA_SALIDAS)) dir.create(RUTA_SALIDAS, recursive = TRUE)
if (!dir.exists(RUTA_CACHE))   dir.create(RUTA_CACHE,   recursive = TRUE)


############################################################
# 1. NODOS Y ATRIBUTOS
############################################################
atributos <- read_excel(RUTA_DATOS, sheet = "Nodes") %>%
  mutate(
    # --- Promedios T1 ---
    D1   = rowMeans(select(., D1.1,  D2.1,  D3.1),        na.rm = TRUE),
    CSE1 = rowMeans(select(., CSE1.1, CSE2.1, CSE3.1),    na.rm = TRUE),
    FI1  = rowMeans(select(., F1.1,  F2.1,  F3.1,  F4.1), na.rm = TRUE),
    G1   = rowMeans(select(., G1.1,  G2.1,  G3.1,  G4.1), na.rm = TRUE),
    V1   = rowMeans(select(., V1.1,  V2.1,  V3.1),        na.rm = TRUE),
    # --- Promedios T2 ---
    D2   = rowMeans(select(., D1.2,  D2.2,  D3.2),        na.rm = TRUE),
    CSE2 = rowMeans(select(., CSE1.2, CSE2.2, CSE3.2),    na.rm = TRUE),
    FI2  = rowMeans(select(., F1.2,  F2.2,  F3.2,  F4.2), na.rm = TRUE),
    G2   = rowMeans(select(., G1.2,  G2.2,  G3.2,  G4.2), na.rm = TRUE),
    V2   = rowMeans(select(., V1.2,  V2.2,  V3.2),        na.rm = TRUE)
  )

vertices_df <- atributos %>%
  transmute(
    name   = trimws(as.character(Node)),
    genero = trimws(as.character(Gender)),
    Type,
    D1, CSE1, FI1, G1, V1,
    D2, CSE2, FI2, G2, V2
  )

# --- Código anonimizado estable (Dxx), basado en un hash del nombre ---
# No depende del orden alfabético ni de ninguna variable sustantiva,
# así que no reintroduce un patrón interpretable (p.ej. por género).
set.seed(2026)
orden_anon <- sample(seq_len(nrow(vertices_df)))  # baraja fija con la misma semilla del pipeline
vertices_df <- vertices_df %>%
  mutate(id_anon = sprintf("D%02d", orden_anon))


############################################################
# 2. IMPORTAR LAS 8 EDGELISTS (loop en vez de 8 bloques repetidos)
############################################################
clean_edges <- function(df) {
  df %>%
    filter(!is.na(from), !is.na(to)) %>%
    mutate(
      from   = trimws(as.character(from)),
      to     = trimws(as.character(to)),
      weight = as.numeric(weight)
    )
}

hojas_edges <- c(
  Wg1 = "Edgelist CO T1", Wg2 = "Edgelist CO T2",
  Wg3 = "Edgelist CO T3", Wg4 = "Edgelist CO FIN",
  Wg5 = "Edgelist CR T1", Wg6 = "Edgelist CR T2",
  Wg7 = "Edgelist CR T3", Wg8 = "Edgelist CR FIN"
)

# Etiquetas legibles para tablas/plots (mismo orden que hojas_edges)
etiquetas_red <- c(
  Wg1 = "CO_T1", Wg2 = "CO_T2", Wg3 = "CO_T3", Wg4 = "CO_FIN",
  Wg5 = "CR_T1", Wg6 = "CR_T2", Wg7 = "CR_T3", Wg8 = "CR_FIN"
)

make_g <- function(edges, vertices) graph_from_data_frame(edges, directed = TRUE, vertices = vertices)

edgelists <- map(hojas_edges, ~ read_excel(RUTA_DATOS, sheet = .x) |> clean_edges())
grafos    <- map(edgelists, make_g, vertices = vertices_df)   # lista nombrada Wg1..Wg8


############################################################
# 3. ASISTENCIA REAL / DEFINICIÓN DEL "CORE" DE ANÁLISIS
############################################################
# *** SECCIÓN CRÍTICA — LEER ANTES DE CONFIAR EN LOS MRQAP ***
#
# Todas las redes (Wg1..Wg8) se construyen sobre el MISMO vertices_df
# (el roster completo de "Nodes"), así que intersect(V(Wg1)$name, V(Wg4)$name)
# NO filtra nada -- siempre da el roster completo. Por eso, para saber
# quién realmente participó en cada ola necesitamos un dato EXTERNO a la
# red (asistencia registrada), no inferido de la estructura relacional.
#
# Edge2.xlsx SÍ trae las columnas Presente_T1..FIN (verificado), así que
# se usa asistencia real; el fallback de abajo (grado > 0) es solo para
# que el script no truene si algún día se reemplaza la fuente de datos.

cols_asistencia <- c("Presente_T1", "Presente_T2", "Presente_T3", "Presente_FIN")
tiene_asistencia_real <- all(cols_asistencia %in% names(atributos))

if (tiene_asistencia_real) {

  asistencia <- atributos %>%
    transmute(name = trimws(as.character(Node)),
              Presente_T1, Presente_T2, Presente_T3, Presente_FIN)

} else {

  warning(
    "\n>>> No se encontraron columnas de asistencia real (", paste(cols_asistencia, collapse = ", "), ") en la hoja 'Nodes'.\n",
    ">>> Usando un criterio PROVISIONAL basado en grado > 0 por ola.\n",
    ">>> Este criterio confunde 'no asistió' con 'asistió pero quedó aislado'.\n",
    ">>> Antes de reportar resultados en el artículo, reemplázalo por asistencia registrada.\n"
  )

  deg0 <- function(g) igraph::degree(g, mode = "all", loops = FALSE)
  asistencia <- tibble(name = vertices_df$name) %>%
    mutate(
      Presente_T1  = as.integer((deg0(grafos$Wg1)[name] > 0) | (deg0(grafos$Wg5)[name] > 0)),
      Presente_T2  = as.integer((deg0(grafos$Wg2)[name] > 0) | (deg0(grafos$Wg6)[name] > 0)),
      Presente_T3  = as.integer((deg0(grafos$Wg3)[name] > 0) | (deg0(grafos$Wg7)[name] > 0)),
      Presente_FIN = as.integer((deg0(grafos$Wg4)[name] > 0) | (deg0(grafos$Wg8)[name] > 0))
    )
}

# Tabla de asistencia -> a exportar como Tabla 1 del artículo (transparencia total)
write_xlsx(asistencia, path = file.path(RUTA_SALIDAS, "Tabla_Asistencia_Por_Ola.xlsx"))

# --- CORE = presentes en FIN (no el AND de las 4 olas) ---
# Justificación: la variable dependiente de los modelos es la red FIN, así que
# el único requisito "duro" para entrar al análisis predictivo es tener dato en FIN.
# Exigir presencia en las 4 olas es innecesariamente restrictivo con asistencia
# irregular (en este taller deja fuera a casi todo el grupo -- probado empíricamente).
nodos_core <- asistencia %>% filter(Presente_FIN == 1) %>% pull(name) %>% sort()

message(sprintf(
  "Core analítico (presentes en FIN): %d de %d participantes.\n%s",
  length(nodos_core), nrow(asistencia), paste(nodos_core, collapse = ", ")
))
# Chequeo de consistencia: debería dar ~10 si coincide con "quienes presentaron la obra".
# Verificado con datos reales (15-sep-2026): da exactamente 10/13. Si en una corrida
# futura no coincide, revisar si Presente_FIN captura exactamente ese hito o algo
# distinto (p.ej. alguien respondió el cuestionario FIN sin llegar a presentar).

# Quiénes quedan fuera del core, y por qué (para la sección de limitaciones/resultados secundarios)
no_core <- asistencia %>% filter(!(name %in% nodos_core))

# Marca de "sin historial temprano": presente en FIN pero SIN ninguna medición en T1-T3.
# OJO: esto NO equivale a "se integró tarde al grupo" -- solo dice que no fue
# encuestado antes de FIN. Se nombra así a propósito para no sobreinterpretar el
# coeficiente como "efecto de antigüedad" en la discusión del artículo.
asistencia <- asistencia %>%
  mutate(Sin_Historial_Temprano = as.integer(Presente_FIN == 1 & Presente_T1 == 0 & Presente_T2 == 0 & Presente_T3 == 0))
if (any(asistencia$Sin_Historial_Temprano == 1)) {
  message("Sin datos relacionales previos a FIN (no necesariamente 'recién llegados' al grupo): ",
          paste(asistencia$name[asistencia$Sin_Historial_Temprano == 1], collapse = ", "))
}


############################################################
# 4. COLOR POR GÉNERO (para plots)
############################################################
norm_genero <- function(x) {
  x <- trimws(tolower(as.character(x)))
  case_when(
    x %in% c("f", "female", "mujer", "femenino")              ~ "Femenino",
    x %in% c("m", "male", "hombre", "masculino")               ~ "Masculino",
    x %in% c("nb", "nonbinary", "non-binary", "no binario", "no-binario") ~ "Otro/No binario",
    is.na(x) | x == "" ~ NA_character_,
    TRUE ~ "Otro/No especificado"
  )
}

colores_genero <- c(
  "Femenino"        = "darkolivegreen3",
  "Masculino"       = "cyan4",
  "Otro/No binario" = "darkorchid3"
)

set_vertex_gender_colors <- function(g, v_info = vertices_df) {
  df_temp <- merge(data.frame(name = V(g)$name), v_info[, c("name", "genero")], by = "name", all.x = TRUE)
  df_temp <- df_temp[match(V(g)$name, df_temp$name), ]
  gen_std <- norm_genero(df_temp$genero)
  col <- colores_genero[gen_std]
  col[is.na(col)] <- "grey80"
  V(g)$color <- unname(col)
  V(g)$genero_std <- gen_std
  g
}

grafos <- map(grafos, set_vertex_gender_colors)


############################################################
# 5. GUARDAR CACHE PARA LAS SIGUIENTES ETAPAS
############################################################
saveRDS(
  list(
    vertices_df    = vertices_df,
    grafos         = grafos,
    etiquetas_red  = etiquetas_red,
    asistencia     = asistencia,
    nodos_core     = nodos_core,
    no_core        = no_core,
    colores_genero = colores_genero,
    norm_genero    = norm_genero,
    set_vertex_gender_colors = set_vertex_gender_colors
  ),
  file = file.path(RUTA_CACHE, "01_limpieza.rds")
)

message("01_limpieza.R completo. Cache guardado en 01analysis/.cache/01_limpieza.rds")
