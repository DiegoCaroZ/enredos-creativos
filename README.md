# Enredos creativos — código y datos de replicación

Código y datos para replicar los análisis de *"Enredos creativos: afinidad y reconocimiento creativo en un taller de teatro para docentes"* (Caro-Zúñiga, Muñoz-Salinas y Jeria; Fondecyt Iniciación N° 11240401).

## Contenido
- `00raw-data/Edge2.xlsx`: aristas (afinidad = CO, reconocimiento creativo = CR) por ola T1–T4 y atributos de nodos. **Todos los nombres son seudónimos** que mantienen el género. Los participantes otorgaron consentimiento informado.
- `01analysis/`: pipeline en R. Corre `00_run_pipeline.R` desde la raíz del proyecto (usa `here`).
  - `01_limpieza.R` → `02_redes.R` → `03*_mrqap*.R` (QAP/MRQAP, 5.000 permutaciones, semi-partialling de Dekker) → `03g_tablas_oficiales.R` (tablas del artículo) → `04/05*` (figuras).
- `02outputs/`: Figura 1 (PDF y PNG) y salidas generadas.

## Requisitos
R ≥ 4.3 con: dplyr, purrr, tibble, tidyr, readxl, writexl, here, sna, igraph, ggraph, tidygraph, ggplot2, patchwork, ggrepel, scales.
