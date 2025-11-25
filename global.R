# =====================================================================
# GLOBAL - Configurações e Funções Compartilhadas
# =====================================================================

# ---- Pacotes ----------------------------------------------------------
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(tidyr)
library(leaflet)
library(plotly)
library(DT)
library(memoise)
library(cachem)
library(promises)
library(future)
library(qs)
library(writexl)
library(htmltools)
library(colourpicker)
library(base64enc)
library(shinyjs)

# Configurar future para processamento assíncrono
if (inherits(future::plan(), "sequential")) future::plan(multisession)
Sys.setenv(APP_DATA_QS = "data/app_master_sf.qs")

# ---- Paleta OCA -------------------------------------------------------
COL <- list(
  primary   = "#c0a38b",  # Sand/Bege
  secondary = "#778c61",  # Sage
  accent    = "#383f2c",  # Brand (dark)
  success   = "#52854c",
  warning   = "#f39c12"
)

# ---- Tema bslib -------------------------------------------------------
theme_oca <- bs_theme(
  version = 5,
  preset = "shiny",
  primary = COL$primary,
  secondary = COL$secondary,
  success = COL$success,
  warning = COL$warning,
  base_font = font_google("Inter", wght = c(300, 400, 500, 600, 700)),
  heading_font = font_google("Inter", wght = c(600, 700)),
  code_font = font_google("Fira Code")
)

# ---- Constantes -------------------------------------------------------
paletas <- c("viridis", "plasma", "magma", "inferno", "cividis", "Turbo")

# ---- Funções Auxiliares -----------------------------------------------

# Operador coalescente
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# Garantir CRS WGS84
ensure_wgs84 <- function(x) {
  if (is.na(sf::st_crs(x))) return(sf::st_set_crs(x, 4326))
  if (sf::st_crs(x)$epsg != 4326) sf::st_transform(x, 4326) else x
}

# Identificar colunas numéricas
num_cols <- function(df) {
  names(df)[vapply(df, is.numeric, logical(1))]
}

# Identificar colunas categóricas padrão
cat_cols_default <- function(df) {
  c(
    intersect(c("NM_REGIAO", "NM_UF", "NM_RGINT", "NM_RGI", "NM_CONCURB", "ASD_PAB"), names(df)),
    grep("^X_|^SEMI_21$|^AAS_21$", names(df), value = TRUE)
  )
}

# Range de coluna numérica
col_range <- function(df, col) {
  rng <- range(df[[col]], na.rm = TRUE)
  if (any(!is.finite(rng))) c(0, 1) else rng
}

# Cache para simplificação de geometrias
.simplify_cache <- cache_mem(max_size = 128 * 1024^2)

# Simplificar geometrias (memoizado)
msimplify_memo <- memoise(function(sfobj, tol_m = 300) {
  sfobj |>
    sf::st_transform(3857) |>
    sf::st_simplify(dTolerance = tol_m, preserveTopology = TRUE) |>
    sf::st_transform(4326)
}, cache = .simplify_cache)

# ---- Carregar Dados ---------------------------------------------------
if (!exists("mun_asd", inherits = TRUE)) {
  qs_path <- Sys.getenv("APP_DATA_QS", unset = "app_master_sf.qs")
  
  if (file.exists(qs_path)) {
    message("[INFO] Carregando dados de: ", qs_path)
    
    base <- qs::qread(qs_path)
    
    if (inherits(base, "sf")) {
      mun_asd <- base
      message("[OK] Dados SF carregados com sucesso")
    } else if ("geom" %in% names(base)) {
      if (inherits(base$geom, "sfc")) {
        sf::st_geometry(base) <- "geom"
        mun_asd <- base
      } else if (is.character(base$geom)) {
        base$geom <- sf::st_as_sfc(base$geom, crs = 4326)
        mun_asd <- sf::st_as_sf(base)
      } else {
        stop("[ERRO] Coluna 'geom' existe mas não é sfc nem WKT")
      }
      message("[OK] Dados convertidos para SF")
    } else {
      stop("[ERRO] Objeto não é SF válido e não possui coluna 'geom'")
    }
    
    message("[INFO] Registros carregados: ", nrow(mun_asd))
  } else {
    stop("[ERRO] Arquivo QS não encontrado: ", qs_path)
  }
}