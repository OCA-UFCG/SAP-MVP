# =====================================================================
# utils/helpers.R - ATUALIZAR COM MAIS PALETAS
# =====================================================================

# =====================================================================
# FUNÇÕES AUXILIARES GERAIS
# =====================================================================

#' Operador null-coalescing
#' @param x Valor a ser testado
#' @param y Valor padrão se x for NULL
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Memoização para simplificação de geometrias
#' @param df Data frame espacial
#' @param tol_m Tolerância em metros
msimplify_memo <- memoise::memoise(function(df, tol_m = 500) {
  rmapshaper::ms_simplify(df, keep = 0.05, keep_shapes = TRUE)
})

#' Extrair apenas colunas numéricas de um dataframe
#' @param df Dataframe
#' @return Vetor com nomes das colunas numéricas
num_cols <- function(df) {
  if (inherits(df, "sf")) {
    df <- sf::st_drop_geometry(df)
  }
  nums <- sapply(df, function(col) {
    is.numeric(col) && !all(is.na(col))
  })
  names(df)[nums]
}

#' Garantir que o CRS é WGS84 (EPSG:4326)
#' @param df Dataframe espacial sf
#' @return Dataframe com CRS WGS84
ensure_wgs84 <- function(df) {
  if (!inherits(df, "sf")) return(df)
  
  crs_atual <- sf::st_crs(df)
  if (is.na(crs_atual) || crs_atual != sf::st_crs(4326)) {
    if (is.na(crs_atual)) {
      warning("CRS não definido. Assumindo WGS84 (EPSG:4326)")
      df <- sf::st_set_crs(df, 4326)
    } else {
      df <- sf::st_transform(df, 4326)
    }
  }
  df
}

#' Paletas de cores disponíveis para mapas
paletas <- c(
  # Viridis family
  "viridis", "plasma", "inferno", "magma", "cividis",
  
  # ColorBrewer sequenciais
  "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", 
  "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", 
  "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
  
  # ColorBrewer divergentes
  "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", 
  "RdYlGn", "Spectral"
)