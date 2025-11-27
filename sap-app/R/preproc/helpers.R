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