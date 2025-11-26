# =====================================================================
# FUNÇÕES AUXILIARES - ELECTRE TRI-B
# =====================================================================

#' Operador %||% - retorna b se a for NULL
#' @export
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Limitar valores entre 0 e 1
#' 
#' @param z Vetor numérico
#' @return Vetor com valores entre 0 e 1
#' @export
clamp01 <- function(z) pmin(pmax(z, 0), 1)

#' Calcular ranges reais para normalização
#' 
#' @param df Data frame com os dados
#' @param criterios Vetor com nomes dos critérios
#' @return Lista nomeada com ranges [min, max] para cada critério
#' @export
calcular_ranges_real <- function(df, criterios) {
  setNames(
    lapply(criterios, function(cn) range(df[[cn]], na.rm = TRUE)),
    criterios
  )
}

#' Converter valores para escala unitária [0,1]
#' 
#' @param x Vetor de valores
#' @param cn Nome do critério
#' @param sense "benefit" ou "cost"
#' @param ranges Lista com ranges dos critérios
#' @return Vetor normalizado [0,1]
#' @export
to_unit <- function(x, cn, sense, ranges) {
  r <- ranges[[cn]]
  den <- r[2] - r[1]
  
  if (!is.finite(den) || den == 0) {
    return(rep(0, length(x)))
  }
  
  if (sense == "benefit") {
    clamp01((x - r[1]) / den)
  } else {
    clamp01((r[2] - x) / den)  # inverte automaticamente
  }
}

#' Converter de escala unitária [0,1] para valores reais
#' 
#' @param u Vetor normalizado [0,1]
#' @param cn Nome do critério
#' @param sense "benefit" ou "cost"
#' @param ranges Lista com ranges dos critérios
#' @return Vetor com valores reais
#' @export
to_real <- function(u, cn, sense, ranges) {
  r <- ranges[[cn]]
  den <- r[2] - r[1]
  
  if (!is.finite(den) || den == 0) {
    return(rep(r[1], length(u)))
  }
  
  u <- clamp01(u)
  
  if (sense == "benefit") {
    r[1] + u * den
  } else {
    r[2] - u * den
  }
}

#' Normalizar pesos (soma = 1)
#' 
#' @param pesos Vetor de pesos brutos
#' @return Vetor de pesos normalizados
#' @export
normalizar_pesos <- function(pesos) {
  s <- sum(pesos, na.rm = TRUE)
  
  if (!is.finite(s) || s <= 0) {
    pesos[] <- 1 / length(pesos)
  } else {
    pesos <- pesos / s
  }
  
  pesos
}

#' Formatar número com separador de milhares
#' 
#' @param x Número
#' @return String formatada
#' @export
formatar_numero <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",")
}
