# =====================================================================
# FUNÇÕES UTILITÁRIAS - SISTEMA DE FILTROS
# =====================================================================

#' Aplicar filtros a um dataframe (função principal - substitui aplicar_filtro e aplicar_filtros_sequencial)
#' 
#' @param df Dataframe a ser filtrado
#' @param filtros Lista de filtros a aplicar
#' @return Dataframe filtrado
aplicar_filtros <- function(df, filtros) {
  if (length(filtros) == 0) return(df)
  
  for (filtro in filtros) {
    tryCatch({
      # Verificar se é filtro composto
      if (!is.null(filtro$tipo) && filtro$tipo == "composto") {
        expr <- rlang::parse_expr(filtro$expressao)
        df <- dplyr::filter(df, !!expr)
        next
      }
      
      # Filtro simples - validações
      if (is.null(filtro$campo) || is.null(filtro$operador) || is.null(filtro$valor)) {
        warning("Filtro incompleto encontrado, pulando...")
        next
      }
      
      campo <- filtro$campo
      operador <- filtro$operador
      valor <- filtro$valor
      
      # Verificar se a coluna existe no dataframe
      if (!campo %in% names(df)) {
        warning("Coluna '", campo, "' não encontrada no dataframe, pulando filtro...")
        next
      }
      
      # Aplicar filtro baseado no operador
      if (operador == "in") {
        if (length(valor) > 0) {
          df <- dplyr::filter(df, .data[[campo]] %in% valor)
        }
      } else if (operador == "between") {
        if (length(valor) == 2 && !any(is.na(valor))) {
          df <- dplyr::filter(df, dplyr::between(.data[[campo]], valor[1], valor[2]))
        }
      } else if (operador %in% c("==", ">", ">=", "<", "<=")) {
        if (length(valor) > 0 && !is.na(valor[1])) {
          expr <- switch(operador,
                         "==" = rlang::expr(.data[[campo]] == !!valor),
                         ">" = rlang::expr(.data[[campo]] > !!valor),
                         ">=" = rlang::expr(.data[[campo]] >= !!valor),
                         "<" = rlang::expr(.data[[campo]] < !!valor),
                         "<=" = rlang::expr(.data[[campo]] <= !!valor)
          )
          if (!is.null(expr)) {
            df <- dplyr::filter(df, !!expr)
          }
        }
      }
      
    }, error = function(e) {
      warning("Erro ao aplicar filtro: ", e$message)
    })
  }
  
  df
}

#' Formatar valor para exibição
#' 
#' @param valor Valor a ser formatado
#' @param operador Operador usado no filtro
#' @return String formatada
formatar_valor_filtro <- function(valor, operador) {
  if (operador == "between") {
    paste0(valor[1], " a ", valor[2])
  } else if (operador == "in") {
    if (length(valor) > 3) {
      paste0(paste(valor[1:3], collapse = ", "), " (+ ", length(valor) - 3, " mais)")
    } else {
      paste(valor, collapse = ", ")
    }
  } else {
    as.character(valor)
  }
}

#' Criar novo filtro simples
#' 
#' @param campo Nome do campo
#' @param operador Operador (==, >, <, etc)
#' @param valor Valor ou vetor de valores
#' @return Lista com estrutura de filtro
criar_filtro_simples <- function(campo, operador, valor) {
  list(
    tipo = "simples",
    campo = campo,
    operador = operador,
    operador_nome = traduzir_operador(operador),
    valor = valor,
    valor_texto = formatar_valor_filtro(valor, operador)
  )
}

#' Criar novo filtro composto
#' 
#' @param expressao Expressão R como string
#' @return Lista com estrutura de filtro
criar_filtro_composto <- function(expressao) {
  list(
    tipo = "composto",
    expressao = expressao,
    operador_nome = "expressão customizada",
    valor_texto = expressao,
    campo = NULL,
    operador = NULL,
    valor = NULL
  )
}

#' Traduzir operador para linguagem natural
#' 
#' @param operador Operador (==, >, <, etc)
#' @return String traduzida
traduzir_operador <- function(operador) {
  switch(operador, 
    "==" = "é igual a", 
    ">" = "é maior que",
    ">=" = "é maior ou igual a", 
    "<" = "é menor que", 
    "<=" = "é menor ou igual a", 
    "between" = "está entre",
    "in" = "está entre as opções",
    operador  # Retornar operador original se não reconhecido
  )
}

#' Determinar tipo de campo e operadores disponíveis
#' 
#' @param campo Vetor de dados do campo
#' @return Lista com tipo e operadores disponíveis
analisar_tipo_campo <- function(campo) {
  valores_unicos <- length(unique(campo[!is.na(campo)]))
  
  if (is.numeric(campo) && valores_unicos > 30) {
    list(
      tipo = "numerico_continuo",
      operadores = c(
        "Selecione..." = "", 
        "é igual a" = "==", 
        "é maior que" = ">",
        "é maior ou igual a" = ">=", 
        "é menor que" = "<",
        "é menor ou igual a" = "<=", 
        "está entre" = "between"
      )
    )
  } else {
    list(
      tipo = "categorico",
      operadores = c(
        "Selecione..." = "", 
        "é igual a" = "==", 
        "está entre as opções" = "in"
      )
    )
  }
}
