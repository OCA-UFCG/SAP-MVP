# =====================================================================
# PROCESSAMENTO DE DADOS
# =====================================================================

#' Preparar dados iniciais para análise
#' 
#' @param df Dataframe espacial de entrada
#' @return Dataframe processado
preparar_dados <- function(df) {
  # Aqui você pode adicionar qualquer pré-processamento necessário
  # Por exemplo: remover NAs, padronizar nomes, etc.
  df
}

#' Aplicar filtros e selecionar colunas
#' 
#' @param df Dataframe espacial
#' @param filtros Lista de filtros a aplicar
#' @param vars_sel Variáveis selecionadas para manter
#' @param colunas_essenciais Colunas que sempre devem ser mantidas
#' @return Dataframe filtrado e com colunas selecionadas
processar_dados_com_filtros <- function(df, filtros, vars_sel, 
                                         colunas_essenciais = c("NM_REGIAO", "NM_UF", "NM_MUN", "CD_MUN")) {
  # Aplicar filtros
  resultado <- aplicar_filtros(df, filtros)
  
  # Adicionar coluna de geometria às essenciais
  if (inherits(df, "sf")) {
    geom_col <- attr(df, "sf_column")
    colunas_essenciais <- c(colunas_essenciais, geom_col)
  }
  
  # Manter apenas colunas essenciais
  colunas_essenciais <- intersect(colunas_essenciais, names(resultado))
  
  # Combinar com variáveis selecionadas e remover duplicatas
  colunas_manter <- unique(c(colunas_essenciais, vars_sel))
  
  # Selecionar colunas
  dplyr::select(resultado, dplyr::any_of(colunas_manter))
}

#' Criar tabela de resumo para exibição
#' 
#' @param df Dataframe (sem geometria)
#' @param vars_sel Variáveis selecionadas
#' @param base_cols Colunas base (região, UF, município)
#' @return Dataframe resumido
criar_tabela_resumo <- function(df, vars_sel, base_cols = c("NM_REGIAO","NM_UF","NM_MUN")) {
  base_cols <- intersect(base_cols, names(df))
  dplyr::select(df, dplyr::any_of(base_cols), dplyr::all_of(vars_sel))
}

#' Calcular estatísticas básicas
#' 
#' @param df Dataframe
#' @param vars Variáveis para calcular estatísticas
#' @return Lista com estatísticas
calcular_estatisticas_basicas <- function(df, vars) {
  stats <- lapply(vars, function(v) {
    valores <- df[[v]]
    if (is.numeric(valores)) {
      list(
        variavel = v,
        media = mean(valores, na.rm = TRUE),
        mediana = median(valores, na.rm = TRUE),
        desvio_padrao = sd(valores, na.rm = TRUE),
        minimo = min(valores, na.rm = TRUE),
        maximo = max(valores, na.rm = TRUE),
        n_validos = sum(!is.na(valores)),
        n_missing = sum(is.na(valores))
      )
    } else {
      list(
        variavel = v,
        tipo = "não numérico",
        n_categorias = length(unique(valores)),
        n_validos = sum(!is.na(valores)),
        n_missing = sum(is.na(valores))
      )
    }
  })
  
  names(stats) <- vars
  stats
}
