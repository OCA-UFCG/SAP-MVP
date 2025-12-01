# =====================================================================
# VISUALIZAÇÕES - GRÁFICOS (VERSÃO SÍNCRONA/SEQUENCIAL)
# 
# NOTA DE ARQUITETURA:
# Anteriormente, este módulo usava processamento paralelo (pacote 'future').
# Foi refatorado para execução SÍNCRONA (sequencial) para garantir estabilidade
# em ambientes containerizados (Docker/Linux), onde o gerenciamento de 
# processos filhos (forking) causava instabilidade e crash da aplicação.
# Como o volume de dados é médio (~1.6k linhas, com Docker é possível verificar isso ao forçar o R a rodar diretamente no terminal), o impacto de performance é irrelevante.
# =====================================================================

library(plotly) # Biblioteca de gráficos interativos (baseada em D3.js)
library(dplyr)  # Manipulação de dados (equivalente a SQL/Pandas)
library(tidyr)  # Organização de dados
library(stats)  # Funções estatísticas base do R

#' Criar histogramas para variáveis selecionadas
#' 
#' @param df Dataframe (tabela) contendo apenas os dados brutos (sem geometria espacial)
#' @param vars Vetor de strings com os nomes das colunas selecionadas
#' @return Objeto plotly (objeto JSON de configuração do gráfico)
criar_histogramas <- function(df, vars) {
  # Validação de UI: Se falhar, exibe mensagem amigável no frontend em vez de crashar
  validate(need(length(vars) >= 1, "Selecione variáveis no sidebar"))
  
  # 'lapply' é equivalente ao .map() do JS ou list comprehension do Python
  # Itera sobre cada variável selecionada para criar um gráfico individual
  plots <- lapply(vars, function(v) {
    # A sintaxe ~.data[[v]] é usada para acessar colunas dinamicamente pelo nome (string)
    plot_ly(df, x = ~.data[[v]], type = "histogram", nbinsx = 30, name = v) |>
      layout(title = v, xaxis = list(title = v), showlegend = FALSE)
  })
  
  # Se for apenas um gráfico, retorna ele direto.
  # Se forem vários, usa subplot (grid) para renderizar lado a lado.
  if (length(plots) == 1) {
    plots[[1]]
  } else {
    subplot(plots, nrows = ceiling(length(plots)/2), shareX = FALSE, shareY = FALSE)
  }
}

#' Criar boxplots para variáveis selecionadas
#' Lógica idêntica à de histogramas, mudando apenas o tipo de plot.
criar_boxplots <- function(df, vars) {
  validate(need(length(vars) >= 1, "Selecione variáveis no sidebar"))
  
  plots <- lapply(vars, function(v) {
    plot_ly(df, y = ~.data[[v]], type = "box", name = v) |>
      layout(title = v, yaxis = list(title = v), showlegend = FALSE)
  })
  
  if (length(plots) == 1) {
    plots[[1]]
  } else {
    subplot(plots, nrows = ceiling(length(plots)/2), shareX = FALSE, shareY = FALSE)
  }
}

#' Criar matriz de correlação
#' 
#' @note Execução Síncrona: O cálculo é feito na thread principal do R.
criar_matriz_correlacao <- function(df, vars) {
  # 'req': Garante que o dataframe existe antes de prosseguir (evita null pointer exceptions)
  req(df)
  validate(need(length(vars) >= 2, "Selecione 2+ variáveis"))
  
  # Pipeline de dados (%>%) - Equivalente a encadeamento de métodos
  # 1. Seleciona colunas; 2. Remove linhas com NAs (drop na)
  df_sel <- df %>% select(all_of(vars)) %>% na.omit()
  
  # Se após limpar NAs não sobrar nada, retorna nulo
  if(nrow(df_sel) == 0) return(NULL)
  
  # Cálculo estatístico (R base) - Rápido para < 10k linhas
  corr_mat <- cor(df_sel, use = "complete.obs")
  
  # Renderização do Heatmap
  plot_ly(
    x = colnames(corr_mat),
    y = colnames(corr_mat),
    z = corr_mat,
    type = "heatmap",
    colors = colorRamp(c("blue", "white", "red")) # Gradiente de cor
  ) %>%
    layout(title = "Correlação (Pearson)")
}

#' Criar gráfico de PCA (Análise de Componentes Principais)
#' 
#' @note Execução Síncrona.
criar_pca <- function(df, vars) {
  req(df)
  validate(need(length(vars) >= 2, "Selecione 2+ variáveis"))
  
  # Preparação dos dados
  df_sel <- df %>% select(all_of(vars)) %>% na.omit()
  
  if(nrow(df_sel) == 0) return(NULL)
  
  # Cálculo do PCA (scale. = TRUE normaliza os dados antes, essencial para PCA)
  pca <- prcomp(df_sel, scale. = TRUE)
  
  # Cálculo da variância explicada (para mostrar nos eixos quanto cada PC representa)
  var_exp <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)
  
  # Cria um novo dataframe apenas com os resultados (PC1 e PC2) para plotagem
  pca_data <- data.frame(
    PC1 = pca$x[,1], # Acessa a primeira coluna da matriz de rotação
    PC2 = pca$x[,2]
  )
  
  # Scatter plot dos componentes
  plot_ly(data = pca_data, x = ~PC1, y = ~PC2, type = 'scatter', mode = 'markers',
          marker = list(opacity = 0.6)) %>%
    layout(
      title = "PCA (normalizado)",
      xaxis = list(title = paste0("PC1 (", var_exp[1], "%)")),
      yaxis = list(title = paste0("PC2 (", var_exp[2], "%)"))
    )
}

#' Configurar outputs de visualização no módulo (Controller)
#' 
#' @param output Objeto de saída do Shiny (onde os gráficos são "desenhados")
#' @param df_plain Reactive (sinal) contendo os dados
#' @param vars_sel Reactive (sinal) contendo as variáveis escolhidas pelo usuário
setup_visualizacoes <- function(output, df_plain, vars_sel) {
  
  # renderPlotly: Função reativa que observa mudanças em df_plain ou vars_sel
  # e re-executa o bloco de código automaticamente.
  
  output$p_hist <- renderPlotly({
    req(df_plain(), vars_sel()) # Guarda-chuva: só executa se os inputs não forem nulos
    criar_histogramas(df_plain(), vars_sel())
  })
  
  output$p_box <- renderPlotly({
    req(df_plain(), vars_sel())
    criar_boxplots(df_plain(), vars_sel())
  })
  
  # Correlação
  # OBS: Anteriormente usava promises para async. Agora chama direto.
  output$p_corr <- renderPlotly({
    req(df_plain(), vars_sel())
    criar_matriz_correlacao(df_plain(), vars_sel())
  })
  
  # PCA
  output$p_pca <- renderPlotly({
    req(df_plain(), vars_sel())
    criar_pca(df_plain(), vars_sel())
  })
}
