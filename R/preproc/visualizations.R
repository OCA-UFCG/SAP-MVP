# =====================================================================
# VISUALIZAÇÕES - GRÁFICOS
# =====================================================================

#' Criar histogramas para variáveis selecionadas
#' 
#' @param df Dataframe sem geometria
#' @param vars Vetor com nomes das variáveis
#' @return Objeto plotly
criar_histogramas <- function(df, vars) {
  validate(need(length(vars) >= 1, "Selecione variáveis no sidebar"))
  
  plots <- lapply(vars, function(v) {
    plot_ly(df, x = ~.data[[v]], type = "histogram", nbinsx = 30, name = v) |>
      layout(title = v, xaxis = list(title = v), showlegend = FALSE)
  })
  
  if (length(plots) == 1) {
    plots[[1]]
  } else {
    subplot(plots, nrows = ceiling(length(plots)/2), shareX = FALSE, shareY = FALSE)
  }
}

#' Criar boxplots para variáveis selecionadas
#' 
#' @param df Dataframe sem geometria
#' @param vars Vetor com nomes das variáveis
#' @return Objeto plotly
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
#' @param df Dataframe sem geometria
#' @param vars Vetor com nomes das variáveis
#' @return Promise com plotly
criar_matriz_correlacao <- function(df, vars) {
  validate(need(length(vars) >= 2, "Selecione 2+ variáveis"))
  
  m <- as.matrix(df[vars])
  future_promise({
    stats::cor(m, use = "pairwise.complete.obs")
  }, seed = TRUE) %...>% (function(C) {
    df_corr <- reshape2::melt(C, varnames = c("x","y"), value.name = "corr")
    plot_ly(df_corr, x = ~x, y = ~y, z = ~corr, colorscale = "RdBu", 
            zmin = -1, zmax = 1, type = "heatmap", showscale = TRUE) |>
      layout(title = "Correlação (Pearson)")
  })
}

#' Criar gráfico de PCA
#' 
#' @param df Dataframe sem geometria
#' @param vars Vetor com nomes das variáveis
#' @return Promise com plotly
criar_pca <- function(df, vars) {
  validate(need(length(vars) >= 2, "Selecione 2+ variáveis"))
  
  m <- as.matrix(df[vars])
  future_promise({
    pr <- stats::prcomp(m, center = TRUE, scale. = TRUE)
    list(scores = pr$x, sdev = pr$sdev)
  }, seed = TRUE) %...>% (function(obj) {
    sc <- as.data.frame(obj$scores)
    expvar <- round(100 * (obj$sdev^2 / sum(obj$sdev^2)), 1)
    ax1 <- paste0("PC1 (", expvar[1], "%)")
    ax2 <- paste0("PC2 (", expvar[2], "%)")
    
    plot_ly(sc, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers",
            marker = list(size = 6, opacity = 0.7)) |>
      layout(title = "PCA (normalizado)", xaxis = list(title = ax1), yaxis = list(title = ax2))
  })
}

#' Configurar outputs de visualização no módulo
#' 
#' @param output Output object do Shiny
#' @param df_plain Reactive com dataframe sem geometria
#' @param vars_sel Reactive com variáveis selecionadas
setup_visualizacoes <- function(output, df_plain, vars_sel) {
  # Histogramas
  output$p_hist <- renderPlotly({
    vars <- vars_sel()
    req(length(vars) > 0)
    d <- df_plain()
    req(d)
    criar_histogramas(d, vars)
  })
  
  # Boxplots
  output$p_box <- renderPlotly({
    vars <- vars_sel()
    req(length(vars) > 0)
    d <- df_plain()
    req(d)
    criar_boxplots(d, vars)
  })
  
  # Matriz de correlação
  output$p_corr <- renderPlotly({
    v <- vars_sel()
    req(length(v) >= 2)
    d <- df_plain()
    req(d)
    criar_matriz_correlacao(d, v)
  })
  
  # PCA
  output$p_pca <- renderPlotly({
    v <- vars_sel()
    req(length(v) >= 2)
    d <- df_plain()
    req(d)
    criar_pca(d, v)
  })
}
