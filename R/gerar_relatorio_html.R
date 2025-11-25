# =====================================================================
# SISTEMA DE GERAÇÃO DE RELATÓRIO HTML INTERATIVO - VERSÃO COMPLETA
# =====================================================================

gerar_relatorio_html <- function(dados, 
                                 qualificacao = NULL,
                                 params = NULL,
                                 config = list(),
                                 output_file) {
  
  # Carregar dependências
  require(htmltools)
  require(plotly)
  require(DT)
  require(dplyr)
  
  cat("\n=== Gerando Relatório HTML Completo ===\n")
  
  # ====================================================================
  # CONFIGURAÇÕES PADRÃO
  # ====================================================================
  
  default_config <- list(
    titulo = "Análise Multicritério ELECTRE Tri-B",
    subtitulo = "Relatório de Resultados",
    autor = "",
    instituicao = "",
    data = Sys.Date(),
    resumo = "",
    template = "tecnico",
    tema = "light",
    cor_principal = "#2c3e50",
    cor_secundaria = "#3498db",
    secoes = c("capa", "resumo", "resultados"),
    elementos_visuais = c("graf_distribuicao"),
    tabelas = c("tab_completa"),
    logo = NULL,
    n_classes = 5,
    label_map = setNames(paste0("Classe ", 1:5), as.character(1:5)),
    paleta_cores = setNames(
      c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
      as.character(1:5)
    ),
    max_linhas_tabela = 50
  )
  
  cfg <- modifyList(default_config, config)
  
  cat("Título:", cfg$titulo, "\n")
  
  # ====================================================================
  # PREPARAR DADOS
  # ====================================================================
  
  if (inherits(dados, "sf")) {
    dados_sf <- dados
    dados_df <- sf::st_drop_geometry(dados)
  } else {
    dados_sf <- NULL
    dados_df <- dados
  }
  
  cat("Dados preparados:", nrow(dados_df), "linhas\n")
  cat("DEBUG: dados é SF?:", inherits(dados, "sf"), "\n")
  cat("DEBUG: dados_sf é NULL?:", is.null(dados_sf), "\n")
  cat("DEBUG: dados_sf tem class_electre?:", !is.null(dados_sf) && "class_electre" %in% names(dados_sf), "\n")  # ← ADICIONAR
  cat("DEBUG: dados_sf tem NM_MUN?:", !is.null(dados_sf) && "NM_MUN" %in% names(dados_sf), "\n")  # ← ADICIONAR
  
  # ====================================================================
  # CRIAR DIRETÓRIO TEMPORÁRIO PARA WIDGETS
  # ====================================================================
  
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  widgets_dir <- file.path(temp_dir, "widgets")
  dir.create(widgets_dir, recursive = TRUE)
  
  cat("Diretório temporário criado:", temp_dir, "\n")
  
  # ====================================================================
  # GERAR WIDGETS E SALVAR
  # ====================================================================
  
  widgets_list <- list()
  
  # ====================================================================
  # GRÁFICOS
  # ====================================================================
  
  # Gráfico: Distribuição de Classes
  if ("graf_distribuicao" %in% cfg$elementos_visuais && "class_electre" %in% names(dados_df)) {
    cat("Gerando gráfico de distribuição...\n")
    
    df_dist <- dados_df %>%
      count(class_electre) %>%
      mutate(
        class_label = cfg$label_map[as.character(class_electre)],
        percent = n / sum(n) * 100
      )
    
    plot_dist <- plot_ly(
      df_dist,
      x = ~class_label,
      y = ~n,
      type = "bar",
      marker = list(
        color = unname(cfg$paleta_cores[as.character(df_dist$class_electre)]),
        line = list(color = "white", width = 2)
      ),
      text = ~paste0(n, " (", round(percent, 1), "%)"),
      textposition = "outside",
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Quantidade: %{y}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = list(
          text = "Distribuição de Municípios por Classe",
          font = list(size = 20, family = "Inter", color = cfg$cor_principal)
        ),
        xaxis = list(title = "Classe de Vulnerabilidade"),
        yaxis = list(title = "Número de Municípios"),
        showlegend = FALSE,
        hovermode = "closest"
      )
    
    widget_file <- file.path(widgets_dir, "grafico_distribuicao.html")
    htmlwidgets::saveWidget(plot_dist, widget_file, selfcontained = TRUE)
    widgets_list$grafico_distribuicao <- widget_file
    
    cat("✓ Gráfico de distribuição salvo\n")
  }
  
  # Gráfico: Classes por Região/UF
  if ("graf_regional" %in% cfg$elementos_visuais) {
    tryCatch({
      cat("Gerando gráfico regional...\n")
      widget <- gerar_grafico_regional(dados_df, cfg)
      widget_file <- file.path(widgets_dir, "grafico_regional.html")
      htmlwidgets::saveWidget(widget, widget_file, selfcontained = TRUE)
      widgets_list$grafico_regional <- widget_file
      cat("✓ Gráfico regional salvo\n")
    }, error = function(e) {
      cat("✗ Erro ao gerar gráfico regional:", e$message, "\n")
    })
  }
  
  # Gráfico: Densidade por Classe
  if ("graf_densidade" %in% cfg$elementos_visuais) {
    tryCatch({
      cat("Gerando gráfico de densidade...\n")
      # Pegar primeiro critério como padrão
      variavel_densidade <- if (!is.null(cfg$criterios) && length(cfg$criterios) > 0) {
        cfg$criterios[1]
      } else {
        NULL
      }
      widget <- gerar_grafico_densidade(dados_df, cfg, variavel = variavel_densidade)
      widget_file <- file.path(widgets_dir, "grafico_densidade.html")
      htmlwidgets::saveWidget(widget, widget_file, selfcontained = TRUE)
      widgets_list$grafico_densidade <- widget_file
      cat("✓ Gráfico de densidade salvo\n")
    }, error = function(e) {
      cat("✗ Erro ao gerar gráfico densidade:", e$message, "\n")
    })
  }
  
  # Gráfico: Qualificação por Classe
  if ("graf_qualif_classe" %in% cfg$elementos_visuais && !is.null(qualificacao)) {
    tryCatch({
      cat("Gerando gráfico de qualificação por classe...\n")
      widget <- gerar_grafico_qualif_classe(dados_df, qualificacao, cfg)
      widget_file <- file.path(widgets_dir, "grafico_qualif_classe.html")
      htmlwidgets::saveWidget(widget, widget_file, selfcontained = TRUE)
      widgets_list$grafico_qualif_classe <- widget_file
      cat("✓ Gráfico qualificação por classe salvo\n")
    }, error = function(e) {
      cat("✗ Erro ao gerar gráfico qualif classe:", e$message, "\n")
    })
  }
  
  # Gráfico: Total de Qualificações
  if ("graf_qualif_total" %in% cfg$elementos_visuais && !is.null(qualificacao)) {
    tryCatch({
      cat("Gerando gráfico de total de qualificações...\n")
      widget <- gerar_grafico_qualif_total(qualificacao, cfg)
      widget_file <- file.path(widgets_dir, "grafico_qualif_total.html")
      htmlwidgets::saveWidget(widget, widget_file, selfcontained = TRUE)
      widgets_list$grafico_qualif_total <- widget_file
      cat("✓ Gráfico total qualificações salvo\n")
    }, error = function(e) {
      cat("✗ Erro ao gerar gráfico qualif total:", e$message, "\n")
    })
  }
  
  # ====================================================================
  # MAPAS
  # ====================================================================
  
  # Mapa: Classificação ELECTRE
  if ("mapa_classificacao" %in% cfg$elementos_visuais && !is.null(dados_sf)) {
    cat("Gerando mapa de classificação...\n")
    
    require(leaflet)
    require(sf)
    
    tryCatch({
      dados_sf_simple <- tryCatch({
        if (requireNamespace("rmapshaper", quietly = TRUE)) {
          rmapshaper::ms_simplify(dados_sf, keep = 0.05)
        } else {
          dados_sf
        }
      }, error = function(e) dados_sf)
      
      dados_sf_simple <- st_transform(dados_sf_simple, 4326)
      
      pal <- colorFactor(
        palette = unname(cfg$paleta_cores),
        domain = dados_sf_simple$class_electre,
        na.color = "#cccccc"
      )
      
      labels <- if ("NM_MUN" %in% names(dados_sf_simple)) {
        sprintf(
          "<strong>%s</strong><br/>Classe: %s",
          dados_sf_simple$NM_MUN,
          dados_sf_simple$class_label
        ) %>% lapply(htmltools::HTML)
      } else {
        sprintf(
          "<strong>Município</strong><br/>Classe: %s",
          dados_sf_simple$class_label
        ) %>% lapply(htmltools::HTML)
      }
      
      mapa <- leaflet(dados_sf_simple) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(class_electre),
          fillOpacity = 0.7,
          color = "#FFFFFF",
          weight = 1,
          opacity = 1,
          highlight = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~class_electre,
          title = "Classe ELECTRE",
          opacity = 0.9,
          labFormat = labelFormat(
            transform = function(x) cfg$label_map[as.character(x)]
          )
        )
      
      widget_file <- file.path(widgets_dir, "mapa_classificacao.html")
      htmlwidgets::saveWidget(mapa, widget_file, selfcontained = TRUE)
      widgets_list$mapa_classificacao <- widget_file
      
      cat("✓ Mapa de classificação salvo\n")
      
    }, error = function(e) {
      cat("✗ Erro ao gerar mapa:", e$message, "\n")
    })
  }
  
  # Mapa: Qualificação Territorial Completo (com camadas)
  if ("mapa_qualificacao" %in% cfg$elementos_visuais && !is.null(dados_sf) && !is.null(qualificacao)) {
    tryCatch({
      cat("Gerando mapa de qualificação completo...\n")
      widget <- gerar_mapa_qualificacao_completo(dados_sf, qualificacao, cfg)
      widget_file <- file.path(widgets_dir, "mapa_qualificacao.html")
      htmlwidgets::saveWidget(widget, widget_file, selfcontained = TRUE)
      widgets_list$mapa_qualificacao <- widget_file
      cat("✓ Mapa de qualificação completo salvo\n")
    }, error = function(e) {
      cat("✗ Erro ao gerar mapa qualificação:", e$message, "\n")
    })
  }
  
  # ====================================================================
  # TABELAS
  # ====================================================================
  
  # Tabela: Resumo por Classe
  if ("tab_resumo_classe" %in% cfg$tabelas && "class_electre" %in% names(dados_df)) {
    cat("Gerando tabela resumo...\n")
    
    df_resumo <- dados_df %>%
      count(class_electre) %>%
      mutate(
        class_label = cfg$label_map[as.character(class_electre)],
        percentual = round(n / sum(n) * 100, 2)
      ) %>%
      arrange(class_electre) %>%
      select(class_label, n, percentual)
    
    names(df_resumo) <- c("Classe", "Quantidade", "Percentual (%)")
    
    dt_resumo <- datatable(
      df_resumo,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = nrow(df_resumo),
        ordering = FALSE
      ),
      class = "table table-striped table-hover"
    ) %>%
      formatStyle(
        "Classe",
        backgroundColor = styleEqual(
          cfg$label_map,
          unname(cfg$paleta_cores)
        ),
        color = "white",
        fontWeight = "bold"
      )
    
    widget_file <- file.path(widgets_dir, "tabela_resumo.html")
    htmlwidgets::saveWidget(dt_resumo, widget_file, selfcontained = TRUE)
    widgets_list$tabela_resumo <- widget_file
    
    cat("✓ Tabela resumo salva\n")
  }
  
  # Tabela: Perfil Médio por Classe
  if ("tab_perfil_medio" %in% cfg$tabelas) {
    tryCatch({
      cat("Gerando tabela de perfil médio...\n")
      widget <- gerar_tabela_perfil_medio(dados_df, cfg)
      widget_file <- file.path(widgets_dir, "tabela_perfil_medio.html")
      htmlwidgets::saveWidget(widget, widget_file, selfcontained = TRUE)
      widgets_list$tabela_perfil_medio <- widget_file
      cat("✓ Tabela perfil médio salva\n")
    }, error = function(e) {
      cat("✗ Erro ao gerar tabela perfil médio:", e$message, "\n")
    })
  }
  
  # Tabela: Estatísticas de Qualificação
  if ("tab_estatisticas_qualif" %in% cfg$tabelas && !is.null(qualificacao)) {
    tryCatch({
      cat("Gerando tabela de estatísticas de qualificação...\n")
      widget <- gerar_tabela_estatisticas_qualif(dados_df, qualificacao, cfg)
      widget_file <- file.path(widgets_dir, "tabela_estatisticas_qualif.html")
      htmlwidgets::saveWidget(widget, widget_file, selfcontained = TRUE)
      widgets_list$tabela_estatisticas_qualif <- widget_file
      cat("✓ Tabela estatísticas qualificação salva\n")
    }, error = function(e) {
      cat("✗ Erro ao gerar tabela estatísticas qualif:", e$message, "\n")
    })
  }
  
  # Tabela: Ranking de Municípios
  if ("tab_ranking" %in% cfg$tabelas) {
    tryCatch({
      cat("Gerando tabela de ranking...\n")
      widget <- gerar_tabela_ranking(dados_df, cfg, top_n = 20)
      widget_file <- file.path(widgets_dir, "tabela_ranking.html")
      htmlwidgets::saveWidget(widget, widget_file, selfcontained = TRUE)
      widgets_list$tabela_ranking <- widget_file
      cat("✓ Tabela ranking salva\n")
    }, error = function(e) {
      cat("✗ Erro ao gerar tabela ranking:", e$message, "\n")
    })
  }
  
  # Tabela: Parâmetros ELECTRE
  if ("tab_parametros" %in% cfg$tabelas && !is.null(params)) {
    tryCatch({
      cat("Gerando tabela de parâmetros...\n")
      widget <- gerar_tabela_parametros(params)
      widget_file <- file.path(widgets_dir, "tabela_parametros.html")
      htmlwidgets::saveWidget(widget, widget_file, selfcontained = TRUE)
      widgets_list$tabela_parametros <- widget_file
      cat("✓ Tabela parâmetros salva\n")
    }, error = function(e) {
      cat("✗ Erro ao gerar tabela parâmetros:", e$message, "\n")
    })
  }
  
  # Tabela: Dados Completos
  if ("tab_completa" %in% cfg$tabelas) {
    cat("Gerando tabela completa...\n")
    
    max_linhas <- if (cfg$max_linhas_tabela > 0) {
      min(cfg$max_linhas_tabela, nrow(dados_df))
    } else {
      nrow(dados_df)
    }
    
    df_tab <- head(dados_df, max_linhas)
    
    dt_completa <- datatable(
      df_tab,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      extensions = "Buttons",
      class = "table table-striped table-hover"
    )
    
    if ("class_electre" %in% names(df_tab)) {
      dt_completa <- dt_completa %>%
        formatStyle(
          "class_electre",
          backgroundColor = styleEqual(
            1:cfg$n_classes,
            unname(cfg$paleta_cores[as.character(1:cfg$n_classes)])
          ),
          color = "white",
          fontWeight = "bold"
        )
    }
    
    widget_file <- file.path(widgets_dir, "tabela_completa.html")
    htmlwidgets::saveWidget(dt_completa, widget_file, selfcontained = TRUE)
    widgets_list$tabela_completa <- widget_file
    
    cat("✓ Tabela completa salva\n")
  }
  
  # ====================================================================
  # INCORPORAR WIDGETS NO HTML
  # ====================================================================
  
  cat("Incorporando widgets no HTML...\n")
  
  widgets_html <- list()
  
  for (widget_name in names(widgets_list)) {
    widget_file <- widgets_list[[widget_name]]
    widget_content <- paste(readLines(widget_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    widgets_html[[widget_name]] <- widget_content
    cat("✓ Widget", widget_name, "lido\n")
  }
  
  # ====================================================================
  # CONSTRUIR HTML FINAL
  # ====================================================================
  
  cat("Construindo HTML final...\n")
  
  html_final <- tags$html(
    lang = "pt-BR",
    
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title(cfg$titulo),
      
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"
      ),
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
      ),
      tags$link(
        rel = "preconnect",
        href = "https://fonts.googleapis.com"
      ),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap"
      ),
      
      criar_css_completo(cfg)
    ),
    
    tags$body(
      
      criar_navbar_completa(cfg),
      
      tags$main(
        class = "main-content",
        
        # Capa
        if ("capa" %in% cfg$secoes) {
          criar_secao_capa_completa(cfg)
        },
        
        # Resumo Executivo
        if ("resumo" %in% cfg$secoes) {
          criar_secao_resumo_completa(dados_df, cfg)
        },
        
        # Resultados
        if ("resultados" %in% cfg$secoes) {
          criar_secao_resultados_completa(widgets_html, cfg)
        },
        
        # Qualificação
        if ("qualificacao" %in% cfg$secoes) {
          criar_secao_qualificacao_completa(widgets_html, qualificacao, cfg)
        },
        
        # Parâmetros
        if ("parametros" %in% cfg$secoes && !is.null(params)) {
          criar_secao_parametros_completa(params, cfg)
        }
      ),
      
      criar_footer_completo(cfg),
      criar_scripts_completos()
    )
  )
  
  # ====================================================================
  # SALVAR HTML
  # ====================================================================
  
  cat("Salvando HTML final em:", output_file, "\n")
  
  save_html(html_final, file = output_file)
  unlink(temp_dir, recursive = TRUE)
  
  if (file.exists(output_file)) {
    cat("✓ Relatório HTML gerado com sucesso!\n")
    cat("Tamanho:", format(file.size(output_file), big.mark = "."), "bytes\n")
  } else {
    stop("Erro: arquivo não foi criado!")
  }
  
  return(invisible(output_file))
}

# ====================================================================
# FUNÇÕES AUXILIARES DE CONSTRUÇÃO DO HTML
# ====================================================================

criar_css_completo <- function(cfg) {
  
  bg_color <- if (cfg$tema == "dark") "#1a1a1a" else "#ffffff"
  text_color <- if (cfg$tema == "dark") "#e0e0e0" else "#333333"
  card_bg <- if (cfg$tema == "dark") "#2d2d2d" else "#f8f9fa"
  
  css <- sprintf('
    :root {
      --cor-principal: %s;
      --cor-secundaria: %s;
      --bg-color: %s;
      --text-color: %s;
      --card-bg: %s;
    }
    
    * {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }
    
    body {
      font-family: "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      background-color: var(--bg-color);
      color: var(--text-color);
      line-height: 1.6;
    }
    
    .navbar-custom {
      background: linear-gradient(135deg, var(--cor-principal) 0%%, var(--cor-secundaria) 100%%);
      box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      padding: 1rem 0;
      position: sticky;
      top: 0;
      z-index: 1000;
    }
    
    .navbar-brand {
      font-size: 1.5rem;
      font-weight: 700;
      color: white !important;
      text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
    }
    
    .nav-link {
      color: rgba(255,255,255,0.9) !important;
      font-weight: 500;
      transition: all 0.3s ease;
      padding: 0.5rem 1rem !important;
      margin: 0 0.2rem;
      border-radius: 5px;
    }
    
    .nav-link:hover {
      background-color: rgba(255,255,255,0.2);
      color: white !important;
      transform: translateY(-2px);
    }
    
    .main-content {
      max-width: 1400px;
      margin: 0 auto;
      padding: 2rem;
    }
    
    .section {
      padding: 4rem 0;
      min-height: 400px;
    }
    
    .section:nth-child(even) {
      background-color: var(--card-bg);
    }
    
    .section-header {
      text-align: center;
      margin-bottom: 3rem;
    }
    
    .section-title {
      font-size: 2.5rem;
      font-weight: 700;
      color: var(--cor-principal);
      margin-bottom: 1rem;
    }
    
    .section-divider {
      width: 80px;
      height: 4px;
      background: linear-gradient(90deg, var(--cor-principal), var(--cor-secundaria));
      margin: 1rem auto;
      border-radius: 2px;
    }
    
    .card-custom {
      background-color: white;
      border: none;
      border-radius: 12px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.07);
      transition: all 0.3s ease;
      margin-bottom: 2rem;
      overflow: hidden;
    }
    
    .card-custom:hover {
      box-shadow: 0 8px 15px rgba(0,0,0,0.15);
      transform: translateY(-3px);
    }
    
    .card-header-custom {
      background: linear-gradient(135deg, var(--cor-principal) 0%%, var(--cor-secundaria) 100%%);
      color: white;
      padding: 1.2rem 1.5rem;
      font-weight: 600;
      font-size: 1.1rem;
    }
    
    .card-body-custom {
      padding: 2rem;
    }
    
    .metric-card {
      text-align: center;
      padding: 2rem;
      background: linear-gradient(135deg, var(--cor-principal) 0%%, var(--cor-secundaria) 100%%);
      border-radius: 12px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      transition: all 0.3s ease;
      height: 100%%;
      color: white;
    }
    
    .metric-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 8px 15px rgba(0,0,0,0.2);
    }
    
    .metric-value {
      font-size: 3rem;
      font-weight: 700;
      margin: 1rem 0;
    }
    
    .metric-label {
      font-size: 1.1rem;
      font-weight: 500;
      opacity: 0.95;
    }
    
    .metric-icon {
      font-size: 3rem;
      opacity: 0.8;
    }
    
    .capa-section {
      background: linear-gradient(135deg, var(--cor-principal) 0%%, var(--cor-secundaria) 100%%);
      color: white;
      min-height: 100vh;
      display: flex;
      align-items: center;
      justify-content: center;
      text-align: center;
    }
    
    .capa-title {
      font-size: 4rem;
      font-weight: 700;
      text-shadow: 2px 2px 8px rgba(0,0,0,0.3);
      margin-bottom: 1rem;
    }
    
    .capa-subtitle {
      font-size: 2rem;
      font-weight: 300;
      opacity: 0.95;
      margin-bottom: 2rem;
    }
    
    .footer-custom {
      background-color: #2c3e50;
      color: white;
      padding: 3rem 0;
      margin-top: 4rem;
      text-align: center;
    }
    
    @keyframes fadeInUp {
      from {
        opacity: 0;
        transform: translateY(30px);
      }
      to {
        opacity: 1;
        transform: translateY(0);
      }
    }
    
    .fade-in-up {
      animation: fadeInUp 0.6s ease-out;
    }
    
    @media print {
      .navbar-custom, .footer-custom {
        display: none;
      }
      .section {
        page-break-inside: avoid;
      }
    }
    
    @media (max-width: 768px) {
      .section-title {
        font-size: 2rem;
      }
      .metric-value {
        font-size: 2rem;
      }
      .capa-title {
        font-size: 2.5rem;
      }
    }
  ', cfg$cor_principal, cfg$cor_secundaria, bg_color, text_color, card_bg)
  
  tags$style(HTML(css))
}

criar_navbar_completa <- function(cfg) {
  
  nav_links <- list()
  
  if ("capa" %in% cfg$secoes) {
    nav_links <- c(nav_links, list(
      tags$li(class = "nav-item", 
              tags$a(class = "nav-link", href = "#inicio", "Início"))
    ))
  }
  
  if ("resumo" %in% cfg$secoes) {
    nav_links <- c(nav_links, list(
      tags$li(class = "nav-item", 
              tags$a(class = "nav-link", href = "#resumo", "Resumo"))
    ))
  }
  
  if ("resultados" %in% cfg$secoes) {
    nav_links <- c(nav_links, list(
      tags$li(class = "nav-item", 
              tags$a(class = "nav-link", href = "#resultados", "Resultados"))
    ))
  }
  
  if ("qualificacao" %in% cfg$secoes) {
    nav_links <- c(nav_links, list(
      tags$li(class = "nav-item", 
              tags$a(class = "nav-link", href = "#qualificacao", "Qualificação"))
    ))
  }
  
  tags$nav(
    class = "navbar navbar-expand-lg navbar-dark navbar-custom",
    tags$div(
      class = "container-fluid",
      tags$a(class = "navbar-brand", href = "#inicio",
             tags$i(class = "fas fa-chart-line me-2"),
             cfg$titulo
      ),
      tags$button(
        class = "navbar-toggler",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#navbarNav",
        tags$span(class = "navbar-toggler-icon")
      ),
      tags$div(
        class = "collapse navbar-collapse",
        id = "navbarNav",
        tags$ul(class = "navbar-nav ms-auto", nav_links)
      )
    )
  )
}

criar_secao_capa_completa <- function(cfg) {
  tags$section(
    id = "inicio",
    class = "capa-section",
    tags$div(
      class = "container fade-in-up",
      
      if (!is.null(cfg$logo)) {
        tags$div(
          class = "mb-4",
          tags$img(
            src = sprintf("data:image/png;base64,%s", 
                          base64enc::base64encode(cfg$logo$datapath)),
            style = "max-width: 200px; max-height: 150px;"
          )
        )
      },
      
      tags$h1(class = "capa-title", cfg$titulo),
      tags$h2(class = "capa-subtitle", cfg$subtitulo),
      
      tags$hr(style = "border-color: rgba(255,255,255,0.3); width: 50%; margin: 2rem auto;"),
      
      if (nchar(cfg$autor) > 0) {
        tags$p(class = "lead fs-4",
               tags$i(class = "fas fa-user me-2"), cfg$autor)
      },
      
      if (nchar(cfg$instituicao) > 0) {
        tags$p(class = "lead fs-5",
               tags$i(class = "fas fa-university me-2"), cfg$instituicao)
      },
      
      tags$p(class = "lead",
             tags$i(class = "fas fa-calendar me-2"),
             format(cfg$data, "%d de %B de %Y"))
    )
  )
}

criar_secao_resumo_completa <- function(dados_df, cfg) {
  
  n_total <- nrow(dados_df)
  n_classes <- length(unique(dados_df$class_electre))
  
  if ("class_electre" %in% names(dados_df)) {
    dist_classes <- dados_df %>%
      count(class_electre) %>%
      arrange(desc(n))
    
    classe_dominante <- cfg$label_map[as.character(dist_classes$class_electre[1])]
    prop_alto <- sum(dados_df$class_electre >= (n_classes - 1)) / n_total * 100
  } else {
    classe_dominante <- "N/A"
    prop_alto <- 0
  }
  
  tags$section(
    id = "resumo",
    class = "section",
    tags$div(
      class = "container",
      
      tags$div(
        class = "section-header fade-in-up",
        tags$h2(class = "section-title",
                tags$i(class = "fas fa-file-alt me-3"),
                "Resumo Executivo"),
        tags$div(class = "section-divider")
      ),
      
      if (nchar(cfg$resumo) > 0) {
        tags$div(
          class = "card-custom fade-in-up mb-4",
          tags$div(
            class = "card-body-custom",
            tags$p(class = "lead", style = "text-align: justify;", cfg$resumo)
          )
        )
      },
      
      tags$div(
        class = "row g-4 fade-in-up",
        
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "metric-card",
            tags$div(class = "metric-icon", tags$i(class = "fas fa-map-marked-alt")),
            tags$div(class = "metric-value", format(n_total, big.mark = ".")),
            tags$div(class = "metric-label", "Municípios Analisados")
          )
        ),
        
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "metric-card",
            tags$div(class = "metric-icon", tags$i(class = "fas fa-layer-group")),
            tags$div(class = "metric-value", n_classes),
            tags$div(class = "metric-label", "Classes")
          )
        ),
        
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "metric-card",
            tags$div(class = "metric-icon", tags$i(class = "fas fa-crown")),
            tags$div(class = "metric-value", style = "font-size: 1.5rem;", classe_dominante),
            tags$div(class = "metric-label", "Classe Dominante")
          )
        ),
        
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "metric-card",
            tags$div(class = "metric-icon", tags$i(class = "fas fa-exclamation-triangle")),
            tags$div(class = "metric-value", sprintf("%.1f%%", prop_alto)),
            tags$div(class = "metric-label", "Alta Vulnerabilidade")
          )
        )
      )
    )
  )
}

criar_secao_resultados_completa <- function(widgets_html, cfg) {
  
  tags$section(
    id = "resultados",
    class = "section",
    tags$div(
      class = "container",
      
      tags$div(
        class = "section-header fade-in-up",
        tags$h2(class = "section-title",
                tags$i(class = "fas fa-chart-bar me-3"),
                "Resultados da Classificação"),
        tags$div(class = "section-divider")
      ),
      
      # Gráfico de distribuição
      if (!is.null(widgets_html$grafico_distribuicao)) {
        criar_card_widget(
          "Distribuição por Classe",
          "chart-bar",
          widgets_html$grafico_distribuicao,
          height = "500px"
        )
      },
      
      # Gráfico regional
      if (!is.null(widgets_html$grafico_regional)) {
        criar_card_widget(
          "Distribuição por UF",
          "map-location-dot",
          widgets_html$grafico_regional,
          height = "500px"
        )
      },
      
      # Gráfico de densidade
      if (!is.null(widgets_html$grafico_densidade)) {
        criar_card_widget(
          "Densidade por Classe",
          "chart-area",
          widgets_html$grafico_densidade,
          height = "500px"
        )
      },
      
      # Mapa de classificação
      if (!is.null(widgets_html$mapa_classificacao)) {
        criar_card_widget(
          "Mapa de Classificação ELECTRE Tri-B",
          "map",
          widgets_html$mapa_classificacao,
          height = "600px",
          padding = FALSE
        )
      },
      
      # Tabela resumo
      if (!is.null(widgets_html$tabela_resumo)) {
        criar_card_widget(
          "Resumo por Classe",
          "table",
          widgets_html$tabela_resumo,
          height = "400px"
        )
      },
      
      # Tabela perfil médio
      if (!is.null(widgets_html$tabela_perfil_medio)) {
        criar_card_widget(
          "Perfil Médio por Classe",
          "chart-line",
          widgets_html$tabela_perfil_medio,
          height = "400px"
        )
      },
      
      # Tabela ranking
      if (!is.null(widgets_html$tabela_ranking)) {
        criar_card_widget(
          "Ranking de Municípios",
          "trophy",
          widgets_html$tabela_ranking,
          height = "500px"
        )
      },
      
      # Tabela completa
      if (!is.null(widgets_html$tabela_completa)) {
        criar_card_widget(
          sprintf("Dados Completos (primeiros %d registros)", cfg$max_linhas_tabela),
          "database",
          widgets_html$tabela_completa,
          height = "600px"
        )
      }
    )
  )
}

criar_secao_qualificacao_completa <- function(widgets_html, qualificacao, cfg) {
  
  if (is.null(qualificacao) || length(qualificacao) == 0) {
    return(
      tags$section(
        id = "qualificacao",
        class = "section",
        tags$div(
          class = "container",
          tags$div(
            class = "section-header fade-in-up",
            tags$h2(class = "section-title",
                    tags$i(class = "fas fa-layer-group me-3"),
                    "Análise de Qualificação Territorial"),
            tags$div(class = "section-divider")
          ),
          tags$div(
            class = "alert alert-warning fade-in-up",
            style = "max-width: 800px; margin: 0 auto;",
            tags$i(class = "fas fa-info-circle me-2"),
            "Dados de qualificação territorial não disponíveis ou não foram calculados."
          )
        )
      )
    )
  }
  
  tags$section(
    id = "qualificacao",
    class = "section",
    tags$div(
      class = "container",
      tags$div(
        class = "section-header fade-in-up",
        tags$h2(class = "section-title",
                tags$i(class = "fas fa-layer-group me-3"),
                "Análise de Qualificação Territorial"),
        tags$div(class = "section-divider")
      ),
      
      # Cards com totais
      tags$div(
        class = "row g-4 fade-in-up mb-4 justify-content-center",
        tags$div(
          class = "col-md-4",
          tags$div(
            class = "card-custom",
            tags$div(class = "card-body-custom text-center",
                     tags$h3(style = "color: var(--cor-principal); font-size: 3rem;", 
                             length(qualificacao)),
                     tags$p(style = "font-size: 1.2rem; margin: 0;", "Camadas Analisadas"))
          )
        )
      ),
      
      # Mapa de qualificação (COM CAMADAS INTERATIVAS)
      if (!is.null(widgets_html$mapa_qualificacao)) {
        criar_card_widget(
          "Mapa Interativo de Qualificação Territorial",
          "layer-group",
          widgets_html$mapa_qualificacao,
          height = "700px",
          padding = FALSE
        )
      },
      
      # Gráfico de qualificação por classe
      if (!is.null(widgets_html$grafico_qualif_classe)) {
        criar_card_widget(
          "Qualificações por Classe ELECTRE",
          "chart-column",
          widgets_html$grafico_qualif_classe,
          height = "500px"
        )
      },
      
      # Gráfico de total de qualificações
      if (!is.null(widgets_html$grafico_qualif_total)) {
        criar_card_widget(
          "Total de Qualificações por Tipo",
          "chart-bar",
          widgets_html$grafico_qualif_total,
          height = "500px"
        )
      },
      
      # Tabela de estatísticas
      if (!is.null(widgets_html$tabela_estatisticas_qualif)) {
        criar_card_widget(
          "Estatísticas de Qualificação Territorial",
          "table",
          widgets_html$tabela_estatisticas_qualif,
          height = "400px"
        )
      },
      
      # Descrição
      tags$div(
        class = "card-custom fade-in-up",
        tags$div(
          class = "card-body-custom",
          tags$p(
            class = "lead",
            "A análise de qualificação territorial identifica a presença de diferentes ",
            "elementos territoriais (comunidades quilombolas, assentamentos, terras indígenas, ",
            "instituições de ensino, unidades prisionais e bancos de sementes) nos municípios ",
            "classificados pelo ELECTRE Tri-B."
          )
        )
      )
    )
  )
}

criar_secao_parametros_completa <- function(params, cfg) {
  tags$section(
    id = "parametros",
    class = "section",
    tags$div(
      class = "container",
      tags$div(
        class = "section-header fade-in-up",
        tags$h2(class = "section-title",
                tags$i(class = "fas fa-cog me-3"),
                "Parâmetros da Análise"),
        tags$div(class = "section-divider")
      ),
      tags$div(
        class = "row justify-content-center",
        tags$div(
          class = "col-md-8",
          tags$div(
            class = "card-custom fade-in-up",
            tags$div(class = "card-header-custom", "Configuração ELECTRE Tri-B"),
            tags$div(
              class = "card-body-custom",
              tags$ul(
                class = "list-group list-group-flush",
                tags$li(class = "list-group-item",
                        tags$strong("Número de classes: "), params$n_classes),
                tags$li(class = "list-group-item",
                        tags$strong("Lambda (λ): "), params$lambda),
                tags$li(class = "list-group-item",
                        tags$strong("Regra: "), params$rule),
                tags$li(class = "list-group-item",
                        tags$strong("Critérios: "), 
                        paste(params$criterios, collapse = ", "))
              )
            )
          )
        )
      )
    )
  )
}

criar_footer_completo <- function(cfg) {
  tags$footer(
    class = "footer-custom",
    tags$div(
      class = "container",
      tags$p(
        sprintf(
          "Relatório gerado em %s",
          format(Sys.time(), "%d/%m/%Y às %H:%M")
        )
      ),
      if (nchar(cfg$instituicao) > 0) {
        tags$p(cfg$instituicao)
      },
      tags$p(
        class = "mt-3",
        tags$small(
          tags$i(class = "fas fa-code me-1"),
          "Desenvolvido com R, Shiny e htmltools"
        )
      )
    )
  )
}

criar_scripts_completos <- function() {
  tagList(
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"
    ),
    tags$script(HTML('
      document.querySelectorAll("a[href^=\'#\']").forEach(anchor => {
        anchor.addEventListener("click", function (e) {
          e.preventDefault();
          const target = document.querySelector(this.getAttribute("href"));
          if (target) {
            target.scrollIntoView({ behavior: "smooth", block: "start" });
          }
        });
      });
    '))
  )
}

# Função auxiliar para criar cards de widgets
criar_card_widget <- function(titulo, icon, conteudo, height = "500px", padding = TRUE) {
  tags$div(
    class = "card-custom fade-in-up mb-4",
    tags$div(
      class = "card-header-custom",
      tags$i(class = paste0("fas fa-", icon, " me-2")),
      titulo
    ),
    tags$div(
      class = if (padding) "card-body-custom" else "card-body-custom p-0",
      tags$iframe(
        srcdoc = conteudo,
        style = sprintf("width: 100%%; height: %s; border: none; border-radius: 8px;", height),
        sandbox = "allow-scripts allow-same-origin"
      )
    )
  )
}