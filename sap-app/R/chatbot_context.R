# =====================================================================
# CHATBOT - Sistema de Captura de Contexto
# =====================================================================

# ---- Construtor de Contexto Base --------------------------------------

build_base_context <- function() {
  list(
    app_name = "Sistema de Análise Multicritério ELECTRE Tri-B",
    app_description = "Análise de Secas e Desertificação usando método ELECTRE Tri-B",
    timestamp = Sys.time()
  )
}

# ---- Contexto: Pré-processamento --------------------------------------

build_preproc_context <- function(preproc_data) {
  context <- build_base_context()
  context$modulo <- "Pré-processamento"
  
  # Dados disponíveis
  if (!is.null(preproc_data$data) && is.reactive(preproc_data$data)) {
    data_sf <- tryCatch(preproc_data$data(), error = function(e) NULL)
    if (!is.null(data_sf) && inherits(data_sf, "sf")) {
      context$n_municipios <- nrow(data_sf)
      context$variaveis_disponiveis <- names(data_sf)
      context$n_variaveis <- length(names(data_sf))
    }
  }
  
  # Filtros aplicados
  if (!is.null(preproc_data$filtros_aplicados)) {
    filtros <- tryCatch(preproc_data$filtros_aplicados(), error = function(e) list())
    if (length(filtros) > 0) {
      context$filtros_ativos <- length(filtros)
      context$filtros_detalhes <- lapply(filtros, function(f) {
        list(
          campo = f$campo,
          operador = f$operador_nome %||% f$operador,
          valor = if(is.null(f$valor_texto)) as.character(f$valor) else f$valor_texto
        )
      })
    } else {
      context$filtros_ativos <- 0
    }
  }
  
  context$ajuda <- list(
    "Use filtros para selecionar municípios específicos",
    "Filtros compostos permitem condições complexas usando | (OU) e & (E)",
    "O construtor de colunas permite criar novas variáveis calculadas"
  )
  
  return(context)
}

# ---- Contexto: Análise ELECTRE (Parâmetros) --------------------------

build_electre_params_context <- function(input, data_info = NULL) {
  context <- build_base_context()
  context$modulo <- "Análise ELECTRE - Configuração de Parâmetros"
  
  # Critérios selecionados
  if (!is.null(input$criterios_selecionados)) {
    context$criterios <- input$criterios_selecionados
    context$n_criterios <- length(input$criterios_selecionados)
  }
  
  # Sentidos dos critérios
  sentidos <- list()
  if (!is.null(input$criterios_selecionados)) {
    for (crit in input$criterios_selecionados) {
      sentido_id <- paste0("sentido_", crit)
      if (!is.null(input[[sentido_id]])) {
        sentidos[[crit]] <- input[[sentido_id]]
      }
    }
  }
  if (length(sentidos) > 0) {
    context$sentidos <- sentidos
  }
  
  # Pesos
  pesos <- list()
  if (!is.null(input$criterios_selecionados)) {
    for (crit in input$criterios_selecionados) {
      peso_id <- paste0("peso_", crit)
      if (!is.null(input[[peso_id]])) {
        pesos[[crit]] <- input[[peso_id]]
      }
    }
  }
  if (length(pesos) > 0) {
    # Normalizar pesos
    soma_pesos <- sum(unlist(pesos))
    if (soma_pesos > 0) {
      pesos_norm <- lapply(pesos, function(p) round(p / soma_pesos, 3))
      context$pesos <- pesos
      context$pesos_normalizados <- pesos_norm
    }
  }
  
  # Perfis e Classes
  if (!is.null(input$n_classes)) {
    context$n_classes <- input$n_classes
    context$n_perfis <- input$n_classes - 1
  }
  
  if (!is.null(input$b_mode)) {
    context$metodo_perfis <- input$b_mode
  }
  
  # Limiares
  limiares <- list()
  if (!is.null(input$limiares_avancado) && input$limiares_avancado) {
    context$limiares_tipo <- "Por critério (avançado)"
    # Aqui você poderia capturar limiares específicos se necessário
  } else {
    context$limiares_tipo <- "Globais (padrão)"
    if (!is.null(input$q_val)) limiares$q <- input$q_val
    if (!is.null(input$p_val)) limiares$p <- input$p_val
    if (!is.null(input$v_val)) limiares$v <- input$v_val
  }
  if (length(limiares) > 0) {
    context$limiares <- limiares
  }
  
  # Lambda e Regra
  if (!is.null(input$lambda_cut)) {
    context$lambda <- input$lambda_cut
  }
  
  if (!is.null(input$rule)) {
    context$regra <- input$rule
    context$regra_descricao <- if(input$rule == "pc") {
      "Pessimista - Atribui à classe mais baixa em caso de empate"
    } else {
      "Otimista - Atribui à classe mais alta em caso de empate"
    }
  }
  
  # Informações sobre os dados
  if (!is.null(data_info)) {
    context$dados_info <- data_info
  }
  
  context$ajuda <- list(
    "ELECTRE Tri-B classifica alternativas em categorias pré-definidas",
    "Critérios são as variáveis de análise (sentido: benefício ou custo)",
    "Pesos definem a importância relativa de cada critério",
    "Perfis (B) são os limites entre as classes",
    "Limiares: q=indiferença, p=preferência, v=veto",
    "Lambda (λ) é o nível mínimo de credibilidade para classificação",
    "Regra pessimista é mais conservadora que a otimista"
  )
  
  return(context)
}

# ---- Contexto: Resultados ELECTRE ------------------------------------

build_electre_results_context <- function(resultados_data, input = NULL) {
  context <- build_base_context()
  context$modulo <- "Análise ELECTRE - Resultados"
  
  if (!is.null(resultados_data) && is.reactive(resultados_data)) {
    results <- tryCatch(resultados_data(), error = function(e) NULL)
    
    if (!is.null(results) && "classe_final" %in% names(results)) {
      # Distribuição de classes
      tab_classes <- table(results$classe_final)
      context$distribuicao_classes <- as.list(tab_classes)
      context$n_municipios_total <- nrow(results)
      
      # Classe dominante
      classe_dominante <- names(which.max(tab_classes))
      context$classe_dominante <- classe_dominante
      
      # Proporção de classes altas (C4 e C5)
      if (all(c("4", "5") %in% names(tab_classes))) {
        n_alto <- sum(tab_classes[c("4", "5")])
        prop_alto <- round(n_alto / nrow(results) * 100, 1)
        context$proporcao_alto <- paste0(prop_alto, "%")
        context$n_municipios_alto <- n_alto
      }
      
      # Estatísticas por classe
      if (!is.null(results$classe_final)) {
        stats_por_classe <- results %>%
          sf::st_drop_geometry() %>%
          group_by(classe_final) %>%
          summarise(
            n = n(),
            prop = round(n() / nrow(results) * 100, 1)
          ) %>%
          arrange(classe_final)
        
        context$estatisticas_classes <- as.list(stats_por_classe)
      }
    }
  }
  
  # Filtros aplicados nos resultados
  if (!is.null(input$filtros_resultados)) {
    filtros <- tryCatch(input$filtros_resultados(), error = function(e) list())
    if (length(filtros) > 0) {
      context$filtros_ativos <- length(filtros)
    }
  }
  
  context$ajuda <- list(
    "Classes mais altas (C4, C5) indicam maior vulnerabilidade/prioridade",
    "A distribuição de classes mostra o padrão geral da classificação",
    "Use os filtros para analisar subgrupos específicos",
    "O mapa permite visualizar a distribuição espacial das classes"
  )
  
  return(context)
}

# ---- Contexto: Qualificação Territorial ------------------------------

build_qualificacao_context <- function(qualif_data, input = NULL) {
  context <- build_base_context()
  context$modulo <- "Qualificação Territorial"
  
  if (!is.null(qualif_data) && is.reactive(qualif_data)) {
    qualif <- tryCatch(qualif_data(), error = function(e) NULL)
    
    if (!is.null(qualif)) {
      context$n_municipios <- nrow(qualif)
      
      # Camadas espaciais disponíveis
      camadas <- c("quilombolas", "assentamentos", "indigenas", 
                   "inst_ensino", "prisoes", "sementes")
      
      totais_camadas <- list()
      for (camada in camadas) {
        col_name <- paste0("total_", camada)
        if (col_name %in% names(qualif)) {
          total <- sum(qualif[[col_name]], na.rm = TRUE)
          totais_camadas[[camada]] <- total
        }
      }
      
      if (length(totais_camadas) > 0) {
        context$totais_camadas <- totais_camadas
      }
      
      # Estatísticas de qualificação
      if ("classe_qualificacao" %in% names(qualif)) {
        tab_qualif <- table(qualif$classe_qualificacao)
        context$distribuicao_qualificacao <- as.list(tab_qualif)
      }
    }
  }
  
  context$ajuda <- list(
    "Camadas espaciais representam áreas prioritárias ou vulneráveis",
    "Quilombolas: Comunidades quilombolas",
    "Assentamentos: Assentamentos rurais",
    "Indígenas: Terras indígenas",
    "Inst. Ensino: Instituições de ensino",
    "Prisões: Unidades prisionais",
    "Sementes: Bancos de sementes",
    "Interseções múltiplas indicam áreas de maior complexidade territorial"
  )
  
  return(context)
}

# ---- Contexto: Relatórios --------------------------------------------

build_relatorios_context <- function(input = NULL) {
  context <- build_base_context()
  context$modulo <- "Geração de Relatórios"
  
  # Seções selecionadas
  if (!is.null(input$secoes_relatorio)) {
    context$secoes_selecionadas <- input$secoes_relatorio
    context$n_secoes <- length(input$secoes_relatorio)
  }
  
  # Configurações de customização
  if (!is.null(input$titulo_relatorio)) {
    context$titulo <- input$titulo_relatorio
  }
  
  if (!is.null(input$paleta_relatorio)) {
    context$paleta_cores <- input$paleta_relatorio
  }
  
  context$ajuda <- list(
    "O relatório HTML é totalmente customizável",
    "Seções disponíveis: Resumo Executivo, Dados, Parâmetros, Resultados, Mapas, Tabelas",
    "Você pode adicionar texto personalizado em cada seção",
    "O relatório pode ser exportado e compartilhado",
    "Use cores consistentes com a identidade visual do seu projeto"
  )
  
  return(context)
}

# ---- Função Principal de Captura de Contexto -------------------------

capture_context <- function(current_tab, input, preproc_data = NULL, 
                           analise_data = NULL, qualif_data = NULL) {
  
  # Determinar qual contexto construir baseado na aba atual
  context <- switch(
    current_tab,
    "preproc" = if (!is.null(preproc_data)) {
      build_preproc_context(preproc_data)
    } else {
      build_base_context()
    },
    "parametros" = build_electre_params_context(input),
    "resultados" = if (!is.null(analise_data$results)) {
      build_electre_results_context(analise_data$results, input)
    } else {
      build_base_context()
    },
    "qualificacao" = if (!is.null(qualif_data)) {
      build_qualificacao_context(qualif_data, input)
    } else {
      build_base_context()
    },
    "relatorios" = build_relatorios_context(input),
    build_base_context()
  )
  
  return(context)
}
