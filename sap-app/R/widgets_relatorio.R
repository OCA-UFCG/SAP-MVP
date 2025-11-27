# =====================================================================
# WIDGETS PARA RELATÓRIOS - VERSÃO SIMPLIFICADA (REUSA CÓDIGO ANÁLISE)
# =====================================================================

# =====================================================================
# GRÁFICO: DENSIDADE POR CLASSE (CÓPIA DO MÓDULO ANÁLISE)
# =====================================================================

gerar_grafico_densidade <- function(dados, config, variavel = NULL) {
  
  require(plotly)
  require(dplyr)
  
  # Se não especificou variável, usar primeira numérica dos critérios
  if (is.null(variavel)) {
    # Pegar critérios dos params se disponível
    if (!is.null(config$criterios)) {
      variavel <- config$criterios[1]
    } else {
      cols_numericas <- names(dados)[sapply(dados, is.numeric)]
      cols_numericas <- setdiff(cols_numericas, c("CD_MUN", "class_electre"))
      if (length(cols_numericas) == 0) {
        stop("Nenhuma variável numérica disponível")
      }
      variavel <- cols_numericas[1]
    }
  }
  
  if (!variavel %in% names(dados)) {
    stop(paste("Variável", variavel, "não encontrada nos dados"))
  }
  
  # LÓGICA EXATA DO MÓDULO ANÁLISE (linhas 1960-1988 do mod_analise.R)
  n_classes <- config$n_classes %||% 5
  labels_atuais <- config$label_map
  cores_atuais <- config$paleta_cores
  
  p <- plot_ly()
  
  for (i in 1:n_classes) {
    df_classe <- dados %>%
      filter(class_electre == i, !is.na(.data[[variavel]]))
    
    if (nrow(df_classe) > 1) {
      dens <- density(df_classe[[variavel]], na.rm = TRUE)
      p <- p %>%
        add_trace(
          x = dens$x,
          y = dens$y,
          type = "scatter",
          mode = "lines",
          name = labels_atuais[as.character(i)],
          line = list(color = unname(cores_atuais[as.character(i)]), width = 2),
          fill = "tozeroy",
          fillcolor = paste0(unname(cores_atuais[as.character(i)]), "40")
        )
    }
  }
  
  p %>%
    layout(
      xaxis = list(title = variavel),
      yaxis = list(title = "Densidade"),
      legend = list(title = list(text = "Classe")),
      hovermode = "x unified"
    )
}

# =====================================================================
# GRÁFICO: REGIONAL (ADAPTADO - COM VERIFICAÇÃO)
# =====================================================================

gerar_grafico_regional <- function(dados, config) {
  
  require(plotly)
  require(dplyr)
  
  # Verificar se tem coluna UF (ordem de prioridade)
  col_uf <- NULL
  if ("UF" %in% names(dados)) {
    col_uf <- "UF"
  } else if ("SIGLA_UF" %in% names(dados)) {
    col_uf <- "SIGLA_UF"
  } else if ("NM_UF" %in% names(dados)) {  # ← ADICIONAR ESTA LINHA
    col_uf <- "NM_UF"                      # ← E ESTA
  } else {
    stop("Dados não possuem coluna UF, SIGLA_UF ou NM_UF")  # ← ATUALIZAR MENSAGEM
  }
  
  # LÓGICA SIMILAR AO MÓDULO ANÁLISE
  df_regional <- dados %>%
    count(.data[[col_uf]], class_electre) %>%
    mutate(
      class_label = config$label_map[as.character(class_electre)]
    )
  
  classes_presentes <- sort(unique(df_regional$class_electre))
  labels_presentes <- config$label_map[as.character(classes_presentes)]
  cores_map <- setNames(
    unname(config$paleta_cores[as.character(classes_presentes)]),
    labels_presentes
  )
  
  plot_ly(
    df_regional,
    x = ~get(col_uf),
    y = ~n,
    color = ~class_label,
    colors = cores_map,
    type = "bar",
    text = ~n,
    textposition = "inside"
  ) %>%
    layout(
      barmode = "stack",
      xaxis = list(title = col_uf),
      yaxis = list(title = "Frequência"),
      legend = list(title = list(text = "Classe"))
    )
}

# =====================================================================
# GRÁFICO: QUALIFICAÇÃO POR CLASSE (CÓPIA DO MÓDULO ANÁLISE)
# =====================================================================

gerar_grafico_qualif_classe <- function(dados, qualificacao, config) {
  
  require(plotly)
  require(dplyr)
  
  if (is.null(qualificacao) || length(qualificacao) == 0) {
    stop("Dados de qualificação não disponíveis")
  }
  
  # LÓGICA EXATA DO MÓDULO ANÁLISE (linhas 2494-2514)
  df_list <- list()
  
  if (!is.null(qualificacao$quilombolas) && nrow(qualificacao$quilombolas) > 0) {
    df_list$quilombolas <- qualificacao$quilombolas %>%
      sf::st_drop_geometry() %>%
      select(class_electre, tipo) %>%
      count(class_electre, tipo)
  }
  
  if (!is.null(qualificacao$assentamentos) && nrow(qualificacao$assentamentos) > 0) {
    df_list$assentamentos <- qualificacao$assentamentos %>%
      sf::st_drop_geometry() %>%
      select(class_electre, tipo) %>%
      count(class_electre, tipo)
  }
  
  if (!is.null(qualificacao$indigenas) && nrow(qualificacao$indigenas) > 0) {
    df_list$indigenas <- qualificacao$indigenas %>%
      sf::st_drop_geometry() %>%
      select(class_electre, tipo) %>%
      count(class_electre, tipo)
  }
  
  if (!is.null(qualificacao$ensino) && nrow(qualificacao$ensino) > 0) {
    df_list$ensino <- qualificacao$ensino %>%
      sf::st_drop_geometry() %>%
      select(class_electre, tipo) %>%
      count(class_electre, tipo)
  }
  
  if (!is.null(qualificacao$prisoes) && nrow(qualificacao$prisoes) > 0) {
    df_list$prisoes <- qualificacao$prisoes %>%
      sf::st_drop_geometry() %>%
      select(class_electre, tipo) %>%
      count(class_electre, tipo)
  }
  
  if (!is.null(qualificacao$sementes) && nrow(qualificacao$sementes) > 0) {
    df_list$sementes <- qualificacao$sementes %>%
      sf::st_drop_geometry() %>%
      select(class_electre, tipo) %>%
      count(class_electre, tipo)
  }
  
  if (length(df_list) == 0) {
    stop("Nenhuma interseção encontrada")
  }
  
  df_combined <- bind_rows(df_list) %>%
    mutate(class_label = config$label_map[as.character(class_electre)])
  
  plot_ly(
    df_combined,
    x = ~class_label,
    y = ~n,
    color = ~tipo,
    type = "bar",
    text = ~n,
    textposition = "outside"
  ) %>%
    layout(
      barmode = "group",
      xaxis = list(title = "Classe ELECTRE"),
      yaxis = list(title = "Quantidade"),
      legend = list(title = list(text = "Tipo"))
    )
}

# =====================================================================
# GRÁFICO: TOTAL DE QUALIFICAÇÕES (CÓPIA DO MÓDULO ANÁLISE)
# =====================================================================

gerar_grafico_qualif_total <- function(qualificacao, config) {
  
  require(plotly)
  require(dplyr)
  
  if (is.null(qualificacao) || length(qualificacao) == 0) {
    stop("Dados de qualificação não disponíveis")
  }
  
  # LÓGICA EXATA DO MÓDULO ANÁLISE (linhas 2520-2560)
  totais <- data.frame(
    Camada = character(),
    Total = integer(),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(qualificacao$quilombolas)) {
    totais <- rbind(totais, data.frame(
      Camada = "Quilombolas",
      Total = nrow(qualificacao$quilombolas)
    ))
  }
  
  if (!is.null(qualificacao$assentamentos)) {
    totais <- rbind(totais, data.frame(
      Camada = "Assentamentos",
      Total = nrow(qualificacao$assentamentos)
    ))
  }
  
  if (!is.null(qualificacao$indigenas)) {
    totais <- rbind(totais, data.frame(
      Camada = "T. Indígenas",
      Total = nrow(qualificacao$indigenas)
    ))
  }
  
  if (!is.null(qualificacao$ensino)) {
    totais <- rbind(totais, data.frame(
      Camada = "Inst. Ensino",
      Total = nrow(qualificacao$ensino)
    ))
  }
  
  if (!is.null(qualificacao$prisoes)) {
    totais <- rbind(totais, data.frame(
      Camada = "U. Prisionais",
      Total = nrow(qualificacao$prisoes)
    ))
  }
  
  if (!is.null(qualificacao$sementes)) {
    totais <- rbind(totais, data.frame(
      Camada = "B. Sementes",
      Total = nrow(qualificacao$sementes)
    ))
  }
  
  if (nrow(totais) == 0) {
    stop("Nenhuma qualificação encontrada")
  }
  
  cores_camadas <- c(
    "Quilombolas" = "#f39c12",
    "Assentamentos" = "#27ae60",
    "T. Indígenas" = "#3498db",
    "Inst. Ensino" = "#9b59b6",
    "U. Prisionais" = "#e74c3c",
    "B. Sementes" = "#16a085"
  )
  
  plot_ly(
    totais,
    labels = ~Camada,
    values = ~Total,
    type = "pie",
    marker = list(colors = cores_camadas[totais$Camada]),
    textposition = "inside",
    textinfo = "label+value+percent"
  ) %>%
    layout(
      showlegend = TRUE,
      legend = list(orientation = "v")
    )
}

# =====================================================================
# MAPA: QUALIFICAÇÃO COMPLETO (CÓPIA EXATA DO MÓDULO ANÁLISE)
# =====================================================================

gerar_mapa_qualificacao_completo <- function(dados_sf, qualificacao, config, dados_espaciais = NULL) {
  
  require(leaflet)
  require(sf)
  
  if (!inherits(dados_sf, "sf")) {
    stop("dados_sf deve ser um objeto sf com geometrias")
  }
  
  if (is.null(qualificacao) || length(qualificacao) == 0) {
    stop("Dados de qualificação não disponíveis")
  }
  
  if (is.null(dados_espaciais)) {
    stop("dados_espaciais é obrigatório para gerar o mapa de qualificação")
  }
  
  # === CÓDIGO ADAPTADO DA ABA ANÁLISE ===
  
  resultado_sf <- dados_sf
  espaciais <- dados_espaciais
  qual <- qualificacao
  camadas_processadas <- names(qual)
  
  n_classes <- config$n_classes %||% 5
  cores_hex <- unname(config$paleta_cores[as.character(1:n_classes)])
  
  pal_mun <- colorFactor(palette = cores_hex, domain = 1:n_classes, na.color = "transparent")
  opacidade <- 0.7
  
  # Inicializar mapa
  m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
  
  # Adicionar municípios de fundo
  m <- m %>%
    addPolygons(
      data = resultado_sf, 
      fillColor = ~pal_mun(class_electre),
      fillOpacity = 0.3, 
      color = "#999999", 
      weight = 0.5, 
      group = "Municípios",
      label = ~paste0(NM_MUN, " - ", class_label)
    )
  
  grupos_visiveis <- c("Municípios")
  
  # Quilombolas
  if ("quilombolas" %in% camadas_processadas && !is.null(espaciais$quilombolas)) {
    quilombolas_filtrados <- tryCatch({
      st_filter(espaciais$quilombolas, qual$quilombolas)
    }, error = function(e) NULL)
    
    if (!is.null(quilombolas_filtrados) && nrow(quilombolas_filtrados) > 0) {
      m <- m %>%
        addPolygons(
          data = quilombolas_filtrados, 
          fillColor = "#f39c12", 
          fillOpacity = opacidade,
          color = "#d68910", 
          weight = 1, 
          group = "Quilombolas",
          popup = ~paste0("<strong>Quilombola</strong>")
        )
      grupos_visiveis <- c(grupos_visiveis, "Quilombolas")
    }
  }
  
  # Assentamentos
  if ("assentamentos" %in% camadas_processadas && !is.null(espaciais$assentamentos)) {
    assentamentos_filtrados <- tryCatch({
      st_filter(espaciais$assentamentos, qual$assentamentos)
    }, error = function(e) NULL)
    
    if (!is.null(assentamentos_filtrados) && nrow(assentamentos_filtrados) > 0) {
      m <- m %>%
        addPolygons(
          data = assentamentos_filtrados, 
          fillColor = "#27ae60", 
          fillOpacity = opacidade,
          color = "#1e8449", 
          weight = 1, 
          group = "Assentamentos",
          popup = ~paste0("<strong>Assentamento</strong>")
        )
      grupos_visiveis <- c(grupos_visiveis, "Assentamentos")
    }
  }
  
  # Terras Indígenas
  if ("indigenas" %in% camadas_processadas && !is.null(espaciais$indigenas)) {
    indigenas_filtrados <- tryCatch({
      st_filter(espaciais$indigenas, qual$indigenas)
    }, error = function(e) NULL)
    
    if (!is.null(indigenas_filtrados) && nrow(indigenas_filtrados) > 0) {
      m <- m %>%
        addPolygons(
          data = indigenas_filtrados, 
          fillColor = "#3498db", 
          fillOpacity = opacidade,
          color = "#2874a6", 
          weight = 1, 
          group = "T. Indígenas",
          popup = ~paste0("<strong>Território Indígena</strong>")
        )
      grupos_visiveis <- c(grupos_visiveis, "T. Indígenas")
    }
  }
  
  # Instituições de Ensino
  if ("ensino" %in% camadas_processadas && !is.null(espaciais$ensino)) {
    ensino_filtrados <- tryCatch({
      ens <- st_filter(espaciais$ensino, qual$ensino)
      geom_type <- unique(as.character(st_geometry_type(ens)))
      if (any(grepl("POLYGON", geom_type))) {
        ens <- st_centroid(ens)
      }
      ens
    }, error = function(e) NULL)
    
    if (!is.null(ensino_filtrados) && nrow(ensino_filtrados) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = ensino_filtrados, 
          radius = 5, 
          fillColor = "#9b59b6",
          fillOpacity = opacidade, 
          color = "#7d3c98", 
          weight = 1,
          group = "Inst. Ensino",
          popup = ~paste0("<strong>Instituição de Ensino</strong>")
        )
      grupos_visiveis <- c(grupos_visiveis, "Inst. Ensino")
    }
  }
  
  # Unidades Prisionais
  if ("prisoes" %in% camadas_processadas && !is.null(espaciais$prisoes)) {
    prisoes_filtrados <- tryCatch({
      pris <- st_filter(espaciais$prisoes, qual$prisoes)
      geom_type <- unique(as.character(st_geometry_type(pris)))
      if (any(grepl("POLYGON", geom_type))) {
        pris <- st_centroid(pris)
      }
      pris
    }, error = function(e) NULL)
    
    if (!is.null(prisoes_filtrados) && nrow(prisoes_filtrados) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = prisoes_filtrados, 
          radius = 5, 
          fillColor = "#e74c3c",
          fillOpacity = opacidade, 
          color = "#c0392b", 
          weight = 1,
          group = "U. Prisionais",
          popup = ~paste0("<strong>Unidade Prisional</strong>")
        )
      grupos_visiveis <- c(grupos_visiveis, "U. Prisionais")
    }
  }
  
  # Bancos de Sementes
  if ("sementes" %in% camadas_processadas && !is.null(espaciais$sementes)) {
    sementes_filtrados <- tryCatch({
      sem <- st_filter(espaciais$sementes, qual$sementes)
      geom_type <- unique(as.character(st_geometry_type(sem)))
      if (any(grepl("POLYGON", geom_type))) {
        sem <- st_centroid(sem)
      }
      sem
    }, error = function(e) NULL)
    
    if (!is.null(sementes_filtrados) && nrow(sementes_filtrados) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = sementes_filtrados, 
          radius = 5, 
          fillColor = "#1abc9c",
          fillOpacity = opacidade, 
          color = "#117a65", 
          weight = 1,
          group = "B. Sementes",
          popup = ~paste0("<strong>Banco de Sementes</strong>")
        )
      grupos_visiveis <- c(grupos_visiveis, "B. Sementes")
    }
  }
  
  # Propriedades Rurais
  if ("prop_rurais" %in% camadas_processadas && !is.null(espaciais$prop_rurais)) {
    prop_rurais_filtrados <- tryCatch({
      prop_temp <- st_filter(espaciais$prop_rurais, qual$prop_rurais)
      geom_types <- st_geometry_type(prop_temp)
      
      if (any(grepl("GEOMETRYCOLLECTION", as.character(geom_types)))) {
        prop_temp <- st_collection_extract(prop_temp, "POLYGON")
        prop_temp <- st_make_valid(prop_temp)
        geom_types_final <- st_geometry_type(prop_temp)
        valid_geoms <- grepl("POLYGON|MULTIPOLYGON", as.character(geom_types_final))
        if (sum(valid_geoms) > 0) {
          prop_temp[valid_geoms, ]
        } else {
          NULL
        }
      } else {
        prop_temp
      }
    }, error = function(e) NULL)
    
    if (!is.null(prop_rurais_filtrados) && nrow(prop_rurais_filtrados) > 0) {
      m <- m %>%
        addPolygons(
          data = prop_rurais_filtrados, 
          fillColor = "#8e44ad", 
          fillOpacity = opacidade,
          color = "#6c3483", 
          weight = 1, 
          group = "Prop. Rurais",
          popup = ~paste0("<strong>Propriedade Rural</strong>")
        )
      grupos_visiveis <- c(grupos_visiveis, "Prop. Rurais")
    }
  }
  
  # Adicionar controle de camadas
  m <- m %>%
    addLayersControl(
      overlayGroups = grupos_visiveis,
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # Adicionar legenda
  cores_legenda <- c("#999999", "#f39c12", "#27ae60", "#3498db",
                     "#9b59b6", "#e74c3c", "#1abc9c", "#8e44ad")
  labels_legenda <- c("Municípios (fundo)", "Quilombolas", "Assentamentos",
                      "T. Indígenas", "Inst. Ensino", "U. Prisionais", 
                      "B. Sementes", "Prop. Rurais")
  
  # Filtrar apenas as camadas visíveis na legenda
  indices_visiveis <- c(1)
  if ("Quilombolas" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 2)
  if ("Assentamentos" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 3)
  if ("T. Indígenas" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 4)
  if ("Inst. Ensino" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 5)
  if ("U. Prisionais" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 6)
  if ("B. Sementes" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 7)
  if ("Prop. Rurais" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 8)
  
  m %>%
    addLegend(
      position = "bottomright",
      colors = cores_legenda[indices_visiveis],
      labels = labels_legenda[indices_visiveis],
      title = "Camadas Territoriais", 
      opacity = 1
    )
}

# =====================================================================
# TABELAS (CÓPIAS DO MÓDULO ANÁLISE)
# =====================================================================

gerar_tabela_perfil_medio <- function(dados, config) {
  require(DT)
  require(dplyr)
  
  # Remover geometria
  if (inherits(dados, "sf")) {
    dados <- sf::st_drop_geometry(dados)
  }
  
  # Pegar critérios se disponível
  crits <- config$criterios %||% {
    cols_numericas <- names(dados)[sapply(dados, is.numeric)]
    setdiff(cols_numericas, c("CD_MUN", "class_electre"))
  }
  
  if (length(crits) == 0) {
    stop("Nenhuma variável numérica disponível")
  }
  
  n_classes <- config$n_classes %||% 5
  
  # LÓGICA DO MÓDULO ANÁLISE (linhas 1991-2004)
  df_perfil <- dados %>%
    group_by(class_electre) %>%
    summarise(across(all_of(crits), ~mean(.x, na.rm = TRUE)), n = n(), .groups = "drop") %>%
    mutate(Classe = config$label_map[as.character(class_electre)], N = n) %>%
    select(Classe, N, all_of(crits))
  
  datatable(
    df_perfil,
    rownames = FALSE,
    options = list(
      pageLength = n_classes,
      dom = "t",
      ordering = FALSE
    )
  ) %>%
    formatRound(columns = crits, digits = 2) %>%
    formatStyle(
      "Classe",
      backgroundColor = styleEqual(
        config$label_map[1:n_classes],
        config$paleta_cores[1:n_classes]
      ),
      fontWeight = "bold",
      color = "white"
    )
}

gerar_tabela_estatisticas_qualif <- function(dados, qualificacao, config) {
  require(DT)
  require(dplyr)
  
  if (is.null(qualificacao) || length(qualificacao) == 0) {
    stop("Dados de qualificação não disponíveis")
  }
  
  # LÓGICA EXATA DO MÓDULO ANÁLISE (linhas 2564-2612)
  n_classes <- config$n_classes %||% 5
  cores_hex <- unname(config$paleta_cores[as.character(1:n_classes)])
  
  stats_list <- list()
  
  for (i in 1:n_classes) {
    stats <- data.frame(
      Classe = config$label_map[as.character(i)],
      Quilombolas = 0,
      Assentamentos = 0,
      `T. Indígenas` = 0,
      `Inst. Ensino` = 0,
      `U. Prisionais` = 0,
      `B. Sementes` = 0,
      check.names = FALSE
    )
    
    if (!is.null(qualificacao$quilombolas)) {
      stats$Quilombolas <- sum(
        sf::st_drop_geometry(qualificacao$quilombolas)$class_electre == i,
        na.rm = TRUE
      )
    }
    
    if (!is.null(qualificacao$assentamentos)) {
      stats$Assentamentos <- sum(
        sf::st_drop_geometry(qualificacao$assentamentos)$class_electre == i,
        na.rm = TRUE
      )
    }
    
    if (!is.null(qualificacao$indigenas)) {
      stats$`T. Indígenas` <- sum(
        sf::st_drop_geometry(qualificacao$indigenas)$class_electre == i,
        na.rm = TRUE
      )
    }
    
    if (!is.null(qualificacao$ensino)) {
      stats$`Inst. Ensino` <- sum(
        sf::st_drop_geometry(qualificacao$ensino)$class_electre == i,
        na.rm = TRUE
      )
    }
    
    if (!is.null(qualificacao$prisoes)) {
      stats$`U. Prisionais` <- sum(
        sf::st_drop_geometry(qualificacao$prisoes)$class_electre == i,
        na.rm = TRUE
      )
    }
    
    if (!is.null(qualificacao$sementes)) {
      stats$`B. Sementes` <- sum(
        sf::st_drop_geometry(qualificacao$sementes)$class_electre == i,
        na.rm = TRUE
      )
    }
    
    stats$Total <- rowSums(stats[, -1])
    stats_list[[i]] <- stats
  }
  
  df_stats <- bind_rows(stats_list)
  
  datatable(
    df_stats,
    rownames = FALSE,
    options = list(
      pageLength = n_classes,
      dom = "t",
      ordering = FALSE
    )
  ) %>%
    formatStyle(
      "Classe",
      backgroundColor = styleEqual(config$label_map[1:n_classes], cores_hex),
      fontWeight = "bold",
      color = "white"
    )
}

gerar_tabela_ranking <- function(dados, config, top_n = 20) {
  require(DT)
  require(dplyr)
  
  if (inherits(dados, "sf")) {
    dados <- sf::st_drop_geometry(dados)
  }
  
  cols_interesse <- c("NM_MUN", "UF", "SIGLA_UF", "class_electre", "class_label")
  cols_disponiveis <- intersect(cols_interesse, names(dados))
  
  df_ranking <- dados %>%
    select(all_of(cols_disponiveis)) %>%
    arrange(class_electre) %>%
    head(top_n) %>%
    mutate(
      ranking = row_number(),
      classe = config$label_map[as.character(class_electre)]
    )
  
  if ("NM_MUN" %in% names(df_ranking)) {
    df_ranking <- df_ranking %>%
      select(ranking, municipio = NM_MUN, classe, everything(), -class_electre, -class_label)
  }
  
  datatable(
    df_ranking,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = "tip"
    ),
    class = "table table-striped table-hover"
  ) %>%
    formatStyle(
      "classe",
      backgroundColor = styleEqual(
        config$label_map,
        unname(config$paleta_cores)
      ),
      color = "white",
      fontWeight = "bold"
    )
}

gerar_tabela_parametros <- function(params) {
  require(DT)
  
  df_params <- data.frame(
    Parametro = c(
      "Número de Classes",
      "Lambda (λ)",
      "Regra de Classificação",
      "Critérios Utilizados",
      "Data da Análise"
    ),
    Valor = c(
      params$n_classes,
      params$lambda,
      params$rule,
      paste(params$criterios, collapse = ", "),
      format(Sys.Date(), "%d/%m/%Y")
    ),
    Descricao = c(
      "Número de categorias na classificação",
      "Limiar de concordância para classificação",
      "Regra de atribuição (pessimista ou otimista)",
      "Variáveis consideradas na análise",
      "Data de geração deste relatório"
    )
  )
  
  datatable(
    df_params,
    rownames = FALSE,
    options = list(
      pageLength = nrow(df_params),
      scrollX = TRUE,
      dom = "t",
      ordering = FALSE
    ),
    class = "table table-bordered"
  ) %>%
    formatStyle(
      "Parametro",
      fontWeight = "bold",
      backgroundColor = "#f8f9fa"
    )
}