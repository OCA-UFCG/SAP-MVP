# =====================================================================
# OUTPUTS - ABA QUALIFICAÇÃO
# =====================================================================

#' Criar outputs da aba Qualificação
#' 
#' @param output Shiny output
#' @param session Shiny session
#' @param ns Namespace function
#' @param input Shiny input
#' @param resultados_electre Reactive com resultados ELECTRE
#' @param data_sf Reactive com dados espaciais
#' @param dados_espaciais Reactive com camadas espaciais
#' @param label_map Reactive com labels das classes
#' @param paleta_cores Reactive com paleta de cores
#' @export
criar_outputs_qualificacao <- function(output, session, ns, input, resultados_electre,
                                       data_sf, dados_espaciais, label_map, paleta_cores) {
  
  # Carregar dados com colunas N_*
  dados_master <- reactive({
    tryCatch({
      master <- qs::qread("data/app_master_sf.qs")
      
      # Selecionar apenas CD_MUN e colunas N_*
      master_n <- master |>
        st_drop_geometry() |>
        select(CD_MUN, starts_with("N_"))
      
      master_n
    }, error = function(e) {
      showNotification(
        paste("Erro ao carregar dados master:", e$message),
        type = "error",
        duration = 5
      )
      NULL
    })
  })
  
  # Resultado ELECTRE como SF com colunas N_*
  resultado_electre_sf <- reactive({
    req(resultados_electre())
    req(data_sf())
    req(dados_master())
    
    results <- resultados_electre()$results
    sf_data <- data_sf()
    master_n <- dados_master()
    
    if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
      sf_results <- sf_data |>
        left_join(results |> select(CD_MUN, class_electre, class_label), by = "CD_MUN") |>
        left_join(master_n, by = "CD_MUN")
    } else {
      sf_results <- sf_data
      if (!is.null(results) && !is.null(sf_data) && 
          nrow(results) > 0 && nrow(sf_data) > 0 &&
          nrow(results) == nrow(sf_data)) {
        sf_results$class_electre <- results$class_electre
        sf_results$class_label <- results$class_label
      }
    }
    sf_results
  })
  
  # Sistema de filtros
  filtros_qualificacao_aplicados <- reactiveVal(list())
  
  criar_sistema_filtros_modal(
    session = session,
    ns = ns,
    id = "qualificacao",
    results_reactive = reactive({
      req(resultado_electre_sf())
      resultado_electre_sf()
    }),
    filtros_aplicados = filtros_qualificacao_aplicados
  )
  
  # Dados filtrados
  resultado_qualif_filtrado <- reactive({
    req(resultado_electre_sf())
    sf_result <- resultado_electre_sf()
    filtros <- filtros_qualificacao_aplicados()
    aplicar_filtros_em_df(sf_result, filtros)
  })
  
  # Armazenar dados processados
  qualificacao_data <- reactiveVal(NULL)
  
  # Limpar qualificação ao executar ELECTRE novamente
  observeEvent(resultados_electre(), {
    qualificacao_data(NULL)  # Limpar dados processados
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Processar ao clicar no botão
  observeEvent(input$run_qualificacao, {
    req(resultado_qualif_filtrado())
    req(input$camadas_qualificacao)
    
    if (length(input$camadas_qualificacao) == 0) {
      showNotification(
        "Selecione pelo menos uma camada territorial para processar.",
        type = "warning",
        duration = 3
      )
      return()
    }
    
    # Mostrar progresso
    withProgress(message = 'Processando qualificação territorial...', value = 0, {
      
      resultado_sf <- resultado_qualif_filtrado()
      
      # Verificar campos necessários
      if (!all(c("class_electre", "class_label") %in% names(resultado_sf))) {
        showNotification(
          "Erro: Dados de classificação não encontrados. Execute a análise ELECTRE primeiro.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      incProgress(0.2, detail = "Verificando dados...")
      
      # Mapeamento de camadas selecionadas para colunas
      mapa_colunas <- c(
        "quilombolas" = "N_Quilombolas",
        "assentamentos" = "N_Assentamentos",
        "indigenas" = "N_Terras_Indigenas",
        "ensino" = "N_Instituicoes_Ensino",
        "prisoes" = "N_Unidades_Prisionais",
        "sementes" = "N_Casas_Sementes",
        "prop_rurais" = "N_Propriedades_Rurais"
      )
      
      # Verificar se colunas existem
      colunas_necessarias <- mapa_colunas[input$camadas_qualificacao]
      if (!all(colunas_necessarias %in% names(resultado_sf))) {
        showNotification(
          paste("Colunas de interseção territorial não encontradas nos dados.",
                "Faltando:", paste(colunas_necessarias[!colunas_necessarias %in% names(resultado_sf)], collapse = ", ")),
          type = "error",
          duration = 5
        )
        return()
      }
      
      incProgress(0.3, detail = "Processando camadas...")
      
      # Processar dados baseado nas camadas selecionadas
      dados_processados <- list()
      
      n_camadas <- length(input$camadas_qualificacao)
      for (i in seq_along(input$camadas_qualificacao)) {
        camada <- input$camadas_qualificacao[i]
        coluna <- mapa_colunas[camada]
        
        incProgress(0.4 / n_camadas, detail = paste("Processando", camada, "..."))
        
        dados_filtrados <- resultado_sf |>
          filter(.data[[coluna]] > 0) |>
          mutate(
            tipo = case_when(
              camada == "quilombolas" ~ "Quilombolas",
              camada == "assentamentos" ~ "Assentamentos",
              camada == "indigenas" ~ "T. Indígenas",
              camada == "ensino" ~ "Inst. Ensino",
              camada == "prisoes" ~ "U. Prisionais",
              camada == "sementes" ~ "B. Sementes",
              camada == "prop_rurais" ~ "Prop. Rurais"
            ),
            n_intersecoes = .data[[coluna]]
          )
        
        if (nrow(dados_filtrados) > 0) {
          dados_processados[[camada]] <- dados_filtrados
        }
      }
      
      incProgress(0.1, detail = "Finalizando...")
      
      qualificacao_data(dados_processados)
      
      showNotification(
        "Qualificação territorial processada com sucesso!",
        type = "message",
        duration = 3
      )
    })
  })
  
  # Reactive que retorna os dados processados
  qualificacao <- reactive({
    qualificacao_data()
  })
  
  # Value boxes - TOTAL DE INTERSEÇÕES
  output$vb_quilombolas <- renderText({
    qual <- qualificacao()
    if (!is.null(qual$quilombolas)) {
      total <- sum(qual$quilombolas$N_Quilombolas, na.rm = TRUE)
      formatC(total, format = "d", big.mark = " ")
    } else { "0" }
  })
  
  output$vb_assentamentos <- renderText({
    qual <- qualificacao()
    if (!is.null(qual$assentamentos)) {
      total <- sum(qual$assentamentos$N_Assentamentos, na.rm = TRUE)
      formatC(total, format = "d", big.mark = " ")
    } else { "0" }
  })
  
  output$vb_indigenas <- renderText({
    qual <- qualificacao()
    if (!is.null(qual$indigenas)) {
      total <- sum(qual$indigenas$N_Terras_Indigenas, na.rm = TRUE)
      formatC(total, format = "d", big.mark = " ")
    } else { "0" }
  })
  
  output$vb_ensino <- renderText({
    qual <- qualificacao()
    if (!is.null(qual$ensino)) {
      total <- sum(qual$ensino$N_Instituicoes_Ensino, na.rm = TRUE)
      formatC(total, format = "d", big.mark = " ")
    } else { "0" }
  })
  
  output$vb_prisoes <- renderText({
    qual <- qualificacao()
    if (!is.null(qual$prisoes)) {
      total <- sum(qual$prisoes$N_Unidades_Prisionais, na.rm = TRUE)
      formatC(total, format = "d", big.mark = " ")
    } else { "0" }
  })
  
  output$vb_sementes <- renderText({
    qual <- qualificacao()
    if (!is.null(qual$sementes)) {
      total <- sum(qual$sementes$N_Casas_Sementes, na.rm = TRUE)
      formatC(total, format = "d", big.mark = " ")
    } else { "0" }
  })
  
  output$vb_prop_rurais <- renderText({
    qual <- qualificacao()
    if (!is.null(qual$prop_rurais)) {
      total <- sum(qual$prop_rurais$N_Propriedades_Rurais, na.rm = TRUE)
      formatC(total, format = "d", big.mark = " ")
    } else { "0" }
  })
  
  # Gráfico: Distribuição por classe ELECTRE - QUANTIDADE DE INTERSEÇÕES
  output$plot_qualif_classes <- renderPlotly({
    req(qualificacao())
    qual <- qualificacao()
    n_classes <- resultados_electre()$params$n_classes %||% 5
    labels_atuais <- label_map()
    
    df_list <- list()
    
    for (camada in names(qual)) {
      if (!is.null(qual[[camada]]) && nrow(qual[[camada]]) > 0) {
        df_list[[camada]] <- qual[[camada]] |> 
          st_drop_geometry() |>
          group_by(class_electre, tipo) |>
          summarise(n = sum(n_intersecoes, na.rm = TRUE), .groups = "drop")
      }
    }
    
    if (length(df_list) == 0) return(NULL)
    
    df_combined <- bind_rows(df_list) |>
      mutate(class_label = labels_atuais[as.character(class_electre)])
    
    plot_ly(df_combined, x = ~class_label, y = ~n, color = ~tipo, type = "bar",
            text = ~n, textposition = "outside") |>
      layout(barmode = "group", 
             xaxis = list(title = "Classe ELECTRE"),
             yaxis = list(title = "Quantidade de Interseções"), 
             legend = list(title = list(text = "Tipo")))
  })
  
  # Gráfico: Total de cada camada - QUANTIDADE TOTAL DE INTERSEÇÕES
  output$plot_qualif_total <- renderPlotly({
    req(qualificacao())
    qual <- qualificacao()
    
    nomes_display <- c(
      "quilombolas" = "Quilombolas",
      "assentamentos" = "Assentamentos",
      "indigenas" = "T. Indígenas",
      "ensino" = "Inst. Ensino",
      "prisoes" = "U. Prisionais",
      "sementes" = "B. Sementes",
      "prop_rurais" = "Prop. Rurais"
    )
    
    mapa_colunas <- c(
      "quilombolas" = "N_Quilombolas",
      "assentamentos" = "N_Assentamentos",
      "indigenas" = "N_Terras_Indigenas",
      "ensino" = "N_Instituicoes_Ensino",
      "prisoes" = "N_Unidades_Prisionais",
      "sementes" = "N_Casas_Sementes",
      "prop_rurais" = "N_Propriedades_Rurais"
    )
    
    totais <- data.frame(
      Camada = character(),
      Total = integer(),
      stringsAsFactors = FALSE
    )
    
    for (camada in names(qual)) {
      if (!is.null(qual[[camada]])) {
        coluna <- mapa_colunas[camada]
        total_intersecoes <- sum(qual[[camada]][[coluna]], na.rm = TRUE)
        
        totais <- rbind(totais, data.frame(
          Camada = nomes_display[camada],
          Total = total_intersecoes
        ))
      }
    }
    
    if (nrow(totais) == 0) return(NULL)
    
    cores_camadas <- c(
      "Quilombolas" = "#f39c12", 
      "Assentamentos" = "#27ae60",
      "T. Indígenas" = "#3498db", 
      "Inst. Ensino" = "#9b59b6",
      "U. Prisionais" = "#e74c3c", 
      "B. Sementes" = "#1abc9c",
      "Prop. Rurais" = "#8e44ad"
    )
    
    plot_ly(totais, labels = ~Camada, values = ~Total, type = "pie",
            marker = list(colors = cores_camadas[totais$Camada]),
            textposition = "inside", textinfo = "label+value+percent") |>
      layout(showlegend = TRUE, legend = list(orientation = "v"))
  })
  
  # Tabela estatísticas - QUANTIDADE DE INTERSEÇÕES POR CLASSE
  output$tab_estatisticas_qualif <- renderDT({
    req(qualificacao())
    qual <- qualificacao()
    n_classes <- resultados_electre()$params$n_classes %||% 5
    labels_atuais <- label_map()
    cores_atuais <- paleta_cores()
    
    stats_list <- list()
    for (i in 1:n_classes) {
      stats <- data.frame(
        Classe = labels_atuais[as.character(i)],
        Quilombolas = if (!is.null(qual$quilombolas)) {
          sum(st_drop_geometry(qual$quilombolas) |> 
                filter(class_electre == i) |> 
                pull(N_Quilombolas), na.rm = TRUE)
        } else 0,
        Assentamentos = if (!is.null(qual$assentamentos)) {
          sum(st_drop_geometry(qual$assentamentos) |> 
                filter(class_electre == i) |> 
                pull(N_Assentamentos), na.rm = TRUE)
        } else 0,
        `T. Indígenas` = if (!is.null(qual$indigenas)) {
          sum(st_drop_geometry(qual$indigenas) |> 
                filter(class_electre == i) |> 
                pull(N_Terras_Indigenas), na.rm = TRUE)
        } else 0,
        `Inst. Ensino` = if (!is.null(qual$ensino)) {
          sum(st_drop_geometry(qual$ensino) |> 
                filter(class_electre == i) |> 
                pull(N_Instituicoes_Ensino), na.rm = TRUE)
        } else 0,
        `U. Prisionais` = if (!is.null(qual$prisoes)) {
          sum(st_drop_geometry(qual$prisoes) |> 
                filter(class_electre == i) |> 
                pull(N_Unidades_Prisionais), na.rm = TRUE)
        } else 0,
        `B. Sementes` = if (!is.null(qual$sementes)) {
          sum(st_drop_geometry(qual$sementes) |> 
                filter(class_electre == i) |> 
                pull(N_Casas_Sementes), na.rm = TRUE)
        } else 0,
        `Prop. Rurais` = if (!is.null(qual$prop_rurais)) {
          sum(st_drop_geometry(qual$prop_rurais) |> 
                filter(class_electre == i) |> 
                pull(N_Propriedades_Rurais), na.rm = TRUE)
        } else 0,
        check.names = FALSE
      )
      stats$Total <- rowSums(stats[, -1])
      stats_list[[i]] <- stats
    }
    
    df_stats <- bind_rows(stats_list)
    cores_hex <- unname(cores_atuais[as.character(1:n_classes)])
    
    datatable(df_stats, rownames = FALSE,
              options = list(pageLength = n_classes, dom = "t", ordering = FALSE)) |>
      formatStyle("Classe",
                  backgroundColor = styleEqual(labels_atuais[1:n_classes], cores_hex),
                  fontWeight = "bold", color = "white")
  })
  
  # Tabela ranking municípios - COM QUANTIDADE DE INTERSEÇÕES
  # Tabela ranking municípios - COM QUANTIDADE DE INTERSEÇÕES
  output$tab_ranking_municipios <- renderDT({
    req(qualificacao())  # Usar dados processados ao invés de resultado_qualif_filtrado
    req(input$camadas_qualificacao)
    
    qual <- qualificacao()
    
    # Consolidar todos os municípios processados
    df_list <- list()
    
    for (camada in names(qual)) {
      if (!is.null(qual[[camada]]) && nrow(qual[[camada]]) > 0) {
        df_list[[camada]] <- qual[[camada]] |> 
          st_drop_geometry() |>
          select(CD_MUN, NM_MUN, class_label, starts_with("N_"))
      }
    }
    
    if (length(df_list) == 0) return(NULL)
    
    # Combinar todos os dados únicos por município
    df_combined <- bind_rows(df_list) |>
      distinct(CD_MUN, .keep_all = TRUE)
    
    # Mapeamento de camadas para colunas
    mapa_colunas <- c(
      "quilombolas" = "N_Quilombolas",
      "assentamentos" = "N_Assentamentos",
      "indigenas" = "N_Terras_Indigenas",
      "ensino" = "N_Instituicoes_Ensino",
      "prisoes" = "N_Unidades_Prisionais",
      "sementes" = "N_Casas_Sementes",
      "prop_rurais" = "N_Propriedades_Rurais"
    )
    
    # Determinar quais colunas mostrar baseado nas camadas selecionadas
    colunas_selecionadas <- mapa_colunas[input$camadas_qualificacao]
    
    # Criar ranking
    ranking <- df_combined |>
      mutate(
        Quilombolas = if ("quilombolas" %in% input$camadas_qualificacao) N_Quilombolas else 0,
        Assentamentos = if ("assentamentos" %in% input$camadas_qualificacao) N_Assentamentos else 0,
        `T. Indígenas` = if ("indigenas" %in% input$camadas_qualificacao) N_Terras_Indigenas else 0,
        `Inst. Ensino` = if ("ensino" %in% input$camadas_qualificacao) N_Instituicoes_Ensino else 0,
        `U. Prisionais` = if ("prisoes" %in% input$camadas_qualificacao) N_Unidades_Prisionais else 0,
        `B. Sementes` = if ("sementes" %in% input$camadas_qualificacao) N_Casas_Sementes else 0,
        `Prop. Rurais` = if ("prop_rurais" %in% input$camadas_qualificacao) N_Propriedades_Rurais else 0,
        Total = Quilombolas + Assentamentos + `T. Indígenas` + 
          `Inst. Ensino` + `U. Prisionais` + `B. Sementes` + `Prop. Rurais`
      ) |>
      filter(Total > 0) |>
      arrange(desc(Total)) |>
      mutate(`#` = row_number())
    
    # Selecionar colunas para exibição baseado nas camadas selecionadas
    colunas_exibir <- c("#", "Município", "Classe")
    
    if ("quilombolas" %in% input$camadas_qualificacao) colunas_exibir <- c(colunas_exibir, "Quilombolas")
    if ("assentamentos" %in% input$camadas_qualificacao) colunas_exibir <- c(colunas_exibir, "Assentamentos")
    if ("indigenas" %in% input$camadas_qualificacao) colunas_exibir <- c(colunas_exibir, "T. Indígenas")
    if ("ensino" %in% input$camadas_qualificacao) colunas_exibir <- c(colunas_exibir, "Inst. Ensino")
    if ("prisoes" %in% input$camadas_qualificacao) colunas_exibir <- c(colunas_exibir, "U. Prisionais")
    if ("sementes" %in% input$camadas_qualificacao) colunas_exibir <- c(colunas_exibir, "B. Sementes")
    if ("prop_rurais" %in% input$camadas_qualificacao) colunas_exibir <- c(colunas_exibir, "Prop. Rurais")
    
    colunas_exibir <- c(colunas_exibir, "Total")
    
    ranking_final <- ranking |>
      select(`#`, Município = NM_MUN, Classe = class_label,
             any_of(c("Quilombolas", "Assentamentos", "T. Indígenas", 
                      "Inst. Ensino", "U. Prisionais", "B. Sementes", 
                      "Prop. Rurais")),
             Total)
    
    n_cols <- ncol(ranking_final) - 1
    
    datatable(ranking_final, rownames = FALSE,
              options = list(
                pageLength = 10,
                order = list(list(n_cols, 'desc')),
                columnDefs = list(list(className = 'dt-center', targets = 0:n_cols))
              )) |>
      formatStyle("Total",
                  backgroundColor = styleInterval(c(5, 10, 20),
                                                  c("#ffffff", "#fff3cd", "#ffc107", "#ff9800")),
                  fontWeight = "bold") |>
      formatStyle("#",
                  backgroundColor = styleInterval(c(1, 2, 3),
                                                  c("#ffd700", "#c0c0c0", "#cd7f32", "#e8e8e8")),
                  fontWeight = "bold")
  })
  
  # Mapa qualificação - COM CAMADAS SELECIONADAS
  output$mapa_qualificacao <- renderLeaflet({
    req(qualificacao())
    req(resultado_qualif_filtrado())
    req(dados_espaciais())
    
    # NOVO: Verificar se deve gerar mapa
    if (!isTRUE(input$gerar_mapa)) {
      # Retornar mapa vazio com mensagem
      return(
        leaflet() |> 
          addProviderTiles(providers$CartoDB.Positron) |>
          setView(lng = -38, lat = -7, zoom = 6) |>
          addControl(
            html = "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.3);'>
                    <h4 style='margin: 0;'>⚠️ Mapa Desativado</h4>
                    <p style='margin: 5px 0 0 0;'>Ative o switch 'Gerar Mapa' para visualizar as camadas territoriais.</p>
                  </div>",
            position = "topright"
          )
      )
    }
    
    withProgress(message = 'Carregando mapa...', value = 0, {
      
      resultado_sf <- resultado_qualif_filtrado()
      espaciais <- dados_espaciais()
      qual <- qualificacao()
      camadas_processadas <- names(qual)
      
      n_classes <- resultados_electre()$params$n_classes %||% 5
      cores_hex <- unname(paleta_cores()[as.character(1:n_classes)])
      
      pal_mun <- colorFactor(palette = cores_hex, domain = 1:n_classes, na.color = "transparent")
      opacidade <- input$opacidade_camadas %||% 0.7
      
      incProgress(0.1, detail = "Iniciando mapa...")
      
      # Inicializar mapa
      m <- leaflet() |> addProviderTiles(providers$CartoDB.Positron)
      
      # Adicionar municípios de fundo
      m <- m |>
        addPolygons(
          data = resultado_sf, 
          fillColor = ~pal_mun(class_electre),
          fillOpacity = 0.3, 
          color = "#999999", 
          weight = 0.5, 
          group = "Municípios",
          label = ~paste0(NM_MUN, " - ", class_label)
        )
      
      incProgress(0.2, detail = "Adicionando camadas...")
      
      # Filtrar camadas espaciais que intersectam municípios filtrados
      grupos_visiveis <- c("Municípios")
      
      # MUDANÇA: trocar input$camadas_qualificacao por camadas_processadas em TODOS os if's
      
      # Quilombolas
      if ("quilombolas" %in% camadas_processadas && !is.null(espaciais$quilombolas)) {
        quilombolas_filtrados <- tryCatch({
          st_filter(espaciais$quilombolas, qual$quilombolas)
        }, error = function(e) NULL)
        
        if (!is.null(quilombolas_filtrados) && nrow(quilombolas_filtrados) > 0) {
          m <- m |>
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
          m <- m |>
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
          m <- m |>
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
      
      incProgress(0.3, detail = "Adicionando pontos...")
      
      # Instituições de Ensino
      if ("ensino" %in% camadas_processadas && !is.null(espaciais$ensino)) {
        ensino_filtrados <- tryCatch({
          ens <- st_filter(espaciais$ensino, qual$ensino)
          # Converter para pontos se necessário
          geom_type <- unique(as.character(st_geometry_type(ens)))
          if (any(grepl("POLYGON", geom_type))) {
            ens <- st_centroid(ens)
          }
          ens
        }, error = function(e) NULL)
        
        if (!is.null(ensino_filtrados) && nrow(ensino_filtrados) > 0) {
          m <- m |>
            addCircleMarkers(
              data = ensino_filtrados, 
              radius = 5, 
              fillColor = "#9b59b6",
              fillOpacity = opacidade, 
              color = "#7d3c98", 
              weight = 1,
              group = "Inst. Ensino",
              popup = ~paste0("<strong>Instituição de Ensino</strong><br/>", 
                              if("nome_feature" %in% names(ensino_filtrados)) nome_feature else "")
            )
          grupos_visiveis <- c(grupos_visiveis, "Inst. Ensino")
        }
      }
      
      # Unidades Prisionais
      if ("prisoes" %in% camadas_processadas && !is.null(espaciais$prisoes)) {
        prisoes_filtrados <- tryCatch({
          pris <- st_filter(espaciais$prisoes, qual$prisoes)
          # Converter para pontos se necessário
          geom_type <- unique(as.character(st_geometry_type(pris)))
          if (any(grepl("POLYGON", geom_type))) {
            pris <- st_centroid(pris)
          }
          pris
        }, error = function(e) NULL)
        
        if (!is.null(prisoes_filtrados) && nrow(prisoes_filtrados) > 0) {
          m <- m |>
            addCircleMarkers(
              data = prisoes_filtrados, 
              radius = 5, 
              fillColor = "#e74c3c",
              fillOpacity = opacidade, 
              color = "#c0392b", 
              weight = 1,
              group = "U. Prisionais",
              popup = ~paste0("<strong>Unidade Prisional</strong><br/>", 
                              if("nome_feature" %in% names(prisoes_filtrados)) nome_feature else "")
            )
          grupos_visiveis <- c(grupos_visiveis, "U. Prisionais")
        }
      }
      
      # Bancos de Sementes
      if ("sementes" %in% camadas_processadas && !is.null(espaciais$sementes)) {
        sementes_filtrados <- tryCatch({
          sem <- st_filter(espaciais$sementes, qual$sementes)
          # Converter para pontos se necessário
          geom_type <- unique(as.character(st_geometry_type(sem)))
          if (any(grepl("POLYGON", geom_type))) {
            sem <- st_centroid(sem)
          }
          sem
        }, error = function(e) NULL)
        
        if (!is.null(sementes_filtrados) && nrow(sementes_filtrados) > 0) {
          m <- m |>
            addCircleMarkers(
              data = sementes_filtrados, 
              radius = 5, 
              fillColor = "#1abc9c",
              fillOpacity = opacidade, 
              color = "#117a65", 
              weight = 1,
              group = "B. Sementes",
              popup = ~paste0("<strong>Banco de Sementes</strong><br/>", 
                              if("nome_feature" %in% names(sementes_filtrados)) nome_feature else "")
            )
          grupos_visiveis <- c(grupos_visiveis, "B. Sementes")
        }
      }
      
      incProgress(0.2, detail = "Finalizando mapa...")
      
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
          m <- m |>
            addPolygons(
              data = prop_rurais_filtrados, 
              fillColor = "#8e44ad", 
              fillOpacity = opacidade,
              color = "#6c3483", 
              weight = 1, 
              group = "Prop. Rurais",
              popup = ~paste0("<strong>Propriedade Rural</strong><br/>", 
                              if("nome_feature" %in% names(prop_rurais_filtrados)) nome_feature else "")
            )
          grupos_visiveis <- c(grupos_visiveis, "Prop. Rurais")
        }
      }
      
      # Adicionar controle de camadas
      m <- m |>
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
      indices_visiveis <- c(1) # Sempre mostrar municípios
      if ("Quilombolas" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 2)
      if ("Assentamentos" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 3)
      if ("T. Indígenas" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 4)
      if ("Inst. Ensino" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 5)
      if ("U. Prisionais" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 6)
      if ("B. Sementes" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 7)
      if ("Prop. Rurais" %in% grupos_visiveis) indices_visiveis <- c(indices_visiveis, 8)
      
      incProgress(0.2, detail = "Pronto!")
      
      m |>
        addLegend(
          position = "bottomright",
          colors = cores_legenda[indices_visiveis],
          labels = labels_legenda[indices_visiveis],
          title = "Camadas Territoriais", 
          opacity = 1
        )
    })
  })
  
  # Export mapa qualificação
  output$export_mapa_qualif <- downloadHandler(
    filename = function() paste0("mapa_qualificacao_", format(Sys.Date(), "%Y%m%d"), ".html"),
    content = function(file) {
      req(resultado_qualif_filtrado())
      req(dados_espaciais())
      req(input$camadas_qualificacao)
      
      withProgress(message = 'Exportando mapa...', value = 0, {
        
        resultado_sf <- resultado_qualif_filtrado()
        espaciais <- dados_espaciais()
        n_classes <- resultados_electre()$params$n_classes %||% 5
        cores_hex <- unname(paleta_cores()[as.character(1:n_classes)])
        
        pal_mun <- colorFactor(palette = cores_hex, domain = 1:n_classes, na.color = "transparent")
        opacidade <- input$opacidade_camadas %||% 0.7
        
        incProgress(0.2)
        
        m <- leaflet() |> 
          addProviderTiles(providers$CartoDB.Positron) |>
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
        
        incProgress(0.2)
        
        if ("quilombolas" %in% input$camadas_qualificacao && !is.null(espaciais$quilombolas)) {
          quilombolas_filtrados <- tryCatch(st_filter(espaciais$quilombolas, qual$quilombolas), error = function(e) NULL)
          if (!is.null(quilombolas_filtrados) && nrow(quilombolas_filtrados) > 0) {
            m <- m |> addPolygons(data = quilombolas_filtrados, fillColor = "#f39c12", 
                                  fillOpacity = opacidade, color = "#d68910", weight = 1, 
                                  group = "Quilombolas")
            grupos_visiveis <- c(grupos_visiveis, "Quilombolas")
          }
        }
        
        if ("assentamentos" %in% input$camadas_qualificacao && !is.null(espaciais$assentamentos)) {
          assentamentos_filtrados <- tryCatch(st_filter(espaciais$assentamentos, qual$assentamentos), error = function(e) NULL)
          if (!is.null(assentamentos_filtrados) && nrow(assentamentos_filtrados) > 0) {
            m <- m |> addPolygons(data = assentamentos_filtrados, fillColor = "#27ae60", 
                                  fillOpacity = opacidade, color = "#1e8449", weight = 1, 
                                  group = "Assentamentos")
            grupos_visiveis <- c(grupos_visiveis, "Assentamentos")
          }
        }
        
        if ("indigenas" %in% input$camadas_qualificacao && !is.null(espaciais$indigenas)) {
          indigenas_filtrados <- tryCatch(st_filter(espaciais$indigenas, qual$indigenas), error = function(e) NULL)
          if (!is.null(indigenas_filtrados) && nrow(indigenas_filtrados) > 0) {
            m <- m |> addPolygons(data = indigenas_filtrados, fillColor = "#3498db", 
                                  fillOpacity = opacidade, color = "#2874a6", weight = 1, 
                                  group = "T. Indígenas")
            grupos_visiveis <- c(grupos_visiveis, "T. Indígenas")
          }
        }
        
        incProgress(0.2)
        
        if ("ensino" %in% input$camadas_qualificacao && !is.null(espaciais$ensino)) {
          ensino_filtrados <- tryCatch({
            ens <- st_filter(espaciais$ensino, qual$ensino)
            geom_type <- unique(as.character(st_geometry_type(ens)))
            if (any(grepl("POLYGON", geom_type))) ens <- st_centroid(ens)
            ens
          }, error = function(e) NULL)
          if (!is.null(ensino_filtrados) && nrow(ensino_filtrados) > 0) {
            m <- m |> addCircleMarkers(data = ensino_filtrados, radius = 5, fillColor = "#9b59b6",
                                       fillOpacity = opacidade, color = "#7d3c98", weight = 1,
                                       group = "Inst. Ensino")
            grupos_visiveis <- c(grupos_visiveis, "Inst. Ensino")
          }
        }
        
        if ("prisoes" %in% input$camadas_qualificacao && !is.null(espaciais$prisoes)) {
          prisoes_filtrados <- tryCatch({
            pris <- st_filter(espaciais$prisoes, qual$prisoes)
            geom_type <- unique(as.character(st_geometry_type(pris)))
            if (any(grepl("POLYGON", geom_type))) pris <- st_centroid(pris)
            pris
          }, error = function(e) NULL)
          if (!is.null(prisoes_filtrados) && nrow(prisoes_filtrados) > 0) {
            m <- m |> addCircleMarkers(data = prisoes_filtrados, radius = 5, fillColor = "#e74c3c",
                                       fillOpacity = opacidade, color = "#c0392b", weight = 1,
                                       group = "U. Prisionais")
            grupos_visiveis <- c(grupos_visiveis, "U. Prisionais")
          }
        }
        
        if ("sementes" %in% input$camadas_qualificacao && !is.null(espaciais$sementes)) {
          sementes_filtrados <- tryCatch({
            sem <- st_filter(espaciais$sementes, qual$sementes)
            geom_type <- unique(as.character(st_geometry_type(sem)))
            if (any(grepl("POLYGON", geom_type))) sem <- st_centroid(sem)
            sem
          }, error = function(e) NULL)
          if (!is.null(sementes_filtrados) && nrow(sementes_filtrados) > 0) {
            m <- m |> addCircleMarkers(data = sementes_filtrados, radius = 5, fillColor = "#1abc9c",
                                       fillOpacity = opacidade, color = "#117a65", weight = 1,
                                       group = "B. Sementes")
            grupos_visiveis <- c(grupos_visiveis, "B. Sementes")
          }
        }
        
        incProgress(0.2)
        
        if ("prop_rurais" %in% input$camadas_qualificacao && !is.null(espaciais$prop_rurais)) {
          prop_rurais_filtrados <- tryCatch({
            prop_temp <- st_filter(espaciais$prop_rurais, qual$prop_rurais)
            geom_types <- st_geometry_type(prop_temp)
            if (any(grepl("GEOMETRYCOLLECTION", as.character(geom_types)))) {
              prop_temp <- st_collection_extract(prop_temp, "POLYGON")
              prop_temp <- st_make_valid(prop_temp)
              geom_types_final <- st_geometry_type(prop_temp)
              valid_geoms <- grepl("POLYGON|MULTIPOLYGON", as.character(geom_types_final))
              if (sum(valid_geoms) > 0) prop_temp[valid_geoms, ] else NULL
            } else prop_temp
          }, error = function(e) NULL)
          
          if (!is.null(prop_rurais_filtrados) && nrow(prop_rurais_filtrados) > 0) {
            m <- m |> addPolygons(data = prop_rurais_filtrados, fillColor = "#8e44ad", 
                                  fillOpacity = opacidade, color = "#6c3483", weight = 1, 
                                  group = "Prop. Rurais")
            grupos_visiveis <- c(grupos_visiveis, "Prop. Rurais")
          }
        }
        
        m <- m |> addLayersControl(overlayGroups = grupos_visiveis,
                                   options = layersControlOptions(collapsed = FALSE))
        
        incProgress(0.2)
        
        htmlwidgets::saveWidget(m, file, selfcontained = TRUE)
      })
    }
  )
  
  # Retornar para uso em relatórios
  return(list(
    intersecoes = reactive({
      qual <- qualificacao()
      if (is.null(qual)) return(NULL)
      qual
    })
  ))
}