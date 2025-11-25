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
  
  # Resultado ELECTRE como SF
  resultado_electre_sf <- reactive({
    req(resultados_electre())
    req(data_sf())
    results <- resultados_electre()$results
    sf_data <- data_sf()
    
    if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
      sf_results <- sf_data |>
        left_join(results |> select(CD_MUN, class_electre, class_label), by = "CD_MUN")
    } else {
      sf_results <- sf_data
      if (nrow(results) == nrow(sf_data)) {
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
  
  # Interseções espaciais
  intersecoes <- reactive({
    req(dados_espaciais())
    req(resultado_qualif_filtrado())
    
    calcular_intersecoes(dados_espaciais(), resultado_qualif_filtrado())
  })
  
  # Value boxes
  output$vb_quilombolas <- renderText({
    inter <- intersecoes()
    if (!is.null(inter$quilombolas) && nrow(inter$quilombolas) > 0) {
      formatC(nrow(inter$quilombolas), format = "d", big.mark = ".")
    } else { "0" }
  })
  
  output$vb_assentamentos <- renderText({
    inter <- intersecoes()
    if (!is.null(inter$assentamentos) && nrow(inter$assentamentos) > 0) {
      formatC(nrow(inter$assentamentos), format = "d", big.mark = ".")
    } else { "0" }
  })
  
  output$vb_indigenas <- renderText({
    inter <- intersecoes()
    if (!is.null(inter$indigenas) && nrow(inter$indigenas) > 0) {
      formatC(nrow(inter$indigenas), format = "d", big.mark = ".")
    } else { "0" }
  })
  
  output$vb_ensino <- renderText({
    inter <- intersecoes()
    if (!is.null(inter$ensino) && nrow(inter$ensino) > 0) {
      formatC(nrow(inter$ensino), format = "d", big.mark = ".")
    } else { "0" }
  })
  
  output$vb_prisoes <- renderText({
    inter <- intersecoes()
    if (!is.null(inter$prisoes) && nrow(inter$prisoes) > 0) {
      formatC(nrow(inter$prisoes), format = "d", big.mark = ".")
    } else { "0" }
  })
  
  output$vb_sementes <- renderText({
    inter <- intersecoes()
    if (!is.null(inter$sementes) && nrow(inter$sementes) > 0) {
      formatC(nrow(inter$sementes), format = "d", big.mark = ".")
    } else { "0" }
  })
  
  # Gráfico: Distribuição por classe ELECTRE
  output$plot_qualif_classes <- renderPlotly({
    req(intersecoes())
    inter <- intersecoes()
    n_classes <- resultados_electre()$params$n_classes %||% 5
    labels_atuais <- label_map()
    
    df_list <- list()
    if (!is.null(inter$quilombolas) && nrow(inter$quilombolas) > 0) {
      df_list$quilombolas <- inter$quilombolas |> st_drop_geometry() |>
        select(class_electre, tipo) |> count(class_electre, tipo)
    }
    if (!is.null(inter$assentamentos) && nrow(inter$assentamentos) > 0) {
      df_list$assentamentos <- inter$assentamentos |> st_drop_geometry() |>
        select(class_electre, tipo) |> count(class_electre, tipo)
    }
    if (!is.null(inter$indigenas) && nrow(inter$indigenas) > 0) {
      df_list$indigenas <- inter$indigenas |> st_drop_geometry() |>
        select(class_electre, tipo) |> count(class_electre, tipo)
    }
    if (!is.null(inter$ensino) && nrow(inter$ensino) > 0) {
      df_list$ensino <- inter$ensino |> st_drop_geometry() |>
        select(class_electre, tipo) |> count(class_electre, tipo)
    }
    if (!is.null(inter$prisoes) && nrow(inter$prisoes) > 0) {
      df_list$prisoes <- inter$prisoes |> st_drop_geometry() |>
        select(class_electre, tipo) |> count(class_electre, tipo)
    }
    if (!is.null(inter$sementes) && nrow(inter$sementes) > 0) {
      df_list$sementes <- inter$sementes |> st_drop_geometry() |>
        select(class_electre, tipo) |> count(class_electre, tipo)
    }
    
    if (length(df_list) == 0) return(NULL)
    
    df_combined <- bind_rows(df_list) |>
      mutate(class_label = labels_atuais[as.character(class_electre)])
    
    plot_ly(df_combined, x = ~class_label, y = ~n, color = ~tipo, type = "bar",
            text = ~n, textposition = "outside") |>
      layout(barmode = "group", xaxis = list(title = "Classe ELECTRE"),
             yaxis = list(title = "Quantidade"), legend = list(title = list(text = "Tipo")))
  })
  
  # Gráfico: Total de cada camada
  output$plot_qualif_total <- renderPlotly({
    req(intersecoes())
    inter <- intersecoes()
    
    totais <- data.frame(Camada = character(), Total = integer(), stringsAsFactors = FALSE)
    
    if (!is.null(inter$quilombolas)) {
      totais <- rbind(totais, data.frame(Camada = "Quilombolas", Total = nrow(inter$quilombolas)))
    }
    if (!is.null(inter$assentamentos)) {
      totais <- rbind(totais, data.frame(Camada = "Assentamentos", Total = nrow(inter$assentamentos)))
    }
    if (!is.null(inter$indigenas)) {
      totais <- rbind(totais, data.frame(Camada = "T. Indígenas", Total = nrow(inter$indigenas)))
    }
    if (!is.null(inter$ensino)) {
      totais <- rbind(totais, data.frame(Camada = "Inst. Ensino", Total = nrow(inter$ensino)))
    }
    if (!is.null(inter$prisoes)) {
      totais <- rbind(totais, data.frame(Camada = "U. Prisionais", Total = nrow(inter$prisoes)))
    }
    if (!is.null(inter$sementes)) {
      totais <- rbind(totais, data.frame(Camada = "B. Sementes", Total = nrow(inter$sementes)))
    }
    
    if (nrow(totais) == 0) return(NULL)
    
    cores_camadas <- c("Quilombolas" = "#f39c12", "Assentamentos" = "#27ae60",
                       "T. Indígenas" = "#3498db", "Inst. Ensino" = "#9b59b6",
                       "U. Prisionais" = "#e74c3c", "B. Sementes" = "#16a085")
    
    plot_ly(totais, labels = ~Camada, values = ~Total, type = "pie",
            marker = list(colors = cores_camadas[totais$Camada]),
            textposition = "inside", textinfo = "label+value+percent") |>
      layout(showlegend = TRUE, legend = list(orientation = "v"))
  })
  
  # Tabela estatísticas
  output$tab_estatisticas_qualif <- renderDT({
    req(intersecoes())
    inter <- intersecoes()
    n_classes <- resultados_electre()$params$n_classes %||% 5
    labels_atuais <- label_map()
    cores_atuais <- paleta_cores()
    
    stats_list <- list()
    for (i in 1:n_classes) {
      stats <- data.frame(Classe = labels_atuais[as.character(i)],
                          Quilombolas = 0, Assentamentos = 0, `T. Indígenas` = 0,
                          `Inst. Ensino` = 0, `U. Prisionais` = 0, `B. Sementes` = 0,
                          check.names = FALSE)
      
      if (!is.null(inter$quilombolas)) {
        stats$Quilombolas <- sum(st_drop_geometry(inter$quilombolas)$class_electre == i, na.rm = TRUE)
      }
      if (!is.null(inter$assentamentos)) {
        stats$Assentamentos <- sum(st_drop_geometry(inter$assentamentos)$class_electre == i, na.rm = TRUE)
      }
      if (!is.null(inter$indigenas)) {
        stats$`T. Indígenas` <- sum(st_drop_geometry(inter$indigenas)$class_electre == i, na.rm = TRUE)
      }
      if (!is.null(inter$ensino)) {
        stats$`Inst. Ensino` <- sum(st_drop_geometry(inter$ensino)$class_electre == i, na.rm = TRUE)
      }
      if (!is.null(inter$prisoes)) {
        stats$`U. Prisionais` <- sum(st_drop_geometry(inter$prisoes)$class_electre == i, na.rm = TRUE)
      }
      if (!is.null(inter$sementes)) {
        stats$`B. Sementes` <- sum(st_drop_geometry(inter$sementes)$class_electre == i, na.rm = TRUE)
      }
      
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
  
  # Tabela ranking municípios
  output$tab_ranking_municipios <- renderDT({
    req(intersecoes())
    req(resultado_qualif_filtrado())
    
    ranking <- criar_ranking_municipios(intersecoes(), resultado_qualif_filtrado())
    
    datatable(ranking, rownames = FALSE,
              options = list(pageLength = 10, order = list(list(0, 'asc')),
                             columnDefs = list(list(className = 'dt-center', targets = 0:8)))) |>
      formatStyle("Total",
                  backgroundColor = styleInterval(c(5, 10, 20),
                                                  c("#ffffff", "#fff3cd", "#ffc107", "#ff9800")),
                  fontWeight = "bold") |>
      formatStyle("#",
                  backgroundColor = styleInterval(c(1, 2, 3),
                                                  c("#ffd700", "#c0c0c0", "#cd7f32", "#e8e8e8")),
                  fontWeight = "bold")
  })
  
  # Mapa qualificação
  output$mapa_qualificacao <- renderLeaflet({
    req(resultado_qualif_filtrado())
    req(intersecoes())
    
    resultado_sf <- resultado_qualif_filtrado()
    inter <- intersecoes()
    n_classes <- resultados_electre()$params$n_classes %||% 5
    cores_hex <- unname(paleta_cores()[as.character(1:n_classes)])
    
    pal_mun <- colorFactor(palette = cores_hex, domain = 1:n_classes, na.color = "transparent")
    opacidade <- input$opacidade_camadas %||% 0.7
    
    m <- leaflet() |> addProviderTiles(providers$CartoDB.Positron)
    
    m <- m |>
      addPolygons(data = resultado_sf, fillColor = ~pal_mun(class_electre),
                  fillOpacity = 0.3, color = "#999999", weight = 0.5, group = "Municípios",
                  label = ~paste0(NM_MUN, " - ", class_label))
    
    if (!is.null(inter$quilombolas) && nrow(inter$quilombolas) > 0) {
      m <- m |>
        addPolygons(data = inter$quilombolas, fillColor = "#f39c12", fillOpacity = opacidade,
                    color = "#d68910", weight = 1, group = "Quilombolas",
                    popup = ~paste0("<strong>Quilombola</strong><br/>Classe: ", class_label))
    }
    
    if (!is.null(inter$assentamentos) && nrow(inter$assentamentos) > 0) {
      m <- m |>
        addPolygons(data = inter$assentamentos, fillColor = "#27ae60", fillOpacity = opacidade,
                    color = "#1e8449", weight = 1, group = "Assentamentos",
                    popup = ~paste0("<strong>Assentamento</strong><br/>Classe: ", class_label))
    }
    
    if (!is.null(inter$indigenas) && nrow(inter$indigenas) > 0) {
      m <- m |>
        addPolygons(data = inter$indigenas, fillColor = "#3498db", fillOpacity = opacidade,
                    color = "#2874a6", weight = 1, group = "Territórios Indígenas",
                    popup = ~paste0("<strong>Território Indígena</strong><br/>Classe: ", class_label))
    }
    
    if (!is.null(inter$ensino) && nrow(inter$ensino) > 0) {
      m <- m |>
        addCircleMarkers(data = inter$ensino, radius = 5, fillColor = "#9b59b6",
                         fillOpacity = opacidade, color = "#7d3c98", weight = 1,
                         group = "Instituições de Ensino",
                         popup = ~paste0("<strong>Instituição de Ensino</strong><br/>Nome: ",
                                         nome_feature, "<br/>Classe: ", class_label))
    }
    
    if (!is.null(inter$prisoes) && nrow(inter$prisoes) > 0) {
      m <- m |>
        addCircleMarkers(data = inter$prisoes, radius = 5, fillColor = "#e74c3c",
                         fillOpacity = opacidade, color = "#c0392b", weight = 1,
                         group = "Unidades Prisionais",
                         popup = ~paste0("<strong>Unidade Prisional</strong><br/>Nome: ",
                                         nome_feature, "<br/>Classe: ", class_label))
    }
    
    if (!is.null(inter$sementes) && nrow(inter$sementes) > 0) {
      m <- m |>
        addCircleMarkers(data = inter$sementes, radius = 5, fillColor = "#16a085",
                         fillOpacity = opacidade, color = "#117a65", weight = 1,
                         group = "Bancos de Sementes",
                         popup = ~paste0("<strong>Banco de Sementes</strong><br/>Nome: ",
                                         nome_feature, "<br/>Classe: ", class_label))
    }
    
    m |>
      addLayersControl(overlayGroups = c("Municípios", "Quilombolas", "Assentamentos",
                                         "Territórios Indígenas", "Instituições de Ensino",
                                         "Unidades Prisionais", "Bancos de Sementes"),
                       options = layersControlOptions(collapsed = FALSE)) |>
      addLegend(position = "bottomright",
                colors = c("#999999", "#f39c12", "#27ae60", "#3498db", "#9b59b6", "#e74c3c", "#16a085"),
                labels = c("Municípios (fundo)", "Quilombolas", "Assentamentos",
                           "Territórios Indígenas", "Instituições de Ensino",
                           "Unidades Prisionais", "Bancos de Sementes"),
                title = "Camadas Territoriais", opacity = 1)
  })
  
  # Export mapa qualificação
  output$export_mapa_qualif <- downloadHandler(
    filename = function() paste0("mapa_qualificacao_", format(Sys.Date(), "%Y%m%d"), ".html"),
    content = function(file) {
      req(resultado_qualif_filtrado())
      req(intersecoes())
      
      resultado_sf <- resultado_qualif_filtrado()
      inter <- intersecoes()
      n_classes <- resultados_electre()$params$n_classes %||% 5
      cores_hex <- unname(paleta_cores()[as.character(1:n_classes)])
      
      pal_mun <- colorFactor(palette = cores_hex, domain = 1:n_classes, na.color = "transparent")
      opacidade <- input$opacidade_camadas %||% 0.7
      
      m <- leaflet() |> addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(data = resultado_sf, fillColor = ~pal_mun(class_electre),
                    fillOpacity = 0.3, color = "#999999", weight = 0.5, group = "Municípios",
                    label = ~paste0(NM_MUN, " - ", class_label))
      
      if (!is.null(inter$quilombolas) && nrow(inter$quilombolas) > 0) {
        m <- m |> addPolygons(data = inter$quilombolas, fillColor = "#f39c12", 
                              fillOpacity = opacidade, color = "#d68910", weight = 1, 
                              group = "Quilombolas")
      }
      if (!is.null(inter$assentamentos) && nrow(inter$assentamentos) > 0) {
        m <- m |> addPolygons(data = inter$assentamentos, fillColor = "#27ae60", 
                              fillOpacity = opacidade, color = "#1e8449", weight = 1, 
                              group = "Assentamentos")
      }
      if (!is.null(inter$indigenas) && nrow(inter$indigenas) > 0) {
        m <- m |> addPolygons(data = inter$indigenas, fillColor = "#3498db", 
                              fillOpacity = opacidade, color = "#2874a6", weight = 1, 
                              group = "Territórios Indígenas")
      }
      if (!is.null(inter$ensino) && nrow(inter$ensino) > 0) {
        m <- m |> addCircleMarkers(data = inter$ensino, radius = 5, fillColor = "#9b59b6",
                                   fillOpacity = opacidade, color = "#7d3c98", weight = 1,
                                   group = "Instituições de Ensino")
      }
      if (!is.null(inter$prisoes) && nrow(inter$prisoes) > 0) {
        m <- m |> addCircleMarkers(data = inter$prisoes, radius = 5, fillColor = "#e74c3c",
                                   fillOpacity = opacidade, color = "#c0392b", weight = 1,
                                   group = "Unidades Prisionais")
      }
      if (!is.null(inter$sementes) && nrow(inter$sementes) > 0) {
        m <- m |> addCircleMarkers(data = inter$sementes, radius = 5, fillColor = "#16a085",
                                   fillOpacity = opacidade, color = "#117a65", weight = 1,
                                   group = "Bancos de Sementes")
      }
      
      m <- m |> addLayersControl(overlayGroups = c("Municípios", "Quilombolas", "Assentamentos",
                                                   "Territórios Indígenas", "Instituições de Ensino",
                                                   "Unidades Prisionais", "Bancos de Sementes"),
                                 options = layersControlOptions(collapsed = FALSE))
      
      htmlwidgets::saveWidget(m, file, selfcontained = TRUE)
    }
  )
}