# =====================================================================
# OUTPUTS - ABA MAPA
# =====================================================================

#' Criar outputs da aba Mapa
#' 
#' @param output Shiny output
#' @param session Shiny session
#' @param ns Namespace function
#' @param input Shiny input
#' @param resultados_electre Reactive com resultados ELECTRE
#' @param data_sf Reactive com dados espaciais
#' @param label_map Reactive com labels das classes
#' @param paleta_cores Reactive com paleta de cores
#' @export
criar_outputs_mapa <- function(output, session, ns, input, resultados_electre, 
                               data_sf, label_map, paleta_cores) {
  
  # Sistema de filtros
  filtros_mapa_aplicados <- reactiveVal(list())
  
  criar_sistema_filtros_modal(
    session = session,
    ns = ns,
    id = "mapa",
    results_reactive = reactive({
      req(resultados_electre())
      resultados_electre()$results
    }),
    filtros_aplicados = filtros_mapa_aplicados
  )
  
  # Atualizar selectize de municípios
  observe({
    req(resultados_electre())
    req(data_sf())
    sf_data <- data_sf()
    if ("NM_MUN" %in% names(sf_data)) {
      municipios <- sort(unique(sf_data$NM_MUN))
      updateSelectizeInput(session, "busca_municipio", choices = municipios, server = TRUE)
    }
  })
  
  # Dados do mapa filtrados
  mapa_filtrado <- reactive({
    req(resultados_electre())
    req(data_sf())
    results <- resultados_electre()$results
    sf_data <- data_sf()
    filtros <- filtros_mapa_aplicados()
    results <- aplicar_filtros_em_df(results, filtros)
    
    if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
      sf_results <- sf_data |>
        inner_join(results |> select(CD_MUN, class_electre, class_label), by = "CD_MUN")
    } else {
      sf_results <- sf_data
      if (nrow(results) == nrow(sf_data)) {
        sf_results$class_electre <- results$class_electre
        sf_results$class_label <- results$class_label
      }
    }
    sf_results
  })
  
  # Mapa principal
  output$mapa_classes <- renderLeaflet({
    req(mapa_filtrado())
    sf_results <- mapa_filtrado()
    n_classes <- resultados_electre()$params$n_classes %||% 5
    labels_atuais <- label_map()
    cores_atuais <- paleta_cores()
    cores_hex <- unname(cores_atuais[as.character(1:n_classes)])
    
    pal <- colorFactor(palette = cores_hex, domain = 1:n_classes, na.color = "#cccccc")
    
    labels <- sprintf("<strong>%s</strong><br/>Classe: %s",
                      sf_results$NM_MUN %||% "Município", sf_results$class_label) |>
      lapply(htmltools::HTML)
    
    sf_simple <- tryCatch(rmapshaper::ms_simplify(sf_results, keep = 0.05),
                          error = function(e) sf_results)
    
    leaflet(sf_simple) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(fillColor = ~pal(class_electre), fillOpacity = 0.75,
                  color = "#FFFFFF", weight = 0.3, opacity = 1,
                  highlight = highlightOptions(weight = 2, color = "#000", fillOpacity = 0.95, bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "12px", direction = "auto")) |>
      addLegend(position = "bottomright", pal = pal, values = ~class_electre,
                title = "Classe ELECTRE", opacity = 0.9,
                labFormat = labelFormat(transform = function(x) labels_atuais[as.character(x)]))
  })
  
  # Zoom para município
  observeEvent(input$btn_zoom_municipio, {
    req(input$busca_municipio)
    req(data_sf())
    sf_data <- data_sf()
    
    if ("NM_MUN" %in% names(sf_data)) {
      municipio_sel <- sf_data |> filter(NM_MUN == input$busca_municipio)
      if (nrow(municipio_sel) > 0) {
        bbox <- st_bbox(municipio_sel)
        leafletProxy(ns("mapa_classes")) |>
          flyToBounds(lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]),
                      lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"]))
        showNotification(paste0("Zoom aplicado: ", input$busca_municipio), type = "message", duration = 2)
      } else {
        showNotification("Município não encontrado no mapa", type = "warning", duration = 3)
      }
    }
  })
  
  # Zoom automático ao filtrar
  observe({
    req(mapa_filtrado())
    filtros_mapa_aplicados()
    sf_filtrado <- mapa_filtrado()
    if (nrow(sf_filtrado) > 0) {
      bbox <- st_bbox(sf_filtrado)
      leafletProxy(ns("mapa_classes")) |>
        flyToBounds(lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]),
                    lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"]))
    }
  })
  
  # Export mapa HTML
  output$export_mapa_principal <- downloadHandler(
    filename = function() paste0("mapa_classificacao_", format(Sys.Date(), "%Y%m%d"), ".html"),
    content = function(file) {
      req(mapa_filtrado())
      sf_results <- mapa_filtrado()
      n_classes <- resultados_electre()$params$n_classes %||% 5
      labels_atuais <- label_map()
      cores_atuais <- paleta_cores()
      cores_hex <- unname(cores_atuais[as.character(1:n_classes)])
      
      pal <- colorFactor(palette = cores_hex, domain = 1:n_classes, na.color = "#cccccc")
      labels <- sprintf("<strong>%s</strong><br/>Classe: %s",
                        sf_results$NM_MUN %||% "Município", sf_results$class_label) |>
        lapply(htmltools::HTML)
      
      sf_simple <- tryCatch(rmapshaper::ms_simplify(sf_results, keep = 0.05),
                            error = function(e) sf_results)
      
      m <- leaflet(sf_simple) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(fillColor = ~pal(class_electre), fillOpacity = 0.75,
                    color = "#FFFFFF", weight = 0.3, opacity = 1,
                    highlight = highlightOptions(weight = 2, color = "#000", fillOpacity = 0.95, bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                textsize = "12px", direction = "auto")) |>
        addLegend(position = "bottomright", pal = pal, values = ~class_electre,
                  title = "Classe ELECTRE", opacity = 0.9,
                  labFormat = labelFormat(transform = function(x) labels_atuais[as.character(x)]))
      
      htmlwidgets::saveWidget(m, file, selfcontained = TRUE)
    }
  )
}