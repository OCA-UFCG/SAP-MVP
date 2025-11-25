# =====================================================================
# outputs/maps.R - ADICIONAR FUNÇÃO criar_download_mapa_png
# =====================================================================

#' Criar download handler para PNG do mapa
#' 
#' @param filtered_data Reactive com dados filtrados
#' @param input Input do Shiny
#' @param var_name_reactive Reactive com nome da variável
#' @param nomes_map_reactive Reactive com mapeamento de nomes editados
#' @return Download handler
criar_download_mapa_png <- function(filtered_data, input, var_name_reactive, nomes_map_reactive) {
  downloadHandler(
    filename = function() {
      var_name <- var_name_reactive() %||% "mapa"
      paste0(var_name, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(var_name_reactive())
      df <- filtered_data()
      req(nrow(df) > 0)
      
      v <- var_name_reactive()
      vals <- preparar_valores_mapa(df, v, isTRUE(input$log_scale))
      
      # Determinar nome para exibição
      # Prioridade: 1) nome customizado, 2) nome renomeado, 3) nome original
      nomes_map <- nomes_map_reactive()
      nome_exibicao <- if (!is.null(input$nome_var_customizado) && nchar(trimws(input$nome_var_customizado)) > 0) {
        input$nome_var_customizado
      } else if (!is.null(nomes_map) && !is.null(nomes_map[[v]])) {
        nomes_map[[v]]
      } else {
        v
      }
      
      # Gerar mapa
      p <- gerar_mapa_png(
        df = df,
        var = v,
        vals = vals,
        pal_name = input$pal,
        reverse_pal = isTRUE(input$reverse_pal),
        log_scale = isTRUE(input$log_scale),
        usar_classes = isTRUE(input$usar_classes),
        n_classes = input$n_classes %||% 5,
        metodo_quebra = input$metodo_quebra %||% "jenks",
        mostrar_limites_ufs = isTRUE(input$mostrar_limites_ufs),
        usar_basemap = FALSE,  # Mude para TRUE se quiser basemap (mais lento)
        nome_exibicao = nome_exibicao
      )
      
      # Salvar PNG
      ggsave(file, plot = p, width = 14, height = 10, dpi = 300, bg = "white")
    }
  )
}
#' Obter limites estaduais (com cache)
obter_limites_ufs <- memoise::memoise(function() {
  tryCatch({
    ufs <- geobr::read_state(year = 2020, showProgress = FALSE)
    sf::st_transform(ufs, 4326)
  }, error = function(e) {
    NULL
  })
})


#' Criar paleta para mapa leaflet
criar_paleta_leaflet <- function(valores, palette_name = "viridis", reverse = FALSE, 
                                 usar_classes = FALSE, n_classes = 5, 
                                 metodo_quebra = "jenks") {
  vals_clean <- valores[is.finite(valores)]
  
  if (length(vals_clean) == 0) {
    return(colorNumeric(palette = palette_name, domain = c(0, 1), na.color = "#cccccc"))
  }
  
  if (usar_classes) {
    n_eff <- min(n_classes, length(unique(vals_clean)))
    n_eff <- max(2, n_eff)
    
    tryCatch({
      brks <- classInt::classIntervals(vals_clean, n = n_eff, style = metodo_quebra)$brks
      brks[1] <- min(vals_clean, na.rm = TRUE) - 1e-10
      brks[length(brks)] <- max(vals_clean, na.rm = TRUE) + 1e-10
      
      pal <- colorBin(
        palette = palette_name,
        domain = valores,
        bins = brks,
        na.color = "#cccccc",
        reverse = reverse
      )
      
      return(pal)
    }, error = function(e) {
      warning("Erro ao criar quebras, usando paleta contínua: ", e$message)
      return(colorNumeric(
        palette = palette_name,
        domain = valores,
        na.color = "#cccccc",
        reverse = reverse
      ))
    })
  } else {
    colorNumeric(
      palette = palette_name,
      domain = valores,
      na.color = "#cccccc",
      reverse = reverse
    )
  }
}

#' Criar quebras de classe para ggplot2
criar_quebras_classe <- function(valores, n_classes, method = "jenks") {
  x_ok <- valores[is.finite(valores)]
  
  if (length(unique(x_ok)) < 2) {
    return(range(x_ok, na.rm = TRUE))
  }
  
  n_eff <- min(n_classes, length(unique(x_ok)) - 1)
  n_eff <- max(2, n_eff)
  
  tryCatch({
    brks <- classInt::classIntervals(x_ok, n = n_eff, style = method)$brks
    
    if (length(unique(brks)) < (n_eff + 1)) {
      brks <- classInt::classIntervals(x_ok, n = n_eff, style = "quantile")$brks
    }
    
    brks
  }, error = function(e) {
    seq(min(x_ok, na.rm = TRUE), max(x_ok, na.rm = TRUE), length.out = n_eff + 1)
  })
}

#' Criar mapa leaflet base
criar_mapa_base <- function(df) {
  req(nrow(df) > 0)
  
  if (sf::st_crs(df) != sf::st_crs(4326)) {
    df <- sf::st_transform(df, 4326)
  }
  
  bb <- sf::st_bbox(df)
  
  leaflet(options = leafletOptions(zoomControl = TRUE, minZoom = 3)) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])
}

#' Preparar valores para visualização no mapa
preparar_valores_mapa <- function(df, var, log_scale = FALSE) {
  vals <- as.numeric(sf::st_drop_geometry(df)[[var]])
  vals[!is.finite(vals)] <- NA_real_
  
  if (log_scale) {
    pos <- vals[is.finite(vals) & vals > 0]
    if (length(pos)) {
      minpos <- min(pos, na.rm = TRUE)
      vals <- log10(pmax(vals, minpos))
    } else {
      vals <- NA_real_
    }
  }
  
  vals
}

#' Criar labels HTML para o mapa
criar_labels_mapa <- function(df, var, nome_exibicao = NULL) {
  if (is.null(nome_exibicao)) nome_exibicao <- var
  
  lab <- sprintf(
    "<b>%s</b><br/>%s: %s",
    df$NM_MUN %||% df$CD_MUN %||% seq_len(nrow(df)),
    nome_exibicao,
    format(round(sf::st_drop_geometry(df)[[var]], 3), big.mark = ".", decimal.mark = ",")
  )
  lapply(lab, htmltools::HTML)
}

#' Atualizar camada do mapa com opções avançadas
atualizar_camada_mapa <- function(proxy, df, var, vals, pal, ns, 
                                  mostrar_ufs = FALSE, ufs_sf = NULL,
                                  nomes_map = NULL, nome_customizado = NULL) {
  
  if (sf::st_crs(df) != sf::st_crs(4326)) {
    df <- sf::st_transform(df, 4326)
  }
  
  # Determinar nome para exibição
  # Prioridade: 1) nome customizado, 2) nome renomeado, 3) nome original
  nome_exibicao <- if (!is.null(nome_customizado) && nchar(trimws(nome_customizado)) > 0) {
    nome_customizado
  } else if (!is.null(nomes_map) && !is.null(nomes_map[[var]])) {
    nomes_map[[var]]
  } else {
    var
  }
  
  lab <- criar_labels_mapa(df, var, nome_exibicao)
  df_s <- tryCatch(msimplify_memo(df, tol_m = 500), error = function(e) df)
  
  proxy <- proxy |>
    clearShapes() |>
    clearControls()
  
  proxy <- proxy |>
    addPolygons(
      data = df_s,
      fillColor = ~pal(vals),
      fillOpacity = 0.8,
      color = "#555",
      weight = 0.6,
      highlight = highlightOptions(weight = 2, color = "#000", fillOpacity = 0.9, bringToFront = TRUE),
      label = lab,
      labelOptions = labelOptions(textsize = "12px", direction = "auto")
    )
  
  if (mostrar_ufs && !is.null(ufs_sf)) {
    if (sf::st_crs(ufs_sf) != sf::st_crs(4326)) {
      ufs_sf <- sf::st_transform(ufs_sf, 4326)
    }
    
    proxy <- proxy |>
      addPolylines(
        data = ufs_sf,
        color = "#000000",
        weight = 1.5,
        opacity = 0.8,
        group = "limites_ufs"
      )
  }
  
  proxy |>
    addLegend(
      position = "bottomright", 
      pal = pal, 
      values = vals, 
      title = nome_exibicao,
      opacity = 0.9
    )
}

#' Configurar sistema de mapas no módulo
setup_sistema_mapas <- function(output, session, input, filtered_data, ns, nomes_map_reactive) {
  
  output$map <- renderLeaflet({
    df <- filtered_data()
    criar_mapa_base(df)
  })
  
  observe({
    req(input$var_map)
    df <- filtered_data()
    req(nrow(df) > 0)
    
    v <- input$var_map
    vals <- preparar_valores_mapa(df, v, isTRUE(input$log_scale))
    
    usar_classes <- !is.null(input$usar_classes) && input$usar_classes
    
    pal <- criar_paleta_leaflet(
      valores = vals,
      palette_name = input$pal,
      reverse = isTRUE(input$reverse_pal),
      usar_classes = usar_classes,
      n_classes = if(usar_classes) input$n_classes %||% 5 else 5,
      metodo_quebra = if(usar_classes) input$metodo_quebra %||% "jenks" else "jenks"
    )
    
    ufs_sf <- NULL
    if (!is.null(input$mostrar_limites_ufs) && input$mostrar_limites_ufs) {
      ufs_sf <- obter_limites_ufs()
    }
    
    leafletProxy(ns("map")) |>
      atualizar_camada_mapa(
        df = df, 
        var = v, 
        vals = vals, 
        pal = pal, 
        ns = ns, 
        mostrar_ufs = isTRUE(input$mostrar_limites_ufs),
        ufs_sf = ufs_sf,
        nomes_map = nomes_map_reactive(),
        nome_customizado = input$nome_var_customizado
      )
  })
}

#' Gerar mapa PNG profissional para download
gerar_mapa_png <- function(df, var, vals, pal_name = "viridis", 
                           reverse_pal = FALSE, log_scale = FALSE,
                           usar_classes = FALSE, n_classes = 5, 
                           metodo_quebra = "jenks",
                           mostrar_limites_ufs = FALSE,
                           usar_basemap = TRUE,
                           nome_exibicao = NULL) {
  
  if (sf::st_crs(df) != sf::st_crs(4326)) {
    df <- sf::st_transform(df, 4326)
  }
  
  if (is.null(nome_exibicao)) nome_exibicao <- var
  
  library(ggplot2)
  library(ggspatial)
  library(cowplot)
  
  ufs_sf <- NULL
  if (mostrar_limites_ufs) {
    ufs_sf <- obter_limites_ufs()
    if (!is.null(ufs_sf) && sf::st_crs(ufs_sf) != sf::st_crs(4326)) {
      ufs_sf <- sf::st_transform(ufs_sf, 4326)
    }
  }
  
  # Criar mapa base
  if (usar_classes) {
    brks <- criar_quebras_classe(vals, n_classes, metodo_quebra)
    bins <- cut(vals, breaks = brks, include.lowest = TRUE, dig.lab = 10)
    df$.__bin__ <- bins
    
    labels_legenda <- character(length(levels(bins)))
    for (i in seq_along(levels(bins))) {
      nivel <- levels(bins)[i]
      nums <- gsub("\\[|\\]|\\(|\\)", "", nivel)
      nums <- strsplit(nums, ",")[[1]]
      min_val <- as.numeric(trimws(nums[1]))
      max_val <- as.numeric(trimws(nums[2]))
      labels_legenda[i] <- sprintf("%.2f - %.2f", min_val, max_val)
    }
    
    lvls <- levels(bins)
    n_lvls <- length(lvls)
    
    if (pal_name %in% rownames(RColorBrewer::brewer.pal.info)) {
      max_colors <- RColorBrewer::brewer.pal.info[pal_name, "maxcolors"]
      base_colors <- RColorBrewer::brewer.pal(min(max_colors, max(3, n_lvls)), pal_name)
    } else {
      base_colors <- switch(pal_name,
                            "viridis" = viridisLite::viridis(n_lvls),
                            "plasma" = viridisLite::plasma(n_lvls),
                            "inferno" = viridisLite::inferno(n_lvls),
                            "magma" = viridisLite::magma(n_lvls),
                            "cividis" = viridisLite::cividis(n_lvls),
                            viridisLite::viridis(n_lvls)
      )
    }
    
    pal_colors <- colorRampPalette(base_colors)(n_lvls)
    if (reverse_pal) pal_colors <- rev(pal_colors)
    names(pal_colors) <- lvls
    
    if (usar_basemap) {
      p_main <- ggplot() +
        annotation_map_tile(type = "cartolight", zoom = NULL, cachedir = tempdir())
    } else {
      p_main <- ggplot()
    }
    
    p_main <- p_main +
      geom_sf(data = df, aes(fill = .__bin__), color = "#888888", linewidth = 0.1) +
      scale_fill_manual(
        values = pal_colors, 
        na.value = "grey85", 
        name = nome_exibicao,
        labels = labels_legenda,
        drop = FALSE
      )
  } else {
    df$.__val__ <- vals
    
    if (usar_basemap) {
      p_main <- ggplot() +
        annotation_map_tile(type = "cartolight", zoom = NULL, cachedir = tempdir())
    } else {
      p_main <- ggplot()
    }
    
    if (pal_name %in% rownames(RColorBrewer::brewer.pal.info)) {
      p_main <- p_main +
        geom_sf(data = df, aes(fill = .__val__), color = "#888888", linewidth = 0.1) +
        scale_fill_distiller(
          palette = pal_name,
          direction = if(reverse_pal) -1 else 1,
          na.value = "grey85",
          name = nome_exibicao
        )
    } else {
      viridis_option <- switch(pal_name,
                               "viridis" = "D", "plasma" = "C", "inferno" = "B", 
                               "magma" = "A", "cividis" = "E", "D"
      )
      
      p_main <- p_main +
        geom_sf(data = df, aes(fill = .__val__), color = "#888888", linewidth = 0.1) +
        scale_fill_viridis_c(
          option = viridis_option,
          direction = if(reverse_pal) -1 else 1,
          na.value = "grey85",
          name = nome_exibicao
        )
    }
  }
  
  if (!is.null(ufs_sf)) {
    p_main <- p_main +
      geom_sf(data = ufs_sf, fill = NA, color = "black", linewidth = 0.5)
    
    # Calcular centroides para posicionar siglas dos estados
    # NOTA: Warnings são esperados porque st_centroid() em dados lat/long (WGS84)
    # pode não ser geometricamente preciso, mas é suficiente para visualização.
    # Alternativa seria reprojetar para métrico (SIRGAS 2000), mas consome muito tempo.
    # Os centroides calculados aqui são apenas para posicionar labels no mapa.
    ufs_centroids <- suppressWarnings(sf::st_centroid(ufs_sf))
    
    p_main <- p_main +
      geom_sf_text(
        data = ufs_centroids,
        aes(label = abbrev_state),
        size = 3,
        fontface = "bold",
        color = "black"
      )
  }
  
  bbox_mun <- sf::st_bbox(df)
  
  p_main <- p_main +
    annotation_scale(
      location = "bl",
      width_hint = 0.2,
      text_cex = 0.8,
      bar_cols = c("black", "white"),
      line_width = 1
    ) +
    annotation_north_arrow(
      location = "tl",
      which_north = "true",
      height = unit(1.2, "cm"),
      width = unit(1.2, "cm"),
      style = north_arrow_fancy_orienteering(
        fill = c("black", "white"),
        line_col = "black"
      )
    ) +
    coord_sf(
      xlim = c(bbox_mun["xmin"], bbox_mun["xmax"]),
      ylim = c(bbox_mun["ymin"], bbox_mun["ymax"]),
      expand = FALSE
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(color = NA, fill = NA),
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 9),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 9, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(x = "Longitude", y = "Latitude")
  
  if (!is.null(ufs_sf)) {
    bbox_poly <- sf::st_as_sfc(bbox_mun)
    sf::st_crs(bbox_poly) <- 4326
    
    brasil_bbox <- sf::st_bbox(c(xmin = -74, xmax = -34, ymin = -34, ymax = 6), crs = 4326)
    
    p_inset <- ggplot() +
      geom_sf(data = ufs_sf, fill = "grey95", color = "grey50", linewidth = 0.3) +
      geom_sf(data = bbox_poly, fill = "red", color = "red", alpha = 0.3, linewidth = 1) +
      coord_sf(
        xlim = c(brasil_bbox["xmin"], brasil_bbox["xmax"]),
        ylim = c(brasil_bbox["ymin"], brasil_bbox["ymax"]),
        expand = FALSE
      ) +
      theme_void() +
      theme(
        panel.background = element_rect(fill = "lightblue", color = "black", linewidth = 1),
        plot.background = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
      )
    
    p_final <- ggdraw(p_main) +
      draw_plot(p_inset, x = 0.73, y = 0.05, width = 0.25, height = 0.25)
  } else {
    p_final <- p_main
  }
  
  p_final
}