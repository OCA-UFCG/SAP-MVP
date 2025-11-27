# =====================================================================
# HANDLERS DE DOWNLOAD
# =====================================================================

#' Criar handlers de download
#' 
#' @param output Shiny output
#' @param session Shiny session
#' @param ns Namespace function
#' @param resultados_electre Reactive com resultados ELECTRE
#' @param data_sf Reactive com dados espaciais
#' @param label_map Reactive com labels das classes
#' @param paleta_cores Reactive com paleta de cores
#' @export
criar_handlers_download <- function(output, session, ns, resultados_electre, 
                                    data_sf, label_map, paleta_cores) {
  
  # Download CSV
  output$dl_resultados_csv <- downloadHandler(
    filename = function() paste0("electre_resultados_", Sys.Date(), ".csv"),
    content = function(file) {
      req(resultados_electre())
      readr::write_csv(resultados_electre()$results, file)
      showNotification("CSV exportado com sucesso!", type = "message", duration = 3)
    }
  )
  
  # Download GeoPackage
  output$dl_resultados_gpkg <- downloadHandler(
    filename = function() paste0("electre_resultados_", Sys.Date(), ".gpkg"),
    content = function(file) {
      req(resultados_electre())
      req(data_sf())
      results <- resultados_electre()$results
      sf_data <- data_sf()
      
      if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
        sf_results <- sf_data |> left_join(results, by = "CD_MUN")
      } else {
        sf_results <- sf_data
        sf_results <- cbind(sf_results, results)
      }
      sf::st_write(sf_results, dsn = file, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
      showNotification("GeoPackage exportado com sucesso!", type = "message", duration = 3)
    }
  )
  
  # Download Mapa PNG
  output$dl_mapa_png <- downloadHandler(
    filename = function() paste0("electre_mapa_", Sys.Date(), ".png"),
    content = function(file) {
      req(resultados_electre())
      req(data_sf())
      showNotification("Gerando mapa PNG...", type = "default", duration = NULL, id = "png_export")
      
      tryCatch({
        if (!requireNamespace("mapview", quietly = TRUE)) {
          stop("O pacote 'mapview' não está instalado.")
        }
        
        results <- resultados_electre()$results
        sf_data <- data_sf()
        n_classes <- resultados_electre()$params$n_classes %||% 5
        cores_hex <- paleta_cores()[1:n_classes]
        
        if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
          sf_results <- sf_data |>
            left_join(results |> select(CD_MUN, class_electre, class_label), by = "CD_MUN")
        } else {
          sf_results <- sf_data
          sf_results$class_electre <- results$class_electre
          sf_results$class_label <- results$class_label
        }
        
        m <- mapview::mapview(sf_results, zcol = "class_electre", col.regions = cores_hex,
                              at = seq(0.5, n_classes + 0.5, by = 1), layer.name = "Classe ELECTRE",
                              legend = TRUE, map.types = "CartoDB.Positron")
        
        mapview::mapshot(m, file = file,
                         remove_controls = c("zoomControl", "layersControl", "homeButton"),
                         vwidth = 1200, vheight = 800)
        
        removeNotification("png_export")
        showNotification("Mapa PNG exportado com sucesso!", type = "message", duration = 3)
        
      }, error = function(e) {
        removeNotification("png_export")
        tryCatch({
          library(ggplot2)
          results <- resultados_electre()$results
          sf_data <- data_sf()
          n_classes <- resultados_electre()$params$n_classes %||% 5
          labels_atuais <- label_map()
          cores_hex <- paleta_cores()[1:n_classes]
          
          if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
            sf_results <- sf_data |> left_join(results, by = "CD_MUN")
          } else {
            sf_results <- sf_data
            sf_results$class_electre <- results$class_electre
          }
          
          p <- ggplot(sf_results) +
            geom_sf(aes(fill = factor(class_electre)), color = "white", size = 0.1) +
            scale_fill_manual(values = setNames(cores_hex, 1:n_classes),
                              labels = labels_atuais[1:n_classes], name = "Classe ELECTRE") +
            theme_minimal() +
            theme(legend.position = "right", panel.grid = element_blank(),
                  axis.text = element_blank(), axis.ticks = element_blank()) +
            labs(title = "Classificação ELECTRE Tri-B")
          
          ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
          removeNotification("png_export")
          showNotification("Mapa exportado com método alternativo!", type = "message", duration = 3)
        }, error = function(e2) {
          removeNotification("png_export")
          showNotification(paste("Erro ao exportar mapa:", e2$message), type = "error", duration = 10)
        })
      })
    }
  )
}