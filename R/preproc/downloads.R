# =====================================================================
# HANDLERS DE DOWNLOAD
# =====================================================================

#' Criar handler de download CSV
#' 
#' @param summary_table Reactive com tabela de resumo
#' @return Download handler
criar_download_csv <- function(summary_table) {
  downloadHandler(
    filename = function() paste0("resumo_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(summary_table(), file)
  )
}

#' Criar handler de download XLSX
#' 
#' @param summary_table Reactive com tabela de resumo
#' @return Download handler
criar_download_xlsx <- function(summary_table) {
  downloadHandler(
    filename = function() paste0("resumo_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(summary_table(), file)
  )
}

#' Criar handler de download GeoPackage
#' 
#' @param filtered Reactive com dados espaciais filtrados
#' @return Download handler
criar_download_gpkg <- function(filtered) {
  downloadHandler(
    filename = function() paste0("subconjunto_", Sys.Date(), ".gpkg"),
    content = function(file) {
      sf::st_write(filtered(), dsn = file, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    }
  )
}

#' Configurar todos os downloads no módulo
#' 
#' @param output Output object do Shiny
#' @param summary_table Reactive com tabela de resumo
#' @param filtered Reactive com dados espaciais filtrados
setup_downloads <- function(output, summary_table, filtered) {
  output$dl_csv <- criar_download_csv(summary_table)
  output$dl_xlsx <- criar_download_xlsx(summary_table)
  output$dl_gpkg <- criar_download_gpkg(filtered)
}
