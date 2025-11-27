# =====================================================================
# OUTPUTS - ABA TABELA
# =====================================================================

#' Criar outputs da aba Tabela
#' 
#' @param output Shiny output
#' @param session Shiny session
#' @param ns Namespace function
#' @param resultados_electre Reactive com resultados ELECTRE
#' @param paleta_cores Reactive com paleta de cores
#' @export
criar_outputs_tabela <- function(output, session, ns, resultados_electre, paleta_cores) {
  
  # Sistema de filtros
  filtros_tabela_aplicados <- reactiveVal(list())
  
  criar_sistema_filtros_modal(
    session = session, 
    ns = ns, 
    id = "tabela",
    results_reactive = reactive({
      req(resultados_electre())
      resultados_electre()$results
    }),
    filtros_aplicados = filtros_tabela_aplicados
  )
  
  # Dados filtrados
  tabela_filtrada <- reactive({
    req(resultados_electre())
    results <- resultados_electre()$results
    filtros <- filtros_tabela_aplicados()
    aplicar_filtros_em_df(results, filtros)
  })
  
  # Tabela completa
  output$tab_completa <- renderDT({
    req(tabela_filtrada())
    df <- tabela_filtrada()
    n_classes <- resultados_electre()$params$n_classes %||% 5
    cores_hex <- paleta_cores()[1:n_classes]
    
    datatable(df, rownames = FALSE, filter = "top", extensions = c("Buttons"),
              options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip", buttons = list(),
                             order = list(list(which(names(df) == "class_electre") - 1, "asc")),
                             language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ registros",
                                             info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
                                             paginate = list(first = "Primeiro", last = "Último",
                                                             `next` = "Próximo", previous = "Anterior")))) |>
      formatStyle("class_electre", backgroundColor = styleEqual(1:n_classes, cores_hex),
                  fontWeight = "bold", color = "white")
  })
  
  # Download CSV
  output$dl_tabela_csv <- downloadHandler(
    filename = function() paste0("electre_tabela_", Sys.Date(), ".csv"),
    content = function(file) {
      req(tabela_filtrada())
      readr::write_csv(tabela_filtrada(), file)
      showNotification("CSV exportado com sucesso!", type = "message", duration = 3)
    }
  )
  
  # Download Excel
  output$dl_tabela_excel <- downloadHandler(
    filename = function() paste0("electre_tabela_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(tabela_filtrada())
      if (!requireNamespace("writexl", quietly = TRUE)) {
        showNotification("Pacote 'writexl' não instalado. Usando CSV.", type = "warning", duration = 5)
        readr::write_csv(tabela_filtrada(), file)
      } else {
        writexl::write_xlsx(tabela_filtrada(), file)
        showNotification("Excel exportado com sucesso!", type = "message", duration = 3)
      }
    }
  )
}