# =====================================================================
# COMPONENTES DE UI - SISTEMA DE FILTROS
# =====================================================================

#' Criar botão para abrir modal de filtros
#' 
#' @param ns Namespace function do módulo
#' @param id ID único do sistema de filtros
#' @return tagList com botão e UI de resumo
criar_botao_filtros_ui <- function(ns, id) {
  tagList(
    actionButton(
      ns(paste0("btn_abrir_modal_", id)),
      "Configurar Filtros",
      icon = icon("filter"),
      class = "btn-info w-100 mb-2"
    ),
    hr(),
    uiOutput(ns(paste0("ui_resumo_filtros_", id)))
  )
}

#' Criar card com lista de filtros ativos
#' 
#' @param filtros Lista de filtros ativos
#' @param ns Namespace function
#' @param id ID do sistema de filtros
#' @return Lista de cards HTML
criar_cards_filtros_ativos <- function(filtros, ns, id) {
  if (length(filtros) == 0) {
    return(tags$div(
      class = "alert alert-info",
      icon("info-circle"), " Nenhum filtro ativo. Adicione filtros usando o formulário abaixo."
    ))
  }
  
  lapply(seq_along(filtros), function(i) {
    f <- filtros[[i]]
    
    if (f$tipo == "composto") {
      texto_filtro <- tags$div(
        tags$strong("Filtro composto:"),
        tags$br(),
        tags$code(f$expressao)
      )
    } else {
      texto_filtro <- tags$div(
        tags$strong(f$campo), " ", f$operador_nome, " ", 
        tags$span(class = "badge bg-primary", f$valor_texto)
      )
    }
    
    card(
      card_body(
        layout_columns(
          col_widths = c(10, 2),
          texto_filtro,
          actionButton(
            ns(paste0("btn_remover_filtro_", id, "_", i)),
            NULL,
            icon = icon("trash"),
            class = "btn-danger btn-sm",
            style = "padding: 2px 8px;"
          )
        )
      ),
      class = "mb-2"
    )
  })
}

#' Criar resumo visual dos filtros aplicados
#' 
#' @param filtros Lista de filtros aplicados
#' @return HTML com resumo
criar_resumo_filtros <- function(filtros) {
  if (length(filtros) == 0) {
    return(tags$div(
      class = "alert alert-secondary mb-0",
      style = "padding: 8px;",
      icon("filter"), " Nenhum filtro aplicado"
    ))
  }
  
  resumo <- lapply(filtros, function(f) {
    if (f$tipo == "composto") {
      tags$div(
        class = "badge bg-warning text-dark mb-1",
        style = "display: block; text-align: left; white-space: normal; padding: 6px;",
        icon("code"), " ", f$expressao
      )
    } else {
      tags$div(
        class = "badge bg-info mb-1",
        style = "display: block; text-align: left; white-space: normal; padding: 6px;",
        f$campo, " ", f$operador_nome, " ", f$valor_texto
      )
    }
  })
  
  tags$div(
    style = "max-height: 200px; overflow-y: auto;",
    resumo
  )
}
