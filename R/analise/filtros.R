# =====================================================================
# SISTEMA DE FILTROS COM MODAL - REUTILIZÁVEL
# =====================================================================

#' Criar botão para abrir modal de filtros
#' 
#' @param ns Namespace function
#' @param id ID único para este sistema de filtros
#' @return UI tagList
#' @export
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

#' Criar sistema completo de filtros com modal
#' 
#' @param session Shiny session
#' @param ns Namespace function
#' @param id ID único para este sistema de filtros
#' @param results_reactive Reactive que retorna o data frame a ser filtrado
#' @param filtros_aplicados ReactiveVal que armazena a lista de filtros
#' @export
criar_sistema_filtros_modal <- function(session, ns, id, results_reactive, filtros_aplicados) {
  
  filtros_pendentes <- reactiveVal(list())
  
  # Botão para abrir modal
  observeEvent(session$input[[paste0("btn_abrir_modal_", id)]], {
    req(results_reactive())
    
    filtros_pendentes(filtros_aplicados())
    
    results <- results_reactive()
    campos <- names(results)
    campos_excluir <- c("geometry", "CD_MUN", if(inherits(results, "sf")) attr(results, "sf_column") else NULL)
    campos_disponiveis <- setdiff(campos, campos_excluir)
    
    isolate({
      updateSelectInput(session, paste0("modal_campo_", id), 
                        choices = c("Selecione..." = "", campos_disponiveis),
                        selected = "")
    })
    
    showModal(modalDialog(
      title = tags$div(icon("filter"), " Construtor de Filtros", 
                       style = "font-size: 1.2em; font-weight: bold;"),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(ns(paste0("btn_limpar_", id)), "Limpar Tudo", 
                     icon = icon("trash"), class = "btn-warning"),
        modalButton("Cancelar"),
        actionButton(ns(paste0("btn_aplicar_", id)), "Aplicar Filtros", 
                     icon = icon("check"), class = "btn-success")
      ),
      
      card(
        card_header(icon("list"), " Filtros Ativos"),
        uiOutput(ns(paste0("ui_modal_filtros_ativos_", id)))
      ),
      hr(),
      card(
        card_header(icon("plus-circle"), " Adicionar Novo Filtro"),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput(ns(paste0("modal_campo_", id)), "Campo:", choices = NULL),
          uiOutput(ns(paste0("ui_modal_operador_", id))),
          uiOutput(ns(paste0("ui_modal_valor_", id)))
        ),
        actionButton(ns(paste0("btn_adicionar_filtro_", id)), "Adicionar Filtro",
                     icon = icon("plus"), class = "btn-primary w-100 mt-2")
      )
    ))
  })
  
  # UI: Operador dinâmico
  output <- session$output
  output[[paste0("ui_modal_operador_", id)]] <- renderUI({
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(results_reactive())
    
    results <- results_reactive()
    campo <- results[[session$input[[paste0("modal_campo_", id)]]]]
    
    if (is.numeric(campo) && length(unique(campo)) > 30) {
      selectInput(ns(paste0("modal_operador_", id)), "Condição:",
                  choices = c("Selecione..." = "", "é igual a" = "==", "é maior que" = ">",
                              "é maior ou igual a" = ">=", "é menor que" = "<",
                              "é menor ou igual a" = "<=", "está entre" = "between"))
    } else {
      selectInput(ns(paste0("modal_operador_", id)), "Condição:",
                  choices = c("Selecione..." = "", "é igual a" = "==", 
                              "está entre as opções" = "in"))
    }
  })
  
  # UI: Valor dinâmico
  output[[paste0("ui_modal_valor_", id)]] <- renderUI({
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(session$input[[paste0("modal_operador_", id)]] != "")
    req(results_reactive())
    
    results <- results_reactive()
    campo <- results[[session$input[[paste0("modal_campo_", id)]]]]
    operador <- session$input[[paste0("modal_operador_", id)]]
    
    if (is.numeric(campo) && length(unique(campo)) > 30) {
      if (operador == "between") {
        tagList(
          numericInput(ns(paste0("modal_valor_min_", id)), "Valor mínimo:", 
                       value = min(campo, na.rm = TRUE)),
          numericInput(ns(paste0("modal_valor_max_", id)), "Valor máximo:", 
                       value = max(campo, na.rm = TRUE))
        )
      } else {
        numericInput(ns(paste0("modal_valor_", id)), "Valor:", 
                     value = median(campo, na.rm = TRUE))
      }
    } else {
      valores_unicos <- sort(unique(campo))
      if (operador == "in") {
        selectizeInput(ns(paste0("modal_valor_", id)), "Valores:",
                       choices = valores_unicos, selected = NULL, multiple = TRUE,
                       options = list(placeholder = "Selecione um ou mais...",
                                      plugins = list("remove_button")))
      } else {
        selectInput(ns(paste0("modal_valor_", id)), "Valor:",
                    choices = c("Selecione..." = "", valores_unicos))
      }
    }
  })
  
  # Adicionar filtro
  observeEvent(session$input[[paste0("btn_adicionar_filtro_", id)]], {
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(session$input[[paste0("modal_operador_", id)]] != "")
    
    campo_nome <- session$input[[paste0("modal_campo_", id)]]
    operador <- session$input[[paste0("modal_operador_", id)]]
    
    novo_filtro <- list(
      campo = campo_nome,
      operador = operador,
      operador_nome = switch(operador, "==" = "é igual a", ">" = "é maior que",
                             ">=" = "é maior ou igual a", "<" = "é menor que", 
                             "<=" = "é menor ou igual a", "between" = "está entre",
                             "in" = "está entre as opções")
    )
    
    if (operador == "between") {
      req(session$input[[paste0("modal_valor_min_", id)]], 
          session$input[[paste0("modal_valor_max_", id)]])
      novo_filtro$valor <- c(session$input[[paste0("modal_valor_min_", id)]], 
                             session$input[[paste0("modal_valor_max_", id)]])
      novo_filtro$valor_texto <- paste(session$input[[paste0("modal_valor_min_", id)]], 
                                       "e", session$input[[paste0("modal_valor_max_", id)]])
    } else if (operador == "in") {
      req(length(session$input[[paste0("modal_valor_", id)]]) > 0)
      novo_filtro$valor <- session$input[[paste0("modal_valor_", id)]]
      if (length(novo_filtro$valor) <= 3) {
        novo_filtro$valor_texto <- paste(novo_filtro$valor, collapse = ", ")
      } else {
        novo_filtro$valor_texto <- paste0(length(novo_filtro$valor), " opções selecionadas")
      }
    } else {
      req(session$input[[paste0("modal_valor_", id)]] != "")
      novo_filtro$valor <- session$input[[paste0("modal_valor_", id)]]
      novo_filtro$valor_texto <- as.character(novo_filtro$valor)
    }
    
    filtros_atuais <- filtros_pendentes()
    filtros_atuais[[length(filtros_atuais) + 1]] <- novo_filtro
    filtros_pendentes(filtros_atuais)
    
    updateSelectInput(session, paste0("modal_campo_", id), selected = "")
    
    showNotification("Filtro adicionado! Clique em 'Aplicar Filtros' para confirmar.", 
                     type = "message", duration = 2)
  })
  
  # UI: Lista de filtros ativos
  output[[paste0("ui_modal_filtros_ativos_", id)]] <- renderUI({
    filtros <- filtros_pendentes()
    if (length(filtros) == 0) {
      return(div(class = "alert alert-info", icon("info-circle"), 
                 " Nenhum filtro adicionado ainda."))
    }
    
    tagList(
      lapply(seq_along(filtros), function(i) {
        f <- filtros[[i]]
        div(class = "alert alert-secondary",
            style = "display: flex; justify-content: space-between; align-items: center; padding: 8px 12px; margin-bottom: 8px;",
            div(style = "flex-grow: 1;",
                tags$strong(f$campo), " ", f$operador_nome, " ", tags$code(f$valor_texto)),
            actionButton(ns(paste0("btn_remove_filtro_", id, "_", i)), icon("times"),
                         class = "btn-sm btn-danger", style = "padding: 2px 8px;")
        )
      })
    )
  })
  
  # Remover filtros individuais
  observe({
    filtros <- filtros_pendentes()
    if (length(filtros) > 0) {
      lapply(seq_along(filtros), function(i) {
        observeEvent(session$input[[paste0("btn_remove_filtro_", id, "_", i)]], {
          filtros_atuais <- filtros_pendentes()
          filtros_atuais[[i]] <- NULL
          filtros_pendentes(filtros_atuais)
        })
      })
    }
  })
  
  # Limpar todos os filtros
  observeEvent(session$input[[paste0("btn_limpar_", id)]], {
    filtros_pendentes(list())
    showNotification("Todos os filtros foram removidos.", type = "warning", duration = 2)
  })
  
  # Aplicar filtros
  observeEvent(session$input[[paste0("btn_aplicar_", id)]], {
    filtros_aplicados(filtros_pendentes())
    removeModal()
    n_filtros <- length(filtros_pendentes())
    if (n_filtros > 0) {
      showNotification(paste0(n_filtros, " filtro(s) aplicado(s) com sucesso!"),
                       type = "message", duration = 3)
    } else {
      showNotification("Filtros removidos. Mostrando todos os dados.", 
                       type = "default", duration = 3)
    }
  })
  
  # UI: Resumo dos filtros aplicados
  output[[paste0("ui_resumo_filtros_", id)]] <- renderUI({
    filtros <- filtros_aplicados()
    if (length(filtros) == 0) {
      return(helpText(icon("info-circle"), " Nenhum filtro aplicado.", br(), br(),
                      "Clique em 'Configurar Filtros' para adicionar."))
    }
    
    tagList(
      div(class = "alert alert-success", style = "padding: 8px; margin-bottom: 10px;",
          tags$strong(icon("check-circle"), sprintf(" %d filtro(s) ativo(s)", length(filtros)))),
      div(style = "max-height: 300px; overflow-y: auto; font-size: 0.85em;",
          lapply(seq_along(filtros), function(i) {
            f <- filtros[[i]]
            div(style = "padding: 6px; margin-bottom: 6px; background: #f8f9fa; border-left: 3px solid #17a2b8; border-radius: 3px;",
                tags$strong(f$campo), br(),
                tags$small(f$operador_nome, " ", tags$code(f$valor_texto)))
          })
      )
    )
  })
}

#' Aplicar lista de filtros em um data frame
#' 
#' @param df Data frame ou sf object
#' @param filtros Lista de filtros
#' @return Data frame filtrado
#' @export
aplicar_filtros_em_df <- function(df, filtros) {
  if (length(filtros) == 0) return(df)
  
  # Verificar se é sf e armazenar geometry
  is_sf <- inherits(df, "sf")
  if (is_sf) {
    geom_col <- attr(df, "sf_column")
    geom <- df[[geom_col]]
    df_work <- sf::st_drop_geometry(df)
  } else {
    df_work <- df
  }
  
  # Criar índice original para rastrear linhas
  df_work$.original_idx <- seq_len(nrow(df_work))
  
  # Aplicar filtros
  for (filtro in filtros) {
    campo <- filtro$campo
    operador <- filtro$operador
    valor <- filtro$valor
    
    if (operador == "==") {
      df_work <- df_work[df_work[[campo]] == valor, ]
    } else if (operador == ">") {
      df_work <- df_work[df_work[[campo]] > valor, ]
    } else if (operador == ">=") {
      df_work <- df_work[df_work[[campo]] >= valor, ]
    } else if (operador == "<") {
      df_work <- df_work[df_work[[campo]] < valor, ]
    } else if (operador == "<=") {
      df_work <- df_work[df_work[[campo]] <= valor, ]
    } else if (operador == "between") {
      df_work <- df_work[df_work[[campo]] >= valor[1] & df_work[[campo]] <= valor[2], ]
    } else if (operador == "in") {
      df_work <- df_work[df_work[[campo]] %in% valor, ]
    }
  }
  
  # Re-adicionar geometry se era sf
  if (is_sf) {
    idx_mantidos <- df_work$.original_idx
    df_work$.original_idx <- NULL
    geom_filtrado <- geom[idx_mantidos]
    df_result <- sf::st_sf(df_work, geometry = geom_filtrado)
    return(df_result)
  } else {
    df_work$.original_idx <- NULL
    return(df_work)
  }
}
