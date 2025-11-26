# =====================================================================
# SISTEMA COMPLETO DE FILTROS COM MODAL
# =====================================================================

#' Criar sistema de filtros com modal interativo
#' 
#' @param session Sessão Shiny
#' @param ns Namespace function
#' @param id ID único do sistema de filtros
#' @param results_reactive Reactive contendo os dados
#' @param filtros_aplicados Reactive com filtros atualmente aplicados
#' @return NULL (side effects via observers)
criar_sistema_filtros_modal <- function(session, ns, id, results_reactive, filtros_aplicados) {
  filtros_pendentes <- reactiveVal(list())
  
  # ========================================
  # OBSERVER: Abrir modal de filtros
  # ========================================
  observeEvent(session$input[[paste0("btn_abrir_modal_", id)]], {
    req(results_reactive())
    filtros_pendentes(filtros_aplicados())
    results <- results_reactive()
    campos <- names(results)
    campos_excluir <- c("geometry", "CD_MUN")
    campos_disponiveis <- setdiff(campos, campos_excluir)
    updateSelectInput(session, paste0("modal_campo_", id), 
                      choices = c("Selecione..." = "", campos_disponiveis))
    
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
        # Suporte para filtros compostos
        checkboxInput(ns(paste0("modal_composto_", id)), 
                      "Filtro composto (usar operadores | ou &)", 
                      value = FALSE),
        conditionalPanel(
          condition = paste0("input.modal_composto_", id),
          ns = ns,
          textAreaInput(ns(paste0("modal_expressao_", id)), 
                        "Expressão do filtro:",
                        placeholder = "Ex: areakm2nv5 > 0 | areakm2nv4 > 0",
                        rows = 3)
        ),
        actionButton(ns(paste0("btn_adicionar_filtro_", id)), "Adicionar Filtro",
                     icon = icon("plus"), class = "btn-primary w-100 mt-2")
      )
    ))
  })
  
  # ========================================
  # OUTPUT: Operadores disponíveis
  # ========================================
  output <- session$output
  output[[paste0("ui_modal_operador_", id)]] <- renderUI({
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(results_reactive())
    req(!isTRUE(session$input[[paste0("modal_composto_", id)]]))
    
    results <- results_reactive()
    campo_nome <- session$input[[paste0("modal_campo_", id)]]
    
    if (!campo_nome %in% names(results)) {
      return(tags$p(class = "text-danger", "Coluna não encontrada"))
    }
    
    campo <- results[[campo_nome]]
    tipo_info <- analisar_tipo_campo(campo)
    
    selectInput(ns(paste0("modal_operador_", id)), "Condição:",
                choices = tipo_info$operadores)
  })
  
  # ========================================
  # OUTPUT: Input de valores
  # ========================================
  output[[paste0("ui_modal_valor_", id)]] <- renderUI({
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(session$input[[paste0("modal_operador_", id)]] != "")
    req(results_reactive())
    req(!isTRUE(session$input[[paste0("modal_composto_", id)]]))
    
    results <- results_reactive()
    campo_nome <- session$input[[paste0("modal_campo_", id)]]
    
    if (!campo_nome %in% names(results)) {
      return(tags$p(class = "text-danger", "Coluna não encontrada"))
    }
    
    campo <- results[[campo_nome]]
    operador <- session$input[[paste0("modal_operador_", id)]]
    tipo_info <- analisar_tipo_campo(campo)
    
    if (tipo_info$tipo == "numerico_continuo") {
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
      valores_unicos <- sort(unique(campo[!is.na(campo)]))
      
      if (length(valores_unicos) > 0) {
        if (operador == "in") {
          selectizeInput(ns(paste0("modal_valor_", id)), "Valores:",
                         choices = valores_unicos, 
                         selected = NULL, 
                         multiple = TRUE,
                         options = list(placeholder = "Selecione um ou mais...",
                                        plugins = list("remove_button")))
        } else {
          selectInput(ns(paste0("modal_valor_", id)), "Valor:",
                      choices = c("Selecione..." = "", valores_unicos))
        }
      } else {
        tags$p(class = "text-warning", "Nenhum valor único encontrado")
      }
    }
  })
  
  # ========================================
  # OBSERVER: Adicionar filtro
  # ========================================
  observeEvent(session$input[[paste0("btn_adicionar_filtro_", id)]], {
    results <- results_reactive()
    
    # Suporte para filtros compostos
    if (isTRUE(session$input[[paste0("modal_composto_", id)]])) {
      expressao <- session$input[[paste0("modal_expressao_", id)]]
      req(nchar(trimws(expressao)) > 0)
      
      novo_filtro <- criar_filtro_composto(expressao)
      
      filtros_atuais <- filtros_pendentes()
      filtros_atuais[[length(filtros_atuais) + 1]] <- novo_filtro
      filtros_pendentes(filtros_atuais)
      
      showNotification("Filtro composto adicionado! Clique em 'Aplicar Filtros' para confirmar.", 
                       type = "message", duration = 2)
      return()
    }
    
    # Filtro simples
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(session$input[[paste0("modal_operador_", id)]] != "")
    
    campo_nome <- session$input[[paste0("modal_campo_", id)]]
    operador <- session$input[[paste0("modal_operador_", id)]]
    
    if (operador == "between") {
      req(session$input[[paste0("modal_valor_min_", id)]], 
          session$input[[paste0("modal_valor_max_", id)]])
      valor <- c(session$input[[paste0("modal_valor_min_", id)]], 
                 session$input[[paste0("modal_valor_max_", id)]])
    } else {
      req(session$input[[paste0("modal_valor_", id)]])
      valor <- session$input[[paste0("modal_valor_", id)]]
    }
    
    novo_filtro <- criar_filtro_simples(campo_nome, operador, valor)
    
    filtros_atuais <- filtros_pendentes()
    filtros_atuais[[length(filtros_atuais) + 1]] <- novo_filtro
    filtros_pendentes(filtros_atuais)
    
    showNotification("Filtro adicionado! Clique em 'Aplicar Filtros' para confirmar.", 
                     type = "message", duration = 2)
  })
  
  # ========================================
  # OUTPUT: Lista de filtros ativos no modal
  # ========================================
  output[[paste0("ui_modal_filtros_ativos_", id)]] <- renderUI({
    filtros <- filtros_pendentes()
    criar_cards_filtros_ativos(filtros, ns, id)
  })
  
  # ========================================
  # OBSERVERS: Remover filtros individuais
  # ========================================
  observe({
    filtros <- filtros_pendentes()
    lapply(seq_along(filtros), function(i) {
      observeEvent(session$input[[paste0("btn_remover_filtro_", id, "_", i)]], {
        filtros_atuais <- filtros_pendentes()
        if (i <= length(filtros_atuais)) {
          filtros_atuais[[i]] <- NULL
          filtros_pendentes(filtros_atuais)
          showNotification("Filtro removido!", type = "warning", duration = 1)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  })
  
  # ========================================
  # OBSERVER: Limpar todos os filtros
  # ========================================
  observeEvent(session$input[[paste0("btn_limpar_", id)]], {
    filtros_pendentes(list())
    showNotification("Todos os filtros foram removidos", type = "warning", duration = 2)
  })
  
  # ========================================
  # OBSERVER: Aplicar filtros
  # ========================================
  observeEvent(session$input[[paste0("btn_aplicar_", id)]], {
    filtros_aplicados(filtros_pendentes())
    removeModal()
    showNotification("Filtros aplicados com sucesso!", type = "message", duration = 2)
  })
  
  # ========================================
  # OUTPUT: Resumo de filtros fora do modal
  # ========================================
  output[[paste0("ui_resumo_filtros_", id)]] <- renderUI({
    filtros <- filtros_aplicados()
    criar_resumo_filtros(filtros)
  })
}
