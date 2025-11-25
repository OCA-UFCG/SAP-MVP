# =====================================================================
# SISTEMA DE CONSTRUTOR DE COLUNAS
# =====================================================================

#' Criar botão para abrir modal de construtor de colunas
#' 
#' @param ns Namespace function do módulo
#' @param id ID único do sistema de colunas
#' @return tagList com botão e UI de resumo
criar_botao_colunas_ui <- function(ns, id) {
  tagList(
    actionButton(
      ns(paste0("btn_abrir_modal_colunas_", id)),
      "Construtor de Colunas",
      icon = icon("table-columns"),
      class = "btn-success w-100 mb-2"
    ),
    hr(),
    uiOutput(ns(paste0("ui_resumo_colunas_", id)))
  )
}

#' Criar sistema de construtor de colunas com modal
#' 
#' @param session Sessão Shiny
#' @param ns Namespace function
#' @param id ID único do sistema
#' @param results_reactive Reactive contendo os dados
#' @param colunas_criadas Reactive com colunas atualmente criadas
criar_sistema_colunas_modal <- function(session, ns, id, results_reactive, colunas_criadas) {
  colunas_pendentes <- reactiveVal(list())
  
  # ========================================
  # OBSERVER: Abrir modal de colunas
  # ========================================
  observeEvent(session$input[[paste0("btn_abrir_modal_colunas_", id)]], {
    req(results_reactive())
    colunas_pendentes(colunas_criadas())
    results <- results_reactive()
    campos <- names(results)
    campos_excluir <- c("geometry")
    campos_disponiveis <- setdiff(campos, campos_excluir)
    
    showModal(modalDialog(
      title = tags$div(icon("table-columns"), " Construtor de Colunas", 
                       style = "font-size: 1.2em; font-weight: bold;"),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(ns(paste0("btn_limpar_colunas_", id)), "Limpar Tudo", 
                     icon = icon("trash"), class = "btn-warning"),
        modalButton("Cancelar"),
        actionButton(ns(paste0("btn_aplicar_colunas_", id)), "Aplicar Colunas", 
                     icon = icon("check"), class = "btn-success")
      ),
      card(
        card_header(icon("list"), " Colunas Criadas"),
        uiOutput(ns(paste0("ui_modal_colunas_ativas_", id)))
      ),
      hr(),
      card(
        card_header(icon("plus-circle"), " Criar Nova Coluna"),
        textInput(ns(paste0("modal_nome_coluna_", id)), "Nome da nova coluna:",
                  placeholder = "Ex: densidade_pop"),
        selectInput(ns(paste0("modal_tipo_operacao_", id)), "Tipo de operação:",
                    choices = c("Selecione..." = "",
                                "Porcentagem do total da coluna (%)" = "pct_total",
                                "Expressão customizada" = "custom")),
        uiOutput(ns(paste0("ui_modal_operacao_colunas_", id))),
        
        # Ajuda contextual
        conditionalPanel(
          condition = paste0("input.modal_tipo_operacao_", id, " == 'custom'"),
          ns = ns,
          div(
            class = "alert alert-info mt-2",
            icon("lightbulb"), " ", strong("Exemplos de expressões:"),
            tags$ul(
              tags$li(code("pop_total / area_km2"), " - densidade populacional"),
              tags$li(code("col1 + col2 + col3"), " - soma de múltiplas colunas"),
              tags$li(code("col1 / sum(col2, na.rm = TRUE)"), " - divisão pela soma"),
              tags$li(code("log10(valor + 1)"), " - transformação logarítmica"),
              tags$li(code("ifelse(col1 > 100, 'Alto', 'Baixo')"), " - categorização"),
              tags$li(code("(col1 - mean(col1, na.rm = TRUE)) / sd(col1, na.rm = TRUE)"), " - z-score"),
              tags$li(code("pmax(col1, col2, col3)"), " - máximo entre colunas"),
              tags$li(code("rowSums(across(c(col1, col2, col3)))"), " - soma de linhas")
            ),
            tags$small(class = "text-muted", 
                       "⚠️ A expressão deve retornar um vetor do mesmo tamanho que o número de linhas")
          )
        ),
        
        actionButton(ns(paste0("btn_adicionar_coluna_", id)), "Adicionar Coluna",
                     icon = icon("plus"), class = "btn-primary w-100 mt-2")
      )
    ))
  })
  
  # ========================================
  # OUTPUT: Input para tipo de operação
  # ========================================
  output <- session$output
  output[[paste0("ui_modal_operacao_colunas_", id)]] <- renderUI({
    req(session$input[[paste0("modal_tipo_operacao_", id)]] != "")
    tipo <- session$input[[paste0("modal_tipo_operacao_", id)]]
    results <- results_reactive()
    campos <- names(results)
    campos_excluir <- c("geometry")
    campos_disponiveis <- setdiff(campos, campos_excluir)
    
    if (tipo == "pct_total") {
      selectInput(ns(paste0("modal_coluna_pct_", id)), "Coluna para calcular % do total:",
                  choices = c("Selecione..." = "", campos_disponiveis))
    } else if (tipo == "custom") {
      tagList(
        textAreaInput(ns(paste0("modal_expressao_coluna_", id)), 
                      "Expressão (use nomes das colunas):",
                      placeholder = "Ex: pop_total / area_km2",
                      rows = 4),
        tags$small(class = "text-muted", 
                   "Colunas disponíveis: ", 
                   paste(head(campos_disponiveis, 10), collapse = ", "),
                   if(length(campos_disponiveis) > 10) "..." else "")
      )
    }
  })
  
  # ========================================
  # OBSERVER: Adicionar coluna
  # ========================================
  observeEvent(session$input[[paste0("btn_adicionar_coluna_", id)]], {
    req(session$input[[paste0("modal_nome_coluna_", id)]] != "")
    req(session$input[[paste0("modal_tipo_operacao_", id)]] != "")
    
    nome <- trimws(session$input[[paste0("modal_nome_coluna_", id)]])
    tipo <- session$input[[paste0("modal_tipo_operacao_", id)]]
    
    # Validar nome da coluna
    if (!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", nome)) {
      showNotification(
        "Nome inválido! Use apenas letras, números e underscore. Deve começar com letra.",
        type = "error",
        duration = 4
      )
      return()
    }
    
    nova_coluna <- list(
      nome = nome,
      tipo = tipo
    )
    
    if (tipo == "pct_total") {
      req(session$input[[paste0("modal_coluna_pct_", id)]] != "")
      nova_coluna$coluna <- session$input[[paste0("modal_coluna_pct_", id)]]
      nova_coluna$descricao <- paste0("(", nova_coluna$coluna, " / soma total) × 100")
      nova_coluna$expressao <- paste0("(", nova_coluna$coluna, " / sum(", nova_coluna$coluna, ", na.rm = TRUE)) * 100")
    } else if (tipo == "custom") {
      req(session$input[[paste0("modal_expressao_coluna_", id)]] != "")
      nova_coluna$expressao <- trimws(session$input[[paste0("modal_expressao_coluna_", id)]])
      nova_coluna$descricao <- nova_coluna$expressao
      
      if (nchar(nova_coluna$expressao) == 0) {
        showNotification("Expressão não pode estar vazia!", type = "error", duration = 3)
        return()
      }
    }
    
    colunas_atuais <- colunas_pendentes()
    colunas_atuais[[length(colunas_atuais) + 1]] <- nova_coluna
    colunas_pendentes(colunas_atuais)
    
    # Limpar inputs
    updateTextInput(session, paste0("modal_nome_coluna_", id), value = "")
    updateSelectInput(session, paste0("modal_tipo_operacao_", id), selected = "")
    
    showNotification("Coluna adicionada! Clique em 'Aplicar Colunas' para confirmar.", 
                     type = "message", duration = 2)
  })
  
  # ========================================
  # OUTPUT: Lista de colunas ativas no modal
  # ========================================
  output[[paste0("ui_modal_colunas_ativas_", id)]] <- renderUI({
    colunas <- colunas_pendentes()
    if (length(colunas) == 0) {
      return(tags$p(class = "text-muted", icon("circle-info"), " Nenhuma coluna criada"))
    }
    lapply(seq_along(colunas), function(i) {
      col <- colunas[[i]]
      div(
        class = "border rounded p-2 mb-2 bg-light",
        div(
          class = "d-flex justify-content-between align-items-start",
          div(
            tags$strong(col$nome),
            tags$br(),
            tags$code(class = "text-muted small", col$descricao)
          ),
          actionButton(
            ns(paste0("btn_remover_coluna_", id, "_", i)),
            icon("trash"),
            class = "btn-sm btn-danger",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", 
                              ns(paste0("remover_coluna_", id)), i)
          )
        )
      )
    })
  })
  
  # ========================================
  # OBSERVER: Remover coluna
  # ========================================
  observeEvent(session$input[[paste0("remover_coluna_", id)]], {
    idx <- session$input[[paste0("remover_coluna_", id)]]
    colunas_atuais <- colunas_pendentes()
    if (idx > 0 && idx <= length(colunas_atuais)) {
      colunas_pendentes(colunas_atuais[-idx])
      showNotification("Coluna removida", type = "message", duration = 2)
    }
  })
  
  # ========================================
  # OBSERVER: Limpar todas as colunas
  # ========================================
  observeEvent(session$input[[paste0("btn_limpar_colunas_", id)]], {
    colunas_pendentes(list())
    showNotification("Todas as colunas foram removidas", type = "warning", duration = 2)
  })
  
  # ========================================
  # OBSERVER: Aplicar colunas
  # ========================================
  observeEvent(session$input[[paste0("btn_aplicar_colunas_", id)]], {
    colunas_criadas(colunas_pendentes())
    removeModal()
    showNotification("Colunas aplicadas com sucesso!", type = "message", duration = 3)
  })
}

#' Aplicar colunas criadas ao dataframe
#' 
#' @param df Dataframe
#' @param colunas Lista de colunas a criar
#' @return Dataframe com novas colunas
aplicar_colunas <- function(df, colunas) {
  if (length(colunas) == 0) return(df)
  
  for (col in colunas) {
    tryCatch({
      # Parse e avalie a expressão
      expr <- rlang::parse_expr(col$expressao)
      
      # Criar ambiente com acesso às colunas do dataframe
      # Usar dplyr::mutate para ter acesso a across(), rowSums(), etc
      df <- dplyr::mutate(df, !!col$nome := !!expr)
      
      # Validar que o resultado é um vetor do tamanho correto
      if (length(df[[col$nome]]) != nrow(df)) {
        warning("Coluna '", col$nome, "' não produziu um vetor do tamanho correto. Removendo...")
        df[[col$nome]] <- NULL
        showNotification(
          paste0("Erro: A coluna '", col$nome, "' não produziu um vetor válido e foi removida."),
          type = "error",
          duration = 5
        )
      }
      
    }, error = function(e) {
      warning("Erro ao criar coluna '", col$nome, "': ", e$message)
      showNotification(
        paste0("Erro ao criar coluna '", col$nome, "': ", e$message),
        type = "error",
        duration = 5
      )
    })
  }
  
  df
}
