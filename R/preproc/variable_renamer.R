# =====================================================================
# utils/variable_renamer.R - VERSÃO CORRIGIDA
# =====================================================================

#' Criar botão para abrir modal de renomear variáveis
criar_botao_renomear_ui <- function(ns, id) {
  tagList(
    actionButton(
      ns(paste0("btn_abrir_modal_renomear_", id)),
      "Renomear Variáveis",
      icon = icon("i-cursor"),
      class = "btn-warning w-100 mb-2"
    ),
    hr(),
    uiOutput(ns(paste0("ui_resumo_renomear_", id)))
  )
}

#' Criar sistema de renomear variáveis com modal
criar_sistema_renomear_modal <- function(session, ns, id, results_reactive, nomes_editados) {
  
  # ========================================
  # OBSERVER: Abrir modal de renomear
  # ========================================
  observeEvent(session$input[[paste0("btn_abrir_modal_renomear_", id)]], {
    req(results_reactive())
    
    results <- results_reactive()
    
    # Pegar nomes ORIGINAIS (antes de renomear)
    nomes_originais <- attr(results, "nomes_originais")
    if (is.null(nomes_originais)) {
      # Se não tem atributo, usar os nomes atuais como originais
      campos <- names(results)
    } else {
      campos <- names(nomes_originais)
    }
    
    campos_excluir <- c("geometry", "CD_MUN", "NM_REGIAO", "NM_UF", "NM_MUN")
    campos_disponiveis <- setdiff(campos, campos_excluir)
    
    # Pegar nomes já editados
    nomes_atuais <- nomes_editados()
    
    # Atualizar choices do select
    updateSelectInput(session, paste0("modal_select_var_", id), 
                      choices = c("Selecione uma variável..." = "", campos_disponiveis))
    
    showModal(modalDialog(
      title = tags$div(icon("i-cursor"), " Renomear Variáveis", 
                       style = "font-size: 1.2em; font-weight: bold;"),
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        actionButton(ns(paste0("btn_limpar_renomear_", id)), "Limpar Tudo", 
                     icon = icon("trash"), class = "btn-warning"),
        modalButton("Cancelar"),
        actionButton(ns(paste0("btn_aplicar_renomear_", id)), "Aplicar Nomes", 
                     icon = icon("check"), class = "btn-success")
      ),
      
      card(
        card_header(icon("list"), " Renomeações Ativas"),
        uiOutput(ns(paste0("ui_modal_renomear_ativos_", id)))
      ),
      
      hr(),
      
      card(
        card_header(icon("plus-circle"), " Adicionar Renomeação"),
        selectInput(
          ns(paste0("modal_select_var_", id)),
          "Selecione a variável:",
          choices = NULL
        ),
        textInput(
          ns(paste0("modal_novo_nome_", id)),
          "Novo nome:",
          placeholder = "Digite o novo nome"
        ),
        actionButton(
          ns(paste0("btn_adicionar_renomear_", id)),
          "Adicionar",
          icon = icon("plus"),
          class = "btn-primary w-100"
        )
      )
    ))
  })
  
  # ========================================
  # OUTPUT: Lista de renomeações ativas
  # ========================================
  output <- session$output
  output[[paste0("ui_modal_renomear_ativos_", id)]] <- renderUI({
    nomes <- nomes_editados()
    if (length(nomes) == 0) {
      return(tags$p(class = "text-muted", icon("circle-info"), " Nenhuma renomeação ativa"))
    }
    
    lapply(seq_along(nomes), function(i) {
      nome_orig <- names(nomes)[i]
      nome_novo <- nomes[[i]]
      
      div(
        class = "border rounded p-2 mb-2 bg-light",
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            tags$strong(nome_orig), " → ", tags$code(nome_novo)
          ),
          actionButton(
            ns(paste0("btn_remover_renomear_", id, "_", i)),
            icon("trash"),
            class = "btn-sm btn-danger",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", 
                              ns(paste0("remover_renomear_", id)), i)
          )
        )
      )
    })
  })
  
  # ========================================
  # OBSERVER: Adicionar renomeação
  # ========================================
  observeEvent(session$input[[paste0("btn_adicionar_renomear_", id)]], {
    var_selecionada <- session$input[[paste0("modal_select_var_", id)]]
    novo_nome <- trimws(session$input[[paste0("modal_novo_nome_", id)]])
    
    req(var_selecionada != "")
    req(nchar(novo_nome) > 0)
    
    # Validar nome
    if (!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", novo_nome)) {
      showNotification(
        "Nome inválido! Use apenas letras, números e underscore. Deve começar com letra.",
        type = "error",
        duration = 4
      )
      return()
    }
    
    # Adicionar ao mapeamento
    nomes_atuais <- nomes_editados()
    nomes_atuais[[var_selecionada]] <- novo_nome
    nomes_editados(nomes_atuais)
    
    # Limpar inputs
    updateSelectInput(session, paste0("modal_select_var_", id), selected = "")
    updateTextInput(session, paste0("modal_novo_nome_", id), value = "")
    
    showNotification("Renomeação adicionada!", type = "message", duration = 2)
  })
  
  # ========================================
  # OBSERVER: Remover renomeação
  # ========================================
  observeEvent(session$input[[paste0("remover_renomear_", id)]], {
    idx <- session$input[[paste0("remover_renomear_", id)]]
    nomes_atuais <- nomes_editados()
    if (idx > 0 && idx <= length(nomes_atuais)) {
      nomes_atuais[[idx]] <- NULL
      nomes_editados(nomes_atuais)
      showNotification("Renomeação removida", type = "warning", duration = 2)
    }
  })
  
  # ========================================
  # OBSERVER: Limpar todos os nomes
  # ========================================
  observeEvent(session$input[[paste0("btn_limpar_renomear_", id)]], {
    nomes_editados(list())
    showNotification("Todas as renomeações foram removidas", type = "warning", duration = 2)
  })
  
  # ========================================
  # OBSERVER: Aplicar nomes
  # ========================================
  observeEvent(session$input[[paste0("btn_aplicar_renomear_", id)]], {
    removeModal()
    showNotification("Nomes aplicados com sucesso!", type = "message", duration = 3)
  })
  
  # ========================================
  # OUTPUT: Resumo de nomes editados (sidebar)
  # ========================================
  output[[paste0("ui_resumo_renomear_", id)]] <- renderUI({
    nomes <- nomes_editados()
    if (length(nomes) == 0) {
      return(tags$p(class = "text-muted small", icon("circle-info"), " Nenhuma variável renomeada"))
    }
    tagList(
      tags$p(class = "fw-bold small mb-1", paste(length(nomes), "variável(is) renomeada(s):")),
      lapply(names(nomes), function(orig) {
        tags$div(
          class = "small text-muted border-start border-3 border-warning ps-2 mb-1",
          tags$strong(orig), " → ", tags$code(nomes[[orig]])
        )
      })
    )
  })
}

#' Aplicar renomeação de variáveis ao dataframe
#' Mantém os nomes originais como atributo
aplicar_renomeacao <- function(df, nomes_map) {
  if (length(nomes_map) == 0) return(df)
  
  # Guardar nomes originais como atributo (se ainda não existir)
  if (is.null(attr(df, "nomes_originais"))) {
    nomes_orig <- setNames(names(df), names(df))
    attr(df, "nomes_originais") <- nomes_orig
  }
  
  # Aplicar renomeação
  for (nome_orig in names(nomes_map)) {
    if (nome_orig %in% names(df)) {
      novo_nome <- nomes_map[[nome_orig]]
      names(df)[names(df) == nome_orig] <- novo_nome
    }
  }
  
  df
}

#' Obter nome original de uma variável
obter_nome_original <- function(nome_atual, df) {
  nomes_orig <- attr(df, "nomes_originais")
  if (is.null(nomes_orig)) return(nome_atual)
  
  # Procurar qual nome original corresponde ao nome atual
  for (orig in names(nomes_orig)) {
    if (nomes_orig[[orig]] == nome_atual) {
      return(orig)
    }
  }
  
  nome_atual
}

#' Converter vetor de nomes atuais para originais
converter_para_nomes_originais <- function(nomes_atuais, nomes_map) {
  if (length(nomes_map) == 0) return(nomes_atuais)
  
  # Criar mapeamento reverso (novo -> original)
  mapa_reverso <- setNames(names(nomes_map), unlist(nomes_map))
  
  sapply(nomes_atuais, function(nome) {
    if (nome %in% names(mapa_reverso)) {
      mapa_reverso[[nome]]
    } else {
      nome
    }
  })
}