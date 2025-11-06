# =====================================================================
# MÓDULO: PRÉ-PROCESSAMENTO
# =====================================================================

# =====================================================================
# FUNÇÕES AUXILIARES PARA SISTEMA DE FILTROS COM MODAL
# =====================================================================

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

criar_sistema_filtros_modal <- function(session, ns, id, results_reactive, filtros_aplicados) {
  filtros_pendentes <- reactiveVal(list())
  
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
  
  output <- session$output
  output[[paste0("ui_modal_operador_", id)]] <- renderUI({
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(results_reactive())
    req(!isTRUE(session$input[[paste0("modal_composto_", id)]]))
    
    results <- results_reactive()
    campo_nome <- session$input[[paste0("modal_campo_", id)]]
    
    # Verificar se a coluna existe
    if (!campo_nome %in% names(results)) {
      return(tags$p(class = "text-danger", "Coluna não encontrada"))
    }
    
    campo <- results[[campo_nome]]
    
    # Determinar tipo de operadores baseado nos dados
    if (is.numeric(campo) && length(unique(campo[!is.na(campo)])) > 30) {
      selectInput(ns(paste0("modal_operador_", id)), "Condição:",
                  choices = c("Selecione..." = "", 
                              "é igual a" = "==", 
                              "é maior que" = ">",
                              "é maior ou igual a" = ">=", 
                              "é menor que" = "<",
                              "é menor ou igual a" = "<=", 
                              "está entre" = "between"))
    } else {
      selectInput(ns(paste0("modal_operador_", id)), "Condição:",
                  choices = c("Selecione..." = "", 
                              "é igual a" = "==", 
                              "está entre as opções" = "in"))
    }
  })
  
  output[[paste0("ui_modal_valor_", id)]] <- renderUI({
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(session$input[[paste0("modal_operador_", id)]] != "")
    req(results_reactive())
    req(!isTRUE(session$input[[paste0("modal_composto_", id)]]))
    
    results <- results_reactive()
    campo_nome <- session$input[[paste0("modal_campo_", id)]]
    
    # Verificar se a coluna existe
    if (!campo_nome %in% names(results)) {
      return(tags$p(class = "text-danger", "Coluna não encontrada"))
    }
    
    campo <- results[[campo_nome]]
    operador <- session$input[[paste0("modal_operador_", id)]]
    
    if (is.numeric(campo) && length(unique(campo[!is.na(campo)])) > 30) {
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
      
      # Converter para character se necessário para melhor exibição
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
  
  observeEvent(session$input[[paste0("btn_adicionar_filtro_", id)]], {
    results <- results_reactive()
    
    # Suporte para filtros compostos
    if (isTRUE(session$input[[paste0("modal_composto_", id)]])) {
      expressao <- session$input[[paste0("modal_expressao_", id)]]
      req(nchar(trimws(expressao)) > 0)
      
      novo_filtro <- list(
        tipo = "composto",
        expressao = expressao,
        operador_nome = "expressão customizada",
        valor_texto = expressao,
        campo = NULL,
        operador = NULL,
        valor = NULL
      )
      
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
    
    novo_filtro <- list(
      tipo = "simples",
      campo = campo_nome,
      operador = operador,
      operador_nome = switch(operador, 
                             "==" = "é igual a", 
                             ">" = "é maior que",
                             ">=" = "é maior ou igual a", 
                             "<" = "é menor que", 
                             "<=" = "é menor ou igual a", 
                             "between" = "está entre",
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
      valores_selecionados <- session$input[[paste0("modal_valor_", id)]]
      req(length(valores_selecionados) > 0)
      novo_filtro$valor <- valores_selecionados
      if (length(novo_filtro$valor) <= 3) {
        novo_filtro$valor_texto <- paste(novo_filtro$valor, collapse = ", ")
      } else {
        novo_filtro$valor_texto <- paste0(length(novo_filtro$valor), " opções selecionadas")
      }
    } else {
      valor_input <- session$input[[paste0("modal_valor_", id)]]
      req(valor_input != "" && !is.null(valor_input))
      novo_filtro$valor <- valor_input
      novo_filtro$valor_texto <- as.character(novo_filtro$valor)
    }
    
    filtros_atuais <- filtros_pendentes()
    filtros_atuais[[length(filtros_atuais) + 1]] <- novo_filtro
    filtros_pendentes(filtros_atuais)
    
    isolate({
      updateSelectInput(session, paste0("modal_campo_", id), selected = "")
    })
    
    showNotification("Filtro adicionado! Clique em 'Aplicar Filtros' para confirmar.", 
                     type = "message", duration = 2)
  })
  
  output[[paste0("ui_modal_filtros_ativos_", id)]] <- renderUI({
    filtros <- filtros_pendentes()
    if (length(filtros) == 0) {
      return(tags$p(class = "text-muted", icon("circle-info"), " Nenhum filtro ativo"))
    }
    lapply(seq_along(filtros), function(i) {
      filtro <- filtros[[i]]
      div(
        class = "border rounded p-2 mb-2 bg-light",
        div(
          class = "d-flex justify-content-between align-items-start",
          div(
            tags$strong(if(filtro$tipo == "composto") "Filtro Composto" else filtro$campo),
            tags$br(),
            tags$span(class = "text-muted", filtro$operador_nome, 
                      if(filtro$tipo == "composto") "" else paste(": ", filtro$valor_texto))
          ),
          actionButton(
            ns(paste0("btn_remover_filtro_", id, "_", i)),
            icon("trash"),
            class = "btn-sm btn-danger",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", 
                              ns(paste0("remover_filtro_", id)), i)
          )
        )
      )
    })
  })
  
  observeEvent(session$input[[paste0("remover_filtro_", id)]], {
    idx <- session$input[[paste0("remover_filtro_", id)]]
    filtros_atuais <- filtros_pendentes()
    if (idx > 0 && idx <= length(filtros_atuais)) {
      filtros_pendentes(filtros_atuais[-idx])
      showNotification("Filtro removido", type = "message", duration = 2)
    }
  })
  
  observeEvent(session$input[[paste0("btn_limpar_", id)]], {
    filtros_pendentes(list())
    showNotification("Todos os filtros foram removidos", type = "warning", duration = 2)
  })
  
  observeEvent(session$input[[paste0("btn_aplicar_", id)]], {
    filtros_aplicados(filtros_pendentes())
    removeModal()
    showNotification("Filtros aplicados com sucesso!", type = "message", duration = 3)
  })
}

aplicar_filtros <- function(df, filtros) {
  if (length(filtros) == 0) return(df)
  
  for (filtro in filtros) {
    tryCatch({
      # Verificar se é filtro composto
      if (!is.null(filtro$tipo) && filtro$tipo == "composto") {
        expr <- rlang::parse_expr(filtro$expressao)
        df <- dplyr::filter(df, !!expr)
        next
      }
      
      # Filtro simples - validações
      if (is.null(filtro$campo) || is.null(filtro$operador) || is.null(filtro$valor)) {
        warning("Filtro incompleto encontrado, pulando...")
        next
      }
      
      campo <- filtro$campo
      operador <- filtro$operador
      valor <- filtro$valor
      
      # Verificar se a coluna existe no dataframe
      if (!campo %in% names(df)) {
        warning("Coluna '", campo, "' não encontrada no dataframe, pulando filtro...")
        next
      }
      
      # Aplicar filtro baseado no operador
      if (operador == "in") {
        if (length(valor) > 0) {
          df <- dplyr::filter(df, .data[[campo]] %in% valor)
        }
      } else if (operador == "between") {
        if (length(valor) == 2 && !any(is.na(valor))) {
          df <- dplyr::filter(df, dplyr::between(.data[[campo]], valor[1], valor[2]))
        }
      } else if (operador %in% c("==", ">", ">=", "<", "<=")) {
        if (length(valor) > 0 && !is.na(valor[1])) {
          expr <- switch(operador,
                         "==" = rlang::expr(.data[[campo]] == !!valor),
                         ">" = rlang::expr(.data[[campo]] > !!valor),
                         ">=" = rlang::expr(.data[[campo]] >= !!valor),
                         "<" = rlang::expr(.data[[campo]] < !!valor),
                         "<=" = rlang::expr(.data[[campo]] <= !!valor)
          )
          if (!is.null(expr)) {
            df <- dplyr::filter(df, !!expr)
          }
        }
      }
      
    }, error = function(e) {
      warning("Erro ao aplicar filtro: ", e$message)
    })
  }
  
  df
}

# =====================================================================
# FUNÇÕES AUXILIARES PARA CONSTRUTOR DE COLUNAS
# =====================================================================

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

criar_sistema_colunas_modal <- function(session, ns, id, results_reactive, colunas_criadas) {
  colunas_pendentes <- reactiveVal(list())
  
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
  
  observeEvent(session$input[[paste0("remover_coluna_", id)]], {
    idx <- session$input[[paste0("remover_coluna_", id)]]
    colunas_atuais <- colunas_pendentes()
    if (idx > 0 && idx <= length(colunas_atuais)) {
      colunas_pendentes(colunas_atuais[-idx])
      showNotification("Coluna removida", type = "message", duration = 2)
    }
  })
  
  observeEvent(session$input[[paste0("btn_limpar_colunas_", id)]], {
    colunas_pendentes(list())
    showNotification("Todas as colunas foram removidas", type = "warning", duration = 2)
  })
  
  observeEvent(session$input[[paste0("btn_aplicar_colunas_", id)]], {
    colunas_criadas(colunas_pendentes())
    removeModal()
    showNotification("Colunas aplicadas com sucesso!", type = "message", duration = 3)
  })
}

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

# ---- UI ---------------------------------------------------------------
mod_preproc_ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    # SIDEBAR
    sidebar = sidebar(
      # DADOS
      card(
        card_header(
          icon("database"), " Fonte de Dados",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Escolha entre usar o banco de dados padrão ou importar seus próprios dados espaciais",
            placement = "right"
          ),
          class = "bg-primary"
        ),
        checkboxInput(ns("use_default"), "Usar banco de dados padrão", value = TRUE),
        conditionalPanel(
          condition = "!input.use_default",
          ns = ns,
          fileInput(
            ns("upload_geo"),
            "Carregar GeoPackage/Shapefile",
            accept = c(".gpkg", ".shp", ".zip"),
            placeholder = "Selecione arquivo..."
          ),
          helpText("Aceita .gpkg ou .zip contendo .shp")
        )
      ),
      
      # CONSTRUTOR DE COLUNAS (movido para cima dos filtros)
      card(
        card_header(
          icon("table-columns"), " Colunas Derivadas",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Crie novas colunas através de operações matemáticas ou expressões customizadas. As colunas criadas estarão disponíveis para uso nos filtros.",
            placement = "right"
          ),
          class = "bg-primary"
        ),
        criar_botao_colunas_ui(ns, "preproc")
      ),
      
      # FILTROS - SISTEMA COM MODAL
      card(
        card_header(
          icon("filter"), " Filtros",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Crie filtros categóricos, numéricos e compostos para selecionar subconjuntos específicos dos dados. Você pode filtrar tanto colunas originais quanto colunas criadas.",
            placement = "right"
          ),
          class = "bg-primary"
        ),
        criar_botao_filtros_ui(ns, "preproc")
      ),
      
      # VARIÁVEIS
      card(
        card_header(
          icon("chart-line"), " Variáveis",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Selecione as variáveis numéricas que serão utilizadas nos gráficos, correlação e PCA. Apenas estas variáveis serão mantidas no dataset final.",
            placement = "right"
          ),
          class = "bg-primary"
        ),
        selectizeInput(
          ns("vars_keep"),
          NULL,
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Selecione variáveis...",
            plugins = list("remove_button")
          )
        )
      ),
      
      # MAPA
      card(
        card_header(
          icon("map"), " Mapa",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Configure a variável e paleta de cores para o mapa coroplético. Use escala log₁₀ para dados assimétricos",
            placement = "right"
          ),
          class = "bg-primary"
        ),
        selectInput(ns("var_map"), "Variável", choices = NULL),
        layout_columns(
          col_widths = c(8, 4),
          selectInput(ns("pal"), "Paleta", choices = paletas, selected = "viridis"),
          checkboxInput(ns("log_scale"), "Log₁₀", FALSE)
        )
      ),
      
      downloadButton(ns("dl_gpkg"), "Download GeoPackage", icon = icon("download"), class = "btn-success w-100")
    ),
    
    # MAIN CONTENT
    navset_card_tab(
      full_screen = TRUE,
      
      # RESUMO
      nav_panel(
        title = "Resumo",
        icon = icon("table-cells"),
        
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span(
            icon("clipboard-list"), " Resumo do Subconjunto",
            tooltip(
              icon("circle-info", class = "ms-2"),
              "Visão geral estatística dos dados filtrados. Baixe em CSV ou Excel para análise externa",
              placement = "bottom"
            )
          ),
          div(
            downloadButton(ns("dl_csv"), "CSV", class = "btn-sm"),
            downloadButton(ns("dl_xlsx"), "Excel", class = "btn-sm ms-1")
          )
        ),
        
        layout_column_wrap(
          width = "250px",
          fill = FALSE,
          value_box(
            title = "Regiões",
            value = textOutput(ns("n_regioes")),
            showcase = icon("globe-americas"),
            theme = "primary"
          ),
          value_box(
            title = "Estados",
            value = textOutput(ns("n_ufs")),
            showcase = icon("map"),
            theme = "primary"
          ),
          value_box(
            title = "Municípios",
            value = textOutput(ns("n_municipios")),
            showcase = icon("city"),
            theme = "primary"
          ),
          value_box(
            title = "Variáveis",
            value = textOutput(ns("n_vars")),
            showcase = icon("list-ol"),
            theme = "primary"
          )
        ),
        
        DTOutput(ns("tab_summary"))
      ),
      
      # DISTRIBUIÇÕES
      nav_panel(
        title = "Distribuições",
        icon = icon("chart-column"),
        
        card_header(
          icon("chart-simple"), " Distribuições das variáveis",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Histogramas e boxplots das variáveis selecionadas no sidebar. Útil para identificar outliers e padrões de distribuição",
            placement = "bottom"
          )
        ),
        
        layout_column_wrap(
          width = 1/2,
          card(
            card_header(
              icon("chart-bar"), " Histogramas",
              tooltip(
                icon("circle-info", class = "ms-2"),
                "Frequência de valores. Mostra a distribuição dos dados",
                placement = "bottom"
              )
            ),
            plotlyOutput(ns("p_hist"), height = "400px")
          ),
          card(
            card_header(
              icon("square"), " Boxplots",
              tooltip(
                icon("circle-info", class = "ms-2"),
                "Resumo visual: mediana, quartis e outliers",
                placement = "bottom"
              )
            ),
            plotlyOutput(ns("p_box"), height = "400px")
          )
        )
      ),
      
      # CORRELAÇÃO
      nav_panel(
        title = "Correlação",
        icon = icon("grip"),
        
        card_header(
          icon("circle-nodes"), " Matriz de Correlação (Pearson)",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Correlação de Pearson entre variáveis. Valores próximos de 1 ou -1 indicam forte correlação. Requer mínimo 2 variáveis",
            placement = "bottom"
          )
        ),
        plotlyOutput(ns("p_corr"), height = "600px")
      ),
      
      # PCA
      nav_panel(
        title = "PCA",
        icon = icon("compass"),
        
        card_header(
          icon("bullseye"), " Análise de Componentes Principais",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "PCA com dados normalizados (z-score). PC1 e PC2 capturam a maior variância dos dados. Útil para redução de dimensionalidade",
            placement = "bottom"
          )
        ),
        plotlyOutput(ns("p_pca"), height = "600px")
      ),
      
      # MAPA
      nav_panel(
        title = "Mapa",
        icon = icon("earth-americas"),
        
        card_header(
          icon("map-location-dot"), " Mapa Coroplético",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Visualização espacial da variável selecionada. Passe o mouse sobre os municípios para ver detalhes",
            placement = "bottom"
          )
        ),
        leafletOutput(ns("map"), height = "calc(100vh - 250px)")
      )
    )
  )
}

# ---- SERVER -----------------------------------------------------------
mod_preproc_server <- function(id, data_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Gerenciar fonte de dados
    data_source <- reactive({
      if (input$use_default) {
        return(data_global())
      } else {
        req(input$upload_geo)
        
        file_ext <- tools::file_ext(input$upload_geo$name)
        
        tryCatch({
          if (file_ext == "gpkg") {
            sf::st_read(input$upload_geo$datapath, quiet = TRUE)
          } else if (file_ext == "shp") {
            sf::st_read(input$upload_geo$datapath, quiet = TRUE)
          } else if (file_ext == "zip") {
            temp_dir <- tempdir()
            unzip(input$upload_geo$datapath, exdir = temp_dir)
            shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
            req(shp_file)
            sf::st_read(shp_file, quiet = TRUE)
          } else {
            NULL
          }
        }, error = function(e) {
          showNotification(
            paste("Erro ao carregar arquivo:", e$message),
            type = "error",
            duration = 5
          )
          NULL
        })
      }
    })
    
    # Validar e preparar dados
    data_sf <- reactive({
      df <- data_source()
      req(inherits(df, "sf"))
      x <- sf::st_make_valid(df)
      ensure_wgs84(x)
    })
    
    # CONSTRUTOR DE COLUNAS
    colunas_criadas_preproc <- reactiveVal(list())
    
    output$ui_resumo_colunas_preproc <- renderUI({
      colunas <- colunas_criadas_preproc()
      if (length(colunas) == 0) {
        return(tags$p(class = "text-muted small", icon("circle-info"), " Nenhuma coluna criada"))
      }
      tagList(
        tags$p(class = "fw-bold small mb-1", paste(length(colunas), "coluna(s) criada(s):")),
        lapply(colunas, function(c) {
          tags$div(
            class = "small text-muted border-start border-3 border-success ps-2 mb-1",
            tags$strong(c$nome), ": ", tags$code(c$descricao)
          )
        })
      )
    })
    
    # CRÍTICO: dados com colunas aplicadas (para uso nos modais)
    data_com_colunas <- reactive({
      df <- data_sf()
      aplicar_colunas(df, colunas_criadas_preproc())
    })
    
    # Passar data_com_colunas para o modal de colunas
    criar_sistema_colunas_modal(session, ns, "preproc", data_com_colunas, colunas_criadas_preproc)
    
    # FILTROS - SISTEMA COM MODAL
    filtros_aplicados_preproc <- reactiveVal(list())
    
    output$ui_resumo_filtros_preproc <- renderUI({
      filtros <- filtros_aplicados_preproc()
      if (length(filtros) == 0) {
        return(tags$p(class = "text-muted small", icon("circle-info"), " Nenhum filtro ativo"))
      }
      tagList(
        tags$p(class = "fw-bold small mb-1", paste(length(filtros), "filtro(s) ativo(s):")),
        lapply(filtros, function(f) {
          texto <- if(f$tipo == "composto") {
            f$expressao
          } else {
            paste(f$operador_nome, f$valor_texto)
          }
          tags$div(
            class = "small text-muted border-start border-3 border-primary ps-2 mb-1",
            tags$strong(if(f$tipo == "composto") "Composto" else f$campo), ": ", texto
          )
        })
      )
    })
    
    # CRÍTICO: Passar data_com_colunas para o modal de filtros (não data_sf)
    criar_sistema_filtros_modal(session, ns, "preproc", data_com_colunas, filtros_aplicados_preproc)
    
    # Atualizar choices
    observe({
      df <- data_com_colunas()
      vnum <- setdiff(num_cols(df), attr(df, "sf_column"))
      updateSelectInput(session, "var_map", choices = vnum, selected = vnum[[1]] %||% character(0))
      updateSelectizeInput(session, "vars_keep", choices = vnum, server = TRUE)
    })
    
    # Dados filtrados usando o novo sistema
    filtered_com_colunas <- reactive({
      df <- data_com_colunas()
      aplicar_filtros(df, filtros_aplicados_preproc())
    })
    
    vars_sel <- reactive({
      v <- input$vars_keep
      if (is.null(v) || length(v) == 0) {
        df <- filtered_com_colunas()
        todas_vars <- setdiff(num_cols(df), attr(df, "sf_column"))
        
        # Defina suas variáveis padrão aqui:
        vars_padrao <- c("spe_mean", "ia_mean", "rural_poverty")
        
        # Usa as padrão se existirem, senão pega as 3 primeiras
        v <- intersect(vars_padrao, todas_vars)
        if (length(v) == 0) {
          v <- head(todas_vars, 3)
        }
      }
      v
    })
    
    # Dados filtrados apenas com variáveis selecionadas + colunas essenciais
    filtered <- reactive({
      df <- filtered_com_colunas()
      
      # Colunas essenciais que sempre devem estar presentes
      colunas_essenciais <- c("NM_REGIAO", "NM_UF", "NM_MUN", "CD_MUN", attr(df, "sf_column"))
      colunas_essenciais <- intersect(colunas_essenciais, names(df))
      
      # Variáveis selecionadas
      v <- vars_sel()
      
      # Combinar e remover duplicatas
      colunas_manter <- unique(c(colunas_essenciais, v))
      
      # Selecionar apenas as colunas necessárias
      dplyr::select(df, dplyr::any_of(colunas_manter))
    })
    
    df_plain <- reactive({ sf::st_drop_geometry(filtered()) })
    
    summary_table <- reactive({
      df <- df_plain()
      base_cols <- intersect(c("NM_REGIAO","NM_UF","NM_MUN"), names(df))
      v <- vars_sel()
      dplyr::select(df, dplyr::any_of(base_cols), dplyr::all_of(v))
    })
    
    # VALUE BOXES
    output$n_regioes <- renderText({
      as.character(dplyr::n_distinct(df_plain()$NM_REGIAO %||% NA))
    })
    
    output$n_ufs <- renderText({
      as.character(dplyr::n_distinct(df_plain()$NM_UF %||% NA))
    })
    
    output$n_municipios <- renderText({
      format(nrow(filtered()), big.mark = ".", decimal.mark = ",")
    })
    
    output$n_vars <- renderText({
      as.character(length(vars_sel()))
    })
    
    # TABELA
    output$tab_summary <- renderDT({
      datatable(
        summary_table(),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # DOWNLOADS
    output$dl_csv <- downloadHandler(
      filename = function() paste0("resumo_", Sys.Date(), ".csv"),
      content = function(file) readr::write_csv(summary_table(), file)
    )
    
    output$dl_xlsx <- downloadHandler(
      filename = function() paste0("resumo_", Sys.Date(), ".xlsx"),
      content = function(file) writexl::write_xlsx(summary_table(), file)
    )
    
    output$dl_gpkg <- downloadHandler(
      filename = function() paste0("subconjunto_", Sys.Date(), ".gpkg"),
      content = function(file) {
        sf::st_write(filtered(), dsn = file, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
      }
    )
    
    # GRÁFICOS
    output$p_hist <- renderPlotly({
      vars <- vars_sel()
      d <- df_plain()
      validate(need(length(vars) >= 1, "Selecione variáveis no sidebar"))
      
      plots <- lapply(vars, function(v) {
        plot_ly(d, x = ~.data[[v]], type = "histogram", nbinsx = 30, name = v) |>
          layout(title = v, xaxis = list(title = v), showlegend = FALSE)
      })
      
      if (length(plots) == 1) {
        plots[[1]]
      } else {
        subplot(plots, nrows = ceiling(length(plots)/2), shareX = FALSE, shareY = FALSE)
      }
    })
    
    output$p_box <- renderPlotly({
      vars <- vars_sel()
      d <- df_plain()
      validate(need(length(vars) >= 1, "Selecione variáveis no sidebar"))
      
      plots <- lapply(vars, function(v) {
        plot_ly(d, y = ~.data[[v]], type = "box", name = v) |>
          layout(title = v, yaxis = list(title = v), showlegend = FALSE)
      })
      
      if (length(plots) == 1) {
        plots[[1]]
      } else {
        subplot(plots, nrows = ceiling(length(plots)/2), shareX = FALSE, shareY = FALSE)
      }
    })
    
    output$p_corr <- renderPlotly({
      v <- vars_sel()
      validate(need(length(v) >= 2, "Selecione 2+ variáveis"))
      
      m <- as.matrix(df_plain()[v])
      future_promise({
        stats::cor(m, use = "pairwise.complete.obs")
      }, seed = TRUE) %...>% (function(C) {
        df <- reshape2::melt(C, varnames = c("x","y"), value.name = "corr")
        plot_ly(df, x = ~x, y = ~y, z = ~corr, colorscale = "RdBu", 
                zmin = -1, zmax = 1, type = "heatmap", showscale = TRUE) |>
          layout(title = "Correlação (Pearson)")
      })
    })
    
    output$p_pca <- renderPlotly({
      v <- vars_sel()
      validate(need(length(v) >= 2, "Selecione 2+ variáveis"))
      
      m <- as.matrix(df_plain()[v])
      future_promise({
        pr <- stats::prcomp(m, center = TRUE, scale. = TRUE)
        list(scores = pr$x, sdev = pr$sdev)
      }, seed = TRUE) %...>% (function(obj) {
        sc <- as.data.frame(obj$scores)
        expvar <- round(100 * (obj$sdev^2 / sum(obj$sdev^2)), 1)
        ax1 <- paste0("PC1 (", expvar[1], "%)")
        ax2 <- paste0("PC2 (", expvar[2], "%)")
        
        plot_ly(sc, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers",
                marker = list(size = 6, opacity = 0.7)) |>
          layout(title = "PCA (normalizado)", xaxis = list(title = ax1), yaxis = list(title = ax2))
      })
    })
    
    # MAPA - usar filtered_com_colunas para ter acesso a todas as colunas
    output$map <- renderLeaflet({
      df <- filtered_com_colunas()
      req(nrow(df) > 0)
      bb <- sf::st_bbox(df)
      
      leaflet(options = leafletOptions(zoomControl = TRUE, minZoom = 3)) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])
    })
    
    observe({
      req(input$var_map)
      df <- filtered_com_colunas()
      req(nrow(df) > 0)
      
      v <- input$var_map
      vals <- as.numeric(sf::st_drop_geometry(df)[[v]])
      vals[!is.finite(vals)] <- NA_real_
      
      if (isTRUE(input$log_scale)) {
        pos <- vals[is.finite(vals) & vals > 0]
        if (length(pos)) {
          minpos <- min(pos, na.rm = TRUE)
          vals <- log10(pmax(vals, minpos))
        } else {
          vals <- NA_real_
        }
      }
      
      pal <- colorNumeric(palette = input$pal, domain = vals, na.color = "#cccccc")
      
      lab <- sprintf(
        "<b>%s</b><br/>%s: %s",
        df$NM_MUN %||% df$CD_MUN %||% seq_len(nrow(df)),
        v,
        format(round(sf::st_drop_geometry(df)[[v]], 3), big.mark = ".", decimal.mark = ",")
      )
      lab <- lapply(lab, htmltools::HTML)
      
      df_s <- tryCatch(msimplify_memo(df, tol_m = 500), error = function(e) df)
      
      leafletProxy(ns("map")) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          data = df_s,
          fillColor = pal(vals),
          fillOpacity = 0.8,
          color = "#555",
          weight = 0.6,
          highlight = highlightOptions(weight = 2, color = "#000", fillOpacity = 0.9, bringToFront = TRUE),
          label = lab,
          labelOptions = labelOptions(textsize = "12px", direction = "auto")
        ) |>
        addLegend(position = "bottomright", pal = pal, values = vals, title = v, opacity = 0.9)
    })
    
    # RETORNAR dados filtrados e variáveis para outros módulos
    return(
      list(
        data = filtered,
        vars = vars_sel,
        data_plain = df_plain
      )
    )
  })
}