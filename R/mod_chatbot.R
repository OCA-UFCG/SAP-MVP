# =====================================================================
# MÓDULO: CHATBOT ASSISTENTE ELECTRE (VERSÃO CORRIGIDA)
# =====================================================================

# Carregar dependências do chatbot
source("R/chatbot_utils.R")
source("R/chatbot_context.R")
source("R/chatbot_prompts.R")
source("R/chatbot_gemini.R")


# Carregar shinyjs se disponível (usado para alguns efeitos, mas opcional)
has_shinyjs <- requireNamespace("shinyjs", quietly = TRUE)

if (!has_shinyjs) {
  message("shinyjs não instalado - usando fallback JavaScript")
}

# Wrapper para runjs que funciona COM ou SEM shinyjs
safe_runjs <- function(code, session = getDefaultReactiveDomain()) {
  if (is.null(session)) return(invisible(NULL))
  
  tryCatch({
    if (has_shinyjs) {
      do.call(shinyjs::runjs, list(code))
    } else {
      session$sendCustomMessage(type = "runjs", message = code)
    }
  }, error = function(e) {
    invisible(NULL)
  })
}

# ---- UI: Ícone Flutuante ---------------------------------------------

mod_chatbot_icon_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Adicionar shinyjs se disponível
    if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::useShinyjs() else NULL,
    
    # CSS inline FORÇADO para garantir posicionamento correto
    tags$style(HTML(sprintf("
      #%s {
        position: fixed !important;
        bottom: 30px !important;
        right: 30px !important;
        left: auto !important;
        width: 60px !important;
        height: 60px !important;
        background: linear-gradient(135deg, #c0a38b 0%%, #a08970 100%%) !important;
        border-radius: 50%% !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        color: white !important;
        cursor: pointer !important;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2) !important;
        z-index: 9998 !important;
        text-decoration: none !important;
      }
      #%s:hover {
        transform: scale(1.1);
        box-shadow: 0 6px 20px rgba(192, 163, 139, 0.4);
      }
      #%s .chatbot-badge {
        position: absolute;
        top: -5px;
        right: -5px;
        background: #52854c;
        color: white;
        font-size: 10px;
        font-weight: bold;
        padding: 2px 6px;
        border-radius: 10px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
      }
    ", ns("chatbot_icon_link"), ns("chatbot_icon_link"), ns("chatbot_icon_link")))),
    
    # Ícone flutuante
    tags$a(
      id = ns("chatbot_icon_link"),
      href = "javascript:void(0);",
      title = "Assistente SAP - Branca",
      onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'}); return false;", 
                        ns("open_chat")),
      tags$img(
        src = "branca.png",
        style = "width: 60px; height: 60px; border-radius: 50%;",
        alt = "Branca - Assistente SAP"
      ),
      tags$span(class = "chatbot-badge", "AI")
    )
  )
}

# ---- UI: Modal do Chat -----------------------------------------------

mod_chatbot_modal_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # CSS inline para o modal
    tags$style(HTML(sprintf("
      #%s {
        display: none;
        position: fixed !important;
        bottom: 30px !important;
        right: 30px !important;
        left: auto !important;
        width: 400px !important;
        height: 600px !important;
        background: white !important;
        border-radius: 16px !important;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.15) !important;
        z-index: 9999 !important;
        overflow: hidden !important;
        flex-direction: column !important;
      }
      @media (max-width: 768px) {
        #%s {
          width: calc(100vw - 20px) !important;
          height: calc(100vh - 100px) !important;
          bottom: 10px !important;
          right: 10px !important;
          left: 10px !important;
          margin: 0 auto !important;
        }
      }
    ", ns("chatbot_modal"), ns("chatbot_modal")))),
    
    tags$div(
      id = ns("chatbot_modal"),
      class = "chatbot-modal",
      
      # Header
      tags$div(
        class = "chatbot-header",
        style = "background: linear-gradient(135deg, #c0a38b 0%, #a08970 100%); color: white; padding: 16px; border-radius: 16px 16px 0 0; flex-shrink: 0;",
        tags$div(
          class = "d-flex justify-content-between align-items-center",
          tags$div(
            class = "d-flex align-items-center",
            tags$img(
              src = "branca.png",
              style = "width: 40px; height: 40px; margin-right: 10px; border-radius: 50%;",
              alt = "Branca"
            ),
            tags$div(
              tags$strong("Assistente SAP"),
              tags$br(),
              tags$small(
                style = "font-size: 0.75em; opacity: 0.9;",
                "com Branca"
              )
            )
          ),
          tags$div(
            actionButton(
              ns("btn_minimize"),
              icon("minus"),
              class = "btn-sm btn-link text-white p-1",
              style = "background: none; border: none;",
              title = "Minimizar"
            ),
            actionButton(
              ns("btn_close"),
              icon("xmark"),
              class = "btn-sm btn-link text-white p-1",
              style = "background: none; border: none;",
              title = "Fechar"
            )
          )
        ),
        # Indicador de contexto
        tags$div(
          class = "mt-1",
          style = "font-size: 0.85em; opacity: 0.9;",
          tags$small(
            icon("map-pin", class = "me-1"),
            textOutput(ns("context_indicator"), inline = TRUE)
          )
        )
      ),
      
      # Body - Área de mensagens
      tags$div(
        id = ns("chat_messages"),
        class = "chatbot-messages",
        style = "flex: 1; overflow-y: auto; padding: 16px; background: #f8f9fa; scroll-behavior: smooth;"
      ),
      
      # Sugestões rápidas (quando chat vazio)
      tags$div(
        id = ns("quick_suggestions"),
        class = "chatbot-suggestions",
        style = "padding: 16px; background: #f8f9fa; border-top: 1px solid #dee2e6;",
        uiOutput(ns("ui_suggestions"))
      ),
      
      # Footer - Input de mensagem
      tags$div(
        class = "chatbot-footer",
        style = "padding: 16px; border-top: 1px solid #dee2e6; background: white; flex-shrink: 0;",
        tags$div(
          class = "d-flex gap-2",
          textAreaInput(
            ns("user_input"),
            label = NULL,
            placeholder = "Digite sua pergunta...",
            rows = 2,
            width = "100%",
            resize = "none"
          ),
          div(
            class = "d-flex flex-column gap-1",
            actionButton(
              ns("btn_send"),
              icon("paper-plane"),
              class = "btn-primary",
              style = "background: #c0a38b; border-color: #c0a38b; border-radius: 8px;",
              title = "Enviar"
            ),
            actionButton(
              ns("btn_clear"),
              icon("trash"),
              class = "btn-sm btn-outline-secondary",
              style = "border-radius: 8px;",
              title = "Limpar conversa"
            )
          )
        ),
        # Contador de mensagens
        tags$div(
          class = "mt-1 text-center",
          tags$small(
            class = "text-muted",
            style = "font-size: 0.75em;",
            textOutput(ns("message_counter"), inline = TRUE)
          )
        )
      )
    )
  )
}

# ---- SERVER ----------------------------------------------------------

mod_chatbot_server <- function(id, current_context = NULL, preproc_data = NULL, analise_data = NULL, 
                               qualif_data = NULL, session_parent = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ID da sessão para cache
    session_id <- paste0("session_", session$token)
    
    # Estado do chat
    chat_visible <- reactiveVal(FALSE)
    first_open <- reactiveVal(TRUE)  # Controla primeira abertura
    current_context <- reactiveVal("preproc")
    
    # Inicializar cache
    init_chatbot_cache(session_id)
    
    # ====================================================================
    # CONTROLE DE VISIBILIDADE COM JQUERY
    # ====================================================================
    
    # Abrir chat ao clicar no ícone
    observeEvent(input$open_chat, {
      chat_visible(TRUE)
      safe_runjs(sprintf("showChatbotModal('%s');", ns("chatbot_modal")))
      safe_runjs(sprintf("$('#%s').hide();", ns("chatbot_icon_link")))
      
      # Mostrar mensagem de boas-vindas na primeira abertura
      if (first_open()) {
        first_open(FALSE)
        
        welcome_message <- paste0(
          "Olá! Sou a **Branca**, sua assistente para análise de secas e ",
          "desertificação do SAP! 🐦\n\n",
          "Estou aqui para te ajudar com:\n",
          "• Explicações sobre o método ELECTRE\n",
          "• Dúvidas sobre configuração de parâmetros\n",
          "• Interpretação de resultados\n",
          "• Sugestões de boas práticas\n\n",
          "Como posso te ajudar hoje?"
        )
        
        insertUI(
          selector = paste0("#", ns("chat_messages")),
          where = "beforeEnd",
          ui = format_bot_message(welcome_message)
        )
        
        # Scroll
        safe_runjs(sprintf("$('#%s').scrollTop($('#%s')[0].scrollHeight);", 
                           ns("chat_messages"), ns("chat_messages")))
      }
    })
    
    # Fechar chat
    observeEvent(input$btn_close, {
      chat_visible(FALSE)
      safe_runjs(sprintf("hideChatbotModal('%s');", ns("chatbot_modal")))
      safe_runjs(sprintf("$('#%s').fadeIn(200);", ns("chatbot_icon_link")))
    })
    
    # Minimizar chat
    observeEvent(input$btn_minimize, {
      chat_visible(FALSE)
      safe_runjs(sprintf("hideChatbotModal('%s');", ns("chatbot_modal")))
      safe_runjs(sprintf("$('#%s').fadeIn(200);", ns("chatbot_icon_link")))
    })
    
    # ====================================================================
    # CAPTURA DE CONTEXTO AUTOMÁTICA
    # ====================================================================
    
    observeEvent(input$current_tab_js, {
      req(input$current_tab_js)
      current_context(input$current_tab_js)
    })
    
    # Indicador de contexto
    output$context_indicator <- renderText({
      ctx <- current_context()
      contexto_nome <- switch(
        ctx,
        "preproc" = "Pré-processamento",
        "parametros" = "Parâmetros ELECTRE",
        "resultados" = "Resultados",
        "qualificacao" = "Qualificação Territorial",
        "relatorios" = "Relatórios",
        "Sistema Principal"
      )
      paste("Contexto:", contexto_nome)
    })
    
    # ====================================================================
    # SUGESTÕES RÁPIDAS
    # ====================================================================
    
    output$ui_suggestions <- renderUI({
      history <- get_history(session_id)
      if (length(history) > 0) return(NULL)
      
      ctx <- current_context()
      suggestions <- get_quick_prompts(ctx)
      
      tags$div(
        tags$p(class = "text-muted small mb-2", 
               icon("lightbulb"), " Perguntas sugeridas:"),
        lapply(suggestions, function(sug) {
          actionButton(
            ns(paste0("sug_", gsub("[^a-z0-9]", "", tolower(sug)))),
            sug,
            class = "btn-sm btn-outline-secondary mb-2 me-2",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
              ns("suggestion_clicked"),
              sug
            )
          )
        })
      )
    })
    
    # Ao clicar em sugestão
    observeEvent(input$suggestion_clicked, {
      req(input$suggestion_clicked)
      updateTextAreaInput(session, "user_input", value = input$suggestion_clicked)
      # Simular clique no botão enviar
      safe_runjs(sprintf("$('#%s').click();", ns("btn_send")))
    })
    
    # ====================================================================
    # ENVIAR MENSAGEM
    # ====================================================================
    
    observeEvent(input$btn_send, {
      req(input$user_input)
      
      # Validar mensagem
      validation <- validate_message(input$user_input)
      if (!validation$valid) {
        showNotification(validation$error, type = "error")
        return()
      }
      
      # Verificar limite de mensagens
      can_send <- can_send_message(session_id)
      if (!can_send$allowed) {
        showNotification(
          "Limite de mensagens atingido. Aguarde 1 hora para continuar.",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      user_message <- input$user_input
      
      # Adicionar mensagem do usuário ao histórico
      add_to_history(session_id, "user", user_message)
      
      # Mostrar mensagem do usuário
      insertUI(
        selector = paste0("#", ns("chat_messages")),
        where = "beforeEnd",
        ui = format_user_message(user_message)
      )
      
      # Esconder sugestões
      safe_runjs(sprintf("$('#%s').hide();", ns("quick_suggestions")))
      
      # Limpar input
      updateTextAreaInput(session, "user_input", value = "")
      
      # Scroll para final
      safe_runjs(sprintf("$('#%s').scrollTop($('#%s')[0].scrollHeight);", 
                         ns("chat_messages"), ns("chat_messages")))
      
      # Mostrar indicador de "pensando..."
      thinking_id <- paste0("thinking_", round(runif(1, 10000, 99999)))
      insertUI(
        selector = paste0("#", ns("chat_messages")),
        where = "beforeEnd",
        ui = tags$div(
          id = thinking_id,
          class = "chat-message bot-message",
          tags$div(
            class = "message-content",
            style = "background: white; padding: 12px 16px; border-radius: 12px; margin-right: 20%; border: 1px solid #dee2e6;",
            icon("spinner", class = "fa-spin"), " Pensando..."
          )
        )
      )
      
      # Scroll para final
      safe_runjs(sprintf("$('#%s').scrollTop($('#%s')[0].scrollHeight);", 
                         ns("chat_messages"), ns("chat_messages")))
      
      # Capturar contexto atual
      ctx <- capture_context(
        current_tab = current_context(),
        input = input,
        preproc_data = preproc_data,
        analise_data = analise_data,
        qualif_data = qualif_data
      )
      
      # Sanitizar contexto
      ctx <- sanitize_context(ctx)
      
      # Obter histórico
      history <- get_history(session_id)
      
      # Enviar para Gemini
      response <- send_message_with_cache(
        user_message = user_message,
        context = ctx,
        conversation_history = history,
        use_cache = TRUE
      )
      
      # Remover indicador de "pensando..."
      removeUI(selector = paste0("#", thinking_id))
      
      # Processar resposta
      if (response$success) {
        # Adicionar ao histórico
        add_to_history(session_id, "assistant", response$content)
        
        # Mostrar mensagem do bot
        insertUI(
          selector = paste0("#", ns("chat_messages")),
          where = "beforeEnd",
          ui = format_bot_message(response$content)
        )
        
        # Incrementar contador
        increment_message_count(session_id)
        
      } else {
        # Mostrar erro
        insertUI(
          selector = paste0("#", ns("chat_messages")),
          where = "beforeEnd",
          ui = format_error_message(response$error)
        )
        
        showNotification(
          paste("Erro ao processar mensagem:", response$error),
          type = "error",
          duration = 5
        )
      }
      
      # Scroll para final novamente
      safe_runjs(sprintf("$('#%s').scrollTop($('#%s')[0].scrollHeight);", 
                         ns("chat_messages"), ns("chat_messages")))
    })
    
    # Enter para enviar (Shift+Enter para nova linha)
    observe({
      safe_runjs(sprintf("
        $('#%s').off('keydown').on('keydown', function(e) {
          if (e.key === 'Enter' && !e.shiftKey) {
            e.preventDefault();
            $('#%s').click();
          }
        });
      ", ns("user_input"), ns("btn_send")))
    })
    
    # ====================================================================
    # LIMPAR CONVERSA
    # ====================================================================
    
    observeEvent(input$btn_clear, {
      showModal(modalDialog(
        title = "Limpar Conversa",
        "Tem certeza que deseja limpar todo o histórico de conversa?",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirm_clear"), "Limpar", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_clear, {
      clear_history(session_id)
      removeUI(selector = paste0("#", ns("chat_messages"), " > *"), multiple = TRUE)
      safe_runjs(sprintf("$('#%s').show();", ns("quick_suggestions")))
      removeModal()
      showNotification("Conversa limpa", type = "message", duration = 2)
    })
    
    # ====================================================================
    # CONTADOR DE MENSAGENS
    # ====================================================================
    
    output$message_counter <- renderText({
      input$btn_send
      can_send <- can_send_message(session_id)
      paste(can_send$remaining, "mensagens restantes nesta hora")
    })
    
    # ====================================================================
    # TESTAR CONEXÃO COM GEMINI (ao iniciar)
    # ====================================================================
    
    # observe({
    #   test_result <- test_gemini_connection()
    #   if (!test_result$connected) {
    #     showNotification(
    #       paste("Chatbot: ", test_result$message),
    #       type = "warning",
    #       duration = 10
    #     )
    #   }
    # }) |> bindEvent(session$clientData, once = TRUE)
    
  })
}