# =====================================================================
# TESTE MINIMALISTA - Chatbot (CORRIGIDO)
# =====================================================================

library(shiny)
library(httr)
library(jsonlite)

# ---- Função Gemini ---------------------------------------------------

send_to_gemini_simple <- function(message) {
  api_key <- Sys.getenv("GEMINI_API_KEY")
  
  if (nchar(api_key) == 0) {
    return(list(success = FALSE, error = "API key não configurada"))
  }
  
  url <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-exp:generateContent?key=%s",
    api_key
  )
  
  body_json <- jsonlite::toJSON(list(
    contents = list(
      list(
        parts = list(
          list(text = message)
        )
      )
    )
  ), auto_unbox = TRUE)
  
  tryCatch({
    response <- httr::POST(url = url, body = body_json, httr::content_type_json())
    
    status <- httr::status_code(response)
    cat(sprintf("Status API: %d\n", status))
    
    if (status != 200) {
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      cat("Erro API:", error_text, "\n")
      return(list(success = FALSE, error = paste("Erro HTTP", status)))
    }
    
    content_text <- httr::content(response, "text", encoding = "UTF-8")
    
    # Mostrar resposta bruta no console
    cat("\n=== RESPOSTA BRUTA JSON ===\n")
    cat(substr(content_text, 1, 500), "\n")
    cat("=== FIM ===\n\n")
    
    result <- jsonlite::fromJSON(content_text, simplifyVector = TRUE)
    
    # Debug: mostrar estrutura
    cat("Estrutura da resposta:\n")
    cat("Tem candidates?", !is.null(result$candidates), "\n")
    
    if (!is.null(result$candidates) && length(result$candidates) > 0) {
      cat("Número de candidates:", length(result$candidates), "\n")
      
      # Tentar diferentes formas de acessar o texto
      text <- NULL
      
      # Forma 1: candidates[[1]]$content$parts[[1]]$text
      if (!is.null(result$candidates[[1]]$content$parts)) {
        text <- result$candidates[[1]]$content$parts[[1]]$text
      }
      
      # Forma 2: candidates[1,]$content$parts[[1]]$text (se for dataframe)
      if (is.null(text) && is.data.frame(result$candidates)) {
        text <- result$candidates[1,]$content$parts[[1]]$text
      }
      
      if (!is.null(text) && nchar(text) > 0) {
        cat("Texto extraído (", nchar(text), " chars):", substr(text, 1, 50), "...\n")
        return(list(success = TRUE, content = text))
      } else {
        cat("Texto vazio ou NULL\n")
        return(list(success = FALSE, error = "Resposta sem texto"))
      }
    } else {
      cat("Sem candidates na resposta\n")
      return(list(success = FALSE, error = "Resposta sem conteúdo"))
    }
    
  }, error = function(e) {
    cat("ERRO:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# ---- UI --------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .chat-container {
        max-width: 600px;
        margin: 20px auto;
        border: 1px solid #ddd;
        border-radius: 10px;
        overflow: hidden;
      }
      .chat-header {
        background: linear-gradient(135deg, #c0a38b 0%, #a08970 100%);
        color: white;
        padding: 15px;
        font-weight: bold;
      }
      .chat-messages {
        height: 400px;
        overflow-y: auto;
        padding: 20px;
        background: #f8f9fa;
      }
      .message {
        margin-bottom: 15px;
        animation: fadeIn 0.3s;
      }
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      .user-message {
        text-align: right;
      }
      .user-message .bubble {
        display: inline-block;
        background: #007bff;
        color: white;
        padding: 10px 15px;
        border-radius: 15px 15px 0 15px;
        max-width: 70%;
        word-wrap: break-word;
      }
      .bot-message {
        text-align: left;
      }
      .bot-message .bubble {
        display: inline-block;
        background: white;
        color: #333;
        padding: 10px 15px;
        border-radius: 15px 15px 15px 0;
        border: 1px solid #ddd;
        max-width: 70%;
        word-wrap: break-word;
        white-space: pre-wrap;
      }
      .error-message .bubble {
        background: #fff3cd;
        color: #856404;
        border-color: #ffc107;
      }
      .chat-input {
        padding: 15px;
        background: white;
        border-top: 1px solid #ddd;
      }
      .thinking {
        text-align: center;
        color: #888;
        font-style: italic;
        padding: 10px;
      }
    "))
  ),
  
  div(
    class = "chat-container",
    
    div(
      class = "chat-header",
      icon("robot"), " Chatbot Gemini - Teste Minimalista"
    ),
    
    div(
      id = "chat_messages",
      class = "chat-messages",
      
      div(
        class = "message bot-message",
        div(
          class = "bubble",
          "Olá! Sou um chatbot de teste. Digite algo e verei se consigo responder."
        )
      )
    ),
    
    div(
      class = "chat-input",
      fluidRow(
        column(
          10,
          textInput(
            "user_input",
            label = NULL,
            placeholder = "Digite sua mensagem...",
            width = "100%"
          )
        ),
        column(
          2,
          actionButton(
            "btn_send",
            icon("paper-plane"),
            class = "btn-primary w-100"
          )
        )
      )
    )
  ),
  
  # JavaScript para Enter
  tags$script(HTML("
    $(document).on('keypress', '#user_input', function(e) {
      if (e.which == 13) {
        $('#btn_send').click();
      }
    });
  "))
)

# ---- Server ----------------------------------------------------------

server <- function(input, output, session) {
  
  observeEvent(input$btn_send, {
    
    req(input$user_input)
    user_msg <- trimws(input$user_input)
    
    if (nchar(user_msg) == 0) return()
    
    cat("\n=== Nova mensagem ===\n")
    cat("Usuário:", user_msg, "\n")
    
    # Mostrar mensagem do usuário
    insertUI(
      selector = "#chat_messages",
      where = "beforeEnd",
      ui = div(
        class = "message user-message",
        div(class = "bubble", user_msg)
      )
    )
    
    # Limpar input
    updateTextInput(session, "user_input", value = "")
    
    # Scroll para final
    session$sendCustomMessage("scroll", "chat_messages")
    
    # Mostrar "pensando..."
    thinking_id <- paste0("thinking_", round(runif(1, 10000, 99999)))
    
    insertUI(
      selector = "#chat_messages",
      where = "beforeEnd",
      ui = div(
        id = thinking_id,
        class = "thinking",
        icon("spinner", class = "fa-spin"), " Pensando..."
      )
    )
    
    # Scroll
    session$sendCustomMessage("scroll", "chat_messages")
    
    # Enviar para Gemini (AGUARDAR resposta)
    cat("Enviando para Gemini...\n")
    response <- send_to_gemini_simple(user_msg)
    cat("Resposta recebida. Success:", response$success, "\n")
    
    if (response$success) {
      cat("Conteúdo:", substr(response$content, 1, 100), "\n")
    }
    
    # Remover "pensando..."
    removeUI(selector = paste0("#", thinking_id))
    
    # Mostrar resposta
    if (response$success) {
      cat("Mostrando resposta do bot\n")
      insertUI(
        selector = "#chat_messages",
        where = "beforeEnd",
        ui = div(
          class = "message bot-message",
          div(class = "bubble", response$content)
        )
      )
    } else {
      cat("Mostrando erro:", response$error, "\n")
      insertUI(
        selector = "#chat_messages",
        where = "beforeEnd",
        ui = div(
          class = "message error-message",
          div(class = "bubble", paste("❌ Erro:", response$error))
        )
      )
    }
    
    # Scroll final
    session$sendCustomMessage("scroll", "chat_messages")
    
    cat("=== Fim ===\n\n")
  })
  
}

# ---- Custom Message Handler ------------------------------------------

shinyApp(
  ui = tagList(
    ui,
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scroll', function(id) {
        var elem = document.getElementById(id);
        if (elem) {
          elem.scrollTop = elem.scrollHeight;
        }
      });
    "))
  ),
  server = server
)