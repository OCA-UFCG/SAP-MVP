# =====================================================================
# CHATBOT - Funções Auxiliares
# =====================================================================

# ---- Cache de Mensagens -----------------------------------------------
.chatbot_cache <- new.env(parent = emptyenv())

# Inicializar cache para uma sessão
init_chatbot_cache <- function(session_id) {
  if (!exists(session_id, envir = .chatbot_cache)) {
    .chatbot_cache[[session_id]] <- list(
      messages = list(),
      count = 0,
      last_reset = Sys.time()
    )
  }
}

# Adicionar mensagem ao histórico
add_to_history <- function(session_id, role, content) {
  init_chatbot_cache(session_id)
  cache <- .chatbot_cache[[session_id]]
  
  cache$messages <- append(
    cache$messages,
    list(list(role = role, content = content, timestamp = Sys.time()))
  )
  
  # Manter apenas últimas 20 mensagens para não sobrecarregar
  if (length(cache$messages) > 20) {
    cache$messages <- tail(cache$messages, 20)
  }
  
  .chatbot_cache[[session_id]] <- cache
}

# Obter histórico de mensagens
get_history <- function(session_id) {
  init_chatbot_cache(session_id)
  .chatbot_cache[[session_id]]$messages
}

# Limpar histórico
clear_history <- function(session_id) {
  init_chatbot_cache(session_id)
  .chatbot_cache[[session_id]]$messages <- list()
}

# ---- Limitador de Taxa -----------------------------------------------

# Verificar se pode enviar mensagem (máx 30 por sessão)
can_send_message <- function(session_id) {
  init_chatbot_cache(session_id)
  cache <- .chatbot_cache[[session_id]]
  
  # Resetar contador a cada hora
  if (difftime(Sys.time(), cache$last_reset, units = "hours") > 1) {
    cache$count <- 0
    cache$last_reset <- Sys.time()
    .chatbot_cache[[session_id]] <- cache
  }
  
  if (cache$count >= 30) {
    return(list(allowed = FALSE, remaining = 0))
  }
  
  return(list(allowed = TRUE, remaining = 30 - cache$count))
}

# Incrementar contador de mensagens
increment_message_count <- function(session_id) {
  init_chatbot_cache(session_id)
  cache <- .chatbot_cache[[session_id]]
  cache$count <- cache$count + 1
  .chatbot_cache[[session_id]] <- cache
}

# ---- Formatação de Mensagens ------------------------------------------

# Formatar mensagem do usuário para display
format_user_message <- function(content) {
  tags$div(
    class = "chat-message user-message",
    tags$div(class = "message-header", 
             tags$strong(icon("user"), " Você"),
             tags$small(class = "text-muted ms-2", format(Sys.time(), "%H:%M"))),
    tags$div(class = "message-content", content)
  )
}

# Formatar mensagem do bot para display
format_bot_message <- function(content) {
  # Converter markdown básico para HTML
  content_html <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", content)
  content_html <- gsub("\\*(.+?)\\*", "<em>\\1</em>", content_html)
  content_html <- gsub("\n", "<br>", content_html)
  content_html <- gsub("```(.+?)```", "<code>\\1</code>", content_html)
  
  tags$div(
    class = "chat-message bot-message",
    tags$div(class = "message-header", 
             tags$strong(icon("robot"), " Assistente ELECTRE"),
             tags$small(class = "text-muted ms-2", format(Sys.time(), "%H:%M"))),
    tags$div(class = "message-content", HTML(content_html))
  )
}

# Formatar mensagem de erro
format_error_message <- function(error) {
  tags$div(
    class = "chat-message error-message",
    tags$div(class = "message-header", 
             tags$strong(icon("triangle-exclamation"), " Erro")),
    tags$div(class = "message-content", 
             "Desculpe, ocorreu um erro ao processar sua mensagem. Por favor, tente novamente.")
  )
}

# ---- Validação de Input -----------------------------------------------

# Validar mensagem do usuário
validate_message <- function(message) {
  if (is.null(message) || nchar(trimws(message)) == 0) {
    return(list(valid = FALSE, error = "Mensagem vazia"))
  }
  
  if (nchar(message) > 1000) {
    return(list(valid = FALSE, error = "Mensagem muito longa (máximo 1000 caracteres)"))
  }
  
  return(list(valid = TRUE))
}

# ---- Sugestões de Perguntas -------------------------------------------

# Obter sugestões baseadas no contexto atual
get_suggestions <- function(contexto_atual) {
  sugestoes <- list(
    "preproc" = c(
      "Como funcionam os filtros compostos?",
      "Quais variáveis estão disponíveis?",
      "Como criar uma nova coluna calculada?"
    ),
    "parametros" = c(
      "O que significa o limiar de indiferença (q)?",
      "Como escolher os pesos dos critérios?",
      "Qual a diferença entre regra pessimista e otimista?"
    ),
    "resultados" = c(
      "O que significa a classe C3?",
      "Como interpretar a distribuição das classes?",
      "Quais municípios estão em situação crítica?"
    ),
    "qualificacao" = c(
      "O que são as camadas espaciais?",
      "Como interpretar as interseções?",
      "Quais áreas têm mais vulnerabilidade?"
    ),
    "relatorios" = c(
      "Quais seções devo incluir no relatório?",
      "Como customizar as cores?",
      "Posso adicionar texto personalizado?"
    )
  )
  
  sugestoes[[contexto_atual]] %||% sugestoes[["parametros"]]
}

# ---- Sanitização ------------------------------------------------------

# Sanitizar contexto antes de enviar para API
sanitize_context <- function(context) {
  # Remover geometrias e dados muito grandes
  if (!is.null(context$data_sf)) {
    context$data_sf <- NULL
  }
  
  if (!is.null(context$data_plain)) {
    # Manter apenas estatísticas agregadas
    if (is.data.frame(context$data_plain) && nrow(context$data_plain) > 0) {
      context$data_summary <- list(
        n_rows = nrow(context$data_plain),
        n_cols = ncol(context$data_plain),
        col_names = names(context$data_plain)
      )
      context$data_plain <- NULL
    }
  }
  
  return(context)
}