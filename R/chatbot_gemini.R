# =====================================================================
# CHATBOT - Integração com Google Gemini API (VERSÃO CORRIGIDA)
# =====================================================================

get_api_key <- function() {
  api_key <- Sys.getenv("GEMINI_API_KEY")
  if (nchar(api_key) == 0) {
    stop("GEMINI_API_KEY não configurada", call. = FALSE)
  }
  return(api_key)
}

test_gemini_connection <- function() {
  tryCatch({
    api_key <- Sys.getenv("GEMINI_API_KEY")
    
    if (nchar(api_key) == 0) {
      return(list(connected = FALSE, message = "GEMINI_API_KEY não configurada"))
    }
    
    url <- sprintf(
      "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-exp:generateContent?key=%s",
      api_key
    )
    
    body_json <- jsonlite::toJSON(list(
      contents = list(list(parts = list(list(text = "OK"))))
    ), auto_unbox = TRUE)
    
    response <- httr::POST(url = url, body = body_json, httr::content_type_json())
    
    if (httr::status_code(response) == 200) {
      return(list(connected = TRUE, message = "API do Gemini conectada com sucesso"))
    } else {
      return(list(connected = FALSE, message = paste("Erro HTTP", httr::status_code(response))))
    }
    
  }, error = function(e) {
    return(list(connected = FALSE, message = paste("Erro:", e$message)))
  })
}

send_to_gemini <- function(messages, max_tokens = 2000) {
  tryCatch({
    if (is.null(messages) || length(messages) == 0) {
      return(list(success = FALSE, error = "Nenhuma mensagem"))
    }
    
    api_key <- get_api_key()
    
    url <- sprintf(
      "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-lite:generateContent?key=%s",
      api_key
    )
    
    # Converter mensagens para formato Gemini
    gemini_contents <- list()
    for (msg in messages) {
      role <- if (msg$role == "assistant") "model" else "user"
      gemini_contents[[length(gemini_contents) + 1]] <- list(
        role = role,
        parts = list(list(text = as.character(msg$content)))
      )
    }
    
    body_list <- list(
      contents = gemini_contents,
      generationConfig = list(
        temperature = 0.7,
        maxOutputTokens = max_tokens,
        topP = 0.95,
        topK = 40
      )
    )
    
    body_json <- jsonlite::toJSON(body_list, auto_unbox = TRUE)
    
    # Requisição - MESMA FORMA QUE FUNCIONOU NO TESTE
    response <- httr::POST(url = url, body = body_json, httr::content_type_json())
    
    status <- httr::status_code(response)
    
    if (status != 200) {
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      return(list(success = FALSE, error = paste0("Erro API (", status, "): ", substr(error_text, 1, 200))))
    }
    
    # Parsear resposta - EXATA LÓGICA QUE FUNCIONOU
    content_text <- httr::content(response, "text", encoding = "UTF-8")
    result <- jsonlite::fromJSON(content_text, simplifyVector = TRUE)
    
    # Extrair texto - EXATA LÓGICA QUE FUNCIONOU
    if (!is.null(result$candidates) && length(result$candidates) > 0) {
      
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
        return(list(success = TRUE, content = text))
      } else {
        return(list(success = FALSE, error = "Resposta sem texto"))
      }
      
    } else {
      return(list(success = FALSE, error = "Resposta sem conteúdo"))
    }
    
  }, error = function(e) {
    return(list(success = FALSE, error = paste("Erro:", e$message)))
  })
}

# Cache
message_cache <- new.env(parent = emptyenv())

init_message_cache <- function(session_id) {
  message_cache[[session_id]] <- list(
    messages = list(),
    last_context = NULL,
    created_at = Sys.time()
  )
}

get_cached_context <- function(session_id) {
  if (!exists(session_id, envir = message_cache)) {
    init_message_cache(session_id)
  }
  return(message_cache[[session_id]]$last_context)
}

update_cache_context <- function(session_id, context) {
  if (!exists(session_id, envir = message_cache)) {
    init_message_cache(session_id)
  }
  message_cache[[session_id]]$last_context <- context
}

clear_message_cache <- function(session_id) {
  if (exists(session_id, envir = message_cache)) {
    rm(list = session_id, envir = message_cache)
  }
}

send_message_with_cache <- function(user_message, context = NULL, conversation_history = NULL, use_cache = TRUE) {
  tryCatch({
    if (is.null(user_message) || nchar(user_message) == 0) {
      return(list(success = FALSE, error = "Mensagem vazia"))
    }
    
    messages <- build_full_prompt(
      user_message = user_message,
      context = context,
      conversation_history = conversation_history
    )
    
    response <- send_to_gemini(messages)
    return(response)
    
  }, error = function(e) {
    return(list(success = FALSE, error = paste("Erro:", e$message)))
  })
}

cleanup_old_caches <- function(max_age_hours = 24) {
  tryCatch({
    current_time <- Sys.time()
    for (session_id in ls(message_cache)) {
      cache_data <- message_cache[[session_id]]
      if (!is.null(cache_data$created_at)) {
        age <- difftime(current_time, cache_data$created_at, units = "hours")
        if (as.numeric(age) > max_age_hours) {
          rm(list = session_id, envir = message_cache)
        }
      }
    }
  }, error = function(e) {
    invisible(NULL)
  })
}

