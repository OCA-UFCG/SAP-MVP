# =====================================================================
# LÓGICA CORE - ELECTRE TRI-B
# =====================================================================

#' Calcular matriz de perfis B
#' 
#' @param criterios Vetor com nomes dos critérios
#' @param n_classes Número de classes
#' @param b_mode "quantis" ou "manual"
#' @param data_plain Data frame com dados
#' @param ranges_real Lista com ranges dos critérios
#' @param perfis_manuais reactiveValues com perfis manuais
#' @param input Shiny input para pegar sentidos
#' @param to_unit Função de conversão para [0,1]
#' @return Matriz de perfis (n_perfis x n_criterios)
#' @export
calcular_perfis_b <- function(criterios, n_classes, b_mode, data_plain, ranges_real, 
                               perfis_manuais, input, to_unit) {
  
  n_perfis <- n_classes - 1
  
  Bm <- matrix(NA_real_, nrow = n_perfis, ncol = length(criterios), 
               dimnames = list(paste0("b", 1:n_perfis), criterios))
  
  if (b_mode == "quantis") {
    # Quantis automáticos - calcula por critério
    probs <- seq(0, 1, length.out = n_classes + 1)[2:n_classes]
    
    for (j in seq_along(criterios)) {
      cn <- criterios[j]
      sense <- input[[paste0("sense_", cn)]] %||% "benefit"
      ucol <- to_unit(data_plain[[cn]], cn, sense, ranges_real)
      Bm[, j] <- quantile(ucol, probs = probs, type = 7, na.rm = TRUE)
    }
  } else {
    # Manual - usar perfis_manuais
    for (j in seq_along(criterios)) {
      cn <- criterios[j]
      sense <- input[[paste0("sense_", cn)]] %||% "benefit"
      
      vals_real <- perfis_manuais[[cn]]
      
      if (is.null(vals_real) || length(vals_real) != n_perfis) {
        r <- ranges_real[[cn]]
        if (sense == "benefit") {
          vals_real <- seq(r[1], r[2], length.out = n_perfis + 2)[2:(n_perfis + 1)]
        } else {
          vals_real <- seq(r[2], r[1], length.out = n_perfis + 2)[2:(n_perfis + 1)]
        }
      }
      
      u <- to_unit(vals_real, cn, sense, ranges_real)
      Bm[, j] <- sort(clamp01(u))
    }
  }
  
  Bm
}

#' Preparar dados para execução do ELECTRE
#' 
#' @param data_plain Data frame com dados
#' @param criterios Vetor com critérios
#' @param ranges_real Lista com ranges
#' @param input Shiny input
#' @param to_unit Função de conversão
#' @return Lista com dataset normalizado e df_cleaned
#' @export
preparar_dados_electre <- function(data_plain, criterios, ranges_real, input, to_unit) {
  
  # 1. Normalizar dados para [0,1]
  X01 <- do.call(cbind, lapply(criterios, function(cn) {
    sense <- input[[paste0("sense_", cn)]] %||% "benefit"
    to_unit(data_plain[[cn]], cn, sense, ranges_real)
  }))
  colnames(X01) <- criterios
  
  # 2. Remover NAs
  keep_idx <- which(complete.cases(as.data.frame(X01)))
  validate(need(length(keep_idx) > 0, "Sem linhas completas após remover NAs."))
  
  dataset <- as.matrix(X01[keep_idx, , drop = FALSE])
  df_cleaned <- data_plain[keep_idx, , drop = FALSE]
  
  list(
    dataset = dataset,
    df_cleaned = df_cleaned
  )
}

#' Obter limiares (globais ou por critério)
#' 
#' @param input Shiny input
#' @param criterios Vetor com critérios
#' @param limiares_avancado Booleano
#' @return Lista com q, p, v
#' @export
obter_limiares <- function(input, criterios, limiares_avancado = FALSE) {
  
  if (isTRUE(limiares_avancado)) {
    Q <- sapply(criterios, function(cn) input[[paste0("q_", cn)]] %||% 0.02)
    P <- sapply(criterios, function(cn) input[[paste0("p_", cn)]] %||% 0.10)
    V <- sapply(criterios, function(cn) input[[paste0("v_", cn)]] %||% 0.50)
  } else {
    n_crit <- length(criterios)
    Q <- rep(input$q_val %||% 0.02, n_crit)
    P <- rep(input$p_val %||% 0.10, n_crit)
    V <- rep(input$v_val %||% 0.50, n_crit)
  }
  
  list(q = Q, p = P, v = V)
}

#' Executar análise ELECTRE Tri-B completa
#' 
#' @param data_plain Reactive com dados
#' @param criterios Reactive com critérios
#' @param W_norm Reactive com pesos normalizados
#' @param ranges_real Reactive com ranges
#' @param B_current Reactive com matriz de perfis
#' @param input Shiny input
#' @param label_map Reactive com labels das classes
#' @param electre_tri_b_py Função ELECTRE
#' @param to_unit Função de conversão
#' @return Lista com results e params
#' @export
executar_electre <- function(data_plain, criterios, W_norm, ranges_real, B_current, 
                              input, label_map, electre_tri_b_py, to_unit) {
  
  crits <- criterios()
  req(length(crits) >= 2)
  
  showNotification("Executando ELECTRE Tri-B...", type = "default", duration = NULL, id = "electre_run")
  
  tryCatch({
    df <- data_plain()
    rng <- ranges_real()
    
    # Preparar dados
    dados_prep <- preparar_dados_electre(df, crits, rng, input, to_unit)
    dataset <- dados_prep$dataset
    df_cleaned <- dados_prep$df_cleaned
    
    # Parâmetros
    W <- as.numeric(W_norm()[crits])
    
    limiares <- obter_limiares(input, crits, input$limiares_avancado)
    Q <- limiares$q
    P <- limiares$p
    V <- limiares$v
    
    Bm <- B_current()
    Bm <- Bm[, crits, drop = FALSE]
    
    n_classes <- input$n_classes %||% 5
    
    # Executar ELECTRE
    classification <- electre_tri_b_py(
      dataset,
      W = W,
      Q = Q,
      P = P,
      V = V,
      B = Bm,
      cut_level = input$lambda_cut,
      rule = input$rule
    )
    
    classification <- as.integer(classification) + 1L
    
    # Labels
    labels_atuais <- label_map()
    results_df <- df_cleaned |>
      mutate(
        class_electre = as.integer(classification),
        class_label = labels_atuais[as.character(class_electre)]
      )
    
    removeNotification("electre_run")
    showNotification("Análise concluída com sucesso!", type = "message", duration = 3)
    
    list(
      results = results_df,
      params = list(
        criterios = crits,
        pesos = W,
        perfis = Bm,
        limiares = list(q = Q, p = P, v = V),
        lambda = input$lambda_cut,
        rule = input$rule,
        n_classes = n_classes
      )
    )
    
  }, error = function(e) {
    removeNotification("electre_run")
    showNotification(
      paste("Erro na execução:", e$message),
      type = "error",
      duration = 10
    )
    return(NULL)
  })
}

#' Gerar resumo dos parâmetros ELECTRE
#' 
#' @param criterios Vetor com critérios
#' @param W_norm Vetor com pesos normalizados
#' @param input Shiny input
#' @param B_current Matriz de perfis
#' @return String com resumo formatado
#' @export
gerar_resumo_parametros <- function(criterios, W_norm, input, B_current) {
  
  n_classes <- input$n_classes %||% 5
  n_perfis <- n_classes - 1
  
  W <- W_norm[criterios]
  
  # Limiares
  if (isTRUE(input$limiares_avancado)) {
    Q <- sapply(criterios, function(cn) input[[paste0("q_", cn)]] %||% 0.02)
    P <- sapply(criterios, function(cn) input[[paste0("p_", cn)]] %||% 0.10)
    V <- sapply(criterios, function(cn) input[[paste0("v_", cn)]] %||% 0.50)
  } else {
    Q <- rep(input$q_val %||% 0.02, length(criterios))
    P <- rep(input$p_val %||% 0.10, length(criterios))
    V <- rep(input$v_val %||% 0.50, length(criterios))
  }
  
  Bm <- tryCatch(B_current, error = function(e) NULL)
  
  txt <- c(
    "# ========================================",
    "# PARÂMETROS ELECTRE TRI-B",
    "# ========================================",
    "",
    sprintf("## Número de classes: %d", n_classes),
    sprintf("## Número de perfis: %d", n_perfis),
    sprintf("## Número de critérios: %d", length(criterios)),
    sprintf("## Lambda (corte): %.2f", input$lambda_cut %||% 0.70),
    sprintf("## Regra: %s", ifelse(input$rule == "pc", "Pessimista (pc)", "Otimista (oc)")),
    "",
    "# ----------------------------------------",
    "# PESOS (W) - soma = 1.0",
    "# ----------------------------------------",
    paste0("W = c(", paste(sprintf("%.3f", W), collapse = ", "), ")"),
    sprintf("names(W) = c(%s)", paste0('"', criterios, '"', collapse = ", ")),
    "",
    "# ----------------------------------------",
    "# LIMIARES (q, p, v) - por critério",
    "# ----------------------------------------",
    paste0("Q = c(", paste(sprintf("%.3f", Q), collapse = ", "), ")  # indiferença"),
    paste0("P = c(", paste(sprintf("%.3f", P), collapse = ", "), ")  # preferência"),
    paste0("V = c(", paste(sprintf("%.3f", V), collapse = ", "), ")  # veto"),
    "",
    "# ----------------------------------------",
    "# PERFIS (B) - matriz [perfis x critérios]",
    "# ----------------------------------------"
  )
  
  if (!is.null(Bm)) {
    txt <- c(txt, "# Valores normalizados [0,1]:")
    txt <- c(txt, paste0("# dim(B) = ", nrow(Bm), " perfis x ", ncol(Bm), " critérios"))
    txt <- c(txt, "B = matrix(c(")
    for (i in 1:nrow(Bm)) {
      linha <- sprintf("  %.4f", Bm[i, ])
      if (i < nrow(Bm)) {
        txt <- c(txt, paste0("  ", paste(linha, collapse = ", "), ",  # ", rownames(Bm)[i]))
      } else {
        txt <- c(txt, paste0("  ", paste(linha, collapse = ", "), "   # ", rownames(Bm)[i]))
      }
    }
    txt <- c(txt, sprintf("), nrow = %d, ncol = %d, byrow = TRUE)", nrow(Bm), ncol(Bm)))
    txt <- c(txt, sprintf("colnames(B) = c(%s)", paste0('"', criterios, '"', collapse = ", ")))
    txt <- c(txt, sprintf("rownames(B) = c(%s)", paste0('"', paste0("b", 1:n_perfis), '"', collapse = ", ")))
  } else {
    txt <- c(txt, "# [Aguardando configuração completa dos perfis]")
  }
  
  paste(txt, collapse = "\n")
}
