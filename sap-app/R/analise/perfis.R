# =====================================================================
# EDITOR DE PERFIS INTERATIVO - ELECTRE TRI-B
# =====================================================================

#' Inicializar perfis com quantis padrao
#' 
#' @param perfis_manuais reactiveValues para armazenar perfis
#' @param input Shiny input
#' @param data_plain Reactive com dados plain
#' @param ranges_real Reactive com ranges dos criterios
#' @param criterios Reactive com lista de criterios
#' @param to_unit Funcao de conversao para [0,1]
#' @param to_real Funcao de conversao para valores reais
#' @export
inicializar_perfis <- function(perfis_manuais, input, data_plain, ranges_real, criterios, to_unit, to_real) {
  observe({
    req(input$criterios_sel)
    req(length(input$criterios_sel) > 0)
    
    crits <- tryCatch(criterios(), error = function(e) NULL)
    if (is.null(crits) || length(crits) == 0) return(NULL)
    
    n_classes <- input$n_classes
    if (is.null(n_classes) || n_classes < 3 || n_classes > 7) n_classes <- 5
    n_perfis <- n_classes - 1
    
    req(data_plain())
    df <- data_plain()
    
    req(ranges_real())
    rng <- ranges_real()
    
    for (cn in crits) {
      if (is.null(perfis_manuais[[cn]])) {
        sense <- input[[paste0("sense_", cn)]] %||% "benefit"
        x <- df[[cn]]
        
        probs <- seq(0, 1, length.out = n_classes + 1)[2:n_classes]
        u <- suppressWarnings(quantile(
          to_unit(x, cn, sense, rng), 
          probs = probs,
          na.rm = TRUE
        ))
        vals <- round(to_real(as.numeric(u), cn, sense, rng), 6)
        perfis_manuais[[cn]] <- vals
      }
    }
  })
}

#' Observar mudancas em n_classes e recalcular perfis
#' 
#' @export
observar_mudanca_classes <- function(input, perfis_manuais, criterios, data_plain, ranges_real, to_unit, to_real) {
  observeEvent(input$n_classes, {
    req(input$criterios_sel)
    req(length(input$criterios_sel) > 0)
    
    crits <- tryCatch(criterios(), error = function(e) NULL)
    if (is.null(crits) || length(crits) == 0) return(NULL)
    
    n_classes <- input$n_classes
    if (is.null(n_classes) || n_classes < 3 || n_classes > 7) n_classes <- 5
    n_perfis <- n_classes - 1
    
    req(data_plain())
    df <- data_plain()
    
    req(ranges_real())
    rng <- ranges_real()
    
    for (cn in crits) {
      sense <- input[[paste0("sense_", cn)]] %||% "benefit"
      x <- df[[cn]]
      
      probs <- seq(0, 1, length.out = n_classes + 1)[2:n_classes]
      u <- suppressWarnings(quantile(
        to_unit(x, cn, sense, rng), 
        probs = probs,
        na.rm = TRUE
      ))
      vals <- round(to_real(as.numeric(u), cn, sense, rng), 6)
      perfis_manuais[[cn]] <- vals
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
}

#' Criar modal do editor de perfis
#' 
#' @param session Shiny session
#' @param ns Namespace function
#' @param input Shiny input
#' @param criterios Reactive com criterios
#' @param perfis_manuais reactiveValues com perfis
#' @param data_plain Reactive com dados
#' @param ranges_real Reactive com ranges
#' @param to_unit Funcao de conversao
#' @param to_real Funcao de conversao
#' @export
#' Criar modal do editor de perfis
#' 
#' @param session Shiny session
#' @param ns Namespace function
#' @param input Shiny input
#' @param criterios Reactive com criterios
#' @param perfis_manuais reactiveValues com perfis
#' @export
criar_modal_perfis <- function(session, ns, input, criterios, perfis_manuais,
                               data_plain, ranges_real, to_unit, to_real) {
  observeEvent(input$btn_abrir_modal_perfis, {
    req(criterios())
    
    crits <- criterios()
    n_classes <- input$n_classes %||% 5
    n_perfis <- n_classes - 1
    
    # ===============================================================
    # NOVO: FORÇAR RECÁLCULO DOS PERFIS ANTES DE ABRIR O MODAL
    # ===============================================================
    req(data_plain())
    req(ranges_real())
    
    df <- data_plain()
    rng <- ranges_real()
    
    # Recalcular perfis usando quantis para TODOS os critérios
    for (cn in crits) {
      sense <- input[[paste0("sense_", cn)]] %||% "benefit"
      x <- df[[cn]]
      
      probs <- seq(0, 1, length.out = n_classes + 1)[2:n_classes]
      u <- suppressWarnings(quantile(
        to_unit(x, cn, sense, rng), 
        probs = probs,
        na.rm = TRUE
      ))
      vals <- round(to_real(as.numeric(u), cn, sense, rng), 6)
      perfis_manuais[[cn]] <- vals
    }
    # ===============================================================
    
    showModal(modalDialog(
      title = tags$div(
        icon("chart-bar"), 
        " Editor Interativo de Perfis",
        style = "font-size: 1.3em; font-weight: bold;"
      ),
      size = "xl",
      easyClose = FALSE,
      footer = tagList(
        actionButton(ns("btn_resetar_perfis"), "Resetar Todos", 
                     icon = icon("undo"), class = "btn-warning"),
        modalButton("Cancelar"),
        actionButton(ns("btn_salvar_perfis"), "Salvar Perfis", 
                     icon = icon("save"), class = "btn-success")
      ),
      
      div(
        style = "max-height: 70vh; overflow-y: auto;",
        
        div(
          class = "alert alert-info",
          icon("info-circle"),
          tags$strong(" Como usar:"),
          tags$ul(
            tags$li("Clique nos histogramas para ajustar os perfis"),
            tags$li("O perfil mais proximo ao clique sera movido"),
            tags$li("As linhas vermelhas pontilhadas mostram os perfis atuais"),
            tags$li("Use os campos numericos ou o botao 'Reset' para ajustes precisos")
          )
        ),
        
        lapply(seq(1, length(crits), by = 2), function(idx) {
          crits_linha <- crits[idx:min(idx + 1, length(crits))]
          
          fluidRow(
            lapply(crits_linha, function(cn) {
              sense <- input[[paste0("sense_", cn)]] %||% "benefit"
              orient <- if (sense == "benefit") "(Maior melhor ↑)" else "(Menor melhor ↓)"
              
              column(
                width = 6,
                wellPanel(
                  style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
                  
                  tags$div(
                    style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
                    tags$strong(paste(cn, orient), style = "font-size: 1.1em;"),
                    actionButton(
                      ns(paste0("reset_perfil_", cn)),
                      "Reset",
                      icon = icon("undo"),
                      class = "btn btn-sm btn-outline-secondary"
                    )
                  ),
                  
                  plotOutput(
                    outputId = ns(paste0("hist_perfil_", cn)),
                    height = "280px",
                    click = ns(paste0("click_perfil_", cn))
                  ),
                  
                  tags$div(
                    style = "margin-top: 10px;",
                    fluidRow(
                      lapply(1:n_perfis, function(i) {
                        column(
                          width = 12 / n_perfis,
                          numericInput(
                            ns(paste0("perfil_modal_", cn, "_", i)),
                            paste0("b", i, ":"),
                            value = if (!is.null(perfis_manuais[[cn]])) perfis_manuais[[cn]][i] else 0,
                            step = 0.01
                          )
                        )
                      })
                    )
                  )
                )
              )
            })
          )
        })
      )
    ))
    
    # Atualizar inputs com valores RECÉM-CALCULADOS
    for (cn in crits) {
      if (!is.null(perfis_manuais[[cn]])) {
        for (i in 1:n_perfis) {
          updateNumericInput(session, paste0("perfil_modal_", cn, "_", i), 
                             value = perfis_manuais[[cn]][i])
        }
      }
    }
  })
}

#' Renderizar histogramas dos perfis
#' 
#' @param output Shiny output
#' @param ns Namespace function
#' @param input Shiny input
#' @param criterios Reactive com criterios
#' @param data_plain Reactive com dados
#' @param ranges_real Reactive com ranges
#' @param perfis_manuais reactiveValues com perfis
#' @export
renderizar_histogramas_perfis <- function(output, ns, input, criterios, data_plain, ranges_real, perfis_manuais) {
  
  # Funcao auxiliar para pegar valores atuais dos perfis
  get_perfis_vals_real <- function(cn, n_perfis) {
    sapply(1:n_perfis, function(i) {
      input[[paste0("perfil_modal_", cn, "_", i)]] %||% 0
    })
  }
  
  observe({
    req(input$criterios_sel)
    req(length(input$criterios_sel) > 0)
    req(input$btn_abrir_modal_perfis)
    
    crits <- tryCatch(criterios(), error = function(e) NULL)
    if (is.null(crits) || length(crits) == 0) return(NULL)
    
    req(data_plain())
    df <- data_plain()
    
    req(ranges_real())
    rng <- ranges_real()
    
    n_classes <- input$n_classes %||% 5
    n_perfis <- n_classes - 1
    
    lapply(crits, function(cn) {
      output[[paste0("hist_perfil_", cn)]] <- renderPlot({
        x <- df[[cn]]
        x <- x[is.finite(x)]
        
        bvals <- get_perfis_vals_real(cn, n_perfis)
        r <- rng[[cn]]
        
        dfp <- data.frame(x = x)
        
        p <- ggplot(dfp, aes(x = x)) +
          geom_histogram(bins = 40, color = "white", fill = "grey70") +
          geom_vline(xintercept = bvals, linetype = "dashed", 
                     linewidth = 0.8, color = "#d73027") +
          coord_cartesian(xlim = r) +
          labs(x = sprintf("%s (valores reais)", cn), y = "Frequencia") +
          theme_minimal(base_size = 12) +
          theme(
            panel.grid.minor = element_blank(),
            plot.margin = margin(10, 10, 10, 10)
          )
        
        if (length(bvals) > 0 && all(is.finite(bvals))) {
          max_y <- max(ggplot_build(p)$data[[1]]$count, na.rm = TRUE)
          for (i in seq_along(bvals)) {
            p <- p + annotate("text", x = bvals[i], y = max_y * 0.95, 
                              label = paste0("b", i), color = "#d73027", 
                              fontface = "bold", size = 4)
          }
        }
        
        p
      })
    })
  })
}

#' Observar cliques nos histogramas
#' 
#' @param input Shiny input
#' @param session Shiny session
#' @param criterios Reactive com criterios
#' @param ranges_real Reactive com ranges
#' @param perfis_manuais reactiveValues com perfis
#' @param to_unit Funcao de conversao
#' @param to_real Funcao de conversao
#' @param clamp01 Funcao de clamping
#' @export
observar_cliques_perfis <- function(input, session, criterios, ranges_real, perfis_manuais, to_unit, to_real, clamp01) {
  
  # NOVO: Armazenar observers para destruir depois
  observers_lista <- reactiveValues(observers = list())
  
  get_perfis_vals_real <- function(cn, n_perfis) {
    sapply(1:n_perfis, function(i) {
      input[[paste0("perfil_modal_", cn, "_", i)]] %||% 0
    })
  }
  
  # NOVO: Limpar observers ao fechar modal
  observeEvent(input$btn_salvar_perfis, {
    lapply(observers_lista$observers, function(obs) obs$destroy())
    observers_lista$observers <- list()
  })
  
  # Criar observers ao abrir modal
  observeEvent(input$btn_abrir_modal_perfis, {
    req(input$criterios_sel)
    req(length(input$criterios_sel) > 0)
    
    # Limpar observers antigos
    lapply(observers_lista$observers, function(obs) obs$destroy())
    observers_lista$observers <- list()
    
    crits <- tryCatch(criterios(), error = function(e) NULL)
    if (is.null(crits) || length(crits) == 0) return(NULL)
    
    n_classes <- input$n_classes %||% 5
    n_perfis <- n_classes - 1
    
    req(ranges_real())
    rng <- ranges_real()
    
    # Criar observers
    novos_observers <- lapply(crits, function(cn) {
      observeEvent(input[[paste0("click_perfil_", cn)]], {
        info <- input[[paste0("click_perfil_", cn)]]
        
        x_clicado <- info$x
        if (!is.finite(x_clicado)) return(NULL)
        
        vals_real <- get_perfis_vals_real(cn, n_perfis)
        
        distancias <- ifelse(is.finite(vals_real), abs(vals_real - x_clicado), Inf)
        idx <- if (all(is.infinite(distancias))) 1L else which.min(distancias)
        
        vals_real[idx] <- x_clicado
        
        sense <- input[[paste0("sense_", cn)]] %||% "benefit"
        u <- to_unit(vals_real, cn, sense, rng)
        u <- sort(clamp01(u))
        vals_real_sorted <- to_real(u, cn, sense, rng)
        
        for (i in 1:n_perfis) {
          updateNumericInput(session, paste0("perfil_modal_", cn, "_", i), 
                             value = round(vals_real_sorted[i], 4))
        }
        
        perfis_manuais[[cn]] <- vals_real_sorted
        
        showNotification(
          paste0("Perfil b", idx, " de '", cn, "' atualizado"),
          type = "message",
          duration = 1.5
        )
        
      }, ignoreInit = TRUE)
    })
    
    # Armazenar observers
    observers_lista$observers <- novos_observers
  })
}

#' Resumo dos perfis definidos
#' 
#' @param output Shiny output
#' @param ns Namespace function
#' @param input Shiny input
#' @param criterios Reactive com criterios
#' @param perfis_manuais reactiveValues com perfis
#' @export
resumo_perfis_definidos <- function(output, ns, input, criterios, perfis_manuais) {
  output$resumo_perfis_definidos <- renderText({
    req(input$criterios_sel)
    req(length(input$criterios_sel) > 0)
    
    crits <- tryCatch(criterios(), error = function(e) NULL)
    if (is.null(crits) || length(crits) == 0) return("Aguardando selecao de criterios...")
    
    n_classes <- input$n_classes %||% 5
    n_perfis <- n_classes - 1
    
    resumo <- sapply(crits, function(cn) {
      perfis <- perfis_manuais[[cn]]
      if (!is.null(perfis)) {
        paste0(cn, ": [", paste(round(perfis, 4), collapse = ", "), "]")
      } else {
        paste0(cn, ": [nao definido]")
      }
    })
    
    paste(resumo, collapse = "\n")
  })
}

#' Handler para salvar perfis do modal
#' 
#' @param session Shiny session
#' @param ns Namespace function
#' @param input Shiny input
#' @param criterios Reactive com criterios
#' @param perfis_manuais reactiveValues com perfis
#' @param perfis_trigger reactiveVal para forcar atualizacao
#' @export
salvar_perfis_modal <- function(session, ns, input, criterios, perfis_manuais, perfis_trigger = NULL) {
  observeEvent(input$btn_salvar_perfis, {
    req(criterios())
    
    crits <- criterios()
    n_classes <- input$n_classes %||% 5
    n_perfis <- n_classes - 1
    
    # Salvar valores dos inputs numericos para perfis_manuais
    for (cn in crits) {
      vals <- sapply(1:n_perfis, function(i) {
        input[[paste0("perfil_modal_", cn, "_", i)]] %||% 0
      })
      perfis_manuais[[cn]] <- vals
    }
    
    # Mudar automaticamente para modo manual
    updateRadioButtons(session, "b_mode", selected = "manual")
    
    # CORREÇÃO: Incrementar trigger para forcar atualizacao de B_current
    if (!is.null(perfis_trigger)) {
      perfis_trigger(perfis_trigger() + 1)
    }
    
    removeModal()
    
    showNotification(
      "Perfis salvos com sucesso!",
      type = "message",
      duration = 2
    )
  })
}

#' Handler para resetar todos os perfis
#' 
#' @param session Shiny session  
#' @param ns Namespace function
#' @param input Shiny input
#' @param criterios Reactive com criterios
#' @param perfis_manuais reactiveValues com perfis
#' @param data_plain Reactive com dados
#' @param ranges_real Reactive com ranges
#' @param to_unit Funcao de conversao
#' @param to_real Funcao de conversao
#' @param perfis_trigger reactiveVal para forcar atualizacao
#' @export
resetar_perfis_modal <- function(session, ns, input, criterios, perfis_manuais, 
                                 data_plain, ranges_real, to_unit, to_real, perfis_trigger = NULL) {
  observeEvent(input$btn_resetar_perfis, {
    req(criterios())
    req(data_plain())
    req(ranges_real())
    
    crits <- criterios()
    df <- data_plain()
    rng <- ranges_real()
    
    n_classes <- input$n_classes %||% 5
    n_perfis <- n_classes - 1
    
    # Recalcular perfis usando quantis
    for (cn in crits) {
      sense <- input[[paste0("sense_", cn)]] %||% "benefit"
      x <- df[[cn]]
      
      probs <- seq(0, 1, length.out = n_classes + 1)[2:n_classes]
      u <- suppressWarnings(quantile(
        to_unit(x, cn, sense, rng), 
        probs = probs,
        na.rm = TRUE
      ))
      vals <- round(to_real(as.numeric(u), cn, sense, rng), 6)
      perfis_manuais[[cn]] <- vals
      
      # Atualizar inputs numericos no modal
      for (i in 1:n_perfis) {
        updateNumericInput(session, paste0("perfil_modal_", cn, "_", i), 
                           value = vals[i])
      }
    }
    
    # CORREÇÃO: Incrementar trigger
    if (!is.null(perfis_trigger)) {
      perfis_trigger(perfis_trigger() + 1)
    }
    
    showNotification(
      "Perfis resetados para valores baseados em quantis!",
      type = "message",
      duration = 2
    )
  })
}

#' Handler para resetar perfil individual
#' 
#' @param session Shiny session
#' @param ns Namespace function
#' @param input Shiny input
#' @param criterios Reactive com criterios
#' @param perfis_manuais reactiveValues com perfis
#' @param data_plain Reactive com dados
#' @param ranges_real Reactive com ranges
#' @param to_unit Funcao de conversao
#' @param to_real Funcao de conversao
#' @param perfis_trigger reactiveVal para forcar atualizacao
#' @export
resetar_perfil_individual <- function(session, ns, input, criterios, perfis_manuais,
                                      data_plain, ranges_real, to_unit, to_real, perfis_trigger = NULL) {
  
  # NOVO: Armazenar observers individuais
  observers_reset <- reactiveValues(observers = list())
  
  # NOVO: Limpar observers ao salvar/fechar modal
  observeEvent(input$btn_salvar_perfis, {
    lapply(observers_reset$observers, function(obs) obs$destroy())
    observers_reset$observers <- list()
  })
  
  # Criar observers ao abrir modal
  observeEvent(input$btn_abrir_modal_perfis, {
    req(criterios())
    req(data_plain())
    req(ranges_real())
    
    # Limpar observers antigos
    lapply(observers_reset$observers, function(obs) obs$destroy())
    observers_reset$observers <- list()
    
    crits <- criterios()
    df <- data_plain()
    rng <- ranges_real()
    
    n_classes <- input$n_classes %||% 5
    n_perfis <- n_classes - 1
    
    # Criar observeEvent para cada criterio
    novos_observers <- lapply(crits, function(cn) {
      observeEvent(input[[paste0("reset_perfil_", cn)]], {
        sense <- input[[paste0("sense_", cn)]] %||% "benefit"
        x <- df[[cn]]
        
        probs <- seq(0, 1, length.out = n_classes + 1)[2:n_classes]
        u <- suppressWarnings(quantile(
          to_unit(x, cn, sense, rng), 
          probs = probs,
          na.rm = TRUE
        ))
        vals <- round(to_real(as.numeric(u), cn, sense, rng), 6)
        perfis_manuais[[cn]] <- vals
        
        # Atualizar inputs numericos no modal
        for (i in 1:n_perfis) {
          updateNumericInput(session, paste0("perfil_modal_", cn, "_", i), 
                             value = vals[i])
        }
        
        # CORREÇÃO: Incrementar trigger
        if (!is.null(perfis_trigger)) {
          perfis_trigger(perfis_trigger() + 1)
        }
        
        showNotification(
          paste0("Perfil '", cn, "' resetado!"),
          type = "message",
          duration = 1.5
        )
      })
    })
    
    # Armazenar observers
    observers_reset$observers <- novos_observers
  })
}