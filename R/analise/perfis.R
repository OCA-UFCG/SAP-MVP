# =====================================================================
# EDITOR DE PERFIS INTERATIVO - ELECTRE TRI-B
# =====================================================================

#' Inicializar perfis com quantis padrão
#' 
#' @param perfis_manuais reactiveValues para armazenar perfis
#' @param input Shiny input
#' @param data_plain Reactive com dados plain
#' @param ranges_real Reactive com ranges dos critérios
#' @param criterios Reactive com lista de critérios
#' @param to_unit Função de conversão para [0,1]
#' @param to_real Função de conversão para valores reais
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

#' Observar mudanças em n_classes e recalcular perfis
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
#' @param criterios Reactive com critérios
#' @param perfis_manuais reactiveValues com perfis
#' @export
criar_modal_perfis <- function(session, ns, input, criterios, perfis_manuais) {
  observeEvent(input$btn_abrir_modal_perfis, {
    req(criterios())
    
    crits <- criterios()
    n_classes <- input$n_classes %||% 5
    n_perfis <- n_classes - 1
    
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
            tags$li("O perfil mais próximo ao clique será movido"),
            tags$li("As linhas vermelhas pontilhadas mostram os perfis atuais"),
            tags$li("Use os campos numéricos ou o botão 'Reset' para ajustes precisos")
          )
        ),
        
        lapply(seq(1, length(crits), by = 2), function(idx) {
          crits_linha <- crits[idx:min(idx + 1, length(crits))]
          
          fluidRow(
            lapply(crits_linha, function(cn) {
              sense <- input[[paste0("sense_", cn)]] %||% "benefit"
              orient <- if (sense == "benefit") "(↑ melhor)" else "(↓ melhor)"
              
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
    
    # Atualizar inputs com valores atuais
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
#' @param criterios Reactive com critérios
#' @param data_plain Reactive com dados
#' @param ranges_real Reactive com ranges
#' @param perfis_manuais reactiveValues com perfis
#' @export
renderizar_histogramas_perfis <- function(output, ns, input, criterios, data_plain, ranges_real, perfis_manuais) {
  
  # Função auxiliar para pegar valores atuais dos perfis
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
          labs(x = sprintf("%s (valores reais)", cn), y = "Frequência") +
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
#' @param criterios Reactive com critérios
#' @param ranges_real Reactive com ranges
#' @param perfis_manuais reactiveValues com perfis
#' @param to_unit Função de conversão
#' @param to_real Função de conversão
#' @param clamp01 Função de clamping
#' @export
observar_cliques_perfis <- function(input, session, criterios, ranges_real, perfis_manuais, to_unit, to_real, clamp01) {
  
  get_perfis_vals_real <- function(cn, n_perfis) {
    sapply(1:n_perfis, function(i) {
      input[[paste0("perfil_modal_", cn, "_", i)]] %||% 0
    })
  }
  
  observe({
    req(input$criterios_sel)
    req(length(input$criterios_sel) > 0)
    
    crits <- tryCatch(criterios(), error = function(e) NULL)
    if (is.null(crits) || length(crits) == 0) return(NULL)
    
    n_classes <- input$n_classes %||% 5
    n_perfis <- n_classes - 1
    
    req(ranges_real())
    rng <- ranges_real()
    
    lapply(crits, function(cn) {
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
  })
}

#' Resumo dos perfis definidos
#' 
#' @param output Shiny output
#' @param ns Namespace function
#' @param input Shiny input
#' @param criterios Reactive com critérios
#' @param perfis_manuais reactiveValues com perfis
#' @export
resumo_perfis_definidos <- function(output, ns, input, criterios, perfis_manuais) {
  output$resumo_perfis_definidos <- renderText({
    req(input$criterios_sel)
    req(length(input$criterios_sel) > 0)
    
    crits <- tryCatch(criterios(), error = function(e) NULL)
    if (is.null(crits) || length(crits) == 0) return("Aguardando seleção de critérios...")
    
    n_classes <- input$n_classes %||% 5
    n_perfis <- n_classes - 1
    
    resumo <- sapply(crits, function(cn) {
      perfis <- perfis_manuais[[cn]]
      if (!is.null(perfis)) {
        paste0(cn, ": [", paste(round(perfis, 4), collapse = ", "), "]")
      } else {
        paste0(cn, ": [não definido]")
      }
    })
    
    paste(resumo, collapse = "\n")
  })
}
