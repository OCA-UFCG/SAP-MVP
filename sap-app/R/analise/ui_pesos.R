# =====================================================================
# UI E LÓGICA DE PESOS - ELECTRE TRI-B
# =====================================================================

#' Criar UI dinâmica para pesos dos critérios
#' 
#' @param output Shiny output
#' @param input Shiny input
#' @param session Shiny session
#' @param ns Namespace function
#' @param criterios Reactive com lista de critérios
#' @export
criar_ui_pesos <- function(output, input, session, ns, criterios) {
  
  # UI: Sliders de pesos
  output$ui_pesos <- renderUI({
    crits <- criterios()
    req(length(crits) > 0)
    
    lapply(crits, function(crit) {
      sliderInput(
        ns(paste0("peso_", crit)),
        crit,
        min = 0,
        max = 1,
        value = 1 / length(crits),
        step = 0.01,
        ticks = FALSE
      )
    })
  })
  
  # UI: Pesos normalizados (exibição)
  output$pesos_normalizados <- renderUI({
    crits <- criterios()
    req(length(crits) > 0)
    
    # Obter pesos brutos
    w_raw <- sapply(crits, function(crit) {
      input[[paste0("peso_", crit)]] %||% 0
    })
    
    # Normalizar
    s <- sum(w_raw, na.rm = TRUE)
    
    if (!is.finite(s) || s <= 0) {
      w_raw[] <- 1 / length(crits)
    } else {
      w_raw <- w_raw / s
    }
    
    w_norm <- setNames(w_raw, crits)
    
    if (!length(w_norm)) return(NULL)
    
    HTML(paste(
      sprintf("<b>%s</b>: %.1f%%", names(w_norm), w_norm * 100),
      collapse = " • "
    ))
  })
}

#' Criar UI para limiares por critério (modo avançado)
#' 
#' @param output Shiny output
#' @param ns Namespace function
#' @param criterios Reactive com critérios
#' @export
criar_ui_limiares_por_criterio <- function(output, ns, criterios) {
  
  output$ui_limiares_por_criterio <- renderUI({
    crits <- criterios()
    req(length(crits) > 0)
    
    lapply(crits, function(cn) {
      div(
        style = "margin-bottom: 1rem; padding: 0.5rem; background: rgba(255,255,255,0.3); border-radius: 6px;",
        tags$strong(cn),
        tags$div(
          class = "row mt-2",
          div(class = "col-4",
              numericInput(ns(paste0("q_", cn)), "q", value = 0.02, min = 0, max = 1, step = 0.01)
          ),
          div(class = "col-4",
              numericInput(ns(paste0("p_", cn)), "p", value = 0.10, min = 0, max = 1, step = 0.01)
          ),
          div(class = "col-4",
              numericInput(ns(paste0("v_", cn)), "v", value = 0.50, min = 0, max = 1, step = 0.05)
          )
        )
      )
    })
  })
}
