# =====================================================================
# OUTPUTS - ABA DASHBOARD
# =====================================================================

#' Criar outputs da aba Dashboard
#' 
#' @param output Shiny output
#' @param session Shiny session
#' @param ns Namespace function
#' @param input Shiny input
#' @param resultados_electre Reactive com resultados ELECTRE
#' @param label_map Reactive com labels das classes
#' @param paleta_cores Reactive com paleta de cores
#' @export
criar_outputs_dashboard <- function(output, session, ns, input,
                                    resultados_electre, label_map, paleta_cores) {
  
  # Sistema de filtros
  filtros_resultados_aplicados <- reactiveVal(list())
  
  criar_sistema_filtros_modal(
    session = session,
    ns = ns,
    id = "resultados",
    results_reactive = reactive({
      req(resultados_electre())
      resultados_electre()$results
    }),
    filtros_aplicados = filtros_resultados_aplicados
  )
  
  # Dados filtrados
  resultados_filtrados <- reactive({
    req(resultados_electre())
    results <- resultados_electre()$results
    aplicar_filtros_em_df(results, filtros_resultados_aplicados())
  })
  
  # Value boxes
  output$vb_total <- renderText({
    req(resultados_filtrados())
    formatar_numero(nrow(resultados_filtrados()))
  })
  
  output$vb_prop_alto <- renderText({
    req(resultados_filtrados())
    results <- resultados_filtrados()
    n_total <- nrow(results)
    if (n_total == 0) return("0.0%")
    n_c4_c5 <- sum(results$class_electre >= 4, na.rm = TRUE)
    sprintf("%.1f%%", (n_c4_c5 / n_total) * 100)
  })
  
  output$vb_dominante <- renderText({
    req(resultados_filtrados())
    results <- resultados_filtrados()
    if (nrow(results) == 0) return("N/A")
    tbl <- table(results$class_electre)
    classe_dom <- as.integer(names(which.max(tbl)))
    label_map()[as.character(classe_dom)]
  })
  
  # Gráfico distribuição
  output$plot_distribuicao <- renderPlotly({
    req(resultados_filtrados())
    results <- resultados_filtrados()
    n_classes <- resultados_electre()$params$n_classes %||% 5
    labels_atuais <- label_map()
    cores_atuais <- paleta_cores()
    
    df_dist <- data.frame(class_electre = 1:n_classes) |>
      left_join(results |> count(class_electre), by = "class_electre") |>
      mutate(
        n = replace_na(n, 0),
        class_label = labels_atuais[as.character(class_electre)],
        percent = n / sum(n) * 100
      )
    
    df_dist$percent[is.nan(df_dist$percent)] <- 0
    cores_graf <- unname(cores_atuais[as.character(1:n_classes)])
    
    if (isTRUE(input$dist_percent)) {
      plot_ly(df_dist, x = ~class_label, y = ~percent, type = "bar",
              marker = list(color = cores_graf),
              text = ~sprintf("%.1f%%", percent), textposition = "outside") |>
        layout(xaxis = list(title = "Classe"), yaxis = list(title = "Percentual (%)"),
               showlegend = FALSE)
    } else {
      plot_ly(df_dist, x = ~class_label, y = ~n, type = "bar",
              marker = list(color = cores_graf),
              text = ~n, textposition = "outside") |>
        layout(xaxis = list(title = "Classe"), yaxis = list(title = "Frequência"),
               showlegend = FALSE)
    }
  })
  
  # Observar variáveis categóricas
  observe({
    req(resultados_electre())
    df <- resultados_electre()$results
    vars_cat <- names(df)[sapply(df, function(x) {
      is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(x)) <= 30)
    })]
    default_var <- if ("UF" %in% vars_cat) "UF" else vars_cat[1]
    updateSelectInput(session, "var_categorica", choices = vars_cat, selected = default_var)
  })
  
  # Gráfico por categoria
  output$plot_por_categoria <- renderPlotly({
    req(resultados_filtrados())
    req(input$var_categorica)
    results <- resultados_filtrados()
    var_cat <- input$var_categorica
    if (!var_cat %in% names(results)) return(NULL)
    
    labels_atuais <- label_map()
    cores_atuais <- paleta_cores()
    
    df_cat <- results |>
      filter(!is.na(.data[[var_cat]])) |>
      count(.data[[var_cat]], class_electre) |>
      rename(categoria = 1) |>
      mutate(categoria = as.character(categoria),
             class_label = labels_atuais[as.character(class_electre)])
    
    if (nrow(df_cat) == 0) return(NULL)
    
    classes_presentes <- sort(unique(df_cat$class_electre))
    labels_presentes <- labels_atuais[as.character(classes_presentes)]
    cores_map <- setNames(unname(cores_atuais[as.character(classes_presentes)]), labels_presentes)
    
    if (input$tipo_grafico_cat == "stack") {
      plot_ly(df_cat, x = ~categoria, y = ~n, color = ~class_label, colors = cores_map,
              type = "bar", text = ~n, textposition = "inside") |>
        layout(barmode = "stack", xaxis = list(title = var_cat),
               yaxis = list(title = "Frequência"), legend = list(title = list(text = "Classe")))
    } else {
      plot_ly(df_cat, x = ~categoria, y = ~n, color = ~class_label, colors = cores_map,
              type = "bar", text = ~n, textposition = "outside") |>
        layout(barmode = "group", xaxis = list(title = var_cat),
               yaxis = list(title = "Frequência"), legend = list(title = list(text = "Classe")))
    }
  })
  
  # Observar densidade
  observe({
    req(resultados_electre())
    crits <- resultados_electre()$params$criterios
    updateSelectInput(session, "var_densidade", choices = crits, selected = crits[1])
  })
  
  # Gráfico densidade
  output$plot_densidade <- renderPlotly({
    req(resultados_filtrados())
    req(input$var_densidade)
    results <- resultados_filtrados()
    var_dens <- input$var_densidade
    n_classes <- resultados_electre()$params$n_classes %||% 5
    labels_atuais <- label_map()
    cores_atuais <- paleta_cores()
    
    if (!var_dens %in% names(results)) return(NULL)
    
    p <- plot_ly()
    for (i in 1:n_classes) {
      df_classe <- results |> filter(class_electre == i, !is.na(.data[[var_dens]]))
      if (nrow(df_classe) > 1) {
        dens <- density(df_classe[[var_dens]], na.rm = TRUE)
        p <- p |>
          add_trace(x = dens$x, y = dens$y, type = "scatter", mode = "lines",
                    name = labels_atuais[as.character(i)],
                    line = list(color = unname(cores_atuais[as.character(i)]), width = 2),
                    fill = "tozeroy",
                    fillcolor = paste0(unname(cores_atuais[as.character(i)]), "40"))
      }
    }
    p |> layout(xaxis = list(title = var_dens), yaxis = list(title = "Densidade"),
                legend = list(title = list(text = "Classe")))
  })
  
  # Tabela perfil médio
  output$tab_perfil_medio <- renderDT({
    req(resultados_filtrados())
    results <- resultados_filtrados()
    crits <- resultados_electre()$params$criterios
    n_classes <- resultados_electre()$params$n_classes %||% 5
    labels_atuais <- label_map()
    cores_atuais <- paleta_cores()
    
    df_perfil <- results |>
      group_by(class_electre) |>
      summarise(across(all_of(crits), ~mean(.x, na.rm = TRUE)), n = n(), .groups = "drop") |>
      mutate(Classe = labels_atuais[as.character(class_electre)], N = n) |>
      select(Classe, N, all_of(crits))
    
    datatable(df_perfil, rownames = FALSE,
              options = list(pageLength = n_classes, dom = "t", ordering = FALSE)) |>
      formatRound(columns = crits, digits = 2) |>
      formatStyle("Classe",
                  backgroundColor = styleEqual(labels_atuais[1:n_classes], cores_atuais[1:n_classes]),
                  fontWeight = "bold", color = "white")
  })
}