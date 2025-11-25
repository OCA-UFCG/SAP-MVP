# =====================================================================
# MÓDULO: ANÁLISE MULTICRITÉRIO (ELECTRE Tri-B) - VERSÃO MODULAR
# =====================================================================
# Este é o arquivo principal que orquestra todos os componentes
# =====================================================================

# Carregar componentes modulares
source("R/analise/config.R", local = TRUE)
source("R/analise/utils.R", local = TRUE)
source("R/analise/filtros.R", local = TRUE)
source("R/analise/perfis.R", local = TRUE)
source("R/analise/electre_core.R", local = TRUE)
source("R/analise/spatial.R", local = TRUE)

# Carregar função ELECTRE externa
source("R/analise/electre_tri_b_func.R", local = TRUE)

# ---- UI ---------------------------------------------------------------
mod_analise_ui <- function(id) {
  ns <- NS(id)
  
  # Carregar UI components
  source("R/analise/ui_components.R", local = TRUE)
  
  criar_ui_analise(ns)
}

# ---- SERVER -----------------------------------------------------------
mod_analise_server <- function(id, preproc_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ====================================================================
    # INICIALIZAÇÃO E REATIVOS BÁSICOS
    # ====================================================================
    
    # Resetar valores ao iniciar sessão (evita cache)
    observe({
      updateNumericInput(session, "q_val", value = 0.02)
      updateNumericInput(session, "p_val", value = 0.10)
      updateNumericInput(session, "v_val", value = 0.50)
      updateCheckboxInput(session, "limiares_avancado", value = FALSE)
    }) |> bindEvent(session$clientData, once = TRUE, ignoreNULL = FALSE)
    
    # Cores e labels dinâmicos
    label_map <- reactive({
      n_classes <- input$n_classes %||% 5
      gerar_labels_classes(n_classes)
    })
    
    paleta_cores <- reactive({
      n_classes <- input$n_classes %||% 5
      gerar_paleta_cores(n_classes)
    })
    
    # Dados do pré-processamento
    data_sf <- reactive({
      req(preproc_data$data)
      preproc_data$data()
    })
    
    data_plain <- reactive({
      req(preproc_data$data_plain)
      preproc_data$data_plain()
    })
    
    vars_disponiveis <- reactive({
      req(preproc_data$vars)
      preproc_data$vars()
    })
    
    # ====================================================================
    # CRITÉRIOS E NORMALIZAÇÃO
    # ====================================================================
    
    # UI dinâmica: critérios
    output$ui_criterios <- renderUI({
      vars <- vars_disponiveis()
      req(length(vars) > 0)
      
      selectizeInput(
        ns("criterios_sel"),
        NULL,
        choices = vars,
        multiple = TRUE,
        selected = head(vars, 4),
        options = list(
          placeholder = "Selecione os critérios...",
          plugins = list("remove_button"),
          maxItems = 12
        )
      )
    })
    
    criterios <- reactive({
      req(input$criterios_sel)
      input$criterios_sel
    })
    
    # Ranges reais para normalização
    ranges_real <- reactive({
      df <- data_plain()
      crits <- criterios()
      req(length(crits) > 0)
      calcular_ranges_real(df, crits)
    })
    
    # ====================================================================
    # STATUS E UI DINÂMICA
    # ====================================================================
    
    output$status_dados <- renderUI({
      n_reg <- nrow(data_plain())
      n_var <- length(vars_disponiveis())
      
      tagList(
        div(
          class = "d-flex justify-content-between mb-2",
          span(icon("database"), " Registros:"),
          strong(formatar_numero(n_reg))
        ),
        div(
          class = "d-flex justify-content-between",
          span(icon("list"), " Variáveis:"),
          strong(n_var)
        )
      )
    })
    
    # Sentido dos critérios
    output$ui_sentido_criterios <- renderUI({
      crits <- criterios()
      req(length(crits) > 0)
      
      lapply(crits, function(cn) {
        div(
          class = "row mb-2",
          div(class = "col-7", tags$label(strong(cn), style = "margin-top: 8px;")),
          div(class = "col-5",
              selectInput(
                ns(paste0("sense_", cn)),
                NULL,
                choices = c("Benefício (↑)" = "benefit", "Custo (↓)" = "cost"),
                selected = "benefit"
              )
          )
        )
      })
    })
    
    # ====================================================================
    # PESOS
    # ====================================================================
    
    source("R/analise/ui_pesos.R", local = TRUE)
    criar_ui_pesos(output, input, session, ns, criterios)
    
    pesos_raw <- reactive({
      crits <- criterios()
      req(length(crits) > 0)
      
      sapply(crits, function(crit) {
        input[[paste0("peso_", crit)]] %||% 0
      })
    })
    
    W_norm <- reactive({
      crits <- criterios()
      req(length(crits) > 0)
      
      w_raw <- pesos_raw()
      w_norm <- normalizar_pesos(w_raw)
      setNames(w_norm, crits)
    })
    
    observeEvent(input$equalizar_pesos, {
      crits <- criterios()
      req(length(crits) > 0)
      
      peso_igual <- 1 / length(crits)
      
      lapply(crits, function(crit) {
        updateSliderInput(session, paste0("peso_", crit), value = peso_igual)
      })
    })
    
    # ====================================================================
    # PERFIS
    # ====================================================================
    
    perfis_manuais <- reactiveValues()
    
    # Inicializar perfis
    inicializar_perfis(perfis_manuais, input, data_plain, ranges_real, criterios, to_unit, to_real)
    
    # Observar mudanças em n_classes
    observar_mudanca_classes(input, perfis_manuais, criterios, data_plain, ranges_real, to_unit, to_real)
    
    # Modal de perfis
    criar_modal_perfis(session, ns, input, criterios, perfis_manuais)
    
    # Renderizar histogramas
    renderizar_histogramas_perfis(output, ns, input, criterios, data_plain, ranges_real, perfis_manuais)
    
    # Observar cliques
    observar_cliques_perfis(input, session, criterios, ranges_real, perfis_manuais, to_unit, to_real, clamp01)
    
    # Resumo perfis
    resumo_perfis_definidos(output, ns, input, criterios, perfis_manuais)
    
    # Matriz B
    B_current <- reactive({
      calcular_perfis_b(
        criterios = criterios(),
        n_classes = input$n_classes %||% 5,
        b_mode = input$b_mode,
        data_plain = data_plain(),
        ranges_real = ranges_real(),
        perfis_manuais = perfis_manuais,
        input = input,
        to_unit = to_unit
      )
    })
    
    # ====================================================================
    # RESUMO PARÂMETROS
    # ====================================================================
    
    output$resumo_entrada_electre <- renderText({
      crits <- criterios()
      req(length(crits) > 0)
      
      gerar_resumo_parametros(
        criterios = crits,
        W_norm = W_norm(),
        input = input,
        B_current = B_current()
      )
    })
    
    # ====================================================================
    # EXECUTAR ELECTRE
    # ====================================================================
    
    resultados_electre <- eventReactive(input$run_electre, {
      executar_electre(
        data_plain = data_plain,
        criterios = criterios,
        W_norm = W_norm,
        ranges_real = ranges_real,
        B_current = B_current,
        input = input,
        label_map = label_map,
        electre_tri_b_py = electre_tri_b_py,
        to_unit = to_unit
      )
    })
    
    # ====================================================================
    # OUTPUTS - DASHBOARD, TABELA, MAPA, QUALIFICAÇÃO
    # ====================================================================
    
    # Carregar outputs modulares
    source("R/analise/outputs_dashboard.R", local = TRUE)
    source("R/analise/outputs_tabela.R", local = TRUE)
    source("R/analise/outputs_mapa.R", local = TRUE)
    source("R/analise/outputs_qualificacao.R", local = TRUE)
    source("R/analise/downloads.R", local = TRUE)
    
    # Dashboard
    criar_outputs_dashboard(
      output, session, ns, input,
      resultados_electre, label_map, paleta_cores
    )
    
    # Tabela
    criar_outputs_tabela(
      output, session, ns,
      resultados_electre, paleta_cores
    )
    
    # Mapa
    criar_outputs_mapa(
      output, session, ns, input,
      resultados_electre, data_sf, label_map, paleta_cores
    )
    
    # Qualificação
    dados_espaciais <- reactive({
      carregar_dados_espaciais()
    })
    
    criar_outputs_qualificacao(
      output, session, ns, input,
      resultados_electre, data_sf, dados_espaciais,
      label_map, paleta_cores
    )
    
    # Downloads
    criar_handlers_download(
      output, session, ns,
      resultados_electre, data_sf, label_map, paleta_cores
    )
    
    # ====================================================================
    # RETORNAR PARA OUTROS MÓDULOS
    # ====================================================================
    
    return(
      list(
        resultados_electre = reactive({
          req(resultados_electre())
          resultados_electre()
        }),
        intersecoes = reactive({
          # Implementar se necessário
          NULL
        }),
        data_sf = reactive({
          req(data_sf())
          data_sf()
        }),
        label_map = reactive({
          label_map()
        }),
        paleta_cores = reactive({
          paleta_cores()
        })
      )
    )
    
  }) # FIM do moduleServer
}
