# =====================================================================
# MÓDULO: ANÁLISE MULTICRITÉRIO (ELECTRE Tri-B)
# =====================================================================

# Funções para gerar paleta e labels dinamicamente baseado em n_classes
gerar_paleta_cores <- function(n_classes) {
  if (n_classes < 3 || n_classes > 7) n_classes <- 5
  
  # Paleta RdYlGn do ColorBrewer (invertida: verde=baixo, vermelho=alto)
  paletas <- list(
    "3" = c("#1a9641", "#ffffbf", "#d7191c"),
    "4" = c("#1a9641", "#a6d96a", "#fdae61", "#d7191c"),
    "5" = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
    "6" = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
    "7" = c("#1a9850", "#66bd63", "#a6d96a", "#d9ef8b", "#fee08b", "#fdae61", "#d73027")
  )
  
  cores <- paletas[[as.character(n_classes)]]
  setNames(cores, as.character(1:n_classes))
}

gerar_labels_classes <- function(n_classes) {
  if (n_classes < 3 || n_classes > 7) n_classes <- 5
  
  descricoes <- c("muito baixo", "baixo", "médio", "alto", "muito alto", "altíssimo", "extremo")
  
  labels <- sapply(1:n_classes, function(i) {
    desc <- if (i <= length(descricoes)) descricoes[i] else paste0("nível ", i)
    paste0("C", i, " (", desc, ")")
  })
  
  setNames(labels, as.character(1:n_classes))
}

# Constantes padrão (5 classes) - mantidas para compatibilidade
LABEL_MAP <- gerar_labels_classes(5)
PAL_NUM <- gerar_paleta_cores(5)

# =====================================================================
# FUNÇÕES AUXILIARES PARA SISTEMA DE FILTROS COM MODAL
# =====================================================================

criar_botao_filtros_ui <- function(ns, id) {
  tagList(
    actionButton(
      ns(paste0("btn_abrir_modal_", id)),
      "Configurar Filtros",
      icon = icon("filter"),
      class = "btn-info w-100 mb-2"
    ),
    hr(),
    uiOutput(ns(paste0("ui_resumo_filtros_", id)))
  )
}

criar_sistema_filtros_modal <- function(session, ns, id, results_reactive, filtros_aplicados) {
  
  filtros_pendentes <- reactiveVal(list())
  
  observeEvent(session$input[[paste0("btn_abrir_modal_", id)]], {
    req(results_reactive())
    filtros_pendentes(filtros_aplicados())
    
    # Atualizar campos disponíveis apenas quando modal abre
    results <- results_reactive()
    campos <- names(results)
    campos_excluir <- c("geometry", "CD_MUN")
    campos_disponiveis <- setdiff(campos, campos_excluir)
    updateSelectInput(session, paste0("modal_campo_", id), 
                      choices = c("Selecione..." = "", campos_disponiveis))
    
    showModal(modalDialog(
      title = tags$div(icon("filter"), " Construtor de Filtros", 
                       style = "font-size: 1.2em; font-weight: bold;"),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(ns(paste0("btn_limpar_", id)), "Limpar Tudo", 
                     icon = icon("trash"), class = "btn-warning"),
        modalButton("Cancelar"),
        actionButton(ns(paste0("btn_aplicar_", id)), "Aplicar Filtros", 
                     icon = icon("check"), class = "btn-success")
      ),
      
      card(
        card_header(icon("list"), " Filtros Ativos"),
        uiOutput(ns(paste0("ui_modal_filtros_ativos_", id)))
      ),
      hr(),
      card(
        card_header(icon("plus-circle"), " Adicionar Novo Filtro"),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput(ns(paste0("modal_campo_", id)), "Campo:", choices = NULL),
          uiOutput(ns(paste0("ui_modal_operador_", id))),
          uiOutput(ns(paste0("ui_modal_valor_", id)))
        ),
        actionButton(ns(paste0("btn_adicionar_filtro_", id)), "Adicionar Filtro",
                     icon = icon("plus"), class = "btn-primary w-100 mt-2")
      )
    ))
  })
  
  output <- session$output
  output[[paste0("ui_modal_operador_", id)]] <- renderUI({
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(results_reactive())
    
    results <- results_reactive()
    campo <- results[[session$input[[paste0("modal_campo_", id)]]]]
    
    if (is.numeric(campo) && length(unique(campo)) > 30) {
      selectInput(ns(paste0("modal_operador_", id)), "Condição:",
                  choices = c("Selecione..." = "", "é igual a" = "==", "é maior que" = ">",
                              "é maior ou igual a" = ">=", "é menor que" = "<",
                              "é menor ou igual a" = "<=", "está entre" = "between"))
    } else {
      selectInput(ns(paste0("modal_operador_", id)), "Condição:",
                  choices = c("Selecione..." = "", "é igual a" = "==", 
                              "está entre as opções" = "in"))
    }
  })
  
  output[[paste0("ui_modal_valor_", id)]] <- renderUI({
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(session$input[[paste0("modal_operador_", id)]] != "")
    req(results_reactive())
    
    results <- results_reactive()
    campo <- results[[session$input[[paste0("modal_campo_", id)]]]]
    operador <- session$input[[paste0("modal_operador_", id)]]
    
    if (is.numeric(campo) && length(unique(campo)) > 30) {
      if (operador == "between") {
        tagList(
          numericInput(ns(paste0("modal_valor_min_", id)), "Valor mínimo:", 
                       value = min(campo, na.rm = TRUE)),
          numericInput(ns(paste0("modal_valor_max_", id)), "Valor máximo:", 
                       value = max(campo, na.rm = TRUE))
        )
      } else {
        numericInput(ns(paste0("modal_valor_", id)), "Valor:", 
                     value = median(campo, na.rm = TRUE))
      }
    } else {
      valores_unicos <- sort(unique(campo))
      if (operador == "in") {
        selectizeInput(ns(paste0("modal_valor_", id)), "Valores:",
                       choices = valores_unicos, selected = NULL, multiple = TRUE,
                       options = list(placeholder = "Selecione um ou mais...",
                                      plugins = list("remove_button")))
      } else {
        selectInput(ns(paste0("modal_valor_", id)), "Valor:",
                    choices = c("Selecione..." = "", valores_unicos))
      }
    }
  })
  
  observeEvent(session$input[[paste0("btn_adicionar_filtro_", id)]], {
    req(session$input[[paste0("modal_campo_", id)]] != "")
    req(session$input[[paste0("modal_operador_", id)]] != "")
    
    results <- results_reactive()
    campo_nome <- session$input[[paste0("modal_campo_", id)]]
    operador <- session$input[[paste0("modal_operador_", id)]]
    
    novo_filtro <- list(
      campo = campo_nome,
      operador = operador,
      operador_nome = switch(operador, "==" = "é igual a", ">" = "é maior que",
                             ">=" = "é maior ou igual a", "<" = "é menor que", 
                             "<=" = "é menor ou igual a", "between" = "está entre",
                             "in" = "está entre as opções")
    )
    
    if (operador == "between") {
      req(session$input[[paste0("modal_valor_min_", id)]], 
          session$input[[paste0("modal_valor_max_", id)]])
      novo_filtro$valor <- c(session$input[[paste0("modal_valor_min_", id)]], 
                             session$input[[paste0("modal_valor_max_", id)]])
      novo_filtro$valor_texto <- paste(session$input[[paste0("modal_valor_min_", id)]], 
                                       "e", session$input[[paste0("modal_valor_max_", id)]])
    } else if (operador == "in") {
      req(length(session$input[[paste0("modal_valor_", id)]]) > 0)
      novo_filtro$valor <- session$input[[paste0("modal_valor_", id)]]
      if (length(novo_filtro$valor) <= 3) {
        novo_filtro$valor_texto <- paste(novo_filtro$valor, collapse = ", ")
      } else {
        novo_filtro$valor_texto <- paste0(length(novo_filtro$valor), " opções selecionadas")
      }
    } else {
      req(session$input[[paste0("modal_valor_", id)]] != "")
      novo_filtro$valor <- session$input[[paste0("modal_valor_", id)]]
      novo_filtro$valor_texto <- as.character(novo_filtro$valor)
    }
    
    filtros_atuais <- filtros_pendentes()
    filtros_atuais[[length(filtros_atuais) + 1]] <- novo_filtro
    filtros_pendentes(filtros_atuais)
    
    # CORREÇÃO: Usar isolate para evitar reações em cadeia
    isolate({
      updateSelectInput(session, paste0("modal_campo_", id), selected = "")
    })
    
    showNotification("Filtro adicionado! Clique em 'Aplicar Filtros' para confirmar.", 
                     type = "message", duration = 2)
  })
  
  output[[paste0("ui_modal_filtros_ativos_", id)]] <- renderUI({
    filtros <- filtros_pendentes()
    if (length(filtros) == 0) {
      return(div(class = "alert alert-info", icon("info-circle"), 
                 " Nenhum filtro adicionado ainda."))
    }
    
    tagList(
      lapply(seq_along(filtros), function(i) {
        f <- filtros[[i]]
        div(class = "alert alert-secondary",
            style = "display: flex; justify-content: space-between; align-items: center; padding: 8px 12px; margin-bottom: 8px;",
            div(style = "flex-grow: 1;",
                tags$strong(f$campo), " ", f$operador_nome, " ", tags$code(f$valor_texto)),
            actionButton(ns(paste0("btn_remove_filtro_", id, "_", i)), icon("times"),
                         class = "btn-sm btn-danger", style = "padding: 2px 8px;")
        )
      })
    )
  })
  
  observe({
    filtros <- filtros_pendentes()
    if (length(filtros) > 0) {
      lapply(seq_along(filtros), function(i) {
        observeEvent(session$input[[paste0("btn_remove_filtro_", id, "_", i)]], {
          filtros_atuais <- filtros_pendentes()
          filtros_atuais[[i]] <- NULL
          filtros_pendentes(filtros_atuais)
        })
      })
    }
  })
  
  observeEvent(session$input[[paste0("btn_limpar_", id)]], {
    filtros_pendentes(list())
    showNotification("Todos os filtros foram removidos.", type = "warning", duration = 2)
  })
  
  observeEvent(session$input[[paste0("btn_aplicar_", id)]], {
    filtros_aplicados(filtros_pendentes())
    removeModal()
    n_filtros <- length(filtros_pendentes())
    if (n_filtros > 0) {
      showNotification(paste0(n_filtros, " filtro(s) aplicado(s) com sucesso!"),
                       type = "message", duration = 3)
    } else {
      showNotification("Filtros removidos. Mostrando todos os dados.", 
                       type = "default", duration = 3)
    }
  })
  
  output[[paste0("ui_resumo_filtros_", id)]] <- renderUI({
    filtros <- filtros_aplicados()
    if (length(filtros) == 0) {
      return(helpText(icon("info-circle"), " Nenhum filtro aplicado.", br(), br(),
                      "Clique em 'Configurar Filtros' para adicionar."))
    }
    
    tagList(
      div(class = "alert alert-success", style = "padding: 8px; margin-bottom: 10px;",
          tags$strong(icon("check-circle"), sprintf(" %d filtro(s) ativo(s)", length(filtros)))),
      div(style = "max-height: 300px; overflow-y: auto; font-size: 0.85em;",
          lapply(seq_along(filtros), function(i) {
            f <- filtros[[i]]
            div(style = "padding: 6px; margin-bottom: 6px; background: #f8f9fa; border-left: 3px solid #17a2b8; border-radius: 3px;",
                tags$strong(f$campo), br(),
                tags$small(f$operador_nome, " ", tags$code(f$valor_texto)))
          })
      )
    )
  })
}

aplicar_filtros_em_df <- function(df, filtros) {
  if (length(filtros) == 0) return(df)
  
  # CORREÇÃO: Verificar se é sf e remover geometry temporariamente
  is_sf <- inherits(df, "sf")
  if (is_sf) {
    geom <- sf::st_geometry(df)
    df <- sf::st_drop_geometry(df)
  }
  
  for (filtro in filtros) {
    campo <- filtro$campo
    operador <- filtro$operador
    valor <- filtro$valor
    
    if (operador == "==") {
      df <- df[df[[campo]] == valor, ]
    } else if (operador == ">") {
      df <- df[df[[campo]] > valor, ]
    } else if (operador == ">=") {
      df <- df[df[[campo]] >= valor, ]
    } else if (operador == "<") {
      df <- df[df[[campo]] < valor, ]
    } else if (operador == "<=") {
      df <- df[df[[campo]] <= valor, ]
    } else if (operador == "between") {
      df <- df[df[[campo]] >= valor[1] & df[[campo]] <= valor[2], ]
    } else if (operador == "in") {
      df <- df[df[[campo]] %in% valor, ]
    }
  }
  
  # CORREÇÃO: Re-adicionar geometry se era sf
  if (is_sf) {
    geom_filtrado <- geom[as.integer(rownames(df))]
    df <- sf::st_sf(df, geometry = geom_filtrado)
  }
  
  df
}

# ---- UI ---------------------------------------------------------------
mod_analise_ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    # SIDEBAR
    sidebar = sidebar(
      # STATUS
      card(
        card_header(
          icon("info-circle"), " Status dos Dados",
          class = "bg-primary"
        ),
        uiOutput(ns("status_dados"))
      ),
      
      # AÇÃO PRINCIPAL
      actionButton(
        ns("run_electre"),
        "Executar ELECTRE Tri-B",
        icon = icon("play"),
        class = "btn-primary w-100 mb-3"
      ),
      
      # DOWNLOADS
      card(
        card_header(icon("download"), " Exportar", class = "bg-primary"),
        downloadButton(ns("dl_resultados_csv"), "Resultados CSV", class = "w-100 mb-2"),
        downloadButton(ns("dl_resultados_gpkg"), "Resultados GeoPackage", class = "w-100 mb-2"),
        downloadButton(ns("dl_mapa_png"), "Mapa PNG", class = "w-100")
      )
    ),
    
    # MAIN CONTENT
    navset_card_tab(
      full_screen = TRUE,
      
      # ==================================================================
      # ABA 1: PARÂMETROS
      # ==================================================================
      nav_panel(
        title = "Parâmetros",
        icon = icon("sliders"),
        
        card_header(
          icon("gear"), " Configuração ELECTRE Tri-B",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Configure critérios, pesos, perfis e limiares para a análise multicritério",
            placement = "bottom"
          )
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          
          # COLUNA ESQUERDA: CRITÉRIOS
          card(
            full_screen = TRUE,
            card_header(icon("list-check"), " Critérios e Sentido"),
            
            helpText("Selecione os critérios e defina se são benefício ou custo:"),
            uiOutput(ns("ui_criterios")),
            
            hr(),
            
            helpText(icon("info-circle"), " Configure o sentido de cada critério:"),
            uiOutput(ns("ui_sentido_criterios"))
          ),
          
          # COLUNA DIREITA: PESOS
          card(
            full_screen = TRUE,
            card_header(icon("balance-scale"), " Pesos dos Critérios"),
            
            helpText("Defina os pesos (serão normalizados automaticamente):"),
            uiOutput(ns("ui_pesos")),
            
            div(
              class = "alert alert-info mt-2",
              icon("calculator"), " Pesos normalizados (soma = 1.0):",
              br(),
              htmlOutput(ns("pesos_normalizados"))
            ),
            
            actionButton(ns("equalizar_pesos"), "Equalizar Pesos", 
                         icon = icon("equals"), class = "btn-sm btn-secondary w-100 mt-2")
          )
        ),
        
        # PERFIS E LIMIARES
        layout_columns(
          col_widths = c(6, 6),
          
          card(
            full_screen = TRUE,
            card_header(
              icon("layer-group"), " Perfis de Classe (B)",
              tooltip(
                icon("circle-info", class = "ms-2"),
                "Define os limites entre as classes. Número de perfis = número de classes - 1. Valores em [0,1] após normalização.",
                placement = "bottom"
              )
            ),
            numericInput(
              ns("n_classes"),
              "Número de Classes",
              value = 5,
              min = 3,
              max = 7,
              step = 1
            ),
            radioButtons(
              ns("b_mode"),
              "Método de definição dos perfis:",
              choices = c("Quantis 20/40/60/80 (padrão)" = "quantis",
                          "Definir manualmente" = "manual"),
              selected = "quantis"
            ),
            uiOutput(ns("ui_perfis_manuais"))
          ),
          
          card(
            full_screen = TRUE,
            card_header(
              icon("ruler"), " Limiares e Classificação",
              tooltip(
                icon("circle-info", class = "ms-2"),
                "q = indiferença, p = preferência, v = veto. Valores típicos: q=0.02, p=0.10, v=0.50. Lambda = credibilidade mínima.",
                placement = "bottom"
              )
            ),
            
            checkboxInput(ns("limiares_avancado"), 
                          "Definir limiares por critério (avançado)", 
                          value = FALSE),
            
            conditionalPanel(
              condition = "!input.limiares_avancado",
              ns = ns,
              helpText("Limiares (aplicados a todos os critérios):"),
              numericInput(ns("q_val"), "Indiferença (q)", value = 0.02, min = 0, max = 1, step = 0.01),
              numericInput(ns("p_val"), "Preferência (p)", value = 0.10, min = 0, max = 1, step = 0.01),
              numericInput(ns("v_val"), "Veto (v)", value = 0.50, min = 0, max = 1, step = 0.05)
            ),
            
            conditionalPanel(
              condition = "input.limiares_avancado",
              ns = ns,
              helpText("Defina limiares específicos para cada critério:"),
              uiOutput(ns("ui_limiares_por_criterio"))
            ),
            
            hr(),
            
            numericInput(ns("lambda_cut"), "Lambda (λ)", value = 0.70, min = 0, max = 1, step = 0.05),
            selectInput(
              ns("rule"),
              "Regra de atribuição",
              choices = c("Pessimista (pc)" = "pc", "Otimista (oc)" = "oc"),
              selected = "pc"
            )
          )
        ),
        
        # RESUMO DOS DADOS DE ENTRADA
        card(
          full_screen = TRUE,
          card_header(
            icon("terminal"), " Resumo dos Dados de Entrada ELECTRE",
            tooltip(
              icon("circle-info", class = "ms-2"),
              "Visualize os parâmetros que serão utilizados na função electre_tri_b_py()",
              placement = "bottom"
            )
          ),
          div(
            style = "background: #f8f9fa; padding: 1rem; border-radius: 6px; font-family: 'Courier New', monospace; font-size: 0.9em; overflow-x: auto;",
            verbatimTextOutput(ns("resumo_entrada_electre"))
          )
        )
      ),
      
      # ==================================================================
      # ABA 2: RESULTADOS (DASHBOARD)
      # ==================================================================
      nav_panel(
        title = "Resultados",
        icon = icon("chart-line"),
        
        layout_sidebar(
          sidebar = sidebar(
            width = 250,
            position = "left",
            
            card(
              card_header("Filtros", class = "bg-primary"),
              criar_botao_filtros_ui(ns, "resultados")
            )
          ),
          
          # MAIN CONTENT
          card_header(
            icon("chart-pie"), " Dashboard - Classificação ELECTRE Tri-B",
            tooltip(
              icon("circle-info", class = "ms-2"),
              "Visão geral dos resultados da classificação multicritério",
              placement = "bottom"
            )
          ),
          
          # VALUE BOXES - MÉTRICAS PRINCIPAIS
          layout_column_wrap(
            width = "250px",
            fill = FALSE,
            value_box(
              title = "Municípios Classificados",
              value = textOutput(ns("vb_total")),
              showcase = icon("map-marked-alt"),
              theme = "primary"
            ),
            value_box(
              title = "Proporção C4-C5 (Alto + Muito Alto)",
              value = textOutput(ns("vb_prop_alto")),
              showcase = icon("triangle-exclamation"),
              theme = "danger"
            ),
            value_box(
              title = "Classe Dominante",
              value = textOutput(ns("vb_dominante")),
              showcase = icon("crown"),
              theme = "info"
            )
          ),
          
          # GRÁFICOS
          layout_columns(
            col_widths = c(6, 6),
            
            # Distribuição das Classes
            card(
              full_screen = TRUE,
              card_header(
                icon("chart-bar"), " Distribuição das Classes",
                popover(
                  icon("gear", class = "ms-2"),
                  "Opções",
                  checkboxInput(ns("dist_percent"), "Mostrar percentuais", value = FALSE)
                )
              ),
              plotlyOutput(ns("plot_distribuicao"), height = "450px")
            ),
            
            # Proporção por UF (ou outra variável categórica)
            card(
              full_screen = TRUE,
              card_header(
                icon("chart-area"), " Classes por Variável Categórica",
                popover(
                  icon("gear", class = "ms-2"),
                  "Opções",
                  selectInput(ns("var_categorica"), "Variável:", choices = NULL, selected = NULL),
                  radioButtons(ns("tipo_grafico_cat"), "Tipo:", 
                               choices = c("Empilhado" = "stack", "Agrupado" = "group"), 
                               selected = "stack")
                )
              ),
              plotlyOutput(ns("plot_por_categoria"), height = "450px")
            )
          ),
          
          # PERFIL MÉDIO POR CLASSE E DENSIDADE
          layout_columns(
            col_widths = c(7, 5),
            
            # Tabela: Perfil médio das variáveis por classe
            card(
              full_screen = TRUE,
              card_header(icon("table"), " Perfil Médio das Variáveis por Classe"),
              DTOutput(ns("tab_perfil_medio"))
            ),
            
            # Densidade por classe
            card(
              full_screen = TRUE,
              card_header(
                icon("chart-line"), " Densidade por Classe",
                popover(
                  icon("gear", class = "ms-2"),
                  "Opções",
                  selectInput(ns("var_densidade"), "Variável:", choices = NULL, selected = NULL)
                )
              ),
              plotlyOutput(ns("plot_densidade"), height = "450px")
            )
          )
        ) # Fecha layout_sidebar
      ),
      
      # ==================================================================
      # ABA 3: TABELA RESULTADOS
      # ==================================================================
      nav_panel(
        title = "Tabela Resultados",
        icon = icon("table"),
        
        layout_sidebar(
          sidebar = sidebar(
            width = 250,
            position = "left",
            
            card(
              card_header("Filtros", class = "bg-primary"),
              criar_botao_filtros_ui(ns, "tabela")
            )
          ),
          
          # MAIN CONTENT
          card_header(
            icon("database"), " Tabela Completa de Resultados",
            tooltip(
              icon("circle-info", class = "ms-2"),
              "Tabela detalhada com filtros e opções de download",
              placement = "bottom"
            )
          ),
          
          card(
            # Botões de download na parte superior
            div(
              class = "d-flex justify-content-end gap-2 mb-3",
              downloadButton(ns("dl_tabela_csv"), "CSV", class = "btn-sm"),
              downloadButton(ns("dl_tabela_excel"), "Excel", class = "btn-sm")
            ),
            DTOutput(ns("tab_completa"))
          )
        )
      ),
      
      # ==================================================================
      # ABA 4: MAPA
      # ==================================================================
      nav_panel(
        title = "Mapa",
        icon = icon("map"),
        
        layout_sidebar(
          sidebar = sidebar(
            width = 250,
            position = "left",
            
            card(
              card_header("Filtros e Busca", class = "bg-primary"),
              
              # Busca por município
              div(
                class = "mb-3",
                tags$label("Buscar Município:"),
                selectizeInput(
                  ns("busca_municipio"),
                  NULL,
                  choices = NULL,
                  options = list(
                    placeholder = "Digite o nome...",
                    maxOptions = 10
                  )
                ),
                actionButton(
                  ns("btn_zoom_municipio"),
                  "Aplicar Zoom",
                  icon = icon("search-location"),
                  class = "btn-primary w-100"
                )
              ),
              
              hr(),
              
              # Filtros com modal
              criar_botao_filtros_ui(ns, "mapa")
            )
          ),
          
          # MAIN CONTENT
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span(
                icon("map-location-dot"), " Mapa de Classificação",
                tooltip(
                  icon("circle-info", class = "ms-2"),
                  "Visualização espacial das classes ELECTRE Tri-B. Use os filtros para explorar.",
                  placement = "bottom"
                )
              ),
              downloadButton(ns("export_mapa_principal"), "Exportar HTML", 
                             class = "btn-sm", icon = icon("download"))
            ),
            card_body(
              leafletOutput(ns("mapa_classes"), height = "calc(100vh - 300px)"),
              fillable = TRUE
            )
          )
        )
      ),
      
      # ==================================================================
      # ABA 5: QUALIFICAÇÃO
      # ==================================================================
      nav_panel(
        title = "Qualificação",
        icon = icon("layer-group"),
        
        
        # Wrapper com rolagem para permitir cards grandes
        div(
          style = "height: calc(100vh - 200px); overflow-y: auto; padding: 10px;",
          
          layout_sidebar(
            sidebar = sidebar(
              width = 250,
              position = "left",
              
              card(
                card_header("Filtros", class = "bg-primary"),
                criar_botao_filtros_ui(ns, "qualificacao")
              )
            ),
            
            # MAIN CONTENT
            card_header(
              icon("chart-area"), " Análise de Qualificação Territorial",
              tooltip(
                icon("circle-info", class = "ms-2"),
                "Análise espacial das camadas territoriais em relação à classificação ELECTRE",
                placement = "bottom"
              )
            ),
            
            # VALUE BOXES
            layout_column_wrap(
              width = "150px",
              fill = FALSE,
              
              value_box(
                title = "Quilombolas",
                value = textOutput(ns("vb_quilombolas")),
                showcase = icon("users"),
                theme = "warning"
              ),
              value_box(
                title = "Assentamentos",
                value = textOutput(ns("vb_assentamentos")),
                showcase = icon("home"),
                theme = "success"
              ),
              value_box(
                title = "T. Indígenas",
                value = textOutput(ns("vb_indigenas")),
                showcase = icon("globe"),
                theme = "info"
              ),
              value_box(
                title = "Inst. Ensino",
                value = textOutput(ns("vb_ensino")),
                showcase = icon("graduation-cap"),
                theme = "primary"
              ),
              value_box(
                title = "U. Prisionais",
                value = textOutput(ns("vb_prisoes")),
                showcase = icon("building-shield"),
                theme = "danger"
              ),
              value_box(
                title = "B. Sementes",
                value = textOutput(ns("vb_sementes")),
                showcase = icon("seedling"),
                theme = "success"
              )
            ),
            
            # GRÁFICOS E MAPA
            layout_columns(
              col_widths = c(12),
              
              # Mapa interativo com camadas
              card(
                full_screen = TRUE,
                height = "700px",
                fill = TRUE,
                card_header(
                  class = "d-flex justify-content-between align-items-center",
                  span(
                    icon("map-marked-alt"), " Mapa de Qualificação Territorial"
                  ),
                  div(
                    popover(
                      icon("gear", class = "ms-2"),
                      "Opções",
                      sliderInput(ns("opacidade_camadas"), "Opacidade das camadas:", 
                                  min = 0.3, max = 1, value = 0.7, step = 0.1)
                    ),
                    downloadButton(ns("export_mapa_qualif"), "Exportar HTML", 
                                   class = "btn-sm ms-2", icon = icon("download"))
                  )
                ),
                card_body(
                  leafletOutput(ns("mapa_qualificacao"), height = "100%"),
                  fillable = TRUE
                )
              )
            ),
            
            layout_columns(
              col_widths = c(6, 6),
              
              # Gráfico: Distribuição por classe
              card(
                full_screen = TRUE,
                height = "400px",
                fill = TRUE,
                card_header(icon("chart-bar"), " Distribuição por Classe ELECTRE"),
                card_body(
                  plotlyOutput(ns("plot_qualif_classes"), height = "100%"),
                  fillable = TRUE
                )
              ),
              
              # Gráfico: Comparativo de camadas
              card(
                full_screen = TRUE,
                height = "400px",
                fill = TRUE,
                card_header(icon("chart-pie"), " Distribuição Total das Camadas"),
                card_body(
                  plotlyOutput(ns("plot_qualif_total"), height = "100%"),
                  fillable = TRUE
                )
              )
            ),
            
            # TABELAS ESTATÍSTICAS
            layout_columns(
              col_widths = c(6, 6),
              
              card(
                full_screen = TRUE,
                height = "300px",
                fill = TRUE,
                card_header(icon("table"), " Estatísticas por Classe ELECTRE"),
                card_body(
                  DTOutput(ns("tab_estatisticas_qualif")),
                  fillable = TRUE
                )
              ),
              
              # NOVA TABELA: Ranking de Municípios
              card(
                full_screen = TRUE,
                height = "300px",
                fill = TRUE,
                card_header(icon("trophy"), " Ranking: Municípios com Mais Interseções"),
                card_body(
                  DTOutput(ns("tab_ranking_municipios")),
                  fillable = TRUE
                )
              )
            )
          )
          
        ) # Fecha div com rolagem
        
      )
    )
  )
}

# ---- SERVER -----------------------------------------------------------
mod_analise_server <- function(id, preproc_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Carregar função ELECTRE
    source("R/electre_tri_b_func.R", local = TRUE)
    
    # Operador %||%
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    # Resetar valores ao iniciar sessão (evita cache)
    observe({
      updateNumericInput(session, "q_val", value = 0.02)
      updateNumericInput(session, "p_val", value = 0.10)
      updateNumericInput(session, "v_val", value = 0.50)
      updateCheckboxInput(session, "limiares_avancado", value = FALSE)
    }) |> bindEvent(session$clientData, once = TRUE, ignoreNULL = FALSE)
    
    # ====================================================================
    # CORES E LABELS DINÂMICOS (baseados em n_classes)
    # ====================================================================
    
    label_map <- reactive({
      n_classes <- input$n_classes %||% 5
      gerar_labels_classes(n_classes)
    })
    
    paleta_cores <- reactive({
      n_classes <- input$n_classes %||% 5
      gerar_paleta_cores(n_classes)
    })
    
    # ====================================================================
    # DADOS DO PRÉ-PROCESSAMENTO
    # ====================================================================
    
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
    # FUNÇÕES AUXILIARES (seguindo lógica do app de referência)
    # ====================================================================
    
    clamp01 <- function(z) pmin(pmax(z, 0), 1)
    
    # Calcular ranges reais para normalização
    ranges_real <- reactive({
      df <- data_plain()
      crits <- criterios()
      req(length(crits) > 0)
      setNames(lapply(crits, function(cn) range(df[[cn]], na.rm = TRUE)), crits)
    })
    
    # Converter para valores unitários [0,1]
    to_unit <- function(x, cn, sense, ranges) {
      r <- ranges[[cn]]
      den <- r[2] - r[1]
      if (!is.finite(den) || den == 0) return(rep(0, length(x)))
      if (sense == "benefit") {
        clamp01((x - r[1]) / den)
      } else {
        clamp01((r[2] - x) / den)  # inverte automaticamente
      }
    }
    
    # Converter de volta para valores reais
    to_real <- function(u, cn, sense, ranges) {
      r <- ranges[[cn]]
      den <- r[2] - r[1]
      if (!is.finite(den) || den == 0) return(rep(r[1], length(u)))
      u <- clamp01(u)
      if (sense == "benefit") {
        r[1] + u * den
      } else {
        r[2] - u * den
      }
    }
    
    # ====================================================================
    # STATUS DOS DADOS
    # ====================================================================
    
    output$status_dados <- renderUI({
      n_reg <- nrow(data_plain())
      n_var <- length(vars_disponiveis())
      
      tagList(
        div(
          class = "d-flex justify-content-between mb-2",
          span(icon("database"), " Registros:"),
          strong(format(n_reg, big.mark = " ", decimal.mark = ","))
        ),
        div(
          class = "d-flex justify-content-between",
          span(icon("list"), " Variáveis:"),
          strong(n_var)
        )
      )
    })
    
    # ====================================================================
    # UI DINÂMICA: CRITÉRIOS
    # ====================================================================
    
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
    
    # Sentido dos critérios (benefício ou custo)
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
    # UI DINÂMICA: PESOS
    # ====================================================================
    
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
    
    # Pesos brutos (não normalizados)
    pesos_raw <- reactive({
      crits <- criterios()
      req(length(crits) > 0)
      
      sapply(crits, function(crit) {
        input[[paste0("peso_", crit)]] %||% 0
      })
    })
    
    # Pesos normalizados (soma = 1)
    W_norm <- reactive({
      crits <- criterios()
      req(length(crits) > 0)
      
      w_raw <- pesos_raw()
      s <- sum(w_raw, na.rm = TRUE)
      
      if (!is.finite(s) || s <= 0) {
        w_raw[] <- 1 / length(crits)
      } else {
        w_raw <- w_raw / s
      }
      
      setNames(w_raw, crits)
    })
    
    output$pesos_normalizados <- renderUI({
      w <- W_norm()
      if (!length(w)) return(NULL)
      
      HTML(paste(
        sprintf("<b>%s</b>: %.1f%%", names(w), w * 100),
        collapse = " • "
      ))
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
    # UI DINÂMICA: LIMIARES POR CRITÉRIO (MODO AVANÇADO)
    # ====================================================================
    
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
    
    # ====================================================================
    # UI DINÂMICA: PERFIS MANUAIS (POR CRITÉRIO)
    # ====================================================================
    
    output$ui_perfis_manuais <- renderUI({
      if (input$b_mode != "manual") return(NULL)
      
      crits <- criterios()
      req(length(crits) > 0)
      
      n_classes <- input$n_classes %||% 5
      n_perfis <- n_classes - 1
      
      df <- data_plain()
      rng <- ranges_real()
      
      tagList(
        helpText("Defina os perfis em valores REAIS para cada critério:"),
        lapply(crits, function(cn) {
          sense <- input[[paste0("sense_", cn)]] %||% "benefit"
          x <- df[[cn]]
          
          # Calcular valores sugeridos via quantis
          u <- suppressWarnings(quantile(
            to_unit(x, cn, sense, rng), 
            probs = seq(0, 1, length.out = n_classes + 1)[2:n_classes],
            na.rm = TRUE
          ))
          vals <- round(to_real(as.numeric(u), cn, sense, rng), 6)
          
          rs <- rng[[cn]]
          step <- signif((rs[2] - rs[1]) / 200, 2)
          
          div(
            style = "margin-bottom: 1rem; padding: 1rem; background: rgba(255,255,255,0.5); border-radius: 8px;",
            tags$label(tags$strong(sprintf("%s (valores reais)", cn))),
            tags$div(
              class = "row",
              lapply(1:n_perfis, function(i) {
                div(
                  class = sprintf("col-%d", 12 %/% n_perfis),
                  numericInput(
                    ns(paste0("B_", cn, "_", i)),
                    paste0("b", i),
                    value = vals[i],
                    min = rs[1],
                    max = rs[2],
                    step = step
                  )
                )
              })
            )
          )
        })
      )
    })
    
    # ====================================================================
    # PERFIS B (matriz n_perfis x n_criterios) - CORRETO POR CRITÉRIO
    # ====================================================================
    
    B_current <- reactive({
      crits <- criterios()
      req(length(crits) > 0)
      
      n_classes <- input$n_classes %||% 5
      n_perfis <- n_classes - 1
      
      df <- data_plain()
      rng <- ranges_real()
      
      # Matriz de perfis (n_perfis x n_criterios)
      Bm <- matrix(NA_real_, nrow = n_perfis, ncol = length(crits), 
                   dimnames = list(paste0("b", 1:n_perfis), crits))
      
      if (input$b_mode == "quantis") {
        # Quantis automáticos - CALCULA POR CRITÉRIO
        probs <- seq(0, 1, length.out = n_classes + 1)[2:n_classes]
        
        for (j in seq_along(crits)) {
          cn <- crits[j]
          sense <- input[[paste0("sense_", cn)]] %||% "benefit"
          ucol <- to_unit(df[[cn]], cn, sense, rng)
          Bm[, j] <- quantile(ucol, probs = probs, type = 7, na.rm = TRUE)
        }
      } else {
        # Manual - VALORES REAIS POR CRITÉRIO, depois converte para [0,1]
        for (j in seq_along(crits)) {
          cn <- crits[j]
          sense <- input[[paste0("sense_", cn)]] %||% "benefit"
          
          # Coletar valores reais digitados pelo usuário
          vals_real <- sapply(1:n_perfis, function(i) {
            input[[paste0("B_", cn, "_", i)]] %||% {
              # Valor padrão caso não exista
              r <- rng[[cn]]
              if (sense == "benefit") {
                r[1] + (i / (n_perfis + 1)) * (r[2] - r[1])
              } else {
                r[2] - (i / (n_perfis + 1)) * (r[2] - r[1])
              }
            }
          })
          
          # Converter valores reais para [0,1]
          u <- to_unit(vals_real, cn, sense, rng)
          Bm[, j] <- sort(clamp01(u))
        }
      }
      
      Bm
    })
    
    # ====================================================================
    # RESUMO DOS DADOS DE ENTRADA (FORMATO CONSOLE R)
    # ====================================================================
    
    output$resumo_entrada_electre <- renderText({
      crits <- criterios()
      req(length(crits) > 0)
      
      n_classes <- input$n_classes %||% 5
      n_perfis <- n_classes - 1
      
      # Pesos
      W <- W_norm()[crits]
      
      # Limiares
      if (isTRUE(input$limiares_avancado)) {
        Q <- sapply(crits, function(cn) input[[paste0("q_", cn)]] %||% 0.02)
        P <- sapply(crits, function(cn) input[[paste0("p_", cn)]] %||% 0.10)
        V <- sapply(crits, function(cn) input[[paste0("v_", cn)]] %||% 0.50)
      } else {
        Q <- rep(input$q_val %||% 0.02, length(crits))
        P <- rep(input$p_val %||% 0.10, length(crits))
        V <- rep(input$v_val %||% 0.50, length(crits))
      }
      
      # Perfis
      Bm <- tryCatch(B_current(), error = function(e) NULL)
      
      # Montar texto
      txt <- c(
        "# ========================================",
        "# PARÂMETROS ELECTRE TRI-B",
        "# ========================================",
        "",
        sprintf("## Número de classes: %d", n_classes),
        sprintf("## Número de perfis: %d", n_perfis),
        sprintf("## Número de critérios: %d", length(crits)),
        sprintf("## Lambda (corte): %.2f", input$lambda_cut %||% 0.70),
        sprintf("## Regra: %s", ifelse(input$rule == "pc", "Pessimista (pc)", "Otimista (oc)")),
        "",
        "# ----------------------------------------",
        "# PESOS (W) - soma = 1.0",
        "# ----------------------------------------",
        paste0("W = c(", paste(sprintf("%.3f", W), collapse = ", "), ")"),
        sprintf("names(W) = c(%s)", paste0('"', crits, '"', collapse = ", ")),
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
        txt <- c(txt, sprintf("colnames(B) = c(%s)", paste0('"', crits, '"', collapse = ", ")))
        txt <- c(txt, sprintf("rownames(B) = c(%s)", paste0('"', paste0("b", 1:n_perfis), '"', collapse = ", ")))
      } else {
        txt <- c(txt, "# [Aguardando configuração completa dos perfis]")
      }
      
      txt <- c(txt, "", "# ----------------------------------------", "# CHAMADA DA FUNÇÃO", "# ----------------------------------------")
      txt <- c(txt, "resultado = electre_tri_b_py(")
      txt <- c(txt, "  X,            # matriz de dados normalizados [n_obs x n_criterios]")
      txt <- c(txt, "  W = W,        # vetor de pesos")
      txt <- c(txt, "  Q = Q,        # limiares de indiferença")
      txt <- c(txt, "  P = P,        # limiares de preferência")
      txt <- c(txt, "  V = V,        # limiares de veto")
      txt <- c(txt, "  B = B,        # matriz de perfis")
      txt <- c(txt, sprintf("  cut_level = %.2f,", input$lambda_cut %||% 0.70))
      txt <- c(txt, sprintf("  rule = '%s'", input$rule %||% "pc"))
      txt <- c(txt, ")")
      
      paste(txt, collapse = "\n")
    })
    
    # ====================================================================
    # EXECUTAR ELECTRE TRI-B
    # ====================================================================
    
    resultados_electre <- eventReactive(input$run_electre, {
      
      # Validações
      crits <- criterios()
      req(length(crits) >= 2)
      
      showNotification("Executando ELECTRE Tri-B...", type = "default", duration = NULL, id = "electre_run")
      
      tryCatch({
        df <- data_plain()
        rng <- ranges_real()
        
        # 1. Normalizar dados para [0,1]
        X01 <- do.call(cbind, lapply(crits, function(cn) {
          sense <- input[[paste0("sense_", cn)]] %||% "benefit"
          to_unit(df[[cn]], cn, sense, rng)
        }))
        colnames(X01) <- crits
        
        # 2. Remover NAs
        keep_idx <- which(complete.cases(as.data.frame(X01)))
        validate(need(length(keep_idx) > 0, "Sem linhas completas após remover NAs."))
        
        dataset <- as.matrix(X01[keep_idx, , drop = FALSE])
        df_cleaned <- df[keep_idx, , drop = FALSE]
        
        # 3. Parâmetros
        W <- as.numeric(W_norm()[crits])
        
        # Limiares: globais ou por critério
        if (isTRUE(input$limiares_avancado)) {
          # Modo avançado: coletar por critério
          Q <- sapply(crits, function(cn) {
            input[[paste0("q_", cn)]] %||% 0.02
          })
          P <- sapply(crits, function(cn) {
            input[[paste0("p_", cn)]] %||% 0.10
          })
          V <- sapply(crits, function(cn) {
            input[[paste0("v_", cn)]] %||% 0.50
          })
        } else {
          # Modo simples: repetir valores globais
          Q <- rep(input$q_val %||% 0.02, ncol(dataset))
          P <- rep(input$p_val %||% 0.10, ncol(dataset))
          V <- rep(input$v_val %||% 0.50, ncol(dataset))
        }
        
        Bm <- B_current()
        Bm <- Bm[, crits, drop = FALSE]
        
        n_classes <- input$n_classes %||% 5
        
        # 4. Executar ELECTRE
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
        
        # 5. Labels usando label_map dinâmico
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
    })
    
    # ====================================================================
    # ABA 2: RESULTADOS (DASHBOARD)
    # ====================================================================
    
    # Dados filtrados para a aba resultados
    resultados_filtrados <- reactive({
      req(resultados_electre())
      results <- resultados_electre()$results
      filtros <- filtros_resultados_aplicados()
      aplicar_filtros_em_df(results, filtros)
    })
    
    # ====================================================================
    # SISTEMA DE FILTROS - ABA RESULTADOS (MODAL)
    # ====================================================================
    
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
    
    # ====================================================================
    # VARIÁVEIS CATEGÓRICAS DISPONÍVEIS
    # ====================================================================
    
    # Variáveis categóricas disponíveis
    # Variáveis categóricas disponíveis
    vars_cat_disponiveis <- reactive({
      req(resultados_electre())
      results <- resultados_electre()$results
      vars_cat <- names(results)[sapply(results, function(x) {
        is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(x)) <= 30)
      })]
      
      # Excluir variáveis problemáticas MAS manter class_label para filtro
      vars_excluir <- c("class_electre", "geometry", "CD_MUN", "NM_MUN", "CD_UF")
      vars_filtro <- setdiff(vars_cat, vars_excluir)
      
      # Adicionar class_label no início (classe ELECTRE com nome legível)
      if ("class_label" %in% names(results)) {
        vars_filtro <- c("class_label", vars_filtro)
      }
      
      vars_filtro
    })
    
    # Value Box: Total de municípios
    output$vb_total <- renderText({
      req(resultados_filtrados())
      format(nrow(resultados_filtrados()), big.mark = ".", decimal.mark = ",")
    })
    
    # Value Box: Proporção C4-C5
    output$vb_prop_alto <- renderText({
      req(resultados_filtrados())
      results <- resultados_filtrados()
      n_total <- nrow(results)
      if (n_total == 0) return("0.0%")
      n_c4_c5 <- sum(results$class_electre >= 4, na.rm = TRUE)
      sprintf("%.1f%%", (n_c4_c5 / n_total) * 100)
    })
    
    # Value Box: Classe dominante
    output$vb_dominante <- renderText({
      req(resultados_filtrados())
      results <- resultados_filtrados()
      
      if (nrow(results) == 0) return("N/A")
      
      tbl <- table(results$class_electre)
      classe_dom <- as.integer(names(which.max(tbl)))
      label_map()[as.character(classe_dom)]
    })
    
    # Gráfico: Distribuição das Classes
    output$plot_distribuicao <- renderPlotly({
      req(resultados_filtrados())
      
      results <- resultados_filtrados()
      n_classes <- resultados_electre()$params$n_classes %||% 5
      labels_atuais <- label_map()
      cores_atuais <- paleta_cores()
      
      # Criar dataframe com TODAS as classes, mesmo que vazias
      df_dist <- data.frame(class_electre = 1:n_classes) |>
        left_join(
          results |> count(class_electre),
          by = "class_electre"
        ) |>
        mutate(
          n = replace_na(n, 0),
          class_label = labels_atuais[as.character(class_electre)],
          percent = n / sum(n) * 100
        )
      
      # Garantir que NaN vire 0
      df_dist$percent[is.nan(df_dist$percent)] <- 0
      
      # Cores exatas para cada classe
      cores_graf <- unname(cores_atuais[as.character(1:n_classes)])
      
      if (isTRUE(input$dist_percent)) {
        plot_ly(
          df_dist,
          x = ~class_label,
          y = ~percent,
          type = "bar",
          marker = list(color = cores_graf),
          text = ~sprintf("%.1f%%", percent),
          textposition = "outside"
        ) |>
          layout(
            xaxis = list(title = "Classe"),
            yaxis = list(title = "Percentual (%)"),
            showlegend = FALSE
          )
      } else {
        plot_ly(
          df_dist,
          x = ~class_label,
          y = ~n,
          type = "bar",
          marker = list(color = cores_graf),
          text = ~n,
          textposition = "outside"
        ) |>
          layout(
            xaxis = list(title = "Classe"),
            yaxis = list(title = "Frequência"),
            showlegend = FALSE
          )
      }
    })
    
    # UI dinâmica: Variáveis categóricas disponíveis
    observe({
      req(data_plain())
      df <- data_plain()
      
      # Identificar colunas categóricas (character, factor ou com poucos valores únicos)
      vars_cat <- names(df)[sapply(df, function(x) {
        is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(x)) <= 30)
      })]
      
      # Priorizar UF se existir
      default_var <- if ("UF" %in% vars_cat) "UF" else vars_cat[1]
      
      updateSelectInput(session, "var_categorica", 
                        choices = vars_cat, 
                        selected = default_var)
    })
    
    # Gráfico: Proporção por variável categórica
    output$plot_por_categoria <- renderPlotly({
      req(resultados_filtrados())
      req(input$var_categorica)
      
      results <- resultados_filtrados()
      var_cat <- input$var_categorica
      
      if (!var_cat %in% names(results)) return(NULL)
      
      labels_atuais <- label_map()
      cores_atuais <- paleta_cores()
      
      # Filtrar e contar
      df_cat <- results |>
        filter(!is.na(.data[[var_cat]])) |>
        count(.data[[var_cat]], class_electre) |>
        rename(categoria = 1) |>
        mutate(
          categoria = as.character(categoria),
          class_label = labels_atuais[as.character(class_electre)]
        )
      
      if (nrow(df_cat) == 0) return(NULL)
      
      # Criar named vector de cores
      classes_presentes <- sort(unique(df_cat$class_electre))
      labels_presentes <- labels_atuais[as.character(classes_presentes)]
      cores_map <- setNames(
        unname(cores_atuais[as.character(classes_presentes)]),
        labels_presentes
      )
      
      if (input$tipo_grafico_cat == "stack") {
        plot_ly(
          df_cat,
          x = ~categoria,
          y = ~n,
          color = ~class_label,
          colors = cores_map,
          type = "bar",
          text = ~n,
          textposition = "inside"
        ) |>
          layout(
            barmode = "stack",
            xaxis = list(title = var_cat),
            yaxis = list(title = "Frequência"),
            legend = list(title = list(text = "Classe"))
          )
      } else {
        plot_ly(
          df_cat,
          x = ~categoria,
          y = ~n,
          color = ~class_label,
          colors = cores_map,
          type = "bar",
          text = ~n,
          textposition = "outside"
        ) |>
          layout(
            barmode = "group",
            xaxis = list(title = var_cat),
            yaxis = list(title = "Frequência"),
            legend = list(title = list(text = "Classe"))
          )
      }
    })
    
    # UI dinâmica: Variáveis numéricas para densidade
    observe({
      req(criterios())
      crits <- criterios()
      updateSelectInput(session, "var_densidade", 
                        choices = crits, 
                        selected = crits[1])
    })
    
    # Gráfico: Densidade por classe
    output$plot_densidade <- renderPlotly({
      req(resultados_filtrados())
      req(input$var_densidade)
      
      results <- resultados_filtrados()
      var_dens <- input$var_densidade
      n_classes <- resultados_electre()$params$n_classes %||% 5
      labels_atuais <- label_map()
      cores_atuais <- paleta_cores()
      
      if (!var_dens %in% names(results)) return(NULL)
      
      # Criar gráfico de densidade para cada classe
      p <- plot_ly()
      
      for (i in 1:n_classes) {
        df_classe <- results |>
          filter(class_electre == i, !is.na(.data[[var_dens]]))
        
        if (nrow(df_classe) > 1) {  # Precisa de pelo menos 2 pontos
          dens <- density(df_classe[[var_dens]], na.rm = TRUE)
          
          p <- p |>
            add_trace(
              x = dens$x,
              y = dens$y,
              type = "scatter",
              mode = "lines",
              name = labels_atuais[as.character(i)],
              line = list(color = unname(cores_atuais[as.character(i)]), width = 2),
              fill = "tozeroy",
              fillcolor = paste0(unname(cores_atuais[as.character(i)]), "40")
            )
        }
      }
      
      p |>
        layout(
          xaxis = list(title = var_dens),
          yaxis = list(title = "Densidade"),
          legend = list(title = list(text = "Classe"))
        )
    })
    
    # Tabela: Perfil médio por classe
    output$tab_perfil_medio <- renderDT({
      req(resultados_filtrados())
      
      results <- resultados_filtrados()
      crits <- resultados_electre()$params$criterios
      n_classes <- resultados_electre()$params$n_classes %||% 5
      labels_atuais <- label_map()
      cores_atuais <- paleta_cores()
      
      # Calcular médias por classe para cada critério
      df_perfil <- results |>
        group_by(class_electre) |>
        summarise(
          across(all_of(crits), ~mean(.x, na.rm = TRUE)),
          n = n(),
          .groups = "drop"
        ) |>
        mutate(
          Classe = labels_atuais[as.character(class_electre)],
          N = n
        ) |>
        select(Classe, N, all_of(crits))
      
      datatable(
        df_perfil,
        rownames = FALSE,
        options = list(
          pageLength = n_classes,
          dom = "t",
          ordering = FALSE
        )
      ) |>
        formatRound(columns = crits, digits = 2) |>
        formatStyle(
          "Classe",
          backgroundColor = styleEqual(
            labels_atuais[1:n_classes],
            cores_atuais[1:n_classes]
          ),
          fontWeight = "bold",
          color = "white"
        )
    })
    
    # ====================================================================
    # ABA 3: TABELA DE RESULTADOS - SISTEMA MODAL
    # ====================================================================
    
    # CORREÇÃO: Implementar sistema de filtros modal para tabela
    filtros_tabela_aplicados <- reactiveVal(list())
    
    criar_sistema_filtros_modal(
      session = session,
      ns = ns,
      id = "tabela",
      results_reactive = reactive({
        req(resultados_electre())
        resultados_electre()$results
      }),
      filtros_aplicados = filtros_tabela_aplicados
    )
    
    # Dados filtrados para a tabela
    tabela_filtrada <- reactive({
      req(resultados_electre())
      results <- resultados_electre()$results
      filtros <- filtros_tabela_aplicados()
      aplicar_filtros_em_df(results, filtros)
    })
    
    # Tabela completa
    output$tab_completa <- renderDT({
      req(tabela_filtrada())
      
      df <- tabela_filtrada()
      n_classes <- resultados_electre()$params$n_classes %||% 5
      cores_hex <- paleta_cores()[1:n_classes]
      
      datatable(
        df,
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = list(),
          order = list(list(which(names(df) == "class_electre") - 1, "asc")),
          language = list(
            search = "Buscar:",
            lengthMenu = "Mostrar _MENU_ registros",
            info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
            paginate = list(
              first = "Primeiro",
              last = "Último",
              `next` = "Próximo",
              previous = "Anterior"
            )
          )
        )
      ) |>
        formatStyle(
          "class_electre",
          backgroundColor = styleEqual(1:n_classes, cores_hex),
          fontWeight = "bold",
          color = "white"
        )
    })
    
    # Download CSV
    output$dl_tabela_csv <- downloadHandler(
      filename = function() paste0("electre_tabela_", Sys.Date(), ".csv"),
      content = function(file) {
        req(tabela_filtrada())
        readr::write_csv(tabela_filtrada(), file)
        showNotification("CSV exportado com sucesso!", type = "message", duration = 3)
      }
    )
    
    # Download Excel
    output$dl_tabela_excel <- downloadHandler(
      filename = function() paste0("electre_tabela_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(tabela_filtrada())
        
        if (!requireNamespace("writexl", quietly = TRUE)) {
          showNotification("Pacote 'writexl' não instalado. Usando CSV.", type = "warning", duration = 5)
          readr::write_csv(tabela_filtrada(), file)
        } else {
          writexl::write_xlsx(tabela_filtrada(), file)
          showNotification("Excel exportado com sucesso!", type = "message", duration = 3)
        }
      }
    )
    
    # ====================================================================
    # ABA 4: MAPA
    # ====================================================================
    
    # UI dinâmica: Filtros do mapa
    # ====================================================================
    # ABA 4: MAPA - FILTROS DINÂMICOS (MODAL)
    # ====================================================================
    
    filtros_mapa_aplicados <- reactiveVal(list())
    
    criar_sistema_filtros_modal(
      session = session,
      ns = ns,
      id = "mapa",
      results_reactive = reactive({
        req(resultados_electre())
        resultados_electre()$results
      }),
      filtros_aplicados = filtros_mapa_aplicados
    )
    
    # Atualizar opções de busca de município
    observe({
      req(resultados_electre())
      req(data_sf())
      
      sf_data <- data_sf()
      
      if ("NM_MUN" %in% names(sf_data)) {
        municipios <- sort(unique(sf_data$NM_MUN))
        updateSelectizeInput(session, "busca_municipio", 
                             choices = municipios, 
                             server = TRUE)
      }
    })
    
    # Dados filtrados para o mapa
    mapa_filtrado <- reactive({
      req(resultados_electre())
      req(data_sf())
      
      results <- resultados_electre()$results
      sf_data <- data_sf()
      filtros <- filtros_mapa_aplicados()
      
      # Aplicar filtros
      results <- aplicar_filtros_em_df(results, filtros)
      
      # Join com geometrias
      if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
        sf_results <- sf_data |>
          inner_join(
            results |> select(CD_MUN, class_electre, class_label),
            by = "CD_MUN"
          )
      } else {
        sf_results <- sf_data
        if (nrow(results) == nrow(sf_data)) {
          sf_results$class_electre <- results$class_electre
          sf_results$class_label <- results$class_label
        }
      }
      
      sf_results
    })
    
    # Mapa leaflet
    output$mapa_classes <- renderLeaflet({
      req(mapa_filtrado())
      
      sf_results <- mapa_filtrado()
      n_classes <- resultados_electre()$params$n_classes %||% 5
      labels_atuais <- label_map()
      cores_atuais <- paleta_cores()
      
      # Cores para todas as classes
      cores_hex <- unname(cores_atuais[as.character(1:n_classes)])
      
      # Paleta de cores
      pal <- colorFactor(
        palette = cores_hex,
        domain = 1:n_classes,
        na.color = "#cccccc"
      )
      
      # Labels
      labels <- sprintf(
        "<strong>%s</strong><br/>Classe: %s",
        sf_results$NM_MUN %||% "Município",
        sf_results$class_label
      ) |>
        lapply(htmltools::HTML)
      
      # Simplificar geometrias
      sf_simple <- tryCatch(
        rmapshaper::ms_simplify(sf_results, keep = 0.05),
        error = function(e) sf_results
      )
      
      # Criar mapa
      leaflet(sf_simple) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(
          fillColor = ~pal(class_electre),
          fillOpacity = 0.75,
          color = "#FFFFFF",
          weight = 0.3,
          opacity = 1,
          highlight = highlightOptions(
            weight = 2,
            color = "#000",
            fillOpacity = 0.95,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~class_electre,
          title = "Classe ELECTRE",
          opacity = 0.9,
          labFormat = labelFormat(
            transform = function(x) labels_atuais[as.character(x)]
          )
        )
    })
    
    
    # Download handler para exportar mapa principal em HTML
    output$export_mapa_principal <- downloadHandler(
      filename = function() {
        paste0("mapa_classificacao_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        req(mapa_filtrado())
        
        sf_results <- mapa_filtrado()
        n_classes <- resultados_electre()$params$n_classes %||% 5
        labels_atuais <- label_map()
        cores_atuais <- paleta_cores()
        cores_hex <- unname(cores_atuais[as.character(1:n_classes)])
        
        pal <- colorFactor(
          palette = cores_hex,
          domain = 1:n_classes,
          na.color = "#cccccc"
        )
        
        labels <- sprintf(
          "<strong>%s</strong><br/>Classe: %s",
          sf_results$NM_MUN %||% "Município",
          sf_results$class_label
        ) |>
          lapply(htmltools::HTML)
        
        sf_simple <- tryCatch(
          rmapshaper::ms_simplify(sf_results, keep = 0.05),
          error = function(e) sf_results
        )
        
        m <- leaflet(sf_simple) |>
          addProviderTiles(providers$CartoDB.Positron) |>
          addPolygons(
            fillColor = ~pal(class_electre),
            fillOpacity = 0.75,
            color = "#FFFFFF",
            weight = 0.3,
            opacity = 1,
            highlight = highlightOptions(
              weight = 2,
              color = "#000",
              fillOpacity = 0.95,
              bringToFront = TRUE
            ),
            label = labels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
          ) |>
          addLegend(
            position = "bottomright",
            pal = pal,
            values = ~class_electre,
            title = "Classe ELECTRE",
            opacity = 0.9,
            labFormat = labelFormat(
              transform = function(x) labels_atuais[as.character(x)]
            )
          )
        
        htmlwidgets::saveWidget(m, file, selfcontained = TRUE)
      }
    )
    
    # Zoom para município selecionado
    # Zoom para município selecionado (via botão)
    observeEvent(input$btn_zoom_municipio, {
      req(input$busca_municipio)
      req(data_sf())
      
      sf_data <- data_sf()
      
      if ("NM_MUN" %in% names(sf_data)) {
        municipio_sel <- sf_data |>
          filter(NM_MUN == input$busca_municipio)
        
        if (nrow(municipio_sel) > 0) {
          bbox <- st_bbox(municipio_sel)
          
          # Converter para numérico sem nomes (corrige aviso jsonlite)
          leafletProxy(ns("mapa_classes")) |>
            flyToBounds(
              lng1 = as.numeric(bbox["xmin"]),
              lat1 = as.numeric(bbox["ymin"]),
              lng2 = as.numeric(bbox["xmax"]),
              lat2 = as.numeric(bbox["ymax"])
            )
          
          showNotification(
            paste0("Zoom aplicado: ", input$busca_municipio),
            type = "message",
            duration = 2
          )
        } else {
          showNotification(
            "Município não encontrado no mapa",
            type = "warning",
            duration = 3
          )
        }
      }
    })
    
    # Zoom automático quando filtros mudarem
    observe({
      req(mapa_filtrado())
      
      # Trigger: filtros aplicados mudaram
      filtros_mapa_aplicados()
      
      sf_filtrado <- mapa_filtrado()
      
      if (nrow(sf_filtrado) > 0) {
        bbox <- st_bbox(sf_filtrado)
        
        # Converter para numérico sem nomes (corrige aviso jsonlite)
        leafletProxy(ns("mapa_classes")) |>
          flyToBounds(
            lng1 = as.numeric(bbox["xmin"]),
            lat1 = as.numeric(bbox["ymin"]),
            lng2 = as.numeric(bbox["xmax"]),
            lat2 = as.numeric(bbox["ymax"])
          )
      }
    })
    
    # ====================================================================
    # DOWNLOADS
    # ====================================================================
    
    output$dl_resultados_csv <- downloadHandler(
      filename = function() paste0("electre_resultados_", Sys.Date(), ".csv"),
      content = function(file) {
        req(resultados_electre())
        readr::write_csv(resultados_electre()$results, file)
        showNotification("CSV exportado com sucesso!", type = "message", duration = 3)
      }
    )
    
    output$dl_resultados_gpkg <- downloadHandler(
      filename = function() paste0("electre_resultados_", Sys.Date(), ".gpkg"),
      content = function(file) {
        req(resultados_electre())
        req(data_sf())
        
        results <- resultados_electre()$results
        sf_data <- data_sf()
        
        if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
          sf_results <- sf_data |>
            left_join(results, by = "CD_MUN")
        } else {
          sf_results <- sf_data
          sf_results <- cbind(sf_results, results)
        }
        
        sf::st_write(sf_results, dsn = file, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
        showNotification("GeoPackage exportado com sucesso!", type = "message", duration = 3)
      }
    )
    
    output$dl_mapa_png <- downloadHandler(
      filename = function() paste0("electre_mapa_", Sys.Date(), ".png"),
      content = function(file) {
        req(resultados_electre())
        req(data_sf())
        
        showNotification("Gerando mapa PNG...", type = "default", duration = NULL, id = "png_export")
        
        tryCatch({
          if (!requireNamespace("mapview", quietly = TRUE)) {
            stop("O pacote 'mapview' não está instalado. Instale com: install.packages('mapview')")
          }
          
          results <- resultados_electre()$results
          sf_data <- data_sf()
          n_classes <- resultados_electre()$params$n_classes %||% 5
          cores_hex <- paleta_cores()[1:n_classes]
          
          if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
            sf_results <- sf_data |>
              left_join(
                results |> select(CD_MUN, class_electre, class_label),
                by = "CD_MUN"
              )
          } else {
            sf_results <- sf_data
            sf_results$class_electre <- results$class_electre
            sf_results$class_label <- results$class_label
          }
          
          m <- mapview::mapview(
            sf_results,
            zcol = "class_electre",
            col.regions = cores_hex,
            at = seq(0.5, n_classes + 0.5, by = 1),
            layer.name = "Classe ELECTRE",
            legend = TRUE,
            map.types = "CartoDB.Positron"
          )
          
          mapview::mapshot(
            m,
            file = file,
            remove_controls = c("zoomControl", "layersControl", "homeButton"),
            vwidth = 1200,
            vheight = 800
          )
          
          removeNotification("png_export")
          showNotification("Mapa PNG exportado com sucesso!", type = "message", duration = 3)
          
        }, error = function(e) {
          removeNotification("png_export")
          
          tryCatch({
            library(ggplot2)
            results <- resultados_electre()$results
            sf_data <- data_sf()
            n_classes <- resultados_electre()$params$n_classes %||% 5
            labels_atuais <- label_map()
            cores_hex <- paleta_cores()[1:n_classes]
            
            if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
              sf_results <- sf_data |> left_join(results, by = "CD_MUN")
            } else {
              sf_results <- sf_data
              sf_results$class_electre <- results$class_electre
            }
            
            p <- ggplot(sf_results) +
              geom_sf(aes(fill = factor(class_electre)), color = "white", size = 0.1) +
              scale_fill_manual(
                values = setNames(cores_hex, 1:n_classes),
                labels = labels_atuais[1:n_classes],
                name = "Classe ELECTRE"
              ) +
              theme_minimal() +
              theme(
                legend.position = "right",
                panel.grid = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank()
              ) +
              labs(title = "Classificação ELECTRE Tri-B")
            
            ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
            
            removeNotification("png_export")
            showNotification("Mapa exportado com método alternativo!", type = "message", duration = 3)
            
          }, error = function(e2) {
            removeNotification("png_export")
            showNotification(
              paste("Erro ao exportar mapa:", e2$message),
              type = "error",
              duration = 10
            )
          })
        })
      }
    )
    
    # ====================================================================
    # ABA 5: QUALIFICAÇÃO - ANÁLISE TERRITORIAL
    # ====================================================================
    
    # Carregar dados espaciais (lista com 6 objetos sf)
    dados_espaciais <- reactive({
      req(file.exists("data/dados_espaciais_para_shiny.qs"))
      
      tryCatch({
        lista_sf <- qs::qread("data/dados_espaciais_para_shiny.qs")
        
        # Estrutura: lista_sf[[1]] = quilombolas, [[2]] = assentamentos, 
        #            [[3]] = indigenas, [[4]] = ensino, [[5]] = prisoes, [[6]] = sementes
        list(
          quilombolas = lista_sf[[1]],
          assentamentos = lista_sf[[2]],
          indigenas = lista_sf[[3]],
          ensino = lista_sf[[4]],
          prisoes = lista_sf[[5]],
          sementes = lista_sf[[6]]
        )
      }, error = function(e) {
        showNotification(
          paste("Erro ao carregar dados espaciais:", e$message),
          type = "error",
          duration = 10
        )
        return(NULL)
      })
    })
    
    # Resultado ELECTRE como sf
    resultado_electre_sf <- reactive({
      req(resultados_electre())
      req(data_sf())
      
      results <- resultados_electre()$results
      sf_data <- data_sf()
      
      if ("CD_MUN" %in% names(results) && "CD_MUN" %in% names(sf_data)) {
        sf_results <- sf_data |>
          left_join(
            results |> select(CD_MUN, class_electre, class_label),
            by = "CD_MUN"
          )
      } else {
        sf_results <- sf_data
        if (nrow(results) == nrow(sf_data)) {
          sf_results$class_electre <- results$class_electre
          sf_results$class_label <- results$class_label
        }
      }
      
      sf_results
    })
    
    # ====================================================================
    # SISTEMA DE FILTROS - ABA QUALIFICAÇÃO (MODAL)
    # ====================================================================
    
    filtros_qualificacao_aplicados <- reactiveVal(list())
    
    criar_sistema_filtros_modal(
      session = session,
      ns = ns,
      id = "qualificacao",
      results_reactive = reactive({
        req(resultado_electre_sf())
        resultado_electre_sf()
      }),
      filtros_aplicados = filtros_qualificacao_aplicados
    )
    
    # Resultado filtrado usando sistema de filtros modal
    resultado_qualif_filtrado <- reactive({
      req(resultado_electre_sf())
      
      sf_result <- resultado_electre_sf()
      filtros <- filtros_qualificacao_aplicados()
      
      aplicar_filtros_em_df(sf_result, filtros)
    })
    
    # Interseções espaciais com cada camada
    # Interseções espaciais com cada camada
    intersecoes <- reactive({
      req(dados_espaciais())
      req(resultado_qualif_filtrado())
      
      dados <- dados_espaciais()
      resultado_sf <- resultado_qualif_filtrado()
      
      # Transformar CRS se necessário
      resultado_sf <- st_transform(resultado_sf, 4326)
      
      # Lista para armazenar resultados
      inter_list <- list()
      
      withProgress(message = 'Processando interseções espaciais...', value = 0, {
        
        # Quilombolas - com validação
        if (!is.null(dados$quilombolas)) {
          incProgress(1/6, detail = "Quilombolas...")
          inter_list$quilombolas <- tryCatch({
            quilombolas_valido <- st_make_valid(dados$quilombolas)
            quilombolas_4326 <- st_transform(quilombolas_valido, 4326)
            quilombo_inter <- st_join(quilombolas_4326, resultado_sf, 
                                      join = st_intersects, left = FALSE) |>
              mutate(tipo = "Quilombola", nome_feature = nm_comunid)
            quilombo_inter
          }, error = function(e) {
            message("Erro quilombolas: ", e$message)
            NULL
          })
        }
        
        # Assentamentos - com validação
        if (!is.null(dados$assentamentos)) {
          incProgress(1/6, detail = "Assentamentos...")
          inter_list$assentamentos <- tryCatch({
            assentamentos_valido <- st_make_valid(dados$assentamentos)
            assentamentos_4326 <- st_transform(assentamentos_valido, 4326)
            assent_inter <- st_join(assentamentos_4326, resultado_sf, 
                                    join = st_intersects, left = FALSE) |>
              mutate(tipo = "Assentamento", nome_feature = nome_proje)
            assent_inter
          }, error = function(e) {
            message("Erro assentamentos: ", e$message)
            NULL
          })
        }
        
        # Territórios Indígenas - com validação
        if (!is.null(dados$indigenas)) {
          incProgress(1/6, detail = "Territórios Indígenas...")
          inter_list$indigenas <- tryCatch({
            indigenas_valido <- st_make_valid(dados$indigenas)
            indigenas_4326 <- st_transform(indigenas_valido, 4326)
            indigena_inter <- st_join(indigenas_4326, resultado_sf, 
                                      join = st_intersects, left = FALSE) |>
              mutate(tipo = "Território Indígena", nome_feature = terrai_nom)
            indigena_inter
          }, error = function(e) {
            message("Erro indígenas: ", e$message)
            NULL
          })
        }
        
        # Instituições de Ensino
        if (!is.null(dados$ensino)) {
          incProgress(1/6, detail = "Instituições de Ensino...")
          inter_list$ensino <- tryCatch({
            suppressWarnings({
              ensino_pontos <- st_centroid(dados$ensino)
            })
            ensino_4326 <- st_transform(ensino_pontos, 4326)
            ensino_inter <- st_join(ensino_4326, resultado_sf, 
                                    join = st_intersects, left = FALSE) |>
              mutate(tipo = "Instituição de Ensino", nome_feature = Nome)
            ensino_inter
          }, error = function(e) {
            message("Erro ensino: ", e$message)
            NULL
          })
        }
        
        # Unidades Prisionais
        if (!is.null(dados$prisoes)) {
          incProgress(1/6, detail = "Unidades Prisionais...")
          inter_list$prisoes <- tryCatch({
            prisoes_4326 <- st_transform(dados$prisoes, 4326)
            prisao_inter <- st_join(prisoes_4326, resultado_sf, 
                                    join = st_intersects, left = FALSE) |>
              mutate(tipo = "Unidade Prisional", nome_feature = Nome)
            prisao_inter
          }, error = function(e) {
            message("Erro prisões: ", e$message)
            NULL
          })
        }
        
        # Bancos de Sementes
        if (!is.null(dados$sementes)) {
          incProgress(1/6, detail = "Bancos de Sementes...")
          inter_list$sementes <- tryCatch({
            # Transformar para mesmo CRS do resultado_sf (4326)
            sementes_4326 <- st_transform(dados$sementes, 4326)
            
            sementes_inter <- st_join(sementes_4326, resultado_sf, 
                                      join = st_intersects, left = FALSE) |>
              mutate(tipo = "Banco de Sementes", 
                     nome_feature = if("Nome" %in% names(sementes_4326)) Nome else "Banco de Sementes")
            sementes_inter
          }, error = function(e) {
            message("Erro sementes: ", e$message)
            NULL
          })
        }
        
      })
      
      inter_list
    })
    
    # Value boxes
    output$vb_quilombolas <- renderText({
      inter <- intersecoes()
      if (!is.null(inter$quilombolas) && nrow(inter$quilombolas) > 0) {
        formatC(nrow(inter$quilombolas), format = "d", big.mark = ".")
      } else {
        "0"
      }
    })
    
    output$vb_assentamentos <- renderText({
      inter <- intersecoes()
      if (!is.null(inter$assentamentos) && nrow(inter$assentamentos) > 0) {
        formatC(nrow(inter$assentamentos), format = "d", big.mark = ".")
      } else {
        "0"
      }
    })
    
    output$vb_indigenas <- renderText({
      inter <- intersecoes()
      if (!is.null(inter$indigenas) && nrow(inter$indigenas) > 0) {
        formatC(nrow(inter$indigenas), format = "d", big.mark = ".")
      } else {
        "0"
      }
    })
    
    output$vb_ensino <- renderText({
      inter <- intersecoes()
      if (!is.null(inter$ensino) && nrow(inter$ensino) > 0) {
        formatC(nrow(inter$ensino), format = "d", big.mark = ".")
      } else {
        "0"
      }
    })
    
    output$vb_prisoes <- renderText({
      inter <- intersecoes()
      if (!is.null(inter$prisoes) && nrow(inter$prisoes) > 0) {
        formatC(nrow(inter$prisoes), format = "d", big.mark = ".")
      } else {
        "0"
      }
    })
    
    output$vb_sementes <- renderText({
      inter <- intersecoes()
      if (!is.null(inter$sementes) && nrow(inter$sementes) > 0) {
        formatC(nrow(inter$sementes), format = "d", big.mark = ".")
      } else {
        "0"
      }
    })
    
    # Gráfico: Distribuição por classe ELECTRE
    output$plot_qualif_classes <- renderPlotly({
      req(intersecoes())
      
      inter <- intersecoes()
      n_classes <- resultados_electre()$params$n_classes %||% 5
      labels_atuais <- label_map()
      cores_atuais <- paleta_cores()
      
      # Combinar todas as interseções
      df_list <- list()
      
      if (!is.null(inter$quilombolas) && nrow(inter$quilombolas) > 0) {
        df_list$quilombolas <- inter$quilombolas |> 
          st_drop_geometry() |>
          select(class_electre, tipo) |>
          count(class_electre, tipo)
      }
      
      if (!is.null(inter$assentamentos) && nrow(inter$assentamentos) > 0) {
        df_list$assentamentos <- inter$assentamentos |>
          st_drop_geometry() |>
          select(class_electre, tipo) |>
          count(class_electre, tipo)
      }
      
      if (!is.null(inter$indigenas) && nrow(inter$indigenas) > 0) {
        df_list$indigenas <- inter$indigenas |>
          st_drop_geometry() |>
          select(class_electre, tipo) |>
          count(class_electre, tipo)
      }
      
      if (!is.null(inter$ensino) && nrow(inter$ensino) > 0) {
        df_list$ensino <- inter$ensino |>
          st_drop_geometry() |>
          select(class_electre, tipo) |>
          count(class_electre, tipo)
      }
      
      if (!is.null(inter$prisoes) && nrow(inter$prisoes) > 0) {
        df_list$prisoes <- inter$prisoes |>
          st_drop_geometry() |>
          select(class_electre, tipo) |>
          count(class_electre, tipo)
      }
      
      if (!is.null(inter$sementes) && nrow(inter$sementes) > 0) {
        df_list$sementes <- inter$sementes |>
          st_drop_geometry() |>
          select(class_electre, tipo) |>
          count(class_electre, tipo)
      }
      
      if (length(df_list) == 0) {
        return(NULL)
      }
      
      df_combined <- bind_rows(df_list) |>
        mutate(class_label = labels_atuais[as.character(class_electre)])
      
      plot_ly(
        df_combined,
        x = ~class_label,
        y = ~n,
        color = ~tipo,
        type = "bar",
        text = ~n,
        textposition = "outside"
      ) |>
        layout(
          barmode = "group",
          xaxis = list(title = "Classe ELECTRE"),
          yaxis = list(title = "Quantidade"),
          legend = list(title = list(text = "Tipo"))
        )
    })
    
    # Gráfico: Total de cada camada
    output$plot_qualif_total <- renderPlotly({
      req(intersecoes())
      
      inter <- intersecoes()
      
      totais <- data.frame(
        Camada = character(),
        Total = integer(),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(inter$quilombolas)) {
        totais <- rbind(totais, data.frame(Camada = "Quilombolas", Total = nrow(inter$quilombolas)))
      }
      
      if (!is.null(inter$assentamentos)) {
        totais <- rbind(totais, data.frame(Camada = "Assentamentos", Total = nrow(inter$assentamentos)))
      }
      
      if (!is.null(inter$indigenas)) {
        totais <- rbind(totais, data.frame(Camada = "T. Indígenas", Total = nrow(inter$indigenas)))
      }
      
      if (!is.null(inter$ensino)) {
        totais <- rbind(totais, data.frame(Camada = "Inst. Ensino", Total = nrow(inter$ensino)))
      }
      
      if (!is.null(inter$prisoes)) {
        totais <- rbind(totais, data.frame(Camada = "U. Prisionais", Total = nrow(inter$prisoes)))
      }
      
      if (!is.null(inter$sementes)) {
        totais <- rbind(totais, data.frame(Camada = "B. Sementes", Total = nrow(inter$sementes)))
      }
      
      if (nrow(totais) == 0) {
        return(NULL)
      }
      
      cores_camadas <- c(
        "Quilombolas" = "#f39c12",
        "Assentamentos" = "#27ae60",
        "T. Indígenas" = "#3498db",
        "Inst. Ensino" = "#9b59b6",
        "U. Prisionais" = "#e74c3c",
        "B. Sementes" = "#16a085"
      )
      
      plot_ly(
        totais,
        labels = ~Camada,
        values = ~Total,
        type = "pie",
        marker = list(colors = cores_camadas[totais$Camada]),
        textposition = "inside",
        textinfo = "label+value+percent"
      ) |>
        layout(
          showlegend = TRUE,
          legend = list(orientation = "v")
        )
    })
    
    # Tabela: Estatísticas por classe
    output$tab_estatisticas_qualif <- renderDT({
      req(intersecoes())
      
      inter <- intersecoes()
      n_classes <- resultados_electre()$params$n_classes %||% 5
      labels_atuais <- label_map()
      cores_atuais <- paleta_cores()
      
      # Criar dataframe de estatísticas
      stats_list <- list()
      
      for (i in 1:n_classes) {
        stats <- data.frame(
          Classe = labels_atuais[as.character(i)],
          Quilombolas = 0,
          Assentamentos = 0,
          `T. Indígenas` = 0,
          `Inst. Ensino` = 0,
          `U. Prisionais` = 0,
          `B. Sementes` = 0,
          check.names = FALSE
        )
        
        if (!is.null(inter$quilombolas)) {
          stats$Quilombolas <- sum(st_drop_geometry(inter$quilombolas)$class_electre == i, na.rm = TRUE)
        }
        
        if (!is.null(inter$assentamentos)) {
          stats$Assentamentos <- sum(st_drop_geometry(inter$assentamentos)$class_electre == i, na.rm = TRUE)
        }
        
        if (!is.null(inter$indigenas)) {
          stats$`T. Indígenas` <- sum(st_drop_geometry(inter$indigenas)$class_electre == i, na.rm = TRUE)
        }
        
        if (!is.null(inter$ensino)) {
          stats$`Inst. Ensino` <- sum(st_drop_geometry(inter$ensino)$class_electre == i, na.rm = TRUE)
        }
        
        if (!is.null(inter$prisoes)) {
          stats$`U. Prisionais` <- sum(st_drop_geometry(inter$prisoes)$class_electre == i, na.rm = TRUE)
        }
        
        if (!is.null(inter$sementes)) {
          stats$`B. Sementes` <- sum(st_drop_geometry(inter$sementes)$class_electre == i, na.rm = TRUE)
        }
        
        stats$Total <- rowSums(stats[, -1])
        stats_list[[i]] <- stats
      }
      
      df_stats <- bind_rows(stats_list)
      cores_hex <- unname(cores_atuais[as.character(1:n_classes)])
      
      datatable(
        df_stats,
        rownames = FALSE,
        options = list(
          pageLength = n_classes,
          dom = "t",
          ordering = FALSE
        )
      ) |>
        formatStyle(
          "Classe",
          backgroundColor = styleEqual(labels_atuais[1:n_classes], cores_hex),
          fontWeight = "bold",
          color = "white"
        )
    })
    
    
    # Mapa de qualificação
    
    # Tabela: Ranking de Municípios por Interseções
    output$tab_ranking_municipios <- renderDT({
      req(intersecoes())
      req(resultado_qualif_filtrado())
      
      inter <- intersecoes()
      resultado_sf <- resultado_qualif_filtrado()
      
      # Calcular contagem de interseções por município
      contagens_municipio <- data.frame(
        CD_MUN = character(),
        NM_MUN = character(),
        Quilombolas = integer(),
        Assentamentos = integer(),
        T_Indigenas = integer(),
        Inst_Ensino = integer(),
        U_Prisionais = integer(),
        B_Sementes = integer(),
        Total = integer(),
        stringsAsFactors = FALSE
      )
      
      # Iterar sobre todos os municípios
      for (i in 1:nrow(resultado_sf)) {
        cd_mun <- resultado_sf$CD_MUN[i]
        nm_mun <- resultado_sf$NM_MUN[i]
        
        counts <- c(0, 0, 0, 0, 0, 0)
        
        # Contar quilombolas
        if (!is.null(inter$quilombolas) && nrow(inter$quilombolas) > 0) {
          df_quilombolas <- st_drop_geometry(inter$quilombolas)
          if ("CD_MUN" %in% names(df_quilombolas)) {
            counts[1] <- sum(df_quilombolas$CD_MUN == cd_mun, na.rm = TRUE)
          }
        }
        
        # Contar assentamentos
        if (!is.null(inter$assentamentos) && nrow(inter$assentamentos) > 0) {
          df_assentamentos <- st_drop_geometry(inter$assentamentos)
          if ("CD_MUN" %in% names(df_assentamentos)) {
            counts[2] <- sum(df_assentamentos$CD_MUN == cd_mun, na.rm = TRUE)
          }
        }
        
        # Contar indígenas
        if (!is.null(inter$indigenas) && nrow(inter$indigenas) > 0) {
          df_indigenas <- st_drop_geometry(inter$indigenas)
          if ("CD_MUN" %in% names(df_indigenas)) {
            counts[3] <- sum(df_indigenas$CD_MUN == cd_mun, na.rm = TRUE)
          }
        }
        
        # Contar ensino
        if (!is.null(inter$ensino) && nrow(inter$ensino) > 0) {
          df_ensino <- st_drop_geometry(inter$ensino)
          if ("CD_MUN" %in% names(df_ensino)) {
            counts[4] <- sum(df_ensino$CD_MUN == cd_mun, na.rm = TRUE)
          }
        }
        
        # Contar prisões
        if (!is.null(inter$prisoes) && nrow(inter$prisoes) > 0) {
          df_prisoes <- st_drop_geometry(inter$prisoes)
          if ("CD_MUN" %in% names(df_prisoes)) {
            counts[5] <- sum(df_prisoes$CD_MUN == cd_mun, na.rm = TRUE)
          }
        }
        
        # Contar sementes
        if (!is.null(inter$sementes) && nrow(inter$sementes) > 0) {
          df_sementes <- st_drop_geometry(inter$sementes)
          if ("CD_MUN" %in% names(df_sementes)) {
            counts[6] <- sum(df_sementes$CD_MUN == cd_mun, na.rm = TRUE)
          }
        }
        
        total <- sum(counts)
        
        # Adicionar apenas se tiver alguma interseção
        if (total > 0) {
          contagens_municipio <- rbind(
            contagens_municipio,
            data.frame(
              CD_MUN = cd_mun,
              NM_MUN = nm_mun,
              Quilombolas = counts[1],
              Assentamentos = counts[2],
              T_Indigenas = counts[3],
              Inst_Ensino = counts[4],
              U_Prisionais = counts[5],
              B_Sementes = counts[6],
              Total = total,
              stringsAsFactors = FALSE
            )
          )
        }
      }
      
      # Ordenar por total decrescente
      contagens_municipio <- contagens_municipio[order(-contagens_municipio$Total), ]
      
      # Adicionar coluna de ranking
      contagens_municipio <- cbind(
        Ranking = 1:nrow(contagens_municipio),
        contagens_municipio
      )
      
      # Remover CD_MUN da exibição
      contagens_municipio <- contagens_municipio[, -which(names(contagens_municipio) == "CD_MUN")]
      
      # Renomear colunas para melhor apresentação
      names(contagens_municipio) <- c("#", "Município", "Quilombolas", "Assentamentos", 
                                      "T. Indígenas", "Inst. Ensino", "U. Prisionais", "B. Sementes", "Total")
      
      datatable(
        contagens_municipio,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          order = list(list(0, 'asc')),
          columnDefs = list(
            list(className = 'dt-center', targets = 0:8)
          )
        )
      ) |>
        formatStyle(
          "Total",
          backgroundColor = styleInterval(
            c(5, 10, 20),
            c("#ffffff", "#fff3cd", "#ffc107", "#ff9800")
          ),
          fontWeight = "bold"
        ) |>
        formatStyle(
          "#",
          backgroundColor = styleInterval(
            c(1, 2, 3),
            c("#ffd700", "#c0c0c0", "#cd7f32", "#e8e8e8")
          ),
          fontWeight = "bold"
        )
    })
    
    output$mapa_qualificacao <- renderLeaflet({
      req(resultado_qualif_filtrado())
      req(intersecoes())
      
      resultado_sf <- resultado_qualif_filtrado()
      inter <- intersecoes()
      n_classes <- resultados_electre()$params$n_classes %||% 5
      labels_atuais <- label_map()
      cores_atuais <- paleta_cores()
      cores_hex <- unname(cores_atuais[as.character(1:n_classes)])
      
      pal_mun <- colorFactor(
        palette = cores_hex,
        domain = 1:n_classes,
        na.color = "transparent"
      )
      
      m <- leaflet() |>
        addProviderTiles(providers$CartoDB.Positron)
      
      # Adicionar municípios ao fundo
      m <- m |>
        addPolygons(
          data = resultado_sf,
          fillColor = ~pal_mun(class_electre),
          fillOpacity = 0.3,
          color = "#999999",
          weight = 0.5,
          group = "Municípios",
          label = ~paste0(NM_MUN, " - ", class_label)
        )
      
      opacidade <- 0.7
      
      if (!is.null(inter$quilombolas) && nrow(inter$quilombolas) > 0) {
        m <- m |>
          addPolygons(
            data = inter$quilombolas,
            fillColor = "#f39c12",
            fillOpacity = opacidade,
            color = "#d68910",
            weight = 1,
            group = "Quilombolas",
            popup = ~paste0("<strong>Quilombola</strong><br/>", 
                            "Classe: ", class_label)
          )
      }
      
      if (!is.null(inter$assentamentos) && nrow(inter$assentamentos) > 0) {
        m <- m |>
          addPolygons(
            data = inter$assentamentos,
            fillColor = "#27ae60",
            fillOpacity = opacidade,
            color = "#1e8449",
            weight = 1,
            group = "Assentamentos",
            popup = ~paste0("<strong>Assentamento</strong><br/>",
                            "Classe: ", class_label)
          )
      }
      
      if (!is.null(inter$indigenas) && nrow(inter$indigenas) > 0) {
        m <- m |>
          addPolygons(
            data = inter$indigenas,
            fillColor = "#3498db",
            fillOpacity = opacidade,
            color = "#2874a6",
            weight = 1,
            group = "Territórios Indígenas",
            popup = ~paste0("<strong>Território Indígena</strong><br/>",
                            "Classe: ", class_label)
          )
      }
      
      if (!is.null(inter$ensino) && nrow(inter$ensino) > 0) {
        m <- m |>
          addCircleMarkers(
            data = inter$ensino,
            radius = 5,
            fillColor = "#9b59b6",
            fillOpacity = opacidade,
            color = "#7d3c98",
            weight = 1,
            group = "Instituições de Ensino",
            popup = ~paste0("<strong>Instituição de Ensino</strong><br/>",
                            "Nome: ", nome_feature, "<br/>",
                            "Classe: ", class_label)
          )
      }
      
      if (!is.null(inter$prisoes) && nrow(inter$prisoes) > 0) {
        m <- m |>
          addCircleMarkers(
            data = inter$prisoes,
            radius = 5,
            fillColor = "#e74c3c",
            fillOpacity = opacidade,
            color = "#c0392b",
            weight = 1,
            group = "Unidades Prisionais",
            popup = ~paste0("<strong>Unidade Prisional</strong><br/>",
                            "Nome: ", nome_feature, "<br/>",
                            "Classe: ", class_label)
          )
      }
      
      if (!is.null(inter$sementes) && nrow(inter$sementes) > 0) {
        m <- m |>
          addCircleMarkers(
            data = inter$sementes,
            radius = 5,
            fillColor = "#16a085",
            fillOpacity = opacidade,
            color = "#117a65",
            weight = 1,
            group = "Bancos de Sementes",
            popup = ~paste0("<strong>Banco de Sementes</strong><br/>",
                            "Nome: ", nome_feature, "<br/>",
                            "Classe: ", class_label)
          )
      }
      
      m |>
        addLayersControl(
          overlayGroups = c("Municípios", "Quilombolas", "Assentamentos", 
                            "Territórios Indígenas", "Instituições de Ensino", 
                            "Unidades Prisionais", "Bancos de Sementes"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    # Download handler para exportar mapa em HTML
    output$export_mapa_qualif <- downloadHandler(
      filename = function() {
        paste0("mapa_qualificacao_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        req(resultado_qualif_filtrado())
        req(intersecoes())
        
        resultado_sf <- resultado_qualif_filtrado()
        inter <- intersecoes()
        n_classes <- resultados_electre()$params$n_classes %||% 5
        labels_atuais <- label_map()
        cores_atuais <- paleta_cores()
        cores_hex <- unname(cores_atuais[as.character(1:n_classes)])
        
        pal_mun <- colorFactor(
          palette = cores_hex,
          domain = 1:n_classes,
          na.color = "transparent"
        )
        
        m <- leaflet() |>
          addProviderTiles(providers$CartoDB.Positron)
        
        # Adicionar municípios ao fundo
        m <- m |>
          addPolygons(
            data = resultado_sf,
            fillColor = ~pal_mun(class_electre),
            fillOpacity = 0.3,
            color = "#999999",
            weight = 0.5,
            group = "Municípios",
            label = ~paste0(NM_MUN, " - ", class_label)
          )
        
        opacidade <- 0.7
        
        if (!is.null(inter$quilombolas) && nrow(inter$quilombolas) > 0) {
          m <- m |>
            addPolygons(
              data = inter$quilombolas,
              fillColor = "#f39c12",
              fillOpacity = opacidade,
              color = "#d68910",
              weight = 1,
              group = "Quilombolas",
              popup = ~paste0("<strong>Quilombola</strong><br/>", 
                              "Classe: ", class_label)
            )
        }
        
        if (!is.null(inter$assentamentos) && nrow(inter$assentamentos) > 0) {
          m <- m |>
            addPolygons(
              data = inter$assentamentos,
              fillColor = "#27ae60",
              fillOpacity = opacidade,
              color = "#1e8449",
              weight = 1,
              group = "Assentamentos",
              popup = ~paste0("<strong>Assentamento</strong><br/>",
                              "Classe: ", class_label)
            )
        }
        
        if (!is.null(inter$indigenas) && nrow(inter$indigenas) > 0) {
          m <- m |>
            addPolygons(
              data = inter$indigenas,
              fillColor = "#3498db",
              fillOpacity = opacidade,
              color = "#2874a6",
              weight = 1,
              group = "Territórios Indígenas",
              popup = ~paste0("<strong>Território Indígena</strong><br/>",
                              "Classe: ", class_label)
            )
        }
        
        if (!is.null(inter$ensino) && nrow(inter$ensino) > 0) {
          m <- m |>
            addCircleMarkers(
              data = inter$ensino,
              radius = 5,
              fillColor = "#9b59b6",
              fillOpacity = opacidade,
              color = "#7d3c98",
              weight = 1,
              group = "Instituições de Ensino",
              popup = ~paste0("<strong>Instituição de Ensino</strong><br/>",
                              "Nome: ", nome_feature, "<br/>",
                              "Classe: ", class_label)
            )
        }
        
        if (!is.null(inter$prisoes) && nrow(inter$prisoes) > 0) {
          m <- m |>
            addCircleMarkers(
              data = inter$prisoes,
              radius = 5,
              fillColor = "#e74c3c",
              fillOpacity = opacidade,
              color = "#c0392b",
              weight = 1,
              group = "Unidades Prisionais",
              popup = ~paste0("<strong>Unidade Prisional</strong><br/>",
                              "Nome: ", nome_feature, "<br/>",
                              "Classe: ", class_label)
            )
        }
        
        if (!is.null(inter$sementes) && nrow(inter$sementes) > 0) {
          m <- m |>
            addCircleMarkers(
              data = inter$sementes,
              radius = 5,
              fillColor = "#16a085",
              fillOpacity = opacidade,
              color = "#117a65",
              weight = 1,
              group = "Bancos de Sementes",
              popup = ~paste0("<strong>Banco de Sementes</strong><br/>",
                              "Nome: ", nome_feature, "<br/>",
                              "Classe: ", class_label)
            )
        }
        
        m <- m |>
          addLayersControl(
            overlayGroups = c("Municípios", "Quilombolas", "Assentamentos", 
                              "Territórios Indígenas", "Instituições de Ensino", 
                              "Unidades Prisionais", "Bancos de Sementes"),
            options = layersControlOptions(collapsed = FALSE)
          )
        
        htmlwidgets::saveWidget(m, file, selfcontained = TRUE)
      }
    )
    
    
  })
}