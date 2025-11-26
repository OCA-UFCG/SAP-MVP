# =====================================================================
# COMPONENTES DE UI REUTILIZÁVEIS
# =====================================================================

#' Criar UI completa do módulo de análise
#' 
#' @param ns Namespace function
#' @return UI da página
#' @export
criar_ui_analise <- function(ns) {
  page_sidebar(
    # SIDEBAR
    sidebar = sidebar(
      criar_sidebar_analise(ns)
    ),
    
    # MAIN CONTENT
    navset_card_tab(
      full_screen = TRUE,
      
      nav_panel(
        title = "Parâmetros",
        icon = icon("sliders"),
        criar_aba_parametros(ns)
      ),
      
      nav_panel(
        title = "Resultados",
        icon = icon("chart-line"),
        criar_aba_resultados(ns)
      ),
      
      nav_panel(
        title = "Tabela Resultados",
        icon = icon("table"),
        criar_aba_tabela(ns)
      ),
      
      nav_panel(
        title = "Mapa",
        icon = icon("map"),
        criar_aba_mapa(ns)
      ),
      
      nav_panel(
        title = "Qualificação",
        icon = icon("layer-group"),
        criar_aba_qualificacao(ns)
      )
    )
  )
}

#' Criar sidebar
#' @export
criar_sidebar_analise <- function(ns) {
  tagList(
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
  )
}

#' Criar aba de parâmetros
#' @export
criar_aba_parametros <- function(ns) {
  div(
    style = "height: calc(100vh - 200px); overflow-y: auto; padding: 10px;",
    
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
      
      # Critérios
      card(
        full_screen = TRUE,
        height = "600px",
        card_header(icon("list-check"), " Critérios e Sentido"),
        helpText("Selecione os critérios e defina se são benefício ou custo:"),
        uiOutput(ns("ui_criterios")),
        hr(),
        helpText(icon("info-circle"), " Configure o sentido de cada critério:"),
        uiOutput(ns("ui_sentido_criterios"))
      ),
      
      # Pesos
      card(
        full_screen = TRUE,
        height = "600px",
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
    
    # Perfis e limiares
    layout_columns(
      col_widths = c(6, 6),
      criar_card_perfis(ns),
      criar_card_limiares(ns)
    ),
    
    # Resumo
    card(
      full_screen = TRUE,
      height = "500px",
      card_header(
        icon("terminal"), " Resumo dos Dados de Entrada ELECTRE"
      ),
      div(
        style = "background: #f8f9fa; padding: 1rem; border-radius: 6px; font-family: 'Courier New', monospace; font-size: 0.9em; overflow-x: auto; height: 400px; overflow-y: auto;",
        verbatimTextOutput(ns("resumo_entrada_electre"))
      )
    )
  )
}

#' Card de perfis
#' @export
criar_card_perfis <- function(ns) {
  card(
    full_screen = TRUE,
    height = "600px",
    card_header(
      icon("layer-group"), " Perfis de Classe (B)",
      tooltip(
        icon("circle-info", class = "ms-2"),
        "Define os limites entre as classes. Número de perfis = número de classes - 1.",
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
                  "Definir manualmente (com histogramas)" = "manual"),
      selected = "quantis"
    ),
    conditionalPanel(
      condition = "input.b_mode == 'manual'",
      ns = ns,
      actionButton(
        ns("btn_abrir_modal_perfis"),
        "Abrir Editor de Perfis Interativo",
        icon = icon("chart-bar"),
        class = "btn-primary w-100"
      ),
      hr(),
      helpText(icon("info-circle"), " Resumo dos perfis definidos:"),
      verbatimTextOutput(ns("resumo_perfis_definidos"))
    )
  )
}

#' Card de limiares
#' @export
criar_card_limiares <- function(ns) {
  card(
    full_screen = TRUE,
    height = "600px",
    card_header(
      icon("ruler"), " Limiares e Classificação"
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
}

# =====================================================================
# COMPONENTES DE UI - COMPLETO COM TODAS AS ABAS
# =====================================================================

#' Criar aba de resultados (Dashboard) - COMPLETA
#' @export
criar_aba_resultados <- function(ns) {
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
    
    # VALUE BOXES
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
    
    # PERFIL MÉDIO E DENSIDADE
    layout_columns(
      col_widths = c(7, 5),
      
      card(
        full_screen = TRUE,
        card_header(icon("table"), " Perfil Médio das Variáveis por Classe"),
        DTOutput(ns("tab_perfil_medio"))
      ),
      
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
  )
}

#' Criar aba de tabela - COMPLETA
#' @export
criar_aba_tabela <- function(ns) {
  layout_sidebar(
    sidebar = sidebar(
      width = 250,
      position = "left",
      
      card(
        card_header("Filtros", class = "bg-primary"),
        criar_botao_filtros_ui(ns, "tabela")
      )
    ),
    
    card_header(
      icon("database"), " Tabela Completa de Resultados",
      tooltip(
        icon("circle-info", class = "ms-2"),
        "Tabela detalhada com filtros e opções de download",
        placement = "bottom"
      )
    ),
    
    card(
      div(
        class = "d-flex justify-content-end gap-2 mb-3",
        downloadButton(ns("dl_tabela_csv"), "CSV", class = "btn-sm"),
        downloadButton(ns("dl_tabela_excel"), "Excel", class = "btn-sm")
      ),
      DTOutput(ns("tab_completa"))
    )
  )
}

#' Criar aba de mapa - COMPLETA
#' @export
criar_aba_mapa <- function(ns) {
  layout_sidebar(
    sidebar = sidebar(
      width = 250,
      position = "left",
      
      card(
        card_header("Filtros e Busca", class = "bg-primary"),
        
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
        
        criar_botao_filtros_ui(ns, "mapa")
      )
    ),
    
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span(
          icon("map-location-dot"), " Mapa de Classificação",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Visualização espacial das classes ELECTRE Tri-B",
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
}

#' Criar aba de qualificação - COMPLETA
#' @export
criar_aba_qualificacao <- function(ns) {
  div(
    style = "height: calc(100vh - 200px); overflow-y: auto; padding: 10px;",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        position = "left",
        
        
        # BOTÃO EXECUTAR QUALIFICAÇÃO
        actionButton(
          ns("run_qualificacao"),
          "Executar Qualificação",
          icon = icon("map-location-dot"),
          class = "btn-success w-100 mb-3"
        ),
        
        # SELEÇÃO DE CAMADAS
        card(
          card_header(
            icon("layer-group"), " Camadas Territoriais",
            class = "bg-info"
          ),
          checkboxGroupInput(
            ns("camadas_qualificacao"),
            NULL,
            choices = c(
              "Quilombolas" = "quilombolas",
              "Assentamentos" = "assentamentos",
              "Terras Indígenas" = "indigenas",
              "Instituições de Ensino" = "ensino",
              "Unidades Prisionais" = "prisoes",
              "Bancos de Sementes" = "sementes",
              "Propriedades Rurais" = "prop_rurais"
            ),
            selected = c("quilombolas", "assentamentos", "indigenas", 
                         "ensino", "prisoes", "sementes", "prop_rurais")
          )
        ),
        
        card(
          card_header("Filtros", class = "bg-primary"),
          criar_botao_filtros_ui(ns, "qualificacao")
        )
      ),
      
      card_header(
        icon("chart-area"), " Análise de Qualificação Territorial",
        tooltip(
          icon("circle-info", class = "ms-2"),
          "Análise espacial das camadas territoriais",
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
        ),
        value_box(
          title = "Propriedades Rurais",
          value = textOutput(ns("vb_prop_rurais")),
          showcase = icon("house"),
          theme = "purple"
        )
      ),
      
      checkboxInput(
        inputId = ns("gerar_mapa"),
        label = strong("Gerar Mapa"),
        value = FALSE
      ),
      
      # MAPA
      layout_columns(
        col_widths = c(12),
        
        card(
          full_screen = TRUE,
          height = "700px",
          fill = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span(icon("map-marked-alt"), " Mapa de Qualificação Territorial"),
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
      
      # GRÁFICOS
      layout_columns(
        col_widths = c(6, 6),
        
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
      
      # TABELAS
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
  )
}