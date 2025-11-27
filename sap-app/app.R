# =====================================================================
# SAP Secas e Desertificação — Aplicativo Principal
# Módulo Priorização de Ações
# =====================================================================

# Carregar configurações globais
source("global.R")

# Carregar módulos
source("R/mod_preproc.R")
source("R/mod_analise.R")
source("R/mod_relatorio.R")

# Carregar módulo do chatbot
source("R/mod_chatbot.R")

# Carregar função de geração de relatórios
source("R/widgets_relatorio.R")  
source("R/gerar_relatorio_html.R")

# =====================================================================
# UI
# =====================================================================

ui <- tagList(
  # Arquivos CSS e JS do chatbot
  tags$head(
    tags$link(rel = "stylesheet", href = "chatbot.css"),
    tags$script(src = "chatbot.js")
  ),
  
  page_navbar(
    title = tags$div(
      class = "d-flex align-items-center",
      tags$img(src = "sap_logo.png", height = "50", class = "me-3"),
      tags$div(
        tags$div("Sistema de Alerta Precoce | Secas e Desertificação", 
                 style = "font-size: 0.95rem; font-weight: 600; line-height: 1.2;"),
        tags$small("Módulo Priorização de Ações", 
                   style = "font-size: 0.75rem; opacity: 0.85;")
      )
    ),
    
    theme = theme_oca,
    
    # MÓDULO 1: PRÉ-PROCESSAMENTO
    nav_panel(
      title = "Pré-processamento",
      icon = icon("filter"),
      mod_preproc_ui("preproc")
    ),
    
    # MÓDULO 2: ANÁLISE
    nav_panel(
      title = "Análise",
      icon = icon("chart-line"),
      mod_analise_ui("analise")
    ),
    
    # MÓDULO 3: RELATÓRIOS
    nav_panel(
      title = "Relatórios",
      icon = icon("file-lines"),
      mod_relatorios_ui("relatorios")
    ),
    
    # NAVBAR ITEMS
    nav_spacer(),
    
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "light")
    )
  ),
  
  # CHATBOT: Ícone flutuante e modal (FORA do page_navbar)
  mod_chatbot_icon_ui("chatbot"),
  mod_chatbot_modal_ui("chatbot")
)

# =====================================================================
# SERVER
# =====================================================================

server <- function(input, output, session) {
  
  # Carregar dados globais
  data_global <- reactive({
    req(exists("mun_asd", inherits = TRUE))
    req(inherits(mun_asd, "sf"))
    mun_asd
  })
  
  # MÓDULO 1: PRÉ-PROCESSAMENTO
  preproc_data <- mod_preproc_server("preproc", data_global)
  
  # MÓDULO 2: ANÁLISE 
  # Recebe dados filtrados do pré-processamento
  # Retorna dados para o módulo de relatórios
  analise_data <- mod_analise_server("analise", preproc_data)
  
  # MÓDULO 3: RELATÓRIOS
  # Recebe dados do módulo de análise
  mod_relatorios_server("relatorios", analise_data)
  
  # CHATBOT: Servidor do assistente
  mod_chatbot_server(
    "chatbot",
    preproc_data = preproc_data,
    analise_data = analise_data,
    qualif_data = NULL,
    session_parent = session
  )
  
}

# =====================================================================
# RUN APP
# =====================================================================

shinyApp(ui, server)