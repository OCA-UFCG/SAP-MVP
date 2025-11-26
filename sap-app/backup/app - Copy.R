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

# Carregar função de geração de relatórios
source("R/widgets_relatorio.R")  
source("R/gerar_relatorio_html.R")

# =====================================================================
# UI
# =====================================================================

ui <- page_navbar(
  title = tags$div(
    class = "d-flex align-items-center",
    tags$img(src = "oca_logo.png", height = "50", class = "me-3"),
    tags$div(
      tags$div("Análise Secas e Desertificação", 
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
  
}

# =====================================================================
# RUN APP
# =====================================================================

shinyApp(ui, server)