# =====================================================================
# MÓDULO PRINCIPAL: PRÉ-PROCESSAMENTO (VERSÃO OTIMIZADA COM ASYNC)
# =====================================================================

library(promises)
library(future)

# Carregar submódulos e utilitários
source("R/preproc/helpers.R")
source("R/preproc/data_processing.R")
source("R/preproc/column_constructor.R")
source("R/preproc/filter_ui.R")
source("R/preproc/filter_utils.R")
source("R/preproc/filter_system.R")
source("R/preproc/visualizations.R")
source("R/preproc/maps.R")
source("R/preproc/downloads.R")
source("R/preproc/variable_renamer.R")

# =====================================================================
# INTERFACE DO USUÁRIO (UI)
# =====================================================================

mod_preproc_ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    # ========================================
    # SIDEBAR
    # ========================================
    sidebar = sidebar(
      width = 300,
      
      card(
        card_header(
          icon("database"), " Fonte de Dados",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Escolha entre usar o banco de dados padrão ou importar seus próprios dados espaciais",
            placement = "right"
          ),
          class = "bg-primary"
        ),
        checkboxInput(ns("use_default"), "Usar banco de dados padrão", value = TRUE),
        conditionalPanel(
          condition = "!input.use_default",
          ns = ns,
          fileInput(
            ns("upload_geo"),
            "Carregar GeoPackage/Shapefile",
            accept = c(".gpkg", ".shp", ".zip"),
            placeholder = "Selecione arquivo..."
          ),
          helpText("Aceita .gpkg ou .zip contendo .shp")
        )
      ),
      
      card(
        card_header(
          icon("i-cursor"), " Renomear Variáveis",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Edite os nomes das variáveis para exibição mais amigável",
            placement = "right"
          ),
          class = "bg-primary"
        ),
        criar_botao_renomear_ui(ns, "data")
      ),
      
      card(
        card_header(
          icon("table-columns"), " Colunas Derivadas",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Crie novas colunas através de operações matemáticas ou expressões customizadas",
            placement = "right"
          ),
          class = "bg-primary"
        ),
        criar_botao_colunas_ui(ns, "data")
      ),
      
      card(
        card_header(
          icon("filter"), " Filtros",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Crie filtros para selecionar subconjuntos específicos dos dados",
            placement = "right"
          ),
          class = "bg-primary"
        ),
        criar_botao_filtros_ui(ns, "data")
      ),
      
      card(
        card_header(
          icon("chart-line"), " Variáveis",
          tooltip(
            icon("circle-info", class = "ms-2"),
            "Selecione as variáveis numéricas para análise",
            placement = "right"
          ),
          class = "bg-primary"
        ),
        selectizeInput(
          ns("vars_keep"),
          NULL,
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Selecione variáveis...",
            plugins = list("remove_button")
          )
        )
      )
    ),
    
    # ========================================
    # ÁREA PRINCIPAL
    # ========================================
    navset_card_tab(
      id = ns("main_tabs"),
      full_screen = TRUE,
      
      nav_panel(
        title = "Resumo",
        icon = icon("table-cells"),
        
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span(icon("clipboard-list"), " Resumo do Subconjunto"),
          div(
            downloadButton(ns("dl_csv"), "CSV", class = "btn-sm"),
            downloadButton(ns("dl_xlsx"), "Excel", class = "btn-sm ms-1")
          )
        ),
        
        layout_column_wrap(
          width = "250px",
          fill = FALSE,
          value_box(
            title = "Regiões",
            value = textOutput(ns("n_regioes")),
            showcase = icon("globe-americas"),
            theme = "primary"
          ),
          value_box(
            title = "Estados",
            value = textOutput(ns("n_ufs")),
            showcase = icon("map"),
            theme = "primary"
          ),
          value_box(
            title = "Municípios",
            value = textOutput(ns("n_municipios")),
            showcase = icon("city"),
            theme = "primary"
          ),
          value_box(
            title = "Variáveis",
            value = textOutput(ns("n_vars")),
            showcase = icon("list-ol"),
            theme = "primary"
          )
        ),
        
        DTOutput(ns("tab_summary"))
      ),
      
      nav_panel(
        title = "Distribuições",
        icon = icon("chart-column"),
        card_header(icon("chart-simple"), " Distribuições das variáveis"),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header(icon("chart-bar"), " Histogramas"),
            plotlyOutput(ns("p_hist"), height = "400px")
          ),
          card(
            card_header(icon("square"), " Boxplots"),
            plotlyOutput(ns("p_box"), height = "400px")
          )
        )
      ),
      
      nav_panel(
        title = "Correlação",
        icon = icon("grip"),
        card_header(icon("circle-nodes"), " Matriz de Correlação (Pearson)"),
        plotlyOutput(ns("p_corr"), height = "600px")
      ),
      
      nav_panel(
        title = "PCA",
        icon = icon("compass"),
        card_header(icon("bullseye"), " Análise de Componentes Principais"),
        plotlyOutput(ns("p_pca"), height = "600px")
      ),
      
      # TAB: MAPA - COM SIDEBAR INTERNO
      nav_panel(
        title = "Mapa",
        icon = icon("earth-americas"),
        
        layout_sidebar(
          sidebar = sidebar(
            width = 280,
            position = "right",
            
            card(
              card_header(icon("sliders-h"), " Configurações", class = "bg-info"),
              
              selectizeInput(
                ns("var_map"), 
                "Variável:", 
                choices = NULL,
                options = list(
                  placeholder = "Digite para buscar...",
                  maxOptions = 1000
                )
              ),
              
              textInput(
                ns("nome_var_customizado"),
                "Nome na legenda (opcional):",
                placeholder = "Deixe vazio para usar o nome da variável"
              ),
              
              selectInput(
                ns("pal"), 
                "Paleta:", 
                choices = paletas, 
                selected = "viridis"
              ),
              
              checkboxInput(ns("reverse_pal"), "Inverter cores", FALSE),
              checkboxInput(ns("log_scale"), "Escala log₁₀", FALSE),
              
              hr(),
              
              checkboxInput(ns("usar_classes"), "Usar classificação em classes", FALSE),
              
              conditionalPanel(
                condition = "input.usar_classes",
                ns = ns,
                sliderInput(
                  ns("n_classes"),
                  "Número de classes:",
                  min = 3,
                  max = 9,
                  value = 5,
                  step = 1
                ),
                selectInput(
                  ns("metodo_quebra"),
                  "Método de quebras:",
                  choices = c(
                    "Jenks (natural breaks)" = "jenks",
                    "Quantis" = "quantile",
                    "Intervalos iguais" = "equal",
                    "Desvio padrão" = "sd",
                    "K-means" = "kmeans"
                  ),
                  selected = "jenks"
                )
              ),
              
              hr(),
              
              checkboxInput(ns("mostrar_limites_ufs"), "Mostrar limites estaduais", FALSE),
              
              hr(),
              
              downloadButton(ns("dl_gpkg"), "GeoPackage", class = "btn-sm w-100 mb-1"),
              downloadButton(ns("dl_mapa_png"), "Mapa (PNG)", class = "btn-sm w-100")
            )
          ),
          
          leafletOutput(ns("map"), height = "700px")
        )
      )
    )
  )
}

# =====================================================================
# LÓGICA DO SERVIDOR (SERVER)
# =====================================================================

mod_preproc_server <- function(id, default_data_path = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ========================================
    # REACTIVE VALUES
    # ========================================
    rv <- reactiveValues(
      data_loaded = FALSE,
      processing = FALSE
    )
    
    # ========================================
    # FONTE DE DADOS
    # ========================================
    data_source <- reactive({
      if (input$use_default) {
        # default_data_path pode ser:
        # 1. Dados já carregados (sf object)
        # 2. Função que retorna dados
        # 3. String com caminho do arquivo
        
        if (is.function(default_data_path)) {
          # É uma função - executar
          default_data_path()
        } else if (inherits(default_data_path, "sf")) {
          # Já são dados SF - retornar diretamente
          default_data_path
        } else if (is.character(default_data_path) && length(default_data_path) == 1) {
          # É caminho de arquivo - ler
          req(file.exists(default_data_path))
          qs::qread(default_data_path)
        } else {
          # Outro tipo - retornar diretamente
          default_data_path
        }
      } else {
        req(input$upload_geo)
        
        file_path <- input$upload_geo$datapath
        file_ext <- tolower(tools::file_ext(input$upload_geo$name))
        
        tryCatch({
          if (file_ext == "gpkg") {
            sf::st_read(file_path, quiet = TRUE)
          } else if (file_ext == "shp") {
            sf::st_read(file_path, quiet = TRUE)
          } else if (file_ext == "zip") {
            temp_dir <- tempdir()
            unzip(file_path, exdir = temp_dir)
            shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
            req(shp_file)
            sf::st_read(shp_file, quiet = TRUE)
          } else {
            NULL
          }
        }, error = function(e) {
          showNotification(paste("Erro ao carregar arquivo:", e$message), type = "error", duration = 5)
          NULL
        })
      }
    })
    
    data_sf_processed <- reactive({
      df <- data_source()
      req(inherits(df, "sf"))
      x <- sf::st_make_valid(df)
      ensure_wgs84(x)
    })
    
    # ========================================
    # CONSTRUTOR DE COLUNAS
    # ========================================
    colunas_criadas_data <- reactiveVal(list())
    
    output$ui_resumo_colunas_data <- renderUI({
      colunas <- colunas_criadas_data()
      if (length(colunas) == 0) {
        return(tags$p(class = "text-muted small", icon("circle-info"), " Nenhuma coluna criada"))
      }
      tagList(
        tags$p(class = "fw-bold small mb-1", paste(length(colunas), "coluna(s) criada(s):")),
        lapply(colunas, function(c) {
          tags$div(
            class = "small text-muted border-start border-3 border-success ps-2 mb-1",
            tags$strong(c$nome), ": ", tags$code(c$descricao)
          )
        })
      )
    })
    
    # ========================================
    # RENOMEAR VARIÁVEIS
    # ========================================
    nomes_editados_data <- reactiveVal(list())
    
    data_com_colunas_original <- reactive({
      df <- data_sf_processed()
      req(df)
      aplicar_colunas(df, colunas_criadas_data())
    })
    
    criar_sistema_colunas_modal(session, ns, "data", data_com_colunas_original, colunas_criadas_data)
    criar_sistema_renomear_modal(session, ns, "data", data_com_colunas_original, nomes_editados_data)
    
    # ========================================
    # SISTEMA DE FILTROS
    # ========================================
    filtros_aplicados_data <- reactiveVal(list())
    
    output$ui_resumo_filtros_data <- renderUI({
      filtros <- filtros_aplicados_data()
      criar_resumo_filtros(filtros)
    })
    
    criar_sistema_filtros_modal(session, ns, "data", data_com_colunas_original, filtros_aplicados_data)
    
    # ========================================
    # ATUALIZAR CHOICES
    # ========================================
    observe({
      df <- data_com_colunas_original()
      req(df)
      
      vnum <- setdiff(num_cols(df), attr(df, "sf_column"))
      nomes_map <- nomes_editados_data()
      
      choices_mapa <- vnum
      names(choices_mapa) <- sapply(vnum, function(nome) {
        if (!is.null(nomes_map[[nome]])) nomes_map[[nome]] else nome
      })
      
      updateSelectizeInput(
        session, 
        "var_map", 
        choices = choices_mapa, 
        selected = vnum[[1]] %||% character(0),
        server = TRUE
      )
      
      vars_padrao <- c("spe_mean", "ia_mean", "rural_poverty")
      selected_vars <- intersect(vars_padrao, vnum)
      if (length(selected_vars) == 0) {
        selected_vars <- head(vnum, 3)
      }
      
      updateSelectizeInput(session, "vars_keep", choices = choices_mapa, selected = selected_vars)
    })
    
    # ========================================
    # PROCESSAMENTO DE DADOS (ASSÍNCRONO)
    # ========================================
    
    filtered_com_colunas_original <- reactive({
      df <- data_com_colunas_original()
      req(df)
      aplicar_filtros(df, filtros_aplicados_data())
    })
    
    filtered_com_colunas <- reactive({
      filtered_com_colunas_original()
    })
    
    vars_sel_originais <- reactive({
      v <- input$vars_keep
      if (is.null(v) || length(v) == 0) {
        df <- filtered_com_colunas_original()
        todas_vars <- setdiff(num_cols(df), attr(df, "sf_column"))
        
        vars_padrao <- c("spe_mean", "ia_mean", "rural_poverty")
        v <- intersect(vars_padrao, todas_vars)
        if (length(v) == 0) {
          v <- head(todas_vars, 3)
        }
      }
      v
    })
    
    vars_sel <- reactive({
      v_orig <- vars_sel_originais()
      nomes_map <- nomes_editados_data()
      
      sapply(v_orig, function(nome) {
        if (!is.null(nomes_map[[nome]])) nomes_map[[nome]] else nome
      }, USE.NAMES = FALSE)
    })
    
    filtered <- reactive({
      df <- filtered_com_colunas_original()
      
      colunas_essenciais <- c("NM_REGIAO", "NM_UF", "NM_MUN", "CD_MUN", attr(df, "sf_column"))
      colunas_essenciais <- intersect(colunas_essenciais, names(df))
      
      v <- vars_sel_originais()
      colunas_manter <- unique(c(colunas_essenciais, v))
      
      df <- dplyr::select(df, dplyr::any_of(colunas_manter))
      aplicar_renomeacao(df, nomes_editados_data())
    })
    
    df_plain <- reactive({ sf::st_drop_geometry(filtered()) })
    
    summary_table <- reactive({
      df <- df_plain()
      base_cols <- intersect(c("NM_REGIAO","NM_UF","NM_MUN"), names(df))
      v <- vars_sel()
      dplyr::select(df, dplyr::any_of(base_cols), dplyr::all_of(v))
    })
    
    # ========================================
    # VALUE BOXES
    # ========================================
    output$n_regioes <- renderText({
      as.character(dplyr::n_distinct(df_plain()$NM_REGIAO %||% NA))
    })
    
    output$n_ufs <- renderText({
      as.character(dplyr::n_distinct(df_plain()$NM_UF %||% NA))
    })
    
    output$n_municipios <- renderText({
      format(nrow(filtered()), big.mark = ".", decimal.mark = ",")
    })
    
    output$n_vars <- renderText({
      vars <- vars_sel()
      req(length(vars) > 0)
      as.character(length(vars))
    })
    
    # ========================================
    # TABELA
    # ========================================
    output$tab_summary <- renderDT({
      datatable(summary_table(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # ========================================
    # DOWNLOADS
    # ========================================
    output$dl_csv <- criar_download_csv(summary_table)
    output$dl_xlsx <- criar_download_xlsx(summary_table)
    output$dl_gpkg <- criar_download_gpkg(filtered)
    
    output$dl_mapa_png <- criar_download_mapa_png(
      filtered_data = filtered_com_colunas,
      input = input,
      var_name_reactive = reactive(input$var_map),
      nomes_map_reactive = nomes_editados_data
    )
    
    # ========================================
    # VISUALIZAÇÕES
    # ========================================
    setup_visualizacoes(output, df_plain, vars_sel)
    
    # ========================================
    # SISTEMA DE MAPAS
    # ========================================
    setup_sistema_mapas(
      output, 
      session, 
      input, 
      filtered_com_colunas, 
      ns,
      nomes_map_reactive = nomes_editados_data
    )
    
    # ========================================
    # RETORNAR
    # ========================================
    return(
      list(
        data = filtered,
        vars = vars_sel,
        data_plain = df_plain
      )
    )
  })
}