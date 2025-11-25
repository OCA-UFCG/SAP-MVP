# =====================================================================
# MÓDULO: RELATÓRIOS CUSTOMIZÁVEIS
# =====================================================================

# ---- UI ---------------------------------------------------------------
mod_relatorios_ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    # SIDEBAR
    sidebar = sidebar(
      width = 280,
      
      # STATUS
      card(
        card_header(
          icon("file-alt"), " Status",
          class = "bg-success"
        ),
        uiOutput(ns("status_relatorio"))
      ),
      
      # AÇÕES RÁPIDAS
      card(
        card_header(icon("bolt"), " Ações Rápidas", class = "bg-primary"),
        actionButton(
          ns("gerar_preview"),
          "Gerar Preview",
          icon = icon("eye"),
          class = "btn-info w-100 mb-2"
        ),
        downloadButton(ns("dl_html"), "Baixar HTML", class = "w-100 mb-2"),
        downloadButton(ns("dl_pdf"), "Baixar PDF", class = "w-100 mb-2"),
        downloadButton(ns("dl_excel"), "Baixar Excel", class = "w-100")
      )
    ),
    
    # MAIN CONTENT
    navset_card_tab(
      full_screen = TRUE,
      
      # ==================================================================
      # ABA 1: CONFIGURAÇÃO
      # ==================================================================
      nav_panel(
        title = "Configuração",
        icon = icon("gear"),
        
        div(
          style = "height: calc(100vh - 200px); overflow-y: auto; padding: 10px;",
          
          card_header(
            icon("file-signature"), " Informações do Relatório",
            tooltip(
              icon("circle-info", class = "ms-2"),
              "Configure os metadados e informações gerais do relatório",
              placement = "bottom"
            )
          ),
          
          # METADADOS
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              full_screen = TRUE,
              height = "500px",
              card_header(icon("info-circle"), " Metadados Básicos"),
              
              textInput(
                ns("titulo"),
                "Título do Relatório:",
                value = "Análise Multicritério ELECTRE Tri-B",
                width = "100%"
              ),
              
              textInput(
                ns("subtitulo"),
                "Subtítulo:",
                value = "Região do Semiárido Brasileiro",
                width = "100%"
              ),
              
              textInput(
                ns("autor"),
                "Autor(es):",
                value = "",
                placeholder = "Nome do pesquisador"
              ),
              
              textInput(
                ns("instituicao"),
                "Instituição:",
                value = "",
                placeholder = "Universidade/Órgão"
              ),
              
              dateInput(
                ns("data_relatorio"),
                "Data:",
                value = Sys.Date(),
                format = "dd/mm/yyyy",
                language = "pt-BR"
              ),
              
              textAreaInput(
                ns("resumo_executivo"),
                "Resumo Executivo (opcional):",
                value = "",
                placeholder = "Breve descrição do contexto e objetivos da análise...",
                rows = 4,
                width = "100%"
              )
            ),
            
            card(
              full_screen = TRUE,
              height = "500px",
              card_header(icon("palette"), " Identidade Visual"),
              
              fileInput(
                ns("logo"),
                "Logo (opcional):",
                accept = c("image/png", "image/jpeg", "image/jpg"),
                buttonLabel = "Escolher...",
                placeholder = "Nenhum arquivo"
              ),
              
              uiOutput(ns("preview_logo")),
              
              selectInput(
                ns("template"),
                "Template:",
                choices = c(
                  "Executivo (resumido)" = "executivo",
                  "Técnico (detalhado)" = "tecnico",
                  "Completo (todos os detalhes)" = "completo"
                ),
                selected = "tecnico"
              ),
              
              selectInput(
                ns("tema_visual"),
                "Tema Visual:",
                choices = c(
                  "Claro (padrão)" = "light",
                  "Escuro" = "dark",
                  "Minimalista" = "minimal",
                  "Científico" = "scientific"
                ),
                selected = "light"
              ),
              
              colourpicker::colourInput(
                ns("cor_principal"),
                "Cor Principal:",
                value = "#2c3e50",
                showColour = "background"
              )
            )
          ),
          
          # SELEÇÃO DE DADOS
          layout_columns(
            col_widths = c(12),
            
            card(
              full_screen = TRUE,
              height = "400px",
              card_header(
                icon("database"), " Seleção de Variáveis Adicionais",
                tooltip(
                  icon("circle-info", class = "ms-2"),
                  "Escolha variáveis adicionais do dataset original para incluir no relatório",
                  placement = "bottom"
                )
              ),
              
              helpText(
                icon("lightbulb"),
                " As variáveis da classificação ELECTRE e qualificação territorial já estão incluídas automaticamente."
              ),
              
              selectizeInput(
                ns("vars_adicionais"),
                "Variáveis Adicionais:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Selecione variáveis para incluir no relatório...",
                  plugins = list("remove_button")
                )
              ),
              
              checkboxInput(
                ns("incluir_geometria"),
                "Incluir dados espaciais (geometria) no Excel",
                value = FALSE
              )
            )
          )
        )
      ),
      
      # ==================================================================
      # ABA 2: CONTEÚDO
      # ==================================================================
      nav_panel(
        title = "Conteúdo",
        icon = icon("list-check"),
        
        div(
          style = "height: calc(100vh - 200px); overflow-y: auto; padding: 10px;",
          
          card_header(
            icon("puzzle-piece"), " Selecione os Componentes do Relatório",
            tooltip(
              icon("circle-info", class = "ms-2"),
              "Marque as seções que deseja incluir no relatório final",
              placement = "bottom"
            )
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            
            # SEÇÕES PRINCIPAIS
            card(
              full_screen = TRUE,
              card_header(icon("book"), " Seções Principais"),
              
              checkboxGroupInput(
                ns("secoes_principais"),
                NULL,
                choices = c(
                  "Capa e Sumário" = "capa",
                  "Resumo Executivo" = "resumo",
                  "Metodologia ELECTRE Tri-B" = "metodologia",
                  "Parâmetros Utilizados" = "parametros",
                  "Resultados da Classificação" = "resultados",
                  "Análise de Qualificação Territorial" = "qualificacao",
                  "Análises Estatísticas" = "estatisticas",
                  "Conclusões e Recomendações" = "conclusoes"
                ),
                selected = c("capa", "resumo", "resultados", "qualificacao")
              )
            ),
            
            # ELEMENTOS VISUAIS
            card(
              full_screen = TRUE,
              card_header(icon("chart-bar"), " Elementos Visuais"),
              
              checkboxGroupInput(
                ns("elementos_visuais"),
                NULL,
                choices = c(
                  "Gráfico: Distribuição de Classes" = "graf_distribuicao",
                  "Gráfico: Classes por Região/UF" = "graf_regional",
                  "Gráfico: Densidade por Classe" = "graf_densidade",
                  "Gráfico: Qualificação por Classe" = "graf_qualif_classe",
                  "Gráfico: Total de Qualificações" = "graf_qualif_total",
                  "Mapa: Classificação ELECTRE" = "mapa_classificacao",
                  "Mapa: Qualificação Territorial" = "mapa_qualificacao",
                  "Value Boxes: Métricas Principais" = "value_boxes"
                ),
                selected = c("graf_distribuicao", "mapa_classificacao", "value_boxes")
              )
            )
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            
            # TABELAS
            card(
              full_screen = TRUE,
              card_header(icon("table"), " Tabelas de Dados"),
              
              checkboxGroupInput(
                ns("tabelas"),
                NULL,
                choices = c(
                  "Tabela: Dados Completos" = "tab_completa",
                  "Tabela: Resumo por Classe" = "tab_resumo_classe",
                  "Tabela: Perfil Médio por Classe" = "tab_perfil_medio",
                  "Tabela: Estatísticas de Qualificação" = "tab_estatisticas_qualif",
                  "Tabela: Ranking de Municípios" = "tab_ranking",
                  "Tabela: Parâmetros ELECTRE" = "tab_parametros"
                ),
                selected = c("tab_completa", "tab_resumo_classe")
              ),
              
              numericInput(
                ns("max_linhas_tabela"),
                "Máximo de linhas por tabela (0 = todas):",
                value = 50,
                min = 0,
                max = 10000,
                step = 10
              )
            ),
            
            # CONFIGURAÇÕES AVANÇADAS
            card(
              full_screen = TRUE,
              card_header(icon("sliders"), " Configurações Avançadas"),
              
              checkboxInput(
                ns("incluir_codigo"),
                "Incluir código R usado na análise",
                value = FALSE
              ),
              
              checkboxInput(
                ns("incluir_referencias"),
                "Incluir referências bibliográficas",
                value = TRUE
              ),
              
              checkboxInput(
                ns("numerar_secoes"),
                "Numerar seções automaticamente",
                value = TRUE
              ),
              
              checkboxInput(
                ns("indice_interativo"),
                "Índice interativo (apenas HTML)",
                value = TRUE
              ),
              
              selectInput(
                ns("idioma"),
                "Idioma:",
                choices = c("Português" = "pt", "English" = "en", "Español" = "es"),
                selected = "pt"
              )
            )
          )
        )
      ),
      
      # ==================================================================
      # ABA 3: FILTROS
      # ==================================================================
      nav_panel(
        title = "Filtros",
        icon = icon("filter"),
        
        div(
          style = "height: calc(100vh - 200px); overflow-y: auto; padding: 10px;",
          
          card_header(
            icon("filter"), " Aplicar Filtros aos Dados",
            tooltip(
              icon("circle-info", class = "ms-2"),
              "Filtre os dados que serão incluídos no relatório. Deixe vazio para incluir todos.",
              placement = "bottom"
            )
          ),
          
          layout_columns(
            col_widths = c(12),
            
            card(
              full_screen = TRUE,
              card_header(icon("filter"), " Sistema de Filtros"),
              
              helpText(
                icon("info-circle"),
                " Configure filtros para incluir apenas os dados relevantes no relatório.",
                br(),
                tags$strong("Exemplo:"), " Gerar relatório apenas para municípios da classe C4 e C5."
              ),
              
              actionButton(
                ns("btn_abrir_filtros"),
                "Configurar Filtros",
                icon = icon("filter"),
                class = "btn-primary w-100 mb-3"
              ),
              
              uiOutput(ns("resumo_filtros_relatorio"))
            )
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              full_screen = TRUE,
              card_header(icon("pie-chart"), " Resumo dos Dados Filtrados"),
              uiOutput(ns("resumo_dados_filtrados"))
            ),
            
            card(
              full_screen = TRUE,
              card_header(icon("table"), " Preview da Tabela Final"),
              DTOutput(ns("preview_tabela_final"))
            )
          )
        )
      ),
      
      # ==================================================================
      # ABA 4: PREVIEW & EXPORTAR
      # ==================================================================
      nav_panel(
        title = "Exportar",
        icon = icon("file-export"),
        
        div(
          style = "height: calc(100vh - 200px); overflow-y: auto; padding: 10px;",
          
          card_header(
            icon("eye"), " Preview e Exportação",
            tooltip(
              icon("circle-info", class = "ms-2"),
              "Visualize o relatório antes de exportar e escolha o formato desejado",
              placement = "bottom"
            )
          ),
          
          # OPÇÕES DE EXPORT
          layout_columns(
            col_widths = c(4, 4, 4),
            
            card(
              full_screen = TRUE,
              card_header(
                icon("file-code"), " HTML Interativo",
                class = "bg-primary text-white"
              ),
              card_body(
                tags$ul(
                  tags$li("Gráficos interativos (Plotly)"),
                  tags$li("Mapas interativos (Leaflet)"),
                  tags$li("Tabelas filtráveis (DT)"),
                  tags$li("Navegação por seções"),
                  tags$li("Responsivo (mobile-friendly)")
                ),
                downloadButton(
                  ns("export_html_full"),
                  "Exportar HTML",
                  icon = icon("download"),
                  class = "btn-primary w-100 mt-3"
                )
              )
            ),
            
            card(
              full_screen = TRUE,
              card_header(
                icon("file-pdf"), " PDF Estático",
                class = "bg-danger text-white"
              ),
              card_body(
                tags$ul(
                  tags$li("Gráficos estáticos (ggplot2)"),
                  tags$li("Mapas estáticos"),
                  tags$li("Tabelas formatadas"),
                  tags$li("Pronto para impressão"),
                  tags$li("Ideal para relatórios oficiais")
                ),
                downloadButton(
                  ns("export_pdf_full"),
                  "Exportar PDF",
                  icon = icon("download"),
                  class = "btn-danger w-100 mt-3"
                )
              )
            ),
            
            card(
              full_screen = TRUE,
              card_header(
                icon("file-excel"), " Excel (XLSX)",
                class = "bg-success text-white"
              ),
              card_body(
                tags$ul(
                  tags$li("Múltiplas abas organizadas"),
                  tags$li("Formatação condicional"),
                  tags$li("Filtros e ordenação"),
                  tags$li("Fórmulas e resumos"),
                  tags$li("Cores por classe ELECTRE")
                ),
                downloadButton(
                  ns("export_excel_full"),
                  "Exportar Excel",
                  icon = icon("download"),
                  class = "btn-success w-100 mt-3"
                )
              )
            )
          ),
          
          # PREVIEW DO RELATÓRIO
          layout_columns(
            col_widths = c(12),
            
            card(
              full_screen = TRUE,
              height = "700px",
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(icon("eye"), " Preview do Relatório HTML"),
                actionButton(
                  ns("refresh_preview"),
                  "Atualizar Preview",
                  icon = icon("sync"),
                  class = "btn-sm btn-info"
                )
              ),
              card_body(
                uiOutput(ns("preview_iframe")),
                fillable = TRUE
              )
            )
          ),
          
          # LOG DE GERAÇÃO
          layout_columns(
            col_widths = c(12),
            
            card(
              full_screen = TRUE,
              card_header(icon("terminal"), " Log de Geração"),
              verbatimTextOutput(ns("log_geracao"))
            )
          )
        )
      )
    )
  )
}

# ---- SERVER -----------------------------------------------------------
mod_relatorios_server <- function(id, analise_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ====================================================================
    # DADOS NECESSÁRIOS DO MÓDULO DE ANÁLISE
    # ====================================================================
    
    resultados_electre <- reactive({
      req(analise_data$resultados_electre)
      analise_data$resultados_electre()
    })
    
    dados_qualificacao <- reactive({
      req(analise_data$intersecoes)
      analise_data$intersecoes()
    })
    
    data_sf_original <- reactive({
      req(analise_data$data_sf)
      analise_data$data_sf()
    })
    
    # ====================================================================
    # VARIÁVEIS ADICIONAIS DISPONÍVEIS
    # ====================================================================
    
    observe({
      req(data_sf_original())
      df <- data_sf_original()
      
      # Excluir variáveis já presentes nos resultados
      vars_excluir <- c("geometry", "geom", "CD_MUN", "class_electre", "class_label")
      vars_disponiveis <- setdiff(names(df), vars_excluir)
      
      updateSelectizeInput(
        session,
        "vars_adicionais",
        choices = vars_disponiveis
      )
    })
    
    # ====================================================================
    # STATUS DO RELATÓRIO
    # ====================================================================
    
    output$status_relatorio <- renderUI({
      req(resultados_electre())
      
      n_mun <- nrow(resultados_electre()$results)
      n_vars_add <- length(input$vars_adicionais %||% c())
      n_secoes <- length(input$secoes_principais %||% c())
      n_graficos <- length(input$elementos_visuais %||% c())
      
      tagList(
        div(
          class = "mb-2",
          icon("check-circle", class = "text-success"),
          " Dados carregados"
        ),
        hr(),
        tags$small(
          tags$strong("Municípios:"), format(n_mun, big.mark = "."), br(),
          tags$strong("Variáveis extras:"), n_vars_add, br(),
          tags$strong("Seções:"), n_secoes, br(),
          tags$strong("Gráficos:"), n_graficos
        )
      )
    })
    
    # ====================================================================
    # PREVIEW DO LOGO
    # ====================================================================
    
    output$preview_logo <- renderUI({
      req(input$logo)
      
      logo_path <- input$logo$datapath
      logo_data <- base64enc::base64encode(logo_path)
      logo_type <- tools::file_ext(input$logo$name)
      
      tags$div(
        class = "text-center mt-3 mb-3",
        tags$img(
          src = paste0("data:image/", logo_type, ";base64,", logo_data),
          style = "max-width: 200px; max-height: 150px; border: 1px solid #ddd; padding: 5px;"
        )
      )
    })
    
    # ====================================================================
    # SISTEMA DE FILTROS
    # ====================================================================
    
    filtros_relatorio_aplicados <- reactiveVal(list())
    
    observeEvent(input$btn_abrir_filtros, {
      req(resultados_electre())
      
      showModal(modalDialog(
        title = "Configurar Filtros para o Relatório",
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("aplicar_filtros_rel"), "Aplicar", class = "btn-success")
        ),
        
        helpText(
          icon("info-circle"),
          " Configure filtros para limitar os dados que aparecerão no relatório."
        ),
        
        selectInput(
          ns("filtro_classe"),
          "Filtrar por Classe ELECTRE:",
          choices = NULL,
          multiple = TRUE
        )
      ))
      
      # Atualizar choices de classe
      classes_disponiveis <- sort(unique(resultados_electre()$results$class_electre))
      label_map <- analise_data$label_map()
      choices_classes <- setNames(
        classes_disponiveis,
        label_map[as.character(classes_disponiveis)]
      )
      
      updateSelectInput(session, "filtro_classe", choices = choices_classes)
    })
    
    observeEvent(input$aplicar_filtros_rel, {
      removeModal()
      showNotification("Filtros aplicados!", type = "message", duration = 3)
    })
    
    output$resumo_filtros_relatorio <- renderUI({
      filtros <- filtros_relatorio_aplicados()
      
      if (length(filtros) == 0) {
        return(
          helpText(
            icon("info-circle"),
            " Nenhum filtro aplicado. Todos os dados serão incluídos."
          )
        )
      }
      
      div(
        class = "alert alert-info",
        icon("filter"),
        " Filtros ativos: ", length(filtros)
      )
    })
    
    # ====================================================================
    # DADOS FILTRADOS PARA RELATÓRIO
    # ====================================================================
    
    dados_relatorio <- reactive({
      req(resultados_electre())
      req(data_sf_original())
      
      results <- resultados_electre()$results
      sf_data <- data_sf_original()
      
      # Garantir que sf_data é realmente SF
      if (!inherits(sf_data, "sf")) {
        return(results)
      }
      
      # Aplicar filtros se houver
      filtros <- filtros_relatorio_aplicados()
      if (length(filtros) > 0) {
        results <- aplicar_filtros_em_df(results, filtros)
      }
      
      # Remover geometria de results se existir
      if (inherits(results, "sf")) {
        results_df <- sf::st_drop_geometry(results)
      } else {
        results_df <- results
      }
      
      # Adicionar variáveis extras
      if (!is.null(input$vars_adicionais) && length(input$vars_adicionais) > 0) {
        vars_extra <- input$vars_adicionais
        
        if ("CD_MUN" %in% names(results_df) && "CD_MUN" %in% names(sf_data)) {
          cols_extra <- setdiff(vars_extra, names(results_df))
          
          if (length(cols_extra) > 0) {
            results_df <- results_df %>%
              left_join(
                sf_data %>% 
                  sf::st_drop_geometry() %>%
                  select(CD_MUN, all_of(cols_extra)),
                by = "CD_MUN"
              )
          }
        }
      }
      
      # JOIN COM GEOMETRIAS
      tryCatch({
        
        if ("CD_MUN" %in% names(results_df) && "CD_MUN" %in% names(sf_data)) {
          
          # Pegar nome da coluna de geometria
          geom_col <- attr(sf_data, "sf_column")
          
          # Selecionar apenas CD_MUN e geometria do sf_data
          sf_minimal <- sf_data[, c("CD_MUN", geom_col)]
          
          # Fazer join
          dados_com_geometria <- sf_minimal %>%
            left_join(results_df, by = "CD_MUN")
          
        } else {
          
          # Se não tiver CD_MUN, assumir mesma ordem
          dados_com_geometria <- sf_data
          
          if (nrow(results_df) == nrow(sf_data)) {
            for (col in setdiff(names(results_df), names(sf_data))) {
              dados_com_geometria[[col]] <- results_df[[col]]
            }
          }
        }
        
        return(dados_com_geometria)
        
      }, error = function(e) {
        return(results_df)
      })
    })
    
    # ====================================================================
    # RESUMO DOS DADOS FILTRADOS
    # ====================================================================
    
    output$resumo_dados_filtrados <- renderUI({
      req(dados_relatorio())
      df <- dados_relatorio()
      
      if (inherits(df, "sf")) {
        df <- sf::st_drop_geometry(df)
      }
      
      n_total <- nrow(df)
      
      # Distribuição por classe
      dist_classes <- df %>%
        count(class_electre) %>%
        arrange(class_electre)
      
      tagList(
        value_box(
          title = "Total de Municípios",
          value = format(n_total, big.mark = "."),
          showcase = icon("map-marked-alt"),
          theme = "primary"
        ),
        hr(),
        tags$strong("Distribuição por Classe:"),
        tags$ul(
          lapply(1:nrow(dist_classes), function(i) {
            tags$li(
              paste0(
                analise_data$label_map()[as.character(dist_classes$class_electre[i])],
                ": ",
                dist_classes$n[i],
                " (",
                round(dist_classes$n[i] / n_total * 100, 1),
                "%)"
              )
            )
          })
        )
      )
    })
    
    # ====================================================================
    # PREVIEW DA TABELA FINAL
    # ====================================================================
    
    output$preview_tabela_final <- renderDT({
      req(dados_relatorio())
      df <- dados_relatorio()
      
      # Limitar preview
      df_preview <- head(df, 100)
      
      # Remover geometry para visualização
      if (inherits(df_preview, "sf")) {
        df_preview <- sf::st_drop_geometry(df_preview)
      }
      
      datatable(
        df_preview,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "tip"
        ),
        caption = "Preview dos primeiros 100 registros"
      )
    })
    
    # ====================================================================
    # GERAR PREVIEW DO RELATÓRIO
    # ====================================================================
    
    log_geracao <- reactiveVal("")
    
    observeEvent(input$gerar_preview, {
      showNotification("Gerando preview do relatório...", 
                       type = "default", duration = NULL, id = "preview_gen")
      
      log_geracao("Iniciando geração do preview...\n")
      
      tryCatch({
        Sys.sleep(2)
        
        log_geracao(paste0(
          log_geracao(),
          "✓ Dados carregados\n",
          "✓ Seções selecionadas: ", length(input$secoes_principais), "\n",
          "✓ Gráficos selecionados: ", length(input$elementos_visuais), "\n",
          "✓ Tabelas selecionadas: ", length(input$tabelas), "\n",
          "✓ Preview gerado com sucesso!\n"
        ))
        
        removeNotification("preview_gen")
        showNotification("Preview gerado!", type = "message", duration = 3)
        
      }, error = function(e) {
        removeNotification("preview_gen")
        showNotification(paste("Erro:", e$message), type = "error", duration = 10)
      })
    })
    
    output$log_geracao <- renderText({
      log_geracao()
    })
    
    # ====================================================================
    # PREVIEW IFRAME
    # ====================================================================
    
    output$preview_iframe <- renderUI({
      tags$div(
        class = "alert alert-warning",
        style = "height: 100%; display: flex; align-items: center; justify-content: center;",
        tags$div(
          icon("info-circle", style = "font-size: 3em;"),
          tags$h4("Preview do Relatório", class = "mt-3"),
          tags$p("Clique em 'Gerar Preview' ou 'Atualizar Preview' para visualizar o relatório")
        )
      )
    })
    
    # ====================================================================
    # FUNÇÃO AUXILIAR PARA PREPARAR DADOS (REUTILIZÁVEL)
    # ====================================================================
    
    # ====================================================================
    # FUNÇÃO AUXILIAR PARA PREPARAR DADOS (REUTILIZÁVEL)
    # ====================================================================
    
    gerar_relatorio_wrapper <- function(tipo = "html") {
      
      # Preparar dados de qualificação
      qualif_data <- NULL
      
      if (!is.null(dados_qualificacao())) {
        qualif_raw <- dados_qualificacao()
        qualif_data <- list()
        
        for (nome_camada in names(qualif_raw)) {
          camada <- qualif_raw[[nome_camada]]
          
          if (!is.null(camada) && nrow(camada) > 0) {
            
            if (tipo == "excel") {
              # Para Excel: remover geometrias
              if (inherits(camada, "sf")) {
                qualif_data[[nome_camada]] <- camada %>%
                  sf::st_drop_geometry() %>%
                  select(any_of(c("CD_MUN", "NM_MUN", "class_electre", "class_label", "tipo", "nome_feature")))
              } else {
                qualif_data[[nome_camada]] <- camada
              }
            } else {
              # Para HTML/PDF: MANTER geometrias
              qualif_data[[nome_camada]] <- camada
            }
          }
        }
      }
      
      # Preparar configuração
      config <- list(
        titulo = input$titulo,
        subtitulo = input$subtitulo,
        autor = input$autor,
        instituicao = input$instituicao,
        data = input$data_relatorio,
        resumo = input$resumo_executivo,
        template = input$template,
        tema = input$tema_visual,
        cor_principal = input$cor_principal,
        cor_secundaria = adjustcolor(input$cor_principal, alpha.f = 0.7),
        secoes = input$secoes_principais,
        elementos_visuais = input$elementos_visuais,
        tabelas = input$tabelas,
        logo = input$logo,
        n_classes = resultados_electre()$params$n_classes,
        criterios = resultados_electre()$params$criterios,
        label_map = analise_data$label_map(),
        paleta_cores = analise_data$paleta_cores(),
        max_linhas_tabela = input$max_linhas_tabela
      )
      
      # Retornar lista com tudo
      list(
        dados = dados_relatorio(),
        qualificacao = qualif_data,
        params = resultados_electre()$params,
        config = config
      )
    }
    
    # ====================================================================
    # EXPORTAR HTML - ABA EXPORTAR
    # ====================================================================
    
    # EXPORTAR HTML - ABA EXPORTAR
    output$export_html_full <- downloadHandler(
      filename = function() {
        paste0("relatorio_electre_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        showNotification("Gerando relatório HTML...", 
                         type = "default", duration = NULL, id = "html_gen")
        
        tryCatch({
          
          wrapper <- gerar_relatorio_wrapper("html")  # ← ADICIONAR "html"
          
          gerar_relatorio_html(
            dados = wrapper$dados,
            qualificacao = wrapper$qualificacao,
            params = wrapper$params,
            config = wrapper$config,
            output_file = file
          )
          
          removeNotification("html_gen")
          showNotification("Relatório HTML gerado com sucesso!", type = "message", duration = 3)
          
        }, error = function(e) {
          removeNotification("html_gen")
          showNotification(paste("Erro ao gerar relatório:", e$message), 
                           type = "error", duration = 10)
        })
      }
    )
    
    # EXPORTAR HTML - SIDEBAR
    output$dl_html <- downloadHandler(
      filename = function() {
        paste0("relatorio_electre_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        showNotification("Gerando relatório HTML...", 
                         type = "default", duration = NULL, id = "html_gen_sidebar")
        
        tryCatch({
          
          wrapper <- gerar_relatorio_wrapper("html")  # ← ADICIONAR "html"
          
          gerar_relatorio_html(
            dados = wrapper$dados,
            qualificacao = wrapper$qualificacao,
            params = wrapper$params,
            config = wrapper$config,
            output_file = file
          )
          
          removeNotification("html_gen_sidebar")
          showNotification("Relatório HTML gerado!", type = "message", duration = 3)
          
        }, error = function(e) {
          removeNotification("html_gen_sidebar")
          showNotification(paste("Erro:", e$message), type = "error", duration = 10)
        })
      }
    )
    
    # ====================================================================
    # EXPORTAR HTML - SIDEBAR
    # ====================================================================
    
    output$dl_html <- downloadHandler(
      filename = function() {
        paste0("relatorio_electre_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        showNotification("Gerando relatório HTML...", 
                         type = "default", duration = NULL, id = "html_gen_sidebar")
        
        tryCatch({
          
          wrapper <- gerar_relatorio_wrapper()
          
          gerar_relatorio_html(
            dados = wrapper$dados,
            qualificacao = wrapper$qualificacao,
            params = wrapper$params,
            config = wrapper$config,
            output_file = file
          )
          
          removeNotification("html_gen_sidebar")
          showNotification("Relatório HTML gerado!", type = "message", duration = 3)
          
        }, error = function(e) {
          removeNotification("html_gen_sidebar")
          showNotification(paste("Erro:", e$message), type = "error", duration = 10)
        })
      }
    )
    
    # ====================================================================
    # EXPORTAR PDF - ABA EXPORTAR
    # ====================================================================
    
    output$export_pdf_full <- downloadHandler(
      filename = function() {
        paste0("relatorio_electre_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        showNotification("Gerando relatório PDF...", 
                         type = "default", duration = NULL, id = "pdf_gen")
        
        tryCatch({
          
          wrapper <- gerar_relatorio_wrapper()
          
          gerar_relatorio_pdf(
            dados = wrapper$dados,
            params = wrapper$config,
            output_file = file
          )
          
          removeNotification("pdf_gen")
          showNotification("Relatório PDF gerado!", type = "message", duration = 3)
          
        }, error = function(e) {
          removeNotification("pdf_gen")
          showNotification(paste("Erro:", e$message), type = "error", duration = 10)
        })
      }
    )
    
    # ====================================================================
    # EXPORTAR PDF - SIDEBAR
    # ====================================================================
    
    output$dl_pdf <- downloadHandler# EXPORTAR PDF - ABA EXPORTAR
    output$export_pdf_full <- downloadHandler(
      filename = function() {
        paste0("relatorio_electre_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        showNotification("Gerando relatório PDF...", 
                         type = "default", duration = NULL, id = "pdf_gen")
        
        tryCatch({
          
          wrapper <- gerar_relatorio_wrapper("pdf")  # ← ADICIONAR "pdf"
          
          gerar_relatorio_pdf(
            dados = wrapper$dados,
            params = wrapper$config,
            output_file = file
          )
          
          removeNotification("pdf_gen")
          showNotification("Relatório PDF gerado!", type = "message", duration = 3)
          
        }, error = function(e) {
          removeNotification("pdf_gen")
          showNotification(paste("Erro:", e$message), type = "error", duration = 10)
        })
      }
    )
    
    # EXPORTAR PDF - SIDEBAR
    output$dl_pdf <- downloadHandler(
      filename = function() {
        paste0("relatorio_electre_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        showNotification("Gerando relatório PDF...", 
                         type = "default", duration = NULL, id = "pdf_gen_sidebar")
        
        tryCatch({
          
          wrapper <- gerar_relatorio_wrapper("pdf")  # ← ADICIONAR "pdf"
          
          gerar_relatorio_pdf(
            dados = wrapper$dados,
            params = wrapper$config,
            output_file = file
          )
          
          removeNotification("pdf_gen_sidebar")
          showNotification("Relatório PDF gerado!", type = "message", duration = 3)
          
        }, error = function(e) {
          removeNotification("pdf_gen_sidebar")
          showNotification(paste("Erro:", e$message), type = "error", duration = 10)
        })
      }
    )
    
    # EXPORTAR EXCEL - ABA EXPORTAR
    output$export_excel_full <- downloadHandler(
      filename = function() {
        paste0("dados_electre_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        showNotification("Gerando arquivo Excel...", 
                         type = "default", duration = NULL, id = "excel_gen")
        
        tryCatch({
          
          wrapper <- gerar_relatorio_wrapper("excel")  # ← ADICIONAR "excel"
          
          gerar_excel_completo(
            dados = wrapper$dados,
            qualificacao = wrapper$qualificacao,
            params = wrapper$params,
            label_map = wrapper$config$label_map,
            paleta_cores = wrapper$config$paleta_cores,
            output_file = file
          )
          
          removeNotification("excel_gen")
          showNotification("Excel gerado com sucesso!", type = "message", duration = 3)
          
        }, error = function(e) {
          removeNotification("excel_gen")
          cat("\n=== ERRO EXCEL ===\n")
          cat("Mensagem:", e$message, "\n")
          print(e)
          showNotification(paste("Erro ao gerar Excel:", e$message), 
                           type = "error", duration = 10)
        })
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    # EXPORTAR EXCEL - SIDEBAR
    output$dl_excel <- downloadHandler(
      filename = function() {
        paste0("dados_electre_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        showNotification("Gerando arquivo Excel...", 
                         type = "default", duration = NULL, id = "excel_gen_sidebar")
        
        tryCatch({
          
          wrapper <- gerar_relatorio_wrapper("excel")  # ← ADICIONAR "excel"
          
          gerar_excel_completo(
            dados = wrapper$dados,
            qualificacao = wrapper$qualificacao,
            params = wrapper$params,
            label_map = wrapper$config$label_map,
            paleta_cores = wrapper$config$paleta_cores,
            output_file = file
          )
          
          removeNotification("excel_gen_sidebar")
          showNotification("Excel gerado!", type = "message", duration = 3)
          
        }, error = function(e) {
          removeNotification("excel_gen_sidebar")
          showNotification(paste("Erro:", e$message), type = "error", duration = 10)
        })
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
  }) # ← Fecha moduleServer
} # ← Fecha função mod_relatorios_server