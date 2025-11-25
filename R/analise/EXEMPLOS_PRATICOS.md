# DIAGRAMA DE DEPENDÊNCIAS E EXEMPLOS PRÁTICOS

## 📊 Diagrama de Dependências

```
┌─────────────────────────────────────────────────────────────┐
│                     mod_analise.R                            │
│                  (Arquivo Principal)                         │
└───────────────────────┬─────────────────────────────────────┘
                        │
        ┌───────────────┼───────────────┐
        │               │               │
        ▼               ▼               ▼
┌───────────┐   ┌───────────┐   ┌───────────┐
│ config.R  │   │  utils.R  │   │filtros.R  │
└───────────┘   └─────┬─────┘   └─────┬─────┘
                      │               │
        ┌─────────────┴───────┬───────┴───────┬─────────────┐
        │                     │               │             │
        ▼                     ▼               ▼             ▼
┌──────────────┐      ┌──────────────┐  ┌──────────┐  ┌──────────┐
│  perfis.R    │      │electre_core.R│  │spatial.R │  │  ui_*    │
└──────────────┘      └──────────────┘  └──────────┘  └──────────┘
        │                     │               │             │
        └─────────────────────┴───────────────┴─────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        │                     │                     │
        ▼                     ▼                     ▼
┌──────────────┐      ┌──────────────┐      ┌──────────────┐
│outputs_*.R   │      │ downloads.R  │      │ Outros...    │
└──────────────┘      └──────────────┘      └──────────────┘
```

### Legenda de Dependências

- **config.R**: Independente (base)
- **utils.R**: Depende apenas de config.R
- **filtros.R**: Usa utils.R
- **perfis.R**: Usa utils.R, config.R
- **electre_core.R**: Usa utils.R, filtros.R
- **spatial.R**: Usa utils.R
- **outputs_*.R**: Usam múltiplos arquivos anteriores

## 🎯 Exemplos Práticos de Uso

### Exemplo 1: Criar Novo Módulo de Análise Simplificada

```r
# ============================================
# R/mod_analise_simples.R
# ============================================
# Módulo simplificado que reutiliza componentes

# Carregar apenas os componentes necessários
source("R/analise/config.R", local = TRUE)
source("R/analise/utils.R", local = TRUE)
source("R/analise/electre_core.R", local = TRUE)

mod_simples_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Análise ELECTRE Simplificada"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("criterios"), "Critérios:", choices = NULL, multiple = TRUE),
        numericInput(ns("n_classes"), "Classes:", value = 5, min = 3, max = 7),
        actionButton(ns("executar"), "Executar", class = "btn-primary")
      ),
      mainPanel(
        plotOutput(ns("resultado"))
      )
    )
  )
}

mod_simples_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    
    # Reutilizar funções de normalização
    ranges <- reactive({
      calcular_ranges_real(dados(), input$criterios)
    })
    
    # Reutilizar paleta e labels
    cores <- reactive({
      gerar_paleta_cores(input$n_classes)
    })
    
    labels <- reactive({
      gerar_labels_classes(input$n_classes)
    })
    
    # Resultado
    resultado <- eventReactive(input$executar, {
      # Usar executar_electre() com parâmetros simplificados
      executar_electre(
        data_plain = dados,
        criterios = reactive(input$criterios),
        # ... outros parâmetros
      )
    })
    
    output$resultado <- renderPlot({
      req(resultado())
      # Plotar usando cores()
      barplot(table(resultado()$results$class_electre), 
              col = cores(), 
              names.arg = labels())
    })
  })
}
```

### Exemplo 2: Sistema de Filtros em Relatórios

```r
# ============================================
# R/mod_relatorios.R
# ============================================
# Módulo de relatórios que usa filtros

source("R/analise/filtros.R", local = TRUE)
source("R/analise/utils.R", local = TRUE)

mod_relatorios_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      criar_botao_filtros_ui(ns, "filtro_relatorio")
    ),
    mainPanel(
      DTOutput(ns("tabela")),
      downloadButton(ns("download_pdf"), "Download Relatório PDF")
    )
  )
}

mod_relatorios_server <- function(id, dados_electre) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Sistema de filtros
    filtros_aplicados <- reactiveVal(list())
    
    criar_sistema_filtros_modal(
      session = session,
      ns = ns,
      id = "filtro_relatorio",
      results_reactive = dados_electre,
      filtros_aplicados = filtros_aplicados
    )
    
    # Dados filtrados
    dados_filtrados <- reactive({
      aplicar_filtros_em_df(dados_electre(), filtros_aplicados())
    })
    
    # Tabela
    output$tabela <- renderDT({
      datatable(dados_filtrados())
    })
    
    # Download
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("relatorio_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        # Gerar PDF com dados_filtrados()
        rmarkdown::render(
          "templates/relatorio_template.Rmd",
          output_file = file,
          params = list(dados = dados_filtrados())
        )
      }
    )
  })
}
```

### Exemplo 3: Validação de Dados com Utils

```r
# ============================================
# R/mod_validacao.R
# ============================================
# Módulo que valida dados antes do ELECTRE

source("R/analise/utils.R", local = TRUE)
source("R/analise/config.R", local = TRUE)

validar_dados_electre <- function(df, criterios) {
  
  erros <- c()
  avisos <- c()
  
  # 1. Verificar se critérios existem
  criterios_faltando <- setdiff(criterios, names(df))
  if (length(criterios_faltando) > 0) {
    erros <- c(erros, paste("Critérios não encontrados:", 
                            paste(criterios_faltando, collapse = ", ")))
  }
  
  # 2. Verificar NAs
  for (crit in criterios) {
    if (crit %in% names(df)) {
      pct_na <- mean(is.na(df[[crit]])) * 100
      if (pct_na > 50) {
        erros <- c(erros, paste0(crit, ": ", round(pct_na, 1), "% de NAs"))
      } else if (pct_na > 10) {
        avisos <- c(avisos, paste0(crit, ": ", round(pct_na, 1), "% de NAs"))
      }
    }
  }
  
  # 3. Verificar variância
  for (crit in criterios) {
    if (crit %in% names(df)) {
      if (var(df[[crit]], na.rm = TRUE) == 0) {
        erros <- c(erros, paste0(crit, ": variância zero (constante)"))
      }
    }
  }
  
  # 4. Calcular ranges e verificar outliers
  ranges <- calcular_ranges_real(df, criterios)
  
  for (crit in criterios) {
    r <- ranges[[crit]]
    q1 <- quantile(df[[crit]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[crit]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    
    outliers_sup <- sum(df[[crit]] > (q3 + 3*iqr), na.rm = TRUE)
    outliers_inf <- sum(df[[crit]] < (q1 - 3*iqr), na.rm = TRUE)
    
    if (outliers_sup + outliers_inf > 0) {
      avisos <- c(avisos, paste0(crit, ": ", outliers_sup + outliers_inf, " outliers extremos"))
    }
  }
  
  list(
    valido = length(erros) == 0,
    erros = erros,
    avisos = avisos,
    ranges = ranges
  )
}

# Usar no módulo
mod_validacao_server <- function(id, dados, criterios) {
  moduleServer(id, function(input, output, session) {
    
    validacao <- reactive({
      req(dados(), criterios())
      validar_dados_electre(dados(), criterios())
    })
    
    output$relatorio_validacao <- renderUI({
      v <- validacao()
      
      tagList(
        if (!v$valido) {
          div(class = "alert alert-danger",
              icon("exclamation-triangle"),
              tags$strong(" Erros encontrados:"),
              tags$ul(lapply(v$erros, tags$li))
          )
        },
        if (length(v$avisos) > 0) {
          div(class = "alert alert-warning",
              icon("exclamation-circle"),
              tags$strong(" Avisos:"),
              tags$ul(lapply(v$avisos, tags$li))
          )
        },
        if (v$valido && length(v$avisos) == 0) {
          div(class = "alert alert-success",
              icon("check-circle"),
              " Dados validados com sucesso!"
          )
        }
      )
    })
  })
}
```

### Exemplo 4: Exportar Configurações ELECTRE

```r
# ============================================
# R/exportar_configuracoes.R
# ============================================
# Salvar/carregar configurações ELECTRE

source("R/analise/config.R", local = TRUE)
source("R/analise/utils.R", local = TRUE)

#' Exportar configuração ELECTRE para JSON
exportar_config_electre <- function(criterios, pesos, perfis, limiares, 
                                     lambda, rule, n_classes, filepath) {
  
  config <- list(
    versao = "1.0",
    data = Sys.Date(),
    parametros = list(
      n_classes = n_classes,
      criterios = criterios,
      pesos = setNames(as.list(pesos), criterios),
      perfis = perfis,
      limiares = limiares,
      lambda = lambda,
      rule = rule
    ),
    cores = gerar_paleta_cores(n_classes),
    labels = gerar_labels_classes(n_classes)
  )
  
  jsonlite::write_json(config, filepath, pretty = TRUE, auto_unbox = TRUE)
  
  message("Configuração exportada para: ", filepath)
}

#' Importar configuração ELECTRE de JSON
importar_config_electre <- function(filepath) {
  
  if (!file.exists(filepath)) {
    stop("Arquivo não encontrado: ", filepath)
  }
  
  config <- jsonlite::read_json(filepath, simplifyVector = TRUE)
  
  # Validar versão
  if (is.null(config$versao)) {
    warning("Versão da configuração não especificada")
  }
  
  # Retornar lista estruturada
  list(
    versao = config$versao,
    data = config$data,
    n_classes = config$parametros$n_classes,
    criterios = config$parametros$criterios,
    pesos = unlist(config$parametros$pesos),
    perfis = config$parametros$perfis,
    limiares = config$parametros$limiares,
    lambda = config$parametros$lambda,
    rule = config$parametros$rule,
    cores = config$cores,
    labels = config$labels
  )
}

# Uso no Shiny
observeEvent(input$btn_exportar_config, {
  req(resultados_electre())
  
  params <- resultados_electre()$params
  
  filepath <- file.path("configs", paste0("electre_config_", Sys.Date(), ".json"))
  
  exportar_config_electre(
    criterios = params$criterios,
    pesos = params$pesos,
    perfis = params$perfis,
    limiares = params$limiares,
    lambda = params$lambda,
    rule = params$rule,
    n_classes = params$n_classes,
    filepath = filepath
  )
  
  showNotification("Configuração exportada!", type = "message")
})

observeEvent(input$file_import_config, {
  req(input$file_import_config)
  
  config <- importar_config_electre(input$file_import_config$datapath)
  
  # Atualizar UI com config importada
  updateNumericInput(session, "n_classes", value = config$n_classes)
  updateSelectInput(session, "criterios_sel", selected = config$criterios)
  updateNumericInput(session, "lambda_cut", value = config$lambda)
  updateSelectInput(session, "rule", selected = config$rule)
  
  # Atualizar pesos
  for (crit in config$criterios) {
    updateSliderInput(session, paste0("peso_", crit), 
                      value = config$pesos[[crit]])
  }
  
  showNotification("Configuração importada!", type = "message")
})
```

### Exemplo 5: Testes Unitários

```r
# ============================================
# tests/testthat/test_utils.R
# ============================================

library(testthat)
source("R/analise/utils.R")

test_that("clamp01 funciona corretamente", {
  expect_equal(clamp01(c(-1, 0, 0.5, 1, 2)), c(0, 0, 0.5, 1, 1))
  expect_equal(clamp01(NA), NA_real_)
})

test_that("normalizar_pesos funciona", {
  pesos <- c(2, 3, 5)
  norm <- normalizar_pesos(pesos)
  
  expect_equal(sum(norm), 1)
  expect_equal(norm, c(0.2, 0.3, 0.5))
})

test_that("to_unit e to_real são inversos", {
  ranges <- list(temp = c(0, 100))
  valores <- c(0, 25, 50, 75, 100)
  
  u <- to_unit(valores, "temp", "benefit", ranges)
  back <- to_real(u, "temp", "benefit", ranges)
  
  expect_equal(back, valores, tolerance = 1e-10)
})

# ============================================
# tests/testthat/test_filtros.R
# ============================================

test_that("aplicar_filtros_em_df funciona", {
  df <- data.frame(
    a = 1:10,
    b = letters[1:10]
  )
  
  filtros <- list(
    list(campo = "a", operador = ">", valor = 5)
  )
  
  resultado <- aplicar_filtros_em_df(df, filtros)
  
  expect_equal(nrow(resultado), 5)
  expect_equal(resultado$a, 6:10)
})
```

---

## 🚀 Começando

Para começar a usar a estrutura modular:

1. **Copie os arquivos** para a estrutura proposta
2. **Teste o módulo principal** com dados de exemplo
3. **Explore componentes individuais** para entender cada função
4. **Crie seus próprios módulos** reutilizando componentes
5. **Contribua** com melhorias e novos componentes

## 📞 Suporte

Para dúvidas ou sugestões, consulte o README_MODULAR.md principal.
