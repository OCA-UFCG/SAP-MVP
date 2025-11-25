# =====================================================================
# FUNÇÃO DE GERAÇÃO DE EXCEL COMPLETO - VERSÃO PROFISSIONAL
# =====================================================================

gerar_excel_completo <- function(dados, 
                                 qualificacao = NULL,
                                 params = NULL,
                                 label_map = NULL,
                                 paleta_cores = NULL,
                                 output_file) {
  
  require(openxlsx)
  require(dplyr)
  
  cat("\n=== Gerando Excel Completo ===\n")
  
  # ====================================================================
  # PREPARAR DADOS
  # ====================================================================
  
  # Converter SF para data.frame se necessário
  if (inherits(dados, "sf")) {
    dados_df <- sf::st_drop_geometry(dados)
  } else {
    dados_df <- dados
  }
  
  # Configurações padrão
  n_classes <- length(unique(dados_df$class_electre))
  
  if (is.null(label_map)) {
    label_map <- setNames(paste0("Classe ", 1:n_classes), as.character(1:n_classes))
  }
  
  if (is.null(paleta_cores)) {
    paleta_cores <- setNames(
      c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c")[1:n_classes],
      as.character(1:n_classes)
    )
  }
  
  # ====================================================================
  # ADICIONAR CONTAGENS DE QUALIFICAÇÃO POR MUNICÍPIO
  # ====================================================================
  
  if (!is.null(qualificacao) && "CD_MUN" %in% names(dados_df)) {
    
    cat("Adicionando contagens de qualificação...\n")
    
    nomes_camadas <- c(
      quilombolas = "N_Quilombolas",
      assentamentos = "N_Assentamentos",
      indigenas = "N_T_Indigenas",
      ensino = "N_Inst_Ensino",
      prisoes = "N_U_Prisionais",
      sementes = "N_B_Sementes"
    )
    
    # Inicializar colunas com zero
    for (col_nome in nomes_camadas) {
      dados_df[[col_nome]] <- 0
    }
    
    # Contar ocorrências por município
    for (tipo in names(nomes_camadas)) {
      if (!is.null(qualificacao[[tipo]]) && nrow(qualificacao[[tipo]]) > 0) {
        
        camada_df <- qualificacao[[tipo]]
        
        # Converter SF para df se necessário
        if (inherits(camada_df, "sf")) {
          camada_df <- sf::st_drop_geometry(camada_df)
        }
        
        # Contar por CD_MUN
        if ("CD_MUN" %in% names(camada_df)) {
          contagens <- camada_df %>%
            count(CD_MUN, name = "n")
          
          # Atualizar dados_df
          for (i in 1:nrow(contagens)) {
            cd_mun <- contagens$CD_MUN[i]
            n_count <- contagens$n[i]
            
            idx <- which(dados_df$CD_MUN == cd_mun)
            if (length(idx) > 0) {
              dados_df[[nomes_camadas[tipo]]][idx] <- n_count
            }
          }
        }
      }
    }
    
    # Adicionar coluna TOTAL
    dados_df$Total_Qualificacoes <- rowSums(dados_df[, nomes_camadas], na.rm = TRUE)
    
    cat("✓ Colunas de qualificação adicionadas\n")
  }
  
  # ====================================================================
  # CRIAR WORKBOOK
  # ====================================================================
  
  wb <- createWorkbook()
  
  # ====================================================================
  # ESTILOS
  # ====================================================================
  
  # Cabeçalho principal
  header_style <- createStyle(
    fontSize = 11,
    fontColour = "#FFFFFF",
    halign = "center",
    valign = "center",
    fgFill = "#2c3e50",
    border = "TopBottomLeftRight",
    borderColour = "#2c3e50",
    textDecoration = "bold",
    wrapText = TRUE
  )
  
  # Título
  titulo_style <- createStyle(
    fontSize = 16,
    fontColour = "#2c3e50",
    halign = "left",
    valign = "center",
    textDecoration = "bold"
  )
  
  # Subtítulo
  subtitulo_style <- createStyle(
    fontSize = 12,
    fontColour = "#34495e",
    halign = "left",
    valign = "center",
    textDecoration = "italic"
  )
  
  # Números
  numero_style <- createStyle(numFmt = "#,##0.00")
  inteiro_style <- createStyle(numFmt = "#,##0")
  percent_style <- createStyle(numFmt = "0.00%")
  
  # Estilo para colunas de qualificação (destaque)
  qualif_header_style <- createStyle(
    fontSize = 11,
    fontColour = "#FFFFFF",
    halign = "center",
    valign = "center",
    fgFill = "#16a085",
    border = "TopBottomLeftRight",
    borderColour = "#16a085",
    textDecoration = "bold",
    wrapText = TRUE
  )
  
  cat("✓ Workbook criado\n")
  
  # ====================================================================
  # ABA 1: CAPA
  # ====================================================================
  
  cat("Gerando aba: Capa...\n")
  addWorksheet(wb, "Capa")
  
  # Título
  writeData(wb, "Capa", "Relatório ELECTRE Tri-B", startRow = 2, startCol = 2)
  addStyle(wb, "Capa", titulo_style, rows = 2, cols = 2)
  
  # Informações
  info_data <- data.frame(
    Campo = c(
      "Data de Geração:",
      "Total de Registros:",
      "Número de Classes:",
      "Método:"
    ),
    Valor = c(
      format(Sys.Date(), "%d/%m/%Y"),
      format(nrow(dados_df), big.mark = "."),
      n_classes,
      "ELECTRE Tri-B"
    )
  )
  
  writeData(wb, "Capa", info_data, startRow = 4, startCol = 2, colNames = FALSE)
  addStyle(wb, "Capa", createStyle(fontColour = "#2c3e50", textDecoration = "bold"), 
           rows = 4:7, cols = 2, gridExpand = TRUE)
  
  setColWidths(wb, "Capa", cols = 2:3, widths = c(25, 30))
  
  cat("✓ Capa gerada\n")
  
  # ====================================================================
  # ABA 2: DADOS COMPLETOS (COM QUALIFICAÇÕES)
  # ====================================================================
  
  cat("Gerando aba: Dados Completos...\n")
  addWorksheet(wb, "Dados Completos")
  
  # Escrever dados
  writeData(wb, "Dados Completos", dados_df, headerStyle = header_style, borders = "all")
  
  # Identificar colunas de qualificação
  cols_qualif <- grep("^N_", names(dados_df))
  col_total <- which(names(dados_df) == "Total_Qualificacoes")
  
  # Aplicar estilo diferenciado nos cabeçalhos de qualificação
  if (length(cols_qualif) > 0) {
    for (col_idx in cols_qualif) {
      addStyle(wb, "Dados Completos", qualif_header_style, rows = 1, cols = col_idx)
    }
  }
  
  if (length(col_total) > 0) {
    addStyle(wb, "Dados Completos", 
             createStyle(fontSize = 11, fontColour = "#FFFFFF", halign = "center",
                         fgFill = "#e67e22", textDecoration = "bold"),
             rows = 1, cols = col_total)
  }
  
  # Auto-ajustar colunas
  setColWidths(wb, "Dados Completos", cols = 1:ncol(dados_df), widths = "auto")
  
  # Adicionar filtros
  addFilter(wb, "Dados Completos", row = 1, cols = 1:ncol(dados_df))
  
  # Congelar painéis
  freezePane(wb, "Dados Completos", firstRow = TRUE)
  
  # Formatação condicional por classe
  if ("class_electre" %in% names(dados_df)) {
    col_idx <- which(names(dados_df) == "class_electre")
    
    for (i in 1:n_classes) {
      style_classe <- createStyle(
        fgFill = paleta_cores[i],
        fontColour = "#FFFFFF",
        textDecoration = "bold"
      )
      
      conditionalFormatting(
        wb, "Dados Completos",
        cols = col_idx,
        rows = 2:(nrow(dados_df) + 1),
        rule = paste0("=$", LETTERS[col_idx], "2=", i),
        style = style_classe
      )
    }
  }
  
  # Formatar colunas de qualificação como inteiros
  if (length(cols_qualif) > 0) {
    addStyle(wb, "Dados Completos", inteiro_style,
             rows = 2:(nrow(dados_df) + 1), cols = cols_qualif, 
             gridExpand = TRUE, stack = TRUE)
  }
  
  if (length(col_total) > 0) {
    addStyle(wb, "Dados Completos", 
             createStyle(numFmt = "#,##0", textDecoration = "bold"),
             rows = 2:(nrow(dados_df) + 1), cols = col_total, 
             gridExpand = TRUE, stack = TRUE)
  }
  
  cat("✓ Dados Completos gerados (com", length(cols_qualif), "colunas de qualificação)\n")
  
  # ====================================================================
  # ABA 3: RESUMO POR CLASSE
  # ====================================================================
  
  cat("Gerando aba: Resumo por Classe...\n")
  
  if ("class_electre" %in% names(dados_df)) {
    addWorksheet(wb, "Resumo por Classe")
    
    # Calcular resumo
    resumo <- dados_df %>%
      count(class_electre) %>%
      mutate(
        Classe = label_map[as.character(class_electre)],
        Quantidade = n,
        Percentual = n / sum(n)
      ) %>%
      arrange(class_electre) %>%
      select(Classe, Quantidade, Percentual)
    
    # Escrever dados
    writeData(wb, "Resumo por Classe", resumo, startRow = 1, headerStyle = header_style)
    
    # Formatação
    setColWidths(wb, "Resumo por Classe", cols = 1:3, widths = c(30, 15, 15))
    
    # Formatar números
    addStyle(wb, "Resumo por Classe", inteiro_style, 
             rows = 2:(nrow(resumo) + 1), cols = 2, gridExpand = TRUE)
    addStyle(wb, "Resumo por Classe", percent_style, 
             rows = 2:(nrow(resumo) + 1), cols = 3, gridExpand = TRUE)
    
    # Colorir células de Classe
    for (i in 1:nrow(resumo)) {
      style_linha <- createStyle(
        fgFill = paleta_cores[i],
        fontColour = "#FFFFFF",
        textDecoration = "bold",
        halign = "left"
      )
      addStyle(wb, "Resumo por Classe", style_linha, rows = i + 1, cols = 1)
    }
    
    # Adicionar linha de total
    writeData(wb, "Resumo por Classe", "TOTAL", 
              startRow = nrow(resumo) + 2, startCol = 1)
    writeFormula(wb, "Resumo por Classe", 
                 startRow = nrow(resumo) + 2, startCol = 2,
                 x = paste0("SUM(B2:B", nrow(resumo) + 1, ")"))
    writeFormula(wb, "Resumo por Classe", 
                 startRow = nrow(resumo) + 2, startCol = 3,
                 x = paste0("SUM(C2:C", nrow(resumo) + 1, ")"))
    
    total_style <- createStyle(
      textDecoration = "bold",
      border = "top",
      borderColour = "#000000"
    )
    addStyle(wb, "Resumo por Classe", total_style, 
             rows = nrow(resumo) + 2, cols = 1:3, gridExpand = TRUE)
    
    cat("✓ Resumo por Classe gerado\n")
  }
  
  # ====================================================================
  # ABA 4: PERFIL MÉDIO POR CLASSE
  # ====================================================================
  
  cat("Gerando aba: Perfil Médio...\n")
  
  if ("class_electre" %in% names(dados_df) && !is.null(params) && !is.null(params$criterios)) {
    addWorksheet(wb, "Perfil Médio")
    
    crits <- params$criterios
    crits_disponiveis <- intersect(crits, names(dados_df))
    
    if (length(crits_disponiveis) > 0) {
      # Calcular perfil médio
      perfil <- dados_df %>%
        group_by(class_electre) %>%
        summarise(
          across(all_of(crits_disponiveis), ~mean(.x, na.rm = TRUE)),
          N = n(),
          .groups = "drop"
        ) %>%
        mutate(Classe = label_map[as.character(class_electre)]) %>%
        select(Classe, N, all_of(crits_disponiveis))
      
      # Escrever dados
      writeData(wb, "Perfil Médio", perfil, headerStyle = header_style)
      
      # Formatação
      setColWidths(wb, "Perfil Médio", cols = 1:(ncol(perfil)), widths = "auto")
      
      # Formatar números
      addStyle(wb, "Perfil Médio", inteiro_style, 
               rows = 2:(nrow(perfil) + 1), cols = 2, gridExpand = TRUE)
      addStyle(wb, "Perfil Médio", numero_style, 
               rows = 2:(nrow(perfil) + 1), cols = 3:ncol(perfil), gridExpand = TRUE)
      
      # Colorir coluna Classe
      for (i in 1:nrow(perfil)) {
        style_linha <- createStyle(
          fgFill = paleta_cores[i],
          fontColour = "#FFFFFF",
          textDecoration = "bold"
        )
        addStyle(wb, "Perfil Médio", style_linha, rows = i + 1, cols = 1)
      }
      
      cat("✓ Perfil Médio gerado\n")
    }
  }
  
  # ====================================================================
  # ABA 5: ESTATÍSTICAS DE QUALIFICAÇÃO
  # ====================================================================
  
  if (!is.null(qualificacao) && length(qualificacao) > 0) {
    cat("Gerando aba: Qualificação Territorial...\n")
    
    addWorksheet(wb, "Qualificação Territorial")
    
    # Contar por tipo e classe
    stats_list <- list()
    
    nomes_camadas <- c(
      quilombolas = "Quilombolas",
      assentamentos = "Assentamentos",
      indigenas = "Terras Indígenas",
      ensino = "Inst. Ensino",
      prisoes = "U. Prisionais",
      sementes = "B. Sementes"
    )
    
    for (i in 1:n_classes) {
      stats <- data.frame(
        Classe = label_map[as.character(i)],
        stringsAsFactors = FALSE
      )
      
      for (tipo in names(nomes_camadas)) {
        if (!is.null(qualificacao[[tipo]]) && nrow(qualificacao[[tipo]]) > 0) {
          camada_df <- if (inherits(qualificacao[[tipo]], "sf")) {
            sf::st_drop_geometry(qualificacao[[tipo]])
          } else {
            qualificacao[[tipo]]
          }
          
          count <- sum(camada_df$class_electre == i, na.rm = TRUE)
          stats[[nomes_camadas[tipo]]] <- count
        } else {
          stats[[nomes_camadas[tipo]]] <- 0
        }
      }
      
      stats$Total <- rowSums(stats[, -1])
      stats_list[[i]] <- stats
    }
    
    df_stats <- bind_rows(stats_list)
    
    # Escrever dados
    writeData(wb, "Qualificação Territorial", df_stats, headerStyle = header_style)
    
    # Formatação
    setColWidths(wb, "Qualificação Territorial", cols = 1:ncol(df_stats), widths = "auto")
    
    # Colorir coluna Classe
    for (i in 1:nrow(df_stats)) {
      style_linha <- createStyle(
        fgFill = paleta_cores[i],
        fontColour = "#FFFFFF",
        textDecoration = "bold"
      )
      addStyle(wb, "Qualificação Territorial", style_linha, rows = i + 1, cols = 1)
    }
    
    # Formatar números
    addStyle(wb, "Qualificação Territorial", inteiro_style, 
             rows = 2:(nrow(df_stats) + 1), cols = 2:ncol(df_stats), gridExpand = TRUE)
    
    # Linha de total
    writeData(wb, "Qualificação Territorial", "TOTAL", 
              startRow = nrow(df_stats) + 2, startCol = 1)
    
    for (col in 2:ncol(df_stats)) {
      writeFormula(wb, "Qualificação Territorial", 
                   startRow = nrow(df_stats) + 2, startCol = col,
                   x = paste0("SUM(", LETTERS[col], "2:", LETTERS[col], nrow(df_stats) + 1, ")"))
    }
    
    total_style <- createStyle(
      textDecoration = "bold",
      border = "top",
      borderColour = "#000000"
    )
    addStyle(wb, "Qualificação Territorial", total_style, 
             rows = nrow(df_stats) + 2, cols = 1:ncol(df_stats), gridExpand = TRUE)
    
    cat("✓ Qualificação Territorial gerada\n")
  }
  
  # ====================================================================
  # ABA 6: PARÂMETROS ELECTRE
  # ====================================================================
  
  if (!is.null(params)) {
    cat("Gerando aba: Parâmetros...\n")
    
    addWorksheet(wb, "Parâmetros ELECTRE")
    
    # Informações gerais
    params_df <- data.frame(
      Parametro = c(
        "Número de Classes",
        "Lambda (λ)",
        "Regra de Classificação",
        "Critérios Utilizados",
        "Número de Critérios",
        "Data da Análise"
      ),
      Valor = c(
        params$n_classes,
        params$lambda,
        ifelse(params$rule == "pc", "Pessimista (pc)", "Otimista (oc)"),
        paste(params$criterios, collapse = ", "),
        length(params$criterios),
        format(Sys.Date(), "%d/%m/%Y")
      ),
      Descricao = c(
        "Número de categorias na classificação",
        "Limiar de concordância (cut-level)",
        "Regra de atribuição de classes",
        "Variáveis consideradas na análise",
        "Quantidade de critérios avaliados",
        "Data de geração deste relatório"
      )
    )
    
    # Escrever dados
    writeData(wb, "Parâmetros ELECTRE", params_df, headerStyle = header_style)
    
    # Formatação
    setColWidths(wb, "Parâmetros ELECTRE", cols = 1:3, widths = c(30, 40, 50))
    
    # Estilo para coluna Parâmetro
    param_style <- createStyle(textDecoration = "bold", fontColour = "#2c3e50")
    addStyle(wb, "Parâmetros ELECTRE", param_style, 
             rows = 2:(nrow(params_df) + 1), cols = 1, gridExpand = TRUE)
    
    cat("✓ Parâmetros gerados\n")
  }
  
  # ====================================================================
  # SALVAR WORKBOOK
  # ====================================================================
  
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat("\n✓ Excel gerado com sucesso!\n")
  cat("Arquivo:", output_file, "\n")
  cat("Tamanho:", format(file.size(output_file), big.mark = "."), "bytes\n")
  cat("Abas criadas:", length(names(wb)), "\n")
  cat("  -", paste(names(wb), collapse = "\n  - "), "\n")
  
  return(invisible(output_file))
}