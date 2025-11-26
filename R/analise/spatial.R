# =====================================================================
# OPERAÇÕES ESPACIAIS - QUALIFICAÇÃO TERRITORIAL
# =====================================================================

#' Carregar dados espaciais
#' 
#' @param filepath Caminho para arquivo .qs
#' @return Lista com sf objects ou NULL
#' @export
carregar_dados_espaciais <- function(filepath = "data/dados_espaciais_para_shiny.qs") {
  
  if (!file.exists(filepath)) {
    showNotification(
      "Arquivo de dados espaciais não encontrado",
      type = "warning",
      duration = 5
    )
    return(NULL)
  }
  
  tryCatch({
    lista_sf <- qs::qread(filepath)
    list(
      quilombolas = lista_sf[[1]], 
      assentamentos = lista_sf[[2]],
      indigenas = lista_sf[[3]], 
      ensino = lista_sf[[4]],
      prisoes = lista_sf[[5]], 
      sementes = lista_sf[[6]],
      prop_rurais = lista_sf[[7]]
    )
  }, error = function(e) {
    showNotification(
      paste("Erro ao carregar dados espaciais:", e$message),
      type = "error",
      duration = 10
    )
    return(NULL)
  })
}

#' Calcular interseções espaciais
#' 
#' @param dados_espaciais Lista com camadas espaciais
#' @param resultado_sf SF object com resultados ELECTRE
#' @return Lista com interseções calculadas
#' @export
calcular_intersecoes <- function(dados_espaciais, resultado_sf, camadas_selecionadas = NULL) {
  
  if (is.null(dados_espaciais) || is.null(resultado_sf)) {
    return(NULL)
  }
  
  # DEBUG: Mostrar camadas recebidas
  message("DEBUG: Camadas recebidas: ", paste(camadas_selecionadas, collapse=", "))
  message("DEBUG: Número de camadas: ", length(camadas_selecionadas))
  
  # Se não especificar camadas OU vetor vazio, processar todas
  if (is.null(camadas_selecionadas) || length(camadas_selecionadas) == 0) {
    message("DEBUG: Camadas vazias ou NULL - processando TODAS")
    camadas_selecionadas <- c("quilombolas", "assentamentos", "indigenas", 
                              "ensino", "prisoes", "sementes", "prop_rurais")
  } else {
    message("DEBUG: Processando camadas selecionadas: ", paste(camadas_selecionadas, collapse=", "))
  }
  
  resultado_sf <- st_transform(resultado_sf, 4326)
  
  inter_list <- list()
  
  # Calcular número de camadas a processar
  n_camadas <- length(camadas_selecionadas)
  progresso_atual <- 0
  
  withProgress(message = 'Processando interseções espaciais...', value = 0, {
    
    # Quilombolas
    if ("quilombolas" %in% camadas_selecionadas && !is.null(dados_espaciais$quilombolas)) {
      progresso_atual <- progresso_atual + 1
      incProgress(1/n_camadas, detail = "Quilombolas...")
      inter_list$quilombolas <- tryCatch({
        quilombolas_valido <- st_make_valid(dados_espaciais$quilombolas)
        quilombolas_4326 <- st_transform(quilombolas_valido, 4326)
        quilombo_inter <- st_join(quilombolas_4326, resultado_sf, 
                                  join = st_intersects, left = FALSE) |>
          mutate(tipo = "Quilombola", nome_feature = nm_comunid)
        quilombo_inter
      }, error = function(e) NULL)
    }
    
    # Assentamentos
    if ("assentamentos" %in% camadas_selecionadas && !is.null(dados_espaciais$assentamentos)) {
      progresso_atual <- progresso_atual + 1
      incProgress(1/n_camadas, detail = "Assentamentos...")
      inter_list$assentamentos <- tryCatch({
        assentamentos_valido <- st_make_valid(dados_espaciais$assentamentos)
        assentamentos_4326 <- st_transform(assentamentos_valido, 4326)
        assent_inter <- st_join(assentamentos_4326, resultado_sf, 
                                join = st_intersects, left = FALSE) |>
          mutate(tipo = "Assentamento", nome_feature = nome_proje)
        assent_inter
      }, error = function(e) NULL)
    }
    
    # Territórios Indígenas
    if ("indigenas" %in% camadas_selecionadas && !is.null(dados_espaciais$indigenas)) {
      progresso_atual <- progresso_atual + 1
      incProgress(1/n_camadas, detail = "Territórios Indígenas...")
      inter_list$indigenas <- tryCatch({
        indigenas_valido <- st_make_valid(dados_espaciais$indigenas)
        indigenas_4326 <- st_transform(indigenas_valido, 4326)
        indigena_inter <- st_join(indigenas_4326, resultado_sf, 
                                  join = st_intersects, left = FALSE) |>
          mutate(tipo = "Território Indígena", nome_feature = terrai_nom)
        indigena_inter
      }, error = function(e) NULL)
    }
    
    # Instituições de Ensino
    if ("ensino" %in% camadas_selecionadas && !is.null(dados_espaciais$ensino)) {
      progresso_atual <- progresso_atual + 1
      incProgress(1/n_camadas, detail = "Instituições de Ensino...")
      inter_list$ensino <- tryCatch({
        suppressWarnings({ ensino_pontos <- st_centroid(dados_espaciais$ensino) })
        ensino_4326 <- st_transform(ensino_pontos, 4326)
        ensino_inter <- st_join(ensino_4326, resultado_sf, 
                                join = st_intersects, left = FALSE) |>
          mutate(tipo = "Instituição de Ensino", nome_feature = Nome)
        ensino_inter
      }, error = function(e) NULL)
    }
    
    # Unidades Prisionais
    if ("prisoes" %in% camadas_selecionadas && !is.null(dados_espaciais$prisoes)) {
      progresso_atual <- progresso_atual + 1
      incProgress(1/n_camadas, detail = "Unidades Prisionais...")
      inter_list$prisoes <- tryCatch({
        prisoes_4326 <- st_transform(dados_espaciais$prisoes, 4326)
        prisao_inter <- st_join(prisoes_4326, resultado_sf, 
                                join = st_intersects, left = FALSE) |>
          mutate(tipo = "Unidade Prisional", nome_feature = Nome)
        prisao_inter
      }, error = function(e) NULL)
    }
    
    # Bancos de Sementes
    if ("sementes" %in% camadas_selecionadas && !is.null(dados_espaciais$sementes)) {
      progresso_atual <- progresso_atual + 1
      incProgress(1/n_camadas, detail = "Bancos de Sementes...")
      inter_list$sementes <- tryCatch({
        sementes_4326 <- st_transform(dados_espaciais$sementes, 4326)
        sementes_inter <- st_join(sementes_4326, resultado_sf, 
                                  join = st_intersects, left = FALSE) |>
          mutate(tipo = "Banco de Sementes", 
                 nome_feature = if("Nome" %in% names(sementes_4326)) sementes_4326$Nome else "Banco de Sementes")
        sementes_inter
      }, error = function(e) NULL)
    }
    
    # Propriedades Rurais
    if ("prop_rurais" %in% camadas_selecionadas && !is.null(dados_espaciais$prop_rurais)) {
      progresso_atual <- progresso_atual + 1
      incProgress(1/n_camadas, detail = "Propriedades Rurais...")
      inter_list$prop_rurais <- tryCatch({
        # Validar geometria
        prop_rurais_valido <- st_make_valid(dados_espaciais$prop_rurais)
        
        # Identificar tipos de geometria
        geom_types <- st_geometry_type(prop_rurais_valido)
        
        # Separar GEOMETRYCOLLECTION das demais
        idx_geomcol <- which(grepl("GEOMETRYCOLLECTION", as.character(geom_types)))
        
        if (length(idx_geomcol) > 0) {
          # Processar GEOMETRYCOLLECTION separadamente
          geomcol_features <- prop_rurais_valido[idx_geomcol, ]
          outras_features <- prop_rurais_valido[-idx_geomcol, ]
          
          # Extrair polígonos das GEOMETRYCOLLECTION
          geomcol_polygons <- st_collection_extract(geomcol_features, "POLYGON")
          
          # Validar
          if (!is.null(geomcol_polygons) && nrow(geomcol_polygons) > 0) {
            geomcol_polygons <- st_make_valid(geomcol_polygons)
            # Combinar de volta
            prop_rurais_valido <- rbind(outras_features, geomcol_polygons)
          } else {
            # Se não extraiu nada, manter apenas as outras features
            prop_rurais_valido <- outras_features
          }
        }
        
        # Filtrar apenas polígonos válidos (POLYGON ou MULTIPOLYGON)
        geom_types_final <- st_geometry_type(prop_rurais_valido)
        valid_geoms <- grepl("POLYGON|MULTIPOLYGON", as.character(geom_types_final))
        
        if (sum(valid_geoms) == 0) {
          message("prop_rurais: Nenhum polígono válido encontrado")
          return(NULL)
        }
        
        prop_rurais_valido <- prop_rurais_valido[valid_geoms, ]
        
        # Transformar para 4326
        prop_rurais_4326 <- st_transform(prop_rurais_valido, 4326)
        
        # Fazer interseção espacial
        prop_inter <- st_join(prop_rurais_4326, resultado_sf, 
                              join = st_intersects, left = FALSE)
        
        # Verificar se há interseções
        if (is.null(prop_inter) || nrow(prop_inter) == 0) {
          message("prop_rurais: Nenhuma interseção encontrada")
          return(NULL)
        }
        
        # Determinar nome do campo para identificação
        nome_col <- if("Nome" %in% names(prop_inter)) {
          "Nome"
        } else if("nome" %in% names(prop_inter)) {
          "nome"
        } else if("NOME" %in% names(prop_inter)) {
          "NOME"
        } else if("NM_IMOVEL" %in% names(prop_inter)) {
          "NM_IMOVEL"
        } else {
          NULL
        }
        
        # Adicionar colunas necessárias
        if (!is.null(nome_col)) {
          prop_inter <- prop_inter |>
            mutate(tipo = "Propriedade Rural", 
                   nome_feature = .data[[nome_col]])
        } else {
          prop_inter <- prop_inter |>
            mutate(tipo = "Propriedade Rural", 
                   nome_feature = "Propriedade Rural")
        }
        
        message(sprintf("prop_rurais: %d interseções encontradas", nrow(prop_inter)))
        prop_inter
      }, error = function(e) {
        message("Erro ao processar prop_rurais: ", e$message)
        return(NULL)
      })
    }

    
  })
  
  
  inter_list
}

#' Criar tabela de ranking de municípios
#' 
#' @param intersecoes Lista com interseções
#' @param resultado_sf SF object com municípios
#' @return Data frame com ranking
#' @export
criar_ranking_municipios <- function(intersecoes, resultado_sf) {
  
  if (is.null(intersecoes) || is.null(resultado_sf)) {
    return(NULL)
  }
  
  contagens_municipio <- data.frame(
    CD_MUN = character(), 
    NM_MUN = character(),
    Quilombolas = integer(), 
    Assentamentos = integer(),
    T_Indigenas = integer(), 
    Inst_Ensino = integer(),
    U_Prisionais = integer(), 
    B_Sementes = integer(),
    Prop_Rurais = integer(),
    Total = integer(), 
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(resultado_sf)) {
    cd_mun <- resultado_sf$CD_MUN[i]
    nm_mun <- resultado_sf$NM_MUN[i]
    counts <- c(0, 0, 0, 0, 0, 0, 0)
    
    # Contar interseções para cada camada
    if (!is.null(intersecoes$quilombolas) && nrow(intersecoes$quilombolas) > 0) {
      df_quilombolas <- st_drop_geometry(intersecoes$quilombolas)
      if ("CD_MUN" %in% names(df_quilombolas)) {
        counts[1] <- sum(df_quilombolas$CD_MUN == cd_mun, na.rm = TRUE)
      }
    }
    
    if (!is.null(intersecoes$assentamentos) && nrow(intersecoes$assentamentos) > 0) {
      df_assentamentos <- st_drop_geometry(intersecoes$assentamentos)
      if ("CD_MUN" %in% names(df_assentamentos)) {
        counts[2] <- sum(df_assentamentos$CD_MUN == cd_mun, na.rm = TRUE)
      }
    }
    
    if (!is.null(intersecoes$indigenas) && nrow(intersecoes$indigenas) > 0) {
      df_indigenas <- st_drop_geometry(intersecoes$indigenas)
      if ("CD_MUN" %in% names(df_indigenas)) {
        counts[3] <- sum(df_indigenas$CD_MUN == cd_mun, na.rm = TRUE)
      }
    }
    
    if (!is.null(intersecoes$ensino) && nrow(intersecoes$ensino) > 0) {
      df_ensino <- st_drop_geometry(intersecoes$ensino)
      if ("CD_MUN" %in% names(df_ensino)) {
        counts[4] <- sum(df_ensino$CD_MUN == cd_mun, na.rm = TRUE)
      }
    }
    
    if (!is.null(intersecoes$prisoes) && nrow(intersecoes$prisoes) > 0) {
      df_prisoes <- st_drop_geometry(intersecoes$prisoes)
      if ("CD_MUN" %in% names(df_prisoes)) {
        counts[5] <- sum(df_prisoes$CD_MUN == cd_mun, na.rm = TRUE)
      }
    }
    
    if (!is.null(intersecoes$sementes) && nrow(intersecoes$sementes) > 0) {
      df_sementes <- st_drop_geometry(intersecoes$sementes)
      if ("CD_MUN" %in% names(df_sementes)) {
        counts[6] <- sum(df_sementes$CD_MUN == cd_mun, na.rm = TRUE)
      }
    }
    if (!is.null(intersecoes$prop_rurais) && nrow(intersecoes$prop_rurais) > 0) {
      df_prop_rurais <- st_drop_geometry(intersecoes$prop_rurais)
      if ("CD_MUN" %in% names(df_prop_rurais)) {
        counts[7] <- sum(df_prop_rurais$CD_MUN == cd_mun, na.rm = TRUE)
      }
    }
    
    total <- sum(counts)
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
          Prop_Rurais = counts[7],
          Total = total, 
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  # Verificar se há dados
  if (nrow(contagens_municipio) == 0) {
    return(NULL)
  }
  
  # Ordenar por total
  contagens_municipio <- contagens_municipio[order(-contagens_municipio$Total), ]
  
  # Adicionar ranking
  contagens_municipio <- cbind(
    Ranking = 1:nrow(contagens_municipio), 
    contagens_municipio
  )
  
  # Remover CD_MUN da visualização
  contagens_municipio <- contagens_municipio[, -which(names(contagens_municipio) == "CD_MUN")]
  
  # Renomear colunas
  names(contagens_municipio) <- c(
    "#", "Município", "Quilombolas", "Assentamentos",
    "T. Indígenas", "Inst. Ensino", "U. Prisionais",
    "B. Sementes", "Prop. Rurais", "Total"
  )
  
  contagens_municipio
}