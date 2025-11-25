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
}

#' Calcular interseções espaciais
#' 
#' @param dados_espaciais Lista com camadas espaciais
#' @param resultado_sf SF object com resultados ELECTRE
#' @return Lista com interseções calculadas
#' @export
calcular_intersecoes <- function(dados_espaciais, resultado_sf) {
  
  if (is.null(dados_espaciais) || is.null(resultado_sf)) {
    return(NULL)
  }
  
  resultado_sf <- st_transform(resultado_sf, 4326)
  
  inter_list <- list()
  
  withProgress(message = 'Processando interseções espaciais...', value = 0, {
    
    # Quilombolas
    if (!is.null(dados_espaciais$quilombolas)) {
      incProgress(1/6, detail = "Quilombolas...")
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
    if (!is.null(dados_espaciais$assentamentos)) {
      incProgress(1/6, detail = "Assentamentos...")
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
    if (!is.null(dados_espaciais$indigenas)) {
      incProgress(1/6, detail = "Territórios Indígenas...")
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
    if (!is.null(dados_espaciais$ensino)) {
      incProgress(1/6, detail = "Instituições de Ensino...")
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
    if (!is.null(dados_espaciais$prisoes)) {
      incProgress(1/6, detail = "Unidades Prisionais...")
      inter_list$prisoes <- tryCatch({
        prisoes_4326 <- st_transform(dados_espaciais$prisoes, 4326)
        prisao_inter <- st_join(prisoes_4326, resultado_sf, 
                                join = st_intersects, left = FALSE) |>
          mutate(tipo = "Unidade Prisional", nome_feature = Nome)
        prisao_inter
      }, error = function(e) NULL)
    }
    
    # Bancos de Sementes
    if (!is.null(dados_espaciais$sementes)) {
      incProgress(1/6, detail = "Bancos de Sementes...")
      inter_list$sementes <- tryCatch({
        sementes_4326 <- st_transform(dados_espaciais$sementes, 4326)
        sementes_inter <- st_join(sementes_4326, resultado_sf, 
                                  join = st_intersects, left = FALSE) |>
          mutate(tipo = "Banco de Sementes", 
                 nome_feature = if("Nome" %in% names(sementes_4326)) sementes_4326$Nome else "Banco de Sementes")
        sementes_inter
      }, error = function(e) NULL)
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
    Total = integer(), 
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(resultado_sf)) {
    cd_mun <- resultado_sf$CD_MUN[i]
    nm_mun <- resultado_sf$NM_MUN[i]
    counts <- c(0, 0, 0, 0, 0, 0)
    
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
          Total = total, 
          stringsAsFactors = FALSE
        )
      )
    }
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
    "B. Sementes", "Total"
  )
  
  contagens_municipio
}
