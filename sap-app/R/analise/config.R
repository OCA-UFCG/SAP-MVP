# =====================================================================
# CONFIGURAÇÕES E CONSTANTES - ELECTRE TRI-B
# =====================================================================

#' Gerar paleta de cores baseada no número de classes
#' 
#' @param n_classes Número de classes (3-7)
#' @return Vetor nomeado com cores hexadecimais
#' @export
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

#' Gerar labels descritivos para as classes
#' 
#' @param n_classes Número de classes (3-7)
#' @return Vetor nomeado com labels das classes
#' @export
gerar_labels_classes <- function(n_classes) {
  if (n_classes < 3 || n_classes > 7) n_classes <- 5
  
  descricoes <- c("muito baixo", "baixo", "médio", "alto", "muito alto", "altíssimo", "extremo")
  
  labels <- sapply(1:n_classes, function(i) {
    desc <- if (i <= length(descricoes)) descricoes[i] else paste0("nível ", i)
    paste0("C", i, " (", desc, ")")
  })
  
  setNames(labels, as.character(1:n_classes))
}

#' Cores para camadas de qualificação territorial
#' @export
CORES_CAMADAS <- list(
  quilombolas = "#f39c12",
  assentamentos = "#27ae60",
  indigenas = "#3498db",
  ensino = "#9b59b6",
  prisoes = "#e74c3c",
  sementes = "#16a085",
  municipios = "#999999"
)

#' Labels para camadas de qualificação
#' @export
LABELS_CAMADAS <- c(
  quilombolas = "Quilombolas",
  assentamentos = "Assentamentos",
  indigenas = "Territórios Indígenas",
  ensino = "Instituições de Ensino",
  prisoes = "Unidades Prisionais",
  sementes = "Bancos de Sementes"
)

# Constantes padrão (5 classes) - mantidas para compatibilidade
LABEL_MAP <- gerar_labels_classes(5)
PAL_NUM <- gerar_paleta_cores(5)
