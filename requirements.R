# requirements.R
# Detect, install and load all required packages automatically

r_files <- c(
  "app.R",
  "global.R",
  "./R/electre_tri_b_func.R",
  "./R/mod_analise.R",
  "./R/mod_preproc.R"
)

extract_packages <- function(file) {
  if (!file.exists(file)) return(character(0))

  lines <- readLines(file, warn = FALSE)

  matches <- regmatches(lines, gregexpr("(?<=library\\(|require\\()([A-Za-z0-9\\.]+)", lines, perl = TRUE))
  unlist(matches)
}

packages <- unique(unlist(lapply(r_files, extract_packages)))

# Remove base packages
base_pkgs <- rownames(installed.packages(priority = "base"))
packages <- setdiff(packages, base_pkgs)

cat("📦 Pacotes detectados:\n", paste(packages, collapse = ", "), "\n\n")

# Instalar os que faltam
missing <- setdiff(packages, installed.packages()[, "Package"])

if (length(missing) > 0) {
  cat("⬇ Instalando pacotes faltantes:\n", paste(missing, collapse = ", "), "\n\n")
  install.packages(missing)
} else {
  cat("✅ Nenhum pacote faltando.\n")
}

# Carregar pacotes
invisible(lapply(packages, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

cat("🚀 Ambiente pronto. Todos os pacotes carregados.\n")

# Lista Definitiva de Pacotes
packages <- c(
  # --- Core & UI ---
  "shiny",
  "bslib",
  "shinyWidgets",
  "shinyjs",
  "waiter",
  "htmltools",
  
  # --- Dados ---
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "readr",
  "lubridate",
  "qs",           # Essencial para seus arquivos .qs
  "openxlsx",     # Para gerar_excel_completo.R
  "readxl",
  
  # --- Espacial (O mais chato de instalar) ---
  "sf",
  "leaflet",
  "leaflet.extras",
  "geojsonsf",
  "units",        # Dependencia comum do sf
  
  # --- Visualizacao & Relatorios ---
  "plotly",
  "ggplot2",
  "DT",
  "viridis",
  "RColorBrewer",
  "rmarkdown",
  "knitr",
  "kableExtra",   # Causa do ultimo erro (precisa de textshaping)
  "systemfonts",  # Dependencia do kableExtra
  "textshaping",  # Dependencia do kableExtra
  
  # --- Chatbot & API ---
  "httr2",
  "jsonlite",
  
  # --- Async & Utils ---
  "future",
  "promises",
  "remotes"
)

# Funcao de instalacao com verificacao de erro
install_package_robust <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("⬇ Instalando:", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
    
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("❌ ERRO FATAL: Falha ao instalar", pkg))
    }
  }
}

# Instala tudo
invisible(lapply(packages, install_package_robust))
cat("🚀 Todos os pacotes instalados com sucesso.\n")
