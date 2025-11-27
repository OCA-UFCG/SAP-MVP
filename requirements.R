# requirements.R - Lista Otimizada e Limpa

# Definir repositorio de BINARIOS (Isso acelera o build em 10x)
# Usando snapshot para Ubuntu 22.04 (Jammy) que é a base do rocker/shiny
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))

packages <- c(
  # --- Core Shiny & UI ---
  "shiny",
  "bslib",
  "shinyWidgets",
  "shinyjs",
  "waiter",       # Telas de carregamento
  "htmltools",
  "shinyBS",
  "shinycssloaders",
  "colourpicker",
  
  # --- Dados & Manipulacao ---
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "readr",
  "lubridate",
  "qs",           # CRITICO: Para ler seus arquivos .qs
  "openxlsx",     # Provavel uso em gerar_excel_completo.R
  "writexl",
  "readxl",
  
  # --- Espacial ---
  "sf",           # O mais pesado de todos
  "leaflet",
  "leaflet.extras",
  "geojsonsf",
  "units",
  
  # --- Visualizacao ---
  "plotly",
  "ggplot2",
  "DT",
  "viridis",
  "RColorBrewer",
  
  # --- Relatorios ---
  "rmarkdown",
  "knitr",
  "kableExtra",
  "systemfonts",
  "textshaping",
  
  # --- Chatbot & API ---
  "httr2",
  "jsonlite",
  
  # --- Async ---
  "future",
  "promises"
)

# Funcao de instalacao simples e rapida
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message(paste("⬇ Instalando (Binario):", p))
    install.packages(p)
  }
}

invisible(lapply(packages, install_if_missing))
cat("🚀 Todos os pacotes instalados via Binários.\n")
