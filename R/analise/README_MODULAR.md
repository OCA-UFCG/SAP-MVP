# Estrutura Modular - Módulo de Análise ELECTRE Tri-B

## 📁 Estrutura de Arquivos

```
R/
├── mod_analise.R                      # Arquivo principal (orquestrador)
└── analise/
    ├── config.R                       # Configurações e constantes
    ├── utils.R                        # Funções auxiliares gerais
    ├── filtros.R                      # Sistema de filtros modal (reutilizável)
    ├── perfis.R                       # Editor de perfis interativo
    ├── electre_core.R                 # Lógica core do ELECTRE Tri-B
    ├── spatial.R                      # Operações espaciais
    ├── ui_components.R                # Componentes de UI
    ├── ui_pesos.R                     # UI específica de pesos
    ├── outputs_dashboard.R            # Outputs da aba Dashboard
    ├── outputs_tabela.R               # Outputs da aba Tabela
    ├── outputs_mapa.R                 # Outputs da aba Mapa
    ├── outputs_qualificacao.R         # Outputs da aba Qualificação
    └── downloads.R                    # Handlers de download
```

## 🎯 Responsabilidades de Cada Arquivo

### `mod_analise.R` (Principal)
- **Função**: Orquestrador que carrega e coordena todos os componentes
- **Responsabilidades**:
  - Fazer source de todos os arquivos auxiliares
  - Definir UI e Server do módulo principal
  - Coordenar reativos principais
  - Chamar funções modulares

### `config.R`
- **Função**: Configurações centralizadas
- **Contém**:
  - `gerar_paleta_cores()` - Paletas dinâmicas por n_classes
  - `gerar_labels_classes()` - Labels das classes
  - `CORES_CAMADAS` - Cores das camadas de qualificação
  - Constantes globais

### `utils.R`
- **Função**: Utilitários gerais reutilizáveis
- **Contém**:
  - `%||%` - Operador de coalescência
  - `clamp01()` - Limitar valores [0,1]
  - `calcular_ranges_real()` - Ranges para normalização
  - `to_unit()` / `to_real()` - Conversões
  - `normalizar_pesos()` - Normalização de pesos
  - `formatar_numero()` - Formatação de números

### `filtros.R`
- **Função**: Sistema completo de filtros com modal
- **Componentes Reutilizáveis**:
  - `criar_botao_filtros_ui()` - UI do botão
  - `criar_sistema_filtros_modal()` - Lógica completa do modal
  - `aplicar_filtros_em_df()` - Aplicar filtros em data frames
- **Reutilizável**: ✅ Pode ser usado em outros módulos!

### `perfis.R`
- **Função**: Editor interativo de perfis
- **Contém**:
  - `inicializar_perfis()` - Inicialização com quantis
  - `criar_modal_perfis()` - Modal interativo
  - `renderizar_histogramas_perfis()` - Plots com cliques
  - `observar_cliques_perfis()` - Interatividade
  - `resumo_perfis_definidos()` - Resumo textual

### `electre_core.R`
- **Função**: Lógica central do ELECTRE
- **Contém**:
  - `calcular_perfis_b()` - Matriz de perfis
  - `preparar_dados_electre()` - Normalização e limpeza
  - `obter_limiares()` - Limiares globais ou por critério
  - `executar_electre()` - Execução completa
  - `gerar_resumo_parametros()` - Resumo formatado

### `spatial.R`
- **Função**: Operações espaciais para qualificação
- **Contém**:
  - `carregar_dados_espaciais()` - Carrega camadas
  - `calcular_intersecoes()` - Interseções espaciais
  - `criar_ranking_municipios()` - Ranking por interseções

### `ui_components.R`
- **Função**: Componentes de UI estruturados
- **Contém**:
  - `criar_ui_analise()` - UI principal
  - `criar_sidebar_analise()` - Sidebar
  - `criar_aba_parametros()` - Aba Parâmetros
  - `criar_card_perfis()` - Card de perfis
  - `criar_card_limiares()` - Card de limiares

### `outputs_*.R`
- **Função**: Outputs específicos por aba
- **Arquivos**:
  - `outputs_dashboard.R` - Gráficos e métricas do dashboard
  - `outputs_tabela.R` - DataTables e exports
  - `outputs_mapa.R` - Mapas Leaflet
  - `outputs_qualificacao.R` - Qualificação territorial

### `downloads.R`
- **Função**: Handlers de download centralizados
- **Contém**:
  - Download CSV
  - Download GeoPackage
  - Download PNG de mapas
  - Download de relatórios

## 🔄 Como Usar

### 1. Carregar o Módulo Principal

```r
# No app.R ou server.R
source("R/mod_analise.R")

# UI
ui <- fluidPage(
  mod_analise_ui("analise")
)

# Server
server <- function(input, output, session) {
  resultados <- mod_analise_server("analise", preproc_data)
}
```

### 2. Reutilizar Componentes

#### Exemplo: Sistema de Filtros em Outro Módulo

```r
# No seu outro módulo
source("R/analise/filtros.R")

# UI
mod_outro_ui <- function(id) {
  ns <- NS(id)
  criar_botao_filtros_ui(ns, "meus_filtros")
}

# Server
mod_outro_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    filtros_aplicados <- reactiveVal(list())
    
    criar_sistema_filtros_modal(
      session = session,
      ns = ns,
      id = "meus_filtros",
      results_reactive = dados,
      filtros_aplicados = filtros_aplicados
    )
    
    dados_filtrados <- reactive({
      aplicar_filtros_em_df(dados(), filtros_aplicados())
    })
  })
}
```

#### Exemplo: Usar Funções de Normalização

```r
source("R/analise/utils.R")

# Calcular ranges
ranges <- calcular_ranges_real(df, c("var1", "var2"))

# Normalizar
valores_normalizados <- to_unit(df$var1, "var1", "benefit", ranges)

# Desnormalizar
valores_reais <- to_real(valores_normalizados, "var1", "benefit", ranges)
```

#### Exemplo: Usar Paletas e Labels

```r
source("R/analise/config.R")

# Gerar paleta para 5 classes
cores <- gerar_paleta_cores(5)
# Retorna: c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c")

# Gerar labels
labels <- gerar_labels_classes(5)
# Retorna: c("C1 (muito baixo)", "C2 (baixo)", ...)
```

## 🛠️ Adicionar Novos Componentes

### 1. Criar Novo Arquivo

```r
# R/analise/meu_componente.R

#' Minha função reutilizável
#' @param x Input
#' @return Output
#' @export
minha_funcao <- function(x) {
  # Implementação
}
```

### 2. Adicionar Source no Principal

```r
# Em R/mod_analise.R
source("R/analise/meu_componente.R", local = TRUE)
```

### 3. Usar no Módulo

```r
# No server do módulo
resultado <- minha_funcao(dados)
```

## ✅ Vantagens da Estrutura Modular

1. **Manutenibilidade**: Cada arquivo tem uma responsabilidade clara
2. **Reutilização**: Componentes podem ser usados em outros módulos
3. **Testabilidade**: Funções isoladas são mais fáceis de testar
4. **Colaboração**: Múltiplos desenvolvedores podem trabalhar simultaneamente
5. **Documentação**: Cada arquivo é autodocumentado
6. **Performance**: Source local evita poluir namespace global
7. **Escalabilidade**: Fácil adicionar novos recursos

## 📝 Convenções

- **Nomes de funções**: `snake_case`
- **Nomes de arquivos**: `snake_case.R`
- **Documentação**: Roxygen2 style comments
- **Exports**: Marcar com `@export` funções reutilizáveis
- **Dependências**: Documentar pacotes necessários

## 🔍 Troubleshooting

### Erro: "object not found"
- Verificar se fez source do arquivo correto
- Verificar se a função está exportada

### Erro: "namespace collision"
- Usar `local = TRUE` no source
- Usar namespaces explícitos quando necessário

### Performance lenta
- Avaliar se algum source está sendo chamado repetidamente
- Considerar memoização para funções caras

## 📚 Próximos Passos

1. **Testes Unitários**: Criar `tests/testthat/test_analise_utils.R`
2. **Documentação**: Gerar pkgdown site
3. **Vignettes**: Criar tutoriais de uso
4. **CI/CD**: Configurar GitHub Actions
5. **Package**: Transformar em pacote R formal

---

**Mantido por**: Artur
**Última atualização**: 2025-11-08
