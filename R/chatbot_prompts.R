# =====================================================================
# CHATBOT - Prompts Especializados por Contexto
# =====================================================================

# ---- Prompt Base (Sistema) -------------------------------------------

get_system_prompt <- function() {
  "Você é um assistente especializado em análise multicritério usando o método ELECTRE Tri-B.
Você auxilia usuários de um sistema de análise de secas e desertificação no Brasil.

Suas responsabilidades:
1. Explicar conceitos do método ELECTRE Tri-B de forma clara e didática
2. Ajudar na configuração de parâmetros (critérios, pesos, perfis, limiares)
3. Interpretar resultados e estatísticas
4. Sugerir melhores práticas de análise
5. Responder dúvidas sobre a interface do aplicativo

Diretrizes:
- Seja claro, objetivo e didático
- Use exemplos quando apropriado
- Evite jargão técnico excessivo, mas seja preciso
- Forneça explicações estruturadas quando necessário
- Se não souber algo específico dos dados, seja honesto
- Priorize a compreensão do usuário sobre rigor matemático extremo
- Use marcadores (bullets) e numeração para organizar informações

Quando interpretar resultados:
- Contextualize os números com o domínio de aplicação (secas e desertificação)
- Explique o que os valores significam na prática
- Sugira próximos passos de análise quando relevante"
}

# ---- Prompt de Documentação ELECTRE ----------------------------------

get_electre_documentation <- function() {
  "## DOCUMENTAÇÃO DO MÉTODO ELECTRE TRI-B

**O que é ELECTRE Tri-B?**
O ELECTRE Tri-B é um método de classificação multicritério que atribui alternativas (neste caso, municípios) a categorias pré-definidas. Diferente de métodos de ranking, ele foca em classificar, não em ordenar.

**Componentes Principais:**

1. **Critérios (g)**: Variáveis de análise. Cada critério pode ser:
   - Benefício (↑): Quanto maior, melhor (ex: cobertura vegetal)
   - Custo (↓): Quanto menor, melhor (ex: área degradada)

2. **Pesos (w)**: Importância relativa de cada critério
   - Devem somar 1.0 (normalizado automaticamente)
   - Maiores pesos = maior influência na classificação
   - Exemplo: se desertificação é prioridade, dê mais peso a critérios relacionados

3. **Perfis de Classe (B)**: Limites entre as classes
   - Número de perfis = Número de classes - 1
   - Perfis definem os valores de referência para cada critério
   - Exemplo com 5 classes: B1, B2, B3, B4 separam C1, C2, C3, C4, C5
   - Métodos de definição:
     * Quantis (padrão): Divide automaticamente em percentis
     * Manual: Você define com base em conhecimento do domínio

4. **Limiares**: Controlam a flexibilidade da classificação
   - **q (indiferença)**: Diferença considerada insignificante
     * Valor típico: 0.02 (2%)
     * Menor q = classificação mais rígida
   
   - **p (preferência)**: Diferença para preferência clara
     * Valor típico: 0.10 (10%)
     * Entre q e p há zona de hesitação
   
   - **v (veto)**: Diferença que impede classificação superior
     * Valor típico: 0.50 (50%)
     * Veto forte pode forçar classificação em classe inferior
   
   - Relação: 0 ≤ q < p < v ≤ 1

5. **Lambda (λ)**: Credibilidade mínima para atribuição
   - Valor típico: 0.70 (70%)
   - Quanto maior, mais rigorosa a classificação
   - Valores comuns: 0.60 a 0.80

6. **Regra de Atribuição**:
   - **Pessimista (pc)**: Em caso de dúvida, classifica na categoria inferior
     * Mais conservadora
     * Recomendada quando precaução é importante
   
   - **Otimista (oc)**: Em caso de dúvida, classifica na categoria superior
     * Mais permissiva
     * Útil para priorizar áreas com potencial

**Interpretação de Classes:**
- C1 (muito baixo): Situação favorável, baixa prioridade
- C2 (baixo): Situação razoável
- C3 (médio): Situação intermediária, atenção moderada
- C4 (alto): Situação preocupante, alta prioridade
- C5 (muito alto): Situação crítica, prioridade máxima

No contexto de secas e desertificação:
- Classes altas (C4, C5) indicam áreas mais vulneráveis/prioritárias
- Classes baixas (C1, C2) indicam áreas menos afetadas

**Boas Práticas:**
1. Escolha critérios relevantes e não redundantes
2. Valide pesos com especialistas ou partes interessadas
3. Teste diferentes configurações de limiares
4. Compare regras pessimista e otimista
5. Analise a distribuição final de classes (evite concentração excessiva em uma classe)
6. Valide resultados com conhecimento local
7. Documente todas as escolhas metodológicas"
}

# ---- Prompt Contextual por Módulo -----------------------------------

get_contextual_prompt <- function(context) {
  base_prompt <- paste(
    "## CONTEXTO ATUAL DO SISTEMA\n",
    jsonlite::toJSON(context, auto_unbox = TRUE, pretty = TRUE),
    "\n\n"
  )
  
  # Adicionar orientações específicas do módulo
  # Garantir que modulo não seja NULL ou vazio
  modulo <- if (!is.null(context$modulo) && length(context$modulo) == 1) {
    context$modulo
  } else {
    "default"
  }
  
  module_guidance <- switch(
    modulo,
    
    "Pré-processamento" = "
**Você está no módulo de PRÉ-PROCESSAMENTO**

O usuário pode estar:
- Explorando os dados disponíveis
- Aplicando filtros para selecionar municípios
- Criando novas colunas calculadas
- Preparando dados para análise ELECTRE

Foque em:
- Explicar o sistema de filtros (simples e compostos)
- Sugerir critérios de filtragem relevantes
- Ajudar na interpretação das variáveis disponíveis
- Orientar sobre preparação de dados para análise multicritério
",
    
    "Análise ELECTRE - Configuração de Parâmetros" = "
**Você está no módulo de CONFIGURAÇÃO DE PARÂMETROS ELECTRE TRI-B**

O usuário está configurando a análise multicritério. Preste atenção em:
- Critérios selecionados e seus sentidos
- Pesos atribuídos (se estão balanceados)
- Número de classes escolhido
- Método de definição de perfis
- Limiares configurados (q, p, v)
- Lambda e regra de atribuição

Foque em:
- Explicar cada parâmetro e seu impacto
- Sugerir valores adequados para o contexto
- Alertar sobre configurações potencialmente problemáticas
- Orientar sobre escolha entre regra pessimista e otimista
- Explicar trade-offs entre diferentes configurações
",
    
    "Análise ELECTRE - Resultados" = "
**Você está visualizando os RESULTADOS da análise ELECTRE TRI-B**

O usuário já executou a análise e está interpretando resultados.

Foque em:
- Interpretar a distribuição de classes
- Explicar o que cada classe significa no contexto
- Identificar padrões espaciais ou estatísticos
- Sugerir análises complementares
- Ajudar a identificar municípios prioritários
- Explicar visualizações (mapas, gráficos)
",
    
    "Qualificação Territorial" = "
**Você está no módulo de QUALIFICAÇÃO TERRITORIAL**

Este módulo analisa a sobreposição de diferentes camadas espaciais prioritárias.

Foque em:
- Explicar o que cada camada espacial representa
- Interpretar interseções e suas implicações
- Ajudar a identificar áreas de alta complexidade territorial
- Explicar a relevância de cada tipo de área prioritária
",
    
    "Geração de Relatórios" = "
**Você está no módulo de GERAÇÃO DE RELATÓRIOS**

O usuário está preparando um relatório final.

Foque em:
- Sugerir seções relevantes para incluir
- Orientar sobre estruturação do relatório
- Ajudar na customização visual
- Sugerir textos explicativos para as seções
- Dar dicas de apresentação de resultados
",
    
    # Padrão para valores desconhecidos ou NULL
    "default" = "
**Sistema ELECTRE Tri-B - Análise Multicritério**

O usuário está navegando pelo sistema de análise de secas e desertificação.

Foque em:
- Responder perguntas gerais sobre o método ELECTRE Tri-B
- Explicar funcionalidades do sistema
- Orientar sobre fluxo de análise
- Ajudar com dúvidas conceituais
",
    
    # Retornar default se nenhum caso corresponder
    ""
  )
  
  return(paste(base_prompt, module_guidance))
}

# ---- Prompt Completo para Gemini -------------------------------------

build_full_prompt <- function(user_message, context, conversation_history = NULL) {
  
  # Prompt do sistema
  system_prompt <- get_system_prompt()
  
  # Documentação ELECTRE
  electre_docs <- get_electre_documentation()
  
  # Contexto atual
  contextual_prompt <- get_contextual_prompt(context)
  
  # Montar prompt completo
  full_context <- paste(
    system_prompt,
    "\n\n---\n\n",
    electre_docs,
    "\n\n---\n\n",
    contextual_prompt,
    sep = ""
  )
  
  # Adicionar histórico de conversa se existir
  messages <- list()
  
  if (!is.null(conversation_history) && length(conversation_history) > 0) {
    # Adicionar contexto como primeira mensagem
    messages <- append(messages, list(list(
      role = "user",
      content = paste(full_context, "\n\nPERGUNTA INICIAL: (contexto estabelecido)")
    )))
    messages <- append(messages, list(list(
      role = "assistant", 
      content = "Contexto compreendido! Estou pronto para ajudar. Como posso auxiliá-lo?"
    )))
    
    # Adicionar histórico (últimas 10 mensagens para não sobrecarregar)
    recent_history <- tail(conversation_history, 10)
    for (msg in recent_history) {
      if (msg$role %in% c("user", "assistant")) {
        messages <- append(messages, list(list(
          role = msg$role,
          content = msg$content
        )))
      }
    }
  } else {
    # Primeira mensagem
    messages <- append(messages, list(list(
      role = "user",
      content = full_context
    )))
    messages <- append(messages, list(list(
      role = "assistant",
      content = "Contexto compreendido! Como posso ajudar?"
    )))
  }
  
  # Adicionar pergunta atual do usuário
  messages <- append(messages, list(list(
    role = "user",
    content = user_message
  )))
  
  return(messages)
}

# ---- Prompts para Sugestões Rápidas ---------------------------------

get_quick_prompts <- function(module) {
  prompts <- list(
    "preproc" = c(
      "Como usar os filtros?",
      "Quais variáveis estão disponíveis?",
      "Como criar uma coluna calculada?"
    ),
    "parametros" = c(
      "O que é ELECTRE Tri-B?",
      "Como escolher os pesos?",
      "Qual o melhor número de classes?",
      "Diferença entre q, p e v?"
    ),
    "resultados" = c(
      "Como interpretar as classes?",
      "O que significa C4 e C5?",
      "Municípios mais críticos?",
      "Próximos passos de análise?"
    ),
    "qualificacao" = c(
      "O que são as camadas espaciais?",
      "Como interpretar interseções?",
      "Áreas prioritárias?"
    ),
    "relatorios" = c(
      "Quais seções incluir?",
      "Como estruturar o relatório?",
      "Dicas de visualização?"
    )
  )
  
  prompts[[module]] %||% prompts[["parametros"]]
}