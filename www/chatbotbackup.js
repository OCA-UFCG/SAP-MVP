/* =====================================================================
   CHATBOT - JavaScript para Detecção de Contexto
   ===================================================================== */

$(document).ready(function() {
  
  // Handler para executar JavaScript quando shinyjs não está disponível
  Shiny.addCustomMessageHandler('runjs', function(code) {
    try {
      eval(code);
    } catch(e) {
      console.warn("Erro ao executar JavaScript:", e);
    }
  });
  
  // Função para mostrar/esconder modal
  window.showChatbotModal = function(modalId) {
    var $modal = $('#' + modalId);
    if ($modal.length) {
      $modal.css('display', 'flex').addClass('visible').hide().fadeIn(200);
    }
  };
  
  window.hideChatbotModal = function(modalId) {
    var $modal = $('#' + modalId);
    if ($modal.length) {
      $modal.fadeOut(200, function() {
        $(this).removeClass('visible').css('display', 'none');
      });
    }
  };
  
  // Função para detectar aba/contexto atual
  function detectCurrentContext() {
    // O contexto depende da estrutura do seu app
    // Aqui detectamos pelo título da aba ativa ou URL hash
    
    // Método 1: Detectar pela aba Bootstrap ativa
    var activeTab = $(".nav-link.active, .nav-item.active").text().trim().toLowerCase();
    
    // Mapear texto da aba para contexto
    var contextMap = {
      "pré-processamento": "preproc",
      "preprocessamento": "preproc",
      "parâmetros": "parametros",
      "parametros": "parametros",
      "resultados": "resultados",
      "tabela resultados": "resultados",
      "qualificação territorial": "qualificacao",
      "qualificacao": "qualificacao",
      "relatórios": "relatorios",
      "relatorios": "relatorios"
    };
    
    // Detectar contexto baseado na aba ativa
    for (var key in contextMap) {
      if (activeTab.includes(key)) {
        return contextMap[key];
      }
    }
    
    // Método 2: Detectar pela URL ou hash
    var hash = window.location.hash.toLowerCase();
    if (hash.includes("preproc")) return "preproc";
    if (hash.includes("param")) return "parametros";
    if (hash.includes("result")) return "resultados";
    if (hash.includes("qualif")) return "qualificacao";
    if (hash.includes("relat")) return "relatorios";
    
    // Método 3: Detectar pelo conteúdo visível (último recurso)
    if ($("[id*='parametros']:visible").length > 0) return "parametros";
    if ($("[id*='resultados']:visible").length > 0) return "resultados";
    if ($("[id*='qualificacao']:visible").length > 0) return "qualificacao";
    if ($("[id*='relatorios']:visible").length > 0) return "relatorios";
    if ($("[id*='preproc']:visible").length > 0) return "preproc";
    
    // Padrão
    return "preproc";
  }
  
  // Função para enviar contexto atual para o Shiny
  function updateChatbotContext(chatbotId) {
    var context = detectCurrentContext();
    var inputId = chatbotId + "-current_tab_js";
    
    Shiny.setInputValue(inputId, context, {priority: "event"});
  }
  
  // Observar mudanças de aba
  $(document).on("shown.bs.tab shown.bs.collapse", function(e) {
    setTimeout(function() {
      updateAllChatbots();
    }, 100);
  });
  
  // Observar cliques em navegação
  $(document).on("click", ".nav-link, .nav-item", function() {
    setTimeout(function() {
      updateAllChatbots();
    }, 200);
  });
  
  // Função auxiliar para atualizar todos os chatbots
  function updateAllChatbots() {
    $("[id*='chatbot']").each(function() {
      var chatbotId = $(this).attr("id");
      if (chatbotId && chatbotId.includes("chatbot")) {
        // Extrair namespace
        var namespace = chatbotId.split("-")[0];
        updateChatbotContext(namespace);
      }
    });
  }
  
  // Inicializar contexto na primeira carga
  setTimeout(function() {
    updateAllChatbots();
  }, 1000);
  
  // Função auxiliar: Scroll automático para final das mensagens
  function scrollToBottom(messagesId) {
    var messagesDiv = $("#" + messagesId);
    if (messagesDiv.length) {
      messagesDiv.scrollTop(messagesDiv[0].scrollHeight);
    }
  }
  
  // Observar novas mensagens e fazer scroll
  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.addedNodes.length > 0) {
        var target = $(mutation.target);
        if (target.hasClass("chatbot-messages")) {
          setTimeout(function() {
            target.scrollTop(target[0].scrollHeight);
          }, 100);
        }
      }
    });
  });
  
  // Observar todas as áreas de mensagens
  $(".chatbot-messages").each(function() {
    observer.observe(this, {
      childList: true,
      subtree: true
    });
  });
  
  // Ajustar altura do textarea ao digitar (auto-expand)
  $(document).on("input", ".chatbot-footer textarea", function() {
    this.style.height = "auto";
    this.style.height = Math.min(this.scrollHeight, 120) + "px";
  });
  
  // Prevenir submit do form ao pressionar Enter (já tratado no módulo)
  $(document).on("keydown", ".chatbot-footer textarea", function(e) {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
    }
  });
  
  // Log para debug
  console.log("Chatbot JavaScript inicializado");
  
});
