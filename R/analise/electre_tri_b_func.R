# Replica a electre_tri_b do Python (classes 0..p-1)
# dataset: matriz (n x m), numérica, já com custos invertidos (benefício)
# W,Q,P,V: vetores de tamanho m
# B: perfis em ordem crescente (b1..b_{p-1}), formato matrix (p-1 x m) ou list de vetores
# cut_level: lambda
# rule: "pc" (pessimista) ou "oc" (otimista)
electre_tri_b_py <- function(dataset, W, Q, P, V, B, cut_level = 0.70,
                             rule = c("pc","oc")) {
  
  rule <- match.arg(rule)
  X <- as.matrix(dataset)
  if (!is.numeric(X)) stop("dataset deve ser numérico.")
  n <- nrow(X); m <- ncol(X)
  
  W <- as.numeric(W); Q <- as.numeric(Q); P <- as.numeric(P); V <- as.numeric(V)
  stopifnot(length(W)==m, length(Q)==m, length(P)==m, length(V)==m)
  
  # Perfis: aceitar list ou matrix
  if (is.list(B)) {
    Bmat <- do.call(rbind, B)
  } else {
    Bmat <- as.matrix(B)
  }
  stopifnot(ncol(Bmat) == m)
  n_profiles <- nrow(Bmat)      # = p-1
  p <- n_profiles + 1L
  
  # --- Função de credibilidade S(a >= b) exatamente como no Python ---
  S_ab <- function(a, b) {
    d <- b - a  # benefício: d>=0 significa a pior que b nesse critério
    
    # Concordância parcial c_j (triangular) — Python usa:
    # if d >= P -> 0 ; else if d < Q -> 1 ; else (P - d)/(P - Q)
    c <- numeric(m)
    for (j in seq_len(m)) {
      if (d[j] >= P[j]) {
        c[j] <- 0
      } else if (d[j] < Q[j]) {
        c[j] <- 1
      } else {
        denom <- P[j] - Q[j]
        c[j] <- if (denom == 0) as.numeric(d[j] <= Q[j]) else (P[j] - d[j]) / denom
      }
    }
    # Concordância global: média ponderada e divide por soma(W) (como no Python)
    Cglob <- sum(W * c)
    if (sum(W) != 0) Cglob <- Cglob / sum(W)
    
    # Discordância D_j (com veto) — Python usa:
    # if d < P -> 0 ; elif d >= V -> 1 ; else (-P + d)/(V - P)
    D <- numeric(m)
    for (j in seq_len(m)) {
      if (d[j] < P[j]) {
        D[j] <- 0
      } else if (d[j] >= V[j]) {
        D[j] <- 1
      } else {
        denom <- V[j] - P[j]
        D[j] <- if (denom == 0) as.numeric(d[j] > P[j]) else (d[j] - P[j]) / denom
      }
    }
    
    # Credibilidade: começa em Cglob e aplica produto onde D_j > Cglob (estrito)
    S <- Cglob
    if (Cglob < 1) {
      mask <- D > Cglob
      if (any(mask)) {
        S <- Cglob * prod((1 - D[mask]) / (1 - Cglob))
      }
    }
    S
  }
  
  # Pré-calcular S(a >= b^h) e S(b^h >= a) para todos
  Sx_b <- matrix(NA_real_, n, n_profiles)
  Sb_x <- matrix(NA_real_, n, n_profiles)
  for (i in seq_len(n)) {
    a <- X[i, ]
    for (h in seq_len(n_profiles)) {
      b <- Bmat[h, ]
      Sx_b[i, h] <- S_ab(a, b)
      Sb_x[i, h] <- S_ab(b, a)
    }
  }
  
  # --- Fuzzy symbols exatamente como no Python ---
  # '>' : S(a>=b) >= λ  e  S(b>=a) <  λ  (outranking estrito)
  # '<' : S(a>=b) <  λ  e  S(b>=a) >= λ
  # 'I' : ambos >= λ
  # 'R' : ambos <  λ
  
  # --- Classificação exatamente como classification_algorithm() (pc/oc) ---
  cls <- integer(n)
  
  if (rule == "pc") {
    # Pessimist: percorre perfis em ordem crescente (b1..b_{p-1})
    # class_i começa em n_profiles e, a cada '>' em b^h, vira n_profiles - count
    for (i in seq_len(n)) {
      class_i <- n_profiles
      count <- 0L
      for (h in seq_len(n_profiles)) {
        count <- count + 1L
        if (Sx_b[i, h] >= cut_level && Sb_x[i, h] < cut_level) {
          class_i <- as.integer(n_profiles - count)
        }
      }
      cls[i] <- class_i
    }
  } else {
    # Optimist: percorre de cima pra baixo; primeiro '<' define a classe = count
    for (i in seq_len(n)) {
      class_i <- 0L
      count <- 0L
      for (hh in seq(from = n_profiles, to = 1L, by = -1L)) {
        count <- count + 1L
        if (Sx_b[i, hh] < cut_level && Sb_x[i, hh] >= cut_level) {
          class_i <- as.integer(count)
        }
      }
      cls[i] <- class_i
    }
  }
  
  return(cls)  # 0..(p-1), igual ao Python
}
