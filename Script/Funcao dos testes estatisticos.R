
##### Pacotes ####

library("igraph")

# Funcoes ####

source("Script/Funcao das medidas de centralidades.R")

# teste de correlacao de pearson e spearman

Teste_corr_completo <- function(G, centralidade, alpha) {
  # Calcular medidas tradicionais
  degree_cent <- degree(G, normalized = TRUE)
  a_alpha_cent <- C_A_alpha(G, alpha)
  eigenvector_cent <- eigen_centrality(G)$vector
  kirkiland_cent <- C_kirkland(G)
  
  #nova medida
  medida <- centralidade(G, alpha)
  
  # Lista de medidas para comparar
  medidas <- list(
    degree = degree_cent,
    kirkland = kirkiland_cent,
    eigenvector = eigenvector_cent,
    a_alpha = a_alpha_cent
  )
  
  resultados <- list()
  
  for(nome in names(medidas)) {
    # Alinhar vertices
    vertices <- V(G)
    x <- medida[vertices]
    y <- medidas[[nome]][vertices]
    
    # Correlação de Pearson
    pearson_test <- cor.test(x, y, method = "pearson")
    
    # Correlação de Spearman
    spearman_test <- cor.test(x, y, method = "spearman")
    
    resultados[[nome]] <- list(
      pearson = list(r = pearson_test$estimate),
      spearman = list(rho = spearman_test$estimate)
    )
  }
  return(resultados)
}

Teste_corr_p_grau <- function(G, centralidade, alpha){
  
  x <- centralidade(G, alpha)
  y <- degree(G, normalized = TRUE)
  
  x <- round(x, 10)
  y <- round(y, 10)  
  return(cor.test(x, y, method = "pearson")$estimate)
}

Teste_corr_p_kirkland <- function(G, centralidade, alpha){
  
  x <- centralidade(G, alpha)
  y <- C_kirkland(G)
  
  x <- round(x, 10)
  y <- round(y, 10)   
  return(cor.test(x, y, method = "pearson")$estimate)
}

Teste_corr_p_eigenvector <- function(G, centralidade, alpha){
  
  x <- centralidade(G, alpha)
  y <- eigen_centrality(G)$vector
  
  x <- round(x, 10)
  y <- round(y, 10)   
  return(cor.test(x, y, method = "pearson")$estimate)
}

Teste_corr_a_p_alpha <- function(G, centralidade, alpha){
  
  x <- centralidade(G, alpha)
  y <- C_A_alpha(G, alpha)
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "pearson")$estimate)
}

#####
Teste_corr_grau_eig_p <- function(G){
  
  x <- degree(G, normalized = TRUE)
  y <- eigen_centrality(G)$vector
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "pearson")$estimate)
}

Teste_corr_grau_aa_p <- function(G, alpha){
  
  x <- degree(G, normalized = TRUE)
  y <- C_A_alpha(G, alpha)  
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "pearson")$estimate)
}

Teste_corr_eig_aa_p <- function(G, alpha){
  
  x <- eigen_centrality(G)$vector
  y <- C_A_alpha(G, alpha)  
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "pearson")$estimate)
}



Teste_corr_s_grau <- function(G, centralidade, alpha){
  
  x <- centralidade(G, alpha)
  y <- degree(G, normalized = TRUE)
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "spearman")$estimate)
}

Teste_corr_s_kirkland <- function(G, centralidade, alpha){
  
  x <- centralidade(G, alpha)
  y <- C_kirkland(G)
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "spearman")$estimate)
}

Teste_corr_s_eigenvector <- function(G, centralidade, alpha){
  
  x <- centralidade(G, alpha)
  y <- eigen_centrality(G)$vector
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "spearman")$estimate)
}

Teste_corr_a_s_alpha <- function(G, centralidade, alpha){
  
  aa <- (1 - alpha)/alpha
  aa <- round(aa, 10)
  
  x <- centralidade(G, alpha)
  y <- C_A_alpha(G, aa, alpha)
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "spearman")$estimate)
}

#####
Teste_corr_grau_eig_s <- function(G){
  
  x <- degree(G, normalized = TRUE)
  y <- eigen_centrality(G)$vector
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "spearman")$estimate)
}

Teste_corr_grau_aa_s <- function(G, alpha){
  
  x <- degree(G, normalized = TRUE)
  y <- C_A_alpha(G, alpha)  
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "spearman")$estimate)
}

Teste_corr_eig_aa_s <- function(G, alpha){
  
  x <- eigen_centrality(G)$vector
  y <- C_A_alpha(G, alpha)  
  
  x <- round(x, 10)
  y <- round(y, 10) 
  return(cor.test(x, y, method = "spearman")$estimate)
}

# Função que retorna os 3 menores e 3 maiores valores outliers
valores_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  
  outliers_menores <- sort(x[x < lim_inf])
  outliers_maiores <- sort(x[x > lim_sup])
  
  menores_3 <- head(outliers_menores, 3)
  maiores_3 <- tail(outliers_maiores, 3)
  
  return(list(menores = menores_3, maiores = maiores_3))
}

# Função que retorna os índices dos 3 menores e 3 maiores outliers
indices_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  
  # Identificar quais são outliers
  is_outlier_menor <- x < lim_inf
  is_outlier_maior <- x > lim_sup
  
  # Pegar índices dos outliers
  indices_menores <- which(is_outlier_menor)
  indices_maiores <- which(is_outlier_maior)
  
  # Ordenar pelos valores para pegar os mais extremos
  if(length(indices_menores) > 0) {
    valores_menores <- x[indices_menores]
    indices_menores <- indices_menores[order(valores_menores)]
    indices_menores_3 <- head(indices_menores, 3)
  } else {
    indices_menores_3 <- numeric(0)
  }
  
  if(length(indices_maiores) > 0) {
    valores_maiores <- x[indices_maiores]
    indices_maiores <- indices_maiores[order(valores_maiores, decreasing = TRUE)]
    indices_maiores_3 <- head(indices_maiores, 3)
  } else {
    indices_maiores_3 <- numeric(0)
  }
  
  return(list(menores = indices_menores_3, maiores = indices_maiores_3))
}
