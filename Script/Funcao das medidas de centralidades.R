
# Pacotes ####

library("knitr")
library("kableExtra")
library("igraph")
library("rgraph6")
library("formattable")
library("dplyr")
library("purrr")
library("gridExtra")
library("gt")
library("flextable")
library("DT")
library("forcats")

# Funcoes ####

# centralidade de grau

C_grau <- function(G){
  return(degree(G))
}

# centralidade de proximidade

C_proximidade <- function(G){
  return(round(closeness(G), 5))
}

#centralidade de intermediaçao 

C_intermediacao <- function(G){
  return(round(betweenness(G, directed = F), 5))
}

# centralidade de K-Passeio

C_kpasseio <- function(G, N_passeio){
  for (n in 1:N_passeio) {
    B <- as.matrix(as_adjacency_matrix(G))
    if (n == 1){
      c<-0
    }
    else{
      for (j in 1:(n-1)){
        B <- B%*%as.matrix(as_adjacency_matrix(G))
      }
    }
    c <- B + c
  }
  ck <- as.vector(t(c%*%as.vector(rep(1,ncol(as.matrix(as_adjacency_matrix(G)))))))
  return(ck)
}

#centralidade de Katz 

C_katz <- function(G){
  return(round(alpha.centrality(G, alpha = (round(1/eigen_centrality(G, directed = F, scale = T, weights = NULL)$value,3) - 0.01)), 5))
}

# centralidade de autovetor 

C_autovetor <- function(G){
  return(round(eigen_centrality(G, directed = F, scale = T, weights = NULL)$vector, 5))
}

#centralidade de centroide

C_centroide <- function(G, centro) {
  return(round(distances(G)[centro,], 5))
}

#centralidade delta

C_delta <- function(G){
  cde <- vector()
  for (j in 1:vcount(G)){
    adj <- as.matrix(as_adjacency_matrix(G))
    cde <- c(cde,(ecount(G) - ecount(graph_from_adjacency_matrix(adj[-j,-j],mode = "undirected", weighted = NULL)))/ecount(G))
  }
  return(round(cde,5))
}

# centralidade de kirkland

C_kirkland <- function(G){
  ck <- vector()
  autval <- round(sort(eigen(as.matrix(laplacian_matrix(G)))$values),10)[2]
  for (j in 1:vcount(G)){
    adj <- as.matrix(as_adjacency_matrix(G))
    ck <- c(ck,((round(sort(eigen(laplacian_matrix(graph_from_adjacency_matrix(adj[-j,-j])))$values),10)[2])/(autval)))
  }
  return(round(ck, 5))
}

# centralidade de informacao

C_informacao <- function(G){
  library('sna')
  cinf <- infocent(as.matrix(as_adjacency_matrix(G)), gmode = "graph", rescale = F)
  detach("package:sna", unload = TRUE)
  return(round(cinf,5))
}

# centralidade A_alpha

C_A_alpha <- function(g, a, c = 1) {
  Aa <- ((a * diag(degree(g))) + ((1 - a) * as.matrix(as_adjacency_matrix(g))))
  indice <- which.max(eigen(Aa)$values)
  return(round(abs(eigen(Aa)$vectors[, indice]),8))
}

# centralidade B_alpha
 
C_B_alpha <- function(g, a) {
  L = diag(degree(g)) - as.matrix(as_adjacency_matrix(g))
  Bb <- (a * as.matrix(as_adjacency_matrix(g)) + ((1 - a) * L))
  indice <- which.max(eigen(Bb)$values)
  Cc <- round(eigen(Bb)$vectors[, indice],8)
  if (all(Cc <= 0)) {
    return(abs(Cc))
  }
  else{
    return(Cc)
  }
}















