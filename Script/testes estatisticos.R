##### Pacotes ####

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

##### Lista dos grafos ####

source("Script/Base de dados house graphs.R")
source("Script/Funcao geradora da base artificial de assis et all (LAURA).R")

##### Funcoes das medidas de centralidade ####

source("Script/Funcao das medidas de centralidades.R")

##### Funcoes dos testes estatisticos ####

source("Script/Funcao dos testes estatisticos.R")

##### Funcoes da geracao de grafos ####

source("Script/Funcao geradora de grafos.R")

##### Teste ####

# Gerando n grafos uniciclicos aleatorios 

n <- 1000

# Lista de grafos uniciclicos
uni_l <- list()
sample_l <- list()

for (i in 1:n){
  uni_l[[i]] <- gerar_uniciclico(100, metodo = "random_walk")
  sample_l[[i]] <- random_connected_gnp(100, 0.1)
}

plot(sample_l[[2]])

# Lista das correlacoes de grau
corr_degree_l  <- list()
corr_degree_u  <- list()

for (i in 1:n){
  corr_degree_u[i] <- Teste_corr_grau(uni_l[[i]], C_B_alpha, 0.7)[[1]]
  corr_degree_l[i] <- Teste_corr_grau(sample_l[[i]], C_B_alpha, 0.7)[[1]]
}

# Lista das correlacoes de eigenvector
corr_eigenvector_l  <- list()
corr_eigenvector_u  <- list()

for (i in 1:n){
  corr_eigenvector_u[i] <- Teste_corr_eigenvector(uni_l[[i]], C_B_alpha, 0.7)[[1]]
  corr_eigenvector_l[i] <- Teste_corr_eigenvector(sample_l[[i]], C_B_alpha, 0.7)[[1]]
  
}

# Lista das correlacoes de a_alpha
corr_a_alpha_l  <- list()
corr_a_alpha_u  <- list()


for (i in 1:n){
  corr_a_alpha_u[i] <- Teste_corr_a_alpha(uni_l[[i]], C_B_alpha, 0.7)[[1]]
  corr_a_alpha_l[i] <- Teste_corr_a_alpha(sample_l[[i]], C_B_alpha, 0.7)[[1]]
}

# Histograma das corr b_alpha e grau

x <- unlist(corr_degree_l, use.names = FALSE)
hist(x)

x <- unlist(corr_degree_u, use.names = FALSE)
hist(x)

# Histograma das corr b_alpha e eigenvector

x <- unlist(corr_eigenvector_l, use.names = FALSE)
hist(x)

x <- unlist(corr_eigenvector_u, use.names = FALSE)
hist(x)

# Histograma das corr b_alpha e a_alpha

x <- unlist(corr_a_alpha_l, use.names = FALSE)
hist(x)

x <- unlist(corr_a_alpha_u, use.names = FALSE)
hist(x)

