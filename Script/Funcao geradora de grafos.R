# Pacotes
library(igraph)

# Funcao 

# Gera grafos uniciclicos atraves de 3 metodos: ####

## cycle_tree = Criar ciclo base e adicionar árvores (pode ser desconexo)
## add_edge = Começar com árvore e adicionar 1 aresta (conexo)
## random_walk = Caminhada aleatória + árvores (conexo)
 
gerar_uniciclico <- function(n, metodo = "add_edge") {
  
  if(n < 3) stop("n deve ser >= 3")
  
  if(metodo == "add_edge") {
    # Método mais confiável: árvore + 1 aresta
    for(tentativa in 1:100) {
      arvore <- sample_tree(n)
      
      # Escolher vértices aleatórios até encontrar não adjacentes
      u <- sample(1:n, 1)
      v <- sample((1:n)[-u], 1)
      
      if(!are_adjacent(arvore, u, v)) {
        grafo <- add_edges(arvore, c(u, v))
        return(grafo)
      }
    }
    # Se falhar, tentar recursivamente
    return(gerar_uniciclico(n, metodo))
    
  } else if(metodo == "cycle_tree") {
    # Método: ciclo base + árvores
    tamanho_ciclo <- sample(3:min(n, n-1), 1)
    grafo <- make_ring(tamanho_ciclo, directed = FALSE)
    
    # Adicionar vértices restantes
    if(n > tamanho_ciclo) {
      vertices_restantes <- n - tamanho_ciclo
      
      for(i in 1:vertices_restantes) {
        # Adicionar vértice conectado a um vértice aleatório existente
        grafo <- add_vertices(grafo, 1)
        novo_id <- vcount(grafo)
        alvo <- sample(1:(novo_id-1), 1)
        grafo <- add_edges(grafo, c(alvo, novo_id))
      }
    }
    
    return(grafo)
    
  } else if(metodo == "random_walk") {
    # Método: construir incrementalmente
    grafo <- make_empty_graph(directed = FALSE)
    grafo <- add_vertices(grafo, 1)
    
    for(i in 2:n) {
      grafo <- add_vertices(grafo, 1)
      # Conectar ao grafo existente
      alvo <- sample(1:(i-1), 1)
      grafo <- add_edges(grafo, c(alvo, i))
    }
    
    # Adicionar mais uma aresta para criar o ciclo
    for(tentativa in 1:100) {
      u <- sample(1:n, 1)
      v <- sample((1:n)[-u], 1)
      if(!are_adjacent(grafo, u, v)) {
        grafo <- add_edges(grafo, c(u, v))
        return(grafo)
      }
    }
    
    return(grafo)
  } else {
    stop("Método inválido. Use 'add_edge', 'cycle_tree' ou 'random_walk'")
  }
}

random_connected_gnp <- function(n, p, max_trials = 1000) {
  for (i in 1:max_trials) {
    g <- sample_gnp(n, p)
    if (is_connected(g)) {
      message("Grafo conexo encontrado na tentativa ", i)
      return(g)
    }
  }
  stop(paste("Não foi possível gerar grafo conexo após", max_trials, "tentativas. Tente aumentar p."))
}


#plot(gerar_uniciclico(25, metodo = "random_walk"), main = "Unicíclico Aleatório (n = ???, método = ???)",
#     vertex.color = "lightgreen", vertex.size = 20,
#     layout = layout_with_fr)


#####

#plot(sample_k_regular(10, 3), main = "Grafo 3-Regular (Cúbico)")

