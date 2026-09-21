library(igraph)

# Função para criar grade NxM
criar_grade <- function(n, m, vizinhanca = 4) {
  g <- make_lattice(c(n, m), nei = ifelse(vizinhanca == 4, 1, 2), directed = FALSE)
  return(g)
}

# Função para gerar versões (completo, esparso conexo, MST)
gerar_variacoes <- function(g, seed_offset = 0) {
  # Completo
  g_completo <- g
  
  # Esparso: remover 20% das arestas, garantindo conectividade
  repeat {
    set.seed(123 + seed_offset)
    g_esparso <- delete_edges(g, sample(E(g), length(E(g)) * 0.2))
    if (is_connected(g_esparso)) break
    seed_offset <- seed_offset + 1  # muda a semente até ficar conexo
  }
  
  # MST: atribuir pesos aleatórios e extrair árvore geradora mínima
  set.seed(456 + seed_offset)
  E(g)$weight <- runif(ecount(g))
  g_mst <- mst(g, weights = E(g)$weight)
  
  return(list(completo = g_completo, esparso = g_esparso, mst = g_mst))
}

# Gerar todos os grafos (24 no total)
gerar_base_assis <- function() {
  tamanhos <- list(c(32,16), c(32,32))  # 512 e 1024 vértices
  vizinhancas <- c(4,8)
  instancias <- c("A","B")
  
  base <- list()
  for (tam in tamanhos) {
    for (viz in vizinhancas) {
      for (inst in instancias) {
        seed_offset <- ifelse(inst == "A", 0, 100)
        g <- criar_grade(tam[1], tam[2], viz)
        variacoes <- gerar_variacoes(g, seed_offset)
        
        # Criar entradas separadas para cada versão
        for (versao in names(variacoes)) {
          nome_base <- paste0("grade_", tam[1], "x", tam[2],
                              "_viz", viz, "_", inst, "_", versao)
          base[[nome_base]] <- variacoes[[versao]]
        }
      }
    }
  }
  return(base)
}

# Executar
base_assis <- gerar_base_assis()


# Função para gerar versões (esparso conexo)
gerar_ec <- function(g, seed_offset = 0) {
  # Esparso: remover 20% das arestas, garantindo conectividade
  repeat {
    g_esparso <- delete_edges(g, sample(E(g), length(E(g)) * 0.2))
    if (is_connected(g_esparso)) break
    seed_offset <- seed_offset + 1  # muda a semente até ficar conexo
  }
  
  return(list(esparso = g_esparso))
}

gerar_base_ec <- function(N = 1) {
  tamanhos <- list(c(32,16))  
  vizinhancas <- 4
  instancias <- c("A")
  
  base <- list()
  for (n in 1:N){
  for (tam in tamanhos) {
    for (viz in vizinhancas) {
      for (inst in instancias) {
        seed_offset <- n
        g <- criar_grade(tam[1], tam[2], viz)
        variacoes <- gerar_ec(g, seed_offset)
        
        # Criar entradas separadas para cada versão
        for (versao in names(variacoes)) {
          nome_base <- paste0("grade_", tam[1], "x", tam[2],
                              "_viz", viz, "_", inst, "_", versao)
          base[[n]] <- variacoes[[versao]]
        }
      }
    }
  }
  }
  return(base)
}

# Função para gerar versões (esparso conexo)
gerar_mst <- function(g, seed_offset = 0) {
  # MST: atribuir pesos aleatórios e extrair árvore geradora mínima
  E(g)$weight <- runif(ecount(g))
  g_mst <- mst(g, weights = E(g)$weight)
  
  return(list(mst = g_mst))
}

# Gerar todos os grafos (24 no total)
gerar_base_mst <- function(N = 1) {
  tamanhos <- list(c(32,16))  # 512 e 1024 vértices
  vizinhancas <- 4
  instancias <- c("A")
  
  base <- list()
  for (n in 1:N){
    for (tam in tamanhos) {
      for (viz in vizinhancas) {
        for (inst in instancias) {
          seed_offset <- n
          g <- criar_grade(tam[1], tam[2], viz)
          variacoes <- gerar_mst(g, seed_offset)
          
          # Criar entradas separadas para cada versão
          for (versao in names(variacoes)) {
            nome_base <- paste0("grade_", tam[1], "x", tam[2],
                                "_viz", viz, "_", inst, "_", versao)
            base[[n]] <- variacoes[[versao]]
          }
        }
      }
    }
  }
  return(base)
}

# Função para gerar versões (esparso conexo)
gerar_comp <- function(g, seed_offset = 0) {
  # Completo
  g_completo <- g
  
  return(list(completo = g_completo))
}

gerar_base_cc <- function(N = 1) {
  tamanhos <- list(c(32,16))  # 512 e 1024 vértices
  vizinhancas <- 4
  instancias <- c("A")
  
  base <- list()
  for (n in 1:N){
    for (tam in tamanhos) {
      for (viz in vizinhancas) {
        for (inst in instancias) {
          seed_offset <- n
          g <- criar_grade(tam[1], tam[2], viz)
          variacoes <- gerar_comp(g, seed_offset)
          
          # Criar entradas separadas para cada versão
          for (versao in names(variacoes)) {
            nome_base <- paste0("grade_", tam[1], "x", tam[2],
                                "_viz", viz, "_", inst, "_", versao)
            base[[n]] <- variacoes[[versao]]
          }
        }
      }
    }
  }
  return(base)
}

gerar_base_cc()

# Gerando e criando a base mst e ec

library(igraph)

gerar_grafos_unicos_ec <- function(n_grafos) {
  grafos_unicos <- list()
  hashes <- character()
  
  while(length(grafos_unicos) < n_grafos) {
    # Usa sua função para gerar um grafo
    novo_grafo <- gerar_base_ec(1)[[1]]  # Assumindo que retorna uma lista de grafos
    
    # Verifica se o grafo é válido (pode ser NULL se não conseguir gerar)
    if(is.null(novo_grafo)) next
    
    # Calcula o código canônico (invariante completo)
    canonico <- tryCatch({
      canonical_permutation(novo_grafo)$labeling
    }, error = function(e) NULL)
    
    if(is.null(canonico)) next
    
    # Usa o grafo canônico como chave
    grafo_canonico <- permute(novo_grafo, canonico)
    hash_grafo <- paste(as.numeric(grafo_canonico[]), collapse = "")
    
    if(!(hash_grafo %in% hashes)) {
      grafos_unicos[[length(grafos_unicos) + 1]] <- novo_grafo
      hashes <- c(hashes, hash_grafo)
      cat("Grafos únicos gerados:", length(grafos_unicos), "/", n_grafos, "\n")
    }
  }
  
  return(grafos_unicos)
}

gerar_grafos_unicos_mst <- function(n_grafos) {
  grafos_unicos <- list()
  hashes <- character()
  
  while(length(grafos_unicos) < n_grafos) {
    # Usa sua função para gerar um grafo
    novo_grafo <- gerar_base_mst(1)[[1]]  # Assumindo que retorna uma lista de grafos
    
    # Verifica se o grafo é válido (pode ser NULL se não conseguir gerar)
    if(is.null(novo_grafo)) next
    
    # Calcula o código canônico (invariante completo)
    canonico <- tryCatch({
      canonical_permutation(novo_grafo)$labeling
    }, error = function(e) NULL)
    
    if(is.null(canonico)) next
    
    # Usa o grafo canônico como chave
    grafo_canonico <- permute(novo_grafo, canonico)
    hash_grafo <- paste(as.numeric(grafo_canonico[]), collapse = "")
    
    if(!(hash_grafo %in% hashes)) {
      grafos_unicos[[length(grafos_unicos) + 1]] <- novo_grafo
      hashes <- c(hashes, hash_grafo)
      cat("Grafos únicos gerados:", length(grafos_unicos), "/", n_grafos, "\n")
    }
  }
  
  return(grafos_unicos)
}

verificar_isomorfismos <- function(lista_grafos) {
  n <- length(lista_grafos)
  hashes <- character(n)
  
  # Calcula hash canônico para cada grafo
  for(i in 1:n) {
    if(is.null(lista_grafos[[i]])) next
    
    canonico <- tryCatch({
      canonical_permutation(lista_grafos[[i]])$labeling
    }, error = function(e) NULL)
    
    if(!is.null(canonico)) {
      grafo_canonico <- permute(lista_grafos[[i]], canonico)
      hashes[i] <- paste(as.numeric(grafo_canonico[]), collapse = "")
    }
  }
  
  # Encontra duplicatas
  duplicatas <- list()
  hashes_validos <- hashes[hashes != ""]
  
  if(length(hashes_validos) > 0) {
    hash_duplicados <- names(table(hashes_validos)[table(hashes_validos) > 1])
    
    for(hash in hash_duplicados) {
      indices <- which(hashes == hash)
      if(length(indices) > 1) {
        # Para cada grupo de duplicatas, gera todos os pares
        pares <- combn(indices, 2, simplify = FALSE)
        for(par in pares) {
          duplicatas[[length(duplicatas) + 1]] <- par
        }
      }
    }
  }
  
  return(list(
    tem_isomorfos = length(duplicatas) > 0,
    total_duplicatas = length(duplicatas)
  ))
}

#grafos_ec <- gerar_grafos_unicos_ec(1000)
#grafos_mst <- gerar_grafos_unicos_mst(1000)

#verificar_isomorfismos(grafos_ec)
#verificar_isomorfismos(grafos_mst)

#saveRDS(grafos_ec, file =  "lista_grafos/grafos_ec.rds")
#saveRDS(grafos_mst, file =  "lista_grafos/grafos_mst.rds")


