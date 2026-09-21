library(igraph)
library(knitr)

analisar_grafo <- function(grafo, titulo = "") {
  
  # Função para contar ciclos (aproximação para grafos grandes)
  contar_ciclos <- function(g) {
    m <- ecount(g)
    n <- vcount(g)
    comp <- components(g)$no
    return(m - n + comp)
  }
  
  # Função para encontrar o maior ciclo (aproximação)
  maior_ciclo <- function(g) {
    if (ecount(g) == 0) return(0)
    if (vcount(g) <= 600) {
      tryCatch({
        g_temp <- g
        while(any(degree(g_temp) < 2) && vcount(g_temp) > 2) {
          g_temp <- delete_vertices(g_temp, which(degree(g_temp) < 2))
        }
        if (vcount(g_temp) > 2) {
          diam <- diameter(g_temp, weights = NA)
          return(diam)
        } else {
          return(0)
        }
      }, error = function(e) {
        return(0)
      })
    } else {
      return(0) 
    }
  }
  
  # Calcula as estatísticas
  stats <- data.frame(
    Estatística = c(
      "Número de vértices",
      "Número de arestas",
      "Grau máximo",
      "Grau mínimo",
      "Grau médio",
      "Densidade",
      "Número de triângulos",
      "Número total de ciclos",
      "Maior ciclo",
      "Componentes conexos",
      "É conexo?",
      "Diâmetro"
    ),
    Valor = c(
      vcount(grafo),
      ecount(grafo),
      max(degree(grafo)),
      min(degree(grafo)),
      round(mean(degree(grafo)), 2),
      round(edge_density(grafo), 4),
      sum(count_triangles(grafo)) / 3,
      contar_ciclos(grafo),
      maior_ciclo(grafo),
      components(grafo)$no,
      ifelse(is_connected(grafo), "Sim", "Não"),
      ifelse(is_connected(grafo), diameter(grafo), "N/A")
    )
  )
  
  # Retorna tabela formatada para Rmarkdown
  return(kable(stats, 
               format = "markdown", 
               caption = paste0("Estatísticas do Grafo", titulo),
               align = c("l", "r")))
}

g <- sample_gnm(100, 200)
analisar_grafo(g, " oi")
