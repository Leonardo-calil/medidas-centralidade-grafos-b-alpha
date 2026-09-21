
##### Pacotes ####

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

##### Lista Dos grafops house graphs ####

t3 <- readLines("lista_grafos/list_2_graphs.g6")
lista_grafos_3v <- igraph_from_graph6(t3)

t4 <- readLines("lista_grafos/list_4_graphs.g6")
lista_grafos_4v <- igraph_from_graph6(t4)

t5 <- readLines("lista_grafos/list_8_graphs.g6")
lista_grafos_5v <- igraph_from_graph6(t5)

t6 <- readLines("lista_grafos/list_19_graphs.g6")
lista_grafos_6v <- igraph_from_graph6(t6)

t7 <- readLines("lista_grafos/list_30_graphs.g6")
lista_grafos_7v <- igraph_from_graph6(t7)

t8 <- readLines("lista_grafos/list_43_graphs.g6")
lista_grafos_8v <- igraph_from_graph6(t8)

t9 <- readLines("lista_grafos/list_60_graphs_graph6.g6")
lista_grafos_9v <- igraph_from_graph6(t9)


lista_td_grafos <- c(lista_grafos_6v, lista_grafos_7v, lista_grafos_8v,lista_grafos_9v)

rm(lista_grafos_3v, lista_grafos_4v, lista_grafos_5v, t3, t4, t5, t6, t7, t8, t9)
