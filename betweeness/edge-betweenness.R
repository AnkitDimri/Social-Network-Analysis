library (igraph)

G = read.csv("~/ankit/Github/Social-Network-Analysis/dolphin.csv")
G = graph_from_data_frame (G, directed = FALSE)


find_community <-function (G) {
  com = components (G)$no
  
  while (com == 1) {
    
    cat ("Number of components: ", com, "\n")
    ebt = edge_betweenness (G)  
    G = G -  E(G) [which.max (ebt)]
    com = components (G)$no
  }
  
  cat ("Number of components: ", com, "\n")
  
  com1 = groups (components (G)) [1]
  com2 = groups (components (G)) [2]
  comps = c(c(com1), c(com2))
  
  return (comps)
}
