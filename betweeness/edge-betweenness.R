library (igraph)


find_community <- function (G) {
  g = G
  com = components (g)$no
  
  while (com == 1) {
    
    cat ("Number of components: ", com, "\n")
    ebt = edge_betweenness (g)  
    g = g -  E(g) [which.max (ebt)]
    com = components (g)$no
  }
  
  cat ("Number of components: ", com, "\n")
  
  com1 = groups (components (g)) [1]
  com2 = groups (components (g)) [2]
  comps = c(c(com1), c(com2))
  
  return (comps)
}

show_communities <- function (G) {
  
  comps = find_community (G)
  
  c1 = as.integer (comps$`1`)
  c2 = as.integer (comps$`2`)
  print (c1)
  print (c2)
  
  V(G)$color = ifelse (V(G)$name %in% c2, "green", "blue")
  
  # for (j in 1:length (c1)) {
  #   for (i in 1:length (V (G))) {
  #     if (as.integer (V(G)$name [i]) == c1 [j]) {
  #       V(G)$color [i] = "green"  
  #     }
  #   }
  # }
  # print (V(G)$color)

  plot (G, color = V(G)$color, vertex.size = 8)  
}


G = read.csv("~/ankit/Github/Social-Network-Analysis/dolphin.csv")
G = graph_from_data_frame (G, directed = FALSE)
show_communities (G)
