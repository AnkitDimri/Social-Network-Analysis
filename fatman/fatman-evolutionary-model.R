library (igraph)

evolution (100)

evolution <- function(N) {
  
  G = make_empty_graph (n = N) 
  
  G = assign_bmi (G)
  G = add_foci_nodes (G)
  G = add_foci_edges (G)
  G = get_color (G)
  G = add_homophili (G)
 
  for (i in 1:10){
    
    G = closure (G)
    G = social_influence (G)
    G = get_color (G)
    test.layout <- layout_(G,with_dh(weight.edge.lengths = edge_density(G)/ 100))
    filename = paste ("~/ankit/Github/Social-Network-Analysis/fatman/evolution", i, ".png", sep = "")
    //png (filename, width = 1500, height = 1000)
    plot (G, vertex.size = V(G)$size, vertex.shape = V (G)$shape, layout = layout_with_kk)
    //dev.off()
    
    i = i + 1
  }
  
}

assign_bmi <- function (lg) {
  V (lg)$name = V (lg)
  V (lg)$bmi = as.integer (runif (length (V(lg)), 15, 41))
  V (lg)$size = V (lg)$bmi/2 
  V (lg)$type = "person"
  V (lg)$color = "yellow"
  V (lg)$shape = "circle"
  return (lg)
}

add_foci_nodes <- function (lg) {
  lg = lg + vertices (name = c("gym", "eatout", "movie club", "karate club", "yoga club"), shape = "rectangle", size = 25, type = "foci", color = "blue", bmi = 0)
  return (lg)
}

add_foci_edges <- function (lg) {
  foci_nodes = V(lg) [V(lg)$type == "foci"]
  person_nodes = V(lg) [V(lg)$type == "person"]
  
  for (i in 1:length (person_nodes))
    lg = lg + edge (person_nodes [i], foci_nodes [as.integer (runif (1, 1, 6))])

  lg = as.undirected (lg, mode = "collapse")
  return (lg)
}

get_color <- function (lg) {
  for (i in 1:length (V(lg))) {
    if (V (lg)$type [i] == "person") {
      if (V (lg)$bmi [i] > 32) {
        V (lg)$color [i] = "red"
      }
      else {
        if (V (lg)$bmi [i] > 22) {
          V (lg)$color [i] = "orange"
        }
        else {
          V (lg)$color [i] = "green"
        }
      }
    }
  }
  
  return (lg)
}

add_homophili <- function (lg) {
  
  pnodes = V(lg) [V(lg)$type == "person"]
  
  for (i in 1:length (pnodes)) {
    for (j in 1:length (pnodes)) {
      if (i != j) {
        diff = abs (pnodes$bmi [i] - pnodes$bmi [j])
        prob = 1 / (diff + 1000)
        
        if (runif (1, 0, 1) < prob) {
          lg = lg + edge (pnodes [i], pnodes [j])
        }
      }
    }
  }
  
  lg = as.undirected (lg, mode = "collapse")
  lg = simplify (lg, remove.multiple = TRUE, remove.loops = TRUE)
  
  return (lg)
}

closure <- function (lg) {
  
  for (i in 1:length (V (lg))) 
    for (j in 1:length (V (lg))) 
      if (i != j) 
        if (! (V (lg)$type [i] == "foci" && V (lg)$type [j] == "foci")) {
          k = length (intersect (neighbors (lg, V (lg) [i]), neighbors (lg, V (lg) [j])))
          p = 0.01 # probablity of connecting with 1 common neighbour
          p = 1 - (1 - p)^k
          
          if (runif (1, 0, 1) < p)
            lg = lg + edge (V (lg) [i], V (lg) [j])
        }
  
  lg = as.undirected (lg, mode = "collapse")
  lg = simplify (lg, remove.multiple = TRUE, remove.loops = TRUE)
  
  return (lg)
}

social_influence <- function (lg) {
  
  n_e = neighbors (lg, V (lg) [V (lg)$name == "eatout"])
  n_g = neighbors (lg, V (lg) [V (lg)$name == "gym"])
  
  for (i in 1:length (n_g))
    if (V (lg)$size [n_g [i]] > 15) {
      V (lg)$bmi [n_g [i]] = V (lg)$bmi [n_g [i]] - 1
      V (lg)$size [n_g [i]] = V (lg)$bmi [n_g [i]]/2 
    }
  
  for (i in 1:length (n_e))
    if (V (lg)$size [n_e [i]] < 40) {
      V (lg)$bmi [n_e [i]] = V (lg)$bmi [n_e [i]] + 1
      V (lg)$size [n_e [i]] = V (lg)$bmi [n_e [i]]/2
    }
  
  return (lg)
}
