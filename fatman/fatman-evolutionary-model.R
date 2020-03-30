library (igraph)

G = make_empty_graph (n = 100) 

G = assign_bmi (G)
G = add_foci_nodes (G)
G = add_foci_edges (G)
G = get_color (G)
G = add_homophili (G)
G = closure (G)


tkplot (G, vertex.size = V(G)$size, vertex.shape = V (G)$shape, layout = layout_with_kk)



assign_bmi <- function (lg) {
  V (lg)$name = V (lg)
  V (lg)$bmi = as.integer (runif (length (V(lg)), 15, 41))
  V (lg)$size = V (lg)$bmi - 10
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
  simplify (lg, remove.multiple = TRUE, remove.loops = TRUE)
  
  return (lg)
}
