library (igraph)

G = make_empty_graph (n = 100) 

G = assign_bmi (G)
G = add_foci_nodes (G)
G = add_foci_edges (G)
G = get_color (G)



plot (G, vertex.size = V(G)$size, vertex.shape = V (G)$shape, layout = layout_with_kk)



assign_bmi <- function (lg) {
  V (lg)$name = as.integer (runif (length (V(lg)), 15, 41))
  V (lg)$size = V (lg)$name - 10
  V (lg)$type = "person"
  V (lg)$color = "yellow"
  V (lg)$shape = "circle"
  return (lg)
}

add_foci_nodes <- function (lg) {
  lg = lg + vertices (name = c("gym", "eatout", "movie club", "karate club", "yoga club"), shape = "rectangle", size = 25, type = "foci", color = "blue")
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
      if (V (lg)$name [i] > 32) {
        V (lg)$color [i] = "red"
      }
      else {
        if (V (lg)$name [i] > 22) {
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
