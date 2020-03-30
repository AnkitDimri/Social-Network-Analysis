library (igraph)

G = make_empty_graph (n = 100) 

G = assign_bmi (G)
G = add_foci_nodes (G)



plot (G, vertex.size = V(G)$size)



assign_bmi <- function(g) {
  V (g)$name = as.integer (runif (length (V(g)), 15, 40))
  V (g)$size = V (g)$name - 10
  V (g)$type = "person"
  V (g)$shape = "circle"
  return (g)
}

add_foci_nodes <- function (lg) {
  lg = lg + vertices (name = c("gym", "eatout", "movie club", "karate club", "yoga club"), shape = "rectangle", size = 25, type = "foci")
  return (lg)
}
