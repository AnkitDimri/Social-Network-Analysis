library (igraph)

G = make_empty_graph (n = 100) 

G = assign_bmi(G)

labels = V(G)$name




plot (G, labels = labels)



assign_bmi <- function(g) {
  V (g)$name = as.integer (runif (length (V(g)), 15, 40))
  V (g)$type = "person"
  return (g)
}
