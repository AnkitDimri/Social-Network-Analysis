library (igraph) # header file

cascade <- function (graph) {
  
  G = read_raph (graph)
  
  V (G)$accepted = "B"
  V (G)$accepted
  
  G = seeder (G)
  
  V (G)$accepted
}

seeder <- function (G) {
  
  r = runif (1, 1, lentgh (V (G)))
  r = floor (r)
  
  V (G)$accepted [r] = "A"
}