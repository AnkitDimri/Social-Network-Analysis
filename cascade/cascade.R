library (igraph) # header file

cascade <- function (graph, frmt) {
  
  G = read_graph (graph, format = frmt)
  V (G)$accepted = "B"
  G = color (G)
  plot (G, vertex.color = V (G)$color)
  
  G = seeder (G)
  G =  color (G)
  plot (G, vertex.color = V (G)$color)
  
  G = seeder (G)
  G = color (G)
  plot (G, vertex.color = V (G)$color)
}

seeder <- function (lg) {
  
  r = runif (1, 1, length (V (lg)) + 1)
  r = floor (r)
  
  V (lg)$accepted [r] = "A"
  
  return (lg)
}

color <- function (lg) {
  
  V (lg)$color = ifelse (V (lg)$accepted == "A", "green", "orange")
  
  return (lg)
}


cascade ("../karate.gml", "gml")

