library (igraph) # header file

cascade <- function (graph, frmt, threshold) {
  
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
  
  # start cascading
  G = spread (G, threshold)
  
  while (G$change == T) {
    G = color (G)
    plot (G, vertex.color = V (G)$color)
    G = spread (G, threshold)
  }
  
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

spread <- function (lg, t) {
  
  s = c ()
  flag = 0
  for (i in 1:length (V (lg))) {
    n = neighbors (lg, V (lg) [i])
    count = 0
    
    for (j in 1:length (n))
      if (V (lg) [n [j]]$accepted == "A")
        count = count + 1
    if (count / length (n) >= t) {
      s = c (s, i)
      flag = 1
    }
  }
  
  V (lg) [s]$accepted = "A"
  if (flag)
    lg$change = T
  else
    lg$change = F
  
  return (lg)
}


cascade ("../karate.gml", "gml", 0.3)

