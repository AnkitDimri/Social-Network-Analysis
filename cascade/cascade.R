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
    if (length (V (G) [V (G)$accepted == "A"]) == length (V (G))) {
      G = color (G)
      plot (G, vertex.color = V (G)$color)
      break
    }
  }
  
  size = length (V (G) [V (G)$accepted == "A"])
  cat ("\nCascade size is", size, "out of", length (V (G)), "nodes\n")
  
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
  
  a = c ()
  for (i in 1:length (V (lg))) 
    if (V(lg) [i]$accepted == "A")
      a = c (a, V (lg) [i])
    
  u = c ()
  n = c ()
  for (i in 1:length (a)) {
    n = neighbors (lg, a [i])
    u = union (u, n)
  }
  
  u_ = intersect (u, a)
  if (length (u_ != 0))
    u = u [u != intersect (u,a)]
  s = c ()
  flag = 0
  print (u)
  for (i in 1:length (u)) {
    n = neighbors (lg, u [i])
    count = 0
    
    for (j in 1:length (n))
      if (V (lg) [n [j]]$accepted == "A")
        count = count + 1
    if (count / length (n) >= t) {
      s = c (s, u [i])
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

