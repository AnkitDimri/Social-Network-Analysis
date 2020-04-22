library (igraph) # header file

cascade ("../karate.gml", "gml", 0.3)

cascade <- function (graph, frmt, threshold) {
  
  # Read the graph from file
  G = read_graph (graph, format = frmt)
  V (G)$accepted = "B" # Initial behaviour/ technology etc
  G = color (G)
  plot (G, vertex.color = V (G)$color)
  
  G = seeder (G)
  G =  color (G)
  plot (G, vertex.color = V (G)$color)
  
  G = seeder (G)
  G = color (G)
  plot (G, vertex.color = V (G)$color) # plot showing the initial 2 seeds
  
  # start cascading
  G = spread (G, threshold)
  
  # cascde while there is a change
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
  
  # print the cascade size
  size = length (V (G) [V (G)$accepted == "A"])
  cat ("\nCascade size is", size, "out of", length (V (G)), "nodes\n")
  
}

# Return a randomly seeded node
seeder <- function (lg) {
  
  r = runif (1, 1, length (V (lg)) + 1)
  r = floor (r)
  
  V (lg)$accepted [r] = "A" # New node seeded with new behaviour/ Technology erc
  
  return (lg)
}

# Green color for cascaded and red for not cascaded nodes
color <- function (lg) {
  
  V (lg)$color = ifelse (V (lg)$accepted == "A", "green", "orange")
  
  return (lg)
}

# Spread the cascade at each iteration to neighbouring nodes which have their thershold value fulfilled
spread <- function (lg, t) {
  
  # Find all the cascaded nodes
  a = c ()
  for (i in 1:length (V (lg))) 
    if (V(lg) [i]$accepted == "A")
      a = c (a, V (lg) [i])
    
  u = c ()
  n = c ()
  # Set of all neighbours of the cascaded nodes
  for (i in 1:length (a)) {
    n = neighbors (lg, a [i])
    u = union (u, n)
  }
  
  # Remove already cascaded nodes from the neighbours set
  for (i in 1:length (a))
    u = u [ u != a [i]]
  
  s = c ()
  flag = 0 # check for change
  print (u) # Print the nodes which are succeptible to cascade in a given iteration
  # Check their value and spread the cascade if threshold reached
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


