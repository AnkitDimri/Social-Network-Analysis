input:

> cascade ("../karate.gml", "gml", 0.3)

output:

[1]  2  3  4  5  6  7  8  9 11 12 13 14 18 20 22 32
[1]  2  3  4  6  7  8  9 14 32 34
[1]  3  8  9 14 32 31 17 34
[1]  9 32 31 10 28 29 33 34
[1] 32 31 28 33 34
[1] 28 33 34 25 26
[1] 28 24 15 16 19 21 23 30 27

Cascade size is 34 out of 34 nodes


####################
special code snippet change:

cascade <- function (graph, frmt, threshold) {

  G = read_graph (graph, format = frmt)
  V (G)$accepted = "B"
  G = color (G)
  plot (G, vertex.color = V (G)$color)
  #
  # G = seeder (G)
  # G =  color (G)
  # plot (G, vertex.color = V (G)$color)
  #
  # G = seeder (G)
  # G = color (G)
  # plot (G, vertex.color = V (G)$color)

  ## Lets seed highest degrees
  V (G) [1]$accepted = "A"
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
