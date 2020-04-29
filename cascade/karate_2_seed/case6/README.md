input: Here seed node is node 34, highest degree, with threshold set at 0.4 (complete cascade at 0.3)

cascade ("../karate.gml", "gml", 0.4)

output:

[1]  9 10 14 15 16 19 20 21 23 24 27 28 29 30 31 32 33
[1]  3 33 30  9 14 20 24 28 29 31 32
[1]  3 24  9 31 32 14 20 28 29
[1]  1  3 26 28  2 32 14 20 29
[1]  1  3 26 25  2 32 14 20 29
[1]  1  2  4  8 14 29 26 25 32 20
[1]  1  2  4  8 26 25 32 20
[1]  1  2  4  8 26 25 20
[1]  1  2  4  8 20

Cascade size is 20 out of 34 nodes



###############
Special change in code snippet

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
  V (G) [34]$accepted = "A"
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
