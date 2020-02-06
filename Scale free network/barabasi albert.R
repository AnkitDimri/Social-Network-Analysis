library (igraph)

G = barabasi.game (10000, directed = F)
t = table (degree (G))
plot (t, xlab = "Degree", ylab = "Frequency", main="Barabsi-Albert SFN using barabasi.game")


# Implementing Scale free network to show it follows power low in degree distribution

sfn <- function (num) {
  
  G = make_empty_graph (n = num)
  G = G + edge (1, 2)
  
  for (i in 3:num) {
    for (j in 1:i-1) {
      j = j+1
      p = (degree (G) [j] )/ (2*length(E(G)))
      prob = runif (1)
      
      if (prob < p) {
        G = G + edge(i, j)
      }
    }
  }
  
  t = table (degree (G))
  plot (t, xlab = "degree", ylab = "frequency", main="Implementation of Scale Free Network using Barabsi-Albert algorithm")
}

sfn (300)

