library(igraph)

#  sample_pa(n = 10000, power = 10, m = NULL, out.dist = NULL, out.seq = NULL,
#           out.pref = FALSE, zero.appeal = 1, directed = TRUE,
#           algorithm = c("psumtree", "psumtree-multiple", "bag"),
#           start.graph = NULL) 

G <- sample_pa(5000)
t = table(degree(G))
plot(t / sum(t),xlab="Degree", ylab="Frequency")
