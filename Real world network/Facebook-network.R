library(igraph)

G = read_graph("/home/ankit/ankit/Github/Social-Network-Analysis/Real world network/facebook-data.txt",format = "edgelist")
t = table(degree(G))
plot(t / sum(t),xlab="Degree", ylab="Frequency")