library(igraph)

Rfb = erdos.renyi.game(4039, 0.0108) #fb graph with number of nodes 4039
t = table(degree(Rfb))
# Plotting the frequency plot of degree of each node
plot(t / sum(t),xlab="Degree", ylab="Frequency")