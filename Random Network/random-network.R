library(igraph)

Rfb = erdos.renyi.game(5000, 0.03)
t = table(degree(Rfb))
# Plotting the frequency plot of degree of each node
plot(t / sum(t),xlab="Degree", ylab="Frequency")