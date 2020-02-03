library(igraph)

D = read.csv("/home/ankit/ankit/Github/Social-Network-Analysis/Real world network/dolphin.csv")

D=data.frame(D)
G=graph_from_data_frame(D)
t = table(degree(G))
plot(t , xlab="Degree", ylab="Frequency")

