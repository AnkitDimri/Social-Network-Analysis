library (igraph)

G= graph (c(1,'A', 2,"B", 3, "A", 4, "C", 1, "C"), directed = F)

V(G)$type= c(T,F,T,F,T,T,F)

V(G)$color= ifelse (V(G)$type, "lightblue", "salmon")
V(G)$shape= ifelse(V(G)$type, "square", "circle")
plot(G, layout=layout.bipartite, edge.color="black")
write_graph(G, "~/Ankit/sna/graphs/bipartite.csv", format = "edgelist")

g=read_graph ( "~/Ankit/sna/graphs/facebook_combined.txt", format="edgelist")
tkplot (g)
