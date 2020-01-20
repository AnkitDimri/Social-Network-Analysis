library (igraph)

g= read_graph("~/Ankit/sna/graphs/Friendship-network_data_2013.csv", format="edgelist")

V(g)
length (V(g))
length (E(g))

print (g)

V(g)$labels = NA
plot (g, vertex.color = "yellow", edge.color = "blue", edge.arrow.size = 0.1, layout = layout_with_kk, rescale=FALSE, xlim=c(-100,100), ylim=c(-100,100), vertex.size=15)

tkplot (g, vertex.color = "yellow", edge.color = "blue", edge.arrow.size = 0.1, layout = layout_with_kk,vertex.size=5, vertex.size=2)


reciprocity (g)


gu = as.undirected(g, mode="mutual")
V(gu)$labels = NA

tkplot (gu, vertex.color = "black", edge.color = "red", edge.arrow.size = 0.1, layout = layout_with_kk, vertex.size=2)


components(gu)

comps = decompose (gu)
tkplot (comps [[2]], vertex.color = "black", edge.color = "red", edge.arrow.size = 0.1, layout = layout_with_kk, vertex.size=2)

metadata_df = read.delim("~/Ankit/sna/graphs/metadata_2013.txt", sep = "\t", header=F)
metadata_df = as.data.frame(metadata_df)
metadata_df [1,3]

gender = as.data.frame.list(metadata_df [c (1, 3)])
gender

gdf = read.csv("~/Ankit/sna/graphs/Friendship-network_data_2013.csv", sep=" ", header=F)
G= graph_from_data_frame(gdf, directed=T, vertices = metadata_df)

metadata_df

V(G)$name

G

G=decompose(G)

G


tkplot (, vertex.color = "black", edge.color = "red", edge.arrow.size = 0.1, layout = layout_with_kk, vertex.size=2)
