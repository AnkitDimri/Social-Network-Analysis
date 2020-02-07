library (igraph)
  
G = read_graph("~/ankit/Github/Social-Network-Analysis/sociopatterns/Friendship-network.csv", format = "edgelist", directed = T)
  
# plotting the graph
tkplot (G, vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)

# Read metadata
meta = read.csv ("~/ankit/Github/Social-Network-Analysis/sociopatterns/metadata_2013.txt", header = F, sep = "\t")

# Metadata which does not map to an existing vertex: delete them
for (i in meta [,1]) {
  if (!(i %in% V(G))) {
    print (meta [which (meta$V1 == i),1])
    r <- with (meta, which (V1 == i, arr.ind = T))
    meta = meta [-r,]
  }
}

# converting the graph into undirected graph of mutual friends
UG = as.undirected (G, mode="mutual")

# Print degrees of the graph
V(UG)$degree = igraph::degree (UG)
V(UG)$degree
# Delete vertices with 0 degree
Gd = delete.vertices (UG, V(UG) [V(UG)$degree == 0])
# Plot this graph
tkplot (Gd, vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)
# Plot degree distribution
t = table (V(Gd)$degree)
plot (t)

# Finding and printing the closeness of the graph
V

# Assign genders
V(UG)[meta [,1]]$gender = meta [,3]
# Delete vertices which do not have gender specified
UGG = delete.vertices (UG, V(UG)[is.na(V(UG)$gender)])
UGG = delete.vertices (UGG, V(UGG)$gender == "3")

V(UGG)$type = ifelse(V(UGG)$gender == "1", T , F)
# plot the undirected graph
tkplot (UGG, vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)

# components in the undirected graph
comps = components(UGG)

# decompose the graph into components
comp = decompose(UGG)

# plot the biggest component
g = comp [[2]]
tkplot (g, vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)

