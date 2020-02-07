library (igraph)
  
G = read_graph("~/ankit/Github/Social-Network-Analysis/sociopatterns/Friendship-network.csv", format = "edgelist", directed = T)
  
# plotting the graph
tkplot (G, vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)

# converting the graph into undirected graph of mutual friends
UG = as.undirected (G, mode="mutual")
# plot the undirected graph
tkplot (UG, vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)

# components in the undirected graph
comps = components(UG)

# decompose the graph into components
comp = decompose(UG)

# plot the biggest component
g = comp [[2]]
tkplot (g, vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)

