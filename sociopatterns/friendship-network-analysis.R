library (igraph)
  
G = read_graph("~/ankit/Github/Social-Network-Analysis/sociopatterns/Friendship-network.csv", format = "edgelist", directed = T)
  
# plotting the graph
tkplot (G, vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)