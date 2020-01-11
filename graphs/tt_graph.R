library (igraph)

tt = make_empty_graph (n=100)
tt = tt + edge (1, 2)
tt = tt + edge (1, 3)
tt = tt + edge (1, 4)

vis <- c (1, 2, 3, 4)
i = 2
while (length (vis) < 100) {
  
  j = i+1
  while (is.element (j, vis) && j < 101) {
    j = j+1
  }
  
  tt = tt + edge (i, j)
  tt = tt + edge (i, j+1)
  
  vis <- c(vis, j)
  vis <- c(vis, j+1)
  
  i = i+1
}

plot (tt, vertex.color = "yellow", edge.color = "blue", edge.arrow.size = 0.1, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-4,5), vertex.size=50)
