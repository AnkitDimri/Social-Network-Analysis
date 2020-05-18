library (igraph)

G = read_graph ("karate.gml", format = "gml")

color <- function (lg) {
  V (lg)$color [V (lg)$status == "susceptible"] = "green"
  V (lg)$color [V (lg)$status == "exposed"] = "yellow"
  V (lg)$color [V (lg)$status == "infectious"] = "red"
  V (lg)$color [V (lg)$status == "recovered"] = "blue"
  
  return (lg)
}

epidemic <- function (lg, seeds, p, t_e, t_i, t_r, itr) {
  
  V (lg)$status = "susceptible"
  V (lg)$status [seeds] = "infectious"
  V (lg)$time = 0
  k = 1

  while (k <= itr) {
    
    V (lg)$time = V (lg)$time + 1 
    # recovering infectious who have passed their infectious time
    for (i in 1:length (V (lg)))
      if (V (lg)$status [i] == "infectious")
        if (V (lg)$time [i] > t_i) {
          V (lg)$status [i] = "recovered"
          V (lg)$time [i] = 0
        }
    
    # making recovered people susceptible
    for (i in 1:length (V (lg)))
      if (V (lg)$status [i] == "recovered")
        if (V (lg)$time [i] > t_r) {
          V (lg)$status [i] = "susceptible"
          V (lg)$time [i] = 0
        }
    
    # making exposed to infectious
    for (i in 1:length (V (lg)))
      if (V (lg)$status [i] == "exposed")
        if (V (lg)$time [i] > t_i) {
          V (lg)$status [i] = "infectious"
          V (lg)$time [i] = 0
        }
          
    
    # infectious spreading the disease (making people exposed)
    exposed = c ()
    for (i in 1:length (V (lg)))
      if (V (lg)$status [i] == "infectious") {
        n = neighbors (lg, V (lg) [i])
        exposed = union (exposed, n)  
      }
    
    exposed = exposed [V (lg)$status [exposed] == "susceptible"]
    
    for (i in 1:length (exposed)) 
      if (runif (1) < p) {
        V (lg)$status [exposed [i]] = "exposed"
        V (lg)$time [exposed [i]] = 0
      }
    
    cat ("\n\n\nDay", k)
    plot (lg, vertex.color = V (color (lg))$color, main = paste ("Day", k))
    k = k+1
    
    # printing the states
    # plot infected
    cat ("\nSusceptible : ", V (lg) [V (lg)$status == "susceptible"])
    cat ("\nInfectious : ", V (lg) [V (lg)$status == "infectious"])
    cat ("\nExposed : ", V (lg) [V (lg)$status == "exposed"])
    cat ("\nRecovered : ", V (lg) [V (lg)$status == "recovered"])
  }  
}

epidemic (G, c (1, 20), 0.4, 3, 1, 8, 20)


# DOLPHIN NETWORK (alpha = 5, beta = 0.9)
D = read.csv("dolphin.csv", header = F)
D = data.frame (D)
gd = make_empty_graph (n = 62)
# To make graph sorted and traversable for function
for (x in 1:nrow (D))
  gd = gd + edge (D [x, "V1"], D [x, "V2"])
gd = as.undirected (gd, mode = "collapse")

epidemic (gd, c (1), 0.4, 2, 2, 2, 10)
