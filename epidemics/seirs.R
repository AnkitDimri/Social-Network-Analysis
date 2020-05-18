library (igraph)

G = read_graph ("karate.gml", format = "gml")

color <- function (lg) {
  V (lg)$color [V (lg)$status == "susceptible"] = "green"
  V (lg)$color [V (lg)$status == "exposed"] = "yellow"
  V (lg)$color [V (lg)$status == "infectious"] = "red"
  V (lg)$color [V (lg)$status == "recovered"] = "blue"
  
  return (lg)
}

epidemic <- function (lg, seeds, p, t_e, t_i, t_r) {
  
  V (lg)$status = "susceptible"
  V (lg)$status [seeds] = "infectious"
  V (lg)$time = 0
  k = 1

  
  while (k != 10) {
    
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
    
    k = k+1
    plot (lg, vertex.color = V (color (lg))$color, main = paste ("Day", k))
    
  }  
}

epidemic (G, c (1, 20), 0.4, 1, 1, 1)

