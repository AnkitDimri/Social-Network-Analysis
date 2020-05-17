library (igraph)

G = read_graph ("karate.gml", format = "gml")

color <- function (lg) {
  V (lg)$color [V (lg)$status == "susceptible"] = "green"
  V (lg)$color [V (lg)$status == "exposed"] = "yellow"
  V (lg)$color [V (lg)$status == "infectious"] = "red"
  V (lg)$color [V (lg)$status == "recovered"] = "blue"
}


