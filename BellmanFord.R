# BellmanFord Algorithm
# ---------------------
# Input : 
# - vertices : nom des noeuds (vector)
# - edges : tableau contenant les arcs pondérés (dataframe)
#   - from : arc allant de (vector)
#   - to : arc allant vers (vector)
#   - weight : le poid de l'arc (vector)
# - start : le noeud où on commence (str)

# Output (list): 
#   - distance : la distance minimal du noeud start aux autres noeuds (vector)
#   - parents  : les parents de chaque noeud (vector)


BellmanFord <- function(vertices, edges, start) {
  
  # intialisation
  n <- length(vertices)
  distance <- rep(Inf, n)
  parents <- rep(NA, n)
  
  names(distance) <- vertices
  names(parents) <- vertices
  distance[start] <- 0
  
  # relaxation des arcs
  for (i in 1:(n-1)) {
    for (j in 1:(nrow(edges))) {
      
      u <- edges$from[j]
      v <- edges$to[j]
      w <- edges$weight[j]
      
      if (distance[v] > distance[u] + w) {
        distance[v] <- distance[u] + w
        parents[v] <- u
      }
    }
  }
  
  # vérifications des cercles négatifs
  for (k in 1:(nrow(edges))) {
    
    u <- edges$from[k]
    v <- edges$to[k]
    w <- edges$weight[k]
    
    if (distance[v] > distance[u] + w) {
      # trouver le cercle négatif
      negativeLoop <- c(v, u)
      for (l in 1:(n-1)) {
        u <- negativeLoop[1]
        for (j in 1:(nrow(edges))) {
          
          v <- edges$to[j]
          w <- edges$weight[j]
          if (v != u) {
            if (distance[v] > distance[u] + w) {
              negativeLoop <- c(v, negativeLoop)
            }
          }
        }
      }
      cat("\nWarning! - Graph contains a negative-weight cycle!\nThe shortest path will not be efficient!\n")
    }
  }
  return(list(distance = distance, parents = parents))
}


# --- Test de l'agorithme

vertices <- c("r", "a", "b", "c")
edges <- data.frame(from   = c("r", "a", "b", "c", "c"),
                    to     = c("a", "b", "c", "r", "a"), 
                    weight = c(3, 1, 2, 7, -4))

# Create the graph
g <- graph_from_data_frame(edges, 
                           directed = TRUE)

plot(g, 
     edge.label=E(g)$weight, 
     edge.label.cex=1, 
     vertex.size = 25,
     edge.arrow.size = 0.5)

BellmanFord(vertices, edges, "r")
