# vertices <- c("r", "A", "B", "C", "D", "E", "F")

# from = c("r", "r", "C", "C", "C", "C", "D", "E", "F", "F", "B"),
# to   = c("A", "C", "A", "B", "D", "E", "E", "D", "E", "B", "A")

vertices <- 1:7

from_ <- c(3, 3, 2, 2, 2, 2, 6, 1, 7, 7, 5)
to_   <- c(4, 2, 4, 5, 6, 1, 1, 6, 1, 5, 4)  

# --- Input 
edges <- data.frame(from   = from_,
                    to     = to_,
                    weight = c(7, 1, 1, 3, 1, 3, 1, 5, 5, 4, 4)) 


# --- pour le graph
coords <- matrix(c(2, 2,
                   4, 0, 
                   6, 2, 
                   8, 0, 
                   6, -2, 
                   2, -2,
                   0, 0), byrow = TRUE, ncol = 2)


g <- graph_from_data_frame(edges, 
                           directed = TRUE)

plot(g, 
     edge.label=E(g)$weight, 
     edge.label.cex=1, 
     vertex.size = 25, 
     edge.arrow.size = 0.5, 
     layout = coords)


# --- Output 
# Avec la fonction dijkstra(edges, 3), on doit obtenir :
#   distance -> 4 1 0 7 4 9 Inf
#   parents  -> 2 3 NA 3 2 1 NA
