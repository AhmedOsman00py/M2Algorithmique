library(igraph)
                    #  1  2  3  4  5  6  7
adj_matrix <- matrix(c(0, 0, 0, 0, 0, 5, 0, 
                       3, 0, 0, 1, 3, 1, 0, 
                       0, 1, 0, 7, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 4, 0, 0, 0,
                       1, 0, 0, 0, 0, 0, 0, 
                       5, 0, 0, 0, 4, 0, 0), ncol = 7, byrow = TRUE)

g <- graph.adjacency(adj_matrix, mode = "directed", weighted = TRUE)

coords <- matrix(c(8, 0,  # 4
                   4, 0,  # 2
                   2, 2,  # 1
                   0, 0,  # 7
                   2, -2, # 6
                   6, 2,  # 3
                   6, -2),# 5
                 byrow = TRUE, ncol = 2)

plot(g, 
     edge.label=E(g)$weight, 
     edge.label.cex=1, 
     vertex.size = 25,
     edge.arrow.size = 0.5, 
     layout = coords)

edges <- data.frame(node = c(4, 2, 1, 7, 6, 3, 5), 
                    node.x = coords[, 1], 
                    node.y = coords[, 2])

potential <- function(edges, node_start, end) {
  
  u.x <- edges[edges$node == node_start, ]$node.x
  u.y <- edges[edges$node == node_start, ]$node.y
  
  end.x <- edges[edges$node == end, ]$node.x
  end.y <- edges[edges$node == end, ]$node.y
  
  return(sqrt((u.x - end.x)^2 + (u.y - end.y)^2))
}

node_potential <- data.frame(node = edges$node, distance_from_end = rep(NA, nrow(edges)))
end_node <- 5
for (node_ in edges$node) {
  node_potential[node_potential$node == node_, ]$distance_from_end <- potential(edges, node_, end_node)
}

node_potential_sorted <- node_potential[order(node_potential$node), ]

a_star <- function(graph, edges_coords, heuristic, start, goal) {
  #' Finds the shortest distance between two nodes using the A-star (A*) algorithm
  #' @param graph an adjacency-matrix-representation of the graph where (x,y) is the weight of the edge or 0 if there is no edge.
  #' @param heuristic an estimation of distance from node x to y that is guaranteed to be lower than the actual distance. E.g. straight-line distance
  #' @param start the node to start from.
  #' @param goal the node we're searching for
  #' @return The shortest distance to the goal node. Can be easily modified to return the path.
  
  # This contains the distances from the start node to all other nodes, initialized with a distance of "Infinity"
  distances = rep(Inf, nrow(graph))
  
  # The distance from the start node to itself is of course 0
  distances[start] = 0
  
  # This contains the priorities with which to visit the nodes, calculated using the heuristic.
  priorities = rep(Inf, nrow(graph))
  
  # start node has a priority equal to straight line distance to goal. It will be the first to be expanded.
  priorities[start] = heuristic(edges_coords, start, goal)
  
  # This contains whether a node was already visited
  visited = rep(FALSE, nrow(graph))
  
  # While there are nodes left to visit...
  repeat {
    # ... find the node with the currently lowest priority...
    lowest_priority = Inf
    lowest_priority_index = -1
    for(i in seq_along(priorities)) {
      # ... by going through all nodes that haven't been visited yet
      if(priorities[i] < lowest_priority && !visited[i]){
        lowest_priority = priorities[i]
        lowest_priority_index = i
      }
    }
    if (lowest_priority_index == -1){
      # There was no node not yet visited --> Node not found
      return (-1)
    } else if (lowest_priority_index == goal){
      # Goal node found
      print("Goal node found!")
      return(distances[lowest_priority_index])
    }
    cat("Visiting node ", lowest_priority_index, " with currently lowest priority of ", lowest_priority)
    
    # ...then, for all neighboring nodes that haven't been visited yet....
    for(i in seq_along(graph[lowest_priority_index,])) {
      if(graph[lowest_priority_index,i] != 0 && !visited[i]){
        # ...if the path over this edge is shorter...
        if(distances[lowest_priority_index] + graph[lowest_priority_index,i] < distances[i]){
          # ...save this path as new shortest path
          distances[i] = distances[lowest_priority_index] + graph[lowest_priority_index,i]
          # ...and set the priority with which we should continue with this node
          priorities[i] = distances[i] + heuristic(edges_coords, i, goal)
          cat("Updating distance of node ", i, " to ", distances[i], " and priority to ", priorities[i], "\n")
        }
        # Lastly, note that we are finished with this node.
        visited[lowest_priority_index] = TRUE
        cat("Visited nodes: ", visited, "\n")
        cat("Currently lowest distances: ", distances, "\n")
      }
    }
  }
}


# --- tester l'algorithme
a_star(adj_matrix, edges, potential, 3, 6)
