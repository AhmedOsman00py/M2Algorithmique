#' BellmanFord's algorithm (naive solution)
#'
#' @description
#' Finds the minimal distance from the starting node to the other nodes.
#'
#' The algorithm works by iterating over all the edges of the graph several times. During each iteration, the distances of the vertices are updated using the distances of the neighbouring vertices. More precisely, for each edge (u, v) of the graph, the distance of "v" is updated if it is shorter than the distance of "u" plus the weight of the edge (u, v).
#'
#' @param vertices name of the nodes (vector (int or str))
#' @param edges Data.frame object containing weighted edges with three variables (from, to and weight)
#'      from   : outgoing edge
#'      to     : incoming edge
#'      weight : the weight of the edge
#' @param start starting node (int or str (depends on the vertice variable type)).
#'
#' @return list of two elements.
#' distance : the minimum distance from the start node to the other nodes (vector)
#' parents  : the predecessor of each node (vector)
#' @export
#'
#' @examples
#' vertices <- c("r", "a", "b", "c")
#'
#' edges <- data.frame(from   = c("r", "a", "b", "c", "c"),
#'                     to     = c("a", "b", "c", "r", "a"),
#'                     weight = c(3, 1, 2, 7, -4))
#'
#' start <- "r"
#'
#' BellmanFord(vertices, edges, start)
#'

BellmanFord <- function(vertices, edges, start) {

  # intialization
  n <- length(vertices)
  distance <- rep(Inf, n)
  parents <- rep(NA, n)

  names(distance) <- vertices
  names(parents) <- vertices
  distance[start] <- 0

  # relaxation of the edges
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

  # checks for negative circles
  for (k in 1:(nrow(edges))) {

    u <- edges$from[k]
    v <- edges$to[k]
    w <- edges$weight[k]

    if (distance[v] > distance[u] + w) {
      # find the negative circle
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
