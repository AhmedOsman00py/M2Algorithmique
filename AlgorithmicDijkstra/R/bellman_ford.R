#' BellmanFord naif Algorithm to find shortest path from a node to others
#'
#' @param vertices vector contains names of each node. Names can be a int or str.
#' @param edges Data.frame object with three variables (from, to and weight)
#' that contains the edges of the graph_DataFrame (from, to) with the weight of the edge (w)
#' @param start initial node (could be int or str).
#'
#' @return list of two elements.
#' Distance: the shortest distance from the start node to other nodes (vector)
#' parents : the predecessor of each node (vector)
#' @export
#'
#' @examples
#' vertices <- c("r", "a", "b", "c")
#' edges <- data.frame(from   = c("r", "a", "b", "c", "c"),
#'                     to = c("a", "b", "c", "r", "a"),
#'                     weight = c(3, 1, 2, 7, -4))
#' start="r"

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

