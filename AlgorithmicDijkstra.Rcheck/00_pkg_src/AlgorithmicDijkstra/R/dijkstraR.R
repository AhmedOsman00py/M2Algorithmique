#' Dijkstra's algorithm, finds shortest distance to other nodes
#'
#' @param graph_DataFrame Data.frame object with three variables (from, to and weight)
#' that contains the edges of a directed or undirected graph (from  to) with the weight of the edge (weight).
#' "from" and "to" must be vector of numbers starting from 1,2,3,..(int or str type).
#' @param start_node initial node.
#' @return solution is a list of two elements.
#' solution$distance : is a vector of shortest distance from start_node to others. for example solution$distance[i]
#' is shortest distance from start_node to ith node.
#' solution$path is vector of predecessor to each node. we can identify the spanning tree of the graph.

#' @description
#' The algorithm takes a graph_DataFrame and an initial node and calculates the shortest path from the initial node to every other node in the graph_DataFrame.
#' @examples
#' graph_DataFrame <-
#'  data.frame(from=c(1,6,2,2,2,2,7,7,5,3,3),
#'           to=c(6,1,1,6,5,4,1,5,4,4,2),
#'           weight=c(5,1,3,11,3,10,5,4,4,7,1))
#' dijkstra(graph_DataFrame, 3)
#' @examples
#' graph_DataFrame <-
#'  data.frame(from=c("1","6","2","2","2","2","7","7","5","3","3"),
#'           to=c("6","1","1","6","5","4","1","5","4","4","2"),
#'           weight=c(5,1,3,11,3,10,5,4,4,7,1))
#' dijkstra(graph_DataFrame,"3")
#' @export

dijkstra <- function(graph_DataFrame, start_node){

  #assertions

  if(is.element(F,graph_DataFrame[[3]] >0)==TRUE){
    return(cat("\nWarning! - Edge's weight must be postive to apply Dijkstra"))}

  stopifnot(is.data.frame(graph_DataFrame) && ncol(graph_DataFrame) == 3)
  stopifnot(colnames(graph_DataFrame) == c("from", "to", "weight"))

  stopifnot((is.numeric(graph_DataFrame[[1]]) && is.numeric(graph_DataFrame[[2]]))||(is.character(graph_DataFrame[[1]]) && is.character(graph_DataFrame[[2]])))
  stopifnot((is.numeric(start_node)||is.character(start_node))&& length(start_node) == 1 && is.element(start_node, graph_DataFrame[[1]]))

  # to make it universal, changing graph_DataFrame nodes to numeric

  if(is.character(graph_DataFrame[[1]]) && is.character(graph_DataFrame[[2]])==TRUE){
    graph_DataFrame[ , 1] <- ifelse(is.numeric(graph_DataFrame[ ,1]), graph_DataFrame[ ,1], lapply(graph_DataFrame[1], as.numeric))
    graph_DataFrame[ , 2] <- ifelse(is.numeric(graph_DataFrame[ ,2]), graph_DataFrame[ ,2], lapply(graph_DataFrame[2], as.numeric))
    start_node <- as.numeric(start_node)
  }

  # Vector of nodes to check
  checkNode <- sort(unique(append(graph_DataFrame[ ,1],graph_DataFrame[ ,2])))

  # Vector holds predecessor of each node, o for the start_node and nodes that we ca not
  # visit from start_node

  parentNode <- rep(NA,length(checkNode))

  # Vector which holds distances to initial node

  distanceNode<-  rep(Inf, length(checkNode))
  names(distanceNode) <- checkNode


  distanceNode[start_node] <- 0
  while(length(checkNode) > 0){
    #find node with lowest distance from node to check

    nodeDistanceToCheck <- distanceNode[names(distanceNode) %in% checkNode]
    node <- nodeDistanceToCheck[which.min(nodeDistanceToCheck)]

    # extracting edges to be checked

    nodeEdges <- graph_DataFrame[which(graph_DataFrame[ ,1] == names(node)), ]

    # apply distances to distance vector

    if(nrow(nodeEdges) > 0){ # to avoid wells

      for (i in 1:nrow(nodeEdges)) {
        if(distanceNode[nodeEdges[i, 2]] > node + nodeEdges[i, 3]){
          distanceNode[nodeEdges[i, 2]] <- node + nodeEdges[i, 3]

          parentNode[as.integer(nodeEdges[i,2])] = as.numeric(names(node))

        }
      }
    }

    #removing current node from list of nodes to be checked

    checkNode <- checkNode[ -which(checkNode == names(node))]
  }

  solution <- list()
  solution$distance <- unname(distanceNode)
  solution$path  <- parentNode
  return(solution)
}
