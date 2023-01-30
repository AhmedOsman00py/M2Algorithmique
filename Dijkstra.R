#' Dijkstra's algorithm, finds shortest distance to other nodes
#' @param graph_DataFrame Data.frame object with three variables (v1, v2 and w) that contains the edges of the graph_DataFrame (from v1 to v2) with the weight of the edge (w)
#' @param start_node initial node (must be a scalar number).
#' @return  solution$distance is a vector of distances from start_node for example solution$distance[i]
#' is distance from start_node to ieme node.
#' @return solution$path is vector of predecessor to each node. we can identify the spanning tree of the graph

#' @description  
#' The algorithm takes a graph_DataFrame and an initial node and calculates the shortest path from the initial node to every other node in the graph_DataFrame.
#' @examples
#'graph_DataFrame <-
#'  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(graph_DataFrame, 1)
#' dijkstra(graph_DataFrame, 3)
#' @export


dijkstra <- function(graph_DataFrame, start_node){
  
  #assertions
  if(is.element(F,graph_DataFrame[[3]] >0)==TRUE){
    return(cat("\nWarning! - Edge's weight must be postive to apply Dijkstra"))}
 
  stopifnot(is.data.frame(graph_DataFrame) && ncol(graph_DataFrame) == 3)
  stopifnot(colnames(graph_DataFrame) == c("v1", "v2", "w"))
  stopifnot((is.numeric(graph_DataFrame[[1]]) && is.numeric(graph_DataFrame[[2]]))||(is.character(graph_DataFrame[[1]]) && is.character(graph_DataFrame[[2]])))
  stopifnot((is.numeric(start_node)||is.character(start_node))&& length(start_node) == 1 && is.element(start_node, graph_DataFrame[[1]]))

  # to make it universal, changing graph_DataFrame nodes to numeric if it is a character
  if(is.character(graph_DataFrame[[1]]) && is.character(graph_DataFrame[[2]])==TRUE){
    graph_DataFrame[ , 1] <- ifelse(is.numeric(graph_DataFrame[ ,1]), graph_DataFrame[ ,1], lapply(graph_DataFrame[1], as.numeric))
    graph_DataFrame[ , 2] <- ifelse(is.numeric(graph_DataFrame[ ,2]), graph_DataFrame[ ,2], lapply(graph_DataFrame[2], as.numeric))
    start_node <- as.numeric(start_node)
  }
 
  # Vector of nodes to check 
  checkNode <- sort(unique(graph_DataFrame[ ,1]))
  
  # Vector holds predecessor of each node, o for the start_node and nodes that we ca not
  # visit from start_node
  
  parentNode <- rep(0,length(checkNode))
  
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
    
    for (i in 1:nrow(nodeEdges)) {
      if(distanceNode[nodeEdges[i, 2]] > node + nodeEdges[i, 3]){
        distanceNode[nodeEdges[i, 2]] <- node + nodeEdges[i, 3]
        
        parentNode[as.integer(nodeEdges[i,2])] = as.numeric(names(node))
        
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


#Test of algo

set.seed(27)
adj_matrix<-matrix(sample(0:5, 25, replace=TRUE, prob=c(0.7,rep(0.3/5,5))), ncol=5)

for (i in c(1:nrow(adj_matrix))) { adj_matrix[i,i]=0}
# plot the graph generated before

g1 <- graph_from_adjacency_matrix( adj_matrix, mode="directed", weighted = T,diag =F)

plot(g1,edge.label=E(g1)$weight)

### prepartion of input, we represent g1 as DataFrame

df1=data.frame(v1=c(1,2,2,2,4,5,3),v2=c(4,4,3,5,3,3,1),w=c(3,1,4,1,2,4,4))

dijkstra(df1,2)

df2=data.frame(v1=c("1","2",2,"2","4","5","3"),v2=c("4","4","3","5","3","3","1"),w=c(3,1,4,1,2,4,4))

dijkstra(df2,"2)

