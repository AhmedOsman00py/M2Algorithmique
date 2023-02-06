
library(igraph)
library(dplyr)
library(ggplot2)

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

simulation.complexity <- matrix(rep(NA,10*30),ncol = 10)

for (s in c(1:10)) {
  
  Taken_time=rep(NA,30)
  
  for (v in c(1:30)*10) {
    
    # Data prepartion
    df<-as_data_frame(sample_gnp(v,0.6),what = "edges")
    df["weight"]<-sample(c(1:14),size =length(df$to),replace = T)
    start_node<- sample(df$from,size =1)
    
    # Dijkstra algorithm
    
    start.time <- Sys.time()
    dijkstra(df,start_node)
    end.time <- Sys.time()
    
    Taken_time[v/10] <- end.time - start.time
    
  }
  
  simulation.complexity[,s] <-Taken_time
}

mydata=as.data.frame(simulation.complexity)
mydata_simulation <- data.frame(log_vertex=log(c(1:30)*10),log_time=log(rowMeans(mydata)))
lm_dijkstra=lm(log_time~ log_vertex,data=mydata_simulation)

ggplot(data=mydata_simulation, aes(x=log_vertex, y=log_time)) + geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) + geom_point(shape=17, fill="black", color="darkred", size=3)+
   stat_smooth(method = "lm",se=F) + theme_minimal()

