#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
void dijkstraRcpp(NumericMatrix graph, int source, int dest) {
  int D = dest;
  dest = dest-1;
  int V = graph.nrow();
  source = source - 1;

  NumericVector dist(V);


  LogicalVector visited(V);


  for (int i = 0; i < V; i++) {

    dist[i] = INT_MAX;
    visited[i] = false;
  }

  dist[source] = 0;

  for (int count = 0; count < V - 1; count++) {

    int min = INT_MAX, min_index;

    for (int v = 0; v < V; v++)     {

      if (visited[v] == false && dist[v] <= min)         {

        min = dist[v];

        min_index = v;
      }
    }

    visited[min_index] = true;

    for (int v = 0; v < V; v++)      {
      if (!visited[v] && graph(min_index, v) && dist[min_index] != INT_MAX && dist[min_index]+graph(min_index , v) < dist[v])          {
        dist[v] = dist[min_index] + graph(min_index,v);
      }
    }
  }




  if(dest!=source and dist[dest]!=INT_MAX){
    std::vector<int> path ;

    path.push_back(dest);
    while(dest!=source){
      for(int i=0 ; i<V ; i++){
        if(graph(i,dest) !=0 && dist[dest] - graph(i,dest) == dist[i]){
          path.push_back(i);
          dest = i ;
          break;
        }
      }
    }

    Rcpp::Rcout<<"Le plus court chemin entre "<<source + 1<<" et "<< D  <<" est : ";
    while (!path.empty()) {
      Rcpp::Rcout<<path.back() + 1 <<"  ";

      path.pop_back();
    }
    Rcpp::Rcout<< " et distant de : "<<dist[D]<<std::endl;

  }
  if(dist[dest] == INT_MAX){
    Rcpp::Rcout<<"pas de chemin possible entre "<< source + 1 <<" et " << dest ;
  }

  return ;



}
