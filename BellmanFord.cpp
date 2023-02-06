

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List bellmanFord(NumericMatrix adjacencyMatrix, int source) {

  int n = adjacencyMatrix.nrow(); // Nombre de sommets

  // Initialisation des distances et des prédécesseurs
  NumericVector distance(n);
  IntegerVector predecessor(n);

  for (int i = 0; i < n; i++) {
    distance[i] = R_PosInf; // Distance infinie par défaut
    predecessor[i] = -1; // Pas de prédécesseur par défaut
  }

  distance[source] = 0; // La distance à la source est 0

  // Relaxation des arêtes |V|-1 fois (chaque sommet est visité au moins une fois)
  for (int i = 1; i <= n-1; i++) {
    for (int u = 0; u < n; u++) { // Pour chaque sommet u...
      for (int v = 0; v < n; v++) { // ... et chaque voisin v...

        if (adjacencyMatrix(u,v) > 0) { // ... si l'arête existe...

          double alt = distance[u] + adjacencyMatrix(u,v); // Calculer la distance alternative

          if (alt < distance[v]) { // Si elle est meilleure que la précédente...
            distance[v] = alt;   // ... alors mettre à jour la distance et le prédécesseur.
            predecessor[v] = u;  
          }                      

        }                        

      }                        

    }                        

  }                        

  return List::create(_["distance"]=distance, _["predecessor"]=predecessor);  
}