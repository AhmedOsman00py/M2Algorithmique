#include <iostream>
#include <vector>
#include <queue>
#include <limits.h>
using namespace std; 
 
// Définir le nombre de sommets dans le graphe 
#define V 9 
  
// Fonction pour trouver le plus court chemin de source à destinationination 
int dijkstra(int graph[V][V], int source, int destination) 
{ 
    // Tableau pour stocker la distance du sommet à la source  
    int dist[V];  

    // Tableau booléen pour vérifier si un sommet a déjà été visité ou non  
    bool visited[V];  

    // Initialisation des distances et des visites à false  
    for (int i = 0; i < V; i++) { 
        dist[i] = INT_MAX; 
        visited[i] = false; 
    }

    // Distance de la source à elle-même est toujours 0  
    dist[source] = 0;  

    // Trouver le plus court chemin pour tous les sommets  
    for (int count = 0; count < V - 1; count++) {

        // Prendre le sommet avec la distance minimale parmi ceux qui n'ont pas encore été visités.  														     u     v     w     x     y     z      a      b      c      d      e       f       g       h       i       j        k        l         m         n          o          p           q            r            s            t            u            v             w              x               y                z                 a                  b                   c                    d                     e                      f                       g                        h                         i                          j                           k                            l                             m                              n                               o                                p                                  q                                   r                                    s                                     t                                      u                                       v                                        w 

        int min_dist = INT_MAX, min_index;  

        for (int v = 0; v < V; v++) {

            if (visited[v] == false && dist[v] <= min_dist) {

                min_dist = dist[v]; 

                min_index = v; 

            }

        }

        // Marquer le sommet choisi comme visité et inclure sa distance dans la solution finale.  

        visited[min_index] = true;  

        for (int j = 0; j < V; j++) {

            if (!visited[j] && graph[min_index][j] && dist[min_index] != INT_MAX && dist[min_index] + graph[min_index][j] < dist[j]) {

                dist[j] = dist[min_index] + graph[min_index][j]; 

            }

        }

    } 

    return dist[destination]; 
}