#include <iostream>
#include <vector>
#include <climits>
 
using namespace std;
 
// Nombre de sommets dans le graphe
#define V 
 
// Fonction pour trouver le sommet avec la distance minimale, à partir duquel on peut encore atteindre un autre sommet
int minDistance(int dist[], bool visited[])
{
    // Initialisation de la distance minimale à INT_MAX qui est le plus grand entier possible.  
    int min = INT_MAX, min_index;

    for (int v = 0; v < V; v++) {

        if (visited[v] == false && dist[v] <= min) {

            min = dist[v], min_index = v;

        }

    }

    return min_index;
}

 
// Affiche la solution  
void printSolution(int dist[]) {

    cout << "Distance des sommets du graphe \n";

    for (int i = 0; i < V; i++) {

        cout << i << " \t\t " << dist[i] << endl;

    }    
}

 
// Algorithme de Dijkstra pour trouver la distance minimale connaissant la matrice d'adjacence.  
void dijkstra(int graph[V][V], int src) {     // src est le sommet source.  

    int dist[V];      // Tableau pour stocker les distances des sommets par rapport au sommet source.  

                       // visited[] est un tableau booléen qui indique si un sommet a déjà été visité ou non.    

    bool visited[V];   // Initialisation de toutes les distances à l'infini et tous les sommets non visités.  

    for (int i = 0; i < V; i++) {         // On initialise toutes les distances à l'infini et tous les sommets non visités.  

        dist[i] = INT_MAX, visited[i] = false;     }      // La distance du sommet source à elle-même est toujours nulle.    

    dist[src] = 0;      // Trouver le chemin le plus court pour chaque sommet      
    for (int count = 0; count < V - 1; count++) {          
        // Trouver le sommet avec la distance minimale, à partir duquel on peut encore atteindre un autre sommet          
        int u = minDistance(dist, visited);          
         // Marquer le noeud comme visité          
        visited[u] = true;          
        // Mettre à jour la distance des voisins du noeud choisi          
        for (int v = 0; v < V ; v++)              
            if (!visited[v] && graph[u][v] && dist[u] != INT_MAX && dist[u]+graph [u][v] < dist [v])                  
                dist [v]=dist [u]+graph [u][v];      }      
            printSolution(dist); }      

int main() {
    V
    int graph [V][V]={{4  , 0  ,  0  ,  0  ,  0  ,    5  , 0},
                      {3  , 1  ,  0  ,  10 ,  3  ,   11  , 0},
                      {0  , 1  ,  0  ,  7  ,  0  ,   0   , 0},
                      {0  , 0  ,  0  ,  7  ,   0 ,    0  , 0},
                      {0  , 0  ,  0  ,  4  ,  4  ,    0  , 0},
                      {1  , 0  ,  0  ,  0  ,  0  ,    9  , 0},
                      {5  , 0  ,  0  ,  0  ,  4  ,    0  , INT_MAX}
                 };        
    dijkstra(graph, 2);      
    return 0 ;     }