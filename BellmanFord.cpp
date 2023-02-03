#include <iostream>
#include <vector>
#include <limits.h>
 
using namespace std;
 
// Définition de la structure pour les arcs du graphe 
struct Edge { 
    int source, destination;
    int  weight; 
}; 
  
// Fonction pour trouver le plus court chemin entre le sommet source et tous les autres sommets 
void BellmanFord(vector<Edge> edges, int n, int source) { 
    // Initialisation des distances à partir du sommet source à tous les autres sommets à l'infini sauf pour le sommet source qui est initialisé à 0.0  
    vector<double> distance(n, INT_MAX); 
    distance[source] = 0.0; 
    // Répéter n-1 fois où n est le nombre de sommets  
    for (int i = 1; i <= n - 1; i++) { 
        // Parcourir tous les arcs du graphe  
        for (auto j : edges) { 
            // Si la distance du sommet destination est plus grande que la somme de la distance du sommet source et du poids de l'arc alors mettre à jour la distance du sommet destination  
            if (distance[j.destination] > distance[j.source] + j.weight) {

                distance[j.destination] = distance[j.source] + j.weight; 
            }
        }
    } 
    // Vérifier si il y a un cycle négatif dans le graphe  
    for (auto j : edges) { 
        if (distance[j.destination] > distance[j.source] + j.weight) {
            cout << "Le graphe contient un cycle négatif" << endl; return;  
        }
    }  
    cout << "Les distances les plus courtes depuis le noeud " << source << endl;  
    for (int i = 0; i < n; i++) { cout << i << " - " << distance[i] << endl; }  
}

//int main(){
//    int n = 9;
//    Edge edge;
//    edge.source[n]{1,2,3,4,5,6,7,8,9};
//    edge.destination[n]{3,2,5,4,1,7,9,8};
//    edge.weight[n]{7,1,2,1,3,4,5,1,2};
//    int source = 1;
//    bellman = BellmanFord(edge, n, source);
//    bellman;
//    return 0;
// 
//}