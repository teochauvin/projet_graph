Mis à jour 21/11 

AJout dossier build_intersection_graphe 
- Première tentative pour composer les avancées d'Etienne et les miennes pour lire un fichier json, le convertir en objet graphe, calculer les intersections des segments, et finalement construire le graphe d'intersection. 

La compilation est la même que précédement, 
> ocamlc -o graph_structure graph_structure.ml 
> ocamlfind ocamlc -package yojson -linkpkg -g -o instance2graph graph_structure.cmo instances_to_graph.ml
> ./instance2graph fichier.json 

Le temps d'execution est de l'ordre de la seconde pour m=4000 
Le code du calcul d'intersection est en O(n²) pour le moment donc pour les graphes les plus gros (m=70000 environ 20x plus de segments) on peut compter un temps d'execution environ 400 fois plus important (maximum 13 min). On voit qu'une méthode en (n+k)log(n) va être nécéssaire pour les plus gros graphes. Il reste à stocker les graphes d'intersection en mémoire.  

Mis à jour 20/11

Dans graphe conversion 
- ajout du code qui converti les graphes en graphes d'intersection en utilisant le typage que Etienne a fait
- Correction du graphe de test 

Mise à jour 19/11 : 

Dans graphe conversion : 
- mise en ligne d'un graphe simple n=6 et m=8 pour tester sur un exemple simple avec une verification possible à la main 
- ajout d'un pdf qui détaille la structure du graphe test 

Module utile : 
- Heap (tas binaire) : réimplémentation on en aura besoin pour implémenter la sweepline 

