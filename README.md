# Avancement du projet 

## Validé 

- Ajout des fichiers graphStructure, instanceConverter, color, convert et le script compile.sh (10/12) 
- Ajout des fichiers graph_structure.ml et instances_to_graph.ml (25/11)

## En cours 

- Etude de la Large Neighborhood Search (LNC)
- DSATUR Greedy (approche probabiliste)

## Dernières tâches effectuées 

- Ajout de la méthode DSATUR Branch and Bound 
- Utilisation d'un foncteur de signature Hashtbl
- Lecture des fichiers d'instances .json 
- Passage des graphes aux graphes d'intersections 
- Sauvegarde des graphes d'intersections dans des fichiers binaires

## Historique des modifications du dossier verifie

- Ajout du code de l'algorithme DSATURBNB (10/12) 
- Ajout du code de l'algorithme DSATUR (18/11) 

## Problème à résoudre

On peut convertir des petits graphs d'instances (test.json) en graphes d'intersection (test.g), ensuite les méthodes dsatur et dsaturbnb fonctionnent pour la coloration. Cependant si on fait la même chose avec un gros graphe alors l'execution se fige. La conversion fonctionne mais impossible de charger le graph d'intersectin. Si quelqu'un comprend l'erreur et arrive à la corriger c'est super. 
