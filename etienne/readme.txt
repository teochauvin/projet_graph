## Mise à jour du 07/12/22:

- fichier graph_structure.ml
- instances_utils.ml
- main.ml
- compile.sh (commandes de compilations des trois fichier ci-dessus, à exécuter avec : source compile.sh ou votre commande pref)

## Mise à jour du 19/11/22:

Concernant le fichier graph_structure.ml :

- Ajout des coordonnées (x, y) = (int, int) du noeud dans le type nodes_characteristics
- Ajout de la fonction add_adj dans le module Graph.
	val add_adj : graph (Hashtbl) -> int -> int -> unit 
  La fonction prend en paramètre deux identifiants de noeuds ainsi que le graphe. Elle permet de mettre à jour les caractéristiques des noeuds correspondants aux identifiants (id1, id2) passés en paramètres :
  Elle ajoute id2 dans la liste des noeuds adjacents à id1 et elle incrémente le degré du noeud id1. 
  Idem pour id2.

Création du fichier instance_to_graph.ml qui permet la lecture des fichiers d’instances dans le format json. Le fichier utilise le module Yojson pour le traitement des données. Les instructions concernant sa compilation et son exécution sont les suivantes :

Pour la compilation :
- installer le module Yojson avec opam avec la commande
    opam install Yojson
- compiler le fichier de définition de structure des graphes "graph_structure.ml" avec
	  ocamlc -c graph_structure.ml
- compiler le fichier instance_to_graph.ml avec la commande ci-dessous :
    ocamlfind ocamlc -package yojson -linkpkg -g -o instance2graph graph_structure.cmo instance_to_graph.ml
- à l'exécution de instance2graph on donne au fichier le nom du fichier json à lire, exemple :
    ./instance2graph vispecn2518.instance.json (préciser le chemin du fichier json)
