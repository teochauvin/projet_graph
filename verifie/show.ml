module Graph = GraphStructure.Graph
module Util = InstanceConverter


(* ============================================================================================= *)
(*                              DEMANDE D'AFFICHAGE DE GRAPHES                                 *)
(* ============================================================================================= *)

(* Code pour afficher un graph avec une méthode X *)
let () = 

  (* Récupère les arguments utiles pour colorier un graphe d'intersection A en un graphe B à l'aide d'une méthode C *)
  let colored_graph_path = Sys.argv.(1) in  

  (* On charge le graphe coloré en mémoire *)
  let colored_graph = Util.load_graph colored_graph_path in  

  Graph.show_coloration colored_graph;

  print_string "\nNombre de couleurs utilisées : ";
  print_int (Graph.chromatic_number colored_graph);
  print_string "\n\n"