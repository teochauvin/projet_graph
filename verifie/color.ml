module Graph = GraphStructure.Graph
module Util = InstanceConverter

(* INFORMATION POUR L'EXECUTION *)
(* ex : ./color reecn3382.intergraph reecn3382.dsatur dsatur *)

(* ============================================================================================= *)
(*                              DEMANDE DE COLORATION DE GRAPHES                                 *)
(* ============================================================================================= *)

(* Code pour colorer un graph avec une méthode X *)
let () = 

  (* Récupère les arguments utiles pour colorier un graphe d'intersection A en un graphe B à l'aide d'une méthode C *)
  let graphe_intersection_path = Sys.argv.(1) in 
  let colored_graph_path = Sys.argv.(2) in 
  let coloring_method = Sys.argv.(3) in 

  (* On charge le graphe vierge en mémoire *)
  
  (* Ca ca fonctionne uniquement si on ne cherche pas à colorier derrière *)
  let intersection_graph = Util.load_graph graphe_intersection_path in  
  let colored = (Graph.dsaturbnb intersection_graph) in

  Graph.show_coloration colored



  


