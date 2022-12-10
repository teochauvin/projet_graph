module Graph = GraphStructure.Graph
module Util = InstanceConverter

(* INFORMATION POUR L'EXECUTION *)
(* ./convert reecn3382.instance.json reecn3382.intergraph *)

(* ESTIMATION DU TEMPS DE CONVERSION *)
(* Taille 3000 -> 5s s*)
(* Taille 30000 -> 5min30s*)
(* Taille 73000 -> 22min estimées *) 

(* ============================================================================================= *)
(*                             CONVERTION DES GRAPHES D'INSTANCES                                *)
(* ============================================================================================= *)

let () = 

  (* Récupère le chemin d'accès au graphe d'instance et le chemin désiré pour sauvegader le graphe d'intersection*)
  let graphe_instance_path = Sys.argv.(1) in 
  let graphe_intersection_path = Sys.argv.(2) in 

  (* Test si il y a collision dans les chemins d'accès *)
  if graphe_instance_path = graphe_intersection_path then 
    begin 
      Printf.printf "Attention écrasement du fichier initial, fin du process\n";
      exit 0
    end
  
  (* Si aucun problème alors on peut fabriquer le graph d'intersection, et ensuite le sauvegarder *)
  else 
    let intersection_graph = Util.build_intersection_graph graphe_instance_path in
    Util.save_graph graphe_intersection_path intersection_graph



