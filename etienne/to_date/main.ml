module Graph = Graph_structure.Graph
module Util = Instances_utils

let () = 
  let filename = Sys.argv.(1) in 
  let filename2 = Sys.argv.(2) in 

  if filename = filename2 then 
    begin 
      Printf.printf "Attention écrasement du fichier initial, fin du process\n";
      exit 0
    end
  else 
    let intersection_graph = Util.build_intersection_graph filename in 
    Util.save_intersection_graph intersection_graph filename2


(* Taille 3000 -> 5s s*)
(* Taille 30000 -> 5min30s*)
(* Taille 73000 -> 22min estimées 
   

  let filename_instance = "../instances/reecn3382.instance.json" in 
  let filename_bin = "../intersection_graphs/reecn3382_ig" in 
  let intersection_graph = Util.build_intersection_graph filename_instance in 
  Util.save_graph filename_bin intersection_graph;
  Printf.printf "graph sauvegardé";
  let intersection_graph = Util.load_graph filename_bin in 
  Graph.print_graph intersection_graph
  
  *)