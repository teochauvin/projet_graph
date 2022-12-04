(* Pour compiler le .ml de lecture de fichier :
- installer le module Yojson avec opam 
- compiler le fichier de définition de structure des graphes "graph_structure.ml"
- compiler le fichier instance_to_graph avec la commande ci-dessous :
  ocamlfind ocamlc -package yojson -linkpkg -g -o instances2graph graph_structure.cmo instances_to_graph.ml
- à l'exécution de instance2graph on donne au fichier le nom du fichier json à lire
  exemple : ./instance2graph vispecn2518.instance.json (préciser le chemin du fichier json)
*)

(* import du module Graph pour l'utilisation de ses propriétés *)
module Graph = Graph_structure.Graph

open Graph

(* val extract_data_from_json_string : string -> raw_graph *)
(* Prend en paramètre une chaine de caractère au format json et renvoie les données de l'instance dans un type raw_graph *)
let extract_data_from_json_file = fun filename ->
  (* val json : Yojson.Basic.t *)
  (* Création d'un objet json du module Yojson directement depuis le nom du fichier passé en paramètre*)
  let json = Yojson.Basic.from_file filename in 
  
  (* Extraction des données de l'objet json à l'aide de fonctions de la librairie *)
  let id = json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string in
  let n = json |> Yojson.Basic.Util.member "n" |> Yojson.Basic.Util.to_int in 
  let m = json |> Yojson.Basic.Util.member "m" |> Yojson.Basic.Util.to_int in 
  let xs = json |> Yojson.Basic.Util.member "x" |> Yojson.Basic.Util.to_list |> List.map (Yojson.Basic.Util.to_int) in 
  let ys = json |> Yojson.Basic.Util.member "y" |> Yojson.Basic.Util.to_list |> List.map (Yojson.Basic.Util.to_int) in 
  let edges_i = json |> Yojson.Basic.Util.member "edge_i" |> Yojson.Basic.Util.to_list |> List.map (Yojson.Basic.Util.to_int) in 
  let edges_j = json |> Yojson.Basic.Util.member "edge_j" |> Yojson.Basic.Util.to_list |> List.map (Yojson.Basic.Util.to_int) in

  Graph.build_raw_graph_record id n m xs ys edges_i edges_j 

let build_intersection_graph = fun filename ->
  (* Extraction des données brutes depuis le fichier json *)
  let raw_graph = extract_data_from_json_file filename in
  
  (* Construction du graphe d'intersection *)
  let list_seg = Graph.build_segment raw_graph in 
  
  (* Initialise un graphe vide *)
  let graph = Graph.create (List.length list_seg) in 
  
  let intersection_graph = Graph.build_from_segments graph list_seg in
  
  intersection_graph


let () =
  (* Exemple d'utilisation du main *)
  (* Ici on récupère le nom de l'instance en paramètre de l'exécution *)
  let filename = Sys.argv.(1) in

  (* Construction du graphe d'intersection *)
  let intersection_graph = build_intersection_graph filename in 

  Printf.printf "Graphe d'intersection :";
  ignore (Graph.print_graph intersection_graph)
