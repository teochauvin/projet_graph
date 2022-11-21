(* Pour compiler le .ml de lecture de fichier :
- installer le module Yojson avec opam 
- compiler le fichier de définition de structure des graphes "graph_structure.ml"
- compiler le fichier instance_to_graph avec la commande ci-dessous :
  ocamlfind ocamlc -package yojson -linkpkg -g -o instance2graph graph_structure.cmo instance_to_graph.ml
- à l'exécution de instance2graph on donne au fichier le nom du fichier json à lire
  exemple : ./instance2graph vispecn2518.instance.json (préciser le chemin du fichier json)
*)

(* import du module Graph pour l'utilisation de ses propriétés *)
module Graph = Graph_structure.Graph

(* type raw_graph pour stocker les données contenues dans les fichiers json avant de créer un graphe *)
(* type raw_graph = {id : string ; n : int ; m : int ; x : int list ; y : int list ; edge_i : int list ; edge_j : int list} *) 

(* val read_json : string -> string *)
(* Renvoie une chaine de caractères contenant l'ensemble d'un fichier json d'instance dont le nom est passé en paramètre *)
let read_json = fun filename -> 
  let chan = open_in filename in 

  let rec aux = fun file_content -> 
    let current_line = input_line chan in 
    let new_content = file_content ^ current_line in
    try
      aux new_content
    with End_of_file -> 
      new_content in 
  aux ""

(* val extract_data_from_json_string : string -> raw_graph *)
(* Prend en paramètre une chaine de caractère au format json et renvoie les données de l'instance dans un type raw_graph *)
let extract_data_from_json_string = fun json_string ->
  (* val json : Yojson.Basic.t *)
  (* Convertit la chaine de caractère dans un objet json du module Yojson *)
  let json = Yojson.Basic.from_string json_string in 
  
  (* Extraction des données de l'objet json à l'aide de fonctions de la librairie *)
  let id = json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string in
  let n = json |> Yojson.Basic.Util.member "n" |> Yojson.Basic.Util.to_int in 
  let m = json |> Yojson.Basic.Util.member "m" |> Yojson.Basic.Util.to_int in 
  let xs = json |> Yojson.Basic.Util.member "x" |> Yojson.Basic.Util.to_list |> List.map (Yojson.Basic.Util.to_int) in 
  let ys = json |> Yojson.Basic.Util.member "y" |> Yojson.Basic.Util.to_list |> List.map (Yojson.Basic.Util.to_int) in 
  let edges_i = json |> Yojson.Basic.Util.member "edge_i" |> Yojson.Basic.Util.to_list |> List.map (Yojson.Basic.Util.to_int) in 
  let edges_j = json |> Yojson.Basic.Util.member "edge_j" |> Yojson.Basic.Util.to_list |> List.map (Yojson.Basic.Util.to_int) in

  Graph.build_graph_record id n m xs ys edges_i edges_j 

let build_intersection_graph = fun filename ->

  let filename = Sys.argv.(1) in

  (* Lecture du fichier *)
  let instance = read_json filename in 

  (* Extraction des données brutes *)
  let raw_graph = extract_data_from_json_string instance in
  
  (* Construction du graphe d'intersection *)
  let list_seg = Graph.build_segment raw_graph in 
  
  (* Initialise un graphe vide *)
  let graph = Graph.create (List.length list_seg) in 
  Printf.printf "\n Graph d'intersection : \n";
  Graph.build_from_segments graph list_seg


let () =
  (* Exemple d'utilisation du main *)
  (* Ici on récupère le nom de l'instance en paramètre de l'exécution *)
  let filename = Sys.argv.(1) in

  (* Construction du graphe d'intersection *)
  build_intersection_graph filename;