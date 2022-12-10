module Graph = GraphStructure.Graph

(* Fonction qui *)
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

(* Fonction qui prend un fichier d'instance et renvoie un objet graphe d'inersection *)
let build_intersection_graph = fun filename ->

  (* Extraction des données brutes depuis le fichier json *)
  let instance_graph = extract_data_from_json_file filename in
  
  (* Construction du graphe d'intersection *)
  let segment_list = Graph.extract_segment instance_graph in 
  
  (* Initialise un graphe vide *)
  let intersection_graph = Graph.create (List.length segment_list) in 
  
  (* Fabrique le graphe d'intersection final *)
  Graph.edit_intersection_graph intersection_graph segment_list; 

  intersection_graph
  

(* Sauvegarder un graphe dans un fichier binaire *)
let save_graph = fun filename graph -> 
  
  let och = open_out filename in
  output_value och graph;
  close_out och;

  Printf.printf "File saved\n"
 
(* Récupérer un graphe sauvegardé dans un fichier binaire *)
let load_graph = fun filename -> 

  let ich = open_in filename in
  let graph = input_value ich in
  close_in ich;

  Printf.printf "File loaded\n";
  graph



