(* Methode naive pour creer le graph d'intersection *)

(* fonctions annexes *)
exception Empty 

let rec compress = fun l -> 
  match l with 
    a :: (b :: _ as t) -> if a = b then compress t else a :: compress t 
  | smaller -> smaller 

let rec get = fun l i ->
  match l with 
  | [] -> raise Empty
  | h::t -> if i=0 then h else get t (i-1)

(* pop le premier éléments d'une liste sous une condition cond *) 
let pop_cond = fun l cond -> 
  match l with
    [] -> l
  | h :: t -> if cond h then t else l

(* représentation d'un Graph *) 
module Graph = struct
  (* à chaque noeud est associe des caractéristique rassemblées ici dans un record *) 
  type nodes_characteristics = {adj : int list ; color : int ; degree : int ; dsat : int}

  (* Construire les segments *)
  type segment = {
    id_s: int; 
    x1: int; 
    y1:int; 
    x2: int; 
    y2: int}

  type graph = {
    id: string; 
    n: int; 
    m: int; 
    x: int list; 
    y: int list; 
    edge_i: int list; 
    edge_j: int list}
  
  (* Exceptions *) 
  exception Empty_graph of string 
  exception Empty_list of string 
  exception Error 
  
  (* Fonction communes au module Hashtbl : justifie le besoin d'un foncteur *) 
  let create = fun n -> Hashtbl.create n 

  let add = fun graph (id : int) (characteristics : nodes_characteristics) -> Hashtbl.add graph id characteristics

  let find = fun graph id -> Hashtbl.find graph id
  
  (* Représentation d'un graph sous format texte console *) 
  let print_graph = fun graph ->
    Hashtbl.iter (
      fun id characteristics -> 
        Printf.printf "\nid : %d, color : %d, degree : %d, dsat : %d et adj : " id characteristics.color characteristics.degree characteristics.dsat; 
        List.iter (Printf.printf "%d|") characteristics.adj) graph;
    Printf.printf("\n\n\n")

  (* Recupere la couleur d'un noeud dans un graph *) 
  let get_color = fun graph id -> 
    let node_char = find graph id in 
    node_char.color
  
  (* Verifie qu'un noeud est coloré dans un graphe *) 
  let is_colored = fun graph id -> get_color graph id <> 0
  
  (* Récupère le degré d'un noeud dans un graphe *) 
  let get_degree = fun graph id -> 
    let node_char = find graph id in  
    List.length node_char.adj
   
  (* Récupère le degré de saturation d'un noeud dans un graphe *)
  (* Degré de saturation = taille de la liste des couleurs différentes non nulles, sans redondances *) 
  let get_saturation_degree = fun graph id -> 
    let node_char = find graph id in 
    let adj = node_char.adj in 
    
    let rec aux = fun nodes_to_visit acc ->
      match nodes_to_visit with 
        [] -> List.length (compress acc)
      | id :: t -> 
          let color = get_color graph id in 
          aux t (if color <> 0 then color :: acc else acc) in
    aux adj []
  
  (* Change le degré de saturation d'un noeud dans un graphe *) 
  let change_dsat = fun graph id ->
    let node_char = find graph id in 
    let new_dsat = get_saturation_degree graph id in 
    let new_node_char = {adj = node_char.adj ; color = node_char.color ; degree = node_char.degree ; dsat = new_dsat} in 
    Hashtbl.replace graph id new_node_char
   
  (* Change la couleur d'un noeud dans un graphe *) 
  (* Met à jour le degré de saturation de tous les noeuds adjacents *) 
  let change_color = fun graph id new_color ->
    let node_char = find graph id in 
    let new_node_char = {adj = node_char.adj ; color = new_color ; degree = node_char.degree ; dsat = node_char.dsat} in 
    Hashtbl.replace graph id new_node_char;
    List.iter (fun id -> change_dsat graph id) new_node_char.adj
  
  (* Trouve la couleur la plus petite disponible sur un noeud *) 
  let lowest_color_available = fun graph id ->
    let node_char = find graph id in 
    let adjs_colors = List.map (fun id -> get_color graph id) node_char.adj in 
    let csorted_adjs_colors = pop_cond (List.sort (fun color1 color2 -> color1 - color2) (compress adjs_colors)) (fun x -> x = 0) in

    let rec aux = fun l av_color -> 
      match l with 
        [] -> av_color 
      | color :: t -> if color = av_color then aux t (av_color + 1) else av_color in 

    aux csorted_adjs_colors 1
  
  (* Teste si le graphe est entièrement coloré *) 
  let is_all_colored = fun graph ->
    let nodes_list = Hashtbl.fold (fun id _ -> List.cons id) graph [] in
    
    let rec aux = fun l -> 
      match l with 
        [] -> true 
      | id :: t -> if is_colored graph id then aux t else false in 
    aux nodes_list
  
  (* Fonction principale qui execute l'algo DSATUR *) 
  let color_with_DSATUR = fun graph ->
  
    (* Trier les noeuds par ordre de degré décroissants *) 
    let raw_deg_Vlist = Hashtbl.fold (fun id _ -> 
      let node_char = find graph id in List.cons (id, node_char.degree, node_char.dsat)
      ) graph [] in
    
    let deg_sorted_Vlist = List.sort (fun (id1, degree1, _) (id2, degree2, _) -> degree2-degree1) raw_deg_Vlist in 
    
    (* Colorer le noeud de degré maximal (premier élément de la liste) avec 1 *) 
    match deg_sorted_Vlist with 
      [] -> raise (Empty_graph "Graphe vide")
    | (id, _, _) :: _ -> change_color graph id 1 ;
  
  (* Boucle while(graphe pas coloré entièrement) *) 
  let rec loop = fun () ->
    
    (* Trier les noeuds par degré de saturaton croissant, en cas d'égalité trier par degré croissant *) 
    let raw_dsat_Vlist = Hashtbl.fold (fun id _ -> 
      let node_char = find graph id in List.cons (id, node_char.degree, node_char.dsat)
      ) graph [] in

    let dsat_sorted_Vlist = List.sort (
      fun (id1, degree1, dsat1) (id2, degree2, dsat2) -> if dsat2-dsat1 = 0 then degree2-degree1 else dsat2-dsat1
      ) raw_dsat_Vlist in 
     
    (* Colorier le noeud le premier noeud de cette liste qui n'est pas encore colorié *) 
    let rec aux = fun l -> 
      match l with 
        [] -> raise (Empty_list "List vide")
      | (id, _, _) :: t -> if is_colored graph id then aux t else id in 
    
    let id_chosen_vertex = aux dsat_sorted_Vlist in
    
    (* Colorier ce noeud avec la couleur la plus petite possible *) 
    change_color graph id_chosen_vertex (lowest_color_available graph id_chosen_vertex);
     
    (* Verifier si le graphe est entièrement colorié *) 
    if is_all_colored graph then graph else loop () in 
  
  loop ()

  let build_segment = fun (graph_b:graph) ->
    let x = graph_b.x in 
    let y = graph_b.y in 
  
    let rec aux = fun e_i e_j s i ->
      match (e_i, e_j) with 
      | ([], []) -> s 
      | (hi::ti, hj::tj) -> aux ti tj ({id_s=i; x1=get x hi; y1= get y hi; x2=get x hj; y2= get y hj}::s) (i+1) 
      | (_, _) -> raise Error in 
  
    aux graph_b.edge_i graph_b.edge_j [] 1 

  let print_list_segment = fun list_seg -> 
    List.iter (fun s -> Printf.printf "id %d : e1 (%d, %d) | e2 (%d, %d)\n" s.id_s s.x1 s.y1 s.x2 s.y2) list_seg
  
  (* Fonctions pour l'intersection *)
  let ccw = fun (x1,y1) (x2,y2) (x3,y3) -> 
    let t = (y3 - y1)*(x2 - x1) - (y2-y1)*(x3-x1) in
    if t < 0 then -1 else 1

  let is_alligned = fun (x1,y1) (x2,y2) (x3,y3) -> (x2-x1)*(y3-y2) - (y2-y1)*(x3-x2) = 0
  
  (* Ignoble *)
  let intersect = fun s1 s2 ->
  
    match (s1,s2) with
    | ({id_s = id1; x1 = x1; y1 = y1; x2 = x2; y2 = y2}, {id_s = id2; x1 = x3; y1 = y3; x2 = x4; y2 = y4}) -> 
      let a = (x1,y1) in 
      let b = (x2,y2) in 
      let c = (x3,y3) in 
      let d = (x4,y4) in
      
      (* Amusez vous bien à comprendre ca *)
      if a <> c && a <> d && b <> c && b <> d then 
        if ccw c d a <> ccw c d b && ccw a b c <> ccw a b d then 
          if is_alligned a b c || is_alligned a b d || is_alligned c d a || is_alligned c d b then false 
          else true 
        else false 
      else false 
  
  (* Construire la liste des segments adjacents *)
  let adj = fun s l ->
    let rec aux = fun s l acc ->
      match l with 
      | [] -> acc 
      | h::t -> if h.id_s <> s.id_s && intersect s h then aux s t (h::acc) else aux s t acc in
    aux s l [] 
  
  (* ajouter dans le graph *)
  let format_graph = fun graph s s_adj -> 
    let id = s.id_s in 
  
    let rec aux = fun l acc -> 
      match l with 
      | [] -> acc
      | h::t -> aux t (h.id_s::acc) in 
  
    let adj_int = aux s_adj [] in 
    let node_chara = {adj = adj_int; color = 0; degree = List.length adj_int; dsat = 0} in 
  
    add graph id node_chara 
  
  (* Prend une liste de segments et construit un graphe d'intersection *)
  let build_from_segments = fun graph s_list -> 
  
    let rec aux = fun l ->
      match l with 
      | [] -> ()
      | s::t -> format_graph graph s (adj s s_list); aux t in 
    aux s_list;
  
    print_graph graph

end


(* MAIN *)
let () = 

  (* Format attendu après traitement du .json un type graphe comme ecrit ci dessous *) 
  
  (* construction des segments + affichage *)
  let list_seg = Graph.build_segment {id="test";n=6; m=8; x=[2;3;4;5;3;1]; y=[3;4;3;2;1;2]; edge_i=[0;1;0;3;2;1;0;1]; edge_j=[2;5;4;5;4;3;3;4]} in 
  
  Graph.print_list_segment list_seg; 
  Printf.printf "\n Graph d'intersection : \n";
  
  let graph = Graph.create (List.length list_seg) in 
  Graph.build_from_segments graph list_seg