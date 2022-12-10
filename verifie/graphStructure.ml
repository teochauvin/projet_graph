(* ============================================================================================= *)
(*                                       INFORMATIONS                                            *)
(*                                       MODULE GRAPH                                            *)
(*            COLORATION:                                 CONVERSION INSTANCE-INTERSECTION       *)
(*              DSATUR                                                                           *)
(*      DSATUR BRANCH AND BOUND                                                                  *)
(*                                                                                               *)
(*                                                                                               *)
(* ============================================================================================= *)







(* Signature du module Hashtbl*)
module type Hash = sig
  type ('a, 'b) t
  val create : ?random:bool -> int -> ('a, 'b) t
  val clear : ('a, 'b) t -> unit
  val reset : ('a, 'b) t -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val find : ('a, 'b) t -> 'a -> 'b
  val find_opt : ('a, 'b) t -> 'a -> 'b option
  val find_all : ('a, 'b) t -> 'a -> 'b list
  val mem : ('a, 'b) t -> 'a -> bool
  val remove : ('a, 'b) t -> 'a -> unit
  val replace : ('a, 'b) t -> 'a -> 'b -> unit
  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  val filter_map_inplace : ('a -> 'b -> 'b option) -> ('a, 'b) t -> unit
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  val length : ('a, 'b) t -> int
  val randomize : unit -> unit
  val is_randomized : unit -> bool
  (* val rebuild : ?random:bool -> ('a, 'b) t -> ('a, 'b) t *)
  type statistics = {
    num_bindings : int;
    num_buckets : int;
    max_bucket_length : int;
    bucket_histogram : int array;
  }
  val stats : ('a, 'b) t -> statistics
  val to_seq : ('a, 'b) t -> ('a * 'b) Seq.t
  val to_seq_keys : ('a, 'b) t -> 'a Seq.t
  val to_seq_values : ('a, 'b) t -> 'b Seq.t
  val add_seq : ('a, 'b) t -> ('a * 'b) Seq.t -> unit
  val replace_seq : ('a, 'b) t -> ('a * 'b) Seq.t -> unit
  val of_seq : ('a * 'b) Seq.t -> ('a, 'b) t
  module type HashedType =sig 
    type t val equal : t -> t -> bool 
    val hash : t -> int 
  end
  module type S =
    sig
      type key
      type 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val length : 'a t -> int
      val stats : 'a t -> statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
    end
  
  module type SeededHashedType =sig 
    type t 
    val equal : t -> t -> bool 
    val hash : int -> t -> int 
  end
  module type SeededS =sig
      type key
      type 'a t
      val create : ?random:bool -> int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val length : 'a t -> int
      val stats : 'a t -> statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
  end
  
  val hash : 'a -> int
  val seeded_hash : int -> 'a -> int
  val hash_param : int -> int -> 'a -> int
  val seeded_hash_param : int -> int -> int -> 'a -> int
end
                                      
(*foncteur, définition du module graphe en retour, tout ajout au module graph se fait à l'interieur de la defnition du foncteur  *)
module Make=functor (Dom: Hash)-> struct

  (* ============================================================================================= *)
  (*                                  TYPE DEFS & EXCEPTIONS                                       *)
  (* ============================================================================================= *)

  (* à chaque noeud est associe des caractéristique rassemblées ici dans un record *)   
  type nodes_characteristics = {adj : int list ; color : int ; degree : int ; dsat : int}
    
  (* Construire les segments *)
  type segment = {
    id_s: int; 
    x1: int; 
    y1:int; 
    x2: int; 
    y2: int}

  (* Le type des graphes d'instances *)
  type instance_graph = {
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
  exception Empty
  exception Error 




  (* ============================================================================================= *)
  (*                                      FONCTIONS UTILES                                         *)
  (* ============================================================================================= *)

  (* convertion de option 'a into 'a *)
  let get_value_from_option = fun option_value default_value ->
    match option_value with 
    | Some x -> x 
    | None -> default_value

  (* Elimine les doublons d'une liste quelconque d'entier *)
  let compress = fun l -> 
    let sorted_l = List.sort (fun color1 color2 -> color1 - color2) l in 

    let rec aux = fun sl ->
      match sl with 
        a :: (b :: _ as t) -> if a = b then aux t else a :: aux t 
      | smaller -> smaller in

    aux sorted_l 
    
  (* Récupère l'élément d'indice i d'une liste l*)
  let rec get = fun l i ->
    match l with 
    | [] -> raise Empty
    | h::t -> if i=0 then h else get t (i-1)

  (* pop le premier éléments d'une liste sous une condition cond *) 
  let pop_cond = fun l cond -> 
    match l with
      [] -> l
    | h :: t -> if cond h then t else l

  (* Renvoie les informations dans un record de type 'raw_graph' *)
  let build_raw_graph_record = fun id n m xs ys edges_i edges_j ->
    {id = id ; n = n ; m = m ; x = xs ; y = ys ; edge_i = edges_i ; edge_j = edges_j}

  (* Fonction communes au module d'entrée du foncteur - ici Hashtbl *) 
  let create =Dom.create 

  let add = Dom.add 

  let find = Dom.find

  let length = Dom.length









  (* ============================================================================================= *)
  (*                                           PRINTS                                              *)
  (* ============================================================================================= *)

  (* Représentation d'un graph sous format texte console *) 
  let print_graph = fun graph ->
    Dom.iter (
      fun id characteristics -> 
        Printf.printf "\nid : %d, color : %d, degree : %d, dsat : %d et adj : " id characteristics.color characteristics.degree characteristics.dsat; 
        List.iter (Printf.printf "%d|") characteristics.adj) graph;
    Printf.printf("\n\n\n")

  let show_coloration = fun graph ->
    Dom.iter (fun id characteristics -> Printf.printf "\nid : %d, color %d" id characteristics.color) graph

  (* Représente la liste de segments sous format texte console *)
  let print_list_segment = fun list_seg -> 
    List.iter (fun s -> Printf.printf "id %d : e1 (%d, %d) | e2 (%d, %d)\n" s.id_s s.x1 s.y1 s.x2 s.y2) list_seg







  (* ============================================================================================= *)
  (*                            COLOR, VERTICES AND DEGREE FUNCTIONS                               *)
  (* ============================================================================================= *)

  (* Recupere la couleur d'un noeud dans un graph *) 
  let get_vertex_color = fun graph id -> 
    let node_char = find graph id in 
    node_char.color

  (* Verifie qu'un noeud est coloré dans un graphe *) 
  let is_colored = fun graph id -> get_vertex_color graph id <> 0

  (* Récupère le degré d'un noeud dans un graphe *) 
  let get_vertex_degree = fun graph id -> 

    let node_char = find graph id in  
    List.length node_char.adj
  
  (* Récupère le degré de saturation d'un noeud dans un graphe *)
  (* Degré de saturation = taille de la liste des couleurs différentes non nulles, sans redondances *) 
  let get_vertex_dsat = fun graph id -> 

    let node_char = find graph id in 
    let adj = node_char.adj in 
    
    let rec aux = fun nodes_to_visit acc ->
      match nodes_to_visit with 
        [] -> List.length (compress acc)
      | id :: t -> 
          let color = get_vertex_color graph id in 
          aux t (if color <> 0 then color :: acc else acc) in

    aux adj []

  (* Met à jour le degré de saturation d'un noeud *) 
  let update_vertex_dsat = fun graph id ->

    let node_char = find graph id in 
    let new_dsat = get_vertex_dsat graph id in 

    let new_node_char = {adj = node_char.adj ; color = node_char.color ; degree = node_char.degree ; dsat = new_dsat} in 
    Dom.replace graph id new_node_char
  
  (* Colorie un noeud *) 
  (* Met à jour les degrés de saturations *) 
  let color_vertex = fun graph id new_color ->

    let node_char = find graph id in 
    let new_node_char = {adj = node_char.adj ; color = new_color ; degree = node_char.degree ; dsat = node_char.dsat} in 

    Dom.replace graph id new_node_char;
    List.iter (fun id -> update_vertex_dsat graph id) new_node_char.adj

  (* Trouve la couleur la plus petite disponible sur un noeud *) 
  let lowest_coloration = fun graph id ->

    let node_char = find graph id in 
    let adjs_colors = List.map (fun id -> get_vertex_color graph id) node_char.adj in 
    let csorted_adjs_colors = pop_cond (List.sort (fun color1 color2 -> color1 - color2) (compress adjs_colors)) (fun x -> x = 0) in

    let rec aux = fun l av_color -> 
      match l with 
        [] -> av_color 
      | color :: t -> if color = av_color then aux t (av_color + 1) else av_color in 

    aux csorted_adjs_colors 1

  (* Teste si le graphe est entièrement coloré *) 
  let is_fully_colored = fun graph ->
    let nodes_list = Dom.fold (fun id _ -> List.cons id) graph [] in
    
    let rec aux = fun l -> 
      match l with 
        [] -> true 
      | id :: t -> if is_colored graph id then aux t else false in 

    aux nodes_list

  (* tester si une coloration est possible *)
  let is_valid_coloration = fun graph id couleur -> 

    let node_char = find graph id in 
    let adjs_colors = List.map (fun id -> get_vertex_color graph id) node_char.adj in

    not (List.mem couleur adjs_colors)

  (* Renvoie la liste des noeuds non coloriés d'un graph *)
  let get_uncolored_vertices = fun graph -> 
    let nodes = Dom.fold (fun id _ -> List.cons id) graph [] in 

    let rec aux = fun l r ->
      match l with 
      | [] -> r
      | hd::tl -> 
        if is_colored graph hd then 
          aux tl r 
        else 
          aux tl (hd::r) 
      in
    
    aux nodes [] 


  (* Retourne le nombre chromatique d'un graphe après coloration *)
  let chromatic_number = fun graph -> 

    let l = Dom.fold (fun id characteristics -> List.cons characteristics.dsat) graph [] in
    (List.fold_left (fun x y -> max x y) 0 l) + 1










  (* ============================================================================================= *)
  (*                            BUILDING INTERSECTION GRAPHS FUNCTIONS                             *)
  (* ============================================================================================= *)
    
  (* Fabrique une liste d'objets segments à partir du graphe d'instance *)
  let extract_segment = fun (graph_inst : instance_graph) ->

    (* Extraction des coordonnées x et y de chaque noeud *)
    let x = graph_inst.x in 
    let y = graph_inst.y in 
    
    (* Combinaison des coordonnées x et y pour créer des objets 2D : segments *)
    let rec aux = fun e_i e_j s i ->
      match (e_i, e_j) with 
      | ([], []) -> s 
      | (hi::ti, hj::tj) -> aux ti tj ({id_s=i; x1=get x hi; y1= get y hi; x2=get x hj; y2= get y hj}::s) (i+1) 
      | (_, _) -> raise Error in 
      
    (* On retourne les arêtes *)
    aux graph_inst.edge_i graph_inst.edge_j [] 1 
  
  (* Fonction ClockWise utile pour le calcul d'intersections *)
  let ccw = fun (x1,y1) (x2,y2) (x3,y3) -> 
    let t = (y3 - y1)*(x2 - x1) - (y2-y1)*(x3-x1) in
    if t < 0 then -1 else 1

  (* Vérifie que 3 points sont allignés dans le plan *)
  let is_alligned = fun (x1,y1) (x2,y2) (x3,y3) -> (x2-x1)*(y3-y2) - (y2-y1)*(x3-x2) = 0
  
  (* Fonction qui vérifie si deux segments du plan s'intersectent *)
  let intersect = fun s1 s2 ->
    
    (* Voir l'algorithme clockwise *)
    match (s1,s2) with
      | ({id_s = id1; x1 = x1; y1 = y1; x2 = x2; y2 = y2}, {id_s = id2; x1 = x3; y1 = y3; x2 = x4; y2 = y4}) -> 
        let a = (x1,y1) in 
        let b = (x2,y2) in 
        let c = (x3,y3) in 
        let d = (x4,y4) in
        
        (* Amusez vous bien à comprendre ça *)
        if a <> c && a <> d && b <> c && b <> d then 
          if ccw c d a <> ccw c d b && ccw a b c <> ccw a b d then 
            not (is_alligned a b c || is_alligned a b d || is_alligned c d a || is_alligned c d b) 
          else false 
        else false 
  
  (* Construire la liste des segments voisins *)
  let neighbours_segment = fun s l ->

    (* Des segments qui s'intersectent dans le grace d'instance sont voisins dans le graphe d'intersection *)
    let rec aux = fun s l acc ->
      match l with 
      | [] -> acc 
      | h::t -> if h.id_s <> s.id_s && intersect s h then aux s t (h::acc) else aux s t acc in

    aux s l [] 
  
  (* ajouter dans le graph *)
  let insert_segment = fun graph s s_adj -> 

    (* On récupère l'iD d'un segment *)
    let id = s.id_s in 
  
    (* On accumule les voisins de ce semgent *)
    let rec aux = fun l acc -> 
      match l with 
      | [] -> acc
      | h::t -> aux t (h.id_s::acc) in 
      
    let adj_int = aux s_adj [] in 

    (* On édite les caractéristiques du noeud dans le graphe d'intersection *)
    let l = List.length adj_int in 
    let node_chara = {adj = adj_int; color = if l <> 0 then 0 else 1 ; degree = l; dsat = 0} in 
    
    (* On ajoute ce noeud (id, caracteristiques) dans le graphe d'intersection *)
    add graph id node_chara 
  
  (* Prend une liste de segments et construit un graphe d'intersection *)
  let edit_intersection_graph = fun graph s_list -> 
    
    (* On insere chaque segment pour construire le graphe d'intersection complet *)
    let rec aux = fun l ->

      match l with 
      | [] -> ()
      | s::t -> insert_segment graph s (neighbours_segment s s_list); aux t in

    aux s_list















  (* ============================================================================================= *)
  (*                                      COLORING ALGORITHMS                                      *)
  (* ============================================================================================= *)

  (* Fonction principale qui execute l'algo DSATUR *) 
  (* Complexite O(n^2) *)
  let dsatur = fun graph ->

    (* Récupère les (id, degrés, degré de staturaiton) de chaque noeud du graph *) 
    let vertex_list= Dom.fold (fun id _ -> 
      let node_char = find graph id in List.cons (id, node_char.degree, node_char.dsat)) graph [] in
    
    (* Trie la liste des noeuds par degrés décroissants *)
    let sorted_vertex_degree = List.sort (fun (id1, degree1, _) (id2, degree2, _) -> degree2-degree1) vertex_list in 
    
    (* Colorer le noeud de degré maximal (premier élément de la liste) avec 1 *) 
    match sorted_vertex_degree with 
      [] -> raise (Empty_graph "Graphe vide")
    | (id, _, _) :: _ -> color_vertex graph id 1 ;

    (* Boucle while(graphe pas coloré entièrement) *) 
    let rec dsatur_recursive = fun () -> 

      (* Vérifier si le graph est entièrement colorié *)
      if is_fully_colored graph then 
        Some graph 

      (* Sinon on continue le processus de coloriage *)
      else 
        begin 

          let vertex_list= Dom.fold (fun id _ -> 
            let node_char = find graph id in List.cons (id, node_char.degree, node_char.dsat)) graph [] in
          
          (* Trie la liste des noeuds par degré de saturation décroissant et en cas d'égalité par degré décroissant *)
          let sorted_vertex_dsat = List.sort (
            fun (id1, degree1, dsat1) (id2, degree2, dsat2) -> if dsat2-dsat1 = 0 then degree2-degree1 else dsat2-dsat1) vertex_list in 
          
          (* Renvoyer le premier noeud de la liste qui n'est pas encore colorié *) 
          let rec first_uncolored_vertex = fun l -> 
            match l with 
            | [] -> raise (Empty_list "List vide")
            | (id, _, _) :: t -> if is_colored graph id then first_uncolored_vertex t else id in 
          
          (* On récupère le premier noeud non colorié dans la liste *)
          let uncolored_vertex = first_uncolored_vertex sorted_vertex_dsat in
          
          (* Colorier ce noeud avec la couleur la plus petite possible *) 
          color_vertex graph uncolored_vertex (lowest_coloration graph uncolored_vertex);

          (* On réitère le procédé *)
          dsatur_recursive () 
        
        end 
      in
    
    (* Récupère la valeur sans option, dans le cas erreur on renvoie le graph initial *)
    get_value_from_option (dsatur_recursive ()) graph 


  (* Fonction principale qui exectute l'algo DSATUR BRANCH AND BOUND *)
  (* Complexite : O() *)
  let dsaturbnb = fun graph ->

    (* Calcule de la borne inferieure *)
    let lb = chromatic_number (dsatur (Dom.copy graph)) in 

    (* Initialisation de la borne supérieure *)
    let ub = length graph in 

    (* FOnction dsatur recursive *)
    let rec dsatur_recursive = fun g lb ub -> 

      (* Si le graph est entièrement colorié *)
      if is_fully_colored g then Some g
      
      (* Sinon on continue à le colorier *)
      else
        begin 
          (* Fonction recursive qui teste la coloration d'un noeud v par une couleur c *)
          let rec try_colors = fun g v c -> 

            (* On autorise que les couleurs connues et une nouvelle couleur *)
            if c > (ub+1) then 
              None 

            (* Si c'est le cas *)
            else 
              begin 
                
                (* Si la coloration est valide dans le graph *)
                if is_valid_coloration g v c then 

                  (* On fait une copie du graph *)
                  let graph_copy = Dom.copy g in 

                  (* On effectue la coloration sur la copie*)
                  color_vertex graph_copy v c; 

                  (* On calcule la nouvelle borne superieure *)
                  let new_ub = max lb (max ub c) in

                  (* Si la nouvelle borne supérieure passe en dessous de la borne inférieure *)
                  if (lb <= new_ub) then 
                    begin 

                      (* On applique la fonction dsatur, son resultat est soit renvoyé car c'est une coloration valide soit on change la coloration *)
                      match dsatur_recursive graph_copy lb new_ub with 
                        |Some result -> Some result 
                        |None -> try_colors g v (c + 1)

                    end
                  (* Si on est encore au dessus de la borne inférieure on change la coloration *)
                  else
                    try_colors g v (c + 1)

                else
                  try_colors g v (c+1) 

              end 
            in
          
          (* Fonction qui parcours les noeuds candidats *)
          let rec try_vertices = fun g vertices -> 

            (* Soit la liste est vide soit on extrait un noeud pour essayer de lui appliquer une coloration *)
            match vertices with 
              | [] -> None 
              | v :: vertices_tl -> 
                match (try_colors g v 1) with 
                  |Some result -> Some result 
                  |None -> try_vertices g vertices_tl 
              
            in
          
          
          (* On récupère tous les noeuds nos coloriés *)
          let uncolored_vertices = get_uncolored_vertices g in 
          try_vertices g uncolored_vertices

        end

      in 

    (* Récupère la valeur sans option, dans le cas erreur on renvoie le graph initial *)
    get_value_from_option (dsatur_recursive graph lb ub) graph

end

(* On génére notre module à l'aide du foncteur *)
module Graph=Make(Hashtbl)

