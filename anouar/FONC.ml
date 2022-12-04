
module type Hash = sig
  type (!'a, !'b) t
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
  val rebuild : ?random:bool -> ('a, 'b) t -> ('a, 'b) t
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
      type !'a t
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
      type !'a t
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
(*fin de la définition de signature nécessaire...possible de mettre ça dans un fichier à part,aucune modification ne s'effectue ici*)
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)

                                      
(*foncteur, définition du module graphe en retour, tout ajout au module graph se fait à l'interieur de la defnition du foncteur  *)
module Make=functor (Dom: Hash)-> struct
  type nodes_characteristics = {adj : int list ; color : int ; degree : int ; dsat : int}

  exception Empty_graph of string 
  exception Empty_list of string

  let rec compress = fun l -> 
    match l with 
      a :: (b :: _ as t) -> if a = b then compress t else a :: compress t 
    | smaller -> smaller 

  (* pop le premier éléments d'une liste sous une condition cond *) 
  let pop_cond = fun l cond -> 
    match l with
      [] -> l
    | h :: t -> if cond h then t else l

  (* Fonction communes au module Hashtbl : justifie le besoin d'un foncteur *) 
  let create =Dom.create 

  let add = Dom.add 

  let find = Dom.find

  (* Représentation d'un graph sous format texte console *) 
  let print_graph = fun graph ->
    Dom.iter (fun id characteristics -> Printf.printf "id : %d, color : %d, degree : %d, dsat : %d\n" id characteristics.color characteristics.degree characteristics.dsat) graph

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
    Dom.replace graph id new_node_char
  
  (* Change la couleur d'un noeud dans un graphe *) 
  (* Met à jour le degré de saturation de tous les noeuds adjacents *) 
  let change_color = fun graph id new_color ->
    let node_char = find graph id in 
    let new_node_char = {adj = node_char.adj ; color = new_color ; degree = node_char.degree ; dsat = node_char.dsat} in 
    Dom.replace graph id new_node_char;
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
    let nodes_list = Dom.fold (fun id _ -> List.cons id) graph [] in
    
    let rec aux = fun l -> 
      match l with 
        [] -> true 
      | id :: t -> if is_colored graph id then aux t else false in 
    aux nodes_list

  (* Fonction principale qui execute l'algo DSATUR *) 
  let color_with_DSATUR = fun graph ->

    (* Trier les noeuds par ordre de degré décroissants *) 
    let raw_deg_Vlist = Dom.fold (fun id _ -> 
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
    let raw_dsat_Vlist = Dom.fold (fun id _ -> 
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

  end
  
(*On génére notre module à l'aide du foncteur*)

module Graph=Make(Hashtbl)

(*On donne accés à ce qui est défini dans le module Graph*)

open(Graph) 

(* MAIN *) 
let () =


  
  let graph = create 5 in 
  
  add graph 1 {adj = [2 ; 3 ; 5] ; color = 0 ; degree = 3 ; dsat = 0};
  add graph 2 {adj = [1 ; 3] ; color = 0 ; degree = 2 ; dsat = 0};
  add graph 3 {adj = [1 ; 2 ; 4] ; color = 0 ; degree = 3 ; dsat = 0};
  add graph 4 {adj = [3 ; 5] ; color = 0 ; degree = 2 ; dsat = 0};
  add graph 5 {adj = [4 ; 1] ; color = 0 ; degree = 2 ; dsat = 0};
  print_graph (color_with_DSATUR graph) 
  

  
  (* GRAPHE 2 
  
  let graph = Graph.create 8 in 
  
  Graph.add graph 1 {adj = [2 ; 3 ; 4 ; 5] ; color = 0 ; degree = 4 ; dsat = 0};
  Graph.add graph 2 {adj = [1 ; 3 ; 4 ; 6] ; color = 0 ; degree = 4 ; dsat = 0};
  Graph.add graph 3 {adj = [1 ; 2] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.add graph 4 {adj = [1 ; 2] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.add graph 5 {adj = [1 ; 7 ; 8] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 6 {adj = [2 ; 7 ; 8] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 7 {adj = [5 ; 6 ; 8] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 8 {adj = [5 ; 6 ; 7] ; color = 0 ; degree = 3 ; dsat = 0};
  
  Graph.print_graph (Graph.color_with_DSATUR graph);
  
  *)