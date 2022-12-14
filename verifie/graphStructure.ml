(* ============================================================================================= *)
(*                                       INFORMATIONS                                            *)
(*                                       MODULE GRAPH                                            *)
(*            COLORATION:                                 CONVERSION INSTANCE-INTERSECTION       *)
(*              DSATUR                                                                           *)
(*      DSATUR BRANCH AND BOUND                                                                  *)
(*                                                                                               *)
(*                                                                                               *)
(* ============================================================================================= *)

module Heap = Color_heap.Heap


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
                                      
(*foncteur, d??finition du module graphe en retour, tout ajout au module graph se fait ?? l'interieur de la defnition du foncteur  *)
module Make=functor (Dom: Hash)-> struct

  (* ============================================================================================= *)
  (*                                  TYPE DEFS & EXCEPTIONS                                       *)
  (* ============================================================================================= *)

  (* ?? chaque noeud est associe des caract??ristique rassembl??es ici dans un record *)   
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
  (* l taille de la liste alors O(l log l) *)
  let compress = fun l -> 
    let sorted_l = List.sort (fun color1 color2 -> color1 - color2) l in 

    let rec aux = fun sl ->
      match sl with 
        a :: (b :: _ as t) -> if a = b then aux t else a :: aux t 
      | smaller -> smaller in

    aux sorted_l 
    
  (* R??cup??re l'??l??ment d'indice i d'une liste l*)
  let rec get = fun l i ->
    match l with 
    | [] -> raise Empty
    | h::t -> if i=0 then h else get t (i-1)

  (* pop le premier ??l??ments d'une liste sous une condition cond *) 
  let pop_cond = fun l cond -> 
    match l with
      [] -> l
    | h :: t -> if cond h then t else l

  (* Renvoie les informations dans un record de type 'raw_graph' *)
  let build_raw_graph_record = fun id n m xs ys edges_i edges_j ->
    {id = id ; n = n ; m = m ; x = xs ; y = ys ; edge_i = edges_i ; edge_j = edges_j}

  (* Fonction communes au module d'entr??e du foncteur - ici Hashtbl *) 
  let create =Dom.create 

  let add = Dom.add 

  let find = Dom.find

  let length = Dom.length









  (* ============================================================================================= *)
  (*                                           PRINTS                                              *)
  (* ============================================================================================= *)

  (* Repr??sentation d'un graph sous format texte console *) 
  let print_graph = fun graph ->
    Dom.iter (
      fun id characteristics -> 
        Printf.printf "\nid : %d, color : %d, degree : %d, dsat : %d et adj : " id characteristics.color characteristics.degree characteristics.dsat; 
        List.iter (Printf.printf "%d|") characteristics.adj) graph;
    Printf.printf("\n\n\n")

  let show_coloration = fun graph ->
    Dom.iter (fun id characteristics -> Printf.printf "id : %d, color %d \n" id characteristics.color) graph

  (* Repr??sente la liste de segments sous format texte console *)
  let print_list_segment = fun list_seg -> 
    List.iter (fun s -> Printf.printf "id %d : e1 (%d, %d) | e2 (%d, %d)\n" s.id_s s.x1 s.y1 s.x2 s.y2) list_seg







  (* ============================================================================================= *)
  (*                            COLOR, VERTICES AND DEGREE FUNCTIONS                               *)
  (* ============================================================================================= *)

  (* Recupere la couleur d'un noeud dans un graph *) 
  let get_vertex_color = fun graph id -> 
    let node_char = find graph id in 
    node_char.color

  (* Verifie qu'un noeud est color?? dans un graphe *) 
  let is_colored = fun graph id -> get_vertex_color graph id <> 0

  (* R??cup??re le degr?? d'un noeud dans un graphe *) 
  let get_vertex_degree = fun graph id -> 

    let node_char = find graph id in  
    List.length node_char.adj
  
  (* R??cup??re le degr?? de saturation d'un noeud dans un graphe *)
  (* Degr?? de saturation = taille de la liste des couleurs diff??rentes non nulles, sans redondances *) 
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

  (* Met ?? jour le degr?? de saturation d'un noeud *) 
  (* O(1) *)
  let update_vertex_dsat = fun graph id ->

    let node_char = find graph id in 
    let new_dsat = get_vertex_dsat graph id in 

    let new_node_char = {adj = node_char.adj ; color = node_char.color ; degree = node_char.degree ; dsat = new_dsat} in 
    Dom.replace graph id new_node_char
  
  (* Colorie un noeud et actualise les adjacents *) 
  (* O(k) k<<n *)
  let color_vertex = fun graph id new_color ->

    let node_char = find graph id in 
    let new_node_char = {adj = node_char.adj ; color = new_color ; degree = node_char.degree ; dsat = node_char.dsat} in 

    (* Coloration dans la Hashtbl*)
    Dom.replace graph id new_node_char;

    (* Modifie les degr?? de saturation des adjacents dans la hashtabl *)
    List.iter (fun id -> update_vertex_dsat graph id) new_node_char.adj

  (* Renvoie la liste [(id_adj, dsat_adj) ; ...] d'un noeud *)
  let get_adj_tuple = fun graph id -> 
    let node_char = find graph id in 
    
    let rec aux = fun l acc -> 
      match l with
        [] -> acc 
      | h :: t ->
          begin 
            if not(is_colored graph h) then 
              begin
                let h_char = find graph h in 
                aux t ((h, h_char.dsat) :: acc)
              end 
            else 
              aux t acc 
          end in 

    aux node_char.adj []
  
  (* Trouve la couleur la plus petite disponible sur un noeud *) 
  (* O(k log k) *)

  let lowest_coloration = fun graph id ->

    let node_char = find graph id in 
    let adjs_colors = List.map (fun id -> get_vertex_color graph id) node_char.adj in 

    let csorted_adjs_colors = pop_cond (List.sort (fun color1 color2 -> color1 - color2) (compress adjs_colors)) (fun x -> x = 0) in

    let rec aux = fun l av_color -> 
      match l with 
        [] -> av_color 
      | color :: t -> if color = av_color then aux t (av_color + 1) else av_color in 

    aux csorted_adjs_colors 1 

  (* Teste si le graphe est enti??rement color?? *) 
  (* Tres inefficace et en pratique non utilis?? car O(n) *)
  let is_fully_colored = fun graph ->
    let nodes_list = Dom.fold (fun id _ -> List.cons id) graph [] in
    
    let rec aux = fun l -> 
      match l with 
        [] -> true 
      | id :: t -> if is_colored graph id then aux t else false in 

    aux nodes_list

  (* Donne les couleurs accessible pour un noeud : en O(k log k)*) 
  let accessible_colors_plus_one = fun graph k id -> 

    let node_char = find graph id in 
    let adjs_colors = List.sort (fun a b -> a-b) (List.map (fun id -> get_vertex_color graph id) node_char.adj) in
    let compressed_ac = pop_cond (compress adjs_colors) (fun x -> x=0) in 

    let ac = List.filter (fun a -> not (List.mem a compressed_ac)) (List.init (k+1) (fun x->x+1)) in 

    ac

  (* Renvoie la liste des noeuds non colori??s d'un graph *)
  let get_uncolored_vertex = fun graph -> 
    let nodes = Dom.fold (fun id _ -> List.cons id) graph [] in 

    let rec aux = fun l ->
      match l with 
      | [] -> raise Error 
      | hd::tl -> 
        if is_colored graph hd then 
          aux tl 
        else 
          hd 
        in aux nodes


  (* Retourne le nombre chromatique d'un graphe apr??s coloration *)
  let chromatic_number = fun graph -> 

    let l = Dom.fold (fun id characteristics -> List.cons characteristics.dsat) graph [] in
    (List.fold_left (fun x y -> max x y) 0 l) + 1

  (* Trouve la premi??re clique *)
  let find_clique = fun graph coloration_order -> 

    (* intersection d'id *)
    let intersect = fun l1 l2 -> 
      let l = List.sort (fun a b -> a-b) (List.append l1 l2) in 
      
      let rec aux = fun acc liste -> 

        match liste with 
        |[] | _::[] -> acc 
        |a :: (b :: _ as t) -> if a = b then aux (a::acc) t else aux acc t in 
      aux [] l in 

    let rec find_rec = fun l_id acc -> 

      match l_id with 
      | [] -> acc 
      | hd::tl -> 
        begin
          let adj = (find graph hd).adj in 
          let intersection = intersect adj acc in  

          if List.length intersection = List.length acc then find_rec tl (hd::acc) else acc 
        end in
    
    match coloration_order with 
    | [] -> raise Error 
    | head::liste_id -> find_rec liste_id [head]



  (* ============================================================================================= *)
  (*                            BUILDING INTERSECTION GRAPHS FUNCTIONS                             *)
  (* ============================================================================================= *)
    
  (* Fabrique une liste d'objets segments ?? partir du graphe d'instance *)
  let extract_segment = fun (graph_inst : instance_graph) ->

    (* Extraction des coordonn??es x et y de chaque noeud *)
    let x = graph_inst.x in 
    let y = graph_inst.y in 
    
    (* Combinaison des coordonn??es x et y pour cr??er des objets 2D : segments *)
    let rec aux = fun e_i e_j s i ->
      match (e_i, e_j) with 
      | ([], []) -> s 
      | (hi::ti, hj::tj) -> aux ti tj ({id_s=i; x1=get x hi; y1= get y hi; x2=get x hj; y2= get y hj}::s) (i+1) 
      | (_, _) -> raise Error in 
      
    (* On retourne les ar??tes *)
    aux graph_inst.edge_i graph_inst.edge_j [] 0 
  
  (* Fonction ClockWise utile pour le calcul d'intersections *)
  let ccw = fun (x1,y1) (x2,y2) (x3,y3) -> 
    let t = (y3 - y1)*(x2 - x1) - (y2-y1)*(x3-x1) in
    if t < 0 then -1 else 1

  (* V??rifie que 3 points sont allign??s dans le plan *)
  let is_alligned = fun (x1,y1) (x2,y2) (x3,y3) -> (x2-x1)*(y3-y2) - (y2-y1)*(x3-x2) = 0
  
  (* Fonction qui v??rifie si deux segments du plan s'intersectent *)
  let intersect = fun s1 s2 ->
    
    (* Voir l'algorithme clockwise *)
    match (s1,s2) with
      | ({id_s = id1; x1 = x1; y1 = y1; x2 = x2; y2 = y2}, {id_s = id2; x1 = x3; y1 = y3; x2 = x4; y2 = y4}) -> 
        let a = (x1,y1) in 
        let b = (x2,y2) in 
        let c = (x3,y3) in 
        let d = (x4,y4) in
        
        (* Amusez vous bien ?? comprendre ??a *)
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

    (* On r??cup??re l'iD d'un segment *)
    let id = s.id_s in 
  
    (* On accumule les voisins de ce segment *)
    let rec aux = fun l acc -> 
      match l with 
      | [] -> acc
      | h::t -> aux t (h.id_s::acc) in 
      
    let adj_int = aux s_adj [] in 

    (* On ??dite les caract??ristiques du noeud dans le graphe d'intersection *)
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
  (* *)
  let dsatur = fun graph ->

    (* R??cup??re les (degr?? de saturation, degr??) de chaque noeud du graph *)     
    let n = Dom.length graph in 

    (* O(n) *)
    let rec build_vertex_list = fun n vertex_list -> 
      if n < 0 then vertex_list 
    
      else 
        begin 
          let node_char = find graph n in 
          build_vertex_list (n - 1) ((node_char.dsat, node_char.degree) :: vertex_list)
        end in 

    let vertex_list = build_vertex_list (n - 1) [] in 
  
    (* Cr??e un heap vide dont la fonction de comparaison selon trie en fonction du dsat d??croissant et selon le degr?? en cas d'??galit?? *)
    let (hs : Heap.heap_struct) = {
      heap=Array.make (n) (-1, -1, -1); 
      assoc=Array.make (n) (-1); 
      active_size = ref 0 
    } in 
      
    (* Fonction de comparaison pour trier le tas *)
    let comp = fun (id1, dsat1, d1) (id2, dsat2, d2) -> 
      if dsat2-dsat1=0 then d1-d2 > 0 else dsat1-dsat2 >= 0 in 

    (* initialisation du tas *)
    (* O(n) *)
    Heap.init hs vertex_list comp;

    (* Boucle while(graphe pas color?? enti??rement) *) 
    let rec dsatur_recursive = fun acc -> 

      (* V??rifie si le graph est enti??rement colori?? *)
      (* O(1) *)
      if Heap.is_empty hs then 
        Some (graph, List.rev acc) 

      (* Sinon on continue le processus de coloriage *)
      else 
        begin

          (* On r??cup??re le noeud non colori?? de degr?? de saturation maximal et / ou de degr?? maximal *)
          (* O(1) *)
          let chosen_vertex = Heap.pop hs comp in

          (* Colorier ce noeud avec la couleur la plus petite possible *) 
          (* O(k log k) *)
          color_vertex graph chosen_vertex (lowest_coloration graph chosen_vertex);
          
          (* Mise ?? jour du tas *)
          (* O(k) *)
          let adj_tuple = get_adj_tuple graph chosen_vertex in 

          (* O(k log 2 p) *)
          List.iter (fun (id, dsat) -> Heap.update hs id dsat comp) adj_tuple; 

          (* On r??it??re le proc??d?? *)
          dsatur_recursive (chosen_vertex::acc)
        
        end 
      in
    
    (* R??cup??re la valeur sans option, dans le cas erreur on renvoie le graph initial *)
    get_value_from_option (dsatur_recursive []) (graph, []) 


  (* Fonction principale qui exectute l'algo DSATUR BRANCH AND BOUND *)
  let dsaturbnb = fun graph -> 

    (* Calcule de la borne inferieure *)
    let (colored_graph, coloration_order) = dsatur (Dom.copy graph) in 
    let lb = List.length (find_clique graph coloration_order) in 

    (* Initialisation de la borne sup??rieure *)
    let ub = chromatic_number (colored_graph) in

    (* DSATUR trouve l'optimum donc inutile de poursuivre *)
    if lb = ub then (colored_graph, ub) 
    
    (* Sinon on rentre dans Branch & Bound*)
    else
      begin 

        (* Fonction recursive BNB *)
        let rec dsatur_recursive = fun g k ub lb best_g ->   

          (* Condition d'arret : si on colorie completement le graph (n op??rations de coloration )*) 
          if is_fully_colored g then
            begin 

              (* Si la coloration obtenue est meilleure que la borne sup??rieure *) 
              if k < ub then 
                begin 
                  (* On actualise la upper bound *)
                  let new_ub = k in 

                  (* On retourne le meilleure graphe et la meilleure borne sup??rieure *)
                  Some (Dom.copy g, new_ub)
                end 

              (* La solution est moins bonne, on propage la meilleure solution connue *)
              else 
                Some (best_g, ub)  

            end 

          (* Si le graphe n'est pas completement colorie *) 
          else 
            begin

              (* Choisir un noeud non colorie *)
              let chosen_vertex = get_uncolored_vertex g in 

              (* Boucle for sur les couleurs accessibles plus une *) 
              let rec boucle = fun colors best_g best_ub ->  

                match colors with 
                | [] -> raise Error

                (* On cherche ?? colorier avec une nouvelle couleur *)
                | c::[] -> 
                  begin 

                    (* Si cette coloration reste interessante *)
                    if max lb (k+1) < best_ub then 
                      begin 

                        (* On colorie notre noeud *) 
                        color_vertex g chosen_vertex c; 
                        
                        (* On applique DSATUR recursivement *) 
                        match (dsatur_recursive g (k+1) best_ub lb best_g) with
                        | Some result -> 
                          begin
                            (* On d??colorie le noeud *)
                            color_vertex g chosen_vertex 0;

                            (* On backtrack avec la meilleure solution connue *)
                            Some result
                          end
                        | None -> raise Error 

                      end 
                    
                    (* Sinon on propage la meilleure solution connue *)
                    else 
                      Some(best_g, best_ub) 

                  end 
                
                (* Si on cherche ?? colorier avec une couleur existante dans le graphe *)
                | c::tl -> 
                  begin 

                    (* Si la coloration reste interessante *)
                    if max lb k < best_ub then 
                      begin 

                        (* On colorie notre graph *) 
                        color_vertex g chosen_vertex c; 
                        
                        (* On applique DSATUR recursivement *) 
                        match (dsatur_recursive g k best_ub lb best_g) with
                        | Some result -> 
                          begin
                            (* On r??cup??re la meilleure solution du sous arbre *)
                            let (new_best_g, new_best_ub) = result in 

                            (* On d??colorie le noeud *)
                            color_vertex g chosen_vertex 0;

                            (* On poursuit la coloration en actualisant la meilleure solution connue *)
                            boucle tl new_best_g new_best_ub 
                          end
                        | None -> raise Error 

                      end 

                    (* Si cette coloration n'est pas interessante on poursuit la coloration avec la meilleure solution connue *)
                    else 
                      boucle tl best_g best_ub 

                  end 
                in 

              (* On cherche les couleurs possibles et on entame la boucle *)
              let colors = accessible_colors_plus_one g k chosen_vertex in 

              boucle colors best_g ub 
            end 
          in 

        (* On exectute l'algorithme *)
        get_value_from_option (dsatur_recursive (Dom.copy graph) 0 ub lb (Dom.copy graph)) (graph, ub) 

      end

end 

(* On g??n??re notre module ?? l'aide du foncteur *)
module Graph=Make(Hashtbl)