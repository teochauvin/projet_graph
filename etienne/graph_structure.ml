let rec compress = fun l -> 
  match l with 
    a :: (b :: _ as t) -> if a = b then compress t else a :: compress t 
  | smaller -> smaller 

let pop_cond = fun l cond -> 
  match l with
    [] -> l
  | h :: t -> if cond h then t else l
  
module Graph = struct
  type nodes_characteristics = {x : int ; y : int ; adj : int list ; color : int ; degree : int ; dsat : int}
  
  exception Empty_graph of string 
  exception Empty_list of string 

  let create = fun n -> Hashtbl.create n 

  let add = fun graph (id : int) (characteristics : nodes_characteristics) -> Hashtbl.add graph id characteristics

  let find = fun graph id -> Hashtbl.find graph id

  let print_graph = fun graph ->
    Hashtbl.iter (fun id characteristics -> Printf.printf "id : %d, color : %d, degree : %d, dsat : %d\n" id characteristics.color characteristics.degree characteristics.dsat) graph

  let add_adj = fun graph id1 id2 -> 
    let node_char1 = find graph id1 in 
    let node_char2 = find graph id2 in
    let new_node_char1 = {x = node_char1.x ; y = node_char1.y ; adj = id2 :: node_char1.adj ; color = node_char1.color ; degree = node_char1.degree + 1 ; dsat = node_char1.dsat} in 
    let new_node_char2 = {x = node_char2.x ; y = node_char2.y ; adj = id1 :: node_char2.adj ; color = node_char2.color ; degree = node_char2.degree + 1 ; dsat = node_char2.dsat} in 
    Hashtbl.replace graph id1 new_node_char1;
    Hashtbl.replace graph id2 new_node_char2

  let get_color = fun graph id -> 
    let node_char = find graph id in 
    node_char.color
  
  let is_colored = fun graph id -> get_color graph id <> 0

  let get_degree = fun graph id -> 
    let node_char = find graph id in  
    List.length node_char.adj
  
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

  let change_dsat = fun graph id ->
    let node_char = find graph id in 
    let new_dsat = get_saturation_degree graph id in 
    let new_node_char = {x = node_char.x ; y = node_char.y ; adj = node_char.adj ; color = node_char.color ; degree = node_char.degree ; dsat = new_dsat} in 
    Hashtbl.replace graph id new_node_char

  let change_color = fun graph id new_color ->
    let node_char = find graph id in 
    let new_node_char = {x = node_char.x ; y = node_char.y ; adj = node_char.adj ; color = new_color ; degree = node_char.degree ; dsat = node_char.dsat} in 
    Hashtbl.replace graph id new_node_char;
    List.iter (fun id -> change_dsat graph id) new_node_char.adj

  let lowest_color_available = fun graph id ->
    let node_char = find graph id in 
    let adjs_colors = List.map (fun id -> get_color graph id) node_char.adj in 
    let csorted_adjs_colors = pop_cond (List.sort (fun color1 color2 -> color1 - color2) (compress adjs_colors)) (fun x -> x = 0) in

    let rec aux = fun l av_color -> 
      match l with 
        [] -> av_color 
      | color :: t -> if color = av_color then aux t (av_color + 1) else av_color in 

    aux csorted_adjs_colors 1

  let is_all_colored = fun graph ->
    let nodes_list = Hashtbl.fold (fun id _ -> List.cons id) graph [] in
    
    let rec aux = fun l -> 
      match l with 
        [] -> true 
      | id :: t -> if is_colored graph id then aux t else false in 
    aux nodes_list

  let color_with_DSATUR = fun graph ->
    let raw_deg_Vlist = Hashtbl.fold (fun id _ -> 
      let node_char = find graph id in List.cons (id, node_char.degree, node_char.dsat)
      ) graph [] in

    let deg_sorted_Vlist = List.sort (fun (id1, degree1, _) (id2, degree2, _) -> degree2-degree1) raw_deg_Vlist in 

    match deg_sorted_Vlist with 
      [] -> raise (Empty_graph "Graphe vide")
    | (id, _, _) :: _ -> change_color graph id 1 ;

  let rec loop = fun () ->

    let raw_dsat_Vlist = Hashtbl.fold (fun id _ -> 
      let node_char = find graph id in List.cons (id, node_char.degree, node_char.dsat)
      ) graph [] in

    let dsat_sorted_Vlist = List.sort (
      fun (id1, degree1, dsat1) (id2, degree2, dsat2) -> if dsat2-dsat1 = 0 then degree2-degree1 else dsat2-dsat1
      ) raw_dsat_Vlist in 
    
    let rec aux = fun l -> 
      match l with 
        [] -> raise (Empty_list "List vide")
      | (id, _, _) :: t -> if is_colored graph id then aux t else id in 

    let id_chosen_vertex = aux dsat_sorted_Vlist in
    
    change_color graph id_chosen_vertex (lowest_color_available graph id_chosen_vertex);
    
    if is_all_colored graph then graph else loop () in 
  
  loop ()

end


(* let () =
  (* let graph = Graph.create 5 in 
  Graph.add graph 1 {adj = [2 ; 3 ; 5] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 2 {adj = [1 ; 3] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.add graph 3 {adj = [1 ; 2 ; 4] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 4 {adj = [3 ; 5] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.add graph 5 {adj = [4 ; 1] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.print_graph graph;
  Printf.printf "\n";
  Graph.print_graph (Graph.color_with_DSATUR graph) *)

  let graph = Graph.create 8 in 
  Graph.add graph 1 {adj = [2 ; 3 ; 4 ; 5] ; color = 0 ; degree = 4 ; dsat = 0};
  Graph.add graph 2 {adj = [1 ; 3 ; 4 ; 6] ; color = 0 ; degree = 4 ; dsat = 0};
  Graph.add graph 3 {adj = [1 ; 2] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.add graph 4 {adj = [1 ; 2] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.add graph 5 {adj = [1 ; 7 ; 8] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 6 {adj = [2 ; 7 ; 8] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 7 {adj = [5 ; 6 ; 8] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 8 {adj = [5 ; 6 ; 7] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.print_graph (Graph.color_with_DSATUR graph); *)

