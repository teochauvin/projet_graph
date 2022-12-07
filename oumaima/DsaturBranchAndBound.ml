(* Supprime les éléments redondants d'une liste *) 
let rec compress = fun l -> 
  match l with 
    a :: (b :: _ as t) -> if a = b then compress t else a :: compress t 
  | smaller -> smaller 

(* pop le premier éléments d'une liste sous une condition cond *) 
let pop_cond = fun l cond -> 
  match l with
    [] -> l
  | h :: t -> if cond h then t else l
  
(* représentation d'un Graph *) 
module Graph = struct
  (* à chaque noeud est associe des caractéristique rassemblées ici dans un record *) 
  type nodes_characteristics = {adj : int list ; color : int ; degree : int ; dsat : int}
  
  (* Exceptions *) 
  exception Empty_graph of string 
  exception Empty_list of string 
  
  (* Fonction communes au module Hashtbl : justifie le besoin d'un foncteur *) 
  let create = fun n -> Hashtbl.create n 

  let add = fun graph (id : int) (characteristics : nodes_characteristics) -> Hashtbl.add graph id characteristics

  let find = fun graph id -> Hashtbl.find graph id
  
  (* Représentation d'un graph sous format texte console *) 
  let print_graph = fun graph ->
    Hashtbl.iter (fun id characteristics -> Printf.printf "id : %d, color : %d, degree : %d, dsat : %d\n" id characteristics.color characteristics.degree characteristics.dsat) graph
  
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
      
  (*les couleurs dispo entre a et b*)
  let rec available_colors = fun a b ->
    if a > b then [] else a :: available_colors (succ a) b 

  (*tester si une  couleur est possible*)
  let test_color = fun graph id couleur -> 
    let node_char = find graph id in 
    let adjs_colors = List.map (fun id -> get_color graph id) node_char.adj in
    not (List.mem couleur adjs_colors)

  let rec backtrack = fun graphModif graphOrigine keys-> 
    if not(keys = []) then
      begin
        let head = List.hd keys in
        let head_char = Hashtbl.find graphOrigine head in 
        Hashtbl.replace graphModif head head_char;
        List.iter (fun id -> change_dsat graphModif id) head_char.adj;
        backtrack graphModif graphOrigine (List.tl keys)
      end
  
  let chosen_vertex = fun graph -> 
    (*la liste des sommets*)
    let nodes_list = Hashtbl.fold (fun id _ -> List.cons id) graph [] in
    let rec aux_chosen_vertex = fun l ->
      match l with 
        [] -> raise (Empty_list "List vide")
      |id :: t -> if is_colored graph id then aux_chosen_vertex t else id in 
    aux_chosen_vertex nodes_list
 
  (* Fonction principale d'algo DSATUR Branch and Bound (Backtrack)*) 
  let dsatureBnB = fun graph ->
    (*Juste pour tester, à changer par dsatur *)
    let lB = 3 in 
    let rec aux = fun graph colors visited uB->
      if lB >= uB then 
        begin
          print_string "\n";
          print_graph  graph
        end
      else if not (is_all_colored graph) then 
        begin
          (*Enregistrer une copie du graphe avant modification*)
          let graph2 = Hashtbl.copy graph in 
          (* séléctionner un noued non coloré*)
          let id_chosen_vertex = chosen_vertex graph in
          let k = if colors=[] then 1 else List.length (compress colors) in
          (*loop in colors dispo de 1 jusqu'à k+1*)
          for color = 1 to (k+1) do
            (*tester si color est une couleur possible et colorer le noued avec color*)
            if test_color graph id_chosen_vertex color then 
              begin
                change_color graph id_chosen_vertex color;
                id_chosen_vertex :: visited;
                aux graph (color :: colors) visited uB;
                backtrack graph graph2 visited;
              end
          done
        end
      else if (is_all_colored graph) && List.length (compress colors) <uB then 
        (*changer la vaeur de uB a k*)
        aux graph colors visited (List.length (compress colors));
    in aux graph [] [] (Hashtbl.length graph)
end



let () =

  let graph = Graph.create 5 in 
  
  Graph.add graph 1 {adj = [2 ; 3 ; 5] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 2 {adj = [1 ; 3] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.add graph 3 {adj = [1 ; 2 ; 4] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 4 {adj = [3 ; 5] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.add graph 5 {adj = [4 ; 1] ; color = 0 ; degree = 2 ; dsat = 0};

  Graph.dsatureBnB graph