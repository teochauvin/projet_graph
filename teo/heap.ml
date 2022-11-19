(* Reimplémentation du tas binaire : on en a besoin pour l'implémentation de la sweepline *) 

module Heap = struct 

  type 'a heap = 
    | Node of 'a * 'a heap * 'a heap * int 
    | Leaf 

  exception HeapException 

  let create = fun () -> Leaf 

  (* Inserer un élement dans le Heap *)
  let rec insert = fun heap x ->

    match heap with 
    | Leaf -> Node (x, Leaf, Leaf, 0)
    | Node (v, l, r, n) ->
      let (stay, move) = if x > v then (x, v) else (v, x) in 

        match (l, r) with 
          | (Leaf, Leaf) -> Node (stay, Node (move, Leaf, Leaf, 0), Leaf, 1)
          | (Leaf, _) -> Node (stay, Node (move, Leaf, Leaf, 0), r, n+1)
          | (_, Leaf) -> Node (stay, l, Node (move, Leaf, Leaf, 0), n+1)
          | (Node (_, _, _, n1), Node (_, _, _, n2)) ->
            if n1 <= n2 then
              Node (stay, (insert l move), r, n1+1)
            else 
              Node (stay, l, (insert r move), n2+1)
  
  (* fusionner deux arbres recursivement *)
  let rec merge = fun l r ->
    match (l,r) with 
    | (Leaf, Leaf) -> Leaf 
    | (Leaf, _) -> r 
    | (_, Leaf) -> l 
    | (Node (e1, l1, r1, n1), Node (e2, l2, r2, n2)) -> 
      if e1 >= e2 then 
        Node(e1, merge l1 r1, r, n1 + n2)
      else 
        Node(e2, l, merge l2 r2, n1 + n2) 

  (* On delete le plus gros élément *) 
  let delete = fun heap ->
    match heap with 
    | Leaf -> raise HeapException
    | Node (_, l, r, _) -> merge l r

  (* Print *)
  let print_heap = fun heap ->
    let rec aux = fun h acc ->
      match h with 
      | Leaf -> acc
      | Node (e, l, r, n) -> aux (merge l r) (e::acc) in
    List.iter (Printf.printf "%d ") (aux heap [])
    
end

let () = 

  (*Démonstration que le tas binaire fonctionne *)
  let heap = Heap.create () in  
  let sequence = [19;100;35;16;94;53;23;15;86;94;18] in
  let heap_fill = List.fold_left (fun heap x -> Heap.insert heap x) heap sequence in 
  Heap.print_heap(heap_fill); 


