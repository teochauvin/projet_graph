module Heap = struct 

  type heap_struct = {
    heap: (int*int*int) array; 
    assoc: int array; 
    mutable active_size: int ref 
  }

  (* permutation de deux élements *)
  let permut = fun heap_struct i j ->

    (* permutation dans le tas *)
    let change = heap_struct.heap.(i) in 
    heap_struct.heap.(i) <- heap_struct.heap.(j);
    heap_struct.heap.(j) <- change;

    (* permutation des valeurs dans la table d'association *)
    let (id1, dsat1, d1) = heap_struct.heap.(i) in 
    let (id2, dsat2, d2) = heap_struct.heap.(j) in  

    let tmp = heap_struct.assoc.(id1) in 
    heap_struct.assoc.(id1) <- heap_struct.assoc.(id2);
    heap_struct.assoc.(id2) <- tmp;

    (* retourne unit *)
    () 

  (* n est l'indice de la premiere case libre *)
  let up = fun heap_struct x comp -> 

    (* nombre d'éléments du tas *)
    let len = !(heap_struct.active_size)-1 in 
    heap_struct.heap.(len+1) <- x;

    let rec aux = fun i -> 

      if i=0 then () 
      else 

        let index_pere = (i-1)/2 in 

        let current = heap_struct.heap.(i) in 
        let pere = heap_struct.heap.(index_pere) in 


        (* Si le noeud > pere au sens de la comparaison *)
        if comp current pere then 
          begin
            permut heap_struct i index_pere;
            aux index_pere;
          end
        
        else () 

      in 
    aux (len+1)
  
  let down = fun heap_struct comp ->

    let len = !(heap_struct.active_size)-1 in 
  
    let rec aux = fun i -> 

      if 2*i+2 <= len then 
        begin
          let current = heap_struct.heap.(i) in 
          let left = heap_struct.heap.(2*i+1) in 
          let right = heap_struct.heap.(2*i+2) in

          (* On peut progresser vers la gauche et la droite est non vide *)
          if comp left current then begin
            permut heap_struct i (2*i+1);
            aux (2*i+1);
          end

          (* On peut progresser vers la droite et la gauche est non vide *)
          else if comp right current then begin
            permut heap_struct i (2*i+2);
            aux (2*i+2);
          end
        end 
      
      else if 2*i+1 = len then 
        begin 
          let current = heap_struct.heap.(i) in 
          let left = heap_struct.heap.(2*i+1) in 
          if comp left current then permut heap_struct i (2*i+1);
        end

      (* L'élement ne peut ni progresser à droite ni à gauche alors il est à sa place *)
      else ()
      
    in aux 0  


  (* Initialise un heap avec une liste de valeurs *)
  let init = fun heap_struct values comp ->
  
    let rec aux = fun v k -> 
      match v with 
      | [] -> ()
      | (dsat, d)::tl -> 

        (* On ajoute une association ID:position_heap *)
        heap_struct.assoc.(k) <- k; 
        
        (* On ajoute au heap *)
        up heap_struct (k,dsat,d) comp;

        (* La taille active du heap augmente (nombre d'élement actifs) *)
        heap_struct.active_size := !(heap_struct.active_size) + 1; 
        
        (* On itère sur les élements suivants *)
        aux tl (k+1) in 
          
    aux values 0

  (* Retourne le max et réorganise le heap *)
  let pop = fun heap_struct comp -> 
    
    let (id, _, _) = heap_struct.heap.(0) in 

    (* permutte les elements *)
    permut heap_struct 0 (!(heap_struct.active_size)-1); 

    (* On diminue la taille active *) 
    heap_struct.active_size := !(heap_struct.active_size) - 1; 

    (* on reforme le heap avec un element en moins*)
    down heap_struct comp; 

    (* On pop, id - 1 pour palier au décalge d'indice dans le tas *)
    id

  let print_heap = fun heap_struct -> 

    Printf.printf "\nHEAP \n"; 
    Array.iter (fun (a,b,c) -> Printf.printf "%d : %d, %d\n" a b c) heap_struct.heap; 
    Printf.printf "\nASSOC \n"; 
    Array.iter (Printf.printf "%d") heap_struct.assoc 

  let update = fun heap_struct id new_dsat comp -> 

    (* on récupère les éléments d'information *)
    let position = heap_struct.assoc.(id) in 
    let (id, dsat, d) = heap_struct.heap.(position) in 
    let len = !(heap_struct.active_size)-1 in 

    (* Modifier la valeur *) 
    heap_struct.heap.(position) <- (id, new_dsat, d); 

    (* Fonction recursive down *)
    let rec down = fun i -> 

      if 2*i+2 <= len then 
        begin
          let current = heap_struct.heap.(i) in 
          let left = heap_struct.heap.(2*i+1) in 
          let right = heap_struct.heap.(2*i+2) in

          (* On peut progresser vers la gauche et la droite est non vide *)
          if comp left current then begin
            permut heap_struct i (2*i+1);
            down (2*i+1);
          end

          (* On peut progresser vers la droite et la gauche est non vide *)
          else if comp right current then begin
            permut heap_struct i (2*i+2);
            down (2*i+2);
          end
        end 
      
      else if 2*i+1 = len then 
        begin 
          let current = heap_struct.heap.(i) in 
          let left = heap_struct.heap.(2*i+1) in 
          if comp left current then permut heap_struct i (2*i+1);
        end

      (* L'élement ne peut ni progresser à droite ni à gauche alors il est à sa place *)
      else () in 
    
    (* fonction recursive up *)
    let rec up = fun i -> 

      if i=0 then () 
      else 

        let index_pere = (i-1)/2 in 

        let current = heap_struct.heap.(i) in 
        let pere = heap_struct.heap.(index_pere) in 

        if comp current pere then 
          begin
            permut heap_struct i index_pere;
            up index_pere;
          end
        
        else () in 
    
    (* On monte ou on descend ? *)
    if comp (id, new_dsat, 1) (id, dsat, 1) then up position   
    else if comp (id, dsat, 1) (id, new_dsat, 1) then down position 
    else () 

  let is_empty = fun heap_struct -> 
    !(heap_struct.active_size) = 0

    
end 
