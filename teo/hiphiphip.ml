module Heap = struct 

  type heap_struct = {
    heap: (int*int) array; 
    assoc: int array; 
    mutable size: int ref 
  }

  (* permutation de deux élements *)
  let permut = fun heap_struct i j ->

    (* permutation dans le tas *)
    let change = heap_struct.heap.(i) in 
    heap_struct.heap.(i) <- heap_struct.heap.(j);
    heap_struct.heap.(j) <- change;

    (* permutation des valeurs dans la table d'association *)
    let (id1, v1) = heap_struct.heap.(i) in 
    let (id2, v2) = heap_struct.heap.(j) in  

    let tmp = heap_struct.assoc.(id1) in 
    heap_struct.assoc.(id1) <- heap_struct.assoc.(id2);
    heap_struct.assoc.(id2) <- tmp;

    (* retourne unit *)
    () 

  (* n est l'indice de la premiere case libre *)
  let up = fun heap_struct x -> 

    (* nombre d'éléments du tas *)
    let n = !(heap_struct.size) in 
    heap_struct.heap.(n) <- x;

    let rec aux = fun i -> 

      if i=1 then () 
      else 

        let pere = i/2 in 

        let (id, v) = heap_struct.heap.(i) in 
        let (idp, vp) = heap_struct.heap.(pere) in 

        Printf.printf "(%d %d) et (%d %d)" id v idp vp; 

        if v > vp then 
          begin
            permut heap_struct i pere;
            aux pere;
          end
        
        else () 

      in 
    aux n
  
  let down = fun heap_struct x ->

    let n = !(heap_struct.size) in 
    heap_struct.heap.(1) <- x;
  
    let rec aux = fun i -> 
  
      let (id, v) = heap_struct.heap.(i) in 
      let (idl, vl) = heap_struct.heap.(2*i) in 
      let (idr, vr) = heap_struct.heap.(2*i+1) in
  
      (* On peut progresser vers la gauche et la droite est non vide *)
      if v < vl && 2*i+1 <= n then begin
        permut heap_struct i (2*i);
        aux (2*i);
      end

      (* On peut progresser vers la droite et la gauche est non vide *)
      else if 2*i+1 <= n && v < vr then begin
        permut heap_struct i (2*i+1);
        aux (2*i+1);
      end

      (* L'élement ne peut ni progresser à droite ni à gauche alors il est à sa place *)
      else ()
      
    in aux 1  


  (* Initialise un heap avec une liste de valeurs *)
  let init = fun heap_struct values ->
  
    let rec aux = fun v k -> 
      match v with 
      | [] -> ()
      | hd::tl -> 
        (* On ajoute une association ID:position_heap *)
        heap_struct.assoc.(k) <- !(heap_struct.size); 

        (* On ajoute un élement au heap *)
        up heap_struct (k,hd);
        heap_struct.size := !(heap_struct.size)+1;
        
        (* On itère sur les élements suivants *)
        aux tl (k+1) in 
          
    aux values 1

  (* Retourne le max et réorganise le heap *)
  let pop = fun heap_struct -> 
    
    let hd = heap_struct.heap.(1) in 
    let tl = !(heap_struct.size)-1 in 

    (* on reforme le heap avec un element en moins*)
    down heap_struct heap_struct.heap.(tl); 
    heap_struct.size := tl; 

    (* On pop*)
    hd 

  let print_heap = fun heap_struct -> 

    Printf.printf "\nHEAP \n"; 
    Array.iter (fun (a,b) -> Printf.printf "%d : %d\n" a b) heap_struct.heap; 
    Printf.printf "\nASSOC \n"; 
    Array.iter (Printf.printf "%d") heap_struct.assoc; 
    Printf.printf "\n\nSIZE : %d \n" !(heap_struct.size)

  let get_value = fun heap_struct id -> 

    let position = heap_struct.assoc.(id) in 
    let (id_found, value) = heap_struct.heap.(position) in 
    value

  let update = fun heap_struct id new_value -> 

    (* on récupère les éléments d'information *)
    let position = heap_struct.assoc.(id) in 
    let (id, value) = heap_struct.heap.(position) in 
    let n = !(heap_struct.size) in 

    (* Modifier la valeur *) 
    heap_struct.heap.(position) <- (id, new_value); 

    (* Fonction recursive down *)
    let rec down = fun i -> 

      let (id, v) = heap_struct.heap.(i) in 
      let (idl, vl) = heap_struct.heap.(2*i) in 
      let (idr, vr) = heap_struct.heap.(2*i+1) in
  
      (* On peut progresser vers la gauche et la droite est non vide *)
      if v < vl && 2*i+1 <= n then begin
        permut heap_struct i (2*i);
        down (2*i);
      end

      (* On peut progresser vers la droite et la gauche est non vide *)
      else if 2*i+1 <= n && v < vr then begin
        permut heap_struct i (2*i+1);
        down (2*i+1);
      end

      (* L'élement ne peut ni progresser à droite ni à gauche alors il est à sa place *)
      else () in 
    
    (* fonction recursive up *)
    let rec up = fun i -> 

      if i=1 then () 
      else 

        let pere = i/2 in 

        let (id, v) = heap_struct.heap.(i) in 
        let (idp, vp) = heap_struct.heap.(pere) in 

        Printf.printf "(%d %d) et (%d %d)" id v idp vp; 

        if v > vp then 
          begin
            permut heap_struct i pere;
            up pere;
          end
        
        else () in 
    
    (* On monte ou on descend ? *)
    if new_value > value then up position 
    else if new_value < value then down position 
    else () 
    
end 

open Heap 

let () = 

  (* les valeurs associées aux ID des noeuds : c'est à dire les dsat*)
  let values = [2;3;8;95;56;87;09;124;11] in
  let n = List.length values in 

  Printf.printf "\nflag 1\n"; 

  (* Initialisation de la structure heap *)
  let hs = {
    heap=Array.make (n+1) (-1, -1); 
    assoc=Array.make (n+1) (-1); 
    size= ref 1
  } in 

  Printf.printf "\nflag 2\n"; 

  (* Initialisation de la structure heap avec les bonnes valeurs *) 
  Heap.init hs values;

  Printf.printf "\nflag 3\n"; 

  (* Affichage débug *)
  Heap.print_heap hs; 

  Printf.printf "\nflag 4\n"; 

  (* Pop *) 
  let (id, value) = Heap.pop hs in 
  Printf.printf "\npoped element : id %d, value %d\n" id value;
  
  Printf.printf "\nflag 5\n"; 
  
  (* Evolution de la structure de donnée *)
  Heap.print_heap hs;   

  (* Récurer un élement du heap et afficher la valeur associée *) 
  Printf.printf "\nflag 6\n"; 

  Printf.printf "\nValue found %d\n" (Heap.get_value hs 3); 

  (* Update la valeur d'une case et donc la structure heap toute entière *)
  Printf.printf "\nflag 7\n"; 

  Heap.update hs 3 58; 

  Heap.print_heap hs


