type state = { colonnes : (Card.card Fifo.t) FArray.t;
               registres : (Card.card option) FArray.t option;
               depot : int list ;
               nbCol : int ; nbReg : int }

let split_list n list =
  let rec split_rec n l acc =
    match n,l with
    | 0,_ | _,[] -> List.rev acc, l
    | n,h::t -> split_rec (n-1) t (h::acc) 
  in split_rec n list []

let fill_fifo list fifo =
  let rec fill fifo list =
    match list with
      [] -> fifo
    | h::t -> fill (Fifo.push (Card.of_num h) fifo) t
  in fill fifo list

let fill_col cols perm game =
  let f = 
    match game with
    | "FreeCell"->  (fun x -> if (x mod 2 = 0) then 7 else 6) 
    | "Seahaven" ->  (fun x -> 5) 
    | "MidnightOil" -> (fun x -> 3)
    | "BakersDozen" -> (fun x -> 4)
    | _ -> raise Not_found
  in
  let rec fill_aux cols x p =
       if x = FArray.length cols then cols,p
       else
         let curr,rest = split_list (f x) p in
         let fifo = fill_fifo curr (FArray.get cols x) in
         let cols = FArray.set cols x fifo in
         fill_aux cols (x+1) rest
  in fill_aux cols 0 perm

  

let state_init x y perm game =
  let col, rest = fill_col (FArray.make x Fifo.empty) perm game in
  let reg =  
    if y = 0 then None 
    else Some (FArray.make y None) in
  (* TODO FILL REGISTRE SEAHAVEN *)
    (*else fill_reg ( (FArray.make y None) rest) in*)
  let depot = [0;0;0;0] in
  let s = { colonnes =  col ; registres = reg ; 
  depot = depot ; nbCol = x ; nbReg = y } 
  in s

let create_state game perm = 
  match game with 
  | "FreeCell"->  state_init 8 4 perm game
  | "Seahaven" ->  state_init 10 4 perm game
  | "MidnightOil" -> state_init 18 0 perm game
  | "BakersDozen" -> state_init 13 0 perm game
  | _ -> raise Not_found

(* Print functions *)

let une_colonne_to_string colonne =
    let rec aux l = 
        match l with
        | [] -> "\n"
        | h :: t -> (Card.to_string h) ^ " " ^ (aux t)
    in
    aux (Fifo.to_list colonne)

let cols_to_string cols =
  "\nColonnes : \n" ^
    let rec str_aux cols x =
        if x = FArray.length cols then "\n"
        else "Colonne " ^ string_of_int (x+1) ^ " : " ^
                let rec one_col l = 
                  match l with
                  | [] -> "\n"
                  | h::t -> (Card.to_string h) ^ " " ^ (one_col t)
                in
                one_col (Fifo.to_list ((FArray.get cols x)))
               ^ str_aux cols (x+1) 
    in str_aux cols 0

let reg_to_string regs =
  if regs = None then "Pas de registres\n" else
  "Registres : \n" ^
     let rec str_aux regs x =
         if x = FArray.length regs then "\n"
         else "Registre " ^ string_of_int (x+1) ^ " : " ^ 
                if x = FArray.length regs then "\n"
                else let entry = FArray.get regs x
                     in if entry = None then "empty\n" ^ str_aux regs (x+1) 
                        else Card.to_string (Option.get entry) ^ "\n" ^ str_aux regs (x+1) 
    in str_aux (Option.get regs) 0


let dep_to_string dep =
    let trefle,carreau,coeur,pique = 
      match dep with
        trefle::carreau::coeur::[pique] -> trefle,carreau,coeur,pique
      | _ -> raise Not_found
    in
  "Depot : " ^
    string_of_int trefle ^ " trefle ; " ^
    string_of_int carreau ^ " carreau ; " ^
    string_of_int coeur ^ " coeur ; " ^
    string_of_int pique ^ " pique \n" 


let state_to_string state =
  cols_to_string state.colonnes 
  ^ reg_to_string state.registres
  ^ dep_to_string state.depot
