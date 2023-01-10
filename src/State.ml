(* type state represents an arrangement of the game *)
type state = { colonnes : (int list) FArray.t;
               registres : (Card.card option) FArray.t option;
               depot : int list ;
               nbCol : int ; nbReg : int ;
               history : string list option }

(* get a state's depot *)
let depot state = state.depot

(* split a list into two at index n*)
let split_list n list =
  let rec split_rec n l acc =
    match n,l with
    | 0,_ | _,[] -> List.rev acc, l
    | n,h::t -> split_rec (n-1) t (h::acc)
  in split_rec n list []

(* move kings in BakersDozen according to the rules *)
let move_kings perm game =
  if game <> "BakersDozen" then
    perm
  else
    let kings,rest = List.partition (fun card -> fst(Card.of_num card) = 13 ) perm in
    List.rev_append (List.rev kings) rest

(* fill a starting_state's columns *)
let fill_col cols perm game =
  let f = match game with
    | "FreeCell" -> (fun x -> if (x mod 2 = 0) then 7 else 6)
    | "Seahaven" -> (fun x -> 5)
    | "MidnightOil" -> (fun x -> 3)
    | "BakersDozen" -> (fun x -> 4)
    | _ -> raise Not_found
  in

  let rec fill_aux cols x p =
    if x = FArray.length cols then cols,p
    else
      let curr,rest = split_list (f x) p in
      let curr = move_kings curr game in
      let cols = FArray.set cols x (List.rev curr) in
      fill_aux cols (x+1) rest
  in fill_aux cols 0 perm

(* fill a starting_state's regisres *)
let fill_reg regs perm =
  let () = Printf.printf "Filling regs\n" in
  match perm with
    a::[b] ->
    FArray.set (FArray.set regs 0 (Some (Card.of_num a)))
      2 (Some(Card.of_num b))
  | _ -> failwith "Error"


(* initialise a state according to it's game,
   starting permutation, column and registry numbers *)
let state_init x y perm game =
  let col, rest = fill_col (FArray.make x []) perm game in
  let reg =
    if y = 0 then None
    else if game = "Seahaven" then
      Some ( fill_reg (FArray.make y None) rest )
    else Some (FArray.make y None)
  in
  let depot = [0;0;0;0] in
  let s = { colonnes =  col ; registres = reg ;
  depot = depot ; nbCol = x ; nbReg = y ; history = None }
  in s

(* create state with game and starting permutation *)
let create_state game perm =
  match game with
  | "FreeCell"->  state_init 8 4 perm game
  | "Seahaven" ->  state_init 10 4 perm game
  | "MidnightOil" -> state_init 18 0 perm game
  | "BakersDozen" -> state_init 13 0 perm game
  | _ -> raise Not_found

(* returns empty state *)
let empty_state =
  { colonnes = FArray.make 1 []; registres = None;
  depot = [0]; nbCol = 0 ; nbReg = 0 ; history = None }

(* takes a state and returns the same state with history = None *)
let cpy_state_nohist state =
  { colonnes = state.colonnes; registres = state.registres;
  depot = state.depot; nbCol = state.nbCol ; nbReg = state.nbReg ; history = None }



(* Print functions *)

(* takes a one columns and returns its string description *)
let une_colonne_to_string colonne =
    let rec aux l =
        match l with
        | [] -> "\n"
        | h :: t -> (Card.to_string h) ^ " " ^ (aux t)
    in
    aux (Fifo.to_list colonne)

(* takes a state's columns and returns its string description *)
let cols_to_string cols =
  "\nColonnes : \n" ^
    let rec str_aux cols x =
        if x = FArray.length cols then "\n"
        else "Colonne " ^ string_of_int (x+1) ^ " : " ^
                let rec one_col l =
                  match l with
                  | [] -> "\n"
                  | h::t -> (Card.to_string (Card.of_num h))
                            ^ " " ^ (one_col t)
                in
                one_col (List.rev (FArray.get cols x))
               ^ str_aux cols (x+1)
    in str_aux cols 0

(* takes a state's registers and returns its string description *)
let reg_to_string regs =
  if regs = None then "Pas de registres\n\n" else
  "Registres : \n" ^
     let rec str_aux regs x =
         if x = FArray.length regs then "\n"
         else "Registre " ^ string_of_int (x+1) ^ " : " ^
                if x = FArray.length regs then "\n"
                else let entry = FArray.get regs x
                     in if entry = None then "empty\n" ^ str_aux regs (x+1)
                        else Card.to_string (Option.get entry) ^ "\n" ^ str_aux regs (x+1)
    in str_aux (Option.get regs) 0

(* takes a state depot and returns its sum *)
let dep_to_int dep = match dep with
  | trefle::pique::coeur::[carreau] -> trefle,pique,coeur,carreau
  | _ -> raise Not_found

(* takes a state depot and returns its string description *)
let dep_to_string dep =
  let trefle,pique,coeur,carreau=dep_to_int dep in
  "Depot : " ^
    string_of_int trefle ^ " trefle ; " ^
    string_of_int pique ^ " pique ; " ^
    string_of_int coeur ^ " coeur ; " ^
    string_of_int carreau ^ " carreau \n"

(* takes a state history and returns its string description *)
let history_to_string history =
  if history = None then ""
  else
    let moves = List.fold_left ( fun x y -> x ^ "\n" ^ y ) "" (List.rev (Option.get history)) in
    String.sub moves 1 ((String.length moves) - 1) 

(* takes a state and returns its string description *)
let state_to_string state =
  cols_to_string state.colonnes
  ^ reg_to_string state.registres
  ^ dep_to_string state.depot
  ^ history_to_string state.history ^ "\n"
