open State
open Card


let verify_card state x =
  let card = Card.of_num x in
  let rec rec_verify_columns n =
    if n = state.nbCol then false
    else try
        if List.hd(FArray.get state.colonnes n) = x 
         then true 
        else rec_verify_columns (n+1)
      with Not_found -> rec_verify_columns (n+1)
  in
  let rec rec_verify_regs n = 
    if n = state.nbReg then rec_verify_columns 0
    else if (FArray.get (Option.get state.registres) n) = 
              Some card then 
      true
    else rec_verify_regs (n+1)
  in rec_verify_regs 0

let verify_dest state x y game =
  let rec rec_verify_columns n =
    if n = state.nbCol then false
    else try
        if y = "V" && FArray.get state.colonnes n = [] then 
          if game = "Seahaven" && fst(Card.of_num x) <> 13 then false
          else true
        else if y <> "V" &&
                  List.hd(FArray.get state.colonnes n) = int_of_string y 
        then true 
        else rec_verify_columns (n+1)
      with Not_found -> rec_verify_columns (n+1)
  in
  let rec rec_verify_regs n = 
    if n = state.nbReg then false
    else if (FArray.get (Option.get state.registres) n) = None
    then true
    else rec_verify_regs (n+1)
  in 
  if y = "T" then rec_verify_regs 0
  else rec_verify_columns 0


(* TODO Utiliser FArray.exists ? *)
let pop_card state x =
  let rec rec_pop_cols n =
    if n = state.nbCol then state,false
    else if List.hd(FArray.get state.colonnes n) = (int_of_string x) then
       { colonnes = FArray.set state.colonnes n 
                         (List.tl(FArray.get state.colonnes n));
            registres = state.registres ;
            depot = state.depot ;
            nbCol = state.nbCol ; nbReg = state.nbReg},true
    else rec_pop_cols (n+1)
  in
  let rec rec_pop_regs n =
    if  n = state.nbReg then rec_pop_cols 0
    else if FArray.get (Option.get state.registres) n =
               Some (Card.of_num (int_of_string x))
    then 
        { colonnes = state.colonnes;
          registres = Some (FArray.set
                              (Option.get state.registres) n None) ;
          depot = state.depot ;
          nbCol = state.nbCol ; nbReg = state.nbReg},true
    else rec_pop_regs (n+1)
  in rec_pop_regs 0


let push_card state x y =
  let card = Card.of_num x in
  if y = "T" then  
    let rec rec_push_regs n =
      if  n = state.nbReg then None
      else if (FArray.get (Option.get state.registres) n) = None
      then Some
        { colonnes = state.colonnes;
          registres = Some (FArray.set
                              (Option.get state.registres) n (Some card)) ;
          depot = state.depot ;
          nbCol = state.nbCol ; nbReg = state.nbReg}
      else rec_push_regs (n+1)
    in rec_push_regs 0
  else
  let rec rec_push_columns n =
    if n = state.nbCol then state
    else try
        let col = FArray.get state.colonnes n in
        if y = "V" && col = [] then
          { colonnes = FArray.set state.colonnes n [x];
            registres = state.registres ;
            depot = state.depot ;
            nbCol = state.nbCol ; nbReg = state.nbReg}
        else if y <>"V" && List.hd col = int_of_string y
        then 
          { colonnes = FArray.set state.colonnes n 
                         (x::(FArray.get state.colonnes n));
            registres = state.registres ;
            depot = state.depot ;
            nbCol = state.nbCol ; nbReg = state.nbReg}
        else rec_push_columns (n+1)
      with Not_found -> rec_push_columns (n+1)
  in Some (rec_push_columns 0)


let process_move state x y game=
  Printf.printf "Processing move %s -> %s\n"
    (Card.to_string (Card.of_num (int_of_string x)))
    y ;
  if verify_card state (int_of_string x) = false then (state,false)
  else if verify_dest state (int_of_string x) y game = false then (state,false)
  else
    let tmp_state,_ = pop_card state x in
    let new_state = push_card tmp_state (int_of_string x) y
    in
    (Option.get new_state,true)

let different_colors card_src card_dest =
  match (snd(card_src),snd(card_dest)) with
    (Trefle,Pique) | (Pique, Trefle) 
                     | (Carreau, Coeur) 
                     | (Coeur, Carreau) -> false
    | (x,y) -> if x = y then false else true

let verify_move x y =
  if y = "V" || y = "T" then true
  else
    let card_src = Card.of_num (int_of_string x) in
    let card_dest = Card.of_num (int_of_string y) in

    if different_colors card_src card_dest then false
    else fst(card_src) = fst(card_dest) + 1



(* TODO normalise state depots *)
(* Algo idea:
   check all registers for possible depot moves

   in a loop ->
      check all column heads for possible depot moves
      if none possible -> end of loop
*)
let normalise state =
  let rec normalise' state depot count = match depot with
    | color::depot' ->
      let new_state,bool = pop_card state (string_of_int color) in
      let normalise_rec count' = normalise' new_state depot' count' in
      if bool then
        normalise_rec (count+1)
      else
        normalise_rec count
    | [] ->
      if count = 0 then
        state
      else
        normalise' state state.depot 0
  in
  normalise' state state.depot 0



let check file start game =
  let in_ch = open_in file in
  let rec read_line state n =
    let line = try input_line in_ch with End_of_file -> ""
    in
    let x,y =
      match String.split_on_char ' ' line with
      | [string1;string2] -> (string1,string2)
      | _ ->  "",""
    in
    if x = "" || not (verify_move x y) then (Some state),n
    else
    let new_state, result = process_move state x y game
    in
    let _ = Printf.printf "New State : \n %s \n" (state_to_string state) 
    in
    if result = false || state = new_state then
      (None,n)
    else
      let normalised_state = normalise state in
      read_line normalised_state (n+1);
  in
  read_line (normalise start) 0


(* What's left:

 -> Normaliser les depots

 -> Verify that state is complete at the end

 -> Tester

 -> Clean up

 -> Commenter

*)
