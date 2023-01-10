open State
open Card


(* Verify the card is in a position to move, registry or
column head *)
let verify_card state x =
  let card = Card.of_num x in
  let rec rec_verify_columns n =
    if n = state.nbCol then
      false
    else
      try
        if List.hd(FArray.get state.colonnes n) = x
         then
           true
        else
          rec_verify_columns (n+1)
      with Failure _ | Not_found -> rec_verify_columns (n+1)
  in
  let rec rec_verify_regs n =
    if n = state.nbReg then rec_verify_columns 0
    else if (FArray.get (Option.get state.registres) n) =
              Some card then
      true
    else rec_verify_regs (n+1)
  in rec_verify_regs 0

(* Verify the destination of the move is legal *)
let verify_dest state x y game =
  let rec rec_verify_columns n =
    if n = state.nbCol then false
    else
      try
        if y = "V" && FArray.get state.colonnes n = [] then
          if game = "BakersDozen" ||
             (game = "Seahaven" && fst(Card.of_num x) <> 13)
          then
            false
          else
            true
        else if y <> "V" && List.hd(FArray.get state.colonnes n)
                            = int_of_string y then
          true
        else
          rec_verify_columns (n+1)
      with Failure _ | Not_found -> rec_verify_columns (n+1)
  in
  let rec rec_verify_regs n =
    if n = state.nbReg then false
    else if (FArray.get (Option.get state.registres) n) = None
    then true
    else rec_verify_regs (n+1)
  in
  if y = "T" then rec_verify_regs 0
  else rec_verify_columns 0


(* Return a new state where the part of the move where
we pop the x card has been executed *)
let pop_card state x =
  let rec rec_pop_cols n =
    if n = state.nbCol then state,false
    else
      try
        if List.hd(FArray.get state.colonnes n) = (int_of_string x) then
          {colonnes = FArray.set state.colonnes n
                        (List.tl(FArray.get state.colonnes n));
           registres = state.registres;
           depot = state.depot;
           nbCol = state.nbCol;
           nbReg = state.nbReg;
           history = state.history },true
        else rec_pop_cols (n+1)
      with Failure _ -> rec_pop_cols (n+1)
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
        nbCol = state.nbCol ;
        nbReg = state.nbReg ;
        history = state.history },true
    else rec_pop_regs (n+1)
  in rec_pop_regs 0

(* Return a new state where the part of the move x->y where
we push the x card has been executed *)
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
          nbCol = state.nbCol ; nbReg = state.nbReg ;
          history = state.history }
      else rec_push_regs (n+1)
    in rec_push_regs 0
  else
  let rec rec_push_columns n =
    if n = state.nbCol then state
    else
      try
        let col = FArray.get state.colonnes n in
        if y = "V" && col = [] then
          { colonnes = FArray.set state.colonnes n [x];
            registres = state.registres ;
            depot = state.depot ;
            nbCol = state.nbCol ;
            nbReg = state.nbReg ;
            history = state.history }
        else if y <> "V" && List.hd col = int_of_string y
        then
          { colonnes = FArray.set state.colonnes n
                         (x::(FArray.get state.colonnes n));
            registres = state.registres ;
            depot = state.depot ;
            nbCol = state.nbCol ;
            nbReg = state.nbReg ;
            history = state.history }
        else rec_push_columns (n+1)
      with Failure _ | Not_found -> rec_push_columns (n+1)
  in Some (rec_push_columns 0)

(* Process a move x-> in a certain state with a certain game,
first we verify that the move is legal with verify_card et verify_dest,
then we use pop_card and push_card to process the move and return
the new state *)
let process_move state x y game=
  if verify_card state (int_of_string x) = false then (state,false)
  else if verify_dest state (int_of_string x) y game = false then (state,false)
  else
    let tmp_state,_ = pop_card state x in
    let new_state = push_card tmp_state (int_of_string x) y
    in
    (Option.get new_state,true)

(* Process a move without checking whether it is legal,
 used in Solve.ml *)
let process_move_unchecked state x y =
  let tmp_state,_ = pop_card state x in
  let new_state =
    push_card tmp_state (int_of_string x) y
  in
  Option.get new_state

(* Depending on the rules of the game, returns whether
 the two cards have the right two colors to allow a move *)
let correct_colors card_src card_dest game =
  if game = "BakersDozen" then true
  else if game = "FreeCell" then
  match (snd(card_src),snd(card_dest)) with
    (Trefle,Pique) | (Pique, Trefle)
                     | (Carreau, Coeur)
                     | (Coeur, Carreau) -> false
    | (x,y) -> not (x = y)
  else match (snd(card_src),snd(card_dest)) with
    (Trefle,Pique) | (Pique, Trefle)
                     | (Carreau, Coeur)
                     | (Coeur, Carreau) -> false
    | (x,y) -> x = y

(* Verify if a move x->y is legal in a certain game,
   depending on the rules *)
let verify_move x y game =
  if y = "T" then
    true
  else if y = "V" then
    (game <> "MidnightOil" && game <> "mo")

  else
    (
    let card_src = Card.of_num (int_of_string x) in
    let card_dest = Card.of_num (int_of_string y) in

    if not (correct_colors card_src card_dest game) then
      false
    else
      fst(card_src) = fst(card_dest) - 1
    )

(* Normalise a state's depots *)
let normalise state =
  let update_state depot index =
    let trefle,pique,coeur,carreau = dep_to_int depot in
    match suit_of_num index with
    | Trefle -> (trefle+1)::pique::coeur::[carreau]
    | Pique -> trefle::(pique+1)::coeur::[carreau]
    | Coeur -> trefle::pique::(coeur+1)::[carreau]
    | Carreau -> trefle::pique::coeur::[(carreau+1)]
  in
  let rec normalise' state depot count index = match depot with
    | color::depot' ->
      let new_state,bool = pop_card state (string_of_int (to_num (color+1, suit_of_num index))) in
      let normalise_rec count' new_state' = normalise' new_state' depot' count' (index+1) in
      if bool then
        let new_state' = {
          colonnes = new_state.colonnes;
          registres = new_state.registres ;
          depot = update_state (new_state.depot) index;
          nbCol = new_state.nbCol ;
          nbReg = new_state.nbReg ;
          history = new_state.history ;
        }
        in
        normalise_rec (count+1) new_state'
      else (
        normalise_rec count state
      )
    | [] ->
      if count = 0 then
        state
      else
        normalise' state state.depot 0 0
  in
  normalise' state state.depot 0 0

(* Add a move to a state's history *)
let add_to_history state move =
  let new_history = match state.history with
    | Some h -> Some (move::h)
    | None -> Some [move]
  in
  {colonnes = state.colonnes;
   registres = state.registres;
   depot = state.depot;
   nbCol = state.nbCol;
   nbReg = state.nbReg;
   history = new_history}

let split_move line = match String.split_on_char ' ' line with
  | [string1;string2] -> (string1,string2)
  | _ ->  "",""


(* Take a file, starting permutation and game name
   open the file and read line by line
   for each line process the move, if the move is
   illegal we return (None,n) with n the move number,
   if all moves are valid then the check is successful
 *)
let check file start game =
  let in_ch = open_in file in
  let rec read_line state n =
    let line = try input_line in_ch with End_of_file -> ""
    in
    let x,y =
      split_move line
    in

    if x = "" || not (verify_move x y game) then
      (Some state),n
    else
      let new_state, result = process_move state x y game in
      let _ = Printf.printf "New State : \n %s \n" (state_to_string new_state)
      in
      if result = false || state = new_state then
        (None,n)
      else
        let normalised_state = normalise new_state in
        let _ = Printf.printf "Normalised State : \n %s \n"
                  (state_to_string normalised_state) in
        read_line normalised_state (n+1);
  in
  read_line (normalise start) 1
