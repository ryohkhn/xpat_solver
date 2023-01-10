open State
open Card

let compare_state a b =
  if(Stdlib.compare a.history b.history = 0) then
    0
  else
    let a = State.cpy_state_nohist a in
    let b = State.cpy_state_nohist b in
    let comp = Stdlib.compare a.registres b.registres in
    if (comp = 0) then
      Stdlib.compare a.colonnes b.colonnes
    else comp


module States = Set.Make (struct type t = state let compare = compare_state end)


let print_states states =
  Printf.printf "Taille %d\n" (List.length (States.elements states))


let get_score state =
  List.fold_left (+) 0 (state.depot)


let legal_moves_to_registers state =
  let rec free_registres state n =
    if n = state.nbReg then false
    else if (FArray.get (Option.get state.registres) n) = None then
      true
    else free_registres state (n+1)
  in
  if(free_registres state 0) then
    (*Iterate column heads for possible moves towards registry*)
    let rec reg_moves state n acc =
       if n = state.nbCol then acc
       else
         if FArray.get state.colonnes n = [] then
           reg_moves state (n+1) acc
         else
           let card = (List.hd(FArray.get state.colonnes n)) in
           let move = (string_of_int card) ^ " T" in
            reg_moves state (n+1) (move::acc)
      in reg_moves state 0 []
  else []


let legal_moves_to_empty state list game=
  (* In midnight oil and baker's dozen no moves to empty column *)
  if (game = "BakersDozen" || game = "bd") then list else
  if (game = "MidnightOil" || game = "mo") then list else
  let rec free_columns state n =
     if n = state.nbCol then false
     else if FArray.get state.colonnes n = [] then true
     else free_columns state (n+1)
  in
  if(free_columns state 0) then
    let rec empty_col_moves state n acc =
      if n = state.nbCol then acc
      else
        if List.length (FArray.get state.colonnes n) < 2
        then empty_col_moves state (n+1) acc
        else
          let card = (List.hd(FArray.get state.colonnes n)) in
          (* In seahaven no moves to empty column except if the card is a king *)
          if ( (game = "Seahaven" || game = "st" ) && fst(of_num card) <> 13) then
          empty_col_moves state (n+1) acc
          else
          let move = (string_of_int card) ^ " V" in
          empty_col_moves state (n+1) (move::acc)
    in empty_col_moves state 0 list
  else list


let legal_column_moves state list game =
  let rec column_moves state n acc =
     if n = state.nbCol then acc
     else if FArray.get state.colonnes n = [] then column_moves state (n+1) acc
     else
       let card = (List.hd(FArray.get state.colonnes n)) in
       let rec moves_aux state x card acc2 =
         if x = state.nbCol then acc2
         else if FArray.get state.colonnes x = [] then moves_aux state (x+1) card acc2
         else
           let dest_card =  (List.hd(FArray.get state.colonnes x)) in
           if card = dest_card then moves_aux state (x+1) card acc2
           else
             let move = (string_of_int card) ^ " " ^
                          (string_of_int dest_card) in
             let result =
               Check.verify_move (string_of_int card) (string_of_int dest_card) game
             in
             let new_acc =
               if result then (move::acc2)
               else acc2
             in moves_aux state (x+1) card new_acc
       in column_moves state (n+1) (moves_aux state 0 card acc)
  in column_moves state 0 list


let legal_moves state game =
  let list =
    legal_moves_to_registers state in
    legal_column_moves state (legal_moves_to_empty state list game) game


let rec legal_moves_to_states seen_states possible_states initial_state moves =
  match moves with
  | [] -> possible_states
  | x::moves' ->
    let x',y' = Check.split_move x in
    let new_state = Check.process_move_unchecked initial_state x' y' in
    let new_state = Check.normalise new_state in
    let new_state = Check.add_to_history new_state x in
    if not(States.mem new_state seen_states) then
      let possible_states = States.add new_state possible_states in
      legal_moves_to_states seen_states possible_states initial_state moves'
    else
        legal_moves_to_states seen_states possible_states initial_state moves'


let get_biggest_score possible_states =
  States.fold
    (fun x acc ->
       if get_score acc > get_score x then
         acc
       else
         x
    )
    possible_states State.empty_state


let rec solve' state game seen_states possible_states =
  let new_state = get_biggest_score possible_states in
  let moves = legal_moves new_state game in
  let possible_states = States.remove new_state possible_states in
  let possible_states =
    legal_moves_to_states seen_states possible_states new_state moves
  in
  let seen_states =
    States.add new_state seen_states
  in
  if get_score new_state = 52 then
    Some new_state,0,seen_states
  else if States.is_empty possible_states then
    None,1,seen_states
  else
    solve' state game seen_states possible_states


let solve_diff state game seen_states possible_states diff =
  let rec solve_diff' state game seen_states possible_states diff max =
    let new_state = get_biggest_score possible_states in
    let max = Stdlib.max (get_score new_state) max in
    let moves = legal_moves new_state game in
    let possible_states = States.remove new_state possible_states in
    let possible_states = legal_moves_to_states seen_states possible_states new_state moves in
    let possible_states,removed_states =
      States.partition (fun s -> get_score s > (max-diff)) possible_states
    in
    let seen_states = States.union removed_states seen_states in
    let seen_states = States.add new_state seen_states
    in
    if get_score new_state = 52 then
      Some new_state,0,seen_states
    else if States.is_empty possible_states then
      None,1,seen_states
    else
      solve_diff' state game seen_states possible_states diff max
  in
  solve_diff' state game seen_states possible_states diff 0


let solve state game diff =
  let state = Check.normalise state in
  let seen_states = States.empty in
  let possible_states = States.(empty |> add state) in
  let state,re_val,seen =
    if (fst diff) then
      solve_diff state game seen_states possible_states (snd diff)
    else
      solve' state game seen_states possible_states
  in
  print_states seen;
  (state,re_val)
