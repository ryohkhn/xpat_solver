open State
open Card

(*
legal_moves state game donne les coups possibles depuis un state

dans la fonction principale:

-> un Set des etats deja vu

-> un Set des etats possibles mais pas vu (au debut: le state initial)

-> une boucle qui:

1) choisit un etat parmi le Set possible, probablement celui avec le score le plus haut

2) utilise legal_moves pour avoir toutes les etats possibles

3) parmi ces etats, un par un on decide si on les ajoute au etat possible
pour decider si on ajoute un etat, il faut etre sur que cet etat n'existe pas dans le Set deja vu
-> il faut bien le comparer avec toutes les etats deja vus ( il suffit pas de comparer les historiques )
on va utiliser:
module States = Set.Make (struct type t = state let compare = compare_state end)
alors il faut bien definir compare_state

4) si on decide d'ajouter cet etat, il faut mettre a jour l'historique -> l'historique d'etat precedent + le coup courant

5) on enleve l'etat choisit dans l'etape 1 du set possible et on le mets dans le set deja vu

on arrete la boucle dans deux cas:
l'etat de l'etape 1 est complet -> solution trouve
ou bien il n'y a plus d'etats possibles -> pas de solution


Possibles ameliorations:
"si on a déjà trouvé un état de score 30, il est en effet peu probable que les états de score moins de 20 soit encore utiles pour cette recherche
(on pourra alors paramétrer cet écart via une option du programme). Une telle "distance d'oubli". "
*)


(*
Maintenant, quelle comparaison compare_state doit-on utiliser ?
OCaml propose une comparaison générique nommée Stdlib.compare qui est souvent convenable.
Mais ici on vous propose de mettre un historique des coups dans chaque état.
Or deux historiques différents peuvent pourtant mener à des états
rigoureusement identiques à part ça (mêmes colonnes, mêmes registres, et donc mêmes dépôts).
Pour le bon fonctionnement de l'algorithme de recherche, il est alors crucial que dans ce cas compare_state réponde une égalité (code 0).
Bref, pour compare_state, au lieu de comparer deux états entiers directement via Stdlib.compare,
on pourra comparer leurs zones de registres respectives (p.ex. par Stdlib.compare),
et en cas d'égalité seulement comparer leurs zones de colonnes respectives (via un autre Stdlib.compare). Et rien de plus.
*)

let get_scores state =
  List.fold_left (+) 0 (state.depot)

let compare_state a b =
  if (get_scores a <> get_scores b) then 1 else
  if(Stdlib.compare a.history b.history = 0) then
    0
  else
    let rec compare_cols n =
      if n = a.nbCol then 0
      else
        let x = FArray.get a.colonnes n in
        if FArray.exists (fun y -> (Stdlib.compare x y = 0)) (b.colonnes) then compare_cols (n+1)
        else 1
    in
    let rec compare_regs n =
      if a.registres = None then (if b.registres = None then (compare_cols 0) else 1)
      else if n = a.nbReg then compare_cols 0
      else
        let x = FArray.get (Option.get a.registres) n in
        if FArray.exists (fun y -> (Stdlib.compare x y = 0))
             (Option.get b.registres) then compare_regs (n+1)
        else 1
    in compare_regs 0

let compare_state' a b =
  let compare_cols n =
    if n = a.nbCol then 0
    else
      let tmp = FArray.to_list a.colonnes in
      let tmp' = FArray.to_list b.colonnes in
      if List.for_all2 (fun x y -> x = y) tmp tmp' then
        0
      else
        -1
  in
  let compare_regs n =
    if a.registres <> b.registres then
      -1
    else if a.registres = b.registres || n = a.nbReg then
      compare_cols 0
    else
      let tmp = FArray.to_list (Option.get a.registres) in
      let tmp' = FArray.to_list (Option.get a.registres) in
      if List.for_all2 (fun x y -> (Card.to_num' x)=(Card.to_num' y)) tmp tmp' then
        0
      else
        -1
  in
  if(Stdlib.compare a.history b.history = 0) then
    0
  else if (get_scores a <> get_scores b) then
    -1
  else
    compare_regs 0


module States = Set.Make (struct type t = state let compare = compare_state' end)


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
    legal_moves_to_empty state (legal_moves_to_registers state) game
  in
  legal_column_moves state list game

let print_moves moves =
  let _ = Printf.printf "Coups possibles :\n " in
  let rec print_moves' moves =
    match moves with
      [] -> None
    | h::t -> let _ = Printf.printf "%s\n" h in print_moves' t
  in
  print_moves' moves


let print_states states =
  Printf.printf "Taille %d\n" (List.length (States.elements states))
  (*j
  Printf.printf "Liste des états vus:\n";
  let a = States.iter (fun s -> print_string (State.state_to_string s)) states in
  a *)



let print_history state =
  (*Printf.printf "Historique:\n";*)
  let hist = state.history in
  if hist = None then
    (
      print_string "hist vide\n";
    )
  else
    (
    List.iter (fun s -> print_string (s ^ "; ")) (Option.get hist);
    print_string "\n";
  )


let rec legal_moves_to_states seen_states possible_states initial_state moves =
  match moves with
  | [] -> possible_states
  | x::moves' ->
    let x',y' = Check.split_move x in
    let new_state = Check.process_move_unchecked initial_state x' y' in
    (*
    Printf.printf "Prochaine état avec le coup %s\n" x; *)

    let new_state = Check.normalise new_state in
    let new_state = Check.add_to_history new_state x in

    (*print_string (State.state_to_string new_state);*)
    (*print_history new_state;*)

    (*
    let res = compare_state initial_state new_state in
    Printf.printf "compare init et new %d\n" res;
    let _ = print_string (State.state_to_string  initial_state) in
    let _ = print_string (State.state_to_string  new_state) in
    *)
    if not(States.mem new_state seen_states) then
      let possible_states = States.add new_state possible_states in
      legal_moves_to_states seen_states possible_states initial_state moves'
    else
        legal_moves_to_states seen_states possible_states initial_state moves'

let get_biggest_score possible_states =
  States.fold
    (fun x acc ->
       if get_scores acc > get_scores x then
         acc
       else
         x
    )
    possible_states State.empty_state

let my_mem states state =
  let rec my_mem' list state = match list with
    | [] -> false
    | el::list' -> if (compare_state' el state) = 0 then true else my_mem' list' state
  in
  my_mem' (States.elements states) state

let my_remove state states =
  let rec my_remove' list acc = match list with
    | [] -> acc
    | el::list' -> if (compare_state' el state) = 0 then
        my_remove' list' acc
      else
        my_remove' list' (States.add el acc)
  in
  my_remove' (States.elements states) States.empty


let rec solve' state game seen_states possible_states =
  (* récursion sur tous les états possibles *)
  let rec solve'' seen_states possible_states =
    (* si n'il y a plus d'états possibles on renvoit None *)
    if States.is_empty possible_states then
      None,1,seen_states
    else
      (* on choisit l'état avec le score le plus élevé *)
      let new_state = get_biggest_score possible_states in
      (* on retire l'état précédent de la liste des états possibles *)
      (* let _ = Printf.printf "\n***\nAvant : " in
      let _ = print_states possible_states in *)
      let a' = my_mem possible_states new_state in
      let possible_states = my_remove new_state possible_states in
      (* let possible_states = States.remove new_state possible_states in
      let _ = Printf.printf "Apres : " in
      let _ = print_states possible_states in *)

      (* Printf.printf "Etat avec le plus grand score = %d\n" (get_scores new_state); *)
      (* on appelle récursivement solve' sur cet état *)
      (* print_string "Nouvel état :*****************************************************\n";
         print_string (State.state_to_string new_state); *)
      let ret_state,value,seen_states = solve' new_state game seen_states possible_states in
      (* si ce chemin n'a donné aucun résultat on appelle solve'' sur l'état possible suivant *)
      if ret_state = None then
        solve'' seen_states possible_states
      else
        ret_state,value,seen_states
  in
  (* Si l'état est terminé alors on quitte le solveur *)
  if get_scores state = 52 then
    Some state,0,seen_states
  else
    (* on ajoute aux états vus l'état courant *)
    let seen_states = States.add state seen_states in
    (*print_history state;*)
    (* on récupère tous les coups possibles *)
    let moves = legal_moves state game in
    (* let _ = print_moves moves in *)
    (* on ajoute aux états possibles les états ne faisant pas partie des états vus *)
    let possible_states =
      legal_moves_to_states seen_states possible_states state moves
    in
    (* print_states seen_states; *)
    (* Si aucune prochaine branche est possible on arrête *)
    if States.is_empty possible_states then
      None,1,seen_states
    else
      solve'' seen_states possible_states



let solve state game =
  let state = Check.normalise state in
  let seen_states = States.empty in
  let possible_states = States.(empty |> add state) in
  let state,re_val,seen = solve' state game seen_states possible_states in
  print_states seen;
  (state,re_val)
