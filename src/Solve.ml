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
let compare_state a b=
  0

module States = Set.Make (struct type t = state let compare = compare_state end)


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

let legal_moves_to_empty state list=
  let rec free_columns state n =
     if n = state.nbCol then false
     else if FArray.get state.colonnes n = [] then true
     else free_columns state (n+1)
  in 
  if(free_columns state 0) then
    let rec empty_col_moves state n acc =
      if n = state.nbCol then acc
      else
        if FArray.get state.colonnes n = [] then
          empty_col_moves state (n+1) acc
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
  legal_moves_to_empty state (legal_moves_to_registers state) in
  legal_column_moves state list game

let rec print_moves moves =
  match moves with
    [] -> None
  | h::t -> let _ = Printf.printf "%s\n" h in print_moves t


let solve state game =
  let oldStates = States.empty in
  let possibleStates = States.(empty |> add state) in

  let moves = legal_moves state game in
  let _ = print_moves moves in
  None,0
