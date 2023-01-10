(** In Xpat2, the index of the game is a seed used to shuffle
    pseudo-randomly the cards.
    The shuffle function emulates this permutation generator.
    The input number is the seed (between 1 and 999_999_999).
    The output list is of size 52, and contains all numbers in 0..51
    (hence without duplicates).

*)

(* The numbers manipulated below will be in [0..randmax[ *)
let randmax = 1_000_000_000


(*Converting an integer n in [0..randmax[ to an integer in [0..limit[ *)
let reduce n limit =
  Int.(of_float (to_float n /. to_float randmax *. to_float limit))


(* Crée une liste de 55 paires selon une graine *)
(** Fonction récursive interne :
  @param comp1 première composante du couple
  @param comp2 seconde composante du couple
  @param lastComp seconde composante précédente
  @param list liste finale
  @param size nombre de paires créées *)
let paires seed =
  let rec paires' comp1 comp2 lastComp list size =
    let paire_fun newComp2 = paires' ((comp1 + 21) mod 55)
        newComp2 comp2 ((comp1,comp2)::list) (size + 1)
    in
    if size = 55 then
      list
    else if comp1 = 0 && comp2 = seed then
      paire_fun 1
    else
      let newComp2 =
        if (comp1 = 21 && comp2 = 1) || (comp2 <= lastComp) then
          lastComp - comp2
        else
          lastComp - comp2 + randmax
      in
      paire_fun newComp2
  in
  (* Cas de base, première composante à 0 et seconde composante à seed *)
  paires' 0 seed 0 [] 0


(* Transforme une liste de paires en une liste de leurs secondes composantes *)
let listOfCouple paires =
  let rec listOfCouple' paires list = match paires with
    | (comp1,comp2)::l2 -> listOfCouple' l2 (comp2::list)
    | _ -> list
  in
  List.rev (listOfCouple' paires [])


(* Effectue n tirage(s) sur les FIFO *)
let tirage file1 file2 n =
  let rec tirage' file1 file2 n d =
    if n = 0 then d,file1,file2
    else
      let (a,file1) = Fifo.pop file1 in
      let (b,file2) = Fifo.pop file2 in
      let d =
        if b <= a then
          a - b
        else
          a - b + randmax
      in
      tirage' (Fifo.push b file1) (Fifo.push d file2) (n - 1) d
  in
  tirage' file1 file2 n 0


(* Transforme deux files en liste de permutations *)
let listReduced file1 file2 liste =
  let rec listReduced' file1 file2 liste res =
    if (List.length liste) = 0 then
      res
    else
      let d,file1,file2 = tirage file1 file2 1 in
      let d' = reduce d (List.length liste) in
      let permutation = List.nth liste d' in
      let liste' = List.filteri (fun i _ -> i<>d') liste in
      listReduced' file1 file2 liste' (permutation::res)
  in
  listReduced' file1 file2 liste []


let shuffle n =
  (* Trie une liste de paires selon leurs premières composantes *)
  let pairesList =
    List.sort (fun (comp1,_) (comp1',_) -> compare comp1 comp1') (paires n)
  in

  (* Liste de paires séparée entre les 24 premières paires et les 31 suivantes*)
  let firstSubList,secondSubList =
    State.split_list 24 pairesList
  in

  (* Listes des premières composantes des couples *)
  let firstSubList,secondSubList =
    listOfCouple firstSubList,listOfCouple secondSubList
  in

  let f1_init = Fifo.of_list secondSubList in
  let f2_init = Fifo.of_list firstSubList in

  let _,f1_165,f2_165 = tirage f1_init f2_init 165 in
  let permutation_graine_n =
    listReduced f1_165 f2_165 (List.init 52 (fun x -> x))
  in

  permutation_graine_n
