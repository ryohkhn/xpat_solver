open XpatLib

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode; mutable diff: bool*int}
let config = { game = Freecell; seed = 1; mode = Search ""; diff = (false,0)}

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found

let game_to_string = function
  | Freecell -> "FreeCell"
  | Seahaven -> "Seahaven"
  | Midnight -> "MidnightOil"
  | Baker -> "BakersDozen"

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")

let set_diff_value value =
  try
    let tmp = int_of_string value in
    if tmp <= 0 then raise (Invalid_argument("Error: -diff <value> needs to be at least 1"));
    config.diff <- (true,tmp)
  with
  | Invalid_argument i -> failwith(i)
  | _ -> failwith ("Error: -diff <value> needs to be an int")


let winning_state state =
  let depots =  State.depot state in
  depots = [13;13;13;13]

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  (*
  Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut; *)
  print_newline ();
  Printf.printf "Game %s with seed %d \n"
    (game_to_string conf.game) (conf.seed) ;
  print_newline ();
  let s = State.create_state (game_to_string conf.game) permut in
  Printf.printf "État initial de la partie \n%s" (State.state_to_string s) ;

  match conf.mode with
  | Check x ->
    let res,n = (Check.check x s (game_to_string conf.game)) in
    if res = None || Option.get res = s then Printf.printf "ECHEC %d" n
    else if winning_state (Option.get res) then Printf.printf "SUCCES"
    else Printf.printf "ECHEC %d" n
       ; exit n
  | Search x ->
    let res,n = (Solve.solve s (game_to_string conf.game) config.diff) in
    if n = 1 then Printf.printf "INSOLUBLE"
    else let _ = Printf.printf "SUCCES" in
      let oc = open_out x in
      Printf.fprintf oc "%s\n" (State.history_to_string (Option.get res).history);
      ; exit n


let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file");
       ("-diff", String (fun filename -> set_diff_value filename),
        "<value>:\tValue of the difference.
                        Possible states with a difference of <value> from the maximum score achieved will be deleted.
                        The results are less exhaustive and generally tend towards no solution.")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config

let _ = if not !Sys.interactive then main () else ()


