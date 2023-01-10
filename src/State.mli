type state = { colonnes : (int list) FArray.t;
               registres : (Card.card option) FArray.t option;
               depot : int list ;
               nbCol : int ; nbReg : int ; history : string list option
	}

(* get a state's depot *)
val depot : state -> int list

(* split a list into two at index n*)
val split_list : int -> 'a list -> 'a list * 'a list

(* move kings in BakersDozen according to the rules *)
val create_state : string -> int list -> state

(* returns empty state *)
val empty_state : state

(* takes a state and returns the same state with history = None *)
val cpy_state_nohist : state -> state

(* takes a state history and returns its string description *)
val history_to_string : string list option -> string

(* takes a state and returns its string description *)
val state_to_string : state -> string

(* takes a state depot and returns its sum *)
val dep_to_int : 'a list -> 'a * 'a * 'a * 'a
