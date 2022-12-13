type state = { colonnes : (int list) FArray.t;
               registres : (Card.card option) FArray.t option;
               depot : int list ;
               nbCol : int ; nbReg : int }

val split_list : int -> 'a list -> 'a list * 'a list

val create_state : string -> int list -> state

val state_to_string : state -> string
