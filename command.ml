
type command = 
  | Help
  | Roll
  | Inventory of string
  | Buy
  | No
  | Upgrade
  | Quit
  | Trade
  | EndTurn

exception Empty

exception Malformed

(** [map_roll] is the Roll command if there are no words after "roll" and 
    raises Malformed otherwise *)
let map_roll rest_of_list = 
  match rest_of_list with
  | [] -> Roll
  | _ -> raise Malformed

(** [map_quit] is the Quit command if there are no words after "quit" and 
    raises Malformed otherwise *)
let map_quit = function
  | [] -> Quit
  | _ -> raise Malformed

(** [map_help] is the Help command if there are no words after "help" and 
    raises Malformed otherwise *)
let map_help = function
  | [] -> Help
  | _ -> raise Malformed

(** [map_inventory] is the Roll command on [player_name] if there is a word 
    after "inventory" and raises Malformed otherwise *)
let map_inventory player_name =
  match player_name with
  | "" -> raise Malformed
  | _ -> Inventory player_name

(** [map_buy] is the Buy command if there are no words after "buy" and 
    raises Malformed otherwise *)
let map_buy = function
  | [] -> Buy
  | _ -> raise Malformed

(** [map_no] is the No command if there are no words after "no" and 
    raises Malformed otherwise *)
let map_no = function
  | [] -> No
  | _ -> raise Malformed

(** [map_trade] is the Trade command if there are no words after "trade" and 
    raises Malformed otherwise *)
let map_trade = function
  | [] -> Trade
  | _ -> raise Malformed

(** [map_upgrade] is the Upgrade command if there are no words after "upgrade"
    and raises Malformed otherwise *)
let map_upgrade = function
  | [] -> Upgrade
  | _ -> raise Malformed

(** [map_endturn] is the EndTurn command if there are no words after "endturn" 
    and raises Malformed otherwise *)
let map_endturn = function
  | [] -> EndTurn
  | _ -> raise Malformed

(** [empties_removed] is the list of strings without the empty strings. It
      takes in the list of all strings and removes the empty ones. *)
let rec empties_removed = function
  | [] -> []
  | h::t -> if h <> "" then h::(empties_removed t) else empties_removed t

(** [parse_helper entire_str] is the list of words in [entire_str] split on 
    spaces and with empty strings removed *)
let parse_helper entire_str =
  empties_removed (String.split_on_char ' ' entire_str)

(** [loop_over_list] is the function responsible for handling the quit and
      go cases. It calls [map_quit] and [map_go] depending on the first string
      in the list of strings parsed by [parse_helper] *)
let loop_over_list = function
  | [] -> raise Empty
  |"help"::t -> map_help t
  |"inventory"::t -> map_inventory (String.concat "" t)
  |"buy"::t -> map_buy t
  |"no"::t -> map_no t
  | "quit"::t -> map_quit t
  | "roll"::t -> map_roll t
  | "trade"::t -> map_trade t
  | "upgrade"::t -> map_upgrade t
  | "endturn"::t -> map_endturn t
  | _ -> raise Malformed

(** [loop_over_list] is the main command parsing function. It calls 
    [loop_over_list] on the result from [parse_helper] *)
let parse str =
  loop_over_list (parse_helper str)


