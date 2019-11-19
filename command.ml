
type command = 
  | Help
  | Roll
  | Inventory of string
  | Buy
  | Upgrade
  | Quit
  | Trade

exception Empty

exception Malformed

let map_roll rest_of_list = 
  match rest_of_list with
  | [] -> Roll
  | _ -> raise Malformed

let map_quit = function
  | [] -> Quit
  | _ -> raise Malformed

let map_help = function
  | [] -> Help
  | _ -> raise Malformed

let map_inventory player_name =
  match player_name with
  | "" -> raise Malformed
  | _ -> Inventory player_name

let map_buy = function
  | [] -> Buy
  | _ -> raise Malformed

let map_trade = function
  | [] -> Trade
  | _ -> raise Malformed

let map_upgrade = function
  | [] -> Upgrade
  | _ -> raise Malformed

let parse_helper entire_str =
  (** [empties_removed] is the list of strings without the empty strings. It
      takes in the list of all strings and removes the empty ones. *)
  let rec empties_removed = function
    | [] -> []
    | h::t -> if h <> "" then h::(empties_removed t) else empties_removed t in
  empties_removed (String.split_on_char ' ' entire_str)

let parse str =
  (** [loop_over_list] is the function responsible for handling the quit and
      go cases. It calls [map_quit] and [map_go] depending on the first string
      in the list of strings parsed by [parse_helper] *)
  let loop_over_list = function
    | [] -> raise Empty
    |"help"::t -> map_help t
    |"inventory"::t -> map_inventory (String.concat "" t)
    |"buy"::t -> map_buy t
    | h::t -> if h = "quit" then map_quit t
      else if h = "roll" then map_roll t
      else if h = "trade" then map_trade t
      else if h = "upgrade" then map_upgrade t
      else raise Malformed
  in loop_over_list (parse_helper str)


