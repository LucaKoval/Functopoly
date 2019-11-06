type object_phrase = string list

type command = 
  | Help
  | Roll
  | Inventory
  | Buy
  | Sell 
  | Quit

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

let map_inventory = function
  | []-> Inventory
  |_ -> raise Malformed

let map_buy = function
  | [] -> Buy
  | _ -> raise Malformed

let map_sell = function
  | [] -> Sell
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
      go cases. It calls [map_quit] and [map_go] depending on the first string in 
      the list of strings parsed by [parse_helper] *)
  let loop_over_list = function
    | [] -> raise Empty
    |"help"::t-> map_help t
    |"inventory"::t-> map_inventory t
    |"buy"::t-> map_buy t
    |"sell"::t-> map_sell t
    | h::t -> if h = "quit" then map_quit t 
      else if h = "roll" then map_roll t
      else raise Malformed
  in loop_over_list (parse_helper str)


