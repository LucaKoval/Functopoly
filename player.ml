open Yojson.Basic.Util

type player = {
  id: int;
  score: int;
  location: int;
  properties: string list
}

type players = {
  player_list : player list;
  current_player : int;
  number_of_players : int;
  player_names : string list
}

(** creates the original player data structure for each player *)
let rec to_player numplayers acc= 
match numplayers with
|0-> acc
|x-> to_player (numplayers-1) ({
  id = x;
  score = 0;
  location = 0;
  properties = []
}::acc)


(** creates the original players data structure using user input*)
let to_players num_players input_names = {
player_list = to_player num_players [];
current_player = 0;
number_of_players = num_players;
player_names = input_names
                                            }

let rec get_current_location_helper player_list current_id =
match player_list with
|h::t when h.id = current_id -> h.location
|h::t -> get_current_location_helper t current_id
|_-> failwith "no current player error"

let get_current_location players =
get_current_location_helper players.player_list players.current_player

(** takes in a 0 just for shits and returns a number rolled by 2 dice*)
let dice zero =
  let x = (Random.int 6) + (Random.int 6) +2 +zero in
  print_endline "You rolled a"; print_int x; x

(** [new_property player] is Some property that the current player obtains if any, otherwise None*)
let new_property player =
  failwith "unimplemented"

(** updates the current player's state if its their turn (ex: location, score, potential property changes) and changes to the next player*)
let update_current_player player current_player_id =
  if player.id = current_player_id then {
    id= player.id;
    score = player.score;
    location = player.location + (dice 0);
    properties = ((new_property player)::(player.properties)) }
  else {
    id = player.id;
    score = player.score;
    location= player.location;
    properties = player.properties
  }

(** makes a list of the current_player id to pass into mapping function in update_players*)
let rec make_current_id_list players acc =
  if (players.number_of_players =(List.length acc))
  then acc
  else make_current_id_list players ((players.number_of_players )::acc)

(** updates the players state based on their turn (ex: location, score, potential property changes) and changes to the next player*)
let update_players players =
  List.map2 update_current_player (players.player_list) (make_current_id_list players [])

(** updates the players state based on their turn (ex: location, score, potential property changes) and changes to the next player*)
let new_player players = {
  player_list = update_players players;
  current_player = players.current_player +1;
  number_of_players = players.number_of_players;
  player_names = players.player_names
}

