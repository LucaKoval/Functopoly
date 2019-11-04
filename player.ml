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
    number_of_players :int
}

(** creates the original player data structure for each player*)
let to_player empty_list = {
id = 0;
score = 0;
location = 0;
properties = []
}

 (** creates the original players data structure using user input*)
let to_players user_input = (*{
    player_list = List.map to_player;
    current_player = 0;
    number_of_players = user_input
}*)failwith "unimplemented"

(** takes in a 0 just for shits and returns a number rolled by 2 dice*)
let dice zero =
(Random.int 6) + (Random.int 6) +2 +zero

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
number_of_players = players.number_of_players
}

