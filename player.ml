open Yojson.Basic.Util
open Board
open Indices

type player = {
  id: int;
  score: int;
  location: int;
  properties: string list;
  money: int;
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
  |(0)-> acc
  |x-> to_player (numplayers-1) ({
      id = (x-1);
      score = 1500;
      location = 0;
      properties = [];
      money = 1500
    }::acc)

(** print list of ints*)
let rec print_int_list lst =
  match lst with
  |h::t -> print_int h; print_string " "; print_int_list t
  |[] -> print_string " "

(** print list of strings*)
let rec print_string_list lst =
  match lst with
  |h::t -> print_string h; print_string " "; print_string_list t
  |[] -> print_string " "

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
  print_string "You rolled a "; print_int x; print_endline ""; x 

(** [new_property player] is Some property that the current player obtains if
    any, otherwise None*)
let new_property player =
  if List.length player.properties = 0 then ""
  else List.nth player.properties 0

(** gets the tile from the current location on the board*)
let get_property current_loc board =
match (Indices.return_tile current_loc board) with 
| None -> failwith "no tile at this location"
| Some v -> v

(** returns true if current tile is a property tile*)
let is_property tile =
match tile with
| PropertyTile a -> true
| CardTile a -> false
| TaxTile a -> false
| CornerTile a -> false


(**assuming current location is a property, gets the owner id of the current location's property*)
let get_owner_id tile = 
match tile with
|PropertyTile a -> a.owner
|_-> failwith "get_owner_id: not a property tile"

(**assuming current location is a property, gets the name of the current location's property*)
let get_property_name tile = 
match tile with
|PropertyTile a -> a.name
|_-> failwith "get_property_name: not a property tile"

(**assuming current tile is a property, gets the rent of the current location's property*)
let tile_rent tile = 
match tile with
|PropertyTile a -> a.rent
|_-> failwith "tile_rent: not a property tile"

(** gets player name of the corresponding owner id number*)
let get_owner_name tile player_names =
List.nth player_names (get_owner_id tile)


(** returns the rent if the current location is a property with NO owner, else 0 *)
let get_rent board current_loc =
let current_tile = (get_property current_loc board) in
if (is_property current_tile) then ( 
  if ((get_owner_id current_tile) >  -1) then tile_rent current_tile
  else 0
  )
  else 0

(** updates the current player's state if its their turn (ex: location and changes to the next player*)
let update_current_player player current_player_id =

  if player.id = current_player_id then let new_loc = player.location + (dice 0) in ({
      id= player.id;
      score = if new_loc > 40 then player.score + 200 else if new_loc = 40 then player.score + 400 else player.score;
      location = new_loc mod 40;
      properties = player.properties;
      money = if new_loc > 40 then player.money + 200 else if new_loc = 40 then player.money + 400 else player.money
    })
  else ( {
      id = player.id;
      score = player.score;
      location= player.location;
      properties = player.properties;
      money = player.money
    })

(** if the new location is a unowned property, the price of the new property is returned, 
else if the property is owned, the property price is returned 
else if the location is not a property at all, 0 is returned*)
let roll_change_score playerscore new_loc board player_names =
if (get_rent board (new_loc mod 40))<>0 then
let prop= get_property_name (get_property (new_loc mod 40) board) in
let owner= get_owner_name (get_property (new_loc mod 40) board) player_names in 
let rent = get_rent board (new_loc mod 40) in
print_string prop; print_string " is owned by "; print_string owner; 
print_string " Rent paid: "; print_int rent;
print_endline "";
(if new_loc > 40 then (playerscore + 200-(get_rent board (new_loc mod 40))) 
else if new_loc = 40 then (playerscore + 400-(get_rent board (new_loc mod 40)))
else playerscore- (get_rent board (new_loc mod 40)))
else if (is_property (get_property (new_loc mod 40) board)) then (
let prop= get_property_name (get_property (new_loc mod 40) board) in
print_string prop;
print_string " is available for purchase! Would you like to buy?";
(if new_loc > 40 then playerscore + 200 else if new_loc = 40 then playerscore + 400
else playerscore) )
else (print_string "this is not a property, other card types are currently not implemented at this time, please treat this as a blank space for now"; 0) (* ToDo: tiles that aren't properties?*)


 (** updates the current player's state if its their turn based on roll*)
let rec roll_update_owner players_list player_names current_player_id board rent owner_id new_acc=
match players_list with
|[]-> new_acc
|player::t -> 
begin
  if (player.id = (List.nth owner_id 0)) then roll_update_owner t player_names current_player_id board rent owner_id ({
      id= player.id;
      score = player.score + (List.nth rent 0);
      location = player.location;
      properties = player.properties;
      money = player.money + (List.nth rent 0);
    }::new_acc) 
  else roll_update_owner t player_names current_player_id board rent owner_id ( {
      id = player.id;
      score = player.score;
      location= player.location;
      properties = player.properties;
      money = player.money
    }::new_acc)
    end


 (** updates the current player's state if its their turn based on roll*)
let rec roll_update_current_player players_list player_names current_player_id board acc rent_acc owner_id_acc=
match players_list with
|[]-> roll_update_owner acc player_names current_player_id board rent_acc owner_id_acc []
|player::t -> 
begin
  if (player.id = current_player_id) then let new_loc = player.location + (dice 0) in roll_update_current_player t player_names current_player_id board ({
      id= player.id;
      score = roll_change_score player.score new_loc board player_names;
      location = new_loc mod 40;
      properties = player.properties;
      money = roll_change_score player.money new_loc board player_names
    }::acc) ((player.score-(roll_change_score player.score new_loc board player_names))::rent_acc) ((get_owner_id( get_property (new_loc mod 40) board) )::owner_id_acc)
  else roll_update_current_player t player_names current_player_id board ( {
      id = player.id;
      score = player.score;
      location= player.location;
      properties = player.properties;
      money = player.money
    }::acc) rent_acc owner_id_acc
    end

(** gets price of property to buy or 0 if not a property*)
let get_price current_loc board=
match (get_property current_loc board) with
|PropertyTile a -> a.price
|_-> failwith "get_price: not a property at location"

(** updates owner_id of property in board to be current player_id*)

 (** updates the current player's state if its their turn based on buy*)
let rec buy_update_current_player players_list player_names current_player_id board acc=
match players_list with
|[]-> acc
|player::t ->
begin
  if (player.id = current_player_id) then (
  print_endline "Congrats you now own ";
  print_string (get_property_name (get_property player.location board)); 
  buy_update_current_player t player_names current_player_id board ({
      id= player.id;
      score = player.score;
      location = player.location;
      properties = ((get_property_name (get_property player.location board))::(player.properties));
      money = player.money-(get_price player.location board)
    }::acc))
  else buy_update_current_player t player_names current_player_id board (  {
      id = player.id;
      score = player.score;
      location= player.location;
      properties = player.properties;
      money = player.money
    }::acc)
    end

    (** get's the property price from a string*)
let rec get_property_price prop_name (board_properties:(Board.property_tile list)) =
match board_properties with
|[] -> failwith "get_property_price: not a property in board"
| h::t when h.name =prop_name -> h.price
|h::t ->  get_property_price prop_name t

(** makes a list of the current_player id to pass into mapping function in
    update_players*)
let rec make_current_id_list players acc =
  if (players.number_of_players =(List.length acc))
  then ( acc)
  else (make_current_id_list players ((players.current_player )::acc))

(** updates the players state based on their turn (ex: location, score,
    potential property changes) and changes to the next player*)
let update_players players =
  List.map2 update_current_player (players.player_list)
    (make_current_id_list players [])

(** updates the players rent based on roll *)
let roll_update_players players board=
  roll_update_current_player players.player_list players.player_names players.current_player board [] [] []

let buy_update_players players board =
  buy_update_current_player players.player_list players.player_names players.current_player board []

(** updates the players state based on roll (ex: location, score,
    potential property changes) and changes to the next player*)
let roll_new_player players board = {
    player_list = roll_update_players players board;
  current_player = players.current_player;
  number_of_players = players.number_of_players;
  player_names = players.player_names
}


let rec list_printer lst =
match lst with
|[]-> ()
|h::t -> print_endline h; list_printer t
 
let rec inventory_helper players_list acc current_player_id =
 match players_list with
 |[]-> acc
 |h::t when h.id = current_player_id -> h.properties
 |h::t -> inventory_helper t acc current_player_id

(** updates the players state based on buy (ex: location, score,
    potential property changes) and changes to the next player*)
let buy_new_player players board = {
    player_list = buy_update_players players board;
  current_player = players.current_player;
  number_of_players = players.number_of_players;
  player_names = players.player_names
}

(** updates the players state based on their turn (ex: location, score,
    potential property changes) and changes to the next player*)
let new_player players = {
  player_list = (*update_players*) players.player_list;
  current_player = (players.current_player +1) mod
                   (List.length players.player_names);
  number_of_players = players.number_of_players;
  player_names = players.player_names
}

let rec remove_helper lst el acc=
match lst with
|[]-> acc
|h::t when h = el -> remove_helper lst el acc
|h::t -> remove_helper lst el (h::acc)

(** updates the player two 2 stuff and then passes that new players list into trade_update_player for the final updates*)
let rec trade_update_player2 players_list p1 p2 px_prop py_prop board cash acc=
match players_list with
|[]-> trade_update_player2 players_list p1 p2 px_prop py_prop board cash acc
|player::t->
begin
  if (player.id = p2) then (
  trade_update_player2 t p1 p2 px_prop py_prop board cash ({
      id = player.id;
      score = player.score - cash -(get_property_price py_prop board)+ (get_property_price px_prop board);
      location= player.location;
      properties =  px_prop::(remove_helper player.properties py_prop []);
      money = player.money-cash
    }::acc))
  else trade_update_player2 t p1 p2 px_prop py_prop board cash ({
      id = player.id;
      score = player.score;
      location= player.location;
      properties = player.properties;
      money = player.money
    }::acc)
    end

(** updates the player one 1 stuff and then passes that new players list into trade_update_player2 for the final updates*)
let rec trade_update_player players_list p1 p2 px_prop py_prop board cash acc=
match players_list with
|[]-> trade_update_player2 acc p1 p2 px_prop py_prop board cash []
|player::t->
begin
  if (player.id = p1) then (
  trade_update_player t p1 p2 px_prop py_prop board cash ({
      id = player.id;
      score = player.score + cash+ (get_property_price py_prop board)- (get_property_price px_prop board) ;
      location= player.location;
      properties =  py_prop::(remove_helper player.properties px_prop []);
      money = player.money+cash
    }::acc))
  else trade_update_player t p1 p2 px_prop py_prop board cash ({
      id = player.id;
      score = player.score;
      location= player.location;
      properties = player.properties;
      money = player.money
    }::acc)
    end


(** takes in players: players p1: int, p2: int, p1_prop: string, p2_prop: string, cash: int *)
let trade_new_player players p1 p2 px_prop py_prop board cash=
{
  player_list = trade_update_player players.player_list p1 p2 px_prop py_prop board cash [];
  current_player = players.current_player;
  number_of_players = players.number_of_players;
  player_names = players.player_names
}


