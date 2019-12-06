open Yojson.Basic.Util
open Board
open Indices
open Cards

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
  player_names : string list;
  jail_list: (int * int) list
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
  player_names = input_names;
  jail_list = []
}

let get_current_player (players:players) =
  List.nth players.player_list players.current_player

let rec get_current_location_helper player_list current_id =
  match player_list with
  |h::t when h.id = current_id -> h.location
  |h::t -> get_current_location_helper t current_id
  |_-> failwith "no current player error"

let get_current_location players =
  get_current_location_helper players.player_list players.current_player

(** takes in a 0 just for shits and returns a tuple of numbers rolled by 2 dice*)
let dice zero =
  let x = (Random.int 6) + 1 in let y = (Random.int 6) +1  +zero in
  print_string "You rolled a "; print_int x; print_string " and a "; print_int y; print_endline ""; (x,y) 

(** [new_property player] is Some property that the current player obtains if
    any, otherwise None*)
let new_property player =
  print_endline "new_property";
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
  |_-> (-2)

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
  print_endline "get_owner_name";
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

  if player.id = current_player_id then let dice_roll = (dice 0) in let new_loc = player.location + (fst(dice_roll)+snd(dice_roll)) in ({
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

let get_curr_tile current_loc board =
  match (Indices.return_tile current_loc board) with 
  | None -> failwith "no tile at this location"
  | Some v -> v

let is_card tile =
  match tile with
  | PropertyTile a -> false
  | CardTile a -> true
  | TaxTile a -> false
  | CornerTile a -> false

let rec get_card_type_from_index location (tiles:Board.card_tile list) = 
  match tiles with
  | [] -> ""
  | h::t -> if h.location = location then h.card_tile_name
    else get_card_type_from_index location t

let select_random_card cards = 
  let rand_ind = (Random.int (List.length cards)) in
  List.nth cards rand_ind


let card_main location board players curr_player score = 
  let chance_or_community = get_card_type_from_index location board.card_tiles in
  let selected_card = select_random_card board.cards in
  let curr_player = (List.nth players curr_player) in
  print_string "You landed on a card tile for the ";
  print_string chance_or_community;
  print_endline " deck and picked up the following card.";
  print_endline selected_card.description;
  match selected_card.subtype with
  | AdvanceTo -> 0
  | Collect -> score + int_of_string selected_card.value
  | GetOutOfJail -> 0
  | GoBack -> 0
  | Pay -> score - int_of_string selected_card.value
  | CollectFromAll -> 0
  | _ -> 0


(** if the new location is a unowned property, the price of the new property is returned, 
    else if the property is owned, the property price is returned 
    else if the location is not a property at all, 0 is returned*)
let roll_change_score playerscore new_loc board player_names curr_player players =
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
    print_string " is available for purchase! Would you like to buy? ";
    (if new_loc > 40 then playerscore + 200 else if new_loc = 40 then playerscore + 400
     else playerscore) )
  else (if (new_loc=10||new_loc=30) then 0 
        else (
          (* TODO: tiles that aren't properties? *)
          if is_card (get_curr_tile new_loc board) then 
            card_main new_loc board players curr_player playerscore
          else 0
        )
       )

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



(** check if current_location is of type gotojail *)
let check_jail_type curr_loc board=
  match (get_property curr_loc board) with
  | PropertyTile a -> false
  | CardTile a -> false
  | TaxTile a -> false
  | CornerTile a when (a.corner_tile_type = GoToJail) -> (print_endline "You landed on go to jail. You are now being moved to the location of jail"; true)
  | CornerTile a -> false

(** check if current_location is of type actual jail *)
let check_in_jail curr_loc board curr_id jail_list=
  match (get_property curr_loc board) with
  | PropertyTile a -> false
  | CardTile a -> false
  | TaxTile a -> false
  | CornerTile a when (a.corner_tile_type = JailJustVisiting) -> (if (List.mem_assoc curr_id jail_list) then true else (print_endline "You are now just visiting jail"; false))
  | CornerTile a -> false


(** takes in players_list and curr_player_id and checks if current_player loc is go to jail*)
let rec check_add_jail_list players_list curr_player_id board=
  match players_list with
  | h::t when h.id= curr_player_id -> (check_jail_type h.location board)
  | h::t -> check_add_jail_list t curr_player_id board
  | [] -> false

(** get jail_roll_counter for current player id *)
let get_jail_roll_counter jail_list curr_id =
  List.assoc curr_id jail_list

(** returns true if no change to player state and they stay in jail, otherwise false and regular roll process
    check if in jail_list and counter is good*)
let check_jail_list jail_list curr_id dice_tuple =
  if (List.mem_assoc curr_id jail_list) 
  then (if ((get_jail_roll_counter jail_list curr_id)<2 ) 
        then (if (fst(dice_tuple))<>(snd(dice_tuple)) then (print_endline "You did not roll doubles, you are stuck in jail still"; true) else (print_endline "You rolled doubles! You are free from jail!"; false)) 
        else (print_endline "This is your third roll in jail. You are free now!"; false))
  else false



(** jail_roll_counter called if current property jail roll counter passes jail conditons otherwise regular roll *)

(** gets location of jail*)
let rec get_jail_location (board_tiles:(Board.corner_tile list)) =
  match board_tiles with
  |[] -> failwith "get_jail_location: fails not a corner_tile in board"
  | h::t when h.corner_tile_type = JailJustVisiting -> h.location
  |h::t -> get_jail_location t

(** gets location of go to jail*)
let rec get_gotojail_location (board_tiles:(Board.corner_tile list)) =
  match board_tiles with
  |[] -> failwith "get_gotojail_location: fails not a corner_tile in board"
  | h::t when h.corner_tile_type = GoToJail -> h.location
  |h::t -> get_gotojail_location t

(**  updates jail_list with new_player in jail and changes their location to jail*)
let update_jail_list id jail_list=
  ((id, 0)::jail_list)

(** increases current player's roll count by +1*)
let update_jail_roll_count id jail_list= let curr_roll_count =get_jail_roll_counter jail_list id in 
  (id, curr_roll_count+1)::(List.remove_assoc id jail_list)


(** updates the current player's state if its their turn based on roll*)
let rec roll_update_current_player players players_list player_names current_player_id board acc rent_acc owner_id_acc jail_list=
  match players_list with
  |[]-> (if is_property (get_property (get_current_location players) board) 
         then (roll_update_owner acc player_names current_player_id board rent_acc owner_id_acc [])
         else acc)
  |player::t -> 
    begin
      if (player.id = current_player_id) then let dice_roll = (dice 0) in 
        (*   let new_loc = player.location + (fst(dice_roll)+snd(dice_roll)) in 
             let new_score = (roll_change_score player.score new_loc board player_names) in *)
        (* check if new_loc is actually go to jail and update the location to be visiting jail *)
        (* check if id and jail count stuff then check if not doubles and print still in jail otherwise *)
        (if ((check_jail_list jail_list current_player_id dice_roll)= false)then
           let new_loc = player.location + (fst(dice_roll)+snd(dice_roll)) in 
           let new_score = (roll_change_score player.score new_loc board player_names current_player_id players.player_list) in 
           roll_update_current_player players t player_names current_player_id board ({
               id= player.id;
               score = new_score;
               location = new_loc mod 40;
               properties = player.properties;
               money = new_score
             }::acc) ((player.score-new_score)::rent_acc) ((get_owner_id( get_property (new_loc mod 40) board) )::owner_id_acc ) jail_list
         else (roll_update_current_player players t player_names current_player_id board ( {
             id = player.id;
             score = player.score;
             location= player.location;
             properties = player.properties;
             money = player.money
           }::acc) rent_acc owner_id_acc jail_list))      
      else roll_update_current_player players t player_names current_player_id board ( {
          id = player.id;
          score = player.score;
          location= player.location;
          properties = player.properties;
          money = player.money
        }::acc) rent_acc owner_id_acc jail_list
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
  roll_update_current_player players players.player_list players.player_names players.current_player board [] [] [] players.jail_list

let buy_update_players players board =
  buy_update_current_player players.player_list players.player_names players.current_player board []


(** helper for called at the beginning of roll, if player's location is gotojail, then it changes the current player's location to actual jail*)
let rec update_location_to_jail players_list current_player_id board acc=
  match players_list with
  |[]-> acc
  |player::t ->
    begin
      if (player.id = current_player_id) then (
        update_location_to_jail t current_player_id board ({
            id= player.id;
            score = player.score;
            location = get_jail_location board.corner_tiles;
            properties = player.properties;
            money = player.money
          }::acc) )
      else update_location_to_jail t current_player_id board (  {
          id = player.id;
          score = player.score;
          location= player.location;
          properties = player.properties;
          money = player.money
        }::acc) 
    end

(** called at the beginning of roll, if player's location is gotojail, then it changes the current player's location to actual jail*)
let update_location_to_jail_main players board= {
  player_list = if (get_current_location players =(get_gotojail_location board.corner_tiles)) then update_location_to_jail players.player_list players.current_player board [] else players.player_list;
  current_player = players.current_player;
  number_of_players = players.number_of_players;
  player_names = players.player_names;
  jail_list = players.jail_list

}


(** updates the players state based on roll (ex: location, score,
    potential property changes) and changes to the next player*)
let roll_new_player players board = 
  let more_players= update_location_to_jail_main players board in
  let new_player_list = roll_update_players more_players board in {
    player_list = new_player_list;
    current_player = more_players.current_player;
    number_of_players = more_players.number_of_players;
    player_names = more_players.player_names;
    jail_list = (print_endline "made it to jail_list"; (if (check_jail_type (get_current_location_helper new_player_list more_players.current_player) board) 
                                                        then (update_jail_list more_players.current_player more_players.jail_list)
                                                        else if (check_in_jail (get_current_location_helper new_player_list more_players.current_player) board more_players.current_player more_players.jail_list) 
                                                        then (update_jail_roll_count more_players.current_player more_players.jail_list)
                                                        else (if (List.mem_assoc more_players.current_player more_players.jail_list)
                                                              then (List.remove_assoc more_players.current_player more_players.jail_list) else more_players.jail_list)
                                                       ))
  }


let rec list_printer lst =
  match lst with
  |[]-> ()
  |h::t -> print_endline h; list_printer t

(** takes in an accumulator just for shits*)
let rec inventory_helper players_list acc current_player_id =
  match players_list with
  |[]-> acc
  |h::t when h.id = current_player_id -> h.properties
  |h::t -> inventory_helper t acc current_player_id

(** takes in an accumulator just for shits*)
let rec inventory_money_helper players_list acc current_player_id =
  match players_list with
  |[]-> acc
  |h::t when h.id = current_player_id -> h.money
  |h::t -> inventory_money_helper t acc current_player_id

(** updates the players state based on buy (ex: location, score,
    potential property changes) and changes to the next player*)
let buy_new_player players board = {
  player_list = buy_update_players players board;
  current_player = players.current_player;
  number_of_players = players.number_of_players;
  player_names = players.player_names;
  jail_list = players.jail_list

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
    player_names = players.player_names;
    jail_list = players.jail_list

  }

(** updates the current player's state if its their turn based on buy*)
let rec upgrade_update_current_player players_list player_names current_player_id board acc prop_loc=
  match players_list with
  |[]-> acc
  |player::t ->
    begin
      if (player.id = current_player_id) then (
        upgrade_update_current_player t player_names current_player_id board ({
            id= player.id;
            score = player.score;
            location = player.location;
            properties = ((get_property_name (get_property player.location board))::(player.properties));
            money = player.money-((get_price prop_loc board)/2)
          }::acc) prop_loc)
      else upgrade_update_current_player t player_names current_player_id board (  {
          id = player.id;
          score = player.score;
          location= player.location;
          properties = player.properties;
          money = player.money
        }::acc) prop_loc
    end

let upgrade_update_players players board prop_loc=
  upgrade_update_current_player players.player_list players.player_names players.current_player board [] prop_loc

(**updates the players state based on upgrade (ex: the money changes*)
let upgrade_new_player players board prop_loc= {
  player_list = upgrade_update_players players board prop_loc;
  current_player = players.current_player;
  number_of_players = players.number_of_players;
  player_names = players.player_names;
  jail_list = players.jail_list

}

(** *)
let forfeit_player (curr_player:player) (players:players) =
  print_endline "forfeit_player";
  {players with player_list=(remove_helper players.player_list curr_player []);
                number_of_players=players.number_of_players-1;
                player_names=(remove_helper players.player_names (List.nth players.player_names curr_player.id) []);
  }

(** updates the players state based on their turn (ex: location, score,
    potential property changes) and changes to the next player*)
let new_player players board= 
  let more_players= update_location_to_jail_main players board in {
    player_list = more_players.player_list;
    current_player = (players.current_player +1) mod
                     (List.length players.player_names);
    number_of_players = players.number_of_players;
    player_names = players.player_names;
    jail_list = players.jail_list

  }
