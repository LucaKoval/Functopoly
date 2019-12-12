open Yojson.Basic.Util
open Board
open Indices

(** [modulo x y] is [x mod y] if the result is positive, otherwise [(x mod y)+y] *)
let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

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
  let helper = 
    match numplayers with
    |(0)-> acc
    |x-> to_player (numplayers-1) ({
        id = (x-1);
        score = 0;
        location = 0;
        properties = [];
        money = 0
      }::acc) in
  let player_list = helper in
  List.sort (fun x y -> x.id - y.id) player_list

(** creates the original players data structure using user input*)
let to_players num_players input_names = {
  player_list = to_player num_players [];
  current_player = 0;
  number_of_players = num_players;
  player_names = input_names;
  jail_list = []
}

(** gets the player with the current player id from players *)
let get_current_player (players:players) =
  List.nth players.player_list players.current_player

(** helper for get_current_location that gets the current players location *)
let rec get_current_location_helper player_list current_id =
  match player_list with
  | h::t when h.id = current_id -> h.location
  | h::t -> get_current_location_helper t current_id
  | _-> failwith "no current player error"

(** gets the location of the current player from players *)
let get_current_location players =
  get_current_location_helper players.player_list players.current_player

(** [dice z] is a tuple of numbers rolled by 2 dice*)
let dice zero=
  let x = (Random.int 6) + 1 in let y = (Random.int 6) +1 in
  print_string "You rolled a "; print_int x; print_string " and a "; 
  print_int y; print_endline ""; (x,y) 

(** gets the tile from the current location on the board*)
let get_property current_loc board =
  match Indices.return_tile current_loc board with 
  | None -> failwith "no tile at this location"
  | Some v -> v

(** [is_property t] is true if current tile is a property tile, otherwise false*)
let is_property tile =
  match tile with
  | PropertyTile a -> print_endline "is_property is true"; true
  | CardTile a -> false
  | TaxTile a -> false
  | CornerTile a -> false


(** if given a property, gets its owner id, otherwise gets -2 *)
let get_owner_id tile = 
  match tile with
  |PropertyTile a -> a.owner
  |_-> (-2)

(** assuming current location is a property, gets the property name *)
let get_property_name tile = 
  match tile with
  |PropertyTile a -> a.name
  |_-> failwith "get_property_name: not a property tile"

(** assuming current tile is a property, gets the property rent *)
let tile_rent tile = 
  match tile with
  |PropertyTile a -> a.rent
  |_-> failwith "tile_rent: not a property tile"

(** gets player name of the corresponding owner id number*)
let get_owner_name tile player_names =
  List.nth player_names (get_owner_id tile)

(** gets the rent if the current location is a property with NO owner, else 0 *)
let get_rent board current_loc =
  let current_tile = get_property current_loc board in
  if is_property current_tile
  then if (get_owner_id current_tile)> -1
    then tile_rent current_tile
    else 0
  else 0

(** gets tile at current location *)
let get_curr_tile current_loc board =
  match Indices.return_tile current_loc board with 
  | None -> failwith "no tile at this location"
  | Some v -> v

(** [is_card t] is true if t is of type CardTile, otherwise false *)
let is_card tile =
  match tile with
  | PropertyTile a -> false
  | CardTile a -> print_endline "is_card is true"; true
  | TaxTile a -> false
  | CornerTile a -> false

(** [is_tax t] is true if t is of type TaxTile, otherwise false *)
let is_tax tile =
  match tile with
  | PropertyTile a -> false
  | CardTile a -> false
  | TaxTile a -> print_endline "is_tax is true"; true
  | CornerTile a -> false

(** gets name of card type of given location *)
let rec get_card_type_from_index location (tiles:Board.card_tile list) = 
  match tiles with
  | [] -> ""
  | h::t -> if h.location = location then h.card_tile_name
    else get_card_type_from_index location t

(** gets a random card from list of cards *)
let select_random_card cards = 
  let rand_ind = Random.int (List.length cards) in
  List.nth cards rand_ind

(** takes in a [property] name and the list of property_tiles and returns the 
    location of that [property] on the board *)
let rec get_property_location property = function
  | [] -> failwith "Property not found"
  | h::t -> if (String.lowercase_ascii property) = (String.lowercase_ascii h.name) 
    then h.location
    else get_property_location property t

(** takes a card title and gives a tuple with the tile name and its location *)
let get_new_location board = function
  | "Go" -> ("Go", 0)
  | "Jail" -> ("Jail", 30)
  | property -> (property, get_property_location property board.property_tiles)

(** gets a players_list with the current player at the new location *)
let rec update_location str players_list current_player_id board acc=
  match players_list with
  | []-> acc
  | player::t ->
    begin
      if (player.id = current_player_id) then (
        let new_tile, new_loc = get_new_location board str in
        print_string "Your new location is the ";
        print_string new_tile;
        print_string " tile, which is at the numeric location ";
        print_endline (string_of_int new_loc);
        update_location str t current_player_id board ({
            player with
            location = new_loc;
          }::acc) )
      else update_location str t current_player_id board (player::acc) 
    end

(** gets players with the current player at the new location *)
let update_location_main str players board= {
  players with
  player_list = update_location str players.player_list players.current_player board []
}

(** gets players_list with current player's location sent back *)
let rec update_location_goback str players_list current_player_id board acc=
  match players_list with
  | []-> acc
  | player::t ->
    begin
      if (player.id = current_player_id) then (
        print_string "Your new location is ";
        print_endline (string_of_int (modulo (player.location - (int_of_string str)) 40));
        update_location_goback str t current_player_id board ({
            player with
            location = modulo (player.location - (int_of_string str)) 40;
          }::acc) )
      else update_location_goback str t current_player_id board (player::acc) 
    end

(** gets players with current player's location sent back by card *)
let update_location_goback_main str players board= {
  players with
  player_list = update_location_goback str players.player_list players.current_player board []
}

(** deducts given amount from every player and gives updated player_list *)
let rec update_collect_from_all_player str players_list acc=
  match players_list with
  | []-> acc
  | player::t ->
    begin
      update_collect_from_all_player str t ( { 
          player with
          score = (player.score - (int_of_string str));
          money = (player.money - (int_of_string str))
        }::acc)
    end

(** gives players after money is collected from all players *)
let update_collect_from_all str players= {
  players with
  player_list = update_collect_from_all_player str players.player_list [];
}

(** gives players_list after money is GIVEN to current player *)
let rec update_score_collect str players_list current_player_id board acc=
  match players_list with
  | []-> acc
  | player::t ->
    begin
      if (player.id = current_player_id) then (
        print_string "Your score is now ";
        print_string (string_of_int (player.score + (int_of_string str)));
        print_string " and your money is now ";
        print_endline (string_of_int (player.money + (int_of_string str)));
        update_score_collect str t current_player_id board ({
            player with
            score = player.score + (int_of_string str);
            money = player.money + (int_of_string str)
          }::acc) )
      else update_score_collect str t current_player_id board (player::acc) 
    end

(** gets players after adding money to current player *)
let update_score_collect_main str players board= {
  players with
  player_list = update_score_collect str players.player_list players.current_player board []
}

(** gets players_list after collecting money from current player *)
let rec update_score_pay str players_list current_player_id board acc=
  match players_list with
  | []-> acc
  | player::t ->
    begin
      if (player.id = current_player_id) then (
        print_string "Your score is now ";
        print_string (string_of_int (player.score - (int_of_string str)));
        print_string " and your money is now ";
        print_endline (string_of_int (player.money - (int_of_string str)));
        update_score_pay str t current_player_id board ({
            player with
            score = player.score - (int_of_string str);
            money = player.money - (int_of_string str)
          }::acc) )
      else update_score_pay str t current_player_id board (player::acc) 
    end

(** gets players after collecting money from current player *)
let update_score_pay_main str players board= {
  players with
  player_list = update_score_pay str players.player_list players.current_player board [];
}

(** updates jail_list with new_player in jail after their location changes to jail *)
let update_jail_list id jail_list=
  ((id, 0)::jail_list)

(**  updates players after current player is draws go to jail card *)
let send_to_jail_card players = {
  players with jail_list = update_jail_list players.current_player players.jail_list
}

(** gets a tuple with the updated players information and current player score based on the card drawn *)
let card_main location board players curr_player score = 
  let chance_or_community = get_card_type_from_index location board.card_tiles in
  let selected_card = select_random_card board.cards in
  print_string "You landed on a card tile for the ";
  print_string chance_or_community;
  print_endline " deck and picked up the following card.";
  print_endline selected_card.description;
  match selected_card.subtype with
  | AdvanceTo -> print_endline "advanceto matched in card_main";if selected_card.value="Jail" then (send_to_jail_card(update_location_main selected_card.value players board), score) else (update_location_main selected_card.value players board, score)
  | Collect -> print_endline "collect matched in card_main";(update_score_collect_main selected_card.value players board, score + int_of_string selected_card.value)
  | GoBack -> print_endline "goback matched in card_main";(update_location_goback_main selected_card.value players board, score)
  | Pay -> print_endline "pay matched in card_main";(update_score_pay_main selected_card.value players board, score - int_of_string selected_card.value)
  | CollectFromAll -> print_endline "collectfromall matched in card_main";
    (update_collect_from_all selected_card.value players, score)
  | _ -> print_endline "no match in card_main";(players, score)

(** tuple of players and new player score after rent deduction and gains from passing/landing on go are added *)
let roll_change_score_helper1 playerscore new_loc board player_names players =
  print_endline "roll_change_score_helper1";
  let prop= get_property_name (get_property (modulo new_loc 40) board) in
  let owner= get_owner_name (get_property (modulo new_loc 40) board) player_names in 
  let rent = get_rent board (modulo new_loc 40) in
  print_string prop; print_string " is owned by "; print_string owner; 
  print_string " Rent paid: "; print_int rent;
  print_endline "";
  (if new_loc > 40 then (players, (playerscore + 200-(get_rent board (modulo new_loc 40))))
   else if new_loc = 40 then (players, (playerscore + 400-(get_rent board (modulo new_loc 40))))
   else (players, playerscore- (get_rent board (modulo new_loc 40))))

(** called when the new location is a unowned property, returns tuple with players and the player score based on whether passing go or not *)
let roll_change_score_helper2 playerscore new_loc board player_names players =
  print_endline "roll_change_score_helper2";
  let prop= get_property_name (get_property (modulo new_loc 40) board) in
  print_string prop;
  print_string " is available for purchase! Would you like to buy? ";
  (if new_loc > 40 then (players, playerscore + 200) else if new_loc = 40 then (players, playerscore + 400)
   else (players, playerscore)) 

(** gets a tuple of the players and the new playerscore based on the card characteristics *)
let roll_change_score_helper3 playerscore new_loc board player_names curr_player players =
  print_endline "roll_change_score_helper3";
  if (new_loc=10||new_loc=30) then (players, playerscore) 
  else (
    (* TODO: tiles that aren't properties? *)
    if is_card (get_curr_tile new_loc board) then 
      card_main new_loc board players curr_player playerscore
    else if is_tax (get_curr_tile new_loc board) then
      if new_loc <>4 then (players, playerscore-75) else (print_string "You have been Luxury-Taxed! Say goodbye to $75"; (players, playerscore))
    else (players, playerscore)
  )

(** if the new location is a unowned property, the price of the new property is returned, 
    else if the property is owned, the property price is returned 
    else if the location is not a property at all, 0 is returned*)
let roll_change_score playerscore new_loc board player_names curr_player players =
  if (get_rent board (modulo new_loc 40))<>0 then
    roll_change_score_helper1 playerscore new_loc board player_names players
  else if (is_property (get_property (modulo new_loc 40) board)) then (roll_change_score_helper2 playerscore new_loc board player_names players)
  else (roll_change_score_helper3 playerscore new_loc board player_names curr_player players
       )

(** updates the current player's state if its their turn based on roll*)
let rec roll_update_owner players_list player_names current_player_id board rent owner_id new_acc=
  match players_list with
  |[]-> new_acc
  |player::t -> 
    begin
      if (player.id = (List.nth owner_id 0)) then roll_update_owner t player_names current_player_id board rent owner_id ({
          player with
          score = player.score + (int_of_string (List.nth rent 0));
          money = player.money + (int_of_string (List.nth rent 0));
        }::new_acc)
      else roll_update_owner t player_names current_player_id board rent owner_id (player::new_acc)
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
        (if ((check_jail_list jail_list current_player_id dice_roll)= false)then
           let new_loc = modulo (player.location + (fst(dice_roll)+snd(dice_roll))) 40 in 
           let (players, new_score) = (roll_change_score player.score new_loc board player_names current_player_id players) in 
           roll_update_current_player players t player_names current_player_id board ({ player with
                                                                                        score = new_score;
                                                                                        location = modulo new_loc 40;
                                                                                        money = player.money - (player.score-new_score)
                                                                                      }::acc) ((string_of_int (player.score-new_score))::rent_acc) ((get_owner_id( get_property (modulo new_loc 40) board) )::owner_id_acc ) jail_list
         else (roll_update_current_player players t player_names current_player_id board (player::acc) rent_acc owner_id_acc jail_list))      
      else roll_update_current_player players t player_names current_player_id board (player::acc) rent_acc owner_id_acc jail_list
    end

(** gets price of property to buy or 0 if not a property*)
let get_price current_loc board=
  match (get_property current_loc board) with
  |PropertyTile a -> a.price
  |_-> failwith "get_price: not a property at location"

(** updates the current player's state if its their turn based on buy*)
let rec buy_update_current_player players_list player_names current_player_id board acc=
  match players_list with
  |[]-> acc
  |player::t ->
    begin
      if (player.id = current_player_id) then ( 
        buy_update_current_player t player_names current_player_id board ({ player with
                                                                            properties = ((get_property_name (get_property player.location board))::(player.properties));
                                                                            money = player.money-(get_price player.location board)
                                                                          }::acc))
      else buy_update_current_player t player_names current_player_id board (player::acc)
    end

(** get's the property price from a string*)
let rec get_property_price prop_name (board_properties:(Board.property_tile list)) =
  match board_properties with
  | [] -> failwith "get_property_price: not a property in board"
  | h::t when h.name =prop_name -> h.price
  | h::t ->  get_property_price prop_name t

(** makes a list of the current_player id to pass into mapping function in
    update_players*)
let rec make_current_id_list players acc =
  if (players.number_of_players =(List.length acc))
  then (acc)
  else (make_current_id_list players ((players.current_player )::acc))

(** updates the players rent based on roll *)
let roll_update_players players board=
  roll_update_current_player players players.player_list players.player_names players.current_player board [] [] [] players.jail_list

(** helper for called at the beginning of roll, if player's location is gotojail, then it changes the current player's location to actual jail*)
let rec update_location_to_jail players_list current_player_id board acc=
  match players_list with
  |[]-> acc
  |player::t ->
    begin
      if (player.id = current_player_id) then (
        update_location_to_jail t current_player_id board ({player with
                                                            location = get_jail_location board.corner_tiles
                                                           }::acc) )
      else update_location_to_jail t current_player_id board (player::acc) 
    end

(** called at the beginning of roll, if player's location is gotojail, then it changes the current player's location to actual jail*)
let update_location_to_jail_main players board= {
  players with player_list = if (get_current_location players =(get_gotojail_location board.corner_tiles)) then update_location_to_jail players.player_list players.current_player board [] else players.player_list

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
    jail_list = (
      if (check_jail_type (get_current_location_helper new_player_list more_players.current_player) board) 
      then (update_jail_list more_players.current_player more_players.jail_list)
      else if (check_in_jail (get_current_location_helper new_player_list more_players.current_player) board more_players.current_player more_players.jail_list) 
      then (update_jail_roll_count more_players.current_player more_players.jail_list)
      else (if (List.mem_assoc more_players.current_player more_players.jail_list)
            then (List.remove_assoc more_players.current_player more_players.jail_list)
            else more_players.jail_list)
    )
  }

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
  players with
  player_list = buy_update_current_player players.player_list players.player_names players.current_player board [];
}

(** removes the given element from the list *)
let rec remove_helper lst el acc count=
  match lst with
  |[]-> List.rev acc
  |h::t when h = el && count = 0 -> remove_helper t el acc (count+1)
  |h::t -> remove_helper t el (h::acc) count

(** updates the player two 2 stuff and then passes that new players list into trade_update_player for the final updates*)
let rec trade_update_player2 players_list p1 p2 px_prop py_prop board cash acc=
  match players_list with
  |[]-> acc
  |player::t->
    begin
      if (player.id = p2) then (
        trade_update_player2 t p1 p2 px_prop py_prop board cash ({ 
            player with 
            score = player.score - cash -(get_property_price py_prop board)+ (get_property_price px_prop board);
            properties =  px_prop::(remove_helper player.properties py_prop [] 0);
            money = player.money-cash
          }::acc))
      else trade_update_player2 t p1 p2 px_prop py_prop board cash (player::acc)
    end

(** updates the player one 1 stuff and then passes that new players list into trade_update_player2 for the final updates*)
let rec trade_update_player players_list p1 p2 px_prop py_prop board cash acc=
  match players_list with
  |[]-> trade_update_player2 acc p1 p2 px_prop py_prop board cash []
  |player::t->
    begin
      if (player.id = p1) then (
        trade_update_player t p1 p2 px_prop py_prop board cash ({ 
            player with
            score = player.score + cash+ (get_property_price py_prop board)- (get_property_price px_prop board) ;
            properties =  py_prop::(remove_helper player.properties px_prop [] 0);
            money = player.money+cash
          }::acc))
      else trade_update_player t p1 p2 px_prop py_prop board cash (player::acc)
    end


(** takes in players: players p1: int, p2: int, p1_prop: string, p2_prop: 
    string, cash: int *)
(**  A player can trade a property for either cash or a property or 
              both. player1 is selling the property to player2 for cash or one 
              of player2's properties.
                 Example: So say I call helper_function p1 property_x p2 10 
                 property_y, then
                 1. Remove property_x from the list of p1's properties
                 2. Add property_x to the list of p2's properties
                 3. Add cash = 10 to p1's cash
                 4. Subtract cash = 10 from p2's cash
                 5. Add property_y to p1's properties
                 6. Remove property_y from p2's properties *)
let trade_new_player players p1 p2 px_prop py_prop board cash=
  { players with
    player_list = trade_update_player players.player_list p1 p2 px_prop py_prop 
        board cash []
  }

(** updates the current player's state if its their turn based on buy*)
let rec upgrade_update_current_player players_list player_names current_player_id board acc prop_loc=
  match players_list with
  |[]-> acc
  |player::t ->
    begin
      if (player.id = current_player_id) then (
        upgrade_update_current_player t player_names current_player_id board (
          { player with
            properties = ((get_property_name (get_property player.location board))::(player.properties));
            money = player.money-((get_price prop_loc board)/2)
          }::acc) prop_loc)
      else upgrade_update_current_player t player_names current_player_id board (player::acc) prop_loc
    end


(**updates the players state based on upgrade (ex: the money changes*)
let upgrade_new_player players board prop_loc= {
  players with
  player_list = upgrade_update_current_player players.player_list players.player_names players.current_player board [] prop_loc
}

(** updates the players state based on their turn (ex: location, score,
    potential property changes) and changes to the next player*)
let new_player players board= 
  let more_players= update_location_to_jail_main players board in { 
    players with
    player_list = more_players.player_list;
    current_player = (players.current_player +1) mod
                     (List.length players.player_names);
  }


(** gets the sum of the price value of a list of properties*)
let rec get_prop_value prop_lst board acc=
  match prop_lst with
  |[]-> acc
  |h::t-> get_prop_value t board ((get_property_price h board)+acc)


(** updates the given player's propeties, money, and score based on forfeit info BEFORE forfeit player is removed*)
let rec auction_update_current_player players_list board acc p1_id (fp_id:int) prop_lst amt=
  match players_list with
  |[]-> acc
  |player::t -> let prop_value= (get_prop_value prop_lst board 0) in
    begin
      print_endline ((string_of_int (player.id)) ^ " + " ^ (string_of_int (if player.id > fp_id then -1 else 0)));
      if (player.id = p1_id) then (
        auction_update_current_player t board ({ 
            player with
            id= player.id + (if player.id > fp_id then -1 else 0);
            score = (player.score + prop_value -amt);
            properties = prop_lst@(player.properties);
            money = player.money-amt
          }::acc) p1_id fp_id prop_lst amt)
      else auction_update_current_player t board (  { player with
                                                      id = player.id + (if player.id > fp_id then -1 else 0)
                                                    }::acc) p1_id fp_id prop_lst amt
    end

let string_color = function
  | Brown -> "brown"
  | LightBlue -> "light blue"
  | Magenta -> "magenta"
  | Red -> "red"
  | Orange -> "orange"
  | Yellow -> "yellow"
  | Green -> "green"
  | Blue -> "blue"
  | _ -> "no_color"

let rec property_list_to_string lst acc = 
  match lst with
  | [] -> acc
  | prop::t -> property_list_to_string t (acc^"{\n" ^ 
                                          "name : " ^ prop.name ^ "\n" ^
                                          "location : " ^ (string_of_int prop.location) ^ "\n" ^
                                          "price : " ^ (string_of_int prop.price) ^ "\n" ^
                                          "rent : " ^ (string_of_int prop.rent) ^ "\n" ^
                                          "color : " ^ (string_color prop.color) ^ "\n" ^
                                          "level : " ^ (string_of_int prop.level) ^ "\n" ^
                                          "type : property" ^ 
                                          "\n}\n")

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let string_of_player (player:player) = 
  "{\n" ^ 
  "id : " ^ (string_of_int player.id) ^ "\n" ^
  "score : " ^ (string_of_int player.score) ^ "\n" ^
  "location : " ^ (string_of_int player.location) ^ "\n" ^
  "properties : " ^ (pp_list pp_string player.properties) ^ "\n" ^
  "money : " ^ (string_of_int player.money) ^
  "\n}"


(** [pp_int i] pretty-prints int [i]. *)
let pp_int i = "\"" ^ (string_of_int i) ^ "\""

(** [pp_player_list_ids pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_player_list_ids pp_elt (lst:player list) =
  let pp_elts (lst:player list) =
    let rec loop n acc (lst:player list) =
      match lst with
      | [] -> acc
      | [h] -> acc ^ pp_elt (h.id)
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt (h1.id)) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** updates players with propeties, money, and score based on forfeit auction BEFORE forfeit player is removed*)
let auction_new_player players board (p1_id:int) (fp_id:int) (prop_lst:string list) (amt:int)= 
  print_endline (pp_player_list_ids pp_int players.player_list);
  { 
    players with
    player_list = auction_update_current_player players.player_list board [] p1_id fp_id prop_lst amt;
  }    

(** updates players with properties, money, and score after forfeit auction and the player is REMOVED*)
let forfeit_player (curr_player:player) (players_init:players) board (p1_id, prop_lst, amt) =
  let players = auction_new_player players_init board.property_tiles p1_id curr_player.id prop_lst amt in
  (* print_endline (string_of_int (List.length players.player_list)); *)
  print_endline (pp_player_list_ids pp_int players.player_list);
  let new_player_lst = remove_helper players.player_list curr_player [] 0 in
  print_endline (string_of_player curr_player);
  print_endline (string_of_int (List.length new_player_lst));
  { players with 
    player_list=new_player_lst;
    number_of_players=players.number_of_players-1;
    player_names=(remove_helper players.player_names (List.nth players.player_names curr_player.id) [] 0);
  }

(** updates players_list with a fixed tax fee deduction from the current player *)
let rec update_tax_player players_lst tax_amt acc current_player_id=
  match players_lst with
  |[]-> acc
  |player::t ->
    begin
      if (player.id = current_player_id) then (
        update_tax_player t tax_amt ({ 
            player with
            score = player.score-tax_amt;
            money = player.money-tax_amt
          }::acc) current_player_id)
      else update_tax_player t tax_amt (player::acc) current_player_id
    end

(** updates the players_list with deducting 10% tax from the currennt player *)
let rec update_player_percent_each players_lst acc current_player_id=
  match players_lst with
  |[]-> acc
  |player::t ->
    begin
      if (player.id = current_player_id)
      then (update_player_percent_each t ({ 
          player with
          score = player.score-(player.money/10);
          money = player.money-(player.money/10)
        }::acc) current_player_id)
      else update_player_percent_each t (player::acc) current_player_id
    end

(** updates players with 10% tax from the current player *)
let update_player_percent players board =  { 
  players with
  player_list = update_player_percent_each players.player_list [] players.current_player
}

(** updates player with a fixed tax deduction from the current player *)
let update_player_fixed_tax players board tax_amt=  { 
  players with
  player_list = update_tax_player players.player_list tax_amt [] players.current_player
}

(** prints out the elements of a list with each element on a new line *)
let rec list_printer lst =
  match lst with
  |[]-> ()
  |h::t -> print_endline h; list_printer t