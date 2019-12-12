(** [replace lst i v] is a list [lst] that has it's [i]th entry replaced by
    the value [v] *)
let replace lst i v : 'a list =
  let rec helper i v count acc = function
    | [] -> (List.rev acc)
    | h::t -> if count = i then helper i v (count+1) (v::acc) t
      else helper i v (count+1) (h::acc) t
  in
  helper i v 0 [] lst

(** [is_int s] is a boolean representing whether or not the string [s] is
    an int *)
let is_int s =
  match int_of_string_opt s with
  | None -> false
  | _ -> true

(** [player_list_mem_other player lst] is, given a list [lst] of players of
    length 2, and given a player [player], the other player in [lst] *)
let player_list_mem_other (player:Player.player) (lst:Player.player list) :
  Player.player =
  if (List.nth lst 0).id = player.id then List.nth lst 1
  else List.nth lst 0

(** [find_next_valid_bidder start out lst] is the next player that is still
    in an auction. The function begins by scanning through the list [lst] of
    players at the index [start], and at each index, checks whether that
    player's is in the list [out], which contains the ids of players no
    longer in the auction. *)
let find_next_valid_bidder (start:int) (out:int list) (lst:Player.players) :
  Player.player =
  let rec helper index =
    if index = ((start-1) mod lst.number_of_players) then failwith "impossible"
    else
    if List.mem (List.nth lst.player_list index).id out then
      helper ((index+1) mod lst.number_of_players)
    else begin
      List.nth lst.player_list index
    end
  in
  helper start

(** [loop forfeit_player players out bids highest player_index] is the tuple
    representing the id the of the player that wins the auction, the properties
    that player has won, and the price the player paid for those properties. 
    The auctions that [loop] deals with are auctions resulting from a player 
    [forfeit_player] forfeiting the game. This either happens when a player ends
    their turn with a negative amount of money or if they quit the game. *)
let rec loop (forfeit_player:Player.player) (players:Player.players)
    (out:int list) (bids:int list) (highest:int*int) (player_index:int) =
  if List.length out = players.number_of_players-1 then begin
    print_endline ("Player " ^ (List.nth players.player_names (fst highest)) ^
                   " has won the auction!");
    (fst highest, forfeit_player.properties, snd highest)
  end
  else begin
    if List.mem player_index out then loop forfeit_player players out bids
        highest ((player_index+1) mod players.number_of_players)
    else begin
      print_endline ("Player " ^ (List.nth players.player_names player_index) ^
                     ", please enter either your bid or type \"forfeit\" to stop
                     bidding.");
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> exit 0
      | s -> if s = "forfeit" then
          loop_forfeit_helper forfeit_player players out bids highest
            player_index
        else if is_int s then
          loop_bid_helper forfeit_player players out bids highest player_index s
        else begin
          print_endline ("Please enter either a number for your bid or
          \"forfeit\" to stop bidding.");
          print_string  "> ";
          loop forfeit_player players out bids highest player_index
        end
    end
  end

(** [loop_forfeit_helper players out bids highest player_index] is the logic
    that handles when a player types "forfeit" during an auction. *)
and loop_forfeit_helper (forfeit_player:Player.player) (players:Player.players)
    (out:int list) (bids:int list) (highest:int*int) (player_index:int) =
  if snd (highest) = 0 then
    let highest_index = (
      find_next_valid_bidder
        ((player_index+1) mod players.number_of_players) out players).id
    in
    loop forfeit_player players (player_index::out) bids
      (highest_index, 0)
      ((player_index+1) mod players.number_of_players)
  else
    loop forfeit_player players (player_index::out) bids highest
      ((player_index+1) mod players.number_of_players)

(** [loop_bid_helper players out bids highest player_index s] is the logic that
    handles when a player types a bid [s] during an auction. *)
and loop_bid_helper (forfeit_player:Player.player) (players:Player.players)
    (out:int list) (bids:int list) (highest:int*int) (player_index:int)
    (s:string) =
  if (int_of_string s) <= snd highest then begin
    print_endline ("Please enter a bid that is higher than the
            current-highest bid");
    print_string  "> ";
    loop forfeit_player players out bids highest player_index
  end
  else 
    let new_highest = (player_index, int_of_string s) in
    loop forfeit_player players out (replace bids player_index
                                       (int_of_string s)) new_highest
      ((player_index+1) mod players.number_of_players)

(** [auction forfeit_player players] is an auction resulting from a player 
    [forfeit_player] forfeiting the game. This either happens when a player ends
    their turn with a negative amount of money or if they quit the game. *)
let auction (forfeit_player:Player.player) (players:Player.players)
    (quitting_player:bool) =
  if List.length players.player_list = 2 then begin
    print_endline (
      "Player " ^
      (List.nth players.player_names
         (player_list_mem_other forfeit_player players.player_list).id) ^
      " wins!");
    exit 0;
  end
  else begin
    let saying = (if quitting_player then "quit" else "gone bankrupt") in
    print_endline ("Player " ^ (List.nth players.player_names forfeit_player.id)
                   ^ " has " ^ saying ^ ". All of their properties will be
                   auctioned as one group of properties. The bidding will begin
                   at 0.");
    if forfeit_player.id = 0 then begin
      print_endline ("Player " ^ (List.nth players.player_names 1) ^
                     " will bid first.");
    end
    else
      print_endline ("Player " ^ (List.nth players.player_names 0) ^
                     " will bid first.");
    print_string  "> ";
    loop forfeit_player players [forfeit_player.id] [] (0, 0) 0;
  end

(** [loop_prop forfeit_player players out bids highest player_index prop] is the
    tuple representing the id the of the player that wins the auction, the 
    properties that player has won, and the price the player paid for those
    properties. The auctions that [loop_prop] deals with are auctions resulting
    from a player [forfeit_player] choosing not to buy a property they have
    landed on. This property is represented by the property tile [prop]. *)
let rec loop_prop (forfeit_player:Player.player) (players:Player.players)
    (out:int list) (bids:int list) (highest:int*int) (player_index:int)
    (prop:Board.property_tile) =
  if List.length out = players.number_of_players-1 then begin
    print_endline ("Player " ^ (List.nth players.player_names (fst highest)) ^
                   " has won the auction!");
    (fst highest, [prop.name], snd highest)
  end
  else begin
    if List.mem player_index out then loop_prop forfeit_player players out bids
        highest ((player_index+1) mod players.number_of_players) prop
    else begin
      print_endline ("Player " ^ (List.nth players.player_names player_index) ^
                     ", please enter either your bid or type \"forfeit\" to stop
                     bidding.");
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> exit 0
      | s -> if s = "forfeit" then
          if snd (highest) = 0 then
            let highest_index = (
              find_next_valid_bidder
                ((player_index+1) mod
                 players.number_of_players) out players).id in
            print_endline (string_of_int highest_index);
            loop_prop forfeit_player players (player_index::out) bids
              (highest_index, 0)
              ((player_index+1) mod players.number_of_players) prop
          else
            loop_prop forfeit_player players (player_index::out) bids highest
              ((player_index+1) mod players.number_of_players) prop
        else if is_int s then
          if (int_of_string s) <= snd highest then begin
            print_endline ("Please enter a bid that is higher than the
            current-highest bid");
            print_string  "> ";
            loop_prop forfeit_player players out bids highest player_index prop
          end
          else 
            let new_highest = (player_index, int_of_string s) in
            loop_prop forfeit_player players out
              (replace bids player_index(int_of_string s)) new_highest
              ((player_index+1) mod players.number_of_players) prop
        else begin
          print_endline ("Please enter either a number for your bid or
          \"forfeit\" to stop bidding.");
          print_string  "> ";
          loop_prop forfeit_player players out bids highest player_index prop
        end
    end
  end

(** [auction_prop forfeit_player players] is an auction resulting
    from a player [forfeit_player] choosing not to buy a property they have
    landed on. This property is represented by the tile object [tile]. *)
let auction_prop (forfeit_player:Player.player) (players:Player.players)
    (tile:Indices.tile_object) : (int*string list*int) =
  match tile with
  | PropertyTile prop -> begin
      if List.length players.player_list = 2 then begin
        let winner_id =
          (player_list_mem_other forfeit_player players.player_list).id in
        print_endline ("Player " ^ (List.nth players.player_names winner_id)
                       ^
                       " has won the auction!");
        (winner_id, [prop.name], 0)
      end
      else begin
        print_endline ("The bidding will begin at 0.");
        if forfeit_player.id = 0 then begin
          print_endline ("Player " ^ (List.nth players.player_names 1) ^
                         " will bid first.");
        end
        else
          print_endline ("Player " ^ (List.nth players.player_names 0) ^
                         " will bid first.");
        print_string  "> ";
        loop_prop forfeit_player players [forfeit_player.id] [] (0, 0) 0 prop;
      end
    end
  | _ -> failwith "impossible"