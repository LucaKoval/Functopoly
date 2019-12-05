(* 
let rec loop (forfeit_player:Player.player) (players:Player.players) (out:int list) (bids:int*int list) (highest:int*int) (player_index:int) =
  if List.length out = players.number_of_players-1 then (fst highest, forfeit_player.properties, snd highest)
  else begin
    if List.mem player_index out then loop forfeit_player players out bids highest ((player_index+1) mod players.number_of_players)
    else begin
      print_endline ("Player " ^ (List.nth players.player_names player_index) ^
                     ", please enter your either your bid or type \"forfeit\" to stop bidding.");
    end
  end *)




let auction (forfeit_player:Player.player) (players:Player.players) =
  print_endline ("Player " ^ (List.nth players.player_names forfeit_player.id) ^
                 "has gone bankrupt. All of their properties will be auctioned
                 as one group of properties. The bidding will begin at 0.");
  if forfeit_player.id = 0 then begin
    print_endline ("Player " ^ (List.nth players.player_names 1) ^ "will bid first.");
  end
  else
    print_endline ("Player " ^ (List.nth players.player_names 0) ^  "will bid first.");
  print_string  "> ";
  (* loop forfeit_player players [forfeit_player.id] [] (0, 0) 0; *)
  (* player that wins (id), list of properties, and price they paid *)