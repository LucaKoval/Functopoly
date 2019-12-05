let auction (player:Player.player) (players:Player.players) =
  print_endline ("Player " ^ (List.nth players.player_names player.id) ^
                 "has gone bankrupt. All of their properties will be auctioned
                 as one group of properties. The bidding will begin at 0.");
  print_endline ("Player " ^ (List.nth players.player_names 0) ^);
  print_string  "> ";