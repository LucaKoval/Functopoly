let replace lst i v : 'a list =
  let rec helper i v count acc = function
    | [] -> (List.rev acc)
    | h::t -> if count = i then helper i v (count+1) (v::acc) t
      else helper i v (count+1) (h::acc) t
  in
  helper i v 0 [] lst

let is_int s =
  match int_of_string_opt s with
  | None -> false
  | _ -> true

let rec loop (forfeit_player:Player.player) (players:Player.players) (out:int list) (bids:int list) (highest:int*int) (player_index:int) =
  (* (player that wins the bid, the properties they get, and the amount they pay) *)
  if List.length out = players.number_of_players-1 then (fst highest, forfeit_player.properties, snd highest)
  else begin
    if List.mem player_index out then loop forfeit_player players out bids highest ((player_index+1) mod players.number_of_players)
    else begin
      print_endline ("Player " ^ (List.nth players.player_names player_index) ^
                     ", please enter either your bid or type \"forfeit\" to stop bidding.");
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> exit 0
      | s -> if s = "forfeit" then
          loop forfeit_player players (player_index::out) bids highest ((player_index+1) mod players.number_of_players)
        else if is_int s then
          let new_highest = if int_of_string s > snd highest then (player_index, int_of_string s)
            else highest
          in
          loop forfeit_player players out (replace bids player_index (int_of_string s)) new_highest ((player_index+1) mod players.number_of_players)
        else begin
          print_endline "Please enter either a number for your bid or \"forfeit\" to stop bidding.";
          print_string  "> ";
          loop forfeit_player players out bids highest player_index
        end
    end
  end

let player_list_mem_other (player:Player.player) (lst:Player.players) : Player.player =
  if (List.nth lst.player_list 0).id = player.id then List.nth lst.player_list 1
  else List.nth lst.player_list 1

let auction (forfeit_player:Player.player) (players:Player.players) =
  if List.length players.player_list = 2 then begin
    print_endline ("Player " ^ (List.nth players.player_names (player_list_mem_other forfeit_player players).id) ^
                   " wins!");
    exit 0;
  end
  else begin
    print_endline ("Player " ^ (List.nth players.player_names forfeit_player.id) ^
                   " has gone bankrupt. All of their properties will be auctioned
                 as one group of properties. The bidding will begin at 0.");
    if forfeit_player.id = 0 then begin
      print_endline ("Player " ^ (List.nth players.player_names 1) ^ " will bid first.");
    end
    else
      print_endline ("Player " ^ (List.nth players.player_names 0) ^ " will bid first.");
    print_string  "> ";
    loop forfeit_player players [forfeit_player.id] [] (0, 0) 0;
  end