(** [pp_int i] pretty-prints int [i]. *)
let pp_int i = "\"" ^ (string_of_int i) ^ "\""

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_player_list_ids pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_player_list_ids pp_elt (lst:Player.player list) =
  let pp_elts (lst:Player.player list) =
    let rec loop n acc (lst:Player.player list) =
      match lst with
      | [] -> acc
      | [h] -> acc ^ pp_elt (h.id)
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt (h1.id)) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [pp_player_list_names pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_player_list_names pp_elt (lst:string list) =
  let pp_elts (lst:string list) =
    let rec loop n acc (lst:string list) =
      match lst with
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

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

let rec remove_person_helper el acc = function
  | []-> List.rev acc
  | h::t when h = el -> remove_person_helper el acc t
  | h::t -> remove_person_helper el (h::acc) t

let player_list_mem_other (player:Player.player) (lst:Player.player list) : Player.player =
  if (List.nth lst 0).id = player.id then List.nth lst 1
  else List.nth lst 0

let find_next_valid_bidder (start:int) (out:int list) (lst:Player.players) : Player.player =
  print_endline (string_of_int start);
  print_endline (pp_player_list_ids pp_int lst.player_list);
  let rec helper index =
    if index = ((start-1) mod lst.number_of_players) then failwith "impossible"
    else
    if List.mem (List.nth lst.player_list index).id out then
      helper ((index+1) mod lst.number_of_players)
    else begin
      print_endline ("Getting the " ^ (string_of_int start) ^ "nth element of lst.player_list");
      List.nth lst.player_list index
    end
  in
  helper start

let rec loop (forfeit_player:Player.player) (players:Player.players) (out:int list) (bids:int list) (highest:int*int) (player_index:int) =
  (* (player that wins the bid, the properties they get, and the amount they pay) *)
  if List.length out = players.number_of_players-1 then begin
    print_endline ("Player " ^ (List.nth players.player_names (fst highest)) ^ " has won the auction!");
    (fst highest, forfeit_player.properties, snd highest)
  end
  else begin
    if List.mem player_index out then loop forfeit_player players out bids highest ((player_index+1) mod players.number_of_players)
    else begin
      print_endline ("Player " ^ (List.nth players.player_names player_index) ^
                     ", please enter either your bid or type \"forfeit\" to stop bidding.");
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> exit 0
      | s -> if s = "forfeit" then
          if snd (highest) = 0 then
            let highest_index = (find_next_valid_bidder ((player_index+1) mod players.number_of_players) out players).id in
            print_endline (string_of_int highest_index);
            loop forfeit_player players (player_index::out) bids (highest_index, 0) ((player_index+1) mod players.number_of_players)
          else
            loop forfeit_player players (player_index::out) bids highest ((player_index+1) mod players.number_of_players)
        else if is_int s then
          if (int_of_string s) <= snd highest then begin
            print_endline ("Please enter a bid that is higher than the
            current-highest bid");
            print_string  "> ";
            loop forfeit_player players out bids highest player_index
          end
          else 
            let new_highest = (player_index, int_of_string s) in
            loop forfeit_player players out (replace bids player_index (int_of_string s)) new_highest ((player_index+1) mod players.number_of_players)
        else begin
          print_endline "Please enter either a number for your bid or \"forfeit\" to stop bidding.";
          print_string  "> ";
          loop forfeit_player players out bids highest player_index
        end
    end
  end

let auction (forfeit_player:Player.player) (players:Player.players) (removing_player:bool) =
  if removing_player && List.length players.player_list = 2 then begin
    print_endline ("Player " ^ (List.nth players.player_names (player_list_mem_other forfeit_player players.player_list).id) ^
                   " wins!");
    exit 0;
  end
  else if not removing_player && List.length players.player_list = 2 then begin
    let winner_id = (player_list_mem_other forfeit_player players.player_list).id in
    (winner_id, forfeit_player.properties, 0)
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

let rec loop_prop (forfeit_player:Player.player) (players:Player.players) (out:int list) (bids:int list) (highest:int*int) (player_index:int) (prop:Board.property_tile) =
  (* (player that wins the bid, the properties they get, and the amount they pay) *)
  if List.length out = players.number_of_players-1 then begin
    print_endline ("Player " ^ (List.nth players.player_names (fst highest)) ^ " has won the auction!");
    (fst highest, [prop.name], snd highest)
  end
  else begin
    if List.mem player_index out then loop_prop forfeit_player players out bids highest ((player_index+1) mod players.number_of_players) prop
    else begin
      print_endline ("Player " ^ (List.nth players.player_names player_index) ^
                     ", please enter either your bid or type \"forfeit\" to stop bidding.");
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> exit 0
      | s -> if s = "forfeit" then
          if snd (highest) = 0 then
            let highest_index = (find_next_valid_bidder ((player_index+1) mod players.number_of_players) out players).id in
            print_endline (string_of_int highest_index);
            loop_prop forfeit_player players (player_index::out) bids (highest_index, 0) ((player_index+1) mod players.number_of_players) prop
          else
            loop_prop forfeit_player players (player_index::out) bids highest ((player_index+1) mod players.number_of_players) prop
        else if is_int s then
          if (int_of_string s) <= snd highest then begin
            print_endline ("Please enter a bid that is higher than the
            current-highest bid");
            print_string  "> ";
            loop_prop forfeit_player players out bids highest player_index prop
          end
          else 
            let new_highest = (player_index, int_of_string s) in
            loop_prop forfeit_player players out (replace bids player_index (int_of_string s)) new_highest ((player_index+1) mod players.number_of_players) prop
        else begin
          print_endline "Please enter either a number for your bid or \"forfeit\" to stop bidding.";
          print_string  "> ";
          loop_prop forfeit_player players out bids highest player_index prop
        end
    end
  end

let auction_prop (forfeit_player:Player.player) (players:Player.players) (tile:Indices.tile_object) : (int*string list*int) =
  match tile with
  | PropertyTile prop -> begin
      if List.length players.player_list = 2 then begin
        let winner_id = (player_list_mem_other forfeit_player players.player_list).id in
        (winner_id, [prop.name], 0)
      end
      else begin
        print_endline ("The bidding will begin at 0.");
        if forfeit_player.id = 0 then begin
          print_endline ("Player " ^ (List.nth players.player_names 1) ^ " will bid first.");
        end
        else
          print_endline ("Player " ^ (List.nth players.player_names 0) ^ " will bid first.");
        print_string  "> ";
        loop_prop forfeit_player players [forfeit_player.id] [] (0, 0) 0 prop;
      end
    end
  | _ -> failwith "impossible"