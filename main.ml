open Player
open Command 
open Board
open Yojson
open Indices
open Auction

(** [get_num_players] is the number of players  *)
let get_num_players = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Monopoly Game engine.\n");
  print_endline "How many players are playing the game? \n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> 0
  | no_players -> int_of_string no_players

(** gets current player name from players *)
let get_current_player_name players =
  List.nth players.player_names (players.current_player)

(** [get_player_names n] is the list of player names entered by the user *)
let rec get_player_names n acc = 
  match n with
  | 0 -> acc
  | _ -> 
    print_string "Please enter the game name for the next player \n";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> []
    | player_name -> if (List.mem player_name acc)
      then (print_string "Another player has taken this name. Please try again.\n";
            get_player_names n acc)
      else (if (String.contains player_name ' ') then 
              (print_string "A player name cannot contain spaces. Please try again.\n";
               get_player_names n acc)
            else get_player_names (n-1) (player_name::acc))

(** [print_string_list lst] prints out a list of strings [lst]*)
let rec print_string_list lst =
  match lst with
  | [] -> ()
  | h::t -> print_string h; print_string_list lst

(** list of commands to print in response to help input *)
let command_list =
  "\nHere's the list of commands you can \
   run:\n\
   roll: Rolls the dice for the next player.\n\
   endturn: Ends the current player's turn and moves the dice to the next 
   player.\n\
   help: Prints the list of commands you can run.\n\
   inventory <player_name>: Prints the inventory for <player_name>.\n\
   buy: Buys a property if you landed on one.\n\
   trade: Initiates a trade sequence.\n\
   upgrade: Displays upgradeable properties and then allows you to upgrade 
   them. \n\
   quit: Quits the game and displays the winner.\n"


(* ====== START TRADE HELPERS ======= *)

(** [valid_property all_p p] is true if the current player owns that property otherwise false *)
let valid_property (all_players:Player.players) property = 
  let curr_player = List.nth all_players.player_list all_players.current_player 
  in List.mem property curr_player.properties 

(** [valid_player p1 p2] is true if p2 is a valid player otherwise false *)
let valid_player trader1 trader2 = 
  List.mem (String.trim trader2) trader1.player_names

(** takes in a list of strings and gets the integer sum of the elements *)
let rec parse_cash = function
  | [] -> []
  | h::t -> try (int_of_string h)::(parse_cash t) 
    with Failure e -> (0)::(parse_cash t)

(** gives a list with the spaces, if any, removed *)
let rec remove_spaces = function
  | [] -> []
  | h::t -> (String.trim h)::(remove_spaces t)

(** gives a list with the zeros, if any, removed *)
let rec remove_zeroes = function
  | [] -> 0
  | h::t -> if h <> 0 then h else (remove_zeroes t)

(** gets a non-empty string if a string list otherwise gets empty string *)
let rec parse_property = function
  | [] -> ""
  | h::t -> if (is_int h) then (parse_property t) 
    else h

(** gets price value of property_to_trade *)
let parse_price str property_to_trade =
  if (String.contains str ',') then (let comma_split = String.split_on_char ',' 
                                         str in
                                     let spaces_removed = remove_spaces 
                                         comma_split in
                                     let cash = remove_zeroes (parse_cash 
                                                                 spaces_removed)
                                     in
                                     let property = parse_property 
                                         spaces_removed in 
                                     (cash, property, property_to_trade)
                                    ) else (try ((int_of_string (String.trim 
                                                                   str)), "", 
                                                 property_to_trade)
                                            with Failure e -> 
                                              (0, (String.trim str), 
                                               property_to_trade) 
                                           )

(** prints trade values cash, property, and trader2*)
let print_price cash property trader2 = 
  print_string "Cash: ";
  print_endline (string_of_int cash);
  print_string "Property: ";
  print_endline property;
  print_endline "";
  print_string trader2

(** Let the bargaining begin! *)
let rec bargaining trader1_price trader1 trader2 property_to_trade =
  let (cash, property, property_to_trade) =  parse_price trader1_price 
      property_to_trade in
  let () = print_price cash property trader2 in
  print_endline ", do you accept or reject this price?";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> exit 0
  | accept_or_reject -> 
    if (String.trim accept_or_reject) = "accept" then 
      (cash, property, property_to_trade)
    else if ((String.trim accept_or_reject) = "reject") then (
      print_string (get_current_player_name trader1);
      print_endline ", your price was rejected. Please propose a better price
        or type endbargain to call off the trade. \
                     Please enter the price in one of the following ways:\
                     1. An int cash value. Example: 10 \
                     2. The name of a property belonging to the player you're 
                     trading with. Example: x \
                     3. Int cash value followed by a comma and the desired 
                     property name. Example: 10, x
        ";
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> exit 0
      | better_price -> if (String.trim accept_or_reject) = "endbargain" then 
          (0, "", property_to_trade)
        else bargaining better_price trader1 trader2 property_to_trade
    )
    else (print_endline "Invalid response. Please re-enter your decision";
          bargaining trader1_price trader1 trader2 property_to_trade)

let rec property_trade (trader1:Player.players) =
  print_endline "Which property do you want to trade?";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> exit 0
  | property_to_trade -> 
    if (not(valid_property trader1 property_to_trade)) then 
      (print_endline "Invalid property. Please re-enter the name of the 
    property you want to trade.";
       property_trade trader1)
    else property_to_trade

let rec execute_trade trader1 trader2 =
  if (not(valid_player trader1 trader2)) 
  then (print_endline "Invalid player name. Please re-enter the name of the 
  player you want to trade with.";
        print_string  "> ";
        match read_line () with
        | exception End_of_file -> exit 0
        | trade_partner -> 
          execute_trade trader1 trade_partner)
  else let property_to_trade = property_trade trader1 in
    print_endline "What price do you want to trade at? \n\
                   (Please enter the price in one of the following ways:\n\
                   1. An int cash value. Example: 10 \n\
                   2. The name of a property belonging to the player you're 
                   trading with. Example: x \n\
                   3. Int cash value followed by a comma and the desired 
                   property name. Example: 10, x \n\
                   )";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> exit 0
    | str -> bargaining str trader1 trader2 property_to_trade

(* ======= END TRADE HELPERS ======= *)

(** gets property name*)
let buy_helper player_info board=
  Player.get_property_name (get_property (Player.get_current_location 
                                            player_info) board)


let rec get_player_id_from_name player_names name acc=
  match player_names with
  |[]-> acc
  |h::t when h = name -> acc
  |h::t -> get_player_id_from_name t name (acc+1)

let rec properties_to_string lst =
  let rec helper acc = function
    | [] -> acc
    | (name,_)::t -> if (List.length t) = 0 then
        helper (acc ^ name) t
      else if (List.length t) = 1 then
        helper (acc ^ name ^ ", and ") t
      else
        helper (acc ^ name ^ ", ") t
  in
  helper "" lst


let validate_command_order cmd1 cmd2 =
  ((String.trim cmd1 = "roll" && String.trim cmd2 = "roll")
   || (String.trim cmd1 = "endturn" && String.trim cmd2 = "endturn")
   || (String.trim cmd1 = "buy" && String.trim cmd2 = "roll")
   || (String.trim cmd1 = "buy" && String.trim cmd2 = "buy")
   || (String.trim cmd1 = "endturn" && String.trim cmd2 = "buy")
  )

let is_property tile = 
  match tile with
  | PropertyTile a -> true
  | _ -> false 

let rec tax_loop str_command player_info board =
  (print_endline "";print_string  "> ";
   match read_line () with
   | "percent" -> print_endline "We have taken 10% of your money muahaha"; Player.update_player_percent player_info board
   | "fixed" -> print_endline "We have taken $200 of your money for... tax... purposes... don't ask questions"; Player.update_player_fixed_tax player_info board 200
   | anything_else -> print_string "that is an invalid entry. Please choose percent or fixed";
     tax_loop str_command player_info board)

let malformed_helper prev_cmd str_command player_info board = 
  (print_endline "The command you entered was Malformed :(\
                  Please try again.";
   print_string  "> ";
   match read_line () with
   | exception End_of_file -> exit 0
   | str -> (prev_cmd, str,
             player_info, board))

let empty_helper prev_cmd str_command player_info board = 
  (print_endline "The command you entered was Empty.\
                  Please try again."; 
   print_string  "> ";
   match read_line () with
   | exception End_of_file -> exit 0;
   | str -> (prev_cmd, str, player_info,
             board)
  )
let quit_helper prev_cmd str_command player_info board = 
  let current_player = Player.get_current_player player_info in
  print_endline "Sad to see you go. Your properties will now be
    auctioned off.";
  let auction_info = Auction.auction current_player player_info true in
  let unsorted_post_forfeit_player_info = Player.forfeit_player current_player 
      player_info board auction_info in
  let post_forfeit_player_info = {
    unsorted_post_forfeit_player_info with
    player_list=(List.sort (fun x y -> x.id - y.id) unsorted_post_forfeit_player_info.player_list);
    current_player=unsorted_post_forfeit_player_info.current_player mod unsorted_post_forfeit_player_info.number_of_players;
  } in
  let current_name = (get_current_player_name post_forfeit_player_info) 
  in
  print_endline ("Player " ^ current_name ^ ", it's your turn now! Your 
          current location is "
                 ^ string_of_int (Player.get_current_location 
                                    post_forfeit_player_info));
  print_endline "";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> exit 0;
  | str -> (str_command, str ,
            post_forfeit_player_info, board)

let roll_helper prev_cmd str_command player_info board = 
  let unsorted_update_player_roll = (roll_new_player player_info board) in
  let update_player_roll = {unsorted_update_player_roll with player_list=(List.sort (fun x y -> x.id - y.id) unsorted_update_player_roll.player_list);} in
  print_endline (Auction.pp_player_list_ids Auction.pp_int update_player_roll.player_list);
  if ((Player.get_current_location update_player_roll) = 4) then (print_endline "You have landed on income tax! Please choose percent or fixed";
  let new_info = tax_loop str_command update_player_roll board in
  (print_endline "";print_string  "> ";
  match read_line () with
  | exception End_of_file -> exit 0;
  | str -> (str_command, str, new_info, board))
) else 
    (print_endline "";print_string  "> ";
     match read_line () with
     | exception End_of_file -> exit 0;
     | str -> (str_command, str, update_player_roll, board))

   let endturn_auction_helper prev_cmd str_command player_info board current_player= (
      let auction_info = Auction.auction current_player player_info true in
      let unsorted_post_forfeit_player_info = Player.forfeit_player current_player 
          player_info board auction_info in
      let post_forfeit_player_info = {
        unsorted_post_forfeit_player_info with
        player_list=(List.sort (fun x y -> x.id - y.id) unsorted_post_forfeit_player_info.player_list);
        current_player=unsorted_post_forfeit_player_info.current_player mod unsorted_post_forfeit_player_info.number_of_players;
      } in
      let current_name = (get_current_player_name post_forfeit_player_info) 
      in
      print_endline ("Player " ^ current_name ^ ", it's your turn now! Your 
          current location is "
                     ^ string_of_int (Player.get_current_location 
                                        post_forfeit_player_info));
      print_endline "";
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> exit 0;
      | str -> (str_command, str, post_forfeit_player_info, board))

let endturn_helper prev_cmd str_command player_info board =
  let current_player = Player.get_current_player player_info in
  if current_player.money < 0 then
    endturn_auction_helper prev_cmd str_command player_info board current_player
  else
    let unsorted_new_player_info = (Player.new_player player_info board) in 
    let new_player_info = {unsorted_new_player_info with player_list=(List.sort (fun x y -> x.id - y.id) unsorted_new_player_info.player_list);} in
    print_endline (Auction.pp_player_list_ids Auction.pp_int new_player_info.player_list);
    let current_name = (get_current_player_name new_player_info) in
    print_string current_name;
    (print_string ", it's your turn now! Your current location is "; 
     print_int (Player.get_current_location new_player_info);
     print_string  "> ";
     match read_line () with
     | exception End_of_file -> exit 0;
     | str -> (str_command, str, new_player_info, board)
    )

let help_helper prev_cmd str_command player_info board =
  print_endline command_list;
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> exit 0
  | str -> (prev_cmd, str, player_info, board)

let inventory_helper prev_cmd str_command player_info board player_name=
  (print_string player_name; print_endline "'s inventory:";
   let money = (Player.inventory_money_helper player_info.player_list 0 
                  (get_player_id_from_name player_info.player_names 
                     player_name 0)) in
   let list = (Player.inventory_helper player_info.player_list [] 
                 (get_player_id_from_name player_info.player_names 
                    player_name 0)) in 
   (Player.list_printer list);
   print_string player_name; print_string "'s money: "; print_int money;
   print_endline "";
   print_string  "> ";
   match read_line () with
   | exception End_of_file -> exit 0
   | str -> (prev_cmd, str, player_info, board))

let property_buy_helper prev_cmd str_command player_info board=
let unsorted_update_player_buy = (Player.buy_new_player player_info board) 
    in
    let update_player_buy = {unsorted_update_player_buy with player_list=(List.sort (fun x y -> x.id - y.id) unsorted_update_player_buy.player_list);} in
    let prop_name = buy_helper player_info board in (
      print_string "Congrats you now own ";
      print_endline (get_property_name (get_property 
                                          (get_current_location 
                                             player_info) board));
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> exit 0
      | str ->  (str_command, str, update_player_buy, 
                 (Board.buy_update_board board 
                    update_player_buy.current_player 
                    prop_name)))

let buy_helper prev_cmd str_command player_info board =
  let curr_location = get_current_location player_info in
  let property = get_property curr_location board in
  if (not(is_property property)) then (
    print_endline "You cannot buy on a tile that isn't a property! Please 
        enter a valid command.";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> exit 0
    | str -> (str_command, str, player_info, board))
  else if ((get_owner_id property) <> -1) then (
    print_endline "You cannot buy a property that is already owned by 
        someone! 
        However, you can trade if you'd like. Please enter a valid command.";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> exit 0
    | str -> (str_command, str, player_info, board))
  else property_buy_helper prev_cmd str_command player_info board

let property_no_helper prev_cmd str_command player_info board current_location=
let current_player = Player.get_current_player player_info in
    let prop = get_property current_location board in
    let prop_name = get_property_name prop in
    print_endline (prop_name ^ " will now be auctioned off.");
    let auction_info = Auction.auction_prop current_player player_info prop in
    let unsorted_post_forfeit_player_info = Player.auction_property_player current_player player_info board auction_info in
    let post_forfeit_player_info = {
      unsorted_post_forfeit_player_info with
      player_list=(List.sort (fun x y -> x.id - y.id) unsorted_post_forfeit_player_info.player_list);
      current_player=unsorted_post_forfeit_player_info.current_player mod unsorted_post_forfeit_player_info.number_of_players;
    } in let current_name = (get_current_player_name post_forfeit_player_info) in print_endline ("Player " ^ current_name ^ ", it's your turn now! Your current location is " ^ (string_of_int (Player.get_current_location 
    post_forfeit_player_info)));
    print_endline "";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> exit 0;
    | str -> (str_command, str, 
              post_forfeit_player_info, board)

let no_helper prev_cmd str_command player_info board =
  let current_location = get_current_location player_info in
  if (is_property (get_property current_location board)) then
    property_no_helper prev_cmd str_command player_info board current_location
  else begin
    print_string "> ";
    match read_line () with
    | exception End_of_file -> exit 0
    | str -> 
      (str_command, str, player_info, board)
  end

let property_upgrade_helper prev_cmd str_command player_info board name upgradeable_properties=
let index = List.assoc name upgradeable_properties in
          let unsorted_update_player_upgrade = (Player.upgrade_new_player player_info
                                                  board index) in
          let update_player_upgrade = {unsorted_update_player_upgrade with player_list=(List.sort (fun x y -> x.id - y.id) unsorted_update_player_upgrade.player_list);} in
          print_endline name;
          let new_board = {board with property_tiles = 
                                        (Upgrade.update_level
                                           index 
                                           board.property_tiles)} in
          print_endline ("You have upgraded " ^ name);
          print_string  "> ";
          match read_line () with
          | exception End_of_file -> exit 0
          | str -> (prev_cmd, str, update_player_upgrade, new_board)

let unupgradeable_helper prev_cmd str_command player_info board=
          begin
            print_endline "That is not a property you can upgrade.";
            print_string  "> ";
            match read_line () with
            | exception End_of_file -> exit 0
            | str -> (prev_cmd, str, player_info, board)
          end

let upgrade_helper prev_cmd str_command player_info board =
  let current_player_id = (player_info.current_player) in
  let color_groups = Upgrade.get_color_groups current_player_id board in
  let upgradeable_properties = Upgrade.get_upgradeable_properties 
      current_player_id board color_groups in
  let prop_string = properties_to_string upgradeable_properties in
  if (List.length upgradeable_properties = 0) then (
      print_endline "You do not have any upgradeable properties at this 
          moment.";
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> exit 0
      | str -> (prev_cmd, str, player_info, board))
  else (print_endline ("You can upgrade the following properties: " ^ 
        prop_string);
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> exit 0
      | name -> if List.mem_assoc name upgradeable_properties then
          property_upgrade_helper prev_cmd str_command player_info board name upgradeable_properties
        else unupgradeable_helper prev_cmd str_command player_info board)

let trade_helper prev_cmd str_command player_info board =
  (print_endline "print player property menu here";
   print_endline "Who do you want to trade with?";
   print_string  "> ";
   match read_line () with
   | exception End_of_file -> exit 0
   | trader2 -> let (cash, property, property_to_trade) = execute_trade 
                    player_info trader2 in
     let player1 = (get_player_id_from_name player_info.player_names 
                      (get_current_player_name player_info) 0) in
     let player2 = (get_player_id_from_name player_info.player_names 
                      trader2 0) in
     print_endline "Trade complete.";
     print_string  "> ";
     match read_line () with
     | exception End_of_file -> exit 0
     | str -> (
         let unsorted_updated_player_info = trade_new_player player_info player1
             player2 property_to_trade property (board.property_tiles) cash in 
         let updated_player_info = {unsorted_updated_player_info with player_list=(List.sort (fun x y -> x.id - y.id) unsorted_updated_player_info.player_list);} in
         (prev_cmd, str, updated_player_info, board)))

let parse_command prev_cmd str_command player_info board parsed_command=
  match parsed_command with
  | Quit -> quit_helper prev_cmd str_command player_info board
  | Roll -> roll_helper prev_cmd str_command player_info board
  | EndTurn -> endturn_helper prev_cmd str_command player_info board
  | Help -> help_helper prev_cmd str_command player_info board
  | Inventory player_name -> inventory_helper prev_cmd str_command player_info board player_name
  | Buy -> buy_helper prev_cmd str_command player_info board
  | No -> no_helper prev_cmd str_command player_info board
  | Upgrade -> upgrade_helper prev_cmd str_command player_info board
  | Trade -> trade_helper prev_cmd str_command player_info board

(** [play_game_recursively ]*)
let rec play_game_recursively (prev_cmd, str_command, player_info, board) =
  let player_info = {player_info with player_list=(List.sort (fun x y -> x.id - y.id) player_info.player_list);} in
  if validate_command_order prev_cmd str_command
  then (
    print_string "You cannot enter a(n) ";
    print_string str_command;
    print_string " after ";
    print_string prev_cmd;
    print_endline ". Please re-enter your next command (Hint: If you entered \
                   consecutive endturns, you may have forgotten to roll).";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> exit 0
    | str -> play_game_recursively (prev_cmd, str, player_info, board))
  else
    let parsed_command = (try Command.parse str_command with
        | Malformed -> (play_game_recursively (malformed_helper prev_cmd str_command player_info board))
        | Empty -> ((play_game_recursively (empty_helper prev_cmd str_command player_info board)))) in
    play_game_recursively (parse_command prev_cmd str_command player_info board parsed_command)


(** [start_game board] begins the game by taking the first player's input
    and then kicking off the recursive workflow by calling 
    [play_game_recursively] *)
let start_game board = 
  let num_players = get_num_players in
  let player_names = List.rev (get_player_names num_players []) in

  let initial_player_info = ANSITerminal.(print_string [blue]
                                            command_list);
    Player.to_players num_players player_names in
  print_endline (Auction.pp_player_list_ids Auction.pp_int initial_player_info.player_list);
  print_string (List.nth player_names 0);
  print_string " goes first: ";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> exit 0
  | str -> play_game_recursively ("", str, initial_player_info, board)

(**  [main_helper file_name] parses the board json *)
let rec main_helper file_name =
  try from_json (Basic.from_file file_name)
  with _ -> print_endline "Looks like something is \
                           wrong with the file \
                           name :( Please try again."; 
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> exit 0
    | file_name -> main_helper file_name

(** [main] is the entrypoint to the code. It asks for the board file name and 
    then calls [start_game] *)
let rec main () =
  Random.self_init ();
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> exit 0
  | file_name -> start_game (main_helper file_name)

(* Execute the game engine. *)
let x = main ()

