open Player

type object_phrase = string list

type command = 
  | Help
  | Roll
  | Inventory
  | Go of object_phrase
  | Buy
  | Sell 
  | Quit

(** [get_num_players] is the number of players  *)
let get_num_players = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Monopoly Game engine.\n");
  print_endline "How many players are playing the game? \n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> 0
  | no_players -> int_of_string no_players

(** takes in a 0 just for shits and returns a number rolled by 2 dice*)
let dice zero =
  (Random.int 6) + (Random.int 6) + 2 + zero

(** [get_player_names n] is the list of player names entered by the user *)
let rec get_player_names n = 
  match n with
  | 0 -> []
  | _ -> 
    print_string "Please enter the game name for the next player \n";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> []
    | player_name -> player_name::(get_player_names (n - 1))

(** [print_string_list lst] prints out a list of strings [lst]*)
let rec print_string_list lst =
  match lst with
  | [] -> ()
  | h::t -> print_string h; print_string_list lst

(** [play_game_recursively ]*)
let rec play_game_recursively player_info current_location board = ()


(** *)
let start_game file_name = 
  let num_players = get_num_players in
  let player_names = get_player_names num_players in

  let initial_player_info = print_endline "\nGreat! Now let's begin the game. \n\
                                           \nHere's the list of commands you can \
                                           run:";
    print_endline "Roll: Rolls the dice.\n\
                   Help: Prints the list of commands you can run.\n\
                   Inventory: Prints the inventory for the player whose turn it is.\n\
                   Buy: Buys a property if you landed on one.\n\
                   Sell <property_name>: Sells the <property_name> property you own.\n\
                   Quit: Quits the game and displays the winner.\n";
    Player.to_players num_players player_names in
  print_string "Player 1 goes first: ";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | str -> play_game_recursively initial_player_info "" ""

(* print_string_list player_names; print_string (string_of_int num_players) *)

let main () =
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> start_game file_name

(* Execute the game engine. *)
let () = main ()

