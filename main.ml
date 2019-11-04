open Player

type object_phrase = string list

type command = 
  | Help
  | Roll
  | Go of object_phrase
  | Buy
  | Sell 
  | Quit

(* type exit_room = {
   name : exit_name;
   id : room_id;
   }

   type each_room = {
   id : room_id;
   description: string;
   exits : exit_room list;
   }*)

(** [get_num_players] is the number of players  *)
let get_num_players = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Monopoly Game engine.\n");
  print_endline "How many players are playing the game? \n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> 0
  | no_players -> int_of_string no_players

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

(** *)
let play_game file_name = 
  let num_players = get_num_players in
  let player_names = get_player_names num_players in
  Player.to_players num_players player_names
(* print_string_list player_names; print_string (string_of_int num_players) *)

let main () =
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()

