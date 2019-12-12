open OUnit2
open Board
open Indices
open Player
(* Our test plan:

   We wrote black box and glass box tests to ensure our code is correct. 
   Another person in the team wrote black box tests for each member’s code. 
   And the author of the code was responsible for adding glass box tests in 
   case some edge cases were missed in the other person's test suite because the 
   author is familiar with the code.

   We divide our tests below based on the functional component of our codebase
   it is testing. Each test simulates a part of the gameplay and calls the 
   functions that implement it. We have categorized them below as OUnit tests
   and manual terminal tests.

   OUnit tests:
   1. Parsing the board - board.ml
   2. Converting the indices into tiles - indices.ml
   3. Commands 
    1. Buy
    2. Upgrade
    3. Trade

   Manual tests:
   1. Commands (manually tested since they all do something very simple and there
   is not much to test for them) -
    1. Quit
    2. Roll
    3. Endturn
    4. Help
    5. Inventory

   Proof of correctness: 
   Our pattern match in play_game_recursively in main.ml is exhaustive and 
   contains all the commands a player can enter. As explained above, we have 
   tested the correctness of the implementations of all these commands. We have 
   also tested the step prior to the pattern matching that includes the board 
   parsing and mapping of indices to tiles. Nothing comes after the pattern 
   matching. So we believe we have tested all possible workflows and components.

*)

let board1 = Yojson.Basic.from_file "board1.json" |> from_json

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

let string_of_tax = function
  | IncomeTax -> "income_tax"
  | LuxuryTax -> "luxury_tax"

let string_of_corner = function
  | Go -> "go"
  | JailJustVisiting -> "jail_just_visiting"
  | FreeParking -> "free_parking"
  | GoToJail -> "go_to_jail"

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

let print_tile prop =
  match prop with
  | None -> "None"
  | Some tile_type -> match tile_type with
    | Indices.PropertyTile prop ->
      "{\n" ^ 
      "name : " ^ prop.name ^ "\n" ^
      "location : " ^ (string_of_int prop.location) ^ "\n" ^
      "price : " ^ (string_of_int prop.price) ^ "\n" ^
      "rent : " ^ (string_of_int prop.rent) ^ "\n" ^
      "color : " ^ (string_color prop.color) ^ "\n" ^
      "level : " ^ (string_of_int prop.level) ^ "\n" ^
      "type : property" ^ 
      "\n}"
    | Indices.CardTile card ->
      "{\n" ^ 
      "card_tile_name : " ^ card.card_tile_name ^ "\n" ^
      "location : " ^ (string_of_int card.location) ^ 
      "\n}"
    | Indices.TaxTile tax ->
      "{\n" ^ 
      "tax_tile_type : " ^ (string_of_tax tax.tax_tile_type) ^ "\n" ^
      "location : " ^ (string_of_int tax.location) ^ 
      "\n}"
    | Indices.CornerTile corner ->
      "{\n" ^
      "name : " ^ (string_of_corner corner.corner_tile_type) ^ "\n" ^
      "location : " ^ (string_of_int corner.location) ^ 
      "\n}"

let make_indices_test
    (name : string)
    (i : int) 
    (board : Board.t) 
    (expected_output : Indices.tile_object option) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Indices.return_tile i board) ~printer:print_tile)

let indices_tests = [
  make_indices_test "should parse a corner tile correctly" 0 board1 (
    Some (Indices.CornerTile ({
        corner_tile_type = Board.Go;
        location = 0
      }))
  );
  make_indices_test "should parse a property correctly" 1 board1 (
    Some (Indices.PropertyTile ({
        name = "Mediterranean Avenue";
        location = 1;
        price = 60;
        rent = 2;
        color = Board.Brown;
        level = 0;
        tile_type = Board.Property;
        owner = -1;
      }))
  );
  make_indices_test "should parse a card tile correctly" 2 board1 (
    Some (Indices.CardTile ({
        card_tile_name = "community_chest";
        location = 2
      }))
  );
  make_indices_test "should parse a tax tile correctly" 4 board1 (
    Some (Indices.TaxTile ({
        tax_tile_type = Board.IncomeTax;
        location = 4
      }))
  );
  make_indices_test "should parse a railroad correctly" 5 board1 (
    Some (Indices.PropertyTile ({
        name = "Reading Railroad";
        location = 5;
        price = 200;
        rent = 25;
        color = Board.NoColor;
        level = -1;
        tile_type = Board.Railroad;
        owner = -1;
      }))
  );
  make_indices_test "should parse a utility correctly" 28 board1 (
    Some (Indices.PropertyTile ({
        name = "Water Works";
        location = 28;
        price = 150;
        rent = -1;
        color = Board.NoColor;
        level = -1;
        tile_type = Board.Utility;
        owner = -1;
      }))
  );
]


let buy_all id board =
  let props = board.property_tiles in
  let rec helper acc = function
    | [] -> acc
    | h::t -> helper ({h with owner=id}::acc) t
  in
  {board with property_tiles=(helper [] props)}

let bought_props1 = buy_all 0 board1;;

let buy_first_set id board =
  let props = board.property_tiles in
  let rec helper acc (props:Board.property_tile list) =
    match props with
    | [] -> acc
    | h::t -> if h.location = 1 || h.location = 3 then
        helper ({h with owner=id}::acc) t
      else
        helper acc t
  in
  {board with property_tiles=(helper [] props)}

let bought_props1 = buy_all 0 board1;;
let bought_first_set = buy_first_set 0 board1;;

type color_groups = {brown:int; light_blue:int; magenta:int; orange:int;
                     red:int; yellow:int; green:int; blue:int}
let print_color_groups (cg:Upgrade.color_groups) = 
  "{brown="^(string_of_int cg.brown)^"; light_blue="^
  (string_of_int cg.light_blue)^"; magenta="
  ^(string_of_int cg.magenta)^"; orange="^(string_of_int cg.orange)^"; red="^
  (string_of_int cg.red)^"; yellow="^(string_of_int cg.yellow)^"; green="^
  (string_of_int cg.green)^"; blue="^(string_of_int cg.blue)^"}"

let make_get_color_groups_test
    (name : string)
    (id : int) 
    (board : Board.t) 
    (expected_output : Upgrade.color_groups) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Upgrade.get_color_groups id board) ~printer:print_color_groups)

let rec print_upgradeable_properties (lst:(string*int) list) =
  let rec helper acc = function
    | [] -> acc
    | (name,id)::t -> if (List.length t) = 0 then
        helper (acc ^ name ^ ":" ^ (string_of_int id)) t
      else if (List.length t) = 1 then
        helper (acc ^ name ^ ":" ^ (string_of_int id) ^ ", and ") t
      else
        helper (acc ^ name ^ ":" ^ (string_of_int id) ^ ", ") t
  in
  helper "" lst

let make_get_upgradeable_properties_test
    (name : string)
    (id : int) 
    (board : Board.t)
    (cg : Upgrade.color_groups)
    (expected_output : (string * int) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Upgrade.get_upgradeable_properties id board cg) 
        ~printer:print_upgradeable_properties)

let get_properties id (board:Board.t) =
  let rec helper acc = function
    | index -> if index = 40 then acc else
        begin
          match Indices.return_tile index board with
          | None -> helper acc (index+1)
          | Some tile -> begin
              match tile with
              | Indices.PropertyTile propTile -> if propTile.owner == id then
                  helper (propTile::acc) (index+1)
                else
                  helper acc (index+1)
              | _ -> helper acc (index+1)
            end
        end
  in
  helper [] 1

let print_properties (props:Board.property_tile list) = 
  let rec helper acc = function
    | [] -> acc
    | h::t -> if (List.length t) = 0 then
        helper (acc ^ (print_tile (Some (Indices.PropertyTile h)))) t
      else if (List.length t) = 1 then
        helper (acc ^ (print_tile (Some (Indices.PropertyTile h))) ^ ", and ") t
      else
        helper (acc ^ (print_tile (Some (Indices.PropertyTile h))) ^ ", ") t
  in
  helper "" props

let make_update_level_tests
    (name : string)
    (index : int) 
    (props : Board.property_tile list)
    (expected_output : Board.property_tile list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Upgrade.update_level index props) ~printer:print_properties)

let upgrade_tests = [
  make_get_color_groups_test "should be all properties" 0 bought_props1 {
    brown=2; light_blue=3; magenta=3; orange=3;
    red=3; yellow=3; green=3; blue=2
  };
  make_get_color_groups_test "should be the first set of properties" 0 
    bought_first_set {
    brown=2; light_blue=0; magenta=0; orange=0;
    red=0; yellow=0; green=0; blue=0
  };
  make_get_color_groups_test "should be none of the properties" 0 board1 {
    brown=0; light_blue=0; magenta=0; orange=0;
    red=0; yellow=0; green=0; blue=0
  };
  make_get_upgradeable_properties_test "should be the first set of properties" 
    0 bought_first_set {
    brown=2; light_blue=0; magenta=0; orange=0;
    red=0; yellow=0; green=0; blue=0
  } (List.rev [("Mediterranean Avenue",0);("Baltic Avenue",0)]);
  make_get_upgradeable_properties_test "should be none of the properties" 0 
    board1 {
    brown=0; light_blue=0; magenta=0; orange=0;
    red=0; yellow=0; green=0; blue=0
  } [];
  make_update_level_tests "should be none of the properties" 0 (get_properties 
                                                                  0 board1) [];
  make_update_level_tests "Nothing should be upgraded" 0 (get_properties 0 
                                                            bought_first_set) (List.rev [
      {name="Baltic Avenue";location=3;price=60;rent=4;color=Board.Brown;
       level=0;tile_type=Board.Property;owner=0};
      {name="Mediterranean Avenue";location=1;price=60;rent=2;color=Board.Brown;
       level=0;tile_type=Board.Property;owner=0}
    ]);
  make_update_level_tests "Mediterranean Avenue should be upgraded once" 1 
    (get_properties 0 bought_first_set) (List.rev [
        {name="Baltic Avenue";location=3;price=60;rent=4;color=Board.Brown;
         level=0;tile_type=Board.Property;owner=0};
        {name="Mediterranean Avenue";location=1;price=60;rent=2;color=Board.Brown;
         level=1;tile_type=Board.Property;owner=0}
      ]);
  make_update_level_tests "Baltic Avenue should be upgraded once" 3 
    (get_properties 0 bought_first_set) (List.rev [
        {name="Baltic Avenue";location=3;price=60;rent=4;color=Board.Brown;
         level=1;tile_type=Board.Property;owner=0};
        {name="Mediterranean Avenue";location=1;price=60;rent=2;color=Board.Brown;
         level=0;tile_type=Board.Property;owner=0}
      ]);
]


let make_player1 = {
  id = 0;
  score = 1600;
  location = 0;
  properties = ["Oriental Avenue"];
  money = 1500;
}
let make_player2 = {
  id = 1;
  score = 1600;
  location = 0;
  properties = ["Vermont Avenue"];
  money = 1500;
}

let make_players1 =
  {
    player_list = [make_player1; make_player2];
    current_player = 0;
    number_of_players = 2;
    player_names = ["p1"; "p2"];
    jail_list = []
  }

let make_player3 = {
  id = 0;
  score = 1650;
  location = 0;
  properties = ["Vermont Avenue"];
  money = 1550;
}
let make_player4 = {
  id = 1;
  score = 1550;
  location = 0;
  properties = ["Oriental Avenue"];
  money = 1450;
}

let make_trade_test
    (name : string)
    (p_info : Player.players) 
    (p1 : int)
    (p2 : int)
    (prop1 : string)
    (prop2 : string)
    (board : Board.t) 
    (score : int)
    (expected_output : Player.players) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Player.trade_new_player p_info p1 p2 prop1 prop2 board.property_tiles 
           score)
    )

let trade_tests = [
  make_trade_test "" make_players1 0 1 "Oriental Avenue" "Vermont Avenue"
    board1 50 {
    player_list = [make_player3; make_player4];
    current_player = 0;
    number_of_players = 2;
    player_names = ["p1"; "p2"];
    jail_list = []
  };
]

let make_find_next_valid_bidder_test
    (name : string)
    (start : int)
    (out : int list)
    (lst : Player.players)
    (expected_output : Player.player) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Auction.find_next_valid_bidder start out lst)
    )

let make_player_list_mem_other_test
    (name : string)
    (player : Player.player)
    (lst : Player.player list)
    (expected_output : Player.player) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Auction.player_list_mem_other player lst)
    )

let auction_tests = [
  make_find_next_valid_bidder_test "should be Player 3" 2 [0] {
    player_list=[
      {
        id=0;
        score=(~-60);
        location=3;
        properties=["Baltic Avenue"];
        money=(~-60);
      };
      {
        id=1;
        score=0;
        location=0;
        properties=[];
        money=0;
      };
      {
        id=2;
        score=0;
        location=0;
        properties=[];
        money=0;
      }
    ];
    current_player=0;
    number_of_players=3;
    player_names=["1"; "2"; "3"];
    jail_list=[];
  } {
    id=2;
    score=0;
    location=0;
    properties=[];
    money=0;
  };

  make_find_next_valid_bidder_test "should be Player 3" 2 [1] {
    player_list=[
      {
        id=0;
        score=0;
        location=0;
        properties=[];
        money=0;
      };
      {
        id=1;
        score=(~-60);
        location=3;
        properties=["Baltic Avenue"];
        money=(~-60);
      };
      {
        id=2;
        score=0;
        location=0;
        properties=[];
        money=0;
      }
    ];
    current_player=1;
    number_of_players=3;
    player_names=["1"; "2"; "3"];
    jail_list=[];
  } {
    id=2;
    score=0;
    location=0;
    properties=[];
    money=0;
  };

  make_find_next_valid_bidder_test "should be Player 1" 0 [2] {
    player_list=[
      {
        id=0;
        score=0;
        location=0;
        properties=[];
        money=0;
      };
      {
        id=1;
        score=0;
        location=0;
        properties=[];
        money=0;
      };
      {
        id=2;
        score=(~-60);
        location=3;
        properties=["Baltic Avenue"];
        money=(~-60);
      }
    ];
    current_player=2;
    number_of_players=3;
    player_names=["1"; "2"; "3"];
    jail_list=[];
  } {
    id=0;
    score=0;
    location=0;
    properties=[];
    money=0;
  };

  make_player_list_mem_other_test "should be player 1" {
    id=2;
    score=(~-60);
    location=3;
    properties=["Baltic Avenue"];
    money=(~-60);
  }
    [
      {
        id=0;
        score=0;
        location=0;
        properties=[];
        money=0;
      };
      {
        id=2;
        score=(~-60);
        location=3;
        properties=["Baltic Avenue"];
        money=(~-60);
      }
    ]
    {
      id=0;
      score=0;
      location=0;
      properties=[];
      money=0;
    };

  make_player_list_mem_other_test "should be player 3" {
    id=0;
    score=(~-60);
    location=3;
    properties=["Baltic Avenue"];
    money=(~-60);
  }
    [
      {
        id=0;
        score=(~-60);
        location=3;
        properties=["Baltic Avenue"];
        money=(~-60);
      };
      {
        id=2;
        score=0;
        location=0;
        properties=[];
        money=0;
      }
    ]
    {
      id=2;
      score=0;
      location=0;
      properties=[];
      money=0;
    };
]

let make_player1 = {
  id = 0;
  score = 1500;
  location = 3;
  properties = [];
  money = 1500;
}
let make_player2 = {
  id = 1;
  score = 1500;
  location = 9;
  properties = [];
  money = 1500;
}
let make_player3 = {
  id = 0;
  score = 1500;
  location = 3;
  properties = ["Baltic Avenue"];
  money = 1440;
}

let make_player4 = {
  id = 1;
  score = 1500;
  location = 9;
  properties = ["Connecticut Avenue"];
  money = 1380;
}

let make_player5 = {
  id = 1;
  score = 1380;
  location = 11;
  properties = ["Connecticut Avenue"];
  money = 1380;
}

let make_player6 = {
  id = 1;
  score = 1380;
  location = 11;
  properties = [ "St. Charles Place"; "Connecticut Avenue"];
  money = 1240;
}

let make_buy_test
    (name : string)
    (player_info : Player.players)
    (board : Board.t) 
    (expected_output : Player.players) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Player.buy_new_player player_info board)
    )

let make_players20 =
  {
    player_list = [make_player1; make_player2];
    current_player = 0;
    number_of_players = 2;
    player_names = ["p1"; "p2"];
    jail_list = []
  }

let make_players21 =
  {
    player_list = [make_player2];
    current_player = 1;
    number_of_players = 1;
    player_names = ["p1"];
    jail_list = []
  }

let make_players22 =
  {
    player_list = [make_player1; make_player2];
    current_player = 1;
    number_of_players = 2;
    player_names = ["p0"; "p1"];
    jail_list = []
  }

let make_players23 =
  {
    player_list = [make_player1; make_player5];
    current_player = 1;
    number_of_players = 2;
    player_names = ["p0"; "p1"];
    jail_list = []
  }

let buy_tests = [
  make_buy_test "first property curr_player 0" make_players20 board1 {
    player_list = [make_player2; make_player3];
    current_player = 0;
    number_of_players = 2;
    player_names = ["p1"; "p2"];
    jail_list = []
  };
  make_buy_test "first property curr_player 1 single player" make_players21 
    board1 {
    player_list = [make_player4];
    current_player = 1;
    number_of_players = 1;
    player_names = ["p1"];
    jail_list = []
  };
  make_buy_test "first property curr_player 1 two players" make_players22 
    board1 {
    player_list = [make_player4; make_player1];
    current_player = 1;
    number_of_players = 2;
    player_names = ["p0"; "p1"];
    jail_list = []
  };
  make_buy_test "nth property curr_player 1 two players" make_players23 
    board1 {
    player_list = [make_player6; make_player1];
    current_player = 1;
    number_of_players = 2;
    player_names = ["p0"; "p1"];
    jail_list = []
  };
]

let suite =
  "test suite for final project"  >::: List.flatten [
    indices_tests;
    upgrade_tests;
    trade_tests;
    auction_tests;
    buy_tests
  ]

let _ = run_test_tt_main suite