open OUnit2
open Board
open Indices

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
  "{brown="^(string_of_int cg.brown)^"; light_blue="^(string_of_int cg.light_blue)^"; magenta="
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
        (Upgrade.get_upgradeable_properties id board cg) ~printer:print_upgradeable_properties)

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
  make_get_color_groups_test "should be the first set of properties" 0 bought_first_set {
    brown=2; light_blue=0; magenta=0; orange=0;
    red=0; yellow=0; green=0; blue=0
  };
  make_get_color_groups_test "should be none of the properties" 0 board1 {
    brown=0; light_blue=0; magenta=0; orange=0;
    red=0; yellow=0; green=0; blue=0
  };
  make_get_upgradeable_properties_test "should be the first set of properties" 0 bought_first_set {
    brown=2; light_blue=0; magenta=0; orange=0;
    red=0; yellow=0; green=0; blue=0
  } (List.rev [("Mediterranean Avenue",0);("Baltic Avenue",0)]);
  make_get_upgradeable_properties_test "should be none of the properties" 0 board1 {
    brown=0; light_blue=0; magenta=0; orange=0;
    red=0; yellow=0; green=0; blue=0
  } [];
  make_update_level_tests "should be none of the properties" 0 (get_properties 0 board1) [];
  make_update_level_tests "Nothing should be upgraded" 0 (get_properties 0 bought_first_set) (List.rev [
      {name="Baltic Avenue";location=3;price=60;rent=4;color=Board.Brown;level=0;tile_type=Board.Property;owner=0};
      {name="Mediterranean Avenue";location=1;price=60;rent=2;color=Board.Brown;level=0;tile_type=Board.Property;owner=0}
    ]);
  make_update_level_tests "Mediterranean Avenue should be upgraded once" 1 (get_properties 0 bought_first_set) (List.rev [
      {name="Baltic Avenue";location=3;price=60;rent=4;color=Board.Brown;level=0;tile_type=Board.Property;owner=0};
      {name="Mediterranean Avenue";location=1;price=60;rent=2;color=Board.Brown;level=1;tile_type=Board.Property;owner=0}
    ]);
]

let suite =
  "test suite for final project"  >::: List.flatten [
    indices_tests;
    upgrade_tests;
  ]

let _ = run_test_tt_main suite