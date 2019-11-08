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
        color = Board.Blue;
        level = -1;
        tile_type = Board.Railroad;
      }))
  );
  make_indices_test "should parse a utility correctly" 28 board1 (
    Some (Indices.PropertyTile ({
        name = "Water Works";
        location = 28;
        price = 150;
        rent = -1;
        color = Board.Blue;
        level = -1;
        tile_type = Board.Utility;
      }))
  );
]

let suite =
  "test suite for final project"  >::: List.flatten [
    indices_tests;
  ]

let _ = run_test_tt_main suite