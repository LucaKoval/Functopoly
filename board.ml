open Yojson.Basic.Util

type color = Brown | LightBlue | Magenta | Orange | Red | Yellow | Green | Blue | NoColor
type tile_type = Property | Railroad | Utility
type card_type = Chance | CommunityChest
type tax_tile_type = IncomeTax | LuxuryTax
type corner_tile_type = Go | JailJustVisiting | FreeParking | GoToJail

type property_tile = {
  name : string;
  location : int;
  price : int;
  rent : int;
  color : color;
  level : int;
  tile_type : tile_type;
  owner : int;
}

type card_tile = {
  card_tile_name : string;
  location : int;
}

type card = {
  name : string;
  types : card_type list;
}

type tax_tile = {
  tax_tile_type : tax_tile_type;
  location : int;
}

type corner_tile = {
  corner_tile_type : corner_tile_type;
  location : int;
}

type t = {
  go_score : int;
  init_score : int;
  property_tiles : property_tile list;
  card_tiles : card_tile list;
  cards : card list;
  tax_tiles : tax_tile list;
  corner_tiles : corner_tile list;
}

let to_color color =
  if (color = "brown") then Brown
  else if (color = "light blue") then LightBlue
  else if (color = "magenta") then Magenta
  else if (color = "orange") then Orange
  else if (color = "red") then Red
  else if (color = "yellow") then Yellow
  else if (color = "green") then Green
  else if (color = "blue") then Blue
  else failwith ("Improper color: " ^ color)

let to_tile tile_type =
  if (tile_type = "property") then Property
  else if (tile_type = "railroad") then Railroad
  else if (tile_type = "utility") then Utility
  else failwith ("Improper tile type: " ^ tile_type)

let property_tile_of_json j = 
  let tile_type = j |> member "type" |> to_string |> to_tile in
  if tile_type = Property then
    {
      name = j |> member "name" |> to_string;
      location = j |> member "location" |> to_int;
      price = j |> member "price" |> to_int;
      rent = j |> member "rent" |> to_int;
      color = j |> member "color" |> to_string |> to_color;
      level = j |> member "level" |> to_int;
      tile_type = tile_type;
      owner = -1;
    }
  else if tile_type = Railroad then
    {
      name = j |> member "name" |> to_string;
      location = j |> member "location" |> to_int;
      price = j |> member "price" |> to_int;
      rent = j |> member "rent" |> to_int;
      color = NoColor;
      level = -1;
      tile_type = tile_type;
      owner = -1;
    }
  else if tile_type = Utility then
    {
      name = j |> member "name" |> to_string;
      location = j |> member "location" |> to_int;
      price = j |> member "price" |> to_int;
      rent = -1;
      color = NoColor;
      level = -1;
      tile_type = tile_type;
      owner = -1;
    }
  else failwith ("Improper tile type") 

let card_tile_of_json j = {
  card_tile_name = j |> member "name" |> to_string;
  location = j |> member "location" |> to_int;
}

let to_card_type card_type =
  if (card_type = "chance") then Chance
  else if (card_type = "community_chest") then CommunityChest
  else failwith ("Improper card type: " ^ card_type)

let card_of_json j = {
  name = "";
  types = j |> member "types" |> to_list |> List.map
            (fun x -> x |> member "type" |> to_string |> to_card_type);
}

let to_tax_tile_type tax_tile_type = 
  if (tax_tile_type = "income_tax") then IncomeTax
  else if (tax_tile_type = "luxury_tax") then LuxuryTax
  else failwith ("Improper tax tile type: " ^ tax_tile_type)

let tax_tile_of_json j = 
  {
    tax_tile_type = j |> member "name" |> to_string |> to_tax_tile_type;
    location = j |> member "location" |> to_int;
  }

let to_corner_tile_type corner_tile_type =
  if (corner_tile_type = "go") then Go
  else if (corner_tile_type = "jail_just_visiting") then JailJustVisiting
  else if (corner_tile_type = "free_parking") then FreeParking
  else if (corner_tile_type = "go_to_jail") then GoToJail
  else failwith ("Improper corner tile type: " ^ corner_tile_type)

let corner_tile_of_json j = {
  corner_tile_type = j |> member "name" |> to_string |> to_corner_tile_type;
  location = j |> member "location" |> to_int;
}

let board_of_json j = {
  go_score = j |> member "go_score" |> to_int;
  init_score = j |> member "init_score" |> to_int;
  property_tiles = j |> member "property_tiles" |> to_list |>
                   List.map property_tile_of_json;
  card_tiles = j |> member "card_tiles" |> to_list |>
               List.map card_tile_of_json;
  cards = j |> member "cards" |> to_list |>
          List.map card_of_json;
  tax_tiles = j |> member "tax_tiles" |> to_list |>
              List.map tax_tile_of_json;
  corner_tiles = j |> member "corner_tiles" |> to_list |>
                 List.map corner_tile_of_json;
}

let from_json j =
  try board_of_json j
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let rec buy_update_properties (board_property_tiles:property_tile list) current_player_id property_name acc=
  match board_property_tiles with 
  | []-> List.rev acc
  | h::t when (h.name = property_name) -> 
    buy_update_properties t current_player_id property_name ({
        name = h.name;
        location = h.location;
        price = h.price;
        rent = h.rent;
        color = h.color;
        level = h.level;
        tile_type = h.tile_type;
        owner = current_player_id;
      }::acc)
  | h::t -> buy_update_properties t current_player_id property_name (h::acc)

let buy_update_board board current_player_id property_name= {
  go_score = board.go_score;
  init_score = board.init_score;
  property_tiles = buy_update_properties board.property_tiles current_player_id property_name [];
  card_tiles = board.card_tiles;
  cards = board.cards;
  tax_tiles = board.tax_tiles;
  corner_tiles = board.corner_tiles
}