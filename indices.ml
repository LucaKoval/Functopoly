open Board

type tile_object = PropertyTile of Board.property_tile
                 | CardTile of Board.card_tile 
                 | TaxTile of Board.tax_tile 
                 | CornerTile of Board.corner_tile

let rec find_property_tile i (lst:Board.property_tile list) =
  match lst with
  | [] -> None
  | h::t -> if h.location = i then Some (PropertyTile h)
    else find_property_tile i t

let rec find_card_tile i (lst:Board.card_tile list) =
  match lst with
  | [] -> None
  | h::t -> if h.location = i then Some (CardTile h)
    else find_card_tile i t

let rec find_tax_tile i (lst:Board.tax_tile list) =
  match lst with
  | [] -> None
  | h::t -> if h.location = i then Some (TaxTile h)
    else find_tax_tile i t

let rec find_corner_tile i (lst:Board.corner_tile list) =
  match lst with
  | [] -> None
  | h::t -> if h.location = i then Some (CornerTile h)
    else find_corner_tile i t

let return_tile i (board:Board.t) =
  let props = board.property_tiles in
  let prop_found = find_property_tile i props in
  if prop_found <> None then prop_found else
    let card_tiles = board.card_tiles in
    let card_found = find_card_tile i card_tiles in
    if card_found <> None then card_found else
      let taxes = board.tax_tiles in
      let tax_found = find_tax_tile i taxes in
      if card_found <> None then tax_found else
        let corners = board.corner_tiles in
        let corner_found = find_corner_tile i corners in
        if corner_found <> None then corner_found else None