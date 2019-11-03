let rec find_loop i = function
  | [] -> None
  | h::t -> if h.location = i then Some h else find_loop i t

let return_tile i board =
  let props = board.properties in
  let prop_found = find_loop i props in
  if prop_found <> None then prop_found else
    let card_tiles = board.card_tiles in
    let card_found = find_loop i card_tiles in
    if card_found <> None then card_found else
      let taxes = board.taxes in
      let tax_found = find_loop i taxes in
      if card_found <> None then tax_found else
        let corners = board.corners in
        let corner_found = find_loop i corners in
        if corner_found <> None then corner_found else None