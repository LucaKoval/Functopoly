type color_groups = {brown:int; light_blue:int; magenta:int; orange:int;
                     red:int; yellow:int; green:int; blue:int}
let get_color_groups id (board:Board.t) =
  let color_groups = {brown=0; light_blue=0; magenta=0; orange=0;
                      red=0; yellow=0; green=0; blue=0} in
  let rec helper tracker = function
    | index -> if index = 40 then tracker else
        begin
          match Indices.return_tile index board with
          | None -> helper tracker (index+1)
          | Some tile -> begin
              match tile with
              | Indices.PropertyTile propTile -> if propTile.tile_type == Board.Property && propTile.owner = id then
                  match propTile.color with
                  | Board.Brown -> helper {tracker with brown=tracker.brown+1} (index+1)
                  | Board.LightBlue -> helper {tracker with light_blue=tracker.light_blue+1} (index+1)
                  | Board.Magenta -> helper {tracker with magenta=tracker.magenta+1} (index+1)
                  | Board.Orange -> helper {tracker with orange=tracker.orange+1} (index+1)
                  | Board.Red -> helper {tracker with red=tracker.red+1} (index+1)
                  | Board.Yellow -> helper {tracker with yellow=tracker.yellow+1} (index+1)
                  | Board.Green -> helper {tracker with green=tracker.green+1} (index+1)
                  | Board.Blue -> helper {tracker with blue=tracker.blue+1} (index+1)
                  | _ -> helper tracker (index+1)
                else
                  helper tracker (index+1)
              | _ -> helper tracker (index+1)
            end
        end
  in
  helper color_groups 1

let get_upgradeable_properties id board color_groups =
  let rec helper acc = function
    | index -> if index = 40 then acc else
        begin
          match Indices.return_tile index board with
          | None -> helper acc (index+1)
          | Some tile -> begin
              match tile with
              | Indices.PropertyTile propTile -> if propTile.tile_type == Board.Property then
                  let name_index = (propTile.name, id) in
                  match propTile.color with
                  | Board.Brown -> if color_groups.brown = 2 then
                      helper (name_index::acc) (index+1)
                    else helper acc (index+1)
                  | Board.LightBlue -> if color_groups.light_blue = 3 then
                      helper (name_index::acc) (index+1)
                    else helper acc (index+1)
                  | Board.Magenta -> if color_groups.magenta = 3 then
                      helper (name_index::acc) (index+1)
                    else helper acc (index+1)
                  | Board.Orange -> if color_groups.orange = 3 then
                      helper (name_index::acc) (index+1)
                    else helper acc (index+1)
                  | Board.Red -> if color_groups.light_blue = 3 then
                      helper (name_index::acc) (index+1)
                    else helper acc (index+1)
                  | Board.Yellow -> if color_groups.yellow = 3 then
                      helper (name_index::acc) (index+1)
                    else helper acc (index+1)
                  | Board.Green -> if color_groups.green = 3 then
                      helper (name_index::acc) (index+1)
                    else helper acc (index+1)
                  | Board.Blue -> if color_groups.blue = 2 then
                      helper (name_index::acc) (index+1)
                    else helper acc (index+1)
                  | _ -> helper acc (index+1)
                else
                  helper acc (index+1)
              | _ -> helper acc (index+1)
            end
        end
  in
  helper [] 1

let update_level index props =
  let rec helper acc (props:Board.property_tile list) = 
    match props with
    | [] -> acc
    | h::t -> if h.location = index then
        helper ({h with level=h.level+1}::acc) t
      else
        helper (h::acc) t
  in
  helper [] props