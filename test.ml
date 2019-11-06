open Board

let board = Yojson.Basic.from_file "board1.json" |> from_json