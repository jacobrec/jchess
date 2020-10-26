open Io
open Jchess

let () =
  (* let g = Game.create_from_moves ["e4"; "e5"; "Bc4"; "f5"; "Qh5"] in (\* Black Check *\)  *)
  let g = Game.create_from_moves ["f3"; "e5"; "g4"; "Qh4"] in (* Fool's mate *)
  Draw.board g.board;
  let open Types in
  let valids = Validate.get_all_valid_moves g.board Color.White in
  List.iter (fun (sr, sf, er, ef) ->
      Printf.printf "Move: %s%s -> %s%s\n"
        (File.to_string sf)
        (Rank.to_string sr)
        (File.to_string ef)
        (Rank.to_string er)
    )valids
