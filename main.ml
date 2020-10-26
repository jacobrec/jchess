open Io
open Jchess

let () =
  let g = Game.create_from_moves ["e4"; "e5"; "Bc4"; "f5"; "Qh5"; "g6"] in
  Draw.board g.board
