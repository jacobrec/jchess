open Types

let rows = 8
let cols = 8
type t = Piece.t Array.t

let col_of_file = function
  | File.A -> 0
  | File.B -> 1
  | File.C -> 2
  | File.D -> 3
  | File.E -> 4
  | File.F -> 5
  | File.G -> 6
  | File.H -> 7

let row_of_rank = function
  | Rank.One -> 0
  | Rank.Two -> 1
  | Rank.Three -> 2
  | Rank.Four -> 3
  | Rank.Five -> 4
  | Rank.Six -> 5
  | Rank.Seven -> 6
  | Rank.Eight -> 7

let file_of_col col = 
  Array.get File.array col

let index_of_row_col row col = 
  (col + row * cols)
let index_of_rank_file rank file = 
  let col = col_of_file file in
  let row = row_of_rank rank in
  index_of_row_col row col

let set_piece_idx board piece idx =
  Array.set board idx piece
let add_piece board color varity file rank =
  let idx = index_of_rank_file rank file in
  set_piece_idx board (Piece.make color varity) idx

let get_piece_idx board idx =
  Array.get board idx
let get_piece board file rank =
  let idx = index_of_rank_file rank file in
  get_piece_idx board idx

(* let remove_piece board file rank =
 * let move_piece board file_from rank_from file_to rank_to = *)



let default _ =
  let open Piece in
  let board = Array.make (rows * cols) None in
  let back_rank = [Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook] in
  let front_rank = List.init 8 (fun _ -> Pawn) in
  let add_row pieces color rank =
    let mapper i x =
      let file = file_of_col i in
      add_piece board color x file rank in
    List.iteri mapper pieces in

  add_row back_rank  White Rank.One;
  add_row front_rank White Rank.Two;
  add_row front_rank Black Rank.Seven;
  add_row back_rank  Black Rank.Eight;

  board


let do_move_idx board from_idx to_idx =
  let p = get_piece_idx board from_idx in
  set_piece_idx board p to_idx;
  set_piece_idx board Piece.None from_idx;
  board

