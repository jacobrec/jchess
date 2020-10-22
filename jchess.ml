module File = struct
  type t = A | B | C | D | E | F | G | H
end

module Rank = struct
  type t = One | Two | Three | Four | Five | Six | Seven | Eight
end

module Piece = struct
  type varity = Pawn | Knight | Bishop | Rook | Queen | King
  type color  = White | Black
  type piece = {
      color : color;
      varity : varity;
    }
  type t = None | Piece of piece

  let make color var =
    Piece { color=color; varity=var }
end
    

let print_string_with_color str fg bg =
  Printf.printf "\027[%d;%dm%s\027[0m" fg bg str
let draw_empty_tile tile =
  let black = 0 in
  let white = 7 in
  let background = 40 in
  let bg = match tile with
    | Piece.White -> background + white
    | Piece.Black -> background + black in
  print_string_with_color "  " 39 bg
let draw_piece piece tile =
  let open Piece in
  let black = 0 in
  let white = 7 in
  let background = 40 in
  let foreground = 30 in
  let bg = match tile with
    | White -> background + white
    | Black -> background + black in
  let (fg, outline) = match (piece.color, tile) with
    | (White, White) -> (foreground + black, true)
    | (White, Black) -> (foreground + white, false)
    | (Black, White) -> (foreground + black, false)
    | (Black, Black) -> (foreground + white, true)
  in
  let str = match (piece.varity, outline) with
    | (Pawn, false) -> "♟︎"
    | (Pawn, true) -> "♙"
    | (Knight, false) -> "♞"
    | (Knight, true) -> "♘"
    | (Bishop, false) -> "♝"
    | (Bishop, true) -> "♗"
    | (Rook, false) -> "♜"
    | (Rook, true) -> "♖"
    | (Queen, false) -> "♛"
    | (Queen, true) -> "♕"
    | (King, false) -> "♚"
    | (King, true) -> "♔" in
  print_string_with_color (str ^ " ") fg bg

module Board = struct
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

  let add_piece_xy board piece col row =
    Array.set board (col + row * cols) piece
  let add_piece board color varity file rank =
    let col = col_of_file file in
    let row = row_of_rank rank in
    add_piece_xy board (Piece.make color varity) col row

  let get_piece_xy board col row =
    Array.get board (col + row * cols)
  let get_piece board file rank =
    let col = col_of_file file in
    let row = row_of_rank rank in
    get_piece_xy board col row

  (* let remove_piece board file rank =
   * let move_piece board file_from rank_from file_to rank_to = *)

    

  let default _ =
    let open Piece in
    let board = Array.make (rows * cols) None in
    let back_rank = [Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook] in
    let front_rank = List.init 8 (fun _ -> Pawn) in
    let add_row pieces color rank =
      let row = row_of_rank rank in
      let mapper i x =
        add_piece_xy board (Piece.make color x) i row in
      List.iteri mapper pieces in

    add_row back_rank  White Rank.One;
    add_row front_rank White Rank.Two;
    add_row front_rank Black Rank.Seven;
    add_row back_rank  Black Rank.Eight;

    board

    
  let print ?(facing_white=true) board =
    let open Rank in
    let open File in
    let open Piece in
    let white_ranks = [Eight; Seven; Six; Five; Four; Three; Two; One] in
    let ranks = if facing_white then white_ranks else List.rev white_ranks in
    let files = [A; B; C; D; E; F; G; H] in
    let tile = ref (if facing_white then Black else White) in
    let flip_tile _ = tile := match !tile with
                             | White -> Black
                             | Black -> White in
    List.iter (fun r ->
        List.iter (fun f ->
            flip_tile ();
            match (get_piece board f r) with
            | None -> draw_empty_tile !tile
            | Piece p -> draw_piece p !tile
          ) files;
        flip_tile ();
        print_newline ()
      ) ranks

end


let () =
  let b = Board.default () in
  Board.print b
