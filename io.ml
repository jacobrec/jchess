open Types
open Jchess

module Draw = struct
  let print_string_with_color str fg bg =
    Printf.printf "\027[%d;%dm%s\027[0m" fg bg str

  let empty_tile tile =
    let black = 0 in
    let white = 7 in
    let background = 40 in
    let bg = match tile with
      | Color.White -> background + white
      | Color.Black -> background + black in
    print_string_with_color "  " 39 bg

  let piece piece tile =
    let open Color in
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


let board ?(facing_white=true) board =
  let open Rank in
  let open File in
  let open Piece in
  let open Color in

  let white_check = Validate.is_white_in_check board in
  let black_check = Validate.is_black_in_check board in
  if white_check then print_endline "White is in check" else ();
  if black_check then print_endline "Black is in check" else ();

  let white_ranks = [Eight; Seven; Six; Five; Four; Three; Two; One] in
  let ranks = if facing_white then white_ranks else List.rev white_ranks in
  let files = [A; B; C; D; E; F; G; H] in
  let tile = ref (if facing_white then Black else White) in
  let flip_tile _ = tile := match !tile with
                            | White -> Black
                            | Black -> White in
  print_string "  ";
  List.iter (fun s -> Printf.printf "%s " s) ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"];
  print_newline ();
  List.iteri (fun i r ->
      Printf.printf "%d " (8 - i);
      List.iter (fun f ->
          flip_tile ();
          match (Board.get_piece board f r) with
          | None -> empty_tile !tile
            | Piece p -> piece p !tile
        ) files;
      flip_tile ();
      print_newline ()
    ) ranks


end

