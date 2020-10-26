open Types
open Jchess

module Draw = struct
  let print_string_ansi str ansi_items =
    let joined = String.concat ";" (List.map string_of_int ansi_items) in
    Printf.printf "\027[%sm%s\027[0m" joined str

  let print_string_with_color str fg bg =
    print_string_ansi str [fg;bg]

  let print_string_with_effect str eff =
    print_string_ansi str [eff]

  let print_string_with_bold str =
    print_string_ansi str [1]

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
  let white_check_mate = Validate.is_white_in_checkmate board in
  let black_check_mate = Validate.is_black_in_checkmate board in
  let stalemate = Validate.is_stalemate board in
  if stalemate then print_string_with_bold "Stalemate :/\n"
  else if white_check_mate then print_string_with_bold "Black Wins!!!\n"
  else if white_check then print_string_with_bold " White is in check\n"
  else if black_check_mate then print_string_with_bold "White Wins!!!\n"
  else if black_check then print_string_with_bold " Black is in check\n" else ();

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

