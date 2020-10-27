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
  else if black_check then print_string_with_bold " Black is in check\n"
  else print_endline "  ----------------";

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



let clear_screen _ =
  print_string "\027[2J\027[H";
  flush stdout

let use_alternate_screen bool =
  Printf.printf "\027[?1049%s\n" (if bool then "h" else "l")

let print_playing_help _ =
  ()

let print_possible_moves game =
  let open Game in
  let next_color = if (List.length game.moves) mod 2 = 0 then Color.White else Color.Black in
  let valids = Validate.get_all_valid_moves game.board next_color in
  List.iter (fun (sr, sf, er, ef) ->
      Printf.printf "Move: %s%s -> %s%s\n"
        (File.to_string sf)
        (Rank.to_string sr)
        (File.to_string ef)
        (Rank.to_string er)
    ) valids

let get_move_str _ =
  let input = try Some (read_line ())
              with _ -> None in
  match input with
  | Some s -> if s = "exit" then "quit" else s
  | None -> "quit"

let rec get_move_cmd game =
  let open Game in
  let turn = if (List.length game.moves) mod 2 = 0 then "white" else "black" in
  print_string turn; print_string "> "; flush stdout;
  let str = get_move_str () in
  if str = "quit" then None
  else if str = "help" then (print_playing_help game; get_move_cmd game)
  else if str = "list" then (print_possible_moves game; get_move_cmd game)
  else if str = "board" then (Draw.board game.board; get_move_cmd game)
  else if str = "moves" then (Game.print_moves game; get_move_cmd game)
  else Some str


let rec play_cmdline ?(warn="") game =
  let open Game in
  clear_screen ();
  Draw.board game.board;
  if warn <> "" then print_endline warn;
  let input = get_move_cmd game in
  match input with
  | None -> print_endline "Quitting..."
  | Some s ->
     let (warn, nextgame) =
       try ("", Game.play_move game s)
       with | InvalidMove -> ("Invalid move", game)
            | UnparsableMove -> ("Unable to parse move", game)
            | AmbiguousMove -> ("That move is ambiguous", game)
            | CannotCastleNow -> ("Currently unable to castle", game) in
     play_cmdline ~warn nextgame

let start_cmdline game =
  use_alternate_screen true;
  try (play_cmdline game; use_alternate_screen false)
  with e -> (
    use_alternate_screen false;
    raise e);
  
