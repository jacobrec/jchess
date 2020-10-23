open Types

exception InvalidMove
exception AmbiguousMove
exception UnparsableMove

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
end



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

  let file_of_col col = 
    Array.get [|File.A; File.B; File.C; File.D; File.E; File.F; File.G; File.H|] col

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

    
  let print ?(facing_white=true) board =
    let open Rank in
    let open File in
    let open Piece in
    let open Color in
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
            match (get_piece board f r) with
            | None -> Draw.empty_tile !tile
            | Piece p -> Draw.piece p !tile
          ) files;
        flip_tile ();
        print_newline ()
      ) ranks

  let do_move_idx board from_idx to_idx =
    let p = get_piece_idx board from_idx in
    set_piece_idx board p to_idx;
    set_piece_idx board Piece.None from_idx;
    board

end

module Validate = struct

  type line_type =
    | Horizontal
    | Vertical
    | Diagonal

  let identify_line dx dy =
    if dx = 0 && dy <> 0 then Some Vertical
    else if dy = 0 && dx <> 0 then Some Horizontal
    else if (Int.abs dx) = (Int.abs dy) && dx <> 0 then Some Diagonal
    else None

  let validate_shape_and_landing dx dy piece_info dest =
    let open Piece in
    let piece = piece_info.varity in
    let color = piece_info.color in
    let empty_dest = match dest with
      | None -> true
      | Piece _ -> false in
    let enemy_dest = match dest with
      | None -> false
      | Piece p -> match (color, p.color) with
                   | (Color.White,  Color.White) -> false
                   | (Color.Black,  Color.White) -> true
                   | (Color.White,  Color.Black) -> true
                   | (Color.Black,  Color.Black) -> false in
    let valid_destination = empty_dest || enemy_dest in
    
    let line_shape = identify_line dx dy in
    let valid_move_shape = match piece with
      | Pawn -> let dir = match color with
                  | Color.Black -> -1
                  | Color.White -> 1 in
                let move_normal = (dy = dir && empty_dest) in
                let move_attack = (dy = dir && dx = 1 && enemy_dest) in
                let move_rocket = (dy = (2*dir)) in
                move_normal || move_attack || move_rocket
      | Knight -> (dx = 2 && dy = 1) || (dy = 2 && dx = 1)
      | Bishop -> (match line_shape with | Some Diagonal -> true | _ -> false)
      | Rook -> (match line_shape with | Some Vertical -> true | Some Horizontal -> true | _ -> false)
      | Queen -> (match line_shape with | Some _ -> true | None -> false)
      | King -> (Int.abs dx + Int.abs dy) = 1 in
    (* Check to make sure there is no pieces in path for queen,
       bishop, rook, and pawns first move *)
    valid_destination && valid_move_shape

  let validate_doesnt_cross_through board row col dx dy =
    let open Piece in
    let line = identify_line dx dy in
    let is_line_empty row col dx dy =
      let rec is_line_empty_inner row col dx dy =
        let tdx = if dx < 0 then -1 else if dx > 0 then 1 else 0 in
        let tdy = if dy < 0 then -1 else if dy > 0 then 1 else 0 in
        let idx = Board.index_of_row_col row col in
        let loc = Board.get_piece_idx board idx in
        ((dx = 0 && dy = 0) ||
           (match loc with
            | None -> is_line_empty_inner (row + tdx) (col + tdy) (dx - tdx) (dy - tdy)
            | _ -> false)) in
        let tdx = if dx < 0 then -1 else if dx > 0 then 1 else 0 in
        let tdy = if dy < 0 then -1 else if dy > 0 then 1 else 0 in
      is_line_empty_inner (row + tdy) (col + tdy) (dx - tdx) (dy - tdy) in
         
    match line with
    | Some _ ->
       let line_length = max (Int.abs dx) (Int.abs dy) in
       (* need to actually check lines now*)
       (line_length <= 1 || is_line_empty row col dx dy)
    | None -> true (* horses dont make lines *)
    

  let validate_move board from_rank from_file to_rank to_file =
    let open Piece in
    let from_idx = Board.index_of_rank_file from_rank from_file in
    let to_idx = Board.index_of_rank_file to_rank to_file in
    let piece = Board.get_piece_idx board from_idx in
    let destination = Board.get_piece_idx board to_idx in
    let from_row = Board.row_of_rank from_rank in
    let to_row = Board.row_of_rank to_rank in
    let dy = to_row - from_row in
    let from_col = Board.col_of_file from_file in
    let to_col = Board.col_of_file to_file in
    let dx = to_col - from_col in
    match piece with
    | None -> false
    | Piece piece ->
       validate_shape_and_landing dx dy piece destination &&
       validate_doesnt_cross_through board from_row from_col dx dy

  let validate_parsed_move board move =
    let (from_rank, from_file, to_rank, to_file) = move in
    validate_move board from_rank from_file to_rank to_file
end

module MoveParser = struct
  open Lexing
  let algebraic str_move =
    let lexbuf = from_string str_move in
    print_endline str_move;
    Parser.main Lexer.token lexbuf

  let find_viable_starts board color v ef er =
    ignore (board);
    ignore (v);
    ignore (color);
    ignore (ef);
    ignore (er);
    [(File.A, Rank.One)]

  let infer_start board color v ef er =
    let locs = find_viable_starts board color v ef er in
    if 1 <> List.length locs then raise AmbiguousMove else List.hd locs
  let infer_start_file board color v rank ef er =
    let locs = find_viable_starts board color v ef er in
    let locs = List.filter (fun x -> let (_, r) = x in rank = r) locs in
    let (f, _) = if 1 <> List.length locs then raise AmbiguousMove else List.hd locs in
    f
  let infer_start_rank board color v file ef er =
    let locs = find_viable_starts board color v ef er in
    let locs = List.filter (fun x -> let (f, _) = x in file = f) locs in
    let (_, r) = if 1 <> List.length locs then raise AmbiguousMove else List.hd locs in
    r

  let algebraic_to_uci board color move =
    let open Move in
    match move with
    | Normal (v, _, (ef, er)) -> let (sf, sr) = infer_start board color v ef er in
                                 (sr, sf, er, ef)
    | Full (_, (sf, sr), _, (ef, er)) -> (sr, sf, er, ef)
    | Ranked (v, sr, _, (ef, er)) -> let sf = infer_start_file board color v sr ef er in
                                     (sr, sf, er, ef)
    | Filed (v, sf, _, (ef, er)) -> let sr = infer_start_rank board color v sf ef er in
                                    (sr, sf, er, ef)


  let go board color str_move =
    let an = algebraic str_move in
    print_endline (Move.to_string an);
    algebraic_to_uci board color an


    
end

module Game = struct
  type t = {
      moves : string list;
      board : Board.t;
    }


  let play_move game str_move =
    let parsed_move = MoveParser.go game.board str_move in
    if Validate.validate_parsed_move game.board parsed_move then begin
        let (from_rank, from_file, to_rank, to_file) = parsed_move in
        let from_idx = Board.index_of_rank_file from_rank from_file in
        let to_idx = Board.index_of_rank_file to_rank to_file in
        {moves = str_move :: game.moves; board = Board.do_move_idx game.board from_idx to_idx }
      end else raise InvalidMove

  let create _ =
    { board=Board.default (); moves=[] }

  let create_from_moves moves =
    List.fold_left (fun game move -> play_move game move) (create ()) moves

  let print_moves game =
    let moves = List.rev game.moves in
    List.iteri (fun i m ->
        if (i mod 2 = 0) then
          Printf.printf "%d. %s " (1 + (i / 2)) m
        else
          Printf.printf "%s\n" m
      ) moves;

    (* Print newline if line is not ended *)
    if (List.length moves) mod 2 = 0 then () else print_newline ()

  let whose_turn game = 
    let moves = List.length game.moves in
    if (moves mod 2 = 0) then Color.Black
    else Color.White
end

let () =
  let g = Game.create () in
  Board.print g.board;
  let g = Game.play_move g "e2e3" in
  Board.print g.board;
