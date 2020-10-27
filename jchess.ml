open Types

exception InvalidMove
exception AmbiguousMove
exception UnparsableMove
exception CannotCastleNow

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
    let adx = Int.abs dx in
    let ady = Int.abs dy in
    let valid_move_shape = match piece with
      | Pawn -> let dir = match color with
                  | Color.Black -> -1
                  | Color.White -> 1 in
                let move_normal = (dy = dir && empty_dest && dx = 0) in
                let move_attack = (dy = dir && dx = 1 && enemy_dest) in
                let move_rocket = (dy = (2*dir) && dx = 0 && empty_dest) in
                move_normal || move_attack || move_rocket
      | Knight -> (adx = 2 && ady = 1) || (adx = 1 && ady = 2)
      | Bishop -> (match line_shape with | Some Diagonal -> true | _ -> false)
      | Rook -> (match line_shape with | Some Vertical -> true | Some Horizontal -> true | _ -> false)
      | Queen -> (match line_shape with | Some _ -> true | None -> false)
      | King -> (adx + ady) = 1 in
    valid_destination && valid_move_shape

  let validate_doesnt_cross_through board row col dx dy =
    let open Piece in
    let line = identify_line dx dy in
    let is_line_empty row col dx dy =
      (* Printf.printf "is_line_empty: %d %d %d %d\n" row col dx dy; *)
      let rec is_line_empty_inner row col dx dy =
        let tdx = if dx < 0 then -1 else if dx > 0 then 1 else 0 in
        let tdy = if dy < 0 then -1 else if dy > 0 then 1 else 0 in
        let idx = Board.index_of_row_col row col in
        let loc = Board.get_piece_idx board idx in
        (* Printf.printf "is_line_empty_inner: %d %d %d %d -> %d %d\n" row col dx dy tdx tdy; *)
        ((dx = 0 && dy = 0) ||
           (match loc with
            | None -> is_line_empty_inner (row + tdy) (col + tdx) (dx - tdx) (dy - tdy)
            | _ -> false)) in
        let tdx = if dx < 0 then -1 else if dx > 0 then 1 else 0 in
        let tdy = if dy < 0 then -1 else if dy > 0 then 1 else 0 in
      is_line_empty_inner (row + tdy) (col + tdx) (dx - tdx) (dy - tdy) in
         
    match line with
    | Some _ ->
       let line_length = max (Int.abs dx) (Int.abs dy) in
       (* need to actually check lines now*)
       (line_length <= 1 || is_line_empty row col dx dy)
    | None -> true (* horses dont make lines *)
    

  let rec validate_move board from_rank from_file to_rank to_file =
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
    | Piece piece -> (
      let v_shape = validate_shape_and_landing dx dy piece destination in
      let v_cross = validate_doesnt_cross_through board from_row from_col dx dy in
      (* Printf.printf "validate move: [%s%s => %s%s]: %s [delta %d, %d] (%B, %B)\n"
       *   (File.to_string from_file) (Rank.to_string from_rank)
       *   (File.to_string to_file) (Rank.to_string to_rank)
       *   (Piece.to_algebric_string piece.varity)
       *   dx dy v_shape v_cross; *)
      v_shape && v_cross &&
        (king_not_in_check board piece from_rank from_file to_rank to_file)
    )

  and king_not_in_check board piece from_rank from_file to_rank to_file = 
    let b2 = Array.copy board in
    let from_idx = Board.index_of_rank_file from_rank from_file in
    let to_idx = Board.index_of_rank_file to_rank to_file in
    let b3 = Board.do_move_idx b2 from_idx to_idx in
    if is_in_check b3 piece.color then false else true

  and is_square_threatened_by board file rank color =
    let all = Position.all () in
    let op = List.filter (fun (f, r) ->
        let open Piece in
        let p = Board.get_piece board f r in
        match p with
        | None -> false
        | Piece p -> p.color = color
      ) all in
    let threats = List.filter (fun (sf, sr) ->
                      validate_move board sr sf rank file) op in
    (List.length threats) > 0

  and is_in_check board color =
    let open Color in
    let all = Position.all () in
    let king = List.filter (fun (f, r) ->
        let open Piece in
        let p = Board.get_piece board f r in
        match p with
        | None -> false
        | Piece p -> p.color = color && p.varity = Piece.King
      ) all in
    if (List.length king) < 1 then true else
      let king = List.hd king in
      let (file, rank) = king in
      let other_color = match color with | White -> Black | Black -> White in
      is_square_threatened_by board file rank other_color

  let get_all_valid_moves board color =
    let all = Position.all () in
    let mine = List.filter (fun (f, r) ->
        let open Piece in
        let p = Board.get_piece board f r in
        match p with
        | None -> false
        | Piece p -> p.color = color
      ) all in
    let moves_by_pieces = List.map (fun (sf, sr) ->
                              let valid = List.filter (fun (ef, er) ->
                                              validate_move board sr sf er ef
                                            )
                                            all in
                              List.map (fun (ef, er) -> (sr, sf, er, ef)) valid
                            ) mine in
    let moves = List.flatten moves_by_pieces in
    moves

  let has_valid_moves board color =
    (List.length (get_all_valid_moves board color)) >= 1

  let is_checkmate board color =
    (not (has_valid_moves board color)) &&
      (is_in_check board color)

  let is_stalemate_color board color =
    (not (has_valid_moves board color)) &&
      not (is_in_check board color)

  let is_stalemate board =
    (is_stalemate_color board Color.Black) ||
      (is_stalemate_color board Color.White)

  let is_white_in_check board = is_in_check board Color.White
  let is_black_in_check board = is_in_check board Color.Black
  let is_white_in_checkmate board = is_checkmate board Color.White
  let is_black_in_checkmate board = is_checkmate board Color.Black
    

  let validate_parsed_move board move =
    let (from_rank, from_file, to_rank, to_file) = move in
    validate_move board from_rank from_file to_rank to_file


  let can_castle _color _king_to =
    true
    
end

module MoveParser = struct
  open Lexing
  let algebraic str_move =
    let lexbuf = from_string str_move in
    Parser.main Lexer.token lexbuf

  let find_color_pieces board color v =
    let all = Position.all () in
    List.filter (fun (f, r) ->
        let open Piece in
        let p = Board.get_piece board f r in
        match p with
        | None -> false
        | Piece p -> p.color = color && p.varity = v
      ) all 
    
  let find_viable_starts board color v ef er =
    let locs = find_color_pieces board color v in
    let res = List.filter (fun (sf, sr) ->
                  Validate.validate_move board sr sf er ef) locs in
    res

  let raise_errors_or_hd locs = 
    if 0 = List.length locs then raise InvalidMove
    else if 1 <> List.length locs then raise AmbiguousMove
    else List.hd locs
  let infer_start board color v ef er =
    let locs = find_viable_starts board color v ef er in
    raise_errors_or_hd locs
  let infer_start_file board color v rank ef er =
    let locs = find_viable_starts board color v ef er in
    let locs = List.filter (fun x -> let (_, r) = x in rank = r) locs in
    let (f, _) = raise_errors_or_hd locs in
    f
  let infer_start_rank board color v file ef er =
    let locs = find_viable_starts board color v ef er in
    let locs = List.filter (fun x -> let (f, _) = x in file = f) locs in
    let (_, r) = raise_errors_or_hd locs in
    r

  let algebraic_to_uci board color move =
    let open Move in
    match move with
    | Normal (v, _, (ef, er)) -> let (sf, sr) = infer_start board color v ef er in
                                 UCI.OnePiece ((sf, sr), (ef, er))
    | Full (_, (sf, sr), _, (ef, er)) -> UCI.OnePiece ((sf, sr), (ef, er))
    | Ranked (v, sr, _, (ef, er)) -> let sf = infer_start_file board color v sr ef er in
                                     UCI.OnePiece ((sf, sr), (ef, er))
    | Filed (v, sf, _, (ef, er)) -> let sr = infer_start_rank board color v sf ef er in
                                    UCI.OnePiece ((sf, sr), (ef, er))
    | Castle queenside ->
       let dest = if queenside then File.C else File.G in
       let row = if color = Color.Black then Rank.Eight else Rank.One in
       let rsrc = if queenside then File.A else File.H in
       let rdest = if queenside then File.D else File.F in
       (* TODO: second move should be the rook *)
       UCI.TwoPiece (((File.E, row), (dest, row)), ((rsrc, row), (rdest, row)))


  let go board color str_move =
    let an = algebraic str_move in
    algebraic_to_uci board color an


    
end

module Game = struct
  type t = {
      moves : string list;
      board : Board.t;
    }


  let play_move game str_move =
    let move_count = List.length game.moves in
    let color = if move_count mod 2 = 0 then Color.White else Color.Black in
    let parsed_move = MoveParser.go game.board color str_move in
    match parsed_move with
    | UCI.OnePiece ((from_file, from_rank), (to_file, to_rank)) -> begin
        let parsed_move = (from_rank, from_file, to_rank, to_file) in
        if Validate.validate_parsed_move game.board parsed_move then
          let from_idx = Board.index_of_rank_file from_rank from_file in
          let to_idx = Board.index_of_rank_file to_rank to_file in
          {moves = str_move :: game.moves; board = Board.do_move_idx game.board from_idx to_idx }
        else raise InvalidMove
      end
    | UCI.TwoPiece (((p1_from_file, p1_from_rank), (p1_to_file, p1_to_rank)),
                    ((p2_from_file, p2_from_rank), (p2_to_file, p2_to_rank))) -> begin
        if Validate.can_castle color p1_to_file then
          let p1_from_idx = Board.index_of_rank_file p1_from_rank p1_from_file in
          let p1_to_idx = Board.index_of_rank_file p1_to_rank p1_to_file in

          let p2_from_idx = Board.index_of_rank_file p2_from_rank p2_from_file in
          let p2_to_idx = Board.index_of_rank_file p2_to_rank p2_to_file in

          let new_moves = str_move :: game.moves in
          let b2 = Board.do_move_idx game.board p1_from_idx p1_to_idx in
          let b3 = Board.do_move_idx b2 p2_from_idx p2_to_idx in

          {moves = new_moves; board = b3 }
          
        else raise CannotCastleNow
      end

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
