exception AlgebricConversionError

module File = struct
  type t = A | B | C | D | E | F | G | H
  let array = [|A; B; C; D; E; F; G; H|]
  let from_string str =
    if str      = "a" then A
    else if str = "b" then B
    else if str = "c" then C
    else if str = "d" then D
    else if str = "e" then E
    else if str = "f" then F
    else if str = "g" then G
    else if str = "h" then H
    else raise AlgebricConversionError

  let to_string = function
    | A -> "a"
    | B -> "b"
    | C -> "c"
    | D -> "d"
    | E -> "e"
    | F -> "f"
    | G -> "g"
    | H -> "h"
    
end

module Rank = struct
  type t = One | Two | Three | Four | Five | Six | Seven | Eight
  let array = [|One; Two; Three; Four; Five; Six; Seven; Eight|]

  let from_number i =
    if i >= 1 && i <= 8 then
      Array.get [|One; Two; Three; Four; Five; Six; Seven; Eight|] (i-1)
    else raise AlgebricConversionError

  let to_string = function
    | One -> "1"
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
end

module Color = struct
  type t = White | Black
  let to_string = function
    | White -> "white"
    | Black -> "black"
end

module Position = struct
  type t = File.t * Rank.t

  let to_string x =
    let (f, r) = x in
    (File.to_string f) ^ (Rank.to_string r)

  let all _ =
    let r = Array.to_list Rank.array in
    let f = Array.to_list File.array in
    let res = List.fold_left (fun acc x ->
                  List.fold_left (fun acc y -> (x, y) :: acc) acc r
                ) [] f in
    res
end


module Piece = struct
  type varity = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = {
      color : Color.t;
      varity : varity;
    }
  type t = None | Piece of piece

  let make color var =
    Piece { color=color; varity=var }

  let of_algebric_string str =
    let str = String.lowercase_ascii str in
    if str = "q"      then Queen
    else if str = "k" then King
    else if str = "r" then Rook
    else if str = "b" then Bishop
    else if str = "n" then Knight
    else raise AlgebricConversionError

  let to_algebric_string varity =
    match varity with
    | Pawn -> ""
    | Knight -> "N"
    | Bishop -> "B"
    | Rook -> "R"
    | Queen -> "Q"
    | King -> "K"
end

module UCI = struct
  type t =
    | OnePiece of Position.t * Position.t
    | TwoPiece of (Position.t * Position.t) * (Position.t * Position.t)
    | Promotion of Position.t * Position.t * Piece.varity
end

module Move = struct
  type capture = bool
  type queenside = bool
  type movement =
    | Normal of Piece.varity              * capture * Position.t
    | Full   of Piece.varity * Position.t * capture * Position.t
    | Ranked of Piece.varity * Rank.t     * capture * Position.t
    | Filed  of Piece.varity * File.t     * capture * Position.t
  type t =
    | Regular of movement
    | Castle of queenside
    | PawnPromotion of movement * Piece.varity

  let capture_to_string c = if c then "x" else ""

  let string_of_movement = function
    | Normal (v, c, p) -> (Piece.to_algebric_string v) ^ (capture_to_string c) ^
                              (Position.to_string p)
    | Full (v, ps, c, p) -> (Piece.to_algebric_string v) ^ (Position.to_string ps) ^
                                (capture_to_string c) ^ (Position.to_string p)
    | Ranked (v, rs, c, p) -> (Piece.to_algebric_string v) ^ (Rank.to_string rs) ^
                                  (capture_to_string c) ^ (Position.to_string p)
    | Filed (v, fs, c, p) -> (Piece.to_algebric_string v) ^ (File.to_string fs) ^
                                 (capture_to_string c) ^ (Position.to_string p)

  let to_string = function
    | Regular move -> string_of_movement move
    | Castle queenside -> if queenside then "O-O-O" else "O-O"
    | PawnPromotion (mov, to_piece) -> (string_of_movement mov) ^ "=" ^
                                         (Piece.to_algebric_string to_piece)
       
    
end
