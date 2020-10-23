%token <int> RANK
%token <string> FILE
%token <string> PIECE
%token CAPTURE
%token PROMOTION
%token EOF

%{open Types%}
%start <Move.t> main
%%

main:
  | m = move { m }

move:
  | p=piecespec             c=capture ep=position { Move.Normal (p, c, ep) }
  | p=piecespec sp=position c=capture ep=position { Move.Full (p, sp, c, ep) }
  | p=piecespec f=file      c=capture ep=position { Move.Filed (p, f, c, ep) }
  | p=piecespec r=rank      c=capture ep=position { Move.Ranked (p, r, c, ep) }

capture:
  | CAPTURE { true }
  | { false }

position:
  | f=file r=rank { (f, r) }

promotion:
  | PROMOTION p=PIECE { Piece.of_algebric_string p }

rank:
  | r=RANK { Rank.from_number r } 

file:
  | r=FILE { File.from_string r } 

piecespec:
  | p = PIECE { Piece.of_algebric_string p }
  | { Piece.Pawn }
