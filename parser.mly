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
  | p=piecespec             CAPTURE ep=position EOF { Move.Normal (p, true, ep) }
  | p=piecespec sp=position CAPTURE ep=position EOF { Move.Full (p, sp, true, ep) }
  | p=piecespec f=file      CAPTURE ep=position EOF { Move.Filed (p, f, true, ep) }
  | p=piecespec r=rank      CAPTURE ep=position EOF { Move.Ranked (p, r, true, ep) }

  | p=piecespec                     ep=position EOF { Move.Normal (p, false, ep) }
  | p=piecespec sp=position         ep=position EOF { Move.Full (p, sp, false, ep) }
  | p=piecespec f=file              ep=position EOF { Move.Filed (p, f, false, ep) }
  | p=piecespec r=rank              ep=position EOF { Move.Ranked (p, r, false, ep) }

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
