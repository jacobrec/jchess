%token <int> RANK
%token <string> FILE
%token <string> PIECE
%token CAPTURE
%token PROMOTION
%token KCASTLE QCASTLE
%token EOF

%{open Types%}
%{open Move%}
%start <t> main
%%

main:
  | m = specialmove { m }

specialmove:
  | m=move p=promotion EOF { PawnPromotion (m, p) }
  | m=move             EOF { Regular (m) }
  | KCASTLE            EOF { Castle false }
  | QCASTLE            EOF { Castle true }

move:
  | p=piecespec             CAPTURE ep=position { Normal (p, true, ep) }
  | p=piecespec sp=position CAPTURE ep=position { Full (p, sp, true, ep) }
  | p=piecespec f=file      CAPTURE ep=position { Filed (p, f, true, ep) }
  | p=piecespec r=rank      CAPTURE ep=position { Ranked (p, r, true, ep) }

  | p=piecespec                     ep=position { Normal (p, false, ep) }
  | p=piecespec sp=position         ep=position { Full (p, sp, false, ep) }
  | p=piecespec f=file              ep=position { Filed (p, f, false, ep) }
  | p=piecespec r=rank              ep=position { Ranked (p, r, false, ep) }

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
