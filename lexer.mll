{
  open Parser

  exception ParseError of string
}

rule token = parse
| ['1'-'8'] as x { RANK (int_of_string (String.make 1 x)) }
| ['a'-'h'] as x { FILE (String.make 1 x) }
| ['K' 'Q' 'R' 'N' 'B'] as x { PIECE (String.make 1 x) }
| ['x'] { CAPTURE }
| ['='] { PROMOTION }
| eof { EOF }
| _ { raise (ParseError ("Unknown char: " ^ Lexing.lexeme lexbuf)) }