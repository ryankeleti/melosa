{
  open Base
  open Parser

  type error =
    | Illegal_char of char

  exception Error of error * Span.t

  let keywords = Hashtbl.of_alist_exn (module String) [
    "and", AND;
    "begin", BEGIN;
    "else", ELSE;
    "end", END;
    "false", FALSE;
    "fn", FN;
    "func", FUNC;
    "if", IF;
    "in", IN;
    "let", LET;
    "match", MATCH;
    "mod", MOD;
    "of", OF;
    "open", OPEN;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "with", WITH;
  ]
}

let ws = [' ' '\t']+
let nl = '\n' | '\r' | "\r\n"
let id = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let lid = ['a'-'z' '_'] id*
let uid = ['A'-'Z'] id*
let int_ = '-'? ['0'-'9']+
let sym = ['!' '%' '&' '$' '+' '-' '/' ':' '<' '=' '>' '?' '@' '\\' '~' '\'' '^' '|' '*']
let pop = ['!' '~'] sym*
let iop0 = ['=' '<' '>' '|' '&' '$'] sym*
let iop1 = ['@' '^'] sym*
let iop2 = ['+' '-'] sym*
let iop3 = ['*' '/'] sym*

rule token = parse
  | ws         { token lexbuf }
  | nl         { Lexing.new_line lexbuf; token lexbuf }
  | "--"       { line_comment lexbuf }
  | "_"        { UNDERSCORE }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | ","        { COMMA }
  | ":"        { COLON }
  | "."        { DOT }
  | ";"        { SEMI }
  | "*"        { STAR }
  | "'"        { QUOTE }
  | "="        { EQUAL }
  | "->"       { RARROW }
  | "|"        { PIPE }
  | "+"        { PLUS }
  | "-"        { MINUS }
  | lid as id  { match Hashtbl.find keywords id with
                 | Some kw -> kw
                 | None -> LID id }
  | uid as id  { UID id }
  | pop as op  { POP op }
  | iop0 as op { IOP0 op }
  | iop1 as op { IOP1 op }
  | iop2 as op { IOP2 op }
  | iop3 as op { IOP3 op }
  | int_ as i  { INT i }
  | eof        { EOF }
  | _ as c     { raise (Error (Illegal_char c, Span.from_lexbuf lexbuf)) }

and line_comment = parse
  | nl  { Lexing.new_line lexbuf; token lexbuf } 
  | eof { EOF }
  | _   { line_comment lexbuf }

