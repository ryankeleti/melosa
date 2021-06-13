open Base

type t =
  | Unknown
  | Range of Lexing.position * Lexing.position

let pp f = function
  | Unknown -> Fmt.string f "unknown"
  | Range (s, e) ->
     Fmt.pf f "%s[%d:%d-%d:%d]" s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol) e.pos_lnum (e.pos_cnum - e.pos_bol)

let sexp_of_t = function
  | Unknown -> Sexp.Atom "unknown"
  | Range (s, e) ->
     Sexp.Atom (Printf.sprintf "%s[%d:%d-%d:%d]" s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol) e.pos_lnum (e.pos_cnum - e.pos_bol))

let from_lexbuf lexbuf =
  Range (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

