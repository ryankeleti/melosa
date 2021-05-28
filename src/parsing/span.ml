type pos = Lexing.position

type t =
  | Unknown
  | Range of pos * pos

let pp f = function
  | Unknown -> Format.fprintf f "unknown"
  | Range (s, e) ->
     Format.fprintf f "%s[%d:%d-%d:%d]" s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol) e.pos_lnum (e.pos_cnum - e.pos_bol)

type 'a spanned = { it : 'a; span : t }
[@@deriving show { with_path = false }]

let from_lexbuf lexbuf =
  Range (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

