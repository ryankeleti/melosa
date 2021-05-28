open Base

let handle lexbuf =
  let module I = Parser.MenhirInterpreter in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let succeed v = Ok v in
  let fail _ = Error ("TODO: parse error") in
  I.loop_handle succeed fail supplier

let parse ~fname lexbuf =
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  handle lexbuf (Parser.Incremental.structure_eof lexbuf.lex_curr_p)

