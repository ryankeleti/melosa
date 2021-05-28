open Base
open Cmdliner

let print past =
  past
  |> Melosa_parsing.Past.show_structure
  |> Stdio.print_endline

let read fname =
  let ic = Stdio.In_channel.create fname in
  let lexbuf = Lexing.from_channel ic in
  let past = Melosa_parsing.Handler.parse ~fname lexbuf in
  match past with
  | Ok past -> print past
  | Error e -> Stdio.print_endline e

let source =
  let doc = "the .osa file to compile" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"file" ~doc)

let cmd =
  let doc = "melosa compiler" in
  Term.(const read $ source),
  Term.(info "melosa" ~version:"v0.0.1" ~doc ~exits:default_exits)

let () =
  Term.(exit @@ eval cmd)


