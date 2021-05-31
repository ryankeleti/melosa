open Base

type t =
  | Id of string
  | Dot of t * string

let list id =
  let rec help acc = function
    | Id s -> s :: acc
    | Dot (id, s) -> help (s :: acc) id in
  help [] id

let pp f =
  let rec help f = function
    | Id s -> Fmt.string f s
    | Dot (id, s) -> Fmt.pf f "%a.%s" help id s in
  Fmt.quote help f

let sexp_of_t id =
  Sexp.Atom (String.concat ~sep:"." (list id))

