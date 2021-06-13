open Base

type t =
  | Id of string
  | Dot of t * string

let list =
  let rec help acc = function
    | Id x -> x :: acc
    | Dot (id, x) -> help (x :: acc) id in
  help []

let pp f =
  let rec help f = function
    | Id x -> Fmt.string f x
    | Dot (id, x) -> Fmt.pf f "%a.%s" help id x in
  Fmt.quote help f

let sexp_of_t id =
  Sexp.Atom (String.concat ~sep:"." (list id))

