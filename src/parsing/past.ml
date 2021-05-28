open Span

type id = Lid.t
[@@deriving show]

type exp = exp' spanned
and exp' =
  | Pexp_unit
  | Pexp_id of id
  | Pexp_const of const
  | Pexp_annot of exp * ty
  | Pexp_tuple of exp list
  | Pexp_cons of id * exp option
  | Pexp_if of exp * exp * exp
  | Pexp_match of exp * rule list
  | Pexp_lambda of pat list * exp
  | Pexp_app of exp * exp list
  | Pexp_let of bind list * exp 
  | Pexp_let_rec of bind list * exp 
  | Pexp_seq of exp * exp
and bind = bind' spanned
and bind' = { bind_pat : pat; bound : exp }
and rule = rule' spanned
and rule' = { rule_pat : pat; action : exp }

and pat = pat' spanned
and pat' =
  | Ppat_any
  | Ppat_unit
  | Ppat_var of string
  | Ppat_const of const
  | Ppat_tuple of pat list
  | Ppat_cons of string * pat option
  | Ppat_or of pat * pat

and ty = ty' spanned
and ty' =
  | Pty_any
  | Pty_var of string 
  | Pty_arrow of ty * ty
  | Pty_tuple of ty list
  | Pty_cons of id * ty option

and const =
  | Pconst_int of string
  | Pconst_bool of bool

and mexp = mexp' spanned
and mexp' =
  | Pmexp_id of id
  | Pmexp_annot of mexp * mty
  | Pmexp_str of structure
  | Pmexp_app of mexp * mexp
  | Pmexp_func of func_param * mexp
and func_param = { param_id : string; param_ty : mty }

and mty = mty' spanned
and mty' =
  | Pmty_id of id
  | Pmty_sig of signature
  | Pmty_func of func_param * mty

and ty_decl = ty_info list
and ty_info = ty_info' spanned
and ty_info' = { ty_id : string; kind : ty_kind option; params : ty list option }
and ty_kind =
  | Pkind_variant of ty_cons list
and ty_cons = ty_cons' spanned
and ty_cons' = { cons_id : string; cons_args : ty list option }

and mty_decl = mty_decl' spanned
and mty_decl' = { mty_id : string; mtype : mty option }

and signature = signature_item list
and signature_item = signature_item' spanned
and signature_item' =
  | Psig_val of string * ty
  | Psig_type of ty_decl
  | Psig_modtype of mty_decl
  | Psig_open of id
  | Psig_mod of string * mty

and structure = structure_item list
and structure_item = structure_item' spanned
and structure_item' =
  | Pstr_let of bind list
  | Pstr_let_rec of bind list
  | Pstr_type of ty_decl
  | Pstr_modtype of mty_decl
  | Pstr_exp of exp
  | Pstr_open of id
  | Pstr_mod of string * mexp
[@@deriving show { with_path = false }]

