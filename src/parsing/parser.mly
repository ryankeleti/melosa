%{
  open Past

  let span it (s, e) = Past.{ it; span = Range (s, e) }
%}

%token AND           "and"
%token BEGIN         "begin"
%token COLON         ":"
%token COMMA         ","
%token DOT           "."
%token ELSE          "else"
%token END           "end"
%token EOF           ""
%token EQUAL         "="
%token FALSE         "false"
%token FN            "fn"
%token FUNC          "func"
%token IF            "if" 
%token IN            "in"
%token LET           "let"
%token LPAREN        "("
%token MATCH         "match"
%token MOD           "mod"
%token MINUS         "-"
%token OF            "of"
%token OPEN          "open"
%token PIPE          "|"
%token PLUS          "+"
%token QUOTE         "'"
%token REC           "rec"
%token RARROW        "->"
%token RPAREN        ")"
%token SEMI          ";"
%token SIG           "sig"
%token STAR          "*"
%token STRUCT        "struct"
%token THEN          "then"
%token TRUE          "true"
%token TRY           "try"
%token TYPE          "type"
%token UNDERSCORE    "_"
%token VAL           "val"
%token WITH          "with"

%token <string> LID  "lid"
%token <string> UID  "Uid"
%token <string> INT  "0"
%token <string> POP  "!!" 
%token <string> IOP0
%token <string> IOP1
%token <string> IOP2
%token <string> IOP3

%nonassoc IN
%right    RARROW
%nonassoc ELSE
%left     IOP0 EQUAL
%right    IOP1
%left     IOP2 PLUS MINUS
%left     IOP3 STAR
%nonassoc LID LPAREN UID

%start structure_eof 
%type <Past.structure> structure_eof

%%

let spanned(X) ==
  | x = X;
     { span x $loc }

let op(X) ==
  | x = X;
     { span (Pexp_id (Lid.Id x)) $loc }

let separated_list2(sep, X) :=
  | x1 = X; sep; x2 = X;
     { [x1; x2] }
  | x = X; sep; xs = separated_list2(sep, X);
     { x :: xs }

let lid(dot, id) :=
  | ~ = id;
     { Lid.Id id }
  | ~ = dot; DOT; ~ = id;
     { Lid.Dot (dot, id) }

let mod_lid :=
  lid(mod_lid, UID)

let mty_lid :=
  lid(mod_lid, UID)

let cons_lid :=
  lid(mod_lid, UID)

let val_lid :=
  lid(mod_lid, val_id)

let ty_cons_lid :=
  lid(mod_lid, LID)

let val_id ==
  | LID
  | delimited(LPAREN, POP, RPAREN)
  | delimited(LPAREN, infixop, RPAREN)

let infixop ==
  | IOP0
  | IOP1
  | IOP2
  | IOP3
  | PLUS;
     { "+" }
  | MINUS;
     { "-" }
  | STAR;
     { "*" }
  | EQUAL;
     { "=" }

let exp_seq :=
  | exp
  | e1 = exp; SEMI; e2 = exp_seq;
     { span (Pexp_seq (e1, e2)) $loc }

let exp :=
  | exp_simple
  | spanned(_exp)
  | LET; ~ = binds; IN; e = exp;
     { span (Pexp_let (binds, e)) $loc }
  | LET; REC; ~ = binds; IN; e = exp;
     { span (Pexp_let_rec (binds, e)) $loc }

let _exp ==
  | f = exp_simple; args = nonempty_list(exp_simple);
     { Pexp_app (f, args) }
  | es = separated_list2(COMMA, exp);
     { Pexp_tuple es }
  | id = cons_lid; e = exp_simple;
     { Pexp_cons (id, Some e) }
  | e1 = exp; op = op(infixop); e2 = exp;
     { Pexp_app (op, [e1; e2]) }
  | MATCH; e = exp_seq; WITH; PIPE?; rs = rules; END;
     { Pexp_match (e, rs) }
  | FN; ps = nonempty_list(pat_simple); RARROW; e = exp_seq;
     { Pexp_lambda (ps, e) }
  | IF; c = exp_seq; THEN; t = exp; ELSE; e = exp;
     { Pexp_if (c, t, e) }
 
let exp_simple :=
  | delimited(LPAREN, exp_seq, RPAREN)
  | delimited(BEGIN, exp_seq, END)
  | LPAREN; (e, t) = separated_pair(exp_seq, COLON, ty); RPAREN;
     { span (Pexp_annot (e, t)) $loc }
  | id = val_lid;
     { span (Pexp_id id) $loc }
  | id = cons_lid;
     { span (Pexp_cons (id, None)) $loc }
  | c = const;
     { span (Pexp_const c) $loc }
  | op = op(POP); e = exp_simple;
     { span (Pexp_app (op, [e])) $loc }
  | LPAREN; RPAREN;
     { span Pexp_unit $loc }

let const ==
  | i = INT;
     { Pconst_int i }
  | TRUE;
     { Pconst_bool true }
  | FALSE;
     { Pconst_bool false }

let bind ==
  | bind_pat = pat; EQUAL; bound = exp;
     { span { bind_pat; bound } $loc }

let binds ==
  separated_nonempty_list(AND, bind)

let rule ==
  | rule_pat = pat; RARROW; action = exp;
     { span { rule_pat; action } $loc }

let rules ==
  separated_nonempty_list(PIPE, rule)

let pat ==
  | pat_comma

let pat_comma :=
  | ps = separated_nonempty_list(COMMA, pat_cons);
     {
       match ps with
       | [p] -> p
       | _ -> span (Ppat_tuple ps) $loc
     }

let pat_cons ==
  | id = cons_lid; p = pat_simple;
     { span (Ppat_cons (id, Some p)) $loc }
  | pat_simple

let pat_simple :=
  | id = val_lid;
     { span (Ppat_var id) $loc }
  | id = cons_lid;
     { span (Ppat_cons (id, None)) $loc }
  | c = const;
     { span (Ppat_const c) $loc }
  | UNDERSCORE;
     { span Ppat_any $loc }
  | LPAREN; RPAREN;
     { span Ppat_unit $loc }
  | delimited(LPAREN, pat, RPAREN)

let ty :=
  | t1 = ty; RARROW; t2 = ty;
     { span (Pty_arrow (t1, t2)) $loc }
  | ty_star

let ty_star :=
  | ts = separated_list2(STAR, ty_simple);
     { span (Pty_tuple ts) $loc }
  | ty_simple

let ty_simple :=
  | UNDERSCORE;
     { span Pty_any $loc }
  | delimited(LPAREN, ty, RPAREN)
  | id = preceded(QUOTE, LID);
     { span (Pty_var id) $loc }
  | t = ty; id = ty_cons_lid;
     { span (Pty_cons (id, Some t)) $loc }
  | id = ty_cons_lid;
     { span (Pty_cons (id, None)) $loc }

let ty_def :=
  | TYPE; params = ty_params?; ty_id = LID; kind = preceded(EQUAL, ty_kind);
     { span { ty_id; kind; params } $loc }
  | TYPE; params = ty_params?; ty_id = LID;
     { span { ty_id; kind = Pkind_abstract; params } $loc }

let ty_param :=
  | id = preceded(QUOTE, LID);
     { span (Pty_var id) $loc }

let ty_params :=
  | p = ty_param;
     { [p] }
  | LPAREN; p = ty_param; COMMA; ps = separated_nonempty_list(COMMA, ty_param); RPAREN;
     { p :: ps }

let ty_kind ==
  | ty_variant

let ty_variant :=
  | PIPE?; variants = separated_nonempty_list(PIPE, ty_cons_def);
     { Pkind_variant variants }

let ty_cons_def :=
  | cons_id = UID;
     { span { cons_id; cons_arg = None } $loc }
  | cons_id = UID; OF; t = ty;
     { span { cons_id; cons_arg = Some t } $loc }

let rec_ty_def :=
  separated_nonempty_list(AND, ty_def)

let mexp :=
  | s = delimited(STRUCT, structure, END);
     { span (Pmexp_str s) $loc }
  | FUNC; (param_id, param_ty) = delimited(LPAREN, separated_pair(UID, COLON, mty), RPAREN); EQUAL; me = mexp;
     { span (Pmexp_func ({ param_id; param_ty }, me)) $loc }
  | mexp_app

let mexp_app :=
  | me1 = mexp; me2 = delimited(LPAREN, mexp, RPAREN);
     { span (Pmexp_app (me1, me2)) $loc }
  | mexp_simple

let mexp_simple :=
  | id = mod_lid;
     { span (Pmexp_id id) $loc }
  | LPAREN; (me, mt) = separated_pair(mexp, COLON, mty); RPAREN;
     { span (Pmexp_annot (me, mt)) $loc }
  | delimited(LPAREN, mexp, RPAREN)

let mty :=
  | id = mty_lid;
     { span (Pmty_id id) $loc }
  | s = delimited(SIG, signature, END);
     { span (Pmty_sig s) $loc }
  | FUNC; LPAREN; (param_id, param_ty) = separated_pair(UID, COLON, mty); RPAREN; EQUAL; mt = mty;
     { span (Pmty_func ({ param_id; param_ty }, mt)) $loc }
  | delimited(LPAREN, mty, RPAREN)

let mty_def :=
  | MOD; TYPE; (mty_id, mtype) = pair(UID, preceded(EQUAL, mty)?);
     { span { mty_id; mtype } $loc }

let signature_item := spanned(_signature_item)
let _signature_item :=
  | VAL; (id, t) = separated_pair(val_id, COLON, ty);
     { Psig_val (id, t) }
  | td = preceded(TYPE, rec_ty_def);
     { Psig_type td }
  | MOD; (id, mt) = separated_pair(UID, COLON, mty);
     { Psig_mod (id, mt) }
  | id = preceded(OPEN, mod_lid);
     { Psig_open id }
 
let signature :=
  | list(signature_item)

let structure_item := spanned(_structure_item)
let _structure_item :=
  | VAL; ~ = binds;
     { Pstr_val binds }
  | VAL; REC; ~ = binds;
     { Pstr_val_rec binds }
  | td = rec_ty_def;
     { Pstr_type td }
  | MOD; (id, me) = pair(UID, preceded(EQUAL, mexp));
     { Pstr_mod (id, me) }
  | mtd = mty_def;
     { Pstr_modtype mtd }
  | id = preceded(OPEN, mod_lid);
     { Pstr_open id }
  | e = exp_seq;
     { Pstr_exp e }

let structure :=
  | list(structure_item)

let structure_eof :=
  | ~ = structure; EOF; <>

