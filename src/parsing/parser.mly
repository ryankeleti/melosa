%{
  open Past

  let span it (s, e) = Span.({ it; span = Range (s, e) })
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
%right    SEMI
%nonassoc ELSE
%left     IOP0 EQUAL
%right    IOP1
%left     IOP2 PLUS MINUS
%left     IOP3 STAR

%start structure_eof 
%type <Past.structure> structure_eof

%%

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

let exp :=
  | MATCH; e = exp; WITH; PIPE?; rs = rules; END;
     { span (Pexp_match (e, rs)) $loc }
  | FN; ps = nonempty_list(pat_simple); RARROW; e = exp;
     { span (Pexp_lambda (ps, e)) $loc }
  | LET; ~ = binds; IN; e = exp;
     { span (Pexp_let (binds, e)) $loc }
  | LET; REC; ~ = binds; IN; e = exp;
     { span (Pexp_let_rec (binds, e)) $loc }
  | IF; c = exp; THEN; t = exp; ELSE; e = exp;
     { span (Pexp_if (c, t, e)) $loc }
  | e1 = exp; SEMI; e2 = exp;
     { span (Pexp_seq (e1, e2)) $loc }
  | e = exp_comma;
     { e }

let exp_comma :=
  | es = separated_list2(COMMA, exp_infixop);
     { span (Pexp_tuple es) $loc }
  | e = exp_infixop;
     { e }

let exp_infixop :=
  | e1 = exp_infixop; op = infixop; e2 = exp_infixop;
     { span (Pexp_app (op, [e1; e2])) $loc }
  | e = exp_app;
     { e }

let exp_app :=
  | f = exp_prefix; args = nonempty_list(exp_prefix);
     { span (Pexp_app (f, args)) $loc }
  | e = exp_prefix;
     { e }

let exp_prefix :=
  | op = prefixop; e = exp_simple;
     { span (Pexp_app (op, [e])) $loc }
  | e = exp_simple;
     { e }

let exp_simple :=
  | id = val_lid;
     { span (Pexp_id id) $loc }
  | id = cons_lid;
     { span (Pexp_cons (id, None)) $loc }
  | id = cons_lid; e = exp;
     { span (Pexp_cons (id, Some e)) $loc }
  | c = const;
     { span (Pexp_const c) $loc }
  | LPAREN; RPAREN;
     { span Pexp_unit $loc }
  | LPAREN; e = exp; COLON; t = ty; RPAREN;
     { span (Pexp_annot (e, t)) $loc }
  | LPAREN; e = exp; RPAREN;
     { e }
  | BEGIN; e = exp; RPAREN;
     { e }

let val_id :=
  | id = LID;
     { id }
  | op = POP;
     { op }
  | op = infixop_id;
     { op }

let infixop_id ==
  | op = IOP0;
     { op }
  | op = IOP1;
     { op }
  | op = IOP2;
     { op }
  | op = IOP3;
     { op }
  | PLUS;
     { "+" }
  | MINUS;
     { "-" }
  | STAR;
     { "*" }
  | EQUAL;
     { "=" }

let infixop ==
  | op = infixop_id;
     { span (Pexp_id (Lid.Id op)) $loc }

let prefixop ==
  | op = POP;
     { span (Pexp_id (Lid.Id op)) $loc }

let const :=
  | i = INT;
     { Pconst_int i }
  | TRUE;
     { Pconst_bool true }
  | FALSE;
     { Pconst_bool false }

let bind :=
  | bind_pat = pat; EQUAL; bound = exp;
     { span { bind_pat; bound } $loc }

let binds :=
  separated_nonempty_list(AND, bind)

let rule :=
  | rule_pat = pat; RARROW; action = exp;
     { span { rule_pat; action } $loc }

let rules :=
  separated_nonempty_list(PIPE, rule)

let pat :=
  | p = pat_comma;
     { p }

let pat_comma :=
  | ps = separated_nonempty_list(COMMA, pat_cons);
     {
       match ps with
       | [p] -> p
       | _ -> span (Ppat_tuple ps) $loc
     }

let pat_cons :=
  | id = UID; p = pat_simple;
     { span (Ppat_cons (id, Some p)) $loc }
  | p = pat_simple;
     { p }

let pat_simple :=
  | id = LID;
     { span (Ppat_var id) $loc }
  | id = UID;
     { span (Ppat_cons (id, None)) $loc }
  | c = const;
     { span (Ppat_const c) $loc }
  | UNDERSCORE;
     { span Ppat_any $loc }
  | LPAREN; RPAREN;
     { span Ppat_unit $loc }
  | LPAREN; p = pat; RPAREN;
     { p }

let ty :=
  | t1 = ty; RARROW; t2 = ty;
     { span (Pty_arrow (t1, t2)) $loc }
  | t = ty_star;
     { t }

let ty_star :=
  | ts = separated_list2(STAR, ty_simple);
     { span (Pty_tuple ts) $loc }
  | t = ty_simple;
     { t }

let ty_simple :=
  | UNDERSCORE;
     { span Pty_any $loc }
  | LPAREN; t = ty; RPAREN;
     { t }
  | QUOTE; id = LID;
     { span (Pty_var id) $loc }
  | t = ty; id = ty_cons_lid;
     { span (Pty_cons (id, Some t)) $loc }
  | id = ty_cons_lid;
     { span (Pty_cons (id, None)) $loc }

let ty_def :=
  | TYPE; params = ty_params?; ty_id = LID; kind = preceded(EQUAL, ty_kind)?;
     { span { ty_id; kind; params } $loc }

let ty_param :=
  | QUOTE; id = LID;
     { span (Pty_var id) $loc }

let ty_params :=
  | p = ty_param;
     { [p] }
  | LPAREN; p = ty_param; COMMA; ps = separated_nonempty_list(COMMA, ty_param); RPAREN;
     { p :: ps }

let ty_kind :=
  | ~ = ty_variant;
     { ty_variant }

let ty_variant :=
  | PIPE?; variants = separated_nonempty_list(PIPE, ty_cons_def);
     { (Pkind_variant variants) }

let ty_cons_def :=
  | cons_id = UID;
     { span { cons_id; cons_arg = None } $loc }
  | cons_id = UID; OF; t = ty;
     { span { cons_id; cons_arg = Some t } $loc }

let rec_ty_def :=
  separated_nonempty_list(AND, ty_def)

let mexp :=
  | STRUCT; s = structure; END;
     { span (Pmexp_str s) $loc }
  | FUNC; LPAREN; param_id = UID; COLON; param_ty = mty; RPAREN; EQUAL; me = mexp;
     { span (Pmexp_func ({ param_id; param_ty }, me)) $loc }
  | me = mexp_app;
     { me }

let mexp_app :=
  | me1 = mexp; LPAREN; me2 = mexp; RPAREN;
     { span (Pmexp_app (me1, me2)) $loc } 
  | me = mexp_simple;
     { me }

let mexp_simple :=
  | id = mod_lid;
     { span (Pmexp_id id) $loc }
  | LPAREN; me = mexp; COLON; mt = mty; RPAREN;
     { span (Pmexp_annot (me, mt)) $loc }
  | LPAREN; me = mexp; RPAREN;
     { me }

let mty :=
  | id = mty_lid;
     { span (Pmty_id id) $loc }
  | SIG; s = signature; END;
     { span (Pmty_sig s) $loc } 
  | FUNC; LPAREN; param_id = UID; COLON; param_ty = mty; RPAREN; EQUAL; mt = mty;
     { span (Pmty_func ({ param_id; param_ty }, mt)) $loc }
  | LPAREN; mt = mty; RPAREN;
     { mt }

let mty_def :=
  | MOD; TYPE; mty_id = UID; mtype = preceded(EQUAL, mty)?;
     { span { mty_id; mtype } $loc }

let signature_item :=
  | VAL; id = LID; COLON; t = ty;
     { span (Psig_val (id, t)) $loc }
  | TYPE; td = rec_ty_def;
     { span (Psig_type td) $loc }
  | MOD; id = UID; COLON; mt = mty;
     { span (Psig_mod (id, mt)) $loc }
  | OPEN; id = mod_lid;
     { span (Psig_open id) $loc }
 
let signature :=
  | s = list(signature_item);
     { s }

let structure_item :=
  | LET; ~ = binds;
     { span (Pstr_let binds) $loc }
  | LET; REC; ~ = binds;
     { span (Pstr_let_rec binds) $loc }
  | td = rec_ty_def;
     { span (Pstr_type td) $loc }
  | MOD; id = UID; me = preceded(EQUAL, mexp);
     { span (Pstr_mod (id, me)) $loc }
  | mtd = mty_def;
     { span (Pstr_modtype mtd) $loc }
  | OPEN; id = mod_lid;
     { span (Pstr_open id) $loc } 
  | e = exp;
     { span (Pstr_exp e) $loc }

let structure :=
  | s = list(structure_item);
     { s }

let structure_eof :=
  | s = structure; EOF; 
     { s }

