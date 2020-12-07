%{
    open Ast
%}

%token C_SEP
%token C_HS
%token L_SEP

%token MODULE
%token MACRO
%token MY
%token ON
%token RETURN
%token INIT
%token DEINIT
%token OPERATOR
%token CLASS
%token ALIAS
%token TYPE
%token KIND
%token CATEGORY
%token PROTOCOL
%token IS
%token OF
%token USE
%token HAS
%token IF
%token ORIF
%token ELSE
%token WHILE
%token FOR
%token DO
%token CASE
%token MATCH
%token AT
%token BREAK
%token NEXT
%token THROW
%token TRY
%token CATCH

%token STATIC
%token HIDDEN
%token READONLY
%token FRIEND
%token UNORDERED
%token GETTER
%token SETTER
%token MAIN
%token INLINE
%token NOINHERIT
%token PATTERN
%token ASM
%token STATEMENT
%token NATIVE
%token C_STRUCT
%token C_UNION
%token C_ENUM
%token FLAGS
%token UNCOUNTED
%token STRONG

%token L_IN
%token L_FROM
%token L_TO
%token L_UPTO
%token L_DOWNTO
%token L_BY
%token L_WHILE

%token Y_SCRIPT

%token TILDE
%token DOT
%token EQ
%token EQGT
%token PLUS
%token PLUSEQ
%token PLUSPLUS
%token MINUS
%token MINUSEQ
%token MINUSMINUS
%token STAR
%token STAREQ
%token STARSTAR
%token STARSTAREQ
%token DIV
%token DIVEQ
%token DIVDIV
%token DIVDIVEQ
%token MOD
%token MODEQ
%token MODMOD
%token MODMODEQ
%token AND
%token ANDEQ
%token ANDAND
%token ANDANDEQ
%token BAR
%token BAREQ
%token BARBAR
%token BARBAREQ
%token CARET
%token CARETEQ
%token CARETCARET
%token CARETCARETEQ
%token BANG
%token BANGEQ
%token BANGBANG
%token BANGBANGEQ
%token QUESTION
%token QUESTIONEQ
%token GT
%token GTEQ
%token GTGT
%token GTGTEQ
%token LT
%token LTEQ
%token LTLT
%token LTLTEQ
%token <int> CASCADE

%token LPAREN
%token LBRACKET
%token LBRACE
%token HASHLPAREN
%token HASHLBRACKET
%token HASHLBRACE
%token RPAREN
%token RBRACKET
%token RBRACE

%token <string> IDENT
%token <string> TYPE_NAME
%token <string> LABEL
%token <string> PUNNED
%token <string> TAG
%token <string> LITSYM

%token <int> INT
%token <float> DEC
%token <string> STR
%token <char> CHAR
%token <bool> BOOL
%token THIS
%token WILDCARD
%token <(* depth * nth *) int * int> ANON_ARG

%token EOF


%nonassoc
    C_SEP // a <newline>? , <newline> b
    L_SEP // a <newline> b

%left C_HS // a , b

%right EQGT // a => b

%right
    EQ           // a = b
    PLUSEQ       // a += b
    MINUSEQ      // a -= b
    STAREQ       // a *= b
    STARSTAREQ   // a **= b
    DIVEQ        // a /= b
    DIVDIVEQ     // a //= b
    MODEQ        // a %= b
    MODMODEQ     // a %%= b
    ANDEQ        // a &= b
    ANDANDEQ     // a &&= b
    BAREQ        // a |= b
    BARBAREQ     // a ||= b
    CARETEQ      // a ^= b
    CARETCARETEQ // a ^^= b
    BANGBANGEQ   // a !!= b
    GTGTEQ       // a >>= b
    LTLTEQ       // a >>= b

%left
    ANDAND     // a && b
    BARBAR     // a || b
    CARETCARET // a ^^ b
    BANGBANG   // a !! b

%left
    QUESTIONEQ // a ?= b
    BANGEQ     // a != b
    GT         // a > b
    GTEQ       // a >= b
    LT         // a < b
    LTEQ       // a <= b

%left
    AND   // a & b
    OR    // a | b
    CARET // a ^ b
    GTGT  // a >> b
    LTLT  // a << b

%left MODMOD // a %% b

%left
    PLUS  // a + b
    MINUS // a - b

%left
    STAR   // a * b
    DIV    // a / b
    DIVDIV // a // b
    MOD    // a % b

%right STARSTAR // a ** b

%nonassoc TAG // #a b

%right QUESTION // a?

%right
    TILDE       // ~a
    unary_minus // -a

%left BANG // !a

%nonassoc below_incr_decr

%nonassoc
    PLUSPLUS  // ++a, --a
    MINUSMINUS // a++, a--

// maybe change to %left
%nonassoc CASCADE // a -> b

%nonassoc below_top

%right DOT // a.b

%nonassoc top_prec


//%type <Ast.expr>

%type <Expr.t list> program

%type <Expr.t> literal basic_expr expr paren_expr full_expr type_expr named_type_expr

%type <Message.multi> multi_msg_contents
%type <Message.simple> simple_msg_contents
%type <Message.obj> obj_msg_contents

%type <Type.t> named_type wildcard_type any_type


%start program

//%on_error_reduce named_type_expr named_type named_short_type named_type_seg

%%


let program :=
    p = expr+;
    EOF;
    { p }


// Util

let wpos(rule) ==
    ~ = rule; { $startpos, rule }

let pos(rule) ==
    rule; { $startpos }


// Separators

let comma_sep ==
    | C_SEP
    | C_HS

let any_sep ==
    | comma_sep
    | L_SEP


// General

let ident :=
    | IDENT
    | STATIC; { "static" }
    | HIDDEN; { "hidden" }
    | READONLY; { "readonly" }
    | FRIEND; { "friend" }
    | UNORDERED; { "unordered" }
    | GETTER; { "getter" }
    | SETTER; { "setter" }
    | MAIN; { "main" }
    | INLINE; { "inline" }
    | NOINHERIT; { "noinherit" }
    | PATTERN; { "pattern" }
    | ASM; { "asm" }
    | STATEMENT; { "statement" }
    | NATIVE; { "native" }
    | C_STRUCT; { "c_struct" }
    | C_UNION; { "c_union" }
    | C_ENUM; { "c_enum" }
    | FLAGS; { "flags" }
    | UNCOUNTED; { "uncounted" }
    | STRONG; { "strong" }

let any_ident :=
    | ident
    | MODULE; { "module" }
    | MACRO; { "macro" }
    | MY; { "my" }
    | ON; { "on" }
    | RETURN; { "return" }
    | INIT; { "init" }
    | DEINIT; { "deinit" }
    | OPERATOR; { "operator" }
    | CLASS; { "class" }
    | ALIAS; { "alias" }
    | TYPE; { "type" }
    | KIND; { "kind" }
    | CATEGORY; { "category" }
    | PROTOCOL; { "protocol" }
    | IS; { "is" }
    | OF; { "of" }
    | USE; { "use" }
    | HAS; { "has" }
    | IF; { "if" }
    | ORIF; { "orif" }
    | ELSE; { "else" }
    | WHILE; { "while" }
    | FOR; { "for" }
    | DO; { "do" }
    | CASE; { "case" }
    | MATCH; { "match" }
    | AT; { "at" }
    | BREAK; { "break" }
    | NEXT; { "next" }
    | THROW; { "throw" }
    | TRY; { "try" }
    | CATCH; { "catch" }
    | b = BOOL; { if b then "true" else "false" }
    | THIS; { "this" }

let label :=
    | LABEL
    | L_IN; { "in" }
    | L_FROM; { "from" }
    | L_TO; { "to" }
    | L_UPTO; { "upto" }
    | L_DOWNTO; { "downto" }
    | L_BY; { "by" }
    | L_WHILE; { "while" }


// Literals

let literal :=
    | name
    | litsym
    | int
    | dec
    | char
    | str
    | bool
    | this
    | anon_arg

let litsym :=
    | l = LITSYM; { Expr.Litsym($startpos, l) }
    | Y_SCRIPT; { Expr.Litsym($startpos, "script") }

let name :=
    i = ident; { Expr.Name($startpos, i) }

let int :=
    i = INT; { Expr.Int($startpos, i) }

let dec :=
    d = DEC; { Expr.Dec($startpos, d) }

let char :=
    c = CHAR; { Expr.Char($startpos, c) }

let str :=
    s = STR; { Expr.Str($startpos, s) }

let bool :=
    b = BOOL; { Expr.Bool($startpos, b) }

let this :=
    THIS; { Expr.This $startpos }

let anon_arg :=
    (depth, index) = ANON_ARG; {
        (* weird menhir bug here? *)
        let loc = $startpos in
        Expr.Anon_arg {loc; depth; index}
    }


// Types

let type_params :=
    l = LBRACKET;
    L_SEP?;
    p = separated_nonempty_list(any_sep, any_type);
    L_SEP?;
    r = RBRACKET;
    { $startpos(l), p, $startpos(r) }

let named_type_seg ==
    ~ = TYPE_NAME; <Type.Name>

let wildcard_type_seg ==
    WILDCARD; { Type.Wildcard }

let named_short_type ==
    s = named_type_seg;
    p = type_params?;
    { $startpos(s), s, p }

let basic_wildcard_short_type ==
    s = wildcard_type_seg;
    { $startpos(s), s, None }

let wildcard_short_type ==
    s = wildcard_type_seg;
    p = type_params?;
    { $startpos(s), s, p }

let wildcard_type :=
    w = wildcard_short_type; { [w] }

let named_type_path :=
    p = loption(terminated(named_type_path, DOT));
    t = named_short_type;
    { p @ [t] }

let named_type ==
    append(
        terminated(basic_wildcard_short_type, DOT)*,
        named_type_path //separated_nonempty_list(DOT, named_short_type)
    )

let any_type :=
    | named_type
    | wildcard_type

let type_expr :=
    ~ = any_type; <Expr.Type>

let named_type_expr ==
    ~ = named_type; <Expr.Type>


// Delimiters

let delims_of(l, rule, r) ==
    ~ = l;
    ~ = rule;
    ~ = r;
    { $startpos(l), rule, $startpos(r) }

(*let assoc_of(l, elem, r) ==
    ~ = l;
    L_SEP?;
    elems = separated_list(any_sep, elem);
    L_SEP?;
    ~ = r;
    { $startpos(l), elems, $startpos(r) }*)
let assoc_of(l, elem, r) ==
    delims_of(
        terminated(l, L_SEP?),
        separated_list(any_sep, elem),
        preceded(L_SEP?, r)
    )

let array_of(elem) ==
    assoc_of(HASHLBRACKET, elem, RBRACKET)

let hash_of(k, v) ==
    assoc_of(HASHLPAREN, separated_pair(k, EQGT, v), RPAREN)

let tuple_of(elem) == 
    assoc_of(HASHLBRACE, elem, RBRACE)


// Expressions

let basic_expr :=
    | literal
    | array
    | hash
    | tuple
    | paren
    //| func
    //| block
    //| objc_call

let array :=
    ~ = array_of(full_expr); <Expr.Array>

let hash :=
    ~ = hash_of(basic_expr, full_expr); <Expr.Hash>

let tuple :=
    ~ = tuple_of(full_expr); <Expr.Tuple>

let paren :=
    ~ = delims_of(
        terminated(LPAREN, L_SEP?),
        separated_nonempty_list(comma_sep, paren_expr),
        preceded(L_SEP?, RPAREN)
    );
    <Expr.Paren>


let multi_msg_begin ==
    | ~ = wpos(PUNNED); <Label.Punned>
    | ~ = wpos(label); L_SEP?; ~ = full_expr; <Label.Named>

let multi_msg_contents :=
    b = multi_msg_begin;
    r = list(
        | preceded(any_sep?, multi_msg_begin)
        | ~ = preceded(comma_sep, full_expr); <Label.Anon>
    );
    { b :: r }

let simple_msg_contents ==
    | s = wpos(ident); { `Single s }
    | m = multi_msg_contents; { `Multi m }

let obj_msg_contents ==
    | s = simple_msg_contents; { (s :> Message.obj) }
    | c = named_type; { `Cast c }

let msg_of(contents) ==
    terminated(LBRACKET, L_SEP?);
    ~ = contents;
    preceded(L_SEP?, RBRACKET);
    <>

let simple_msg :=
    msg_of(simple_msg_contents)

let obj_msg :=
    msg_of(obj_msg_contents)


let op_1 ==
    | STAR; { `Times }
    | DIV; { `Div }
    | DIVDIV; { `IntDiv }
    | MOD; { `Mod }

let expr_of(self, sep) :=
    | ~ = named_type_expr; DOT; ~ = ident; <Expr.Member> %prec below_top
    | ~ = self; DOT; ~ = ident; <Expr.Member> %prec below_top
    
    | ~ = self; ~ = obj_msg; <Expr.Obj_message> %prec below_top

    | o = pos(PLUSPLUS); e = self; { Expr.Prefix(o, Prefix.Incr, e) } //%prec below_incr_decr
    | o = pos(MINUSMINUS); e = self; { Expr.Prefix(o, Prefix.Decr, e) } //%prec below_incr_decr
    | e = self; o = midrule(pos(PLUSPLUS)); { Expr.Postfix(e, o, Postfix.Incr) } %prec below_incr_decr
    | e = self; o = midrule(pos(MINUSMINUS)); { Expr.Postfix(e, o, Postfix.Decr) } %prec below_incr_decr

    | o = pos(BANG); e = self; { Expr.Prefix(o, Prefix.Not, e) }

    | o = pos(TILDE); e = self; { Expr.Prefix(o, Prefix.Compl, e) }
    | o = pos(MINUS); e = self; { Expr.Prefix(o, Prefix.Neg, e) } %prec unary_minus

    | e = self; o = pos(QUESTION); { Expr.Postfix(e, o, Postfix.Truthy) }

    | t = wpos(TAG); e = self; { Expr.Tag(t, e) }

    | l = self; o = pos(STARSTAR); r = self; { Expr.Infix(l, o, `Pow, r) }

    | l = self; (p, o) = wpos(op_1); r = self; <Expr.Infix>

    | basic_expr

let expr :=
    expr_of(expr, {()})

let paren_expr :=
    expr_of(paren_expr, L_SEP?)

let full_expr :=
    expr