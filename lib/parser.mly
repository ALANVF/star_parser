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

//%nonassoc tag

%nonassoc
    PLUSPLUS   // ++a, a++
    MINUSMINUS // --a, a--

%right
    TILDE       // ~a
    unary_minus // -a

%left BANG // !a

%right QUESTION // a?

%left DOT // a.b

// maybe change to %left
%nonassoc CASCADE // a -> b


//%type <Ast.expr>

%type <expr list> program


%start program

%%


program: literal+; EOF { $1 }


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
    | LITSYM; { ELitsym($startpos, _1) }
    | Y_SCRIPT; { ELitsym($startpos, "script") }

let name :=
    | IDENT; { EName($startpos, _1) }
    | STATIC; { EName($startpos, "static") }
    | HIDDEN; { EName($startpos, "hidden") }
    | READONLY; { EName($startpos, "readonly") }
    | FRIEND; { EName($startpos, "friend") }
    | UNORDERED; { EName($startpos, "unordered") }
    | GETTER; { EName($startpos, "getter") }
    | SETTER; { EName($startpos, "setter") }
    | MAIN; { EName($startpos, "main") }
    | INLINE; { EName($startpos, "inline") }
    | NOINHERIT; { EName($startpos, "noinherit") }
    | PATTERN; { EName($startpos, "pattern") }
    | ASM; { EName($startpos, "asm") }
    | STATEMENT; { EName($startpos, "statement") }
    | NATIVE; { EName($startpos, "native") }
    | C_STRUCT; { EName($startpos, "c_struct") }
    | C_UNION; { EName($startpos, "c_union") }
    | C_ENUM; { EName($startpos, "c_enum") }
    | FLAGS; { EName($startpos, "flags") }
    | UNCOUNTED; { EName($startpos, "uncounted") }
    | STRONG; { EName($startpos, "strong") }

int: INT { EInt($startpos, $1) }

dec: DEC { EDec($startpos, $1) }

char: CHAR { EChar($startpos, $1) }

str: STR { EStr($startpos, $1) }

bool: BOOL { EBool($startpos, $1) }

this: THIS { EThis $startpos }

anon_arg: ANON_ARG {
    (* weird menhir bug here? *)
    let loc = $startpos in
    let (depth, index) = $1 in
    EAnon_arg {
        loc;
        depth;
        index
    }
}