%{
    (*open Ast*)
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

%type <int list> program

%start program

%%

program:
| i=INT+; EOF {i}