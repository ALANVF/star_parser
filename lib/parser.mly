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