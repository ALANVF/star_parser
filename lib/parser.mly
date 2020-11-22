%{
    (*open Ast*)
%}

%token C_SEP
%token C_HS
%token L_SEP

%token K_MODULE
%token K_MACRO
%token K_MY
%token K_ON
%token K_RETURN
%token K_INIT
%token K_DEINIT
%token K_OPERATOR
%token K_CLASS
%token K_ALIAS
%token K_TYPE
%token K_KIND
%token K_CATEGORY
%token K_PROTOCOL
%token K_IS
%token K_OF
%token K_USE
%token K_HAS
%token K_IF
%token K_ORIF
%token K_ELSE
%token K_WHILE
%token K_FOR
%token K_DO
%token K_CASE
%token K_MATCH
%token K_AT
%token K_BREAK
%token K_NEXT
%token K_THROW
%token K_TRY
%token K_CATCH

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

%token EOF

%type <int list> program

%start program

%%

program:
| i=INT+; EOF {i}