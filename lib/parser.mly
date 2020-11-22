%{
    (*open Ast*)
%}

%token C_SEP
%token C_HS
%token L_SEP

%token <string> IDENT
%token <string> TYPE_NAME
%token <string> LABEL
%token <string> PUNNED
%token <string> TAG

%token <int> INT
%token <float> DEC
%token <string> STR
%token <char> CHAR

%token EOF

%type <int list> program

%start program

%%

program:
| i=INT+; EOF {i}