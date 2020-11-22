%{
%}

%token <int> INT
%token EOF

%type <int list> program

%start program

%%

program:
| i=INT+; EOF {i}