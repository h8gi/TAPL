open DataTypes

%%
%name Untyped
%term LPAREN | RPAREN | LAMBDA | DOT
    | EOF
    | ID of string
    | ILLCH
%nonterm term of Term
       | aterm of Term
       | appterm of Term

%start term
%pos int
%eop EOF

(* prec *)
%noshift EOF
%nonassoc LPAREN RPAREN LAMBDA DOT ILLCH

%nodefault
%verbose
%arg (fileName) : string
%%

term: LAMBDA ID DOT term ((Abs (ID, term)))
    | appterm ((appterm))

appterm: aterm ((aterm))
    | appterm aterm ((App (appterm, aterm)))

aterm: LPAREN term RPAREN ((term))
     | ID ((Var ID))
