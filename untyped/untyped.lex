structure T = Tokens
type pos = int
type svalue = T.svalue		(* semantic value *)
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;
val badCh : string * string * int * int -> unit =
 fn (fileName, bad, line, col) =>
    TextIO.output(TextIO.stdOut,
		  fileName^"["
		  ^Int.toString line^"."^Int.toString col
		  ^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin, !col);

%%
%full
%header (functor UntypedLexFun(structure Tokens: Untyped_TOKENS));
%arg (fileName:string);
%s UNTYPED;
alpha = [A-Za-z];
id = {alpha}({alpha}|[0-9])*;
ws = [\ \t\n];
eol = "\n";
%%
<INITIAL>{ws}* => (lin:=1; eolpos:=0;
		   YYBEGIN UNTYPED; continue ());
<UNTYPED>{ws}* => (continue ());
<UNTYPED>{eol} => (lin:=(!lin)+1;
		   eolpos:=yypos+size yytext; continue());
<UNTYPED>"("   => (col:=yypos-(!eolpos); T.LPAREN(!lin, !col));
<UNTYPED>")"   => (col:=yypos-(!eolpos); T.RPAREN(!lin, !col));
<UNTYPED>"\\"  => (col:=yypos-(!eolpos); T.LAMBDA(!lin, !col));
<UNTYPED>"."   => (col:=yypos-(!eolpos); T.DOT(!lin, !col));
<UNTYPED>{id}  => (col:=yypos-(!eolpos); T.ID(yytext, !lin, !col));
<UNTYPED>. => (col:=yypos-(!eolpos);
	       badCh (fileName, yytext, !lin, !col);
	       T.ILLCH(!lin, !col));
