functor UntypedLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Untyped_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\017\000\002\000\017\000\005\000\017\000\006\000\017\000\000\000\
\\001\000\001\000\018\000\002\000\018\000\005\000\018\000\006\000\018\000\000\000\
\\001\000\001\000\019\000\002\000\019\000\005\000\019\000\006\000\019\000\000\000\
\\001\000\001\000\020\000\002\000\020\000\005\000\020\000\006\000\020\000\000\000\
\\001\000\001\000\007\000\002\000\016\000\005\000\016\000\006\000\005\000\000\000\
\\001\000\001\000\007\000\003\000\006\000\006\000\005\000\000\000\
\\001\000\002\000\015\000\005\000\015\000\000\000\
\\001\000\002\000\012\000\000\000\
\\001\000\004\000\011\000\000\000\
\\001\000\005\000\000\000\000\000\
\\001\000\006\000\009\000\000\000\
\"
val actionRowNumbers =
"\005\000\004\000\000\000\003\000\
\\010\000\005\000\001\000\008\000\
\\007\000\005\000\002\000\006\000\
\\009\000"
val gotoT =
"\
\\001\000\012\000\002\000\002\000\003\000\001\000\000\000\
\\002\000\006\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\008\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\011\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 13
val numrules = 6
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | appterm of unit ->  (Term)
 | aterm of unit ->  (Term) | term of unit ->  (Term)
end
type svalue = MlyValue.svalue
type result = Term
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 4) => true | _ => false
val showTerminal =
fn (T 0) => "LPAREN"
  | (T 1) => "RPAREN"
  | (T 2) => "LAMBDA"
  | (T 3) => "DOT"
  | (T 4) => "EOF"
  | (T 5) => "ID"
  | (T 6) => "ILLCH"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 6) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.term term1, _, term1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, LAMBDA1left, _)) :: rest671)) =>
 let val  result = MlyValue.term (fn _ => let val  (ID as ID1) = ID1
 ()
 val  (term as term1) = term1 ()
 in ((Abs (ID, term)))
end)
 in ( LrTable.NT 0, ( result, LAMBDA1left, term1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.appterm appterm1, appterm1left, 
appterm1right)) :: rest671)) => let val  result = MlyValue.term (fn _
 => let val  (appterm as appterm1) = appterm1 ()
 in ((appterm))
end)
 in ( LrTable.NT 0, ( result, appterm1left, appterm1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.aterm aterm1, aterm1left, aterm1right)) :: 
rest671)) => let val  result = MlyValue.appterm (fn _ => let val  (
aterm as aterm1) = aterm1 ()
 in ((aterm))
end)
 in ( LrTable.NT 2, ( result, aterm1left, aterm1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.aterm aterm1, _, aterm1right)) :: ( _, ( 
MlyValue.appterm appterm1, appterm1left, _)) :: rest671)) => let val  
result = MlyValue.appterm (fn _ => let val  (appterm as appterm1) = 
appterm1 ()
 val  (aterm as aterm1) = aterm1 ()
 in ((App (appterm, aterm)))
end)
 in ( LrTable.NT 2, ( result, appterm1left, aterm1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.term term1, _
, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.aterm (fn _ => let val  (term as term1) = term1 ()
 in ((term))
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.aterm (fn _ => let val  (ID as ID1) = ID1
 ()
 in ((Var ID))
end)
 in ( LrTable.NT 1, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.term x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Untyped_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun LAMBDA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun ILLCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
end
end
