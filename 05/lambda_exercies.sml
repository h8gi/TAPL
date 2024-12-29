(* boolean *)
val tru = fn t => fn _ => t
val fls = fn _ => fn f => f
val test = fn l => fn m => fn n => l m n
val t_and = fn b => fn c => b c fls
val t_or = fn b => fn c => b tru c
val t_not = fn b => test b fls tru

(* pairs *)
val pair = fn f => fn s => fn b => b f s
val fst = fn p => p tru
val snd = fn p => p fls
(* fst (pair 1 2) *)

(* Church Numerals *)
val c0 = fn s => fn z => z
val c1 = fn s => fn z => s z
val c2 = fn s => fn z => s (s z)
val c3 = fn s => fn z => s (s (s z))

val scc = fn n => fn s => fn z => s (n s z)
val plus = fn m => fn n => fn s => fn z => m s (n s z)
(* val times = fn m => fn n => m (plus n) c0; *)
(* val times = fn m => fn n => fn s => fn z => m (n s) z *)
val times = fn m => fn n => fn s => m (n s)
val toInt = fn n => n (fn x => x + 1) 0
val toBool = fn b => b true false

val power = fn m => fn n => n m

val iszro = fn m => m (fn x => fls) tru

local
    val zz = fn () => pair c0 c0	(* to prevent expansion *)
    val ss = fn p => pair (snd p) (plus c1 (snd p))
in
val prd = fn m => fst (m ss (zz ()))
end

val minus = fn m => fn n => n prd m
val realbool = fn b => b true false
val churchbool = fn b => if b then tru else fls
