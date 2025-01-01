structure Untyped :> UNTYPED =
struct
datatype term =
         TmVar of int
         | TmAbs of term
         | TmAppp of term * term
(* dummy parser *)
(* TODO: read https://www.smlnj.org/doc/ml-lpt/manual.pdf *)
fun parse (_: string) : term = TmVar 0
end
