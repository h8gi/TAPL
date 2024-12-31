datatype namedTerm =
	 NamedVar of string
	 | NamedAbs of string * namedTerm
	 | NamedApp of namedTerm * namedTerm

fun showNamedTerm (NamedVar x) = x
  | showNamedTerm (NamedAbs (x, body)) =
    "λ" ^ x ^ ". " ^ showNamedTerm body
  | showNamedTerm (NamedApp (t1, t2)) =
    "(" ^ showNamedTerm t1 ^ " " ^ showNamedTerm t2 ^ ")"

(* let *)
(*     val t = Abs("x", App(Var "x", Var "y")) *)
(* in *)
(*     showNamedTerm t *)
(* end *)
datatype term =
	 Var of int
         | Abs of term
         | App of term * term

fun showTerm t =
    case t of
        Var i => Int.toString i
      | Abs body => "λ. " ^ showTerm body
      | App (t1, t2) => "(" ^ showTerm t1 ^ " " ^ showTerm t2 ^ ")"

type namingContext = string list
val emptyContext : namingContext = []
fun addVar (ctx, x) : namingContext =
    x :: ctx

fun indexOf (ctx: namingContext, x: string) : int =
    let
        fun loop ([], _) = ~1  (* not found *)
          | loop (y::rest, i) =
            if y = x then i
            else loop(rest, i+1)
    in
        loop (ctx, 0)
    end

fun varOfIndex (ctx: namingContext, i: int) : string =
  List.nth(ctx, i)
  handle Subscript => raise Fail "Index out of range"


fun removeNames (ctx: namingContext, t: namedTerm) =
    case t of
        NamedVar name => Var(indexOf(ctx, name))
      | NamedAbs (var, t1) => Abs (removeNames (addVar (ctx, var), t1))
      | NamedApp (t1, t2) => App (removeNames (ctx, t1), removeNames (ctx, t2))

fun freshNameByLength (ctx: namingContext) : string =
    "x" ^ Int.toString (List.length ctx)


fun restoreNames (ctx: namingContext, t: term) =
    case t of
        Var index => NamedVar(varOfIndex(ctx, index))
      | Abs t1 => let
          val newName = freshNameByLength ctx
          val newContext = newName :: ctx
      in
          NamedAbs(newName, restoreNames(newContext, t1))
      end
      | App (t1, t2) => NamedApp(restoreNames(ctx, t1), restoreNames(ctx, t2))
