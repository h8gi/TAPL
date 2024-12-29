datatype term =
	 TmTrue
	 | TmFalse
	 | TmIf of term * term * term
	 | TmZero
	 | TmSucc of term
	 | TmPred of term
	 | TmIsZero of term

fun isnumericval TmZero = true
  | isnumericval (TmSucc t) = isnumericval t
  | isnumericval _ = false

fun isval TmTrue = true
  | isval TmFalse = true
  | isval t = isnumericval(t)

exception NoRuleApplies

fun eval1 (TmIf (TmTrue, t2, t3)) = t2
  | eval1 (TmIf (TmFalse, t2, t3)) = t3
  | eval1 (TmIf (t1, t2, t3)) =
    let val t1' = eval1 t1
    in
	TmIf (t1', t2, t3)
    end
  | eval1 (TmSucc t1) =
    let val t1' = eval1 t1
    in
	TmSucc t1'
    end
  | eval1 (TmPred TmZero) = TmZero
  | eval1 (TmPred (TmSucc nv1)) =
    if isnumericval nv1
    then nv1
    else
	raise NoRuleApplies
  | eval1 (TmPred t1) =
    let val t1' = eval1 t1 in
	TmPred t1'
    end
  | eval1 (TmIsZero TmZero) = TmTrue
  | eval1 (TmIsZero (TmSucc nv1)) =
    if isnumericval nv1
    then TmFalse
    else
	raise NoRuleApplies
  | eval1 (TmIsZero t1) =
    let val t1' = eval1 t1 in
	TmIsZero t1'
    end
  | eval1 _ = raise NoRuleApplies

fun eval t =
    let val t' = eval1 t in
	eval t'
    end
    handle NoRuleApplies => t
