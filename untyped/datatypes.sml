signature DATATYPES =
sig
    datatype Term = Var of string
		  | Abs of string * Term
		  | App of Term * Term
end;

structure DataTypes : DATATYPES =
struct
datatype Term = Var of string
	      | Abs of string * Term
	      | App of Term * Term
end;
