signature UNTYPED =
sig
    type term

    val parse : string -> term
    val eval : term -> term
    val show : term -> string
end
