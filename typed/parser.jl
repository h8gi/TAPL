include("./lexer.jl")

module LispParser
using LispLexer
export cons, Obj

type Obj
    kind::Symbol
    value::Any
end

cons(a::Obj, b::Obj) = Obj(:pair, (a, b))

function parse(stream::IO)
    
end

end

