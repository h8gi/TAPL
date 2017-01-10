include("./lexer.jl")
using LispLexer

type Obj
    kind::Symbol
    value::Any
end

cons(a::Obj, b::Obj) = Obj(:pair, (a, b))

function match_readtoken(lex::Lexer, kind::Symbol)
    if currenttoken(lex).kind == kind
        readtoken(lex)
    else
        error("syntax error", currenttoken(lex));
    end
end

function data(lex::Lexer)
    token = currenttoken(lex)
    if token.kind == :open
        list(lex)
    elseif token.kind == :vectoropen
        vector(lex)
    else
        readtoken(lex)
        Obj(token.kind, token.value)
    end
end

function list(lex::Lexer)
    match_readtoken(lex, :open)    
    acc = []
    while true
        if currenttoken(lex).kind == :close
            match_readtoken(lex, :close)
            break;
        else
            push!(acc, data(lex))
        end
    end
    return Obj(:list, acc)
end

function vector(lex::Lexer)
    match_readtoken(lex, :vectoropen)    
    acc = []
    while true
        if currenttoken(lex).kind == :close
            match_readtoken(lex, :close)
            break;
        else
            push!(acc, data(lex))
        end
    end
    return Obj(:vector, acc)
end

function repl()
end
