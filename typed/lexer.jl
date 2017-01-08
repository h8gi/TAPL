# lexer.jl
# scheme lexer
module LispLexer

export eachtoken, readtokens

type Info
    msg::String
end

type Token
    kind::Symbol
    value::Any
    info::Info
end

Token(sym::Symbol, val) = Token(sym, val, Info(""))

function peekchar(stream::IO)
    mark(stream)
    c = read(stream, Char)
    reset(stream)
    c
end

function readwhile(stream::IO, predicate::Function)
    function loop(acc::String)
        if eof(stream)
            return acc
        end

        let char = peekchar(stream)
            if predicate(char)
                loop(string(acc, read(stream, Char)))                
            else
                acc
            end
        end
    end
    loop("")
end

function readdquote(stream::IO)
    function loop(acc::String)
        if eof(stream)
            return acc
        end

        let char = read(stream, Char)
            if char == '\\'
                loop(string(acc, char, read(stream, Char)))
            elseif char == '"'
                acc
            elseif char == '\n'
                loop(string(acc, '\\', 'n'))
            else
                loop(string(acc, char))
            end
        end
    end
    loop("")
end


function isspecialchar(char::Char)
    char in "!\$%&*/:<=>?^_~"
end

function isheadchar(char::Char)
    isalpha(char) || isspecialchar(char)
end

function issuccchar(char::Char)
    isheadchar(char) || isnumber(char) ||
        char == '+' || char == '-' || char == '.' || char == '@'
end

function readsymbol(stream::IO, head::Char)
    symstr = string(head, readwhile(stream, issuccchar))
    if symstr in ["else", "=>", "define", "unquote", "unquote-splicing",
                  "quote", "lambda", "if", "set!", "begin", "cond", "and", "or",
                  "case", "let", "let*", "letrec", "do", "delay", "quasiquote"]
        Token(:special, symstr)
    else
        Token(:symbol, symstr)
    end
end

function readnumber(stream::IO)
    numstr = readwhile(stream, isnumber)
    if peekchar(stream) == '.'
        numstr = string(numstr, read(stream, Char), readwhile(stream, isnumber))
    end
    Token(:number, numstr)
end

function readaftersharp(stream::IO)
    c = read(stream, Char)
    if c == '('
        Token(:vectoropen, "#(")
    elseif c == 't'
        Token(:bool, "true")
    elseif c == 'f'
        Token(:bool, "false")
    elseif c == '\\'
        Token(:char, read(stream, Char))
    else
        error("Illegal sharp token", c)
    end
end

function readunquote(stream::IO)
    if  peekchar(stream) == '@'
        read(stream, Char)
        Token(:special, "unquote-splicing")
    else
        Token(:special, "unquote")
    end
end

function readtoken(stream::IO)    
    readwhile(stream, isspace)
    if eof(stream)
        return Token(:eof, "EOF")
    end

    mark(stream)
    c = read(stream, Char)
    
    token = if c == '('        
        Token(:open, "(")
    elseif c == ')'
        Token(:close, ")")
    elseif c == '"'
        Token(:string,  readdquote(stream))
    elseif isheadchar(c)
        Token(:symbol,  readsymbol(stream, c))
    elseif isnumber(c)
        reset(stream)
        readnumber(stream)
    elseif c == '#'        
        readaftersharp(stream)
    elseif c == '\''
        Token(:special, "quote")
    elseif c == '`'
        Token(:special, "quasiquote")
    elseif c == ','
        readunquote(stream)
    elseif c == '.'
        Token(:dot, ".")
    else
        error("Illegal token char", c)
    end
    
    unmark(stream)
    return token
end

# EachLine at julia/base/io.jl 
type EachToken
    stream::IO
    ondone::Function
    EachToken(stream) = EachToken(stream, ()->nothing)
    EachToken(stream, ondone) = new(stream, ondone)
end
eachtoken(stream::IO) = EachToken(stream)
function eachtoken(filename::AbstractString)
    s = open(filename)
    EachToken(s, ()->close(s))
end
Base.start(itr::EachToken) = nothing
function Base.done(itr::EachToken, nada)
    if !eof(itr.stream)
        return false
    end
    itr.ondone()
    true
end
Base.next(itr::EachToken, nada) = (readtoken(itr.stream), nothing)
Base.eltype(::Type{EachToken}) = Token
Base.iteratorsize(::Type{EachToken}) = Base.SizeUnknown()

readtokens(s=STDIN) = collect(eachtoken(s))

end
