# lexer.jl
# scheme lexer
module LispLexer
export Lexer, lexer, readtoken, currenttoken

type Info
    position::Int
end

type Token
    kind::Symbol
    value::Any
    info::Info
end

Token(sym::Symbol, val) = Token(sym, val, Info(0))

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

function readnumber(stream::IO, sign::Char = '+')
    numstr = string(sign, readwhile(stream, isnumber))
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
        Token(:abbrev_special, "unquote-splicing")
    else
        Token(:abbrev_special, "unquote")
    end
end

tokenkinds = [:open, :close, :char, :string, :bool, :symbol, :abbrev_special,
              :special, :vectoropen, :dot, :eof]

function readtoken(stream::IO)    
    readwhile(stream, isspace)
    if eof(stream)
        return Token(:eof, "EOF")
    end

    pos = position(stream)
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
    elseif c == '+' || c == '-'
        readnumber(stream, c)
    elseif c == '#'        
        readaftersharp(stream)
    elseif c == '\''
        Token(:abbrev_special, "quote")
    elseif c == '`'
        Token(:abbrev_special, "quasiquote")
    elseif c == ','
        readunquote(stream)
    elseif c == '.'
        Token(:dot, ".")
    elseif c == ';'
        Token(:comment, chomp(readline(stream)))
    else
        error("Illegal token char", c)
    end

    token.info = Info(pos)
    unmark(stream)
    return token
end
# Lexer at julia/base/io.jl 
type Lexer
    stream::IO
    ondone::Function
    currenttoken::Token
    Lexer(stream) = Lexer(stream, ()->nothing, Token(:start, "start"))
    Lexer(stream, ondone) = new(stream, ondone, Token(:start, "start"))
end

function lexer(stream::IO)
    Lexer(stream)
end

function lexer(filename::AbstractString)
    s = open(filename)
    Lexer(s, ()->close(s))
end

Base.start(itr::Lexer) = nothing
function Base.done(itr::Lexer, nada)
    if !eof(itr.stream)
        return false
    end
    itr.ondone()
    true
end
function Base.next(itr::Lexer, nada)
    readtoken(itr)
    (itr.currenttoken, nothing)
end

Base.eltype(::Type{Lexer}) = Token
Base.iteratorsize(::Type{Lexer}) = Base.SizeUnknown()

function readtoken(itr::Lexer)
    itr.currenttoken = readtoken(itr.stream)
end

function currenttoken(itr::Lexer)
    itr.currenttoken
end

end
