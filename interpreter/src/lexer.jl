module Lexer

using ..Errors: Errors

struct LoxLexError <: Errors.LoxError
    offset::Int
    message::String
end
Errors.get_offset(err::LoxLexError) = err.offset
Errors.get_message(err::LoxLexError) = err.message

abstract type Token end
struct LeftParen <: Token end
struct RightParen <: Token end
struct LeftBrace <: Token end
struct RightBrace <: Token end
struct Comma <: Token end
struct Dot <: Token end
struct Minus <: Token end
struct Plus <: Token end
struct Semicolon <: Token end
struct Slash <: Token end
struct Star <: Token end
struct Bang <: Token end
struct BangEqual <: Token end
struct Equal <: Token end
struct EqualEqual <: Token end
struct Greater <: Token end
struct GreaterEqual <: Token end
struct Less <: Token end
struct LessEqual <: Token end
struct Identifier <: Token
    lexeme::String
end
struct LoxString <: Token
    value::String
end
struct LoxNumber <: Token
    value::Float64
end
struct And <: Token end
struct Class <: Token end
struct Else <: Token end
struct False <: Token end
struct Fun <: Token end
struct For <: Token end
struct If <: Token end
struct Nil <: Token end
struct Or <: Token end
struct Print <: Token end
struct Return <: Token end
struct Super <: Token end
struct This <: Token end
struct True <: Token end
struct Var <: Token end
struct While <: Token end
struct Eof <: Token end

# A token with an associated offset in the source string
struct LocatedToken{T<:Token}
    token::T
    offset::Int
end

# Pretty printing, so that REPL output isn't a deluge of types
function Base.show(io::IO, t::Token)
    if t isa Identifier
        print(io, "Identifier{$(t.lexeme)}")
    elseif t isa LoxString
        print(io, "LoxString{$(t.value)}")
    elseif t isa LoxNumber
        print(io, "LoxNumber{$(t.value)}")
    else
        print(io, nameof(typeof(t)))
    end
end
function Base.show(io::IO, lt::LocatedToken)
    print(io, "{")
    Base.show(io, lt.token)
    print(io, "@", lt.offset, "}")
end

# ================================================================
# Lexer state and basic functions to manipulate it.

mutable struct LexerState{S<:AbstractString}
    "the current position in the source string; begins at 0"
    position::Int
    "the string being lexed"
    source::S
    "length of `source`, cached for efficiency"
    source_len::Int
    "tokens parsed so far"
    tokens::Vector{LocatedToken}
    "errors encountered during lexing"
    errors::Vector{LoxLexError}

    function LexerState(source::S) where {S<:AbstractString}
        new{S}(0, source, length(source), LocatedToken[], LoxLexError[])
    end
end


function increment_position!(s::LexerState, n::Int=1)
    s.position += n
end

function is_at_end(s::LexerState)::Bool
    return s.position >= s.source_len
end

function add_token!(s::LexerState, token::Token)
    push!(s.tokens, LocatedToken(token, s.position))
end

function add_error!(s::LexerState, error::LoxLexError)
    push!(s.errors, error)
end

# ================================================================
# Helper functions for lexing.

"""
    get_char!(s::LexerState)::Union{Char,Nothing}

Get the next character from the source string, given the number of characters
already read.

If there are no more characters to read, returns `nothing`.

Otherwise, consumes the character (i.e. advances the position of `s`) and returns it.
"""
function get_char!(s::LexerState)::Union{Char,Nothing}
    if is_at_end(s)
        return nothing
    else
        increment_position!(s)
        return s.source[s.position]
    end
end

"""
    get_char_if_eq!(s::LexerState, expected_char::Char)::Bool

Check if the next character in `s.source` is equal to `expected_char`. If so,
consumes and returns `true`. Otherwise, does not consume the character and
returns `false`.

If there are no more characters to read, returns `false`.
"""
function get_char_if_eq!(s::LexerState, expected_char::Char)::Bool
    if is_at_end(s)
        return false
    else
        actual_char = s.source[s.position+1]
        if actual_char == expected_char
            increment_position!(s)
            return true
        else
            return false
        end
    end
end

"""
    peek_char_eq(s::LexerState, expected_char::Char)::Bool

Check if the next character in `s.source` is equal to `expected_char`, without
consuming it.
"""
function peek_char_eq(s::LexerState, expected_char::Char)::Bool
    if is_at_end(s)
        return false
    else
        return s.source[s.position+1] == expected_char
    end
end

"""
Read all characters while a predicate `pred` is true. Does not include the
first character that does not match the predicate.
"""
function consume_while!(s::LexerState, pred::Function)::String
    word = ""
    while !is_at_end(s)
        next = s.source[s.position+1]
        if pred(next)
            word *= next
            increment_position!(s) # ok can consume it
        else
            break
        end
    end
    return word
end

"""
Read all characters until (but NOT including) a designated marker.
"""
function consume_until!(s::LexerState, marker::Char)::String
    consume_while!(s, c -> c != marker)
end

# ================================================================
# The actual lexing.

function identifier(lexeme::AbstractString)::Token
    if lexeme == "and"
        return And()
    elseif lexeme == "class"
        return Class()
    elseif lexeme == "else"
        return Else()
    elseif lexeme == "false"
        return False()
    elseif lexeme == "fun"
        return Fun()
    elseif lexeme == "for"
        return For()
    elseif lexeme == "if"
        return If()
    elseif lexeme == "nil"
        return Nil()
    elseif lexeme == "or"
        return Or()
    elseif lexeme == "print"
        return Print()
    elseif lexeme == "return"
        return Return()
    elseif lexeme == "super"
        return Super()
    elseif lexeme == "this"
        return This()
    elseif lexeme == "true"
        return True()
    elseif lexeme == "var"
        return Var()
    elseif lexeme == "while"
        return While()
    else
        return Identifier(lexeme)
    end
end

"""
Read the next token from `s.source`, starting from the character at index
`s.position` If the token is successfully read, it is pushed onto `s.tokens`,
and `s.position` is updated.
"""
function read_next_token!(s::LexerState)::Nothing
    next_char = get_char!(s)
    if isnothing(next_char)
        throw(LoxLexError(s.position, "Unexpected end of input"))
    elseif next_char == '('
        add_token!(s, LeftParen())
    elseif next_char == ')'
        add_token!(s, RightParen())
    elseif next_char == '{'
        add_token!(s, LeftBrace())
    elseif next_char == '}'
        add_token!(s, RightBrace())
    elseif next_char == ','
        add_token!(s, Comma())
    elseif next_char == '.'
        add_token!(s, Dot())
    elseif next_char == '-'
        add_token!(s, Minus())
    elseif next_char == '+'
        add_token!(s, Plus())
    elseif next_char == ';'
        add_token!(s, Semicolon())
    elseif next_char == '*'
        add_token!(s, Star())
    elseif next_char == '!'
        next_char_is_equals = get_char_if_eq!(s, '=')
        next_token = next_char_is_equals ? BangEqual() : Bang()
        add_token!(s, next_token)
    elseif next_char == '='
        next_char_is_equals = get_char_if_eq!(s, '=')
        next_token = next_char_is_equals ? EqualEqual() : Equal()
        add_token!(s, next_token)
    elseif next_char == '>'
        next_char_is_equals = get_char_if_eq!(s, '=')
        next_token = next_char_is_equals ? GreaterEqual() : Greater()
        add_token!(s, next_token)
    elseif next_char == '<'
        next_char_is_equals = get_char_if_eq!(s, '=')
        next_token = next_char_is_equals ? LessEqual() : Less()
        add_token!(s, next_token)
    elseif next_char == '/'
        next_char_is_slash = get_char_if_eq!(s, '/')
        if next_char_is_slash
            # line comment beginning with //
            consume_until!(s, '\n')
        else
            add_token!(s, Slash())
        end
    elseif isspace(next_char) # whitespace is a no-op
    elseif next_char == '"'
        # string literal
        str = consume_until!(s, '"')
        closing_quote = get_char!(s)
        if closing_quote != '"'
            throw(LoxLexError(s.position, "Unterminated string literal"))
        end
        add_token!(s, LoxString(str))
    elseif isdigit(next_char)
        # number literal. grab the bit before the decimal point
        num = next_char * consume_while!(s, isdigit)
        # check for a decimal point
        if peek_char_eq(s, '.')
            # check if it's a digit after the decimal point
            # TODO: Kind of ugly that we need to dig into the internals of `s`.
            if s.position + 2 <= s.source_len && isdigit(s.source[s.position+2])
                increment_position!(s) # consume the decimal point
                after_decimal = consume_while!(s, isdigit)
                num *= '.' * after_decimal
            end
            # otherwise don't consume the decimal point.
        end
        value = parse(Float64, num)
        add_token!(s, LoxNumber(value))
    elseif isletter(next_char) || next_char == '_'
        # identifier or keyword
        # Note: Julia's isletter accepts Unicode letters too...
        is_valid_identifier_char(c::Char) = isletter(c) || isdigit(c) || c == '_'
        lexeme = next_char * consume_while!(s, is_valid_identifier_char)
        add_token!(s, identifier(lexeme))
    else
        add_error!(s, LoxLexError(s.position, "Unexpected character: '$next_char'."))
    end
    return nothing
end

function lex(source::AbstractString)::Tuple{Vector{LocatedToken},Vector{LoxLexError}}
    s = LexerState(source)
    while !is_at_end(s)
        read_next_token!(s)
    end
    add_token!(s, Eof())
    return s.tokens, s.errors
end

end # module
