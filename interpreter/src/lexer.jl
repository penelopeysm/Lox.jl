module Lexer

using ..Errors: Location, LoxError, identify_location

struct LoxLexError <: LoxError
    location::Location
    message::String
end
get_location(err::LoxLexError) = err.location
get_message(err::LoxLexError) = err.message

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
    tokens::Vector{Token}
    "errors encountered during lexing"
    errors::Vector{LoxLexError}

    function LexerState(source::S) where {S<:AbstractString}
        new{S}(0, source, length(source), Token[], LoxLexError[])
    end
end


function increment_position!(s::LexerState, n::Int=1)
    s.position += n
end

function is_at_end(s::LexerState)::Bool
    return s.position >= s.source_len
end

function add_token!(s::LexerState, token::Token)
    push!(s.tokens, token)
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
function get_char_if_eq!(
    s::LexerState,
    expected_char::Char,
)::Bool
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
function peek_char_eq(
    s::LexerState,
    expected_char::Char,
)::Bool
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
function consume_while!(
    s::LexerState,
    pred::Function,
)::String
    word = ""
    while true
        is_at_end(s) && return word
        next = s.source[s.position+1]
        if pred(next)
            word *= next
            increment_position!(s) # ok can consume it
        else
            return word # don't consume it!
        end
    end
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
function read_next_token!(s::LexerState, start_loc::Location)::Nothing
    next_char = get_char!(s)
    if isnothing(next_char)
        current_location = identify_location(s.position, s.source, start_loc)
        throw(LoxLexError(current_location, "Unexpected end of input"))
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
            current_location = identify_location(s.position, s.source, start_loc)
            throw(LoxLexError(current_location, "Unterminated string literal"))
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
        current_location = identify_location(s.position, s.source, start_loc)
        add_error!(s, LoxLexError(current_location, "Unexpected character: '$next_char'."))
    end
    return nothing
end

"""
    lex(source::String, start_loc::Location)

Lex. `start_loc` refers to the location of the first character in `source`.
"""
function lex(
    source::AbstractString,
    start_loc::Location,
)::Tuple{Vector{Token},Vector{LoxLexError}}
    s = LexerState(source)
    while !is_at_end(s)
        read_next_token!(s, start_loc)
    end
    add_token!(s, Eof())
    return s.tokens, s.errors
end

end # module
