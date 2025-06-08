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

"""
    get_char(chars_read::Int, source::String)::Union{Tuple{Int,Char},Tuple{Int,Nothing}}

Get the next character from the source string, given the number of characters
already read.

If there are no more characters to read, returns `nothing` as the second element.
"""
function get_char(
    chars_read::Int,
    source::AbstractString,
)::Union{Tuple{Int,Char},Tuple{Int,Nothing}}
    return if chars_read >= length(source)
        chars_read, nothing
    else
        chars_read + 1, source[chars_read+1]
    end
end

"""
    peek_char_eq(chars_read::Int, source::String, expected_char::Char)::Tuple{Int,Bool}

Check if the next character in `source` is equal to `expected_char`. If so,
consumes it and returns `true`. If not, does not consume it and returns
`false`.
"""
function peek_char_eq(
    chars_read::Int,
    source::AbstractString,
    expected_char::Char,
)::Tuple{Int,Bool}
    if chars_read >= length(source)
        return chars_read, false
    else
        actual_char = source[chars_read+1]
        return if actual_char == expected_char
            chars_read + 1, true
        else
            chars_read, false
        end
    end
end

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
Read the next token from `source`, starting from the character at index `chars_read`.
If the token is successfully read, it is pushed onto the `tokens` vector.

Returns the new value of `chars_read` after having read the token.
"""
function read_next_token!(
    chars_read::Int,
    source::AbstractString,
    start_loc::Location,
    tokens::Vector{Token},
    errors::Vector{LoxLexError},
)::Int
    chars_read, next_char = get_char(chars_read, source)
    if isnothing(next_char)
        current_location = identify_location(chars_read, source, start_loc)
        throw(LoxLexError(current_location, "Unexpected end of input"))
    elseif next_char == '('
        push!(tokens, LeftParen())
    elseif next_char == ')'
        push!(tokens, RightParen())
    elseif next_char == '{'
        push!(tokens, LeftBrace())
    elseif next_char == '}'
        push!(tokens, RightBrace())
    elseif next_char == ','
        push!(tokens, Comma())
    elseif next_char == '.'
        push!(tokens, Dot())
    elseif next_char == '-'
        push!(tokens, Minus())
    elseif next_char == '+'
        push!(tokens, Plus())
    elseif next_char == ';'
        push!(tokens, Semicolon())
    elseif next_char == '*'
        push!(tokens, Star())
    elseif next_char == '!'
        chars_read, next_char_is_equals = peek_char_eq(chars_read, source, '=')
        next_token = next_char_is_equals ? BangEqual() : Bang()
        push!(tokens, next_token)
    elseif next_char == '='
        chars_read, next_char_is_equals = peek_char_eq(chars_read, source, '=')
        next_token = next_char_is_equals ? EqualEqual() : Equal()
        push!(tokens, next_token)
    elseif next_char == '>'
        chars_read, next_char_is_equals = peek_char_eq(chars_read, source, '=')
        next_token = next_char_is_equals ? GreaterEqual() : Greater()
        push!(tokens, next_token)
    elseif next_char == '<'
        chars_read, next_char_is_equals = peek_char_eq(chars_read, source, '=')
        next_token = next_char_is_equals ? LessEqual() : Less()
        push!(tokens, next_token)
    elseif next_char == '/'
        chars_read, next_char_is_slash = peek_char_eq(chars_read, source, '/')
        if next_char_is_slash
            # line comment beginning with // -- read until end of line or file
            while chars_read < length(source) && source[chars_read+1] != '\n'
                chars_read += 1
            end
        else
            push!(tokens, Slash())
        end
    elseif isspace(next_char) # whitespace is a no-op
    elseif next_char == '"'
        # string literal
        start_pos = chars_read + 1
        while true
            chars_read += 1
            if source[chars_read] == '"'
                break
            elseif chars_read >= length(source)
                current_location = identify_location(chars_read, source, start_loc)
                push!(errors, LoxLexError("Unterminated string literal"))
                break
            end
        end
        end_pos = chars_read - 1
        push!(tokens, LoxString(source[start_pos:end_pos]))
    elseif isdigit(next_char)
        # number literal
        start_pos = chars_read
        # grab the bit before the decimal point
        while chars_read < length(source) && isdigit(source[chars_read+1])
            chars_read += 1
        end
        # check for a decimal point
        if chars_read < length(source) && source[chars_read+1] == '.'
            # check if it's a digit after the decimal point
            if chars_read + 1 < length(source) && isdigit(source[chars_read+2])
                chars_read += 1
                # grab the bit after the decimal point
                while chars_read < length(source) && isdigit(source[chars_read+1])
                    chars_read += 1
                end
            end
            # otherwise don't consume the decimal point.
        end
        end_pos = chars_read
        value = parse(Float64, source[start_pos:end_pos])
        push!(tokens, LoxNumber(value))
    elseif isletter(next_char) || next_char == '_'
        # identifier or keyword
        # Note: Julia's isletter accepts Unicode letters too
        start_pos = chars_read
        while chars_read < length(source) && (
            isletter(source[chars_read+1]) ||
            isdigit(source[chars_read+1]) ||
            source[chars_read+1] == '_'
        )
            chars_read += 1
        end
        end_pos = chars_read
        lexeme = source[start_pos:end_pos]
        push!(tokens, identifier(lexeme))
    else
        current_location = identify_location(chars_read, source, start_loc)
        push!(errors, LoxLexError(current_location, "Unexpected character: '$next_char'."))
    end
    return chars_read
end

"""
    lex(source::String, start_loc::Location)

Lex. `start_loc` refers to the location of the first character in `source`.
"""
function lex(source::AbstractString, start_loc::Location)::Tuple{Vector{Token},Vector{LoxLexError}}
    N = length(source)
    chars_read = 0
    tokens = Token[]
    errors = LoxLexError[]
    while chars_read < N
        chars_read = read_next_token!(chars_read, source, start_loc, tokens, errors)
    end
    return tokens, errors
end


end # module
