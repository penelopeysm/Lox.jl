module Parser

# TODO: Every LoxParseError in here should have a proper location

using ..Errors: Errors, LoxError, Location
using ..Lexer: Lexer

export parse, to_sexp

struct LoxParseError <: LoxError
    location::Location
    message::String
end
Errors.get_location(err::LoxParseError) = err.location
Errors.get_message(err::LoxParseError) = err.message

abstract type LoxExpr end

abstract type LoxDeclaration end
struct LoxVarDeclaration{Tex<:Union{LoxExpr,Nothing}} <: LoxDeclaration
    identifier::String
    initial_expr::Tex
end
abstract type LoxStatement <: LoxDeclaration end
struct LoxExprStatement{Tex<:LoxExpr} <: LoxStatement
    expression::Tex
end
struct LoxPrintStatement{Tex<:LoxExpr} <: LoxStatement
    expression::Tex
end
struct LoxBlockStatement <: LoxStatement
    statements::Vector{LoxDeclaration}
end

struct LoxProgramme
    statements::Vector{LoxDeclaration}
end
Base.:(==)(prg1::LoxProgramme, prg2::LoxProgramme) = prg1.statements == prg2.statements

# Indeed, we're very quickly reaching the point where we want GADTs...
abstract type LoxBinaryOp end

abstract type LoxBinaryEqualityOp <: LoxBinaryOp end
struct EqualEqual <: LoxBinaryEqualityOp end
struct BangEqual <: LoxBinaryEqualityOp end

abstract type LoxBinaryComparisonOp <: LoxBinaryOp end
struct LessThan <: LoxBinaryComparisonOp end
struct LessThanEqual <: LoxBinaryComparisonOp end
struct GreaterThan <: LoxBinaryComparisonOp end
struct GreaterThanEqual <: LoxBinaryComparisonOp end

abstract type LoxBinaryMulDivOp <: LoxBinaryOp end
struct Multiply <: LoxBinaryMulDivOp end
struct Divide <: LoxBinaryMulDivOp end

abstract type LoxBinaryAddSubOp <: LoxBinaryOp end
struct Add <: LoxBinaryAddSubOp end
struct Subtract <: LoxBinaryAddSubOp end

struct LoxBinary{Top<:LoxBinaryOp,Tex1<:LoxExpr,Tex2<:LoxExpr} <: LoxExpr
    operator::Top
    left::Tex1
    right::Tex2
end

abstract type LoxUnaryOp end
struct Bang <: LoxUnaryOp end
struct MinusUnary <: LoxUnaryOp end
struct LoxUnary{Top<:LoxUnaryOp,Tex<:LoxExpr} <: LoxExpr
    operator::Top
    right::Tex
end

struct LoxGrouping{Tex<:LoxExpr} <: LoxExpr
    expression::Tex
end

# In theory, we _could_ use these type parameters for static type checking
# purposes...
struct LoxLiteral{T<:Union{Float64,Bool,String,Nothing}} <: LoxExpr
    value::T
    LoxLiteral(value::Number) = new{Float64}(Float64(value))
    LoxLiteral(value::T) where {T} = new{T}(value)
end
struct LoxVariable <: LoxExpr
    identifier::String
end

## Printing
to_sexp(expr::LoxLiteral{Float64}) = string(expr.value)
to_sexp(expr::LoxLiteral{Bool}) = expr.value ? "true" : "false"
to_sexp(expr::LoxLiteral{String}) = "\"" * expr.value * "\""
to_sexp(expr::LoxLiteral{Nothing}) = "nil"
to_sexp(var::LoxVariable) = ("var'" * var.identifier * "'")
to_sexp(expr::LoxGrouping) = "(group " * to_sexp(expr.expression) * ")"
to_sexp(expr::LoxUnary) = "(" * to_sexp_op(expr.operator) * " " * to_sexp(expr.right) * ")"
to_sexp(expr::LoxBinary) =
    "(" *
    to_sexp_op(expr.operator) *
    " " *
    to_sexp(expr.left) *
    " " *
    to_sexp(expr.right) *
    ")"
to_sexp_op(op::EqualEqual) = "=="
to_sexp_op(op::BangEqual) = "!="
to_sexp_op(op::LessThan) = "<"
to_sexp_op(op::LessThanEqual) = "<="
to_sexp_op(op::GreaterThan) = ">"
to_sexp_op(op::GreaterThanEqual) = ">="
to_sexp_op(op::Add) = "+"
to_sexp_op(op::Subtract) = "-"
to_sexp_op(op::Multiply) = "*"
to_sexp_op(op::Divide) = "/"
to_sexp_op(op::Bang) = "!"
to_sexp_op(op::MinusUnary) = "-"
to_sexp(stmt::LoxPrintStatement) = "(print " * to_sexp(stmt.expression) * ")"
to_sexp(stmt::LoxStatement) = "(stmt " * to_sexp(stmt.expression) * ")"
to_sexp(var_decl::LoxVarDeclaration{Nothing}) = "(var " * var_decl.identifier * ")"
to_sexp(var_decl::LoxVarDeclaration{Tex}) where {Tex} =
    "(var " * var_decl.identifier * " = " * to_sexp(var_decl.initial_expr) * ")"
to_sexp(stmt::LoxBlockStatement) = "(block " * join(map(to_sexp, stmt.statements), " ") * ")"
to_sexp(prg::LoxProgramme) = join(map(to_sexp, prg.statements), "\n")

function expression(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxExpr}
    return equality(tokens_read, tokens, start_loc, parse_errors)
end

function left_associative_binary(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
    operand_parser::Function,
    operator_mapping::Dict{<:Lexer.Token,<:LoxBinaryOp},
)::Tuple{Int,LoxExpr}
    tokens_read, left_expr = operand_parser(tokens_read, tokens, start_loc, parse_errors)
    next_token = tokens[tokens_read+1]
    while haskey(operator_mapping, next_token)
        # consume the operator
        tokens_read += 1
        operator = operator_mapping[next_token]
        tokens_read, right_expr =
            operand_parser(tokens_read, tokens, start_loc, parse_errors)
        left_expr = LoxBinary(operator, left_expr, right_expr)
        next_token = tokens[tokens_read+1]
    end
    return tokens_read, left_expr
end

function equality(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxExpr}
    operator_mapping =
        Dict(Lexer.EqualEqual() => EqualEqual(), Lexer.BangEqual() => BangEqual())
    return left_associative_binary(
        tokens_read,
        tokens,
        start_loc,
        parse_errors,
        comparison,
        operator_mapping,
    )
end

function comparison(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxExpr}
    operator_mapping = Dict(
        Lexer.Less() => LessThan(),
        Lexer.LessEqual() => LessThanEqual(),
        Lexer.Greater() => GreaterThan(),
        Lexer.GreaterEqual() => GreaterThanEqual(),
    )
    return left_associative_binary(
        tokens_read,
        tokens,
        start_loc,
        parse_errors,
        term,
        operator_mapping,
    )
end

function term(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxExpr}
    operator_mapping = Dict(Lexer.Plus() => Add(), Lexer.Minus() => Subtract())
    return left_associative_binary(
        tokens_read,
        tokens,
        start_loc,
        parse_errors,
        factor,
        operator_mapping,
    )
end

function factor(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxExpr}
    operator_mapping = Dict(Lexer.Star() => Multiply(), Lexer.Slash() => Divide())
    return left_associative_binary(
        tokens_read,
        tokens,
        start_loc,
        parse_errors,
        unary,
        operator_mapping,
    )
end

function unary(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxExpr}
    next_token = tokens[tokens_read+1]
    if next_token isa Lexer.Bang || next_token isa Lexer.Minus
        tokens_read += 1
        operator = if next_token isa Lexer.Bang
            Bang()
        elseif next_token isa Lexer.Minus
            MinusUnary()
        else
            error("wut")
        end
        tokens_read, right_expr = unary(tokens_read, tokens, start_loc, parse_errors)
        return tokens_read, LoxUnary(operator, right_expr)
    else
        return primary(tokens_read, tokens, start_loc, parse_errors)
    end
end

function primary(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxExpr}
    next_token = tokens[tokens_read+1]
    if next_token isa Lexer.False
        return tokens_read + 1, LoxLiteral(false)
    elseif next_token isa Lexer.True
        return tokens_read + 1, LoxLiteral(true)
    elseif next_token isa Lexer.Nil
        return tokens_read + 1, LoxLiteral(nothing)
    elseif next_token isa Lexer.LoxNumber || next_token isa Lexer.LoxString
        return tokens_read + 1, LoxLiteral(next_token.value)
    elseif next_token isa Lexer.Identifier
        return tokens_read + 1, LoxVariable(next_token.lexeme)
    elseif next_token isa Lexer.LeftParen
        tokens_read += 1
        tokens_read, expr = expression(tokens_read, tokens, start_loc, parse_errors)
        if tokens[tokens_read+1] isa Lexer.RightParen
            # Consume the right paren
            tokens_read += 1
            return tokens_read, LoxGrouping(expr)
        else
            throw(LoxParseError(start_loc, "Expected ')' after expression"))
        end
    else
        # parse failure
        throw(LoxParseError(start_loc, "Parse error: " * string(next_token)))
    end
end

"""
Consume tokens until we reach somewhere we can resume parsing from.

In particular, this function returns `tokens_read` such that the _next_ token (i.e.
`tokens[tokens_read + 1]`) is the first token that parsing could conceivably begin from.

If there are no such tokens, then this function returns `tokens_read` such that
`tokens[tokens_read + 1]` is the EOF token.
"""
function synchronise(tokens_read::Int, tokens::Vector{Lexer.Token})::Int
    # Skip over the first token, because we failed to parse it
    tokens_read += 1
    while true
        # Check the current token first before looking ahead, because there might
        # not be anything to look ahead to
        this_token = tokens[tokens_read]
        if this_token isa Lexer.Semicolon
            return tokens_read
        end
        if this_token isa Lexer.Eof
            return tokens_read - 1
        end
        # Then look ahead
        next_token = tokens[tokens_read+1]
        if (
            next_token isa Lexer.Class ||
            next_token isa Lexer.Fun ||
            next_token isa Lexer.Var ||
            next_token isa Lexer.For ||
            next_token isa Lexer.If ||
            next_token isa Lexer.While ||
            next_token isa Lexer.Print ||
            next_token isa Lexer.Return)
            break
        end
        tokens_read += 1
    end
    return tokens_read
end

function var_declaration(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxDeclaration}
    # consume the 'var' token
    tokens[tokens_read+1] isa Lexer.Var || error("var_declaration called without var token")
    tokens_read += 1
    # get identifier
    if tokens[tokens_read+1] isa Lexer.Identifier
        identifier_name = tokens[tokens_read+1].lexeme
        tokens_read += 1
        # check for initialisation value
        if tokens[tokens_read+1] isa Lexer.Equal
            tokens_read += 1
            tokens_read, init_expr =
                expression(tokens_read, tokens, start_loc, parse_errors)
            if tokens[tokens_read+1] isa Lexer.Semicolon
                tokens_read += 1
                return tokens_read, LoxVarDeclaration(identifier_name, init_expr)
            else
                throw(
                    LoxParseError(start_loc, "Expected ';' after variable initialisation"),
                )
            end
        else
            return tokens_read, LoxVarDeclaration(identifier_name, nothing)
        end
    else
        e = LoxParseError(start_loc, "Expected identifier after 'var'")
        push!(parse_errors, e)
        throw(e)
    end
end

function declaration(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxDeclaration}
    return if tokens[tokens_read+1] isa Lexer.Var
        var_declaration(tokens_read, tokens, start_loc, parse_errors)
    else
        statement(tokens_read, tokens, start_loc, parse_errors)
    end
end

function statement(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxStatement}
    # Handle block statements
    if tokens[tokens_read+1] isa Lexer.LeftBrace
        tokens_read += 1
        decls = LoxDeclaration[]
        while !(tokens[tokens_read+1] isa Lexer.RightBrace)
            tokens_read, decl = declaration(tokens_read, tokens, start_loc, parse_errors)
            push!(decls, decl)
        end
        if tokens[tokens_read+1] isa Lexer.RightBrace
            tokens_read += 1
        end
        return tokens_read, LoxBlockStatement(decls)
    end
    # Everything else: print statement, or a simple expression statement
    if tokens[tokens_read+1] isa Lexer.Print
        tokens_read += 1
        is_print_statement = true
    else
        is_print_statement = false
    end
    tokens_read, expr = expression(tokens_read, tokens, start_loc, parse_errors)
    if tokens[tokens_read+1] isa Lexer.Semicolon
        tokens_read += 1
        if is_print_statement
            return tokens_read, LoxPrintStatement(expr)
        else
            return tokens_read, LoxExprStatement(expr)
        end
    else
        throw(LoxParseError(start_loc, "Expected ';' after expression"))
    end
end

function programme(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    parse_errors::Vector{LoxParseError},
)::Tuple{Int,LoxProgramme}
    decls = LoxDeclaration[]
    while true
        if tokens[tokens_read+1] isa Lexer.Eof
            tokens_read += 1
            break
        else
            try
                tokens_read, decl =
                    declaration(tokens_read, tokens, start_loc, parse_errors)
                push!(decls, decl)
            catch e
                if e isa LoxParseError
                    tokens_read = synchronise(tokens_read, tokens)
                    if tokens[tokens_read+1] isa Lexer.Eof
                        # Attempted to synchronise, but reached EOF, i.e. there
                        # wasn't a meaningful place to resume parsing from
                        push!(parse_errors, e)
                    else
                        continue
                    end
                else
                    rethrow(e)
                end
            end
        end
    end
    return tokens_read, LoxProgramme(decls)
end

function parse(
    tokens::Vector{Lexer.Token},
    start_loc::Location,
)::Tuple{LoxProgramme,Vector{LoxParseError}}
    parse_errors = LoxParseError[]
    tokens_read, prog = programme(0, tokens, start_loc, parse_errors)
    if tokens_read < length(tokens)
        throw(LoxParseError(start_loc, "Extra tokens: " * string(tokens[tokens_read:end])))
    end
    return prog, parse_errors
end

end # module
