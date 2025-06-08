module Parser

using ..Errors: LoxError, Location
using ..Lexer: Lexer

export parse, to_sexp

abstract type LoxExpr end

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

## Printing
to_sexp(expr::LoxLiteral{Float64}) = string(expr.value)
to_sexp(expr::LoxLiteral{Bool}) = expr.value ? "true" : "false"
to_sexp(expr::LoxLiteral{String}) = "\"" * expr.value * "\""
to_sexp(expr::LoxLiteral{Nothing}) = "nil"
to_sexp(expr::LoxGrouping) = "(group " * to_sexp(expr.expression) * ")"
to_sexp(expr::LoxUnary) = "(" * to_sexp_op(expr.operator) * " " * to_sexp(expr.right) * ")"
to_sexp(expr::LoxBinary) =
    "(" * to_sexp_op(expr.operator) * " " * to_sexp(expr.left) * " " * to_sexp(expr.right) * ")"
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

function expression(tokens_read::Int, tokens::Vector{Lexer.Token}, start_loc::Location)::Tuple{Int,LoxExpr}
    return equality(tokens_read, tokens, start_loc)
end

function peek_next(tokens_read::Int, tokens::Vector{Lexer.Token})::Union{Nothing,Lexer.Token}
    if tokens_read >= length(tokens)
        return nothing
    else
        return tokens[tokens_read + 1]
    end
end

function left_associative_binary(
    tokens_read::Int,
    tokens::Vector{Lexer.Token},
    start_loc::Location,
    operand_parser::Function,
    operator_mapping::Dict{<:Lexer.Token,<:LoxBinaryOp},
)::Tuple{Int,LoxExpr}
    tokens_read, left_expr = operand_parser(tokens_read, tokens, start_loc)
    next_token = peek_next(tokens_read, tokens)
    while haskey(operator_mapping, next_token)
        # consume the operator
        tokens_read += 1
        operator = operator_mapping[next_token]
        tokens_read, right_expr = operand_parser(tokens_read, tokens, start_loc)
        left_expr = LoxBinary(operator, left_expr, right_expr)
        next_token = peek_next(tokens_read, tokens)
    end
    return tokens_read, left_expr
end

function equality(tokens_read::Int, tokens::Vector{Lexer.Token}, start_loc::Location)::Tuple{Int,LoxExpr}
    operator_mapping = Dict(
        Lexer.EqualEqual() => EqualEqual(),
        Lexer.BangEqual() => BangEqual(),
    )
    return left_associative_binary(tokens_read, tokens, start_loc, comparison, operator_mapping)
end

function comparison(tokens_read::Int, tokens::Vector{Lexer.Token}, start_loc::Location)::Tuple{Int,LoxExpr}
    operator_mapping = Dict(
        Lexer.Less() => LessThan(),
        Lexer.LessEqual() => LessThanEqual(),
        Lexer.Greater() => GreaterThan(),
        Lexer.GreaterEqual() => GreaterThanEqual(),
    )
    return left_associative_binary(tokens_read, tokens, start_loc, term, operator_mapping)
end

function term(tokens_read::Int, tokens::Vector{Lexer.Token}, start_loc::Location)::Tuple{Int,LoxExpr}
    operator_mapping = Dict(
        Lexer.Plus() => Add(),
        Lexer.Minus() => Subtract()
    )
    return left_associative_binary(tokens_read, tokens, start_loc, factor, operator_mapping)
end

function factor(tokens_read::Int, tokens::Vector{Lexer.Token}, start_loc::Location)::Tuple{Int,LoxExpr}
    operator_mapping = Dict(
        Lexer.Star() => Multiply(),
        Lexer.Slash() => Divide()
    )
    return left_associative_binary(tokens_read, tokens, start_loc, unary, operator_mapping)
end

function unary(tokens_read::Int, tokens::Vector{Lexer.Token}, start_loc::Location)::Tuple{Int,LoxExpr}
    next_token = peek_next(tokens_read, tokens)
    if next_token isa Lexer.Bang || next_token isa Lexer.Minus
        tokens_read += 1
        operator = if next_token isa Lexer.Bang
            Bang()
        elseif next_token isa Lexer.Minus
            MinusUnary()
        else
            error("wut")
        end
        tokens_read, right_expr = unary(tokens_read, tokens, start_loc)
        return tokens_read, LoxUnary(operator, right_expr)
    else
        return primary(tokens_read, tokens, start_loc)
    end
end

function primary(tokens_read::Int, tokens::Vector{Lexer.Token}, start_loc::Location)::Tuple{Int,LoxExpr}
    next_token = peek_next(tokens_read, tokens)
    if next_token isa Lexer.False
        return tokens_read + 1, LoxLiteral(false)
    elseif next_token isa Lexer.True
        return tokens_read + 1, LoxLiteral(true)
    elseif next_token isa Lexer.Nil
        return tokens_read + 1, LoxLiteral(nothing)
    elseif next_token isa Lexer.LoxNumber || next_token isa Lexer.LoxString
        return tokens_read + 1, LoxLiteral(next_token.value)
    elseif next_token isa Lexer.LeftParen
        tokens_read += 1
        tokens_read, expr = expression(tokens_read, tokens, start_loc)
        peek_next(tokens_read, tokens) isa Lexer.RightParen ||
            throw(LoxError(start_loc, "Expected ')' after expression"))
        tokens_read += 1 # consume the right paren
        return tokens_read, LoxGrouping(expr)
    end
end

function parse(tokens::Vector{Lexer.Token}, start_loc::Location)::LoxExpr
    tokens_read, expr = expression(0, tokens, start_loc)
    if tokens_read < length(tokens)
        throw(LoxError(start_loc, "Extra tokens: " * string(tokens[tokens_read:end])))
    end
    return expr
end

end # module
