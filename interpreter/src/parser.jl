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
to_sexp(stmt::LoxBlockStatement) =
    "(block " * join(map(to_sexp, stmt.statements), " ") * ")"
to_sexp(prg::LoxProgramme) = join(map(to_sexp, prg.statements), "\n")

mutable struct ParserState
    tokens_read::Int
    tokens::Vector{Lexer.LocatedToken}
    # TODO: remove start_loc
    start_loc::Location
    parse_errors::Vector{LoxParseError}
end

function peek_next(s::ParserState)::Lexer.Token
    return s.tokens[s.tokens_read+1].token
end
function consume_next!(s::ParserState)::Nothing
    s.tokens_read += 1
    return nothing
end
function add_error!(s::ParserState, e::LoxParseError)::Nothing
    push!(s.parse_errors, e)
    return nothing
end

expression!(s::ParserState)::LoxExpr = equality!(s)

function left_associative_binary!(
    s::ParserState,
    operand_parser!::Function,
    operator_mapping::Dict{<:Lexer.Token,<:LoxBinaryOp},
)::LoxExpr
    left_expr = operand_parser!(s)
    next_token = peek_next(s)
    while haskey(operator_mapping, next_token)
        # consume the operator
        consume_next!(s)
        operator = operator_mapping[next_token]
        right_expr = operand_parser!(s)
        left_expr = LoxBinary(operator, left_expr, right_expr)
        next_token = peek_next(s)
    end
    return left_expr
end

function equality!(s::ParserState)::LoxExpr
    operator_mapping =
        Dict(Lexer.EqualEqual() => EqualEqual(), Lexer.BangEqual() => BangEqual())
    return left_associative_binary!(s, comparison!, operator_mapping)
end

function comparison!(s::ParserState)::LoxExpr
    operator_mapping = Dict(
        Lexer.Less() => LessThan(),
        Lexer.LessEqual() => LessThanEqual(),
        Lexer.Greater() => GreaterThan(),
        Lexer.GreaterEqual() => GreaterThanEqual(),
    )
    return left_associative_binary!(s, term!, operator_mapping)
end

function term!(s::ParserState)::LoxExpr
    operator_mapping = Dict(Lexer.Plus() => Add(), Lexer.Minus() => Subtract())
    return left_associative_binary!(s, factor!, operator_mapping)
end

function factor!(s::ParserState)::LoxExpr
    operator_mapping = Dict(Lexer.Star() => Multiply(), Lexer.Slash() => Divide())
    return left_associative_binary!(s, unary!, operator_mapping)
end

function unary!(s::ParserState)::LoxExpr
    next_token = peek_next(s)
    if next_token isa Lexer.Bang || next_token isa Lexer.Minus
        consume_next!(s)
        operator = if next_token isa Lexer.Bang
            Bang()
        elseif next_token isa Lexer.Minus
            MinusUnary()
        else
            # TODO: refactor to avoid this
            error("wut")
        end
        right_expr = unary!(s)
        return LoxUnary(operator, right_expr)
    else
        return primary!(s)
    end
end

function primary!(s::ParserState)::LoxExpr
    next_token = peek_next(s)
    if next_token isa Lexer.False
        consume_next!(s)
        return LoxLiteral(false)
    elseif next_token isa Lexer.True
        consume_next!(s)
        return LoxLiteral(true)
    elseif next_token isa Lexer.Nil
        consume_next!(s)
        return LoxLiteral(nothing)
    elseif next_token isa Lexer.LoxNumber || next_token isa Lexer.LoxString
        consume_next!(s)
        return LoxLiteral(next_token.value)
    elseif next_token isa Lexer.Identifier
        consume_next!(s)
        return LoxVariable(next_token.lexeme)
    elseif next_token isa Lexer.LeftParen
        consume_next!(s)
        expr = expression!(s)
        if peek_next(s) isa Lexer.RightParen
            consume_next!(s)
            return LoxGrouping(expr)
        else
            throw(LoxParseError(s.start_loc, "Expected ')' after expression"))
        end
    else
        # parse failure
        throw(LoxParseError(s.start_loc, "Parse error: " * string(next_token)))
    end
end

"""
Consume tokens until we reach somewhere we can resume parsing from.

In particular, this function mutates `s` such that the _next_ token (i.e. `peek_next(s)`) is
the first token that parsing could conceivably begin from.

If there are no such tokens, then this function mutates `s` such that `peek_next(s)` is the
EOF token.
"""
function synchronise!(s::ParserState)
    while true
        if peek_next(s) isa Lexer.Semicolon
            consume_next!(s)
            return
        end
        if peek_next(s) isa Lexer.Eof
            return
        end
        # Then look ahead
        consume_next!(s)
        next_token = peek_next(s)
        if (
            next_token isa Lexer.Class ||
            next_token isa Lexer.Fun ||
            next_token isa Lexer.Var ||
            next_token isa Lexer.For ||
            next_token isa Lexer.If ||
            next_token isa Lexer.While ||
            next_token isa Lexer.Print ||
            next_token isa Lexer.Return
        )
            break
        end
    end
end

function var_declaration!(s::ParserState)::LoxVarDeclaration
    # consume the 'var' token
    peek_next(s) isa Lexer.Var || error("var_declaration called without var token")
    consume_next!(s)
    # get identifier
    next_token = peek_next(s)
    if next_token isa Lexer.Identifier
        identifier_name = next_token.lexeme
        consume_next!(s)
        # check for initialisation value
        if peek_next(s) isa Lexer.Equal
            consume_next!(s)
            init_expr = expression!(s)
            if peek_next(s) isa Lexer.Semicolon
                consume_next!(s)
                return LoxVarDeclaration(identifier_name, init_expr)
            else
                throw(
                    LoxParseError(
                        s.start_loc,
                        "Expected ';' after variable initialisation",
                    ),
                )
            end
        else
            return LoxVarDeclaration(identifier_name, nothing)
        end
    else
        e = LoxParseError(s.start_loc, "Expected identifier after 'var'")
        add_error!(s, e)
        # TODO: Do we need this?
        throw(e)
    end
end

function declaration!(s::ParserState)::LoxDeclaration
    return if peek_next(s) isa Lexer.Var
        var_declaration!(s)
    else
        statement!(s)
    end
end

function statement!(s::ParserState)::LoxStatement
    # Handle block statements
    if peek_next(s) isa Lexer.LeftBrace
        consume_next!(s)
        decls = LoxDeclaration[]
        while !(peek_next(s) isa Lexer.RightBrace)
            decl = declaration!(s)
            push!(decls, decl)
        end
        if peek_next(s) isa Lexer.RightBrace
            consume_next!(s)
        end
        return LoxBlockStatement(decls)
    end
    # Everything else: print statement, or a simple expression statement
    if peek_next(s) isa Lexer.Print
        consume_next!(s)
        is_print_statement = true
    else
        is_print_statement = false
    end
    expr = expression!(s)
    if peek_next(s) isa Lexer.Semicolon
        consume_next!(s)
        return if is_print_statement
            LoxPrintStatement(expr)
        else
            LoxExprStatement(expr)
        end
    else
        throw(LoxParseError(s.start_loc, "Expected ';' after expression"))
    end
end

function programme!(s::ParserState)::LoxProgramme
    decls = LoxDeclaration[]
    while true
        if peek_next(s) isa Lexer.Eof
            consume_next!(s)
            break
        else
            try
                decl = declaration!(s)
                push!(decls, decl)
            catch e
                if e isa LoxParseError
                    synchronise!(s)
                    if peek_next(s) isa Lexer.Eof
                        # Attempted to synchronise, but reached EOF, i.e. there
                        # wasn't a meaningful place to resume parsing from
                        add_error!(s, e)
                    else
                        continue
                    end
                else
                    rethrow(e)
                end
            end
        end
    end
    return LoxProgramme(decls)
end

function parse(
    tokens::Vector{Lexer.LocatedToken},
    start_loc::Location,
)::Tuple{LoxProgramme,Vector{LoxParseError}}
    parse_errors = LoxParseError[]
    s = ParserState(0, tokens, start_loc, parse_errors)
    prog = programme!(s)
    if s.tokens_read < length(s.tokens)
        throw(
            LoxParseError(start_loc, "Extra tokens: " * string(tokens[s.tokens_read:end])),
        )
    end
    return prog, parse_errors
end

end # module
