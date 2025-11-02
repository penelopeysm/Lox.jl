module Parser

using ..Errors: Errors, LoxError
using ..Lexer: Lexer

export parse, to_sexp

struct LoxParseError <: Errors.LoxError
    offset::Int
    message::String
end
# TODO: fix end offset
Errors.get_offset(err::LoxParseError) = (err.offset, err.offset + 1)
Errors.get_message(err::LoxParseError) = err.message

### AST types

abstract type LoxExpr end

# In theory, we _could_ use these type parameters for static type checking
# purposes...
struct LoxLiteral{T<:Union{Float64,Bool,String,Nothing}} <: LoxExpr
    value::T
    start_offset::Int
    end_offset::Int
    LoxLiteral(lstr::Lexer.LocatedToken{Lexer.LoxString}) = new{String}(
        lstr.token.value,
        lstr.start_offset,
        lstr.end_offset,
    )
    LoxLiteral(lnum::Lexer.LocatedToken{Lexer.LoxNumber}) = new{Float64}(
        Base.parse(Float64, lnum.token.value),
        lnum.start_offset,
        lnum.end_offset,
    )
    LoxLiteral(ltrue::Lexer.LocatedToken{Lexer.True}) = new{Bool}(true, ltrue.start_offset, ltrue.end_offset)
    LoxLiteral(lfalse::Lexer.LocatedToken{Lexer.False}) = new{Bool}(false, lfalse.start_offset, lfalse.end_offset)
    LoxLiteral(lnil::Lexer.LocatedToken{Lexer.Nil}) = new{Nothing}(nothing, lnil.start_offset, lnil.end_offset)
end
start_offset(l::LoxLiteral) = l.start_offset
end_offset(l::LoxLiteral) = l.end_offset

struct LoxVariable <: LoxExpr
    identifier::String
    start_offset::Int
    end_offset::Int
    LoxVariable(liden::Lexer.LocatedToken{Lexer.Identifier}) = new(
        liden.token.lexeme,
        liden.start_offset,
        liden.end_offset
    )
end
start_offset(v::LoxVariable) = v.start_offset
end_offset(v::LoxVariable) = v.end_offset

abstract type LoxDeclaration end
struct LoxVarDeclaration{Tex<:Union{LoxExpr,Nothing}} <: LoxDeclaration
    # this is the variable itself
    variable::LoxVariable
    # the start offset of the 'var' keyword
    var_start_offset::Int
    initial_expr::Tex
end
start_offset(decl::LoxVarDeclaration) = decl.var_start_offset
end_offset(decl::LoxVarDeclaration{Nothing}) = end_offset(decl.variable)
end_offset(decl::LoxVarDeclaration{Tex}) where {Tex<:LoxExpr} = end_offset(decl.initial_expr)

abstract type LoxStatement <: LoxDeclaration end
struct LoxExprStatement{Tex<:LoxExpr} <: LoxStatement
    expression::Tex
    # TODO: end offset must include semicolon
end
start_offset(stmt::LoxExprStatement) = start_offset(stmt.expression)
end_offset(stmt::LoxExprStatement) = end_offset(stmt.expression)

struct LoxPrintStatement{Tex<:LoxExpr} <: LoxStatement
    expression::Tex
    # TODO: start offset must include print
    # TODO: end offset must include semicolon
end

struct LoxBlockStatement <: LoxStatement
    statements::Vector{LoxDeclaration}
    # TODO: start and end offsets with braces
end

struct LoxProgramme
    statements::Vector{LoxDeclaration}
end
Base.:(==)(prg1::LoxProgramme, prg2::LoxProgramme) = prg1.statements == prg2.statements

# Indeed, we're very quickly reaching the point where we want GADTs...
abstract type LoxBinaryOp end
# NOTE: This requires that all subtypes of LoxBinaryOp have a field `ltoken`
start_offset(b::LoxBinaryOp) = start_offset(b.ltoken)
end_offset(b::LoxBinaryOp) = end_offset(b.ltoken)

abstract type LoxBinaryEqualityOp <: LoxBinaryOp end
struct EqualEqual <: LoxBinaryEqualityOp
    ltoken::Lexer.LocatedToken{Lexer.EqualEqual}
end
struct BangEqual <: LoxBinaryEqualityOp
    ltoken::Lexer.LocatedToken{Lexer.BangEqual}
end

abstract type LoxBinaryComparisonOp <: LoxBinaryOp end
struct LessThan <: LoxBinaryComparisonOp
    ltoken::Lexer.LocatedToken{Lexer.Less}
end
struct LessThanEqual <: LoxBinaryComparisonOp
    ltoken::Lexer.LocatedToken{Lexer.LessEqual}
end
struct GreaterThan <: LoxBinaryComparisonOp
    ltoken::Lexer.LocatedToken{Lexer.Greater}
end
struct GreaterThanEqual <: LoxBinaryComparisonOp
    ltoken::Lexer.LocatedToken{Lexer.GreaterEqual}
end

abstract type LoxBinaryMulDivOp <: LoxBinaryOp end
struct Multiply <: LoxBinaryMulDivOp
    ltoken::Lexer.LocatedToken{Lexer.Star}
end
struct Divide <: LoxBinaryMulDivOp
    ltoken::Lexer.LocatedToken{Lexer.Slash}
end

abstract type LoxBinaryAddSubOp <: LoxBinaryOp end
struct Add <: LoxBinaryAddSubOp
    ltoken::Lexer.LocatedToken{Lexer.Plus}
end
struct Subtract <: LoxBinaryAddSubOp
    ltoken::Lexer.LocatedToken{Lexer.Minus}
end

struct LoxBinary{Top<:LoxBinaryOp,Tex1<:LoxExpr,Tex2<:LoxExpr} <: LoxExpr
    operator::Top
    left::Tex1
    right::Tex2
end
start_offset(b::LoxBinary) = start_offset(b.left)
end_offset(b::LoxBinary) = end_offset(b.right)

abstract type LoxUnaryOp end
# NOTE: This requires that all subtypes of LoxUnaryOp have a field `ltoken`
start_offset(u::LoxUnaryOp) = start_offset(u.ltoken)
end_offset(u::LoxUnaryOp) = end_offset(u.ltoken)

struct Bang <: LoxUnaryOp
    ltoken::Lexer.LocatedToken{Lexer.Bang}
end
struct MinusUnary <: LoxUnaryOp
    ltoken::Lexer.LocatedToken{Lexer.Minus}
end
struct LoxUnary{Top<:LoxUnaryOp,Tex<:LoxExpr} <: LoxExpr
    operator::Top
    right::Tex
end
start_offset(u::LoxUnary) = start_offset(u.operator)
end_offset(u::LoxUnary) = end_offset(u.right)

struct LoxGrouping{Tex<:LoxExpr} <: LoxExpr
    expression::Tex
    start_offset::Int
    end_offset::Int
end
start_offset(g::LoxGrouping) = g.start_offset
end_offset(g::LoxGrouping) = g.end_offset

# Note that assignments are expressions, not statements
struct LoxAssignment{Tex<:LoxExpr} <: LoxExpr
    # TODO: Not sure how to broaden this for e.g. field access, I suppose it comes later in
    # the book
    target_variable::LoxVariable
    value_expression::Tex
end
start_offset(a::LoxAssignment) = start_offset(a.target_variable)
end_offset(a::LoxAssignment) = end_offset(a.value_expression)

### Pretty-printing parser outputs

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
to_sexp(expr::LoxAssignment) =
    "(= " * to_sexp(expr.target_variable) * " " * to_sexp(expr.value_expression) * ")"
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
to_sexp(var_decl::LoxVarDeclaration{Nothing}) = "(decl " * to_sexp(var_decl.variable) * ")"
to_sexp(var_decl::LoxVarDeclaration{Tex}) where {Tex} =
    "(decl " * to_sexp(var_decl.variable) * " = " * to_sexp(var_decl.initial_expr) * ")"
to_sexp(stmt::LoxBlockStatement) =
    "(block " * join(map(to_sexp, stmt.statements), " ") * ")"
to_sexp(prg::LoxProgramme) = join(map(to_sexp, prg.statements), "\n")

### The parser state

mutable struct ParserState
    tokens_read::Int
    tokens::Vector{Lexer.LocatedToken}
    parse_errors::Vector{LoxParseError}
end

function peek_next(s::ParserState)::Lexer.Token
    return s.tokens[s.tokens_read+1].token
end
function peek_next_located(s::ParserState)::Lexer.LocatedToken
    return s.tokens[s.tokens_read+1]
end
function get_next_offset(s::ParserState)::Int
    return s.tokens[s.tokens_read+1].start_offset
end
function consume_next!(s::ParserState)::Nothing
    s.tokens_read += 1
    return nothing
end
function add_error!(s::ParserState, e::LoxParseError)::Nothing
    push!(s.parse_errors, e)
    return nothing
end

### The actual parsing

expression!(s::ParserState)::LoxExpr = assignment!(s)

function assignment!(s::ParserState)::LoxExpr
    # need to cache the current location for error reporting below
    current_offset = get_next_offset(s)
    expr = equality!(s)
    if peek_next(s) isa Lexer.Equal
        # check if `expr` is an l-value
        if expr isa LoxVariable
            consume_next!(s)
            value_expr = assignment!(s)
            return LoxAssignment(expr, value_expr)
        else
            throw(LoxParseError(current_offset, "Invalid assignment target"))
        end
    end
    return expr
end

function left_associative_binary!(
    s::ParserState,
    operand_parser!::Function,
    operator_mapping::Dict{<:Lexer.Token},
)::LoxExpr
    left_expr = operand_parser!(s)
    next_ltoken = peek_next_located(s)
    while haskey(operator_mapping, next_ltoken.token)
        # consume the operator
        consume_next!(s)
        operator_constructor = operator_mapping[next_ltoken.token]
        right_expr = operand_parser!(s)
        left_expr = LoxBinary(operator_constructor(next_ltoken), left_expr, right_expr)
        next_ltoken = peek_next_located(s)
    end
    return left_expr
end

function equality!(s::ParserState)::LoxExpr
    operator_mapping =
        Dict(Lexer.EqualEqual() => EqualEqual, Lexer.BangEqual() => BangEqual)
    return left_associative_binary!(s, comparison!, operator_mapping)
end

function comparison!(s::ParserState)::LoxExpr
    operator_mapping = Dict(
        Lexer.Less() => LessThan,
        Lexer.LessEqual() => LessThanEqual,
        Lexer.Greater() => GreaterThan,
        Lexer.GreaterEqual() => GreaterThanEqual,
    )
    return left_associative_binary!(s, term!, operator_mapping)
end

function term!(s::ParserState)::LoxExpr
    operator_mapping = Dict(Lexer.Plus() => Add, Lexer.Minus() => Subtract)
    return left_associative_binary!(s, factor!, operator_mapping)
end

function factor!(s::ParserState)::LoxExpr
    operator_mapping = Dict(Lexer.Star() => Multiply, Lexer.Slash() => Divide)
    return left_associative_binary!(s, unary!, operator_mapping)
end

function unary!(s::ParserState)::LoxExpr
    next_token = peek_next(s)
    if next_token isa Lexer.Bang
        consume_next!(s)
        right_expr = unary!(s)
        return LoxUnary(Bang(next_token), right_expr)
    elseif next_token isa Lexer.Minus
        consume_next!(s)
        right_expr = unary!(s)
        return LoxUnary(MinusUnary(next_token), right_expr)
    else
        return primary!(s)
    end
end

function primary!(s::ParserState)::LoxExpr
    next_ltoken = peek_next_located(s)
    next_token = next_ltoken.token
    if (next_token isa Lexer.False || next_token isa Lexer.True || next_token isa Lexer.Nil || next_token isa Lexer.LoxNumber || next_token isa Lexer.LoxString)
        consume_next!(s)
        return LoxLiteral(next_ltoken)
    elseif next_token isa Lexer.Identifier
        consume_next!(s)
        return LoxVariable(next_ltoken)
    elseif next_token isa Lexer.LeftParen
        leftparen_start_offset = next_ltoken.start_offset
        consume_next!(s)
        expr = expression!(s)
        next_ltoken = peek_next_located(s)
        if next_ltoken.token isa Lexer.RightParen
            rightparen_end_offset = next_ltoken.end_offset
            consume_next!(s)
            return LoxGrouping(expr, leftparen_start_offset, rightparen_end_offset)
        else
            throw(LoxParseError(next_ltoken.end_offset, "Expected ')' after expression"))
        end
    else
        # parse failure
        throw(LoxParseError(next_ltoken.end_offset, "Parse error: " * string(next_token)))
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
    next_ltoken = peek_next_located(s)
    next_ltoken.token isa Lexer.Var || error("var_declaration called without var token")
    var_start_offset = next_ltoken.start_offset
    consume_next!(s)
    # get identifier
    next_ltoken = peek_next_located(s)
    if next_ltoken.token isa Lexer.Identifier
        variable = LoxVariable(next_ltoken)
        consume_next!(s)
        # check for initialisation value
        if peek_next(s) isa Lexer.Equal
            consume_next!(s)
            init_expr = expression!(s)
            if peek_next(s) isa Lexer.Semicolon
                consume_next!(s)
                return LoxVarDeclaration(variable, var_start_offset, init_expr)
            else
                throw(
                    LoxParseError(
                        get_next_offset(s),
                        "Expected ';' after variable initialisation",
                    ),
                )
            end
        else
            return LoxVarDeclaration(variable, var_start_offset, nothing)
        end
    else
        e = LoxParseError(get_next_offset(s), "Expected identifier after 'var'")
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
        throw(LoxParseError(get_next_offset(s), "Expected ';' after expression"))
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
)::Tuple{LoxProgramme,Vector{LoxParseError}}
    parse_errors = LoxParseError[]
    s = ParserState(0, tokens, parse_errors)
    prog = programme!(s)
    if s.tokens_read < length(s.tokens)
        throw(
            LoxParseError(get_next_offset(s), "Extra tokens: " * string(tokens[s.tokens_read:end])),
        )
    end
    return prog, parse_errors
end

end # module
