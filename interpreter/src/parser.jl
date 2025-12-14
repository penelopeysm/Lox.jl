module Parser

using ..Errors: Errors, LoxError
using ..Lexer: Lexer

export parse, to_sexp

struct LoxParseError <: Errors.LoxError
    offset::Int
    message::String
end
# Since parse errors are only associated with a single location, we can safely
# make the 'end offset' be just the character after the start offset
Errors.get_offset(err::LoxParseError) = (err.offset, err.offset + 1)
Errors.get_message(err::LoxParseError) = err.message

### AST types

abstract type LoxExprOrDecl end

abstract type LoxExpr <: LoxExprOrDecl end
abstract type LoxDeclaration <: LoxExprOrDecl end

abstract type LoxStatement <: LoxDeclaration end

struct LoxProgramme
    statements::Vector{LoxDeclaration}
end
Base.:(==)(prg1::LoxProgramme, prg2::LoxProgramme) = prg1.statements == prg2.statements
children(prg::LoxProgramme) = prg.statements

##### Exprs

# In theory, we _could_ use these type parameters for static type checking
# purposes...
struct LoxLiteral{T<:Union{Float64,Bool,String,Nothing}} <: LoxExpr
    value::T
    start_offset::Int
    end_offset::Int
    LoxLiteral(lstr::Lexer.LocatedToken{Lexer.LoxString}) =
        new{String}(lstr.token.value, lstr.start_offset, lstr.end_offset)
    LoxLiteral(lnum::Lexer.LocatedToken{Lexer.LoxNumber}) = new{Float64}(
        Base.parse(Float64, lnum.token.value),
        lnum.start_offset,
        lnum.end_offset,
    )
    LoxLiteral(ltrue::Lexer.LocatedToken{Lexer.True}) =
        new{Bool}(true, ltrue.start_offset, ltrue.end_offset)
    LoxLiteral(lfalse::Lexer.LocatedToken{Lexer.False}) =
        new{Bool}(false, lfalse.start_offset, lfalse.end_offset)
    LoxLiteral(lnil::Lexer.LocatedToken{Lexer.Nil}) =
        new{Nothing}(nothing, lnil.start_offset, lnil.end_offset)
end
start_offset(l::LoxLiteral) = l.start_offset
end_offset(l::LoxLiteral) = l.end_offset
children(::LoxLiteral) = LoxExprOrDecl[]

mutable struct LoxVariable <: LoxExpr
    const identifier::String
    const start_offset::Int
    const end_offset::Int
    env_index::Int # -1 for unresolved; otherwise indicates the number of scopes out
    LoxVariable(liden::Lexer.LocatedToken{Lexer.Identifier}) =
        new(liden.token.lexeme, liden.start_offset, liden.end_offset, -1)
    LoxVariable(lv::LoxVariable, new_name::String) =
        new(new_name, lv.start_offset, lv.end_offset, -1)
end
start_offset(v::LoxVariable) = v.start_offset
end_offset(v::LoxVariable) = v.end_offset
children(::LoxVariable) = LoxExprOrDecl[]

##### Decls

struct LoxVarDeclaration{Tex<:Union{LoxExpr,Nothing}} <: LoxDeclaration
    # this is the variable itself
    variable::LoxVariable
    # the start offset of the 'var' keyword
    var_start_offset::Int
    initial_expr::Tex
end
start_offset(decl::LoxVarDeclaration) = decl.var_start_offset
end_offset(decl::LoxVarDeclaration{Nothing}) = end_offset(decl.variable)
end_offset(decl::LoxVarDeclaration{Tex}) where {Tex<:LoxExpr} =
    end_offset(decl.initial_expr)
children(decl::LoxVarDeclaration{Nothing}) = [decl.variable]
children(decl::LoxVarDeclaration{Tex}) where {Tex<:LoxExpr} =
    [decl.variable, decl.initial_expr]

##### Statements

struct LoxExprStatement{Tex<:LoxExpr} <: LoxStatement
    expression::Tex
    end_offset::Int # this refers to end offset of the semicolon
end
start_offset(stmt::LoxExprStatement) = start_offset(stmt.expression)
end_offset(stmt::LoxExprStatement) = stmt.end_offset
children(stmt::LoxExprStatement) = [stmt.expression]

struct LoxPrintStatement{Tex<:LoxExpr} <: LoxStatement
    expression::Tex
    start_offset::Int
    end_offset::Int
end
start_offset(stmt::LoxPrintStatement) = stmt.start_offset
end_offset(stmt::LoxPrintStatement) = stmt.end_offset
children(stmt::LoxPrintStatement) = [stmt.expression]

struct LoxReturnStatement{Tex<:LoxExpr} <: LoxStatement
    expression::Tex
    start_offset::Int
    end_offset::Int
end
start_offset(stmt::LoxReturnStatement) = stmt.start_offset
end_offset(stmt::LoxReturnStatement) = stmt.end_offset
children(stmt::LoxReturnStatement) = [stmt.expression]

struct LoxIfStatement{
    Tcond<:LoxExpr,
    Tthen<:LoxStatement,
    Telse<:Union{LoxStatement,Nothing},
} <: LoxStatement
    condition::Tcond
    then_branch::Tthen
    else_branch::Telse
    if_start_offset::Int
end
start_offset(stmt::LoxIfStatement) = stmt.if_start_offset
end_offset(stmt::LoxIfStatement) =
    stmt.else_branch === nothing ? end_offset(stmt.then_branch) :
    end_offset(stmt.else_branch)
function children(stmt::LoxIfStatement)
    c = [stmt.condition, stmt.then_branch]
    stmt.else_branch !== nothing && push!(c, stmt.else_branch)
    return c
end

struct LoxWhileStatement{Tcond<:LoxExpr,Tbody<:LoxStatement} <: LoxStatement
    condition::Tcond
    body::Tbody
    while_start_offset::Int
end
start_offset(stmt::LoxWhileStatement) = stmt.while_start_offset
end_offset(stmt::LoxWhileStatement) = end_offset(stmt.body)
children(stmt::LoxWhileStatement) = [stmt.condition, stmt.body]

struct LoxBlockStatement <: LoxStatement
    statements::Vector{LoxDeclaration}
    start_offset::Int
    end_offset::Int
end
start_offset(stmt::LoxBlockStatement) = stmt.start_offset
end_offset(stmt::LoxBlockStatement) = stmt.end_offset
children(stmt::LoxBlockStatement) = stmt.statements

struct LoxFunDeclaration <: LoxDeclaration
    name::LoxVariable
    parameters::Vector{LoxVariable}
    fun_start_offset::Int
    body::LoxBlockStatement
end
start_offset(decl::LoxFunDeclaration) = decl.fun_start_offset
end_offset(decl::LoxFunDeclaration) = end_offset(decl.body)
children(decl::LoxFunDeclaration) = [decl.name, decl.parameters, decl.body]

#### More exprs

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

abstract type LoxLogicalOp <: LoxBinaryOp end
struct And <: LoxLogicalOp
    ltoken::Lexer.LocatedToken{Lexer.And}
end
struct Or <: LoxLogicalOp
    ltoken::Lexer.LocatedToken{Lexer.Or}
end

struct LoxBinary{Top<:LoxBinaryOp,Tex1<:LoxExpr,Tex2<:LoxExpr} <: LoxExpr
    operator::Top
    left::Tex1
    right::Tex2
end
start_offset(b::LoxBinary) = start_offset(b.left)
end_offset(b::LoxBinary) = end_offset(b.right)
children(b::LoxBinary) = [b.left, b.right]

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
children(u::LoxUnary) = [u.right]

struct LoxCall{Tex<:LoxExpr} <: LoxExpr
    callee::Tex
    arguments::Vector{LoxExpr}
    right_paren_end_offset::Int
end
start_offset(c::LoxCall) = start_offset(c.callee)
end_offset(c::LoxCall) = c.right_paren_end_offset
children(c::LoxCall) = [c.callee, c.arguments...]

struct LoxGrouping{Tex<:LoxExpr} <: LoxExpr
    expression::Tex
    start_offset::Int
    end_offset::Int
end
start_offset(g::LoxGrouping) = g.start_offset
end_offset(g::LoxGrouping) = g.end_offset
children(g::LoxGrouping) = [g.expression]

# Note that assignments are expressions, not statements
struct LoxAssignment{Tex<:LoxExpr} <: LoxExpr
    # TODO: Not sure how to broaden this for e.g. field access, I suppose it comes later in
    # the book
    target_variable::LoxVariable
    value_expression::Tex
end
start_offset(a::LoxAssignment) = start_offset(a.target_variable)
end_offset(a::LoxAssignment) = end_offset(a.value_expression)
children(a::LoxAssignment) = [a.target_variable, a.value_expression]

### Pretty-printing parser outputs

to_sexp(expr::LoxLiteral{Float64}) = string(expr.value)
to_sexp(expr::LoxLiteral{Bool}) = expr.value ? "true" : "false"
to_sexp(expr::LoxLiteral{String}) = "\"" * expr.value * "\""
to_sexp(expr::LoxLiteral{Nothing}) = "nil"
to_sexp(var::LoxVariable) = ("var'" * var.identifier * "'")
to_sexp(expr::LoxGrouping) = "(group " * to_sexp(expr.expression) * ")"
to_sexp(expr::LoxUnary) = "(" * to_sexp_op(expr.operator) * " " * to_sexp(expr.right) * ")"
to_sexp(expr::LoxCall) =
    "(call " *
    to_sexp(expr.callee) *
    (length(expr.arguments) == 0 ? "" : " " * join(map(to_sexp, expr.arguments), " ")) *
    ")"
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
to_sexp_op(op::And) = "and"
to_sexp_op(op::Or) = "or"
to_sexp(stmt::LoxPrintStatement) = "(print " * to_sexp(stmt.expression) * ")"
to_sexp(stmt::LoxReturnStatement) = "(return " * to_sexp(stmt.expression) * ")"
to_sexp(stmt::LoxStatement) = "(stmt " * to_sexp(stmt.expression) * ")"
to_sexp(var_decl::LoxVarDeclaration{Nothing}) = "(decl " * to_sexp(var_decl.variable) * ")"
to_sexp(var_decl::LoxVarDeclaration{Tex}) where {Tex} =
    "(decl " * to_sexp(var_decl.variable) * " = " * to_sexp(var_decl.initial_expr) * ")"
to_sexp(stmt::LoxBlockStatement) =
    "(block " * join(map(to_sexp, stmt.statements), " ") * ")"
to_sexp(stmt::LoxIfStatement) =
    "(if " *
    to_sexp(stmt.condition) *
    " " *
    to_sexp(stmt.then_branch) *
    (stmt.else_branch === nothing ? "" : " " * to_sexp(stmt.else_branch)) *
    ")"
to_sexp(fun_decl::LoxFunDeclaration) =
    "(fun " *
    to_sexp(fun_decl.name) *
    " (" *
    (length(fun_decl.parameters) == 0
     ? ""
     : join(map(to_sexp, fun_decl.parameters), " ")) *
    ") " *
    to_sexp(fun_decl.body) *
    ")"
to_sexp(stmt::LoxWhileStatement) =
    "(while " * to_sexp(stmt.condition) * " " * to_sexp(stmt.body) * ")"
to_sexp(prg::LoxProgramme) = join(map(to_sexp, prg.statements), "\n")

### The parser state

mutable struct ParserState
    tokens_read::Int
    tokens::Vector{Lexer.LocatedToken}
    parse_errors::Vector{LoxParseError}
end

function peek_next_unlocated(s::ParserState)::Lexer.Token
    return s.tokens[s.tokens_read+1].token
end
function peek_next(s::ParserState)::Lexer.LocatedToken
    return s.tokens[s.tokens_read+1]
end
function get_next_offset(s::ParserState)::Int
    return s.tokens[s.tokens_read+1].start_offset
end
function consume_next!(s::ParserState)::Lexer.LocatedToken
    s.tokens_read += 1
    return s.tokens[s.tokens_read]
end
function add_error!(s::ParserState, e::LoxParseError)::Nothing
    push!(s.parse_errors, e)
    return nothing
end
function consume_or_error!(
    s::ParserState,
    expected_token::Type{<:Lexer.Token},
    errmsg::String,
)::Lexer.LocatedToken
    next_ltoken = peek_next(s)
    if next_ltoken.token isa expected_token
        return consume_next!(s)
    else
        throw(LoxParseError(next_ltoken.end_offset, errmsg))
    end
end

### The actual parsing

expression!(s::ParserState)::LoxExpr = assignment!(s)

function assignment!(s::ParserState)::LoxExpr
    # need to cache the current location for error reporting below
    current_offset = get_next_offset(s)
    expr = or!(s)
    if peek_next_unlocated(s) isa Lexer.Equal
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
    next_ltoken = peek_next(s)
    while haskey(operator_mapping, next_ltoken.token)
        # consume the operator
        consume_next!(s)
        operator_constructor = operator_mapping[next_ltoken.token]
        right_expr = operand_parser!(s)
        left_expr = LoxBinary(operator_constructor(next_ltoken), left_expr, right_expr)
        next_ltoken = peek_next(s)
    end
    return left_expr
end

function or!(s::ParserState)::LoxExpr
    operator_mapping = Dict(Lexer.Or() => Or)
    return left_associative_binary!(s, and!, operator_mapping)
end

function and!(s::ParserState)::LoxExpr
    operator_mapping = Dict(Lexer.And() => And)
    return left_associative_binary!(s, equality!, operator_mapping)
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
    next_ltoken = peek_next(s)
    if next_ltoken.token isa Lexer.Bang
        consume_next!(s)
        right_expr = unary!(s)
        return LoxUnary(Bang(next_ltoken), right_expr)
    elseif next_ltoken.token isa Lexer.Minus
        consume_next!(s)
        right_expr = unary!(s)
        return LoxUnary(MinusUnary(next_ltoken), right_expr)
    else
        return call!(nothing, s)
    end
end

function call!(::Nothing, s::ParserState)::LoxExpr
    callee_or_primary = primary!(s)
    if peek_next_unlocated(s) isa Lexer.LeftParen
        args, rparen_end_offset = _call_args!(s)
        return call!(LoxCall(callee_or_primary, args, rparen_end_offset), s)
    else
        return callee_or_primary
    end
end
function call!(callee::LoxExpr, s::ParserState)::LoxExpr
    if peek_next_unlocated(s) isa Lexer.LeftParen
        args, rparen_end_offset = _call_args!(s)
        return LoxCall(callee, args, rparen_end_offset)
    else
        return callee
    end
end

function _call_args!(s::ParserState)::Tuple{Vector{LoxExpr},Int}
    consume_or_error!(
        s,
        Lexer.LeftParen,
        "unreachable: _call_args! called without left paren",
    )
    args = LoxExpr[]
    next_token = peek_next(s)
    # check for empty argument list
    if next_token.token isa Lexer.RightParen
        consume_next!(s) # right paren
        return args, next_token.end_offset
    end
    # otherwise, there's one or more arguments
    while true
        # Technically, Lox's spec says up to 255 arguments, but we won't enforce that here.
        # That could be trivially added here by checking length(args).
        push!(args, expression!(s))
        if peek_next_unlocated(s) isa Lexer.Comma
            consume_next!(s) # comma
            continue
        else
            rparen = consume_or_error!(s, Lexer.RightParen, "expected ')' after function call arguments")
            return args, rparen.end_offset
        end
    end
end

function primary!(s::ParserState)::LoxExpr
    next_ltoken = peek_next(s)
    next_token = next_ltoken.token
    if (
        next_token isa Lexer.False ||
        next_token isa Lexer.True ||
        next_token isa Lexer.Nil ||
        next_token isa Lexer.LoxNumber ||
        next_token isa Lexer.LoxString
    )
        consume_next!(s)
        return LoxLiteral(next_ltoken)
    elseif next_token isa Lexer.Identifier
        consume_next!(s)
        return LoxVariable(next_ltoken)
    elseif next_token isa Lexer.LeftParen
        leftparen_start_offset = next_ltoken.start_offset
        consume_next!(s)
        expr = expression!(s)
        rparen_ltoken =
            consume_or_error!(s, Lexer.RightParen, "Expected ')' after expression")
        rightparen_end_offset = rparen_ltoken.end_offset
        return LoxGrouping(expr, leftparen_start_offset, rightparen_end_offset)
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
        if peek_next_unlocated(s) isa Lexer.Semicolon
            consume_next!(s)
            return
        end
        if peek_next_unlocated(s) isa Lexer.Eof
            return
        end
        # Then look ahead
        consume_next!(s)
        next_token = peek_next_unlocated(s)
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
    var_ltoken = consume_or_error!(s, Lexer.Var, "unreachable: var_declaration! called without var token")
    var_start_offset = var_ltoken.start_offset
    # get identifier
    next_ltoken = consume_or_error!(s, Lexer.Identifier, "Expected identifier after 'var'")
    variable = LoxVariable(next_ltoken)
    # check for initialisation value
    if peek_next_unlocated(s) isa Lexer.Equal
        consume_next!(s) # equals sign
        init_expr = expression!(s)
        consume_or_error!(s, Lexer.Semicolon, "Expected ';' after variable initialisation")
        return LoxVarDeclaration(variable, var_start_offset, init_expr)
    else
        return LoxVarDeclaration(variable, var_start_offset, nothing)
    end
end

function fun_declaration!(s::ParserState)::LoxFunDeclaration
    # consume the 'var' token
    fun_ltoken = consume_or_error!(s, Lexer.Fun, "unreachable: fun_declaration! called without fun token")
    fun_start_offset = fun_ltoken.start_offset
    # get function name
    next_ltoken = consume_or_error!(s, Lexer.Identifier, "Expected function name after 'fun'")
    function_name = LoxVariable(next_ltoken)
    # parameters
    consume_or_error!(s, Lexer.LeftParen, "expected '(' after function name")
    params = LoxVariable[]
    # handle empty parameter list
    if peek_next_unlocated(s) isa Lexer.RightParen
        consume_next!(s) # right paren
    else
        while true
            param_ltoken = consume_or_error!(s, Lexer.Identifier, "expected parameter name")
            push!(params, LoxVariable(param_ltoken))
            if peek_next_unlocated(s) isa Lexer.Comma
                consume_next!(s) # comma
            else
                consume_or_error!(s, Lexer.RightParen, "expected ')' after function parameters")
                break
            end
        end
    end
    # function body
    block = block_statement!(s)
    return LoxFunDeclaration(function_name, params, fun_start_offset, block)
end

function declaration!(s::ParserState)::LoxDeclaration
    next_token = peek_next_unlocated(s)
    return if next_token isa Lexer.Var
        var_declaration!(s)
    elseif next_token isa Lexer.Fun
        fun_declaration!(s)
    else
        statement!(s)
    end
end

function block_statement!(s::ParserState)::LoxBlockStatement
    next_ltoken = peek_next(s)
    next_ltoken.token isa Lexer.LeftBrace ||
        error("unreachable: block_statement! called without left brace")
    leftbrace_start_offset = next_ltoken.start_offset
    consume_next!(s)
    decls = LoxDeclaration[]
    while !(peek_next_unlocated(s) isa Lexer.RightBrace)
        decl = declaration!(s)
        push!(decls, decl)
    end
    rightbrace_ltoken =
        consume_or_error!(s, Lexer.RightBrace, "expected '}' after block statements")
    rightbrace_end_offset = rightbrace_ltoken.end_offset
    return LoxBlockStatement(decls, leftbrace_start_offset, rightbrace_end_offset)
end

function if_statement!(s::ParserState)::LoxIfStatement
    next_ltoken =
        consume_or_error!(s, Lexer.If, "unreachable: if_statement! called without if token")
    if_start_offset = next_ltoken.start_offset
    # check for open paren
    consume_or_error!(s, Lexer.LeftParen, "expected '(' after 'if'")
    # condition
    condition_expr = expression!(s)
    # check for close paren
    consume_or_error!(s, Lexer.RightParen, "expected ')' after if condition")
    # then branch
    then_stmt = statement!(s)
    # check for else branch (which is optional)
    else_stmt = begin
        next_token = peek_next_unlocated(s)
        if next_token isa Lexer.Else
            consume_next!(s) # 'else'
            statement!(s)
        else
            nothing
        end
    end
    return LoxIfStatement(condition_expr, then_stmt, else_stmt, if_start_offset)
end

function while_statement!(s::ParserState)::LoxWhileStatement
    while_ltoken = consume_or_error!(
        s,
        Lexer.While,
        "unreachable: while_statement! called without while token",
    )
    while_start_offset = while_ltoken.start_offset
    # check for open paren
    consume_or_error!(s, Lexer.LeftParen, "expected '(' after 'for'")
    # condition
    condition_expr = expression!(s)
    # check for close paren
    consume_or_error!(s, Lexer.RightParen, "expected ')' after while condition")
    # body
    body_stmt = statement!(s)
    return LoxWhileStatement(condition_expr, body_stmt, while_start_offset)
end

function for_statement!(s::ParserState)::LoxBlockStatement
    # We will actually desugar the for loop into a while loop here -- for example:
    #
    #   for (init; cond; incr) body;
    #
    # becomes
    #
    #   {
    #     init;
    #     while (cond) {
    #        body;
    #        incr;
    #     }
    #   }
    #
    # (note that the braces outside are necessary since the initialiser may declare
    # variables that should not leak into the surrounding scope)
    for_ltoken = consume_or_error!(
        s,
        Lexer.For,
        "unreachable: for_statement! called without for token",
    )
    for_start_offset = for_ltoken.start_offset
    consume_or_error!(s, Lexer.LeftParen, "expected '(' after 'for'")
    # initialiser: it can either be empty, a var declaration, or an expression.
    # Note that this block will also consume the semicolon that comes after it
    initialiser = begin
        next_token = peek_next_unlocated(s)
        if next_token isa Lexer.Semicolon
            consume_next!(s) # consume the semicolon
            nothing
        elseif next_token isa Lexer.Var
            var_declaration!(s)
        else
            expr = expression!(s)
            semicolon_ltoken = consume_or_error!(
                s,
                Lexer.Semicolon,
                "expected ';' after for loop initialiser",
            )
            LoxExprStatement(expr, semicolon_ltoken.end_offset)
        end
    end
    # condition
    condition = begin
        if peek_next_unlocated(s) isa Lexer.Semicolon
            # no condition means always true -- but the start and end offsets
            # are just the semicolon itself
            LoxLiteral(Lexer.LocatedToken(Lexer.False, get_next_offset(s), get_next_offset(s)))
        else
            expression!(s)
        end
    end
    consume_or_error!(s, Lexer.Semicolon, "expected ';' after for loop condition")
    # increment
    increment = begin
        if peek_next_unlocated(s) isa Lexer.RightParen
            nothing
        else
            expression!(s)
        end
    end
    # consume the closing right paren
    consume_or_error!(s, Lexer.RightParen, "expected ')' after for loop clauses")
    # then the body
    body_stmt = statement!(s)
    # now we emit the desugared while loop
    while_stmt = LoxWhileStatement(
        condition,
        begin
            if increment === nothing
                body_stmt
            else
                # I _think_ it makes more sense to make the block statement's start offset
                # the increment, because even though the desugared code has the increment
                # after the body, in the original source code the increment comes before the
                # body.
                LoxBlockStatement(
                    [body_stmt, LoxExprStatement(increment, end_offset(increment))],
                    start_offset(increment),
                    end_offset(body_stmt),
                )
            end
        end,
        for_start_offset,
    )
    if initialiser === nothing
        return while_stmt
    else
        return LoxBlockStatement(
            [initialiser, while_stmt],
            start_offset(initialiser),
            end_offset(while_stmt),
        )
    end
end

function expr_statement!(s::ParserState)::LoxExprStatement
    expr = expression!(s)
    semicolon = consume_or_error!(s, Lexer.Semicolon, "Expected ';' after expression")
    return LoxExprStatement(expr, semicolon.end_offset)
end

function print_statement!(s::ParserState)::LoxPrintStatement
    print_ltoken = consume_or_error!(s, Lexer.Print, "unreachable: print_statement! called without print token")
    expr_statement = expr_statement!(s)
    return LoxPrintStatement(expr_statement.expression, print_ltoken.start_offset, expr_statement.end_offset)
end

function return_statement!(s::ParserState)::LoxReturnStatement
    return_ltoken = consume_or_error!(s, Lexer.Return, "unreachable: return_statement! called without return token")
    expr_statement = expr_statement!(s)
    return LoxReturnStatement(expr_statement.expression, return_ltoken.start_offset, expr_statement.end_offset)
end

function statement!(s::ParserState)::LoxStatement
    next_token = peek_next_unlocated(s)
    return if next_token isa Lexer.LeftBrace
        block_statement!(s)
    elseif next_token isa Lexer.If
        if_statement!(s)
    elseif next_token isa Lexer.For
        for_statement!(s)
    elseif next_token isa Lexer.While
        while_statement!(s)
    elseif next_token isa Lexer.Print
        print_statement!(s)
    elseif next_token isa Lexer.Return
        return_statement!(s)
    else
        expr_statement!(s)
    end
end

function programme!(s::ParserState)::LoxProgramme
    decls = LoxDeclaration[]
    while true
        if peek_next_unlocated(s) isa Lexer.Eof
            consume_next!(s)
            break
        else
            try
                decl = declaration!(s)
                push!(decls, decl)
            catch e
                if e isa LoxParseError
                    synchronise!(s)
                    if peek_next_unlocated(s) isa Lexer.Eof
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
            LoxParseError(
                get_next_offset(s),
                "Extra tokens: " * string(tokens[s.tokens_read:end]),
            ),
        )
    end
    return prog, parse_errors
end

"""
    LoxInterpreter.Parser.parse(s::AbstractString)

Convenience function to perform lexing and parsing in one step.
"""
function parse(s::AbstractString)
    tokens, lex_errors = Lexer.lex(s)
    if !isempty(lex_errors)
        foreach(lex_errors) do e
            showerror(stderr, e)
            println(stderr)
        end
        error("failed to lex")
    end
    prog, parse_errors = parse(tokens)
    if !isempty(parse_errors)
        foreach(parse_errors) do e
            showerror(stderr, e)
            println(stderr)
        end
        error("failed to parse")
    end
    return prog
end

end # module
