module Eval

using ..Lexer: Lexer
using ..Errors: Errors, LoxError, Location
using ..Parser: Parser
using ..SemanticAnalysis: SemanticAnalysis

"""
    AbstractLoxValue

An abstract type for anything that can be a value, i.e., bound to a variable, in Lox.
"""
abstract type AbstractLoxValue end

"""
    LoxEnvironment

A mapping of variables to values. Optionally also contains a parent environment, which
allows for inner scopes.
"""
struct LoxEnvironment
    parent_env::Union{LoxEnvironment,Nothing}
    vars::Dict{String,Any}
end

function getvalue(env::LoxEnvironment, var::Parser.LoxVariable)
    if var.env_index == -1
        # could not be statically resolved (for example, if the variable is used
        # in a function definition, but points to a global that's only defined later)
        # so we should traverse until we reach the top of the chain (i.e. global scope)
        # and see if it's there.
        global_scope = env
        while global_scope.parent_env !== nothing
            global_scope = global_scope.parent_env
        end
        if haskey(global_scope.vars, var.identifier)
            return global_scope.vars[var.identifier]
        else
            throw(LoxUndefVarError(var))
        end
        # TODO: Do this better, i.e. separate globals and locals.
    else
        # Use the env_index to find the correct environment
        target_env = env
        for _ in 1:var.env_index
            if target_env.parent_env === nothing
                error("internal error: variable $(var.identifier) has env_index $(var.env_index) but environment chain is too short")
            end
            target_env = target_env.parent_env
        end
        if haskey(target_env.vars, var.identifier)
            return target_env.vars[var.identifier]
        else
            error("internal error: variable $(var.identifier) not found in environment at index $(var.env_index)")
        end
    end
end

function setvalue!(
    env::LoxEnvironment,
    var::Parser.LoxVariable,
    value::AbstractLoxValue,
    is_new_declaration::Bool,
)
    if haskey(env.vars, var.identifier) || is_new_declaration
        # If it's a new variable, we always want to set it in the current scope.
        env.vars[var.identifier] = value
    elseif env.parent_env !== nothing && !is_new_declaration
        # We can only modify parent scopes if it's not a new variable.
        # TODO (stretch): allow modification of variables in parent environments (this
        # requires a new keyword like Python's `global` or `nonlocal`)
        setvalue!(env.parent_env, var, value, false)
    else
        throw(LoxUndefVarError(var))
    end
    return env
end

function _merge_scopes(env::LoxEnvironment)::Dict{String,Any}
    v = env.vars
    if env.parent_env !== nothing
        # child scopes take precedence, so must be the second argument to merge
        return merge(_merge_scopes(env.parent_env), v)
    else
        return v
    end
end

#############
# Lox types #
#############

"""
    lox_repr_type(value::AbstractLoxValue)::String

Get the Julia string representation of the type of a Lox value.
"""
function lox_repr_type end

"""
    lox_show(value::AbstractLoxValue)::String

Get the Julia string representation of a Lox value.
"""
function lox_show end

"""
    lox_truthy(value)::LoxBoolean

Check if a value is "truthy" in Lox. In Lox, `false` and `nil` are falsy, while
everything else is truthy (even 0).
"""
lox_truthy(::AbstractLoxValue) = LoxBoolean(true)

"""
    lox_eq(expr::LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue)::LoxBoolean

Check if two Lox values are equal. In general, all values of different types are not equal.
"""
lox_eq(::Parser.LoxExpr, ::AbstractLoxValue, ::AbstractLoxValue) = LoxBoolean(false)

"""
    lox_bang(expr::LoxExpr, v::AbstractLoxValue)::LoxBoolean

Logical negation.
"""
lox_bang(expr::Parser.LoxExpr, v::AbstractLoxValue) =
    throw(LoxTypeError(expr, "cannot apply '!' operator to value of type $(lox_repr_type(v))"))

"""
    lox_unary_negate(expr::LoxExpr, n::AbstractLoxValue)::LoxNumber

Unary negation for numbers.
"""
lox_unary_negate(expr::Parser.LoxExpr, n::AbstractLoxValue) = throw(LoxTypeError(expr, "expected number for unary negation, but got $(lox_repr_type(n))"))

"""
    lox_neq(expr::LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue)::LoxBoolean

Check if two Lox values are unequal. Inequality is always defined such that (a != b) == !(a == b).
This means that the implementation does not need to create new methods for `lox_neq`; we can just
rely on implementations of `lox_eq`.
"""
lox_neq(expr::Parser.LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue) = lox_bang(expr, lox_eq(expr, left, right))

struct LoxNil <: AbstractLoxValue end
lox_truthy(::LoxNil) = LoxBoolean(false)
lox_eq(::Parser.LoxExpr, ::LoxNil, ::LoxNil) = LoxBoolean(true)
lox_repr_type(::LoxNil) = "nil"
lox_show(::LoxNil) = "nil"

struct LoxNumber <: AbstractLoxValue
    # Internally stored as f64
    value::Float64
end
lox_repr_type(::LoxNumber) = "number"
lox_show(n::LoxNumber) = repr(n.value)
Base.show(io::IO, n::LoxNumber) = print(io, n.value)
lox_unary_negate(::Parser.LoxExpr, n::LoxNumber) = LoxNumber(-n.value)
lox_eq(::Parser.LoxExpr, left::LoxNumber, right::LoxNumber) =
    LoxBoolean(left.value == right.value)
# Mathematical operations
lox_add(::Parser.LoxExpr, left::LoxNumber, right::LoxNumber) =
    LoxNumber(left.value + right.value)
lox_add(expr::Parser.LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue) =
    throw(LoxTypeError(expr, "cannot add values of types $(lox_repr_type(left)) and $(lox_repr_type(right))"))
lox_sub(::Parser.LoxExpr, left::LoxNumber, right::LoxNumber) =
    LoxNumber(left.value - right.value)
lox_sub(expr::Parser.LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue) =
    throw(LoxTypeError(expr, "cannot subtract values of types $(lox_repr_type(left)) and $(lox_repr_type(right))"))
lox_mul(::Parser.LoxExpr, left::LoxNumber, right::LoxNumber) =
    LoxNumber(left.value * right.value)
lox_mul(expr::Parser.LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue) =
    throw(LoxTypeError(expr, "cannot multiply values of types $(lox_repr_type(left)) and $(lox_repr_type(right))"))
function lox_div(expr::Parser.LoxExpr, left::LoxNumber, right::LoxNumber)
    iszero(right.value) && throw(LoxZeroDivisionError(expr))
    return LoxNumber(left.value / right.value)
end
lox_div(expr::Parser.LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue) =
    throw(LoxTypeError(expr, "cannot divide values of types $(lox_repr_type(left)) and $(lox_repr_type(right))"))
lox_lt(::Parser.LoxExpr, left::LoxNumber, right::LoxNumber) =
    LoxBoolean(left.value < right.value)
lox_lt(expr::Parser.LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue) =
    throw(LoxTypeError(expr, "cannot compare values of types $(lox_repr_type(left)) and $(lox_repr_type(right))"))
lox_le(::Parser.LoxExpr, left::LoxNumber, right::LoxNumber) =
    LoxBoolean(left.value <= right.value)
lox_le(expr::Parser.LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue) =
    throw(LoxTypeError(expr, "cannot compare values of types $(lox_repr_type(left)) and $(lox_repr_type(right))"))
lox_gt(::Parser.LoxExpr, left::LoxNumber, right::LoxNumber) =
    LoxBoolean(left.value > right.value)
lox_gt(expr::Parser.LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue) =
    throw(LoxTypeError(expr, "cannot compare values of types $(lox_repr_type(left)) and $(lox_repr_type(right))"))
lox_ge(::Parser.LoxExpr, left::LoxNumber, right::LoxNumber) =
    LoxBoolean(left.value >= right.value)
lox_ge(expr::Parser.LoxExpr, left::AbstractLoxValue, right::AbstractLoxValue) =
    throw(LoxTypeError(expr, "cannot compare values of types $(lox_repr_type(left)) and $(lox_repr_type(right))"))

struct LoxBoolean <: AbstractLoxValue
    value::Bool
end
lox_repr_type(::LoxBoolean) = "boolean"
lox_show(b::LoxBoolean) = repr(b.value)
lox_truthy(b::LoxBoolean) = b
lox_bang(::Parser.LoxExpr, b::LoxBoolean) = LoxBoolean(!b.value)
lox_eq(::Parser.LoxExpr, left::LoxBoolean, right::LoxBoolean) =
    LoxBoolean(left.value == right.value)

struct LoxString <: AbstractLoxValue
    value::String
end
lox_repr_type(::LoxString) = "string"
lox_show(s::LoxString) = s.value
lox_eq(::Parser.LoxExpr, left::LoxString, right::LoxString) =
    LoxBoolean(left.value == right.value)
lox_add(::Parser.LoxExpr, left::LoxString, right::LoxString) =
    LoxString(left.value * right.value)

"""
    LoxList

A heterogeneous, immutable list. Equality is defined by contents, not identity.

Lists can be added (like in Python), which concatenates them.
"""
struct LoxList <: AbstractLoxValue
    elements::Vector{AbstractLoxValue}
end
lox_repr_type(::LoxList) = "list"
lox_show(l::LoxList) = "list<" * join(map(lox_show, l.elements), ",") * ">"
function lox_eq(::Parser.LoxExpr, left::LoxList, right::LoxList)
    return if length(left.elements) != length(right.elements)
        LoxBoolean(false)
    else
        for (lelem, relem) in zip(left.elements, right.elements)
            if lelem != relem
                return LoxBoolean(false)
            end
        end
        return LoxBoolean(true)
    end
end
lox_add(::Parser.LoxExpr, left::LoxList, right::LoxList) =
    LoxList(vcat(left.elements, right.elements))

abstract type AbstractMethod end

# NOTE: LoxMethod is NOT an AbstractLoxValue
struct LoxMethod{T<:Union{Parser.LoxVariable,Nothing}} <: AbstractMethod
    # nothing if it's an anonymous function
    name::T
    parameters::Vector{Parser.LoxVariable}
    body::Parser.LoxBlockStatement
    # Capture the environment where the function was defined
    env::LoxEnvironment

    LoxMethod(decl::Parser.LoxFunDeclaration, env::LoxEnvironment) =
        new{typeof(decl.name)}(decl.name, decl.parameters, decl.body, env)
    LoxMethod(expr::Parser.LoxFunExpr, env::LoxEnvironment) =
        new{Nothing}(nothing, expr.parameters, expr.body, env)
end

struct NativeMethod{F} <: AbstractMethod
    julia_fun::F
end

struct LoxImport <: AbstractMethod end

struct Arity{T<:Union{Int,Nothing}}
    min::Int
    max::T
end
satisfies_arity(a::Arity{Nothing}, nargs::Int) = nargs >= a.min
satisfies_arity(a::Arity{Int}, nargs::Int) = nargs >= a.min && nargs <= a.max
show_arity(a::Arity{Nothing}) = "$(a.min)+"
function show_arity(a::Arity{Int})
    return if a.min == a.max
        "$(a.min)"
    else
        "$(a.min)–$(a.max)"
    end
end

struct MethodTable <: AbstractLoxValue
    func_name::String
    methods::Dict{Arity,AbstractMethod} # keys are arity
end
lox_repr_type(::MethodTable) = "methodtable"
lox_show(mt::MethodTable) = "<Lox method table for $(mt.func_name) with arities [$(join(map(show_arity, collect(keys(mt.methods))), ", "))]>"
function define_method!(
    env::LoxEnvironment,
    var::Parser.LoxVariable,
    value::LoxMethod,
)
    # This defines a new function. Right now, in plain Lox syntax, there is no way to define
    # a variadic function. Thus all Lox-defined functions the arity is a single number.
    fname = var.identifier
    nargs = length(value.parameters)
    arity = Arity(nargs, nargs)
    if haskey(env.vars, fname)
        mt = env.vars[fname]
        if mt isa MethodTable
            # Add to existing method table
            if haskey(mt.methods, arity)
                printstyled("Lox: redefining method for $(fname) with arity $(show_arity(arity))\n"; color=:yellow)
            end
            mt.methods[arity] = value
            return env
        else
            throw(
                LoxTypeError(
                    var,
                    "$(fname) already defined as a non-function value",
                ),
            )
        end
    else
        env.vars[fname] = MethodTable(fname, Dict(arity => value))
        return env
    end
    # TODO: We don't check parent scopes for existing method tables. This means
    # that methods are strictly local to the scope that they are defined in. I'm
    # not sure if this is OK; we might need to change it later.
end
function define_method!(
    env::LoxEnvironment,
    fname::String,
    arity::Arity,
    value::Union{NativeMethod,LoxImport},
)
    if haskey(env.vars, fname)
        mt = env.vars[fname]
        if mt isa MethodTable
            mt.methods[arity] = value
            return env
        else
            error("unreachable: tried to define native method for non-function value")
        end
    else
        env.vars[fname] = MethodTable(fname, Dict(arity => value))
        return env
    end
end

"""
    resolve_method(call_expr::LoxExpr, mt::MethodTable, nargs::Int)

Determine which method is most appropriate when the given function is called with `nargs`
arguments.
"""
function resolve_method(call_expr::Parser.LoxExpr, mt::MethodTable, nargs::Int)
    # Step 1: If there is a method with that EXACT arity, pick it
    exact_arity = Arity(nargs, nargs)
    haskey(mt.methods, exact_arity) && return mt.methods[exact_arity]
    # Step 2: If not, find all possible methods and see if there is exactly one
    # method that satisfies it
    possible_arities = filter(k -> satisfies_arity(k, nargs), keys(mt.methods))
    if length(possible_arities) == 1
        return mt.methods[only(possible_arities)]
    else
        # Step 3: If there is more than one possibility, just complain that it's ambiguous,
        # because I can't be bothered to define more specific tie-breaking rules
        throw(LoxMethodAmbiguityError(call_expr, mt.func_name, nargs, possible_arities))
    end
end

##################
# Runtime errors #
##################

abstract type LoxEvalError <: LoxError end

# This is NOT a LoxEvalError, because it is caught inside the native method, and thus
# has no access to debug info (offsets). It has to be thrown and then caught inside
# lox_eval, which can then augment it with the debug info before rethrowing it.
struct InsideLoxNativeMethodError <: Exception
    message::String
end
# This is the augmented struct.
struct LoxNativeMethodError <: LoxEvalError
    call_expr::Parser.LoxCall
    message::String
end
Errors.get_offset(err::LoxNativeMethodError) =
    (Parser.start_offset(err.call_expr), Parser.end_offset(err.call_expr))
Errors.get_message(err::LoxNativeMethodError) = err.message

struct LoxZeroDivisionError <: LoxEvalError
    division_expr::Parser.LoxBinary{Parser.Divide}
end
Errors.get_offset(err::LoxZeroDivisionError) =
    (Parser.start_offset(err.division_expr), Parser.end_offset(err.division_expr))
Errors.get_message(::LoxZeroDivisionError) = "division by zero"

struct LoxTypeError{Texpr<:Parser.LoxExpr} <: LoxEvalError
    expr::Texpr
    message::String
end
Errors.get_offset(err::LoxTypeError) =
    (Parser.start_offset(err.expr), Parser.end_offset(err.expr))
Errors.get_message(err::LoxTypeError) = err.message

struct LoxUndefVarError <: LoxEvalError
    # LoxVariable subtypes LoxExpr, so we have location info in it
    variable::Parser.LoxVariable
end
Errors.get_offset(err::LoxUndefVarError) =
    (Parser.start_offset(err.variable), Parser.end_offset(err.variable))
Errors.get_message(err::LoxUndefVarError) =
    "undefined variable: `$(err.variable.identifier)`"

struct LoxUnexpectedReturnError <: LoxEvalError
    return_stmt::Parser.LoxReturnStatement
end
Errors.get_offset(err::LoxUnexpectedReturnError) =
    (Parser.start_offset(err.return_stmt), Parser.end_offset(err.return_stmt))
Errors.get_message(err::LoxUnexpectedReturnError) =
    "unexpected return statement outside of function body"

# this one is also NOT a LoxEvalError
struct LoxReturn{T}
    stmt::Parser.LoxReturnStatement
    value::T
end

struct LoxMethodAmbiguityError{Texpr<:Parser.LoxExpr}
    expr::Texpr
    func_name::String
    nargs::Int
    possible_arities::Vector{Arity}
end
Errors.get_offset(err::LoxMethodAmbiguityError) =
    (Parser.start_offset(err.expr), Parser.end_offset(err.expr))
function Errors.get_message(err::LoxMethodAmbiguityError)
    arities_string = join(map(show_arity, possible_arities), "< ")
    return "invocation of $(func_name) with $(nargs) arguments is ambiguous; methods are defined for arities $(arities_string)"
end

##############
# Evaluation #
##############

"""
    lox_eval(::LoxExpr, ::LoxEnvironment)::AbstractLoxValue

Evaluate an expression.
"""
lox_eval(lit::Parser.LoxLiteral{<:Number}, ::LoxEnvironment) = LoxNumber(lit.value)
lox_eval(lit::Parser.LoxLiteral{<:Bool}, ::LoxEnvironment) = LoxBoolean(lit.value)
lox_eval(lit::Parser.LoxLiteral{<:String}, ::LoxEnvironment) = LoxString(lit.value)
lox_eval(::Parser.LoxLiteral{Nothing}, ::LoxEnvironment) = LoxNil()
lox_eval(var::Parser.LoxVariable, env::LoxEnvironment) = getvalue(env, var)
lox_eval(grp::Parser.LoxGrouping, env::LoxEnvironment) = lox_eval(grp.expression, env)
function lox_eval(expr::Parser.LoxFunExpr, env::LoxEnvironment)
    f = LoxMethod(expr, env)
    nargs = length(expr.parameters)
    arity = Arity(nargs, nargs)
    return MethodTable("<anonymous>", Dict(arity => f))
end
function lox_eval(expr::Parser.LoxUnary{Parser.Bang}, env::LoxEnvironment)
    return lox_bang(expr, lox_truthy(lox_eval(expr.right, env)))
end
function lox_eval(expr::Parser.LoxUnary{Parser.MinusUnary}, env::LoxEnvironment)
    return lox_unary_negate(expr, lox_eval(expr.right, env))
end

function lox_eval(expr::Parser.LoxAssignment, env::LoxEnvironment)
    rvalue = lox_eval(expr.value_expression, env)
    setvalue!(env, expr.target_variable, rvalue, false)
    return rvalue
end

function lox_eval(expr::Parser.LoxBinary{Parser.Or}, env::LoxEnvironment)
    left_val = lox_eval(expr.left, env)
    if lox_truthy(left_val).value
        return left_val
    else
        return lox_eval(expr.right, env)
    end
end
function lox_eval(expr::Parser.LoxBinary{Parser.And}, env::LoxEnvironment)
    left_val = lox_eval(expr.left, env)
    if lox_bang(expr, lox_truthy(left_val)).value
        return left_val
    else
        return lox_eval(expr.right, env)
    end
end
lox_eval(expr::Parser.LoxBinary{Parser.EqualEqual}, env::LoxEnvironment) =
    lox_eq(expr, lox_eval(expr.left, env), lox_eval(expr.right, env))
lox_eval(expr::Parser.LoxBinary{Parser.BangEqual}, env::LoxEnvironment) =
    lox_neq(expr, lox_eval(expr.left, env), lox_eval(expr.right, env))
lox_eval(expr::Parser.LoxBinary{Parser.LessThan}, env::LoxEnvironment) =
    lox_lt(expr, lox_eval(expr.left, env), lox_eval(expr.right, env))
lox_eval(expr::Parser.LoxBinary{Parser.LessThanEqual}, env::LoxEnvironment) =
    lox_le(expr, lox_eval(expr.left, env), lox_eval(expr.right, env))
lox_eval(expr::Parser.LoxBinary{Parser.GreaterThan}, env::LoxEnvironment) =
    lox_gt(expr, lox_eval(expr.left, env), lox_eval(expr.right, env))
lox_eval(expr::Parser.LoxBinary{Parser.GreaterThanEqual}, env::LoxEnvironment) =
    lox_ge(expr, lox_eval(expr.left, env), lox_eval(expr.right, env))
lox_eval(expr::Parser.LoxBinary{Parser.Subtract}, env::LoxEnvironment) =
    lox_sub(expr, lox_eval(expr.left, env), lox_eval(expr.right, env))
lox_eval(expr::Parser.LoxBinary{Parser.Multiply}, env::LoxEnvironment) =
    lox_mul(expr, lox_eval(expr.left, env), lox_eval(expr.right, env))
function lox_eval(expr::Parser.LoxBinary{Parser.Divide}, env::LoxEnvironment)
    # Have to evaluate them first to handle order of side effects
    left = lox_eval(expr.left, env)
    right = lox_eval(expr.right, env)
    return lox_div(expr, left, right)
end
lox_eval(expr::Parser.LoxBinary{Parser.Add}, env::LoxEnvironment) =
    lox_add(expr, lox_eval(expr.left, env), lox_eval(expr.right, env))
function _lox_invoke(::Parser.LoxExpr, ::LoxEnvironment, func::LoxMethod, arg_values::Vector{<:AbstractLoxValue})
    # Regenerate the function's original environment (but create an inner
    # one for the function call itself).
    new_env = LoxEnvironment(func.env, Dict{String,Any}())
    for (param, arg_value) in zip(func.parameters, arg_values)
        setvalue!(new_env, param, arg_value, true)
    end
    # Then execute the function
    try
        lox_exec(func.body, new_env)
    catch e
        if e isa LoxReturn
            return e.value
        else
            rethrow()
        end
    end
end
function _lox_invoke(expr::Parser.LoxExpr, ::LoxEnvironment, func::NativeMethod, arg_values::Vector{<:AbstractLoxValue})
    try
        return func.julia_fun(arg_values...)
    catch e
        # This is the only type of exception that should be thrown inside a
        # native method
        if e isa InsideLoxNativeMethodError
            throw(LoxNativeMethodError(expr, e.message))
        else
            # shouldn't happen; if this does it's a bug in the implementation
            rethrow()
        end
    end
end
function _lox_invoke(expr::Parser.LoxExpr, env::LoxEnvironment, func::LoxImport, arg_values::Vector{<:AbstractLoxValue})
    length(arg_values) == 1 || throw(
        LoxTypeError(
            expr,
            "import function expects exactly 1 argument, got $(length(arg_values))",
        ),
    )
    fname_val = only(arg_values)
    fname_val isa LoxString || throw(
        LoxTypeError(
            expr,
            "import function expects a string filename, got value of type $(lox_repr_type(fname_val))",
        ),
    )
    fname = fname_val.value
    # Read the file
    file_contents = read(fname, String)
    # Lex, parse, and execute it in the environment
    tokens, lex_errors = Lexer.lex(file_contents)
    if !isempty(lex_errors)
        throw(LoxImportError(expr, "lexing failed on file $(fname)"))
    end
    prg, parse_errors = Parser.parse(tokens)
    if !isempty(parse_errors)
        throw(LoxImportError(expr, "parsing failed on file $(fname)"))
    end
    SemanticAnalysis.resolve_variables!(prg)
    lox_exec(prg, env)
    return LoxNil()
end
function lox_eval(expr::Parser.LoxCall, env::LoxEnvironment)
    callee = lox_eval(expr.callee, env)
    nargs = length(expr.arguments)
    if callee isa MethodTable
        func = resolve_method(expr, callee, nargs)
            arg_values = map(Base.Fix2(lox_eval, env), expr.arguments)
            # the above infers as Vector{Any}, so we have to convert :(
            arg_values = convert(Vector{AbstractLoxValue}, arg_values)
            _lox_invoke(expr, env, func, arg_values)
    else
        throw(
            LoxTypeError(
                expr.callee,
                "attempted to call a non-function value of type $(lox_repr_type(callee))",
            ),
        )
    end
end

"""
    lox_exec(stmt_or_prg, env::LoxEnvironment)::LoxEnvironment

Execute a statement or programme in Lox. Returns the updated environment.
"""
function lox_exec(stmt::Parser.LoxVarDeclaration{Nothing}, env::LoxEnvironment)
    setvalue!(env, stmt.variable, LoxNil, true)
    return env
end
function lox_exec(stmt::Parser.LoxVarDeclaration{<:Parser.LoxExpr}, env::LoxEnvironment)
    initial_value = lox_eval(stmt.initial_expr, env)
    setvalue!(env, stmt.variable, initial_value, true)
    return env
end
function lox_exec(decl::Parser.LoxFunDeclaration, env::LoxEnvironment)
    define_method!(env, decl.name, LoxMethod(decl, env))
    return env
end
function lox_exec(stmt::Parser.LoxExprStatement, env::LoxEnvironment)
    lox_eval(stmt.expression, env)
    return env
end
function lox_exec(stmt::Parser.LoxReturnStatement, env::LoxEnvironment)
    val = lox_eval(stmt.expression, env)
    throw(LoxReturn(stmt, val))
end
function lox_exec(stmt::Parser.LoxPrintStatement, env::LoxEnvironment)
    value = lox_eval(stmt.expression, env)
    println(lox_show(value))
    return env
end
function lox_exec(stmt::Parser.LoxIfStatement, env::LoxEnvironment)
    condition = lox_eval(stmt.condition, env)
    if lox_truthy(condition).value
        lox_exec(stmt.then_branch, env)
    else
        if stmt.else_branch !== nothing
            lox_exec(stmt.else_branch, env)
        end
    end
    return env
end
function lox_exec(stmt::Parser.LoxWhileStatement, env::LoxEnvironment)
    while lox_truthy(lox_eval(stmt.condition, env)).value
        lox_exec(stmt.body, env)
    end
    return env
end
function lox_exec(stmt::Parser.LoxBlockStatement, env::LoxEnvironment)
    # Generate a new child environment
    new_env = LoxEnvironment(env, Dict{String,Any}())
    foreach(stmt.statements) do child_stmt
        lox_exec(child_stmt, new_env)
    end
    return env
end

function setup_global_environment()
    # In this function we can set up our native methods.
    env = LoxEnvironment(nothing, Dict{String,Any}())

    # clock
    define_method!(env, "clock", Arity(0, 0), NativeMethod(() -> LoxNumber(time())))

    # readfile
    function lox_readfile(fname::LoxString)
        try
            return LoxString(read(fname.value, String))
        catch e
            throw(InsideLoxNativeMethodError("could not read from file $(fname.value)"))
        end
    end
    define_method!(env, "readfile", Arity(1, 1), NativeMethod(lox_readfile))

    # vec
    lox_vec(args...) = LoxList(collect(args))
    define_method!(env, "vec", Arity(0, nothing), NativeMethod(lox_vec))

    # length
    function lox_length(lst::LoxList)
        return LoxNumber(length(lst.elements))
    end
    function lox_length(x::AbstractLoxValue)
        # TODO: we could technically define length for strings too, instead of defining it
        # in the prelude
        throw(InsideLoxNativeMethodError("`length(s)` not defined for value `s` of type $(lox_repr_type(x))"))
    end
    define_method!(env, "length", Arity(1, 1), NativeMethod(lox_length))

    # at
    function lox_at(lst::LoxList, index::LoxNumber)
        idx = Int(round(index.value))
        if idx < 0 || idx >= length(lst.elements)
            throw(InsideLoxNativeMethodError("index $(idx) out of bounds for list of length $(length(lst.elements))"))
        end
        return lst.elements[idx + 1] # YES, FREEDOM FROM JULIA'S 1-INDEXING
    end
    function lox_at(x::AbstractLoxValue, ::LoxNumber)
        throw(InsideLoxNativeMethodError("`at(s, i)` not defined for value `s` of type $(lox_repr_type(x))"))
    end
    define_method!(env, "at", Arity(2, 2), NativeMethod(lox_at))

    # chars
    function lox_chars(s::LoxString)
        return LoxList([LoxString(string(c)) for c in collect(s.value)])
    end
    function lox_chars(x::AbstractLoxValue)
        throw(InsideLoxNativeMethodError("`chars(s)` not defined for value `s` of type $(lox_repr_type(x))"))
    end
    define_method!(env, "chars", Arity(1, 1), NativeMethod(lox_chars))

    # to_number
    function lox_to_number(s::LoxString)
        try
            n = parse(Float64, s.value)
            return LoxNumber(n)
        catch e
            throw(InsideLoxNativeMethodError("could not convert string '$(s.value)' to number"))
        end
    end
    define_method!(env, "to_number", Arity(1, 1), NativeMethod(lox_to_number))

    # import
    define_method!(env, "import", Arity(1, 1), LoxImport())

    return env
end

function lox_exec(
    prg::Parser.LoxProgramme,
    env::LoxEnvironment=setup_global_environment()
)
    # annotate LoxVariables with their environment indices
    SemanticAnalysis.resolve_variables!(prg)
    for stmt in prg.statements
        lox_exec(stmt, env)
    end
    return env
end

end # module Eval
