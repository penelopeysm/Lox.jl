module Eval

using ..Errors: Errors, LoxError, Location
using ..Parser: Parser

struct LoxEnvironment
    parent_env::Union{LoxEnvironment,Nothing}
    vars::Dict{String,Any}
end
LoxEnvironment() = LoxEnvironment(nothing, Dict{String,Any}())
function getvalue(env::LoxEnvironment, var::Parser.LoxVariable)
    if var.identifier == "__scope__"
        return _merge_scopes(env)
    elseif haskey(env.vars, var.identifier)
        return env.vars[var.identifier]
    elseif env.parent_env !== nothing
        return getvalue(env.parent_env, var)
    else
        throw(LoxUndefVarError(var))
    end
end
function setvalue!(
    env::LoxEnvironment,
    var::Parser.LoxVariable,
    value::Any,
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


struct LoxNil end

struct LoxFunction
    declaration::Parser.LoxFunDeclaration
    # Capture the environment where the function was defined
    env::LoxEnvironment
end
Base.show(io::IO, f::LoxFunction) = print(io, "<Lox function $(f.declaration.name.identifier)>")

struct MethodTable
    func_name::String
    methods::Dict{Int,LoxFunction} # Mapping from arity to the LoxFunction
end
Base.show(io::IO, mt::MethodTable) = print(io, "<Lox method table with arities: $(collect(keys(mt.methods)))>")
function setvalue!(
    env::LoxEnvironment,
    var::Parser.LoxVariable,
    value::LoxFunction,
    ::Bool,
)
    fname = var.identifier
    arity = length(value.declaration.parameters)
    if haskey(env.vars, var.identifier)
        mt = env.vars[var.identifier]
        if mt isa MethodTable
            # Add to existing method table
            if haskey(mt.methods, arity)
                printstyled("Lox: redefining $arity-arity method for $(fname)\n"; color=:yellow)
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

abstract type LoxEvalError <: LoxError end

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

struct LoxReturn{T} <: LoxEvalError
    stmt::Parser.LoxReturnStatement
    value::T
end

"""
    lox_eval(::LoxExpr, ::LoxEnvironment)

Evaluate an expression. Note that these return Julia types rather than Lox types (so when we
throw mismatched type errors, for example, the error message refers to Julia types...!) This
could definitely be improved. The only exception to this is LoxNil, which is the
representation of Lox's `nil` value.
"""
lox_eval(lit::Parser.LoxLiteral{<:Number}, ::LoxEnvironment) = lit.value
lox_eval(lit::Parser.LoxLiteral{<:Bool}, ::LoxEnvironment) = lit.value
lox_eval(lit::Parser.LoxLiteral{<:String}, ::LoxEnvironment) = lit.value
lox_eval(::Parser.LoxLiteral{Nothing}, ::LoxEnvironment) = LoxNil()
lox_eval(var::Parser.LoxVariable, env::LoxEnvironment) = getvalue(env, var)
lox_eval(grp::Parser.LoxGrouping, env::LoxEnvironment) = lox_eval(grp.expression, env)
function lox_eval(expr::Parser.LoxUnary{Parser.Bang}, env::LoxEnvironment)
    return !(lox_truthy(lox_eval(expr.right, env)))
end
function lox_eval(expr::Parser.LoxUnary{Parser.MinusUnary}, env::LoxEnvironment)
    return -_lox_eval_expect_f64(expr.right, env)
end

function lox_eval(expr::Parser.LoxAssignment, env::LoxEnvironment)
    rvalue = lox_eval(expr.value_expression, env)
    setvalue!(env, expr.target_variable, rvalue, false)
    return rvalue
end

# Lox: == and != are defined on all types
function lox_eval(expr::Parser.LoxBinary{Parser.Or}, env::LoxEnvironment)
    left_val = lox_eval(expr.left, env)
    if lox_truthy(left_val)
        return left_val
    else
        return lox_eval(expr.right, env)
    end
end
function lox_eval(expr::Parser.LoxBinary{Parser.And}, env::LoxEnvironment)
    left_val = lox_eval(expr.left, env)
    if !lox_truthy(left_val)
        return left_val
    else
        return lox_eval(expr.right, env)
    end
end
lox_eval(expr::Parser.LoxBinary{Parser.EqualEqual}, env::LoxEnvironment) =
    lox_eval(expr.left, env) == lox_eval(expr.right, env)
lox_eval(expr::Parser.LoxBinary{Parser.BangEqual}, env::LoxEnvironment) =
    lox_eval(expr.left, env) != lox_eval(expr.right, env)
# Lox: < <= > >= - * / are only defined on numbers
lox_eval(expr::Parser.LoxBinary{Parser.LessThan}, env::LoxEnvironment) =
    _lox_eval_expect_f64(expr.left, env) < _lox_eval_expect_f64(expr.right, env)
lox_eval(expr::Parser.LoxBinary{Parser.LessThanEqual}, env::LoxEnvironment) =
    _lox_eval_expect_f64(expr.left, env) <= _lox_eval_expect_f64(expr.right, env)
lox_eval(expr::Parser.LoxBinary{Parser.GreaterThan}, env::LoxEnvironment) =
    _lox_eval_expect_f64(expr.left, env) > _lox_eval_expect_f64(expr.right, env)
lox_eval(expr::Parser.LoxBinary{Parser.GreaterThanEqual}, env::LoxEnvironment) =
    _lox_eval_expect_f64(expr.left, env) >= _lox_eval_expect_f64(expr.right, env)
lox_eval(expr::Parser.LoxBinary{Parser.Subtract}, env::LoxEnvironment) =
    _lox_eval_expect_f64(expr.left, env) - _lox_eval_expect_f64(expr.right, env)
lox_eval(expr::Parser.LoxBinary{Parser.Multiply}, env::LoxEnvironment) =
    _lox_eval_expect_f64(expr.left, env) * _lox_eval_expect_f64(expr.right, env)
function lox_eval(expr::Parser.LoxBinary{Parser.Divide}, env::LoxEnvironment)
    # Have to evaluate them first to handle order of side effects
    left = _lox_eval_expect_f64(expr.left, env)
    right = _lox_eval_expect_f64(expr.right, env)
    iszero(right) && throw(LoxZeroDivisionError(expr))
    return left / right
end
# Lox: + works on both numbers and strings
function lox_eval(expr::Parser.LoxBinary{Parser.Add}, env::LoxEnvironment)
    left = lox_eval(expr.left, env)
    right = lox_eval(expr.right, env)
    if left isa Float64 && right isa Float64
        return left + right
    elseif left isa String && right isa String
        return left * right
    else
        throw(
            LoxTypeError(
                expr,
                "cannot add values of types $(typeof(left)) and $(typeof(right))",
            ),
        )
    end
end
function lox_eval(expr::Parser.LoxCall, env::LoxEnvironment)
    callee = lox_eval(expr.callee, env)
    nargs = length(expr.arguments)
    if callee isa MethodTable
        if haskey(callee.methods, nargs)
            func = callee.methods[nargs]
            arg_values = map(Base.Fix2(lox_eval, env), expr.arguments)
            # Regenerate the function's original environment (but create an inner
            # one for the function call itself)
            new_env = LoxEnvironment(func.env, Dict{String,Any}())
            for (param, arg_value) in zip(func.declaration.parameters, arg_values)
                setvalue!(new_env, param, arg_value, true)
            end
            # Then execute the function
            try
                lox_exec(func.declaration.body, new_env; allow_return=true)
            catch e
                if e isa LoxReturn
                    return e.value
                else
                    # bug in implementation
                    rethrow()
                end
            end
            # TODO: handle return values.
        else
            throw(
                LoxTypeError(
                    expr.callee,
                    "no $(nargs)-argument method for `$(callee.func_name)`",
                ),
            )
        end
    else
        throw(
            LoxTypeError(
                expr.callee,
                "attempted to call a non-function value of type $(typeof(callee))",
            ),
        )
    end
end


# struct LoxCall{Tex<:LoxExpr} <: LoxExpr
#     callee::Tex
#     arguments::Vector{LoxExpr}
#     right_paren_end_offset::Int
# end

"""
    _lox_eval_expect_f64(expr::LoxExpr, env::LoxEnvironment)

Evaluate an expression and expect it to return a `Float64`. If the expression does not, throw a LoxTypeError.
"""
function _lox_eval_expect_f64(expr::Parser.LoxExpr, env::LoxEnvironment)
    # TODO: This leads to very uninformative error messages. Fix this by checking 
    # the type at a higher level instead.
    val = lox_eval(expr, env)
    val isa Float64 || throw(LoxTypeError(expr, "expected float, got $(typeof(val))"))
    return val
end

"""
    lox_truthy(value)

Check if a value is "truthy" in Lox. In Lox, `false` and `nil` are falsy, while
everything else is truthy.
"""
lox_truthy(::LoxNil) = false
lox_truthy(value::Bool) = value
lox_truthy(::Any) = true

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
    setvalue!(env, decl.name, LoxFunction(decl, env), true)
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
    if value isa LoxNil
        # Can't figure out how to override show or print for this...
        println("nil")
    else
        println(value)
    end
    return env
end
function lox_exec(stmt::Parser.LoxIfStatement, env::LoxEnvironment)
    condition = lox_eval(stmt.condition, env)
    if lox_truthy(condition)
        lox_exec(stmt.then_branch, env)
    else
        if stmt.else_branch !== nothing
            lox_exec(stmt.else_branch, env)
        end
    end
    return env
end
function lox_exec(stmt::Parser.LoxWhileStatement, env::LoxEnvironment)
    while lox_truthy(lox_eval(stmt.condition, env))
        lox_exec(stmt.body, env)
    end
    return env
end
function lox_exec(stmt::Parser.LoxBlockStatement, env::LoxEnvironment; allow_return=false)
    # Generate a new child environment
    new_env = LoxEnvironment(env, Dict{String,Any}())
    try
        foreach(stmt.statements) do child_stmt
            lox_exec(child_stmt, new_env)
        end
    catch e
        if e isa LoxReturn
            if allow_return
                # bubble it up and let caller handle it
                rethrow()
            else
                throw(LoxUnexpectedReturnError(e.stmt))
            end
        else
            rethrow()
        end
    end
    return env
end

function lox_exec(
    prg::Parser.LoxProgramme,
    env::LoxEnvironment=LoxEnvironment(nothing, Dict{String,Any}()),
)
    for stmt in prg.statements
        lox_exec(stmt, env)
    end
    return env
end

end # module Eval
