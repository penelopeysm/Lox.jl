module Eval

using ..Errors: Errors, LoxError, Location
using ..Parser: Parser

struct LoxEnvironment
    parent_env::Union{LoxEnvironment,Nothing}
    vars::Dict{String,Any}
end
function getvalue(env::LoxEnvironment, var::Parser.LoxVariable)
    if haskey(env.vars, var.identifier)
        return env.vars[var.identifier]
    elseif env.parent_env !== nothing
        return getvalue(env.parent_env, var)
    else
        throw(LoxUndefVarError(var))
    end
end
function setvalue!(env::LoxEnvironment, var::Parser.LoxVariable, value::Any)
    # TODO (stretch): allow modification of variables in parent environments
    env.vars[var.identifier] = value
    return nothing
end

struct LoxNil end

abstract type LoxEvalError <: LoxError end

struct LoxZeroDivisionError <: LoxEvalError
    division_expr::Parser.LoxBinary{Parser.Divide}
end
Errors.get_offset(err::LoxZeroDivisionError) = (Parser.start_offset(err.division_expr), Parser.end_offset(err.division_expr))
Errors.get_message(::LoxZeroDivisionError) = "division by zero"

struct LoxTypeError{Texpr<:Parser.LoxExpr} <: LoxEvalError
    expr::Texpr
    message::String
end
Errors.get_offset(err::LoxTypeError) = (Parser.start_offset(err.expr), Parser.end_offset(err.expr))
Errors.get_message(err::LoxTypeError) = err.message

struct LoxUndefVarError <: LoxEvalError
    # LoxVariable subtypes LoxExpr, so we have location info in it
    variable::Parser.LoxVariable
end
Errors.get_offset(err::LoxUndefVarError) = (Parser.start_offset(err.variable), Parser.end_offset(err.variable))
Errors.get_message(err::LoxUndefVarError) = "undefined variable: `$(err.variable.identifier)`"

"""
    lox_eval(::LoxExpr, ::LoxEnvironment)

Evaluate an expression.
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
    setvalue!(env, expr.target_variable, rvalue)
    return rvalue
end

# Lox: == and != are defined on all types
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
        throw(LoxTypeError(expr, "cannot add values of types $(typeof(left)) and $(typeof(right))"))
    end
end

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
    lox_exec(stmt_or_prg)

Execute a statement or programme in Lox.
"""
function lox_exec(stmt::Parser.LoxVarDeclaration{Nothing}, env::LoxEnvironment)
    setvalue!(env, stmt.variable, LoxNil)
    nothing
end
function lox_exec(stmt::Parser.LoxVarDeclaration{<:Parser.LoxExpr}, env::LoxEnvironment)
    initial_value = lox_eval(stmt.initial_expr, env)
    setvalue!(env, stmt.variable, initial_value)
    nothing
end
function lox_exec(stmt::Parser.LoxExprStatement, env::LoxEnvironment)
    lox_eval(stmt.expression, env)
    nothing
end
function lox_exec(stmt::Parser.LoxPrintStatement, env::LoxEnvironment)
    value = lox_eval(stmt.expression, env)
    println(value)
    nothing
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
    nothing
end
function lox_exec(stmt::Parser.LoxBlockStatement, env::LoxEnvironment)
    # Generate a new child environment
    new_env = LoxEnvironment(env, Dict{String,Any}())
    foreach(stmt.statements) do child_stmt
        lox_exec(child_stmt, new_env)
    end
    nothing
end
function lox_exec(
    prg::Parser.LoxProgramme,
    env::LoxEnvironment=LoxEnvironment(nothing, Dict{String,Any}()),
)
    for stmt in prg.statements
        lox_exec(stmt, env)
    end
    nothing
end

end # module Eval
