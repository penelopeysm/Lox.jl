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
    # TODO: allow modification of variables in parent environments
    env.vars[var.identifier] = value
    return nothing
end

struct LoxNil end

abstract type LoxEvalError <: LoxError end
struct LoxTypeError <: LoxEvalError
    message::String
end
# TODO: Fix offset
Errors.get_offset(::LoxTypeError) = 1
Errors.get_message(err::LoxTypeError) = err.message

struct LoxZeroDivisionError <: LoxEvalError
    offset::Int
end
Errors.get_offset(err::LoxZeroDivisionError) = err.offset
Errors.get_message(::LoxZeroDivisionError) = "division by zero"

struct LoxRuntimeError <: LoxEvalError
    message::String
    offset::Int
end
Errors.get_offset(err::LoxRuntimeError) = err.offset
Errors.get_message(err::LoxRuntimeError) = err.message

struct LoxUndefVarError <: LoxEvalError
    variable::Parser.LoxVariable
end
Errors.get_offset(err::LoxUndefVarError) = err.variable.offset
Errors.get_message(err::LoxUndefVarError) = "undefined variable: `$(err.variable.identifier)`"

"""
    lox_eval(::LoxExpr, ::LoxEnvironment)

Evaluate an expression.
"""
lox_eval(lit::Parser.LoxLiteral{<:Number}, ::LoxEnvironment) = lit.value
lox_eval(lit::Parser.LoxLiteral{<:Bool}, ::LoxEnvironment) = lit.value
lox_eval(lit::Parser.LoxLiteral{<:String}, ::LoxEnvironment) = lit.value
lox_eval(::Parser.LoxLiteral{Nothing}, ::LoxEnvironment) = LoxNil
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
    right == 0.0 && throw(LoxZeroDivisionError(expr.operator_offset))
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
        throw(LoxRuntimeError("cannot add $(typeof(left)) and $(typeof(right))", expr.operator_offset))
    end
end

"""
    _lox_eval_expect_f64(expr::LoxExpr, env::LoxEnvironment)

Evaluate an expression and expect it to return a `Float64`. If the expression does not, throw a LoxTypeError.
"""
function _lox_eval_expect_f64(expr::Parser.LoxExpr, env::LoxEnvironment)
    val = lox_eval(expr, env)
    val isa Float64 || throw(LoxTypeError("Expected float, got $(typeof(val))"))
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
