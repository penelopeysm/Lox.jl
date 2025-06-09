module Eval

using ..Errors: LoxError, Location
using ..Parser: Parser

struct LoxEnvironment
    vars::Dict{String,Any}
end

struct LoxNil end

abstract type LoxEvalError <: LoxError end
struct LoxTypeError <: LoxEvalError
    message::String
end
# TODO: Fix location
get_location(::LoxTypeError) = Location("runtime", 0, 0)
get_message(err::LoxTypeError) = err.message

struct LoxZeroDivisionError <: LoxEvalError end
# TODO: Fix location
get_location(::LoxZeroDivisionError) = Location("runtime", 0, 0)
get_message(::LoxZeroDivisionError) = "Division by zero"

struct LoxRuntimeError <: LoxEvalError
    message::String
end
# TODO: Fix location
get_location(::LoxRuntimeError) = Location("runtime", 0, 0)
get_message(err::LoxRuntimeError) = err.message

"""
    lox_eval(::LoxExpr, ::LoxEnvironment)

Evaluate an expression.
"""
lox_eval(lit::Parser.LoxLiteral{<:Number}, ::LoxEnvironment) = lit.value
lox_eval(lit::Parser.LoxLiteral{<:Bool}, ::LoxEnvironment) = lit.value
lox_eval(lit::Parser.LoxLiteral{<:String}, ::LoxEnvironment) = lit.value
lox_eval(::Parser.LoxLiteral{Nothing}, ::LoxEnvironment) = LoxNil
function lox_eval(var::Parser.LoxVariable, env::LoxEnvironment)
    if haskey(env.vars, var.identifier)
        return env.vars[var.identifier]
    else
        throw(LoxRuntimeError("Undefined variable: `$(var.identifier)`"))
    end
end
lox_eval(grp::Parser.LoxGrouping, env::LoxEnvironment) = lox_eval(grp.expression, env)
function lox_eval(expr::Parser.LoxUnary{Parser.Bang}, env::LoxEnvironment)
    return !(lox_truthy(lox_eval(expr.right, env)))
end
function lox_eval(expr::Parser.LoxUnary{Parser.MinusUnary}, env::LoxEnvironment)
    return -_lox_eval_expect_f64(expr.right, env)
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
    right == 0.0 && throw(LoxZeroDivisionError())
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
        throw(LoxRuntimeError("Cannot add $(typeof(left)) and $(typeof(right))"))
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
    env.vars[stmt.identifier] = LoxNil
    nothing
end
function lox_exec(stmt::Parser.LoxVarDeclaration{<:Parser.LoxExpr}, env::LoxEnvironment)
    initial_value = lox_eval(stmt.initial_expr, env)
    env.vars[stmt.identifier] = initial_value
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
function lox_exec(prg::Parser.LoxProgramme, env::LoxEnvironment=LoxEnvironment(Dict{String,Any}()))
    for stmt in prg.statements
        lox_exec(stmt, env)
    end
    nothing
end

end # module Eval
