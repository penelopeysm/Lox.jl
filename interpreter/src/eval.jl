module Eval

using ..Errors: LoxError, Location
using ..Parser: Parser

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
    lox_eval(::LoxExpr)

Evaluate an expression.
"""
lox_eval(lit::Parser.LoxLiteral{<:Number}) = lit.value
lox_eval(lit::Parser.LoxLiteral{<:Bool}) = lit.value
lox_eval(lit::Parser.LoxLiteral{<:String}) = lit.value
lox_eval(::Parser.LoxLiteral{Nothing}) = LoxNil
lox_eval(grp::Parser.LoxGrouping) = lox_eval(grp.expression)
function lox_eval(expr::Parser.LoxUnary{Parser.Bang})
    return !(lox_truthy(lox_eval(expr.right)))
end
function lox_eval(expr::Parser.LoxUnary{Parser.MinusUnary})
    return -_lox_eval_expect_f64(expr.right)
end

# Lox: == and != are defined on all types
lox_eval(expr::Parser.LoxBinary{Parser.EqualEqual}) = lox_eval(expr.left) == lox_eval(expr.right)
lox_eval(expr::Parser.LoxBinary{Parser.BangEqual}) = lox_eval(expr.left) != lox_eval(expr.right)
# Lox: < <= > >= - * / are only defined on numbers
lox_eval(expr::Parser.LoxBinary{Parser.LessThan}) = _lox_eval_expect_f64(expr.left) < _lox_eval_expect_f64(expr.right)
lox_eval(expr::Parser.LoxBinary{Parser.LessThanEqual}) = _lox_eval_expect_f64(expr.left) <= _lox_eval_expect_f64(expr.right)
lox_eval(expr::Parser.LoxBinary{Parser.GreaterThan}) = _lox_eval_expect_f64(expr.left) > _lox_eval_expect_f64(expr.right)
lox_eval(expr::Parser.LoxBinary{Parser.GreaterThanEqual}) = _lox_eval_expect_f64(expr.left) >= _lox_eval_expect_f64(expr.right)
lox_eval(expr::Parser.LoxBinary{Parser.Subtract}) = _lox_eval_expect_f64(expr.left) - _lox_eval_expect_f64(expr.right)
lox_eval(expr::Parser.LoxBinary{Parser.Multiply}) = _lox_eval_expect_f64(expr.left) * _lox_eval_expect_f64(expr.right)
function lox_eval(expr::Parser.LoxBinary{Parser.Divide})
    right = _lox_eval_expect_f64(expr.right)
    right == 0.0 && throw(LoxZeroDivisionError())
    return _lox_eval_expect_f64(expr.left) / right
end
# Lox: + works on both numbers and strings
function lox_eval(expr::Parser.LoxBinary{Parser.Add})
    left = lox_eval(expr.left)
    right = lox_eval(expr.right)
    if left isa Float64 && right isa Float64
        return left + right
    elseif left isa String && right isa String
        return left * right
    else
        throw(LoxRuntimeError("Cannot add $(typeof(left)) and $(typeof(right))"))
    end
end

"""
    _lox_eval_expect_f64(expr::LoxExpr)

Evaluate an expression and expect it to return a `Float64`. If the expression does not, throw a LoxTypeError.
"""
function _lox_eval_expect_f64(expr::Parser.LoxExpr)
    val = lox_eval(expr)
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

end # module Eval
