module Parser

export to_sexp, test_expr

# TODO: An abstract type seemed most natural, and also happens to mimic the
# book's implementation more closely, but should this just be a struct?
# Problems of not having ADTs...
abstract type LoxExpr end

# Indeed, we're very quickly reaching the point where we want GADTs...
abstract type LoxBinaryOp end
struct EqualEqual <: LoxBinaryOp end
struct BangEqual <: LoxBinaryOp end
struct LessThan <: LoxBinaryOp end
struct LessThanEqual <: LoxBinaryOp end
struct GreaterThan <: LoxBinaryOp end
struct GreaterThanEqual <: LoxBinaryOp end
struct Plus <: LoxBinaryOp end
struct Minus <: LoxBinaryOp end
struct Star <: LoxBinaryOp end
struct Slash <: LoxBinaryOp end
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
    LoxLiteral(value::T) where T = new{T}(value)
end

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
to_sexp_op(op::Plus) = "+"
to_sexp_op(op::Minus) = "-"
to_sexp_op(op::Star) = "*"
to_sexp_op(op::Slash) = "/"
to_sexp_op(op::Bang) = "!"
to_sexp_op(op::MinusUnary) = "-"

test_expr = LoxBinary(Star(), LoxUnary(MinusUnary(), LoxLiteral(123)), LoxGrouping(LoxLiteral(45.67)))

end # module
