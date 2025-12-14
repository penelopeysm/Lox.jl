module SemanticAnalysis

using ..Errors: Errors, LoxError
using ..Parser: Parser as P

struct LoxScope
    variables::Set{String}
    parent::Union{LoxScope,Nothing}
end
LoxScope() = LoxScope(Set{String}(), nothing)

abstract type LoxSemAnaError <: LoxError end
struct LoxSelfReferentialInitialisationError <: LoxSemAnaError
    variable::P.LoxVariable
end
Errors.get_offset(e::LoxSelfReferentialInitialisationError) = (P.start_offset(e.variable), P.end_offset(e.variable))
Errors.get_message(e::LoxSelfReferentialInitialisationError) = "variable '$(e.variable.identifier)' cannot reference itself during initialisation"

function resolve_variables!(ast::P.LoxProgramme)
    _resolve!(ast, LoxScope(), nothing)
    return ast
end

function _resolve!(ast::P.LoxProgramme, scope::LoxScope, ::Nothing)
    for stmt in ast.statements
        _resolve!(stmt, scope, nothing)
    end
end
function _resolve!(stmt::P.LoxBlockStatement, scope::LoxScope, ::Nothing)
    child_scope = LoxScope(Set{String}(), scope)
    for s in stmt.statements
        _resolve!(s, child_scope, nothing)
    end
end
function _resolve!(stmt::P.LoxVarDeclaration, scope::LoxScope, ::Nothing)
    push!(scope.variables, stmt.variable.identifier)
    _resolve!(stmt.initial_expr, scope, stmt.variable.identifier)
end
function _resolve!(stmt::P.LoxWhileStatement, scope::LoxScope, ::Nothing)
    _resolve!(stmt.condition, scope, nothing)
    _resolve!(stmt.body, scope, nothing)
end
function _resolve!(stmt::P.LoxExprStatement, scope::LoxScope, ::Nothing)
    _resolve!(stmt.expression, scope, nothing)
end
function _resolve!(stmt::P.LoxPrintStatement, scope::LoxScope, ::Nothing)
    _resolve!(stmt.expression, scope, nothing)
end
function _resolve!(stmt::P.LoxReturnStatement, scope::LoxScope, ::Nothing)
    _resolve!(stmt.expression, scope, nothing)
end
function _resolve!(stmt::P.LoxIfStatement, scope::LoxScope, ::Nothing)
    _resolve!(stmt.condition, scope, nothing)
    _resolve!(stmt.then_branch, scope, nothing)
    _resolve!(stmt.else_branch, scope, nothing)
end
function _resolve!(fun::P.LoxFunDeclaration, scope::LoxScope, ::Nothing)
    push!(scope.variables, fun.name.identifier)
    child_scope = LoxScope(Set{String}(), scope)
    for param in fun.parameters
        push!(child_scope.variables, param.identifier)
    end
    _resolve!(fun.body, child_scope, nothing)
end
function _resolve!(expr::P.LoxBinary, scope::LoxScope, forbidden)
    _resolve!(expr.left, scope, forbidden)
    _resolve!(expr.right, scope, forbidden)
end
function _resolve!(expr::P.LoxUnary, scope::LoxScope, forbidden)
    _resolve!(expr.right, scope, forbidden)
end
function _resolve!(expr::P.LoxGrouping, scope::LoxScope, forbidden)
    _resolve!(expr.expression, scope, forbidden)
end
function _resolve!(expr::P.LoxCall, scope::LoxScope, forbidden)
    _resolve!(expr.callee, scope, forbidden)
    for arg in expr.arguments
        _resolve!(arg, scope, forbidden)
    end
end
function _resolve!(expr::P.LoxAssignment, scope::LoxScope, forbidden)
    _resolve!(expr.target_variable, scope, forbidden)
    _resolve!(expr.value_expression, scope, forbidden)
end

function _resolve!(var::P.LoxVariable, scope::LoxScope, initializing::Union{String,Nothing})
    if var.identifier == initializing
        throw(LoxSelfReferentialInitialisationError(var))
    end
    current_scope = scope
    count = 0
    while current_scope !== nothing
        if var.identifier in current_scope.variables
            var.env_index = count
            return
        end
        current_scope = current_scope.parent
        count += 1
    end
end


_resolve!(::P.LoxLiteral, ::LoxScope, ::Any) = nothing
_resolve!(::Nothing, ::Any, ::Any) = nothing

# fallback
# _resolve!(::P.LoxExpr, ::LoxScope, ::Any) = nothing
# _resolve!(::P.LoxDeclaration, ::LoxScope, ::Any) = nothing

end # module SemanticAnalysis
