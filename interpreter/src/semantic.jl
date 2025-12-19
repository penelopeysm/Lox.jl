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
Errors.get_offset(e::LoxSelfReferentialInitialisationError) =
    (P.start_offset(e.variable), P.end_offset(e.variable))
Errors.get_message(e::LoxSelfReferentialInitialisationError) =
    "variable '$(e.variable.identifier)' cannot reference itself during initialisation"

# the main entry point
function resolve_variables!(ast::P.LoxProgramme)
    _resolve!(ast, LoxScope(), nothing)
    return ast
end
function _resolve!(ast::P.LoxProgramme, scope::LoxScope, ::Nothing)
    foreach(c -> _resolve!(c, scope, nothing), P.children(ast))
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
function _resolve!(fun::P.LoxFunDeclaration, scope::LoxScope, ::Nothing)
    # TODO: if it's a class method, fun.name.identifier should not be added to the current
    # scope, since methods are not variables in the outer scope.
    push!(scope.variables, fun.name.identifier)
    _resolve!(fun.name, scope, nothing)
    child_scope = LoxScope(Set{String}(["this"]), scope)
    for param in fun.parameters
        push!(child_scope.variables, param.identifier)
    end
    _resolve!(fun.body, child_scope, nothing)
end
function _resolve!(cls::P.LoxClassDeclaration, scope::LoxScope, ::Nothing)
    push!(scope.variables, cls.name.identifier)
    _resolve!(cls.name, scope, nothing)
    # TODO: is this for loop correct?
    for method in cls.methods
        _resolve!(method, scope, nothing)
    end
end
function _resolve!(fun::P.LoxFunExpr, scope::LoxScope, ::Union{String,Nothing})
    # Even if we are currently defining a new variable, e.g. with
    #     var f = fun (a) { ...};
    # it's OK that we reference `f` inside the function body. Hence, even if this
    # method is called with forbidden != nothing, we just ignore it.
    params_scope = LoxScope(Set{String}(), scope)
    for param in fun.parameters
        push!(params_scope.variables, param.identifier)
    end
    _resolve!(fun.body, params_scope, nothing)
end
function _resolve!(var::P.LoxVariable, scope::LoxScope, initialising::Union{String,Nothing})
    if var.identifier == initialising
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
function _resolve!(th::P.LoxThis, scope::LoxScope, ::Union{String,Nothing})
    @show scope
    current_scope = scope
    count = 0
    while current_scope !== nothing
        if "this" in current_scope.variables
            th.env_index = count
            return
        end
        current_scope = current_scope.parent
        count += 1
    end
end

# generic handler for all other nodes that don't have special logic
function _resolve!(e::P.LoxExprOrDecl, scope::LoxScope, forbidden)
    foreach(c -> _resolve!(c, scope, forbidden), P.children(e))
end

end # module SemanticAnalysis
