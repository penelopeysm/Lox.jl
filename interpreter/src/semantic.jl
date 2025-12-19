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

struct LoxInvalidReturnError <: LoxSemAnaError
    return_stmt::P.LoxReturnStatement
end
Errors.get_offset(e::LoxInvalidReturnError) =
    (P.start_offset(e.return_stmt), P.end_offset(e.return_stmt))
Errors.get_message(e::LoxInvalidReturnError) =
    "cannot return a value from an initializer"

# the main entry point
function resolve_variables!(ast::P.LoxProgramme)
    _resolve!(ast, LoxScope(), nothing)
    return ast
end
function _resolve!(ast::P.LoxProgramme, scope::LoxScope, ::Nothing)
    foreach(c -> _resolve!(c, scope, nothing), P.children(ast))
end

function _resolve!(stmt::P.LoxBlockStatement, scope::LoxScope, forbidden)
    child_scope = LoxScope(Set{String}(), scope)
    for s in stmt.statements
        _resolve!(s, child_scope, forbidden)
    end
end
function _resolve!(stmt::P.LoxVarDeclaration, scope::LoxScope, ::Nothing)
    push!(scope.variables, stmt.variable.identifier)
    _resolve!(stmt.initial_expr, scope, stmt.variable.identifier)
end
function _resolve!(fun::P.LoxFunDeclaration{P.LoxNamedFunction}, scope::LoxScope, ::Nothing)
    push!(scope.variables, fun.name.identifier)
    _resolve!(fun.name, scope, nothing)
    param_scope = LoxScope(Set{String}(), scope)
    for param in fun.parameters
        push!(param_scope.variables, param.identifier)
    end
    _resolve!(fun.body, param_scope, nothing)
end
function _resolve!(fun::P.LoxFunDeclaration{P.LoxClassMethod}, scope::LoxScope, forbidden)
    this_scope = LoxScope(Set{String}(["this"]), scope)
    param_scope = LoxScope(Set{String}(), this_scope)
    for param in fun.parameters
        push!(param_scope.variables, param.identifier)
    end
    _resolve!(fun.body, param_scope, forbidden)
end
function _resolve!(cls::P.LoxClassDeclaration, scope::LoxScope, ::Nothing)
    push!(scope.variables, cls.name.identifier)
    _resolve!(cls.name, scope, nothing)
    for method in cls.methods
        forbidden = if method.name.identifier == "init"
            # Strictly speaking, this is bad code, because we're using the string 'return'
            # to indicate the keyword return. But since Lox will prevent you from creating
            # a variable called 'return', it is still unambiguous.
            "return"
        else
            nothing
        end
        _resolve!(method, scope, forbidden)
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
function _resolve!(var::P.LoxVariable, scope::LoxScope, forbidden::Union{String,Nothing})
    if var.identifier == forbidden
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
function _resolve!(ret::P.LoxReturnStatement, scope::LoxScope, forbidden)
    # If you're in an initialiser, you can't return anything but `this`. Note that this
    # diverges from the book, which allows empty `return` statements but not `return this`.
    # I think that is a bit odd because `return` on its own ordinarily returns `nil`.
    if forbidden == "return" && !(ret.expression isa P.LoxThis)
        throw(LoxInvalidReturnError(ret))
    end
    _resolve!(ret.expression, scope, nothing)
end

# generic handler for all other nodes that don't have special logic
function _resolve!(e::P.LoxExprOrDecl, scope::LoxScope, forbidden)
    foreach(c -> _resolve!(c, scope, forbidden), P.children(e))
end

end # module SemanticAnalysis
