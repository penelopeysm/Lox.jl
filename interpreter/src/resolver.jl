module Resolver

using ..Errors: Errors, LoxError
using ..Parser: Parser as P

struct LoxScope
    variables::Set{String}
    parent::Union{LoxScope,Nothing}
end
LoxScope() = LoxScope(Set{String}(), nothing)

abstract type LoxResolverError <: LoxError end

struct LoxSelfReferentialInitialisationError <: LoxResolverError
    variable::P.LoxVariable
end
Errors.get_offset(e::LoxSelfReferentialInitialisationError) =
    (P.start_offset(e.variable), P.end_offset(e.variable))
Errors.get_message(e::LoxSelfReferentialInitialisationError) =
    "variable '$(e.variable.identifier)' cannot reference itself during initialisation"

struct LoxInvalidReturnError <: LoxResolverError
    return_stmt::P.LoxReturnStatement
end
Errors.get_offset(e::LoxInvalidReturnError) =
    (P.start_offset(e.return_stmt), P.end_offset(e.return_stmt))
Errors.get_message(e::LoxInvalidReturnError) =
    "cannot return a value from an initializer"

struct LoxPrivateMemberAccessError{T<:P.LoxExpr} <: LoxResolverError
    expr::T  # loxget or loxset
    private_property::String
end
Errors.get_offset(e::LoxPrivateMemberAccessError) =
    (P.start_offset(e.expr), P.end_offset(e.expr))
Errors.get_message(e::LoxPrivateMemberAccessError) =
    "cannot access private member '$(e.private_property)' outside of its class"

# the main entry point
function resolve_variables!(ast::P.LoxProgramme)
    _resolve!(ast, LoxScope(), nothing, false)
    return ast
end
function _resolve!(ast::P.LoxProgramme, scope::LoxScope, ::Nothing, in_class::Bool)
    foreach(c -> _resolve!(c, scope, nothing, in_class), P.children(ast))
end

function _resolve!(stmt::P.LoxBlockStatement, scope::LoxScope, forbidden, in_class::Bool)
    child_scope = LoxScope(Set{String}(), scope)
    for s in stmt.statements
        _resolve!(s, child_scope, forbidden, in_class)
    end
end
function _resolve!(stmt::P.LoxVarDeclaration, scope::LoxScope, ::Nothing, in_class::Bool)
    push!(scope.variables, stmt.variable.identifier)
    _resolve!(stmt.initial_expr, scope, stmt.variable.identifier, in_class)
end
function _resolve!(fun::P.LoxFunDeclaration{P.LoxNamedFunction}, scope::LoxScope, ::Nothing, in_class::Bool)
    push!(scope.variables, fun.name.identifier)
    _resolve!(fun.name, scope, nothing, in_class)
    param_scope = LoxScope(Set{String}(), scope)
    for param in fun.parameters
        push!(param_scope.variables, param.identifier)
    end
    _resolve!(fun.body, param_scope, nothing, in_class)
end
function _resolve!(fun::P.LoxFunDeclaration{P.LoxClassMethod}, scope::LoxScope, forbidden, ::Bool)
    this_scope = LoxScope(Set{String}(["this"]), scope)
    param_scope = LoxScope(Set{String}(), this_scope)
    for param in fun.parameters
        push!(param_scope.variables, param.identifier)
    end
    _resolve!(fun.body, param_scope, forbidden, true)
end
function _resolve!(cls::P.LoxClassDeclaration, scope::LoxScope, in_class::Nothing)
    in_class && error("unreachable: nested classes are not supported")
    push!(scope.variables, cls.name.identifier)
    _resolve!(cls.name, scope, nothing, in_class)
    for method in cls.methods
        forbidden = if method.name.identifier == "init"
            # Strictly speaking, this is bad code, because we're using the string 'return'
            # to indicate the keyword return. But since Lox will prevent you from creating
            # a variable called 'return', it is still unambiguous.
            "return"
        else
            nothing
        end
        # the flag will be set to true later (in the method for
        # LoxFunDeclaration{LoxClassMethod})
        _resolve!(method, scope, forbidden, in_class)
    end
end
function _resolve!(fun::P.LoxFunExpr, scope::LoxScope, ::Union{String,Nothing}, in_class::Bool)
    # Even if we are currently defining a new variable, e.g. with
    #     var f = fun (a) { ...};
    # it's OK that we reference `f` inside the function body. Hence, even if this
    # method is called with forbidden != nothing, we just ignore it.
    params_scope = LoxScope(Set{String}(), scope)
    for param in fun.parameters
        push!(params_scope.variables, param.identifier)
    end
    _resolve!(fun.body, params_scope, nothing, in_class)
end
function _resolve!(var::P.LoxVariable, scope::LoxScope, forbidden::Union{String,Nothing}, ::Bool)
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
function _resolve!(th::P.LoxThis, scope::LoxScope, ::Union{String,Nothing}, ::Bool)
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
function _resolve!(ret::P.LoxReturnStatement, scope::LoxScope, forbidden, in_class::Bool)
    # If you're in an initialiser, you can't return anything but `this`. Note that this
    # diverges from the book, which allows empty `return` statements but not `return this`.
    # I think that is a bit odd because `return` on its own ordinarily returns `nil`.
    if forbidden == "return" && !(ret.expression isa P.LoxThis)
        throw(LoxInvalidReturnError(ret))
    end
    _resolve_default!(ret, scope, nothing, in_class)
end
function _resolve!(get::P.LoxGet, scope::LoxScope, forbidden, in_class::Bool)
    if !in_class && startswith(get.property.identifier, ("_"))
        throw(LoxPrivateMemberAccessError(get, get.property.identifier))
    end
    _resolve_default!(get, scope, forbidden, in_class)
end
function _resolve!(set::P.LoxSet, scope::LoxScope, forbidden, in_class::Bool)
    if !in_class && startswith(set.property.identifier, ("_"))
        throw(LoxPrivateMemberAccessError(set, set.property.identifier))
    end
    _resolve_default!(set, scope, forbidden, in_class)
end

# generic handler for all other nodes that don't have special logic
function _resolve_default!(e::P.LoxExprOrDecl, scope::LoxScope, forbidden, in_class)
    foreach(c -> _resolve!(c, scope, forbidden, in_class), P.children(e))
end
function _resolve!(e::P.LoxExprOrDecl, scope::LoxScope, forbidden, in_class)
    _resolve_default!(e, scope, forbidden, in_class)
end

end # module SemanticAnalysis
