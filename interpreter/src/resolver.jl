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

struct LoxCircularInheritanceError <: LoxResolverError
    class_decl::P.LoxClassDeclaration
end
Errors.get_offset(e::LoxCircularInheritanceError) =
    (P.start_offset(e.class_decl), P.end_offset(e.class_decl))
Errors.get_message(e::LoxCircularInheritanceError) =
    "class '$(e.class_decl.name.identifier)' cannot inherit from itself"

struct LoxThisOutsideClassError <: LoxResolverError
    this_expr::P.LoxThis
end
Errors.get_offset(e::LoxThisOutsideClassError) =
    (P.start_offset(e.this_expr), P.end_offset(e.this_expr))
Errors.get_message(e::LoxThisOutsideClassError) =
    "'this' cannot be used outside of a class method"

struct LoxSuperOutsideSubclassError <: LoxResolverError
    super_expr::P.LoxSuper
end
Errors.get_offset(e::LoxSuperOutsideSubclassError) =
    (P.start_offset(e.super_expr), P.end_offset(e.super_expr))
Errors.get_message(e::LoxSuperOutsideSubclassError) =
    "'super' cannot be used outside of a subclass method"

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
    _resolve!(ast, LoxScope(), nothing, 0)
    return ast
end

# in_class is 1 if we are in a class method, 2 if we are in a subclass method, and 0
# otherwise.
# YES, this is ugly. I'm sorry.

function _resolve!(ast::P.LoxProgramme, scope::LoxScope, ::Nothing, in_class::Int)
    foreach(c -> _resolve!(c, scope, nothing, in_class), P.children(ast))
end

function _resolve!(stmt::P.LoxBlockStatement, scope::LoxScope, forbidden, in_class::Int)
    child_scope = LoxScope(Set{String}(), scope)
    for s in stmt.statements
        _resolve!(s, child_scope, forbidden, in_class)
    end
end
function _resolve!(stmt::P.LoxVarDeclaration, scope::LoxScope, ::Nothing, in_class::Int)
    push!(scope.variables, stmt.variable.identifier)
    _resolve!(stmt.initial_expr, scope, stmt.variable.identifier, in_class)
end
function _resolve!(fun::P.LoxFunDeclaration{P.LoxNamedFunction}, scope::LoxScope, ::Nothing, in_class::Int)
    push!(scope.variables, fun.name.identifier)
    _resolve!(fun.name, scope, nothing, in_class)
    param_scope = LoxScope(Set{String}(), scope)
    for param in fun.parameters
        push!(param_scope.variables, param.identifier)
    end
    _resolve!(fun.body, param_scope, nothing, in_class)
end
function _resolve!(fun::P.LoxFunDeclaration{<:Union{P.LoxClassMethod,P.LoxSubClassMethod}}, scope::LoxScope, forbidden, ::Int)
    this_scope = LoxScope(Set{String}(["this"]), scope)
    super_scope = if fun.kind isa P.LoxClassMethod
        this_scope
    else
        LoxScope(Set{String}(["super"]), this_scope)
    end
    param_scope = LoxScope(Set{String}(), super_scope)
    for param in fun.parameters
        push!(param_scope.variables, param.identifier)
    end
    in_class = fun.kind isa P.LoxClassMethod ? 1 : 2
    _resolve!(fun.body, param_scope, forbidden, in_class)
end
function _resolve!(cls::P.LoxClassDeclaration, scope::LoxScope, ::Union{String,Nothing}, in_class::Int)
    push!(scope.variables, cls.name.identifier)
    _resolve!(cls.name, scope, nothing, in_class)
    if cls.inherits_from !== nothing
        if cls.inherits_from.identifier == cls.name.identifier
            throw(Errors.LoxCircularInheritanceError(cls))
        end
        _resolve!(cls.inherits_from, scope, nothing, in_class)
    end
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
function _resolve!(fun::P.LoxFunExpr, scope::LoxScope, ::Union{String,Nothing}, in_class::Int)
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
function _resolve!(var::P.LoxVariable, scope::LoxScope, forbidden::Union{String,Nothing}, ::Int)
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
function _resolve!(th::P.LoxThis, scope::LoxScope, ::Union{String,Nothing}, in_class::Int)
    in_class == 0 && throw(LoxThisOutsideClassError(th))
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
function _resolve!(su::P.LoxSuper, scope::LoxScope, ::Union{String,Nothing}, in_class::Int)
    in_class < 2 && throw(LoxSuperOutsideSubclassError(su))
    current_scope = scope
    count = 0
    while current_scope !== nothing
        if "super" in current_scope.variables
            su.env_index = count
            return
        end
        current_scope = current_scope.parent
        count += 1
    end
end
function _resolve!(ret::P.LoxReturnStatement, scope::LoxScope, forbidden, in_class::Int)
    # If you're in an initialiser, you can't return anything but `this`. Note that this
    # diverges from the book, which allows empty `return` statements but not `return this`.
    # I think that is a bit odd because `return` on its own ordinarily returns `nil`.
    if forbidden == "return" && !(ret.expression isa P.LoxThis)
        throw(LoxInvalidReturnError(ret))
    end
    _resolve_default!(ret, scope, nothing, in_class)
end
function _resolve!(get::P.LoxGet, scope::LoxScope, forbidden, in_class::Int)
    if (in_class == 0 || !(get.object isa P.LoxThis)) && startswith(get.property.identifier, ("_"))
        throw(LoxPrivateMemberAccessError(get, get.property.identifier))
    end
    _resolve_default!(get, scope, forbidden, in_class)
end
function _resolve!(set::P.LoxSet, scope::LoxScope, forbidden, in_class::Int)
    if (in_class == 0 || !(set.object isa P.LoxThis)) && startswith(set.property.identifier, ("_"))
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
