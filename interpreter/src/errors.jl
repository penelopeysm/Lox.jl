module Errors

struct Location
    file::String
    line::Int
end

"""
    LoxError(...)

An error in Lox source code execution.

TODO: Expand with more fields such as source code, column number, ...
"""
struct LoxError
    location::Location
    message::AbstractString
end

"""
    report_error(err::LoxError)

Pretty-print a Lox error to the user.
"""
function report_error(err::LoxError)
    printstyled("Lox Error: ", bold=true, color=:red)
    println(err.message)
    print("    at: ")
    printstyled("$(err.location.file):$(err.location.line)", color=:magenta)
    println()
end

end # module
