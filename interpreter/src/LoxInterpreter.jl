module LoxInterpreter

@enum ExitCode begin
    ExitSuccess = 0
    ExitFileNotFound = 64
    ExitRuntimeError = 65
end

"""
    run_file(filename::AbstractString)::Nothing

Execute a Lox source file.
"""
function run_file(filename::AbstractString)::Nothing
    try
        contents = strip(read(filename, String))
        maybe_error = run(contents, filename)
        if maybe_error !== nothing
            report_error(maybe_error)
            exit(ExitRuntimeError)
        end
    catch e
        if e isa SystemError
            report_error(LoxError("startup", 0, "Could not read file '$filename'"))
            exit(ExitFileNotFound)
        end
        # Error in the compiler itself
        rethrow(e)
    end
end

"""
    run_prompt()::Nothing

Launch an interactive Lox REPL.

TODO: Implement this as a Julia REPL mode, either by hooking directly into the
Julia REPL, or through ReplMaker.jl.

TODO: Handle Ctrl-C (try/catch InterruptException doesn't seem to work)
"""
function run_prompt()::Nothing
    println("Running prompt")
    while true
        source = ""
        print("lox> ")
        while !is_complete_lox_snippet(source)
            # NOTE: If it's not eof yet, eof() will block until it gets the
            # next input before returning false -- but it doesn't consume the
            # input, so we can still use readline() after the check
            if eof(stdin)
                println()
                return
            else
                input = readline(stdin)
                source *= (input * "\n")
            end
        end
        maybe_error = run(strip(source), "REPL")
        maybe_error !== nothing && report_error(maybe_error)
    end
end

"""
    is_complete_lox_snippet(source::AbstractString)::Bool

Check if the given Lox source code snippet is complete. This helps us determine
whether the REPL should continue reading input, or if it has enough to execute.

TODO: Right now, this just checks if the source is non-empty, which is a trivial
check. I'm not sure if we need to actually parse the source, or if we could use
some simpler heuristic. I'm not even sure if it's covered in the book.
"""
function is_complete_lox_snippet(source::AbstractString)::Bool
    !isempty(source)
end

"""
    LoxError(...)

An error in Lox source code execution.

TODO: Expand with more fields such as source code, column number, ...
"""
struct LoxError{}
    file::String
    line::Int
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
    printstyled("$(err.file):$(err.line)", color=:magenta)
    println()
end

"""
    run(source::AbstractString, filename::AbstractString)::Union{Nothing, LoxError}

Execute a Lox source code snippet. Returns `nothing` if successful, or a
[`LoxError`](@ref) with diagnostic information if an error occurs.
"""
function run(source::AbstractString, filename::AbstractString)::Union{Nothing, LoxError}
    return LoxError(filename, 1, "Lox interpreter not yet implemented")
end

end # module LoxInterpreter
