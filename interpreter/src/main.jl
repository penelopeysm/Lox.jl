module Main

using ..Errors: Location, LoxError, report_error

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
        location = Location(filename, 1)
        maybe_error = run(contents, location)
        if maybe_error !== nothing
            report_error(maybe_error)
            exit(ExitRuntimeError)
        end
    catch e
        if e isa SystemError
            report_error(
                LoxError(Location("startup", 0), "Could not read file '$filename'"),
            )
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
    line_number = 1
    while true
        source = ""
        nlines = 0
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
                nlines += 1
            end
        end
        source = strip(source)
        isempty(source) || run(source, Location("REPL", line_number))
        line_number += nlines
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
    run(
        source::AbstractString,
        starting_location::Errors.Location
    )::Union{Nothing, LoxError}

Execute a Lox source code snippet. Returns `nothing` if successful, or a
[`LoxError`](@ref) with diagnostic information if an error occurs.

`starting_location` is the line number corresponding to the first line of the
source.
"""
function run(source::AbstractString, starting_location::Location)::Union{Nothing,LoxError}
    try
        throw(LoxError(starting_location, "Lox interpreter not yet implemented"))
    catch e
        if e isa LoxError
            report_error(e)
        else
            rethrow(e)
        end
    end
end

end # module
