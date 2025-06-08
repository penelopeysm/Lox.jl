module LoxInterpreter

export run_file
export run_prompt

include("errors.jl")
include("lexer.jl")
include("parser.jl")

using .Errors: LoxError, LoxStartupError, Location, show_error, report_error

const EXIT_SUCCESS = 0
const EXIT_FILE_NOT_FOUND = 64
const EXIT_RUNTIME_ERROR = 65

"""
    run_file(filename::AbstractString)::Nothing

Execute a Lox source file.
"""
function run_file(filename::AbstractString)::Nothing
    try
        contents = strip(read(filename, String))
        location = Location(filename, 1, 0)
        maybe_error = run(contents, location)
        if maybe_error !== nothing
            report_error(maybe_error)
            exit(EXIT_RUNTIME_ERROR)
        end
    catch e
        if e isa SystemError
            report_error(
                LoxStartupError("Could not read file '$filename'"),
            )
            exit(EXIT_FILE_NOT_FOUND)
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
        isempty(source) || run(source, Location("REPL", line_number, 0))
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
    run(source::AbstractString, start_loc::Errors.Location)::Union{Nothing, LoxError}

Execute a Lox source code snippet. Returns `nothing` if successful, or a
[`LoxError`](@ref) with diagnostic information if an error occurs.

`starting_location` is the line number corresponding to the first line of the
source.
"""
function run(source::AbstractString, start_loc::Location)::Nothing
    try
        tokens, lex_errors = Lexer.lex(source, start_loc)
        println("Tokens: ", tokens)
        if !isempty(lex_errors)
            warning = "Encountered $(length(lex_errors)) lexing errors:"
            indent = " " ^ 4
            for err in lex_errors
                shown = show_error(err)
                shown = indent * replace(shown, "\n" => "\n" * indent)
                warning = warning * "\n" * shown
            end
            @warn warning
        end
        ast = Parser.parse(tokens, start_loc)
        println("AST: ", Parser.to_sexp(ast))
        @warn "Lox interpreter not yet implemented"
    catch e
        if e isa LoxError
            report_error(e)
        else
            rethrow(e)
        end
    end
end

end # module LoxInterpreter
