module LoxInterpreter

export run_file
export run_prompt

include("errors.jl")
include("lexer.jl")
include("parser.jl")
include("eval.jl")

using .Errors: LoxError, Location, show_error, report_error

const EXIT_SUCCESS = 0
const EXIT_FILE_NOT_FOUND = 64
const EXIT_RUNTIME_ERROR = 65

"""
    run_file(filename::AbstractString, silent::Bool)::Nothing

Execute a Lox source file.
"""
function run_file(filename::AbstractString, silent::Bool)::Nothing
    try
        contents = strip(read(filename, String))
        location = Location(filename, 1, 0)
        maybe_error = run(contents, location, silent)
        if maybe_error !== nothing
            report_error(maybe_error, contents, location)
            exit(EXIT_RUNTIME_ERROR)
        end
    catch e
        if e isa SystemError
            printstyled(Base.stderr, "error: "; color=:red, bold=true)
            println(Base.stderr, "file `$filename` was not found")
            exit(EXIT_FILE_NOT_FOUND)
        end
        # Error in the compiler itself
        rethrow(e)
    end
end

"""
    run_prompt(silent::Bool)::Nothing

Launch an interactive Lox REPL.

TODO: Implement this as a Julia REPL mode, either by hooking directly into the
Julia REPL, or through ReplMaker.jl.

TODO: Handle Ctrl-C (try/catch InterruptException doesn't seem to work)
"""
function run_prompt(silent::Bool)::Nothing
    println("Running prompt. Use two newlines to finish a snippet.")
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
        isempty(source) || run(source, Location("REPL", line_number, 0), silent)
        line_number += nlines
    end
end

"""
    is_complete_lox_snippet(source::AbstractString)::Bool

Check if the given Lox source code snippet is complete. This helps us determine
whether the REPL should continue reading input, or if it has enough to execute.

This currently just checks if the snippet ends with two newlines, which
is a very basic heuristic.
"""
function is_complete_lox_snippet(source::AbstractString)::Bool
    endswith(source, "\n\n")
end

"""
    run(
        source::AbstractString,
        start_loc::Errors.Location,
        silent::Bool
    )::Union{Nothing, LoxError}

Execute a Lox source code snippet. Returns `nothing` if successful, or a
[`LoxError`](@ref) with diagnostic information if an error occurs.

`starting_location` is the line number corresponding to the first line of the
source.
"""
function run(source::AbstractString, start_loc::Location, silent::Bool)::Union{Nothing,LoxError}
    try
        tokens, lex_errors = Lexer.lex(source)
        silent || @info "Tokens: $(tokens)"
        if !isempty(lex_errors)
            warning = "Encountered $(length(lex_errors)) lexing errors:"
            indent = " "^4
            for err in lex_errors
                shown = show_error(err, source, start_loc)
                shown = indent * replace(shown, "\n" => "\n" * indent)
                warning = warning * "\n" * shown
            end
            @warn warning
        end
        ast, parse_errors = Parser.parse(tokens)
        silent || @info "AST: $(Parser.to_sexp(ast))"
        if !isempty(parse_errors)
            warning = "Encountered $(length(parse_errors)) parsing errors:"
            indent = " "^4
            for err in parse_errors
                shown = show_error(err, source, start_loc)
                shown = indent * replace(shown, "\n" => "\n" * indent)
                warning = warning * "\n" * shown
            end
            @warn warning
        end
        Eval.lox_exec(ast)
    catch e
        if e isa LoxError
            return e
        else
            rethrow(e)
        end
    end
end

end # module LoxInterpreter
