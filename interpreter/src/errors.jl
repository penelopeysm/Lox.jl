module Errors

"""
    Location(file::String, line::Integer, column::Integer)

A location in the source code, represented by a file name, a line number, and a
column number.
"""
struct Location
    file::String
    line::Integer
    column::Integer
end

"""
    LoxError

An error in Lox source code execution.
"""
struct LoxError <: Exception
    location::Location
    message::String
end

"""
    report_error(err::LoxError)

Pretty-print a Lox error to the user.
"""
function report_error(err::LoxError)
    printstyled("Lox Error: ", bold = true, color = :red)
    println(err.message)
    print("    at: ")
    printstyled(
        "$(err.location.file):$(err.location.line):$(err.location.column)",
        color = :magenta,
    )
    println()
end

"""
    identify_location(chars_read::Int, source::AbstractString, start_loc::Location)::Location

In the lexer, we don't maintain the line and column numbers as we read in
characters; we only count the number of characters read.

This function takes `chars_read`, the number of characters read so far, and the
starting location, and parses the source string to determine the true line and
column numbers at that point.
"""
function identify_location(
    chars_read::Int,
    source::AbstractString,
    start_loc::Location,
)::Location
    # This doesn't change
    file = start_loc.file

    # We need to count how many newlines we've seen
    source_seen = source[1:chars_read]
    num_newlines = count('\n', source_seen)
    line = start_loc.line + num_newlines

    # Then count the number of characters since the last newline
    if num_newlines == 0
        column = start_loc.column + chars_read
    else
        last_line = last(split(source_seen, '\n'))
        column = length(last_line)
    end
    return Location(file, line, column)
end


end # module
