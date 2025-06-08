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
Abstract interface for Lox errors.
"""
abstract type LoxError <: Exception end
function get_location(::LoxError)::Location end
function get_message(::LoxError)::String end

struct LoxStartupError <: LoxError
    message::String
end
get_location(::LoxStartupError) = Location("startup", 0, 0)
get_message(err::LoxStartupError) = err.message

function show_error(err::LoxError)
    io = IOBuffer()
    ctx = IOContext(io, :color => true)
    printstyled(ctx, "Lox Error: ", bold = true, color = :red)
    println(ctx, err.message)
    print(ctx, "    at: ")
    printstyled(ctx,
        "$(err.location.file):$(err.location.line):$(err.location.column)",
        color = :magenta,
    )
    return String(take!(io))
end
report_error(err::LoxError) = println(Base.stderr, show_error(err))

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
