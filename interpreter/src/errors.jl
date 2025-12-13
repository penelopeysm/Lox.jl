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
function get_offset(::LoxError)::Tuple{Int,Int} end
function get_message(::LoxError)::String end

function show_error(err::LoxError, source::AbstractString, start_loc::Location)
    start_offset, end_offset = get_offset(err)
    loc, contents = identify_location(start_offset, source, start_loc)
    io = IOBuffer()
    ctx = IOContext(io, :color => true)
    printstyled(ctx, "error"; color = :red, bold = true)
    printstyled(ctx, " @ $(loc.file):$(loc.line):$(loc.column)"; color = :red)
    println(ctx)
    print(ctx, "    " * contents * "\n")
    ncarets = end_offset - start_offset
    printstyled(
        ctx,
        "    " * " "^(loc.column - 1) * ("^"^ncarets) * " " * get_message(err),
        color = :blue,
    )
    return String(take!(io))
end
report_error(err::LoxError, source::AbstractString, start_loc::Location) =
    println(Base.stderr, show_error(err, source, start_loc))

"""
    identify_location(
        offset::Int,
        source::AbstractString,
        start_loc::Location
    )::Location

In the lexer, parser, etc., we don't maintain the line and column numbers as we read in
characters; we only count the number of characters read, which is stored as `offset`.

This function takes `offset`, the number of characters read so far, and the starting
location, and parses the source string to determine the true line and column numbers at that
point.

It returns two things: a `Location` struct representing the file, line, and column at that offset,
and also a string corresponding to that line in the source code.
"""
function identify_location(
    offset::Int,
    source::AbstractString,
    start_loc::Location,
)::Tuple{Location,AbstractString}
    file = start_loc.file

    offset < 1 && error("offset must be >= 1; got $offset")

    # Calculate line and column numbers
    line_offsets = get_line_beginning_offsets(source)
    relative_line_number = searchsortedlast(line_offsets, offset)
    column_number = offset - line_offsets[relative_line_number] + 1
    line_number = start_loc.line + relative_line_number - 1

    # Get the line
    line_start = line_offsets[relative_line_number]
    line_contents = if relative_line_number == length(line_offsets)
        source[line_start:end]
    else
        source[line_start:line_offsets[relative_line_number+1]-2]
    end

    return Location(file, line_number, column_number), line_contents
end

"""
Return a vector of offsets corresponding to the beginning of each line in `source`.
"""
function get_line_beginning_offsets(source::AbstractString)::Vector{Int}
    source = strip(source)
    return [1, map(x -> x + 1, findall('\n', source))...]
end

end # module
