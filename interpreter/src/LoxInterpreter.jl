module LoxInterpreter

include("errors.jl")
include("lexer.jl")
include("parser.jl")
include("main.jl")

using .Main: run_file, run_prompt

export run_file
export run_prompt

end # module LoxInterpreter
