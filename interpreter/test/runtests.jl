using Test

@testset "LoxInterpreter" begin
    include("lexer.jl")
    include("parser.jl")
    include("e2e.jl")
end
