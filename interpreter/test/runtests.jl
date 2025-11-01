using Test

@testset "LoxInterpreter" begin
    include("lexer.jl")
    include("parser.jl")
end
