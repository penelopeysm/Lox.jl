module ParserTests

using Test
using LoxInterpreter: Errors, Parser, Lexer

const P = Parser

@testset "parser" begin
    function test_parses_to(predicate, input_string::AbstractString)
        input_tokens, lex_errors = Lexer.lex(input_string)
        @test isempty(lex_errors)
        prog, parse_errors = P.parse(input_tokens)
        predicate(prog)
        @test isempty(parse_errors)
    end

    test_parses_to("3;") do prog
        @test prog isa P.LoxProgramme
        @test only(prog.statements) isa P.LoxExprStatement
        lit = only(prog.statements).expression
        @test lit isa P.LoxLiteral{Float64}
        @test lit.value == 3.0
        @test P.start_offset(lit) == 1
        @test P.end_offset(lit) == 2
    end

    # These tests are so tedious to write. In principle, we should have more. But I'm too
    # lazy.
end

end # module
