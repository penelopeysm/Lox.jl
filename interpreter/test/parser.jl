module ParserTests

using Test
using LoxInterpreter: Errors, Parser, Lexer

const P = Parser

# Shorthand.
lit(x, start_offset, end_offset) = P.LoxLiteral(x, start_offset, end_offset)
exp(x) = P.LoxExprStatement(x)
decl(id::String, start_offset, x) = P.LoxVarDeclaration(P.LoxVariable(id, start_offset), x)
prog(xs...) = P.LoxProgramme(collect(xs))

@testset "parser" begin
    function test_parses_to(input_tokens::Vector{Lexer.Token}, expected_programme)
        prog, parse_errors = P.parse(input_tokens)
        @test prog == expected_programme
        @test isempty(parse_errors)
    end

    function test_parses_to(input_string::AbstractString, expected_programme)
        input_tokens, lex_errors = Lexer.lex(input_string)
        @test isempty(lex_errors)
        prog, parse_errors = P.parse(input_tokens)
        @test prog == expected_programme
        @test isempty(parse_errors)
    end

    test_parses_to("3;", prog(exp(lit(3.0, 1, 2))))
    test_parses_to("3; true;", prog(exp(lit(3.0, 1, 2)), exp(lit(true, 4, 8))))
    test_parses_to("var x = 3;", prog(decl("x", 5, lit(3.0, 9, 10))))
end

end # module
