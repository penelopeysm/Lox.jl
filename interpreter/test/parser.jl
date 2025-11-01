module ParserTests

using Test
using LoxInterpreter: Errors, Parser, Lexer

const P = Parser

dummy_location = Errors.Location("test_suite", 1, 1)

# Shorthand.
lit(x) = P.LoxLiteral(x)
exp(x) = P.LoxExprStatement(x)
decl(id::String, x) = P.LoxVarDeclaration(id, x)
prog(xs...) = P.LoxProgramme(collect(xs))

@testset "parser" begin
    function test_parses_to(input_tokens::Vector{Lexer.Token}, expected_programme)
        prog, parse_errors = P.parse(input_tokens, dummy_location)
        @test prog == expected_programme
        @test isempty(parse_errors)
    end

    function test_parses_to(input_string::AbstractString, expected_programme)
        input_tokens, lex_errors = Lexer.lex(input_string, dummy_location)
        @test isempty(lex_errors)
        prog, parse_errors = P.parse(input_tokens, dummy_location)
        @test prog == expected_programme
        @test isempty(parse_errors)
    end

    test_parses_to("3;", prog(exp(lit(3))))
    test_parses_to("3; true;", prog(exp(lit(3)), exp(lit(true))))
    test_parses_to("var x = 3;", prog(decl("x", lit(3))))
end

end # module
