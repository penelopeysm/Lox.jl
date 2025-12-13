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

    test_parses_to("f(3);") do prog
        @test prog isa P.LoxProgramme
        @test only(prog.statements) isa P.LoxExprStatement
        call = only(prog.statements).expression
        @test call isa P.LoxCall
        @test call.callee isa P.LoxVariable
        @test call.callee.identifier == "f"
        @test only(call.arguments) isa P.LoxLiteral{Float64}
        @test only(call.arguments).value == 3.0
        @test P.start_offset(call) == 1
        @test P.end_offset(call) == 5
    end

    test_parses_to("f(3)(\"a\");") do prog
        @test prog isa P.LoxProgramme
        @test only(prog.statements) isa P.LoxExprStatement
        call = only(prog.statements).expression
        @test call isa P.LoxCall
        @test call.callee isa P.LoxCall
        @test call.callee.callee.identifier == "f"
        # we won't check call.callee further since it's the same as the previous test
        @test only(call.arguments) isa P.LoxLiteral{String}
        @test only(call.arguments).value == "a"
        @test P.start_offset(call) == 1
        @test P.end_offset(call) == 10
    end

    # These tests are so tedious to write. In principle, we should have more. But I'm too
    # lazy.
end

end # module
