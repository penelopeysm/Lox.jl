module LexerTests

using Test
using LoxInterpreter: Errors, Lexer

dummy_location = Errors.Location("test_suite", 1, 1)

@testset "lexer" begin
    function test_lexes_to(input, expected_tokens)
        tokens, lex_errors = Lexer.lex(input, dummy_location)
        @test tokens[1:end-1] == expected_tokens
        @test tokens[end] == Lexer.Eof()
        @test isempty(lex_errors)
    end

    # Nothing (and whitespace)
    test_lexes_to("", [])
    test_lexes_to("      ", [])

    # Operators
    test_lexes_to("(", [Lexer.LeftParen()])
    test_lexes_to(")", [Lexer.RightParen()])
    test_lexes_to("{", [Lexer.LeftBrace()])
    test_lexes_to("}", [Lexer.RightBrace()])
    test_lexes_to(",", [Lexer.Comma()])
    test_lexes_to(".", [Lexer.Dot()])
    test_lexes_to("-", [Lexer.Minus()])
    test_lexes_to("+", [Lexer.Plus()])
    test_lexes_to(";", [Lexer.Semicolon()])
    test_lexes_to("/", [Lexer.Slash()])
    test_lexes_to("*", [Lexer.Star()])
    test_lexes_to("!", [Lexer.Bang()])
    test_lexes_to("!=", [Lexer.BangEqual()])
    test_lexes_to("=", [Lexer.Equal()])
    test_lexes_to("==", [Lexer.EqualEqual()])
    test_lexes_to(">", [Lexer.Greater()])
    test_lexes_to(">=", [Lexer.GreaterEqual()])
    test_lexes_to("<", [Lexer.Less()])
    test_lexes_to("<=", [Lexer.LessEqual()])

    # Numbers
    test_lexes_to("1.5", [Lexer.LoxNumber(1.5)])
    test_lexes_to("1.52", [Lexer.LoxNumber(1.52)])
    test_lexes_to("1.", [Lexer.LoxNumber(1), Lexer.Dot()]) # this will fail to parse
    test_lexes_to("1", [Lexer.LoxNumber(1)])

    # Strings
    test_lexes_to("\"hello\"", [Lexer.LoxString("hello")])
    test_lexes_to("\"hello\nworld\"", [Lexer.LoxString("hello\nworld")])

    # Other identifiers
    test_lexes_to("foo", [Lexer.Identifier("foo")])
    test_lexes_to("a_b", [Lexer.Identifier("a_b")])
    # This should have an unexpected character error
    # test_lexes_to("a_#b", [Lexer.Identifier("a_"), Lexer.Identifier("b")])

    # Keywords
    test_lexes_to("and", [Lexer.And()])
    test_lexes_to("class", [Lexer.Class()])
    test_lexes_to("else", [Lexer.Else()])
    test_lexes_to("false", [Lexer.False()])
    test_lexes_to("fun", [Lexer.Fun()])
    test_lexes_to("for", [Lexer.For()])
    test_lexes_to("if", [Lexer.If()])
    test_lexes_to("nil", [Lexer.Nil()])
    test_lexes_to("or", [Lexer.Or()])
    test_lexes_to("print", [Lexer.Print()])
    test_lexes_to("return", [Lexer.Return()])
    test_lexes_to("super", [Lexer.Super()])
    test_lexes_to("this", [Lexer.This()])
    test_lexes_to("true", [Lexer.True()])
    test_lexes_to("var", [Lexer.Var()])
    test_lexes_to("while", [Lexer.While()])
end

end # module
