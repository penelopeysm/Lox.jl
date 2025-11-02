module EndToEndTests

using Test
using LoxInterpreter: LoxInterpreter

function test_file(filename::String, expected_stdout::Vector{String}, expected_stderr::Vector{String}, expected_exit_code::Int)
    @testset "$(filename)" begin
        # When running tests, the cwd is the test folder.
        system_cmd = `julia --project=.. ../interpreter.jl $(filename)`
        sout, serr = PipeBuffer(), PipeBuffer()
        try
            run(system_cmd, Base.devnull, sout, serr)
        catch e
            if e isa Base.ProcessFailedException
                @test only(e.procs).exitcode == expected_exit_code
            else
                rethrow(e)
            end
        end
        actual_stdout = String(take!(sout))
        actual_stderr = String(take!(serr))
        for expected_stdout_substring in expected_stdout
            @test occursin(expected_stdout_substring, actual_stdout)
        end
        for expected_stderr_substring in expected_stderr
            @test occursin(expected_stderr_substring, actual_stderr)
        end
    end
end

@testset "end to end" begin
    @testset "basic" begin
        test_file(
            "../../loxprogs/helloworld.lox",
            ["Hello, world!"],
            String[],
            0
        )
    end

    @testset "lexical scoping" begin
        test_file(
            "../../loxprogs/scopes.lox",
            ["1.0\n2.0\n1.0\n3.0"],
            String[],
            0
        )
    end

    @testset "errors and reporting" begin
        test_file(
            "../../loxprogs/badadd.lox",
            String[],
            ["    print (x + 3);", "          ^^^^^ cannot add values of types String and Float64"],
            LoxInterpreter.EXIT_RUNTIME_ERROR
        )
        test_file(
            "../../loxprogs/undefvar.lox",
            String[],
            ["    print undefined;", "          ^^^^^^^^^ undefined variable: `undefined`"],
            LoxInterpreter.EXIT_RUNTIME_ERROR
        )
        test_file(
            "../../loxprogs/dividezero.lox",
            String[],
            ["    var z = hello / world;", "           ^^^^^^^^^^^^^ division by zero"],
            LoxInterpreter.EXIT_RUNTIME_ERROR
        )
    end
end

end # module
