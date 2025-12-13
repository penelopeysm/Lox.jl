module EndToEndTests

using Test
using LoxInterpreter: LoxInterpreter
using LoxInterpreter.Errors: Errors

function test_file(
    filename::String,
    expected_stdout::Vector{String},
    expected_stderr::Vector{String},
    expected_exit_code::Int,
)
    # This opens a Julia session, so it's quite slow.
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

function test_file_fast(
    filename::String,
    expected_stdout::Vector{String},
    expected_stderr::Vector{String},
    ::Int,
)
    # This runs the interpreter in-process, so it's much faster. But we can't check exit
    # codes.
    @testset "$(filename)" begin
        contents = strip(read(filename, String))
        location = Errors.Location(filename, 1, 0)
        # Julia makes it really difficult to capture stdout/stderr into a buffer or
        # something, so we just redirect to temporary files.
        maybe_error = open("/tmp/stdout", "w") do io
            Base.redirect_stdout(io) do
                LoxInterpreter.run(contents, location, true)
            end
        end
        actual_stdout = read("/tmp/stdout", String)
        actual_stderr = if maybe_error isa Errors.LoxError
            Errors.show_error(maybe_error, contents, location)
        else
            ""
        end
        for expected_stdout_substring in expected_stdout
            @test occursin(expected_stdout_substring, actual_stdout)
        end
        for expected_stderr_substring in expected_stderr
            @test occursin(expected_stderr_substring, actual_stderr)
        end
    end
end

@testset "end to end" begin
    # Choose which test function to use:
    ftest = test_file_fast
    # ftest = test_file

    @testset "basic" begin
        ftest("../../loxprogs/helloworld.lox", ["Hello, world!"], String[], 0)
    end

    @testset "lexical scoping" begin
        ftest("../../loxprogs/scopes.lox", ["1.0\n2.0\n1.0\n3.0"], String[], 0)
    end

    @testset "conditionals" begin
        ftest(
            "../../loxprogs/ifthenelse.lox",
            ["one plus two is less than four"],
            String[],
            0,
        )
        ftest("../../loxprogs/truthy.lox", ["one is truthy", "nil is falsy"], String[], 0)
        ftest("../../loxprogs/danglingelse.lox", ["world"], String[], 0)
        ftest(
            "../../loxprogs/logicals.lox",
            [
                join(
                    [
                        "true",
                        "false",
                        "false",
                        "false",
                        "true",
                        "true",
                        "true",
                        "false",
                        "false",
                        "true",
                        "true",
                        "1.0",
                        "nil",
                        "1.0",
                    ],
                    "\n",
                ),
            ],
            String[],
            0,
        )
    end

    @testset "loops" begin
        ftest("../../loxprogs/while.lox", ["1.0\n2.0\n3.0\n4.0\n5.0"], String[], 0)
        ftest("../../loxprogs/for.lox", ["1.0\n2.0\n3.0\n4.0\n5.0"], String[], 0)
    end

    @testset "errors and reporting" begin
        ftest(
            "../../loxprogs/badadd.lox",
            String[],
            [
                "    print (x + 3);",
                "          ^^^^^ cannot add values of types String and Float64",
            ],
            LoxInterpreter.EXIT_RUNTIME_ERROR,
        )
        ftest(
            "../../loxprogs/undefvar.lox",
            String[],
            ["    print undefined;", "          ^^^^^^^^^ undefined variable: `undefined`"],
            LoxInterpreter.EXIT_RUNTIME_ERROR,
        )
        ftest(
            "../../loxprogs/dividezero.lox",
            String[],
            ["    var z = hello / world;", "           ^^^^^^^^^^^^^ division by zero"],
            LoxInterpreter.EXIT_RUNTIME_ERROR,
        )
    end
end

end # module
