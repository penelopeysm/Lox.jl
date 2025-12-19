module EndToEndTests

using Test
using LoxInterpreter: LoxInterpreter
using LoxInterpreter.Errors: Errors

# When running tests, the cwd is the test folder.
const LOXPROGS_DIR = joinpath(@__DIR__, "..", "..", "loxprogs")

function test_file(
    filename::String,
    expected_stdout::Vector{String},
    expected_stderr::Vector{String},
    expected_exit_code::Int,
)
    # This opens a Julia session, so it's quite slow.
    @testset "$(filename)" begin
        @info "Running test for $(filename)"
        system_cmd = `julia --project=.. ../interpreter.jl $(LOXPROGS_DIR)/$(filename)`
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
        @info "Running test for $(filename)"
        contents = strip(read("$LOXPROGS_DIR/$filename", String))
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
        ftest("helloworld.lox", ["Hello, world!"], String[], 0)
    end

    @testset "lexical scoping" begin
        ftest("scopes.lox", ["1.0\n2.0\n1.0\n3.0"], String[], 0)
    end

    @testset "conditionals" begin
        ftest("ifthenelse.lox", ["one plus two is less than four"], String[], 0)
        ftest("truthy.lox", ["one is truthy", "nil is falsy"], String[], 0)
        ftest("danglingelse.lox", ["world"], String[], 0)
        ftest(
            "logicals.lox",
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
        ftest("while.lox", ["1.0\n2.0\n3.0\n4.0\n5.0"], String[], 0)
        ftest("for.lox", ["1.0\n2.0\n3.0\n4.0\n5.0"], String[], 0)
    end

    @testset "errors and reporting" begin
        ftest(
            "badadd.lox",
            String[],
            [
                "    print (x + 3);",
                "          ^^^^^ cannot add values of types string and number",
            ],
            LoxInterpreter.EXIT_RUNTIME_ERROR,
        )
        ftest(
            "undefvar.lox",
            String[],
            ["    print undefined;", "          ^^^^^^^^^ undefined variable: `undefined`"],
            LoxInterpreter.EXIT_RUNTIME_ERROR,
        )
        ftest(
            "dividezero.lox",
            String[],
            ["    var z = hello / world;", "           ^^^^^^^^^^^^^ division by zero"],
            LoxInterpreter.EXIT_RUNTIME_ERROR,
        )
    end

    @testset "functions" begin
        ftest("multimethods.lox", ["1.0\n6.0"], String[], 0)
        ftest(
            "fibonacci.lox",
            ["0.0\n1.0\n1.0\n2.0\n3.0\n5.0\n8.0\n13.0\n21.0\n34.0"],
            String[],
            0,
        )
        ftest("closures.lox", ["1.0\n2.0\nglobal\nh1\nglobal\nh2\n1.0\n1.0"], String[], 0)
        ftest("functional_pair.lox", ["first\nsecond"], String[], 0)
        ftest("anon_functions.lox", ["1.0\n2.0"], String[], 0)
    end

    @testset "list and string primitives" begin
        ftest("lists.lox", ["list<1.0,2.0,a>\n1.0\n2.0\na\n1.0\n2.0\na"], String[], 0)
        ftest("string_manip.lox", ["h e l l o"], String[], 0)
    end

    @testset "classes" begin
        ftest(
            "classes.lox",
            ["Dog\nWoof!\nWoof!\nHeidi says woof!\nHi! My name is Heidi. Woof!"],
            String[],
            0,
        )
        ftest(
            "class_initialisers.lox",
            ["Heidi says woof!\nUnnamed Dog says woof!"],
            String[],
            0,
        )
        ftest(
            "class_private.lox",
            String[],
            ["    print h._atomic_number;", "          ^^^^^^^^^^^^^^^^ cannot access private member '_atomic_number' outside of its class"],
            LoxInterpreter.EXIT_RUNTIME_ERROR,
        )
        ftest(
            "class_inheritance.lox",
            ["Woof!\nI'm making some noise!\nHOOOOOONK!"],
            String[],
            0,
        )
    end
end

end # module
