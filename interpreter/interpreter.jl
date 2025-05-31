using LoxInterpreter: run_file, run_prompt

function main(args)
    if length(args) > 1
        println("Usage: julia interpreter.jl [script]")
        exit(64)
    elseif length(args) == 1
        run_file(args[1])
    else
        run_prompt()
    end
end

@main
