using LoxInterpreter: run_file, run_prompt

abstract type RunMode end
struct FileMode <: RunMode
    filename::String
end
struct PromptMode <: RunMode end

function parse_args(args)
    # find --silent/-s flag
    silent = false
    if "--silent" in args || "-s" in args
        silent = true
        args = filter(x -> x != "--silent" && x != "-s", args)
    end
    # determine whether to run file or prompt
    if length(args) == 0
        return (PromptMode(), silent)
    elseif length(args) == 1
        return (FileMode(args[1]), silent)
    else
        error("Usage: julia interpreter.jl [--silent|-s] [script]")
    end
end

function main(args)
    mode, silent = parse_args(args)
    if mode isa FileMode
        exit(run_file(mode.filename, silent))
    else
        run_prompt(silent)
    end
end

@main
