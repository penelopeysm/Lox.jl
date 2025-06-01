# Lox.jl

[*Crafting Interpreters*](https://craftinginterpreters.com/), but in Julia.

## `interpreter`

The first half of the book (tree-walk interpreter).

Run with

```
cd interpreter
julia --project=. interpreter.jl [LOX_FILE]
```

where `LOX_FILE` is a file containing Lox code, or leave it empty to enter the REPL.

To mitigate the TTFX on loading the REPL, you can also run

```julia
using LoxInterpreter
run_prompt()
```

Ctrl-D will return you to the Julia REPL, where you can use e.g. Revise.jl as usual and call `run_prompt()` again.
