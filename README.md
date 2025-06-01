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
LoxInterpreter.run_prompt()
```

using e.g. Revise.jl as usual.
