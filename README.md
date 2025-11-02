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

Some sample Lox programmes are given in the `loxprogs` folder.

## From inside Julia

To mitigate the TTFX on loading the REPL, you can also run

```julia
using LoxInterpreter; run_prompt()
```

Ctrl-D will return you to the Julia REPL; if you have Revise.jl loaded you can call `run_prompt()` again and your changes will be reflected.

To run a file from the Julia REPL:

```julia
using LoxInterpreter; run_file("path/to/lox_file.lox")
```
