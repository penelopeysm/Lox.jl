# Lox.jl

[*Crafting Interpreters*](https://craftinginterpreters.com/), but in Julia.

## `interpreter`

The first half of the book (tree-walk interpreter).

Run with

```julia
cd interpreter
julia --project=. interpreter.jl [LOX_FILE]
```

where `LOX_FILE` is a file containing Lox code, or leave it empty to enter the REPL.
