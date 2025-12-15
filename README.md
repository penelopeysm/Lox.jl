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

## Differences from the book

The interpreter contains several differences from the book (mainly because I wanted to experiment with extra features).

- Anonymous functions are implemented (this is a challenge question in the book).

- The magic variable `__scope__` returns a Julia dictionary of anything that's in scope. Since it's a Julia value, you can't actually do anything with it in Lox (except print it). This is mainly for debugging purposes, but if Lox had its own dictionaries, then this would play a similar role to Julia's `Base.@locals`.

```
lox> var x = 1; print __scope__;

Dict{String, Any}("x" => 1.0)
```

- Methods with different arities can be defined for the same function (i.e. function overloading based on number of arguments). This is similar to C++ function overloading, but only for arity (not types: Lox types are (mostly) erased at runtime, at least in the current implementation).

```
lox> fun foo(a) { print a; }
     fun foo(a, b) { print a * b; }
     foo(1); foo(2, 3);

1.0
6.0
```

- There is quite a fair bit of nice pretty-printing for errors!

```
$ cd interpreter; julia --project=. interpreter.jl ../loxprogs/dividezero.lox
error @ ../loxprogs/dividezero.lox:5:9
    var z = hello / world;
            ^^^^^^^^^^^^^ division by zero
```
