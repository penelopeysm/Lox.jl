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

  ```
  lox> print (fun (x) { return x + 1; })(2);
  
  3.0
  ```

- Some escape sequences are implemented (`\n`, `\t`, `\\`, `\"`).

  ```
  lox> print "Hello\nWorld";
  
  Hello
  World
  ```

- Methods with different arities can be defined for the same function (i.e. function overloading based on number of arguments). This is similar to C++ function overloading, but only for arity (not types... yet?).

  ```
  lox> fun foo(a) { print a; }
       fun foo(a, b) { print a * b; }
       foo(1); foo(2, 3);
  
  1.0
  6.0
  ```

- There are lists. Lists are heterogeneous and immutable. There is no special syntax to create them, but you can create a list with `vec(args...)`, which is a native function. There are some other builtin functions for lists, like `length()`. A string can be split into a list of single-character strings with `chars(str)`.

  ```
  lox> var xs = vec("a", "b", "c");
       print xs;
  
  list<a,b,c>
  ```

- You can parse strings into numbers. Look, I just want to be able to do Advent of Code day 1 in my language.

  ```
  lox> print to_number("1") + to_number("2");
  
  3.0
  ```

- You can `import(filename);` to execute the contents of that file. I would prefer to make `import` a statement, but I'm too lazy to twiddle with the lexer and parser (for now). There is a prelude in the `aoc2025` folder, which you can use to make life a bit easier.

  ```
  lox> import("prelude.lox"); print split("Mississippi", "s");

  list<Mi,,i,,ippi>
  ```

- There is quite a fair bit of nice pretty-printing for errors!

  ```
  $ cd interpreter; julia --project=. interpreter.jl ../loxprogs/dividezero.lox
  error @ ../loxprogs/dividezero.lox:5:9
      var z = hello / world;
              ^^^^^^^^^^^^^ division by zero
  ```
