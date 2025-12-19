# Lox.jl

[*Crafting Interpreters*](https://craftinginterpreters.com/), but in Julia.

Specifically, this is the first half of the book (tree-walk interpreter).

Run with

```
cd interpreter
julia --project=. interpreter.jl [LOX_FILE]
```

where `LOX_FILE` is a file containing Lox code, or leave it empty to enter the REPL.

Some sample Lox programmes are given in the `loxprogs` and `aoc2025` folders.

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

- In class initialisers you can `return this` but not `return` (the book forbids the former and allows the latter). This makes more sense to me. (Returning any other value is forbidden, just as in the book.)

- Any class methods or fields that begin with an underscore are private and can only be accessed from within the class itself.

  ```
  lox> class A {}
       var a = A();
       a._private = 1;
  
  error @ REPL:3:1
      a._private = 1;
      ^^^^^^^^^^^^^^ cannot access private member '_private' outside of its class
  ```

## Would I recommend Julia for compiler work?

No way, unfortunately.

The main reason why I used Julia was because I thought I could use this as a project to understand Julia's standard library a bit better.
But, unfortunately, Julia's TTFX is just simply way, way, way, too annoying to deal with, for an interpreter that has to be rerun multiple times during development.
It would be a *much* more pleasant experience to use a language that has fast incremental compilation.
Of the languages I've used before, I would probably consider OCaml or Rust.

Correctness, which is a really important consideration for a compiler, is also much harder to achieve in julia because of its dynamic type system.

I will say that Julia's multiple dispatch system does make for some nice code (as opposed to the book which uses the visitor pattern in Java).
