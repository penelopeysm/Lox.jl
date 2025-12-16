# Advent of Code 2025 in Lox

This is a proof of principle that Lox can do things :-)

It is horrendously, horrendously, slow, so I am probably not going to use it for anything more than Day 1, but it _works_.

Because the import statements are currently resolved with respect to the working directory of the interpreter, this MUST be run from this directory:

```
julia --project=../interpreter ../interpreter/interpreter.jl day1.lox
```
