# tush

A new typed Unix shell scripting language inspired by Haskell.

## Features

- Strong typing to help you write robust scripts that won't fail at runtime.
- Type-safe filepath literals and manipulation.
- Higher-order functions; curried functions.
- Purely-functional data (But everything runs in IO).
- Strict evaluation strategy.

## Examples

Here, `λ` is our prompt and `=>` indicates the return value.

### Executables

First, we can run regular programs (as every shell should be able to) with
bang-path literals. This is how you access executables in your `PATH`. We are
passing `ls` no arguments, so we give it the empty vector []. You could also
pass string arguments here if you wanted to.

```tush
λ !/ls []
=> (0, (cbits
doc
hell.tushi
LICENSE
package.yaml
README.md
Setup.hs
src
stack.yaml
TAGS
test
tush
tush.cabal
tushi
tush.ll
, ))
```

Let's break down the return value: It is a tuple of tuples. It's type is `(Int,
(String, String))`. The `Int` is the exit code of the program. The first
`String` is the printed `stdout` from the program. The second `String` is the
printed `stderr`.

### Functions
There is also a builtin `ls` which may be more convenient for scripting.

```tush
λ ls ./
=> [./.gitignore ./stack.yaml ./LICENSE ./README.md ./Setup.hs ./TAGS ./.ghci ./tush.ll ./hell.tushi ./.git/ ./.stack-work/ ./src/ ./cbits/ ./doc/ ./test/ ./package.yaml ./tush.cabal ./tush/ ./tushi/ ./.#README.md]
```

This is a builtin *function*, so no `!/` prefix here. It returns a vector of
`Path` objects, written as `[Path]`. Its first (and only) argument is a `Path`
directory literal. Directory literals end with trailing slashes. This path is
relative, because it starts with `./`. Absolute paths are written as
`/home/matt/src/tush/`, or in this case we can use a `HOME` `Path` literal with
`~/src/tush/`. This is mostly in line with what you'd expect from a Unix shell,
but this information exists at the type level (or will!).

Another example of a function is `moveTo`. Its first argument is any `Path`, and
it's second argument is a directory `Path`.

```tush
λ moveTo ~/src/tush/tush.org ~/src/tush/doc/
=> ()
```

For those familiar with how Haskell and similar languages work, everything in
tush runs in IO. This is mostly a convenience, since a good shell is mostly only
used for manipulating files other external OS constructs, and these all live in
IO.
