# project-template

[![Build Status](https://travis-ci.org/iu-haskell-fall-2018/project-template.svg?branch=master)](https://travis-ci.org/iu-haskell-fall-2018/project-template)

Template for a Haskell project.

### Prerequisites

This project relies on the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/).

It is recommended to get Stack with batteries included by
installing [Haskell Platform](https://www.haskell.org/platform/).

## Build

To build this project simply run

```sh
stack build
```

This will install all dependencies, including a proper version of GHC
(which should be there already if you have Haskell Platform 8.4.3).

## Run

This project has one executable that you can run with

```
stack exec my-project-exe
```

During development it is recommended a combination of `build` and `exec`:

```
stack build && stack exec my-project-exe
```

Alternatively, you can run

```
stack build file-watch
```

For continuous builds in the background.

## Interpreter

You can run GHCi (GHC interpreter) for the whole project with

```
stack ghci
```

or

```
stack repl
```

During development it might be beneficial to work using an interpreter
for quick reloads, interacting with various parts of code and
trying out things.

Note that you can run executable from GHCi by invoking `main`.

### GHCi commands

There are some useful commands in GHCi that might help
you with the development process:

- `:t` or `:type` — show the type of an expression:

```
>>> :t map (+1)
map (+1) :: Num b => [b] -> [b]
```

- `:l` or `:load` — load file or module:

```
>>> :load src/MyProject.hs
[1 of 1] Compiling MyProject        ( src/MyProject.hs, interpreted )
Ok, one module loaded.
```

- `:set` and `:unset` — turn an option on/off:

```
>>> :set -XOverloadedStrings
>>> :set -Wall -fno-warn-type-defaults
```

## `ghcid`

For faster feedback from the compiler it is recommended to use `ghcid`
(GHCi deamon).

Install `ghcid` with `stack`:

```
stack install ghcid
```

Now you can run `ghcid` with Stack using

```
ghcid -c "stack repl"
```

This will run GHCi with the entire project loaded and will
quickly reload all modules when you modify them to tell you
if you have any errors or warnings.

_Note: you can also run `ghcid` without parameters and it will detect
Stack project, but you'll have to use system-wide `.ghci`.
See [ndmitchell/ghcid#72](https://github.com/ndmitchell/ghcid/issues/72) for more details._

Here's a sample development enviroment with a text editor (left),
`ghcid` (top right) and `stack repl` (bottom right):

![Vim + ghcid + stack repl](images/ghcid-demo.png)
