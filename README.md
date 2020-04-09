# Hodgkin Huxley Model in Haskell

[![Build Status](https://travis-ci.org/iu-haskell-spring-2019/project-template.svg?branch=master)](https://travis-ci.org/iu-haskell-spring-2019/project-template)

### Prerequisites

```sh
brew install pkg-config cairo
```

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

However, in some versions of stack the *exec* file if unavalibale in your local directory after build. If you have any problems, see this [Stack Tutorial for the beginners](https://guide.aelve.com/haskell/stack-cookbook-ai0adh03) and use  

```
stack install --local-bin-path=<dir>
```

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

## Some Info about Hodgkin Huxley Model  
* [Read here (English)](https://neuronaldynamics.epfl.ch/online/Ch2.S2.html)
* [Read here (Python Example)](https://www.bonaccorso.eu/2017/08/19/hodgkin-huxley-spiking-neuron-model-python/)
