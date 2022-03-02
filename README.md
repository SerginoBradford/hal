# HAL

## DESCRIPTION

> The goal of this project is to implement an interpreter for a minimalist dialect of LISP in Haskell.

## PREREQUISITES
What do you need to install ?
```bash
stack
make
```

## HOW TO BUILD
Clone and go into `hal` directory.
Then,
```bash
$ make
```

## FEATURES
The program take a list of files as command line arguments and interpret them sequentially. Symbols defined in a file will still be define in subsequent files.
This interpreter support the following types: signed integers, symbols, list as linked lists of cons.
The builtins commands are: car, cdr, cons, eq?, atom?, +, -, mod, div, *, quote, lambda, define, let, cond. It also handle a repl mode for interactive commands.

## WARNING

:warning: : For EPITECH Students, don't use this repository. Pay attention to :no_entry: 42

---

<div align="center">

<a href="https://github.com/blacky-yg" target="_blank"><img src="https://cdn.jsdelivr.net/npm/simple-icons@3.0.1/icons/github.svg" alt="github.com" width="30"></a>

</div>