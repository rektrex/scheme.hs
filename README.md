# scheme.hs
A simple Scheme implementation

A scheme interpreter with the ability to load scheme files is implemented in `simpleparser1.hs`. A minimal standard library is defined in `stdlib.scm`.

## Usage

```
Lisp>>> (load "stdlib.scm")

Lisp>>> (let (x 1) x)

1

Lisp>>> quit
```
