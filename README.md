---
title: Assignment 2 - BNF2Haskell
author: FIT2102 Programming Paradigms
margin: 1inch
---

Please do not change the names of the functions defined in the Assignment.hs file. You may (and are highly encouraged) to implement your parsers **alongside** these pre-defined functions.

## Running the Code

```
$ stack test
```

This will generate the Haskell files using the sample input BNF files, by running your code for each exercise.

All example BNF files are stored within `examples/input` and the output of your parser will be saved in `examples/output`.

## Running the Interactive Page

In the Haskell folder run:

```
$ stack run
```

In a separate terminal, in the javascript folder run:

```
$ npm i
$ npm run dev
```

You can type BNF in to the LHS of the webpage and inspect the converted Haskell.
