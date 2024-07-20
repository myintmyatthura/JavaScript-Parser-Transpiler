# JavaScript Parser and Transpiler written in Haskell

---

## A Tool To Parse JavaScript Code and Transpile It Into PureScript

---

A highly optimized and memory-efficient JavaScript Parser and Transpiler written in Haskell. Using the Pure Functional Programming Paradigm assisted by the Haskell language itself, the parser is incredibly robust and efficient. This parser is able to parse and transpile all JavaScript code from function declarations, variables, loops and all of which can be nested inside each other. This has an additional feature of prettifying all valid JavaScript code.

**Features:**

- JavaScript parser that is fully featured and
  able to parse almost all vanilla JavaScript
  features.
- Incredibly easy to use with integrated test-cases
  or able to manually parse any JavaScript input.
- Quick building and transpiling using Cabal and
  Stack.
- Able to understand all valid JavaScript input and even
  reasonably invalid JavaScript code.
- Naturally Recursive Abstract Syntax Tree
- Included Pretty-Printer (Prettifier) for JavaScript code

---

**Pre-requisites for execution:**

- Have Ghcup
- Have Cabal and Stack installed
- Dependencies: Need some JavaScript Test Cases file to automatically test

## Running the Code

```
$ stack test
```

This will generate the transpiled JS files using the sample input JS files, by running your pretty printing function for each exercise.

## Running the Javascript Tests

In the javascript folder run:

```
$ npm i
$ npm run dev
```

All example scripts are stored within `javascript/inputs` and the output of your parser will be saved in `javascript/output`.

The tests on the page test:

- The generated code is valid JS (i.e. it runs without errors, including non-termination error)
- The generated code has certain properties of the original code (e.g. immutable variables are still immutable)
- The output is "prettified" from the input based on visual, side-by-side inspection
