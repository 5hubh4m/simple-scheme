# SimpleScheme

A very simple implementation of Scheme in Haskell from the book [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). It is a work in progress.

## Building and Running

The sources can be build by the `.cabal` file using `stack` or `cabal`. Perhaps, we must also initialise the project managers for better library and environment handling.

*`stack` is recommended to avoid `cabal`-hell.*

> `cabal init; cabal build`

or

> `stack init; stack build`

Then the executable can be run by

> `cabal run`

or

> `stack exec scheme`

## Modules

The source is divide into the following sections

#### `SimpleDefs.hs`

> Contains the data definitions for the project and hepler fucntions

#### `SimpleParser.hs`

> Contains parsing functions for various datatypes. Implementation for more complex data types is commented out as it needs further refinement.

#### `SimpleEvaluator.hs`

> Contains the functions for evaluating the lisp expressions.

#### `SimpleError.hs`

> Contains definitions for error handling functions.

#### `SimpleEnv.hs`

> Definitions for supporting mutable environment variables.

#### `Main.hs`

> Contains the REPL and main function.

## TODOs

* Complete the excercises.
* Possbily implement the complete R5RS standard.
