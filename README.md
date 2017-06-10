# partial-order [![Hackage version](https://img.shields.io/hackage/v/partial-order.svg?label=Hackage)](https://hackage.haskell.org/package/partial-order) [![Stackage version](https://www.stackage.org/package/partial-order/badge/lts?label=Stackage)](https://www.stackage.org/package/partial-order) [![Build Status](https://travis-ci.org/mtesseract/haskell-partial-order.svg?branch=master)](https://travis-ci.org/mtesseract/haskell-partial-order)

### About

This packages provides the PartialOrd typeclass suitable for types
admitting a partial order. 

The only module exposed by this package is Data.PartialOrd. Along with
the PartialOrd typeclass and some utility functions for working with
partially ordered types, it exports instances for certain numeric
types along with instances for lists and sets.

Originally it has been written for quick algorithm prototyping for
which it was necessary to use the partial order on lists given by set
inclusion.

Note that this module exports the names `<=`, `>=`, `==`, `/=`, `<`,
`>`, `nub`, `elem` and `notElem`. Therefore one might want to consider
importing this module qualified in order to avoid name conflicts.

For the types Int, Integer, Float, Double the above relation operators
exported by Data.PartialOrd should behave exactly as the operators of
the same names defined for the respective total orders.

The PartialOrd instance for sets (Data.Set) and lists on the other
hand is defined in terms of the subset relation. For example,

  [2,1] <= [1,2,3]

and

  [] <= l for any list l.

Also, we have:

  [1, 2, 3] == [3, 1, 2]

### Contact

In case of questions, feel free to contact the maintainer Moritz
Schulte <mtesseract@silverratio.net>.

### License

This code is licensed under the terms of the BSD3 license. See the
file LICENSE for details.
