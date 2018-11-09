-- Complementary DNA mapping

Take a string of DNA symbols and find the complement of a DNA strand.

> module DNA where

The following is our data and types for DNA. There are only 4 symbols,
which can then later be built up in a list.

> data Base = A | T | G | C
> type DNA = [Base]

Complementary types are mapped as follows such that
A <=> T | C <=> G

> flipDna :: Base -> Base
> flipDna A = T
> flipDna T = A
> flipDna C = G
> flipDna G = C


Afterwards, flipping each base DNA unit is just mapping flipDNA
over the list.

> dnaStrand :: DNA -> DNA
> dnaStrand = map flipDna


-- end
