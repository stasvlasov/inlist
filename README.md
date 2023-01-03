[![R-CMD-check](https://github.com/stasvlasov/inlist/workflows/R-CMD-check/badge.svg)](https://github.com/stasvlasov/inlist/actions)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/stasvlasov/inlist)

The `inlist` package provides presumably more convinient version of 'apply' for lists with table like structure via its `inlist` function. Each list's element is bound to enviroments where filtering and selection is evaluated so its named elements are available as variables prefixed with `.`. It also binds `.`, `..`, `...`, `.n`, `.N`, `..n` and `..N` variables for accessing element itself, original list, elements indexing, length, etc.

inlist also binds .<sub>()</sub> function which can be use to control partial substitution for specific sub expressions in case some variables are not bound for these sub expressions.


# Installation

    devtools::install_github("stasvlasov/inlist")

