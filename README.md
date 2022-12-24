

The `inlist` package provides presumably more convinient version of 'apply' for lists with table like structure via its `inlist` function. Each list's element is bound to enviroments where filtering and selection is evaluated so its named elements are available as variables prefixed with `.`. It also binds `.`, `..`, `...`, `.n`, `.N`, `..n` and `..N` variables for accessing element itself, original list, elements indexing, length, etc.


# Installation

    devtools::install_github("stasvlasov/inlist")

