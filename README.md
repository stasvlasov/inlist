[![R-CMD-check](https://github.com/stasvlasov/inlist/workflows/R-CMD-check/badge.svg)](https://github.com/stasvlasov/inlist/actions)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/stasvlasov/inlist)

The `inlist` package provides a convenient way to subset and apply an expression on lists with a table like structure (i.e. list of lists with similar keys values sturucture). Its main function `inlist(your_list, i, j)` filters `your_list` by `i` and applies `j`. For those familiar with `data.table` the `inlist`'s interface is similar to `data.table`'s `` `[` `` extractor method (i.e., ``data.table:::`[.data.table`(dt, i ,j)`` means subset `dt` using `i` and manipulate with `j`). Each list's element is bound to environment where filtering (`i`) and selection (`j`) is evaluated However, unlike `data.table`'s `i` and `j` list's named elements in `inlist` are bound to variables that are prefixed with `.`. It also binds special variables `.`, `..`, `...`, `.n`, `.N`, `..n` and `..N` variables for accessing the list element itself, the original list, elements' index, length of the list, etc. as well as a special function `._()` which can be used to subsitute default values when some list's elemenst are not available.


# Usage

    l <- list(list(a = 1
                 , b = 1)
            , list(a = 2
                 , b = 2
                 , c = 2)
            , list(a = 3
                 , b = 3
                 , e = 3)
            , list(a = 4
                 , b = 4
                 , e = 4
                 , d = 4
                 , f = 4))
    
    
    inlist(l, .n == length(.), .b)
    
    # [[1]]
    # [1] 3
    
    inlist(l, .e, .a)
    
    # [[1]]
    # [1] 3
    # 
    # [[2]]
    # [1] 4
    
    inlist(l, , .a)
    
    # [[1]]
    # [1] 1
    # 
    # [[2]]
    # [1] 2
    # 
    # [[3]]
    # [1] 3
    # 
    # [[4]]
    # [1] 4
    
    inlist(l, , paste("Hello", .a, "world!"))
    
    # [[1]]
    # [1] "Hello 1 world!"
    # 
    # [[2]]
    # [1] "Hello 2 world!"
    # 
    # [[3]]
    # [1] "Hello 3 world!"
    # 
    # [[4]]
    # [1] "Hello 4 world!"
    
    inlist(l, , paste("Hello", .e, "world!"))
    
    # [[1]]
    # [1] "Hello 3 world!"
    # 
    # [[2]]
    # [1] "Hello 4 world!"
    
    inlist(l, , paste("Hello", .a, ._(.e + ._(.f, 10) , "brave"), "world!"))
    
    # [[1]]
    # [1] "Hello 1 brave world!"
    # 
    # [[2]]
    # [1] "Hello 2 brave world!"
    # 
    # [[3]]
    # [1] "Hello 3 13 world!"
    # 
    # [[4]]
    # [1] "Hello 4 8 world!"


# Installation

    devtools::install_github("stasvlasov/inlist")

