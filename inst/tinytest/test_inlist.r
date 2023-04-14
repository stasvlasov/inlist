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

## test placeholders
expect_equal(
    inlist(l, .n == length(.), .b)
  , list(3)
)

## test filter
expect_equal(
    inlist(l, .e, .a)
  , list(3, 4)
)


expect_equal(
    inlist(l, , .a)
  , list(1, 2, 3, 4)
)


expect_equal(
    inlist(l, , paste("Hello", .a, "world!"))
  , list("Hello 1 world!", "Hello 2 world!", "Hello 3 world!", "Hello 4 world!")
)


expect_equal(
    inlist(l, , paste("Hello", .e, "world!"))
  , list("Hello 3 world!", "Hello 4 world!")
)


expect_equal(
    inlist(l, , paste("Hello", .a, ._(.e + ._(.f, 10) , "brave"), "world!"))
  , list("Hello 1 brave world!", "Hello 2 brave world!", "Hello 3 13 world!", 
         "Hello 4 8 world!")
)



## Check types
l <- NULL
expect_equal(
    inlist(l, .n == length(.), .b)
  , list()
)


l <- list()
expect_equal(
    inlist(l, .n == length(.), .b)
  , list()
)


l <- ""
expect_error(
    inlist(l, .n == length(.), .b)
)

l <- 1
expect_error(
    inlist(l, .n == length(.), .b)
)

l <- NA
expect_error(
    inlist(l, .n == length(.), .b)
)
