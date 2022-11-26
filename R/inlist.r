inlist <- function(l, extractor, selector) {
    parent_frame <- parent.frame()
    .eval <- \(envir, expr, call = `(`, fallback = NULL) {
        ## `(` is identity function
        if(
            all.vars(expr) |>
            sapply(\(v) eval(bquote(exists(.(v))), envir, parent_frame)) |>
            all()
        ) {
            do.call(call, list(eval(expr, envir, parent_frame)))
        } else {
            return(fallback)
        }
    }
    sys_call <- sys.call()
    ## filter
    extractor <- sys_call[[2]]
    extract_l <- sapply(l, .eval, extractor, isTRUE, FALSE)
    ## map
    selector <- sys_call[[3]]
    select_l <- lapply(l[extract_l], .eval, selector, list)
    ## return()
    Filter(Negate(is.null), select_l) |>
        lapply(unlist, recursive = FALSE)
}
