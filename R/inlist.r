##' Presumably more convinient version of 'apply' for lists with table like structure.
##'
##' List element is bound to `extractor` and `selector` enviroments so its named elements available as variables
##' @param l list
##' @param extractor Predicate to filter list elements before applying.
##' @param selector 
##' @return List
##' 
##' @export 
inlist <- function(l, extractor, selector) {
    sys_call <- as.list(sys.call())
    print(sys_call)
    parent_frame <- parent.frame()
    ## eval in elements envir
    .eval <- \(envir, index, expr, call, fallback, .l = l, n = index) {
        ## `(` is identity function
        ## prepend dot to names
        names(envir) <- ifelse(names(envir) != "", paste0(".", names(envir)), "")
        envir <- c(envir, list(. = envir
                             , .. = .l
                             , .n = index
                             , .N = length(.l)
                             , ... = l
                             , ..n = n
                             , ..N = length(l)))
        vars <- all.vars(expr)
        ## find vars names that starts with .
        vars <- vars[substr(vars,0,1) == "."]
        vars_exist <- sapply(vars, \(v) eval(bquote(exists(.(v))), envir, parent_frame))
        if(all(vars_exist)) {
            do.call(call, list(eval(expr, envir, parent_frame)))
        } else {
            return(fallback)
        }
    }
    ## apply with index
    .apply <- \(.l, expr, call = `(`, fallback = NULL, simplify = FALSE, ..n = NULL) {
        mapply(\(e, i, n) .eval(e, i, expr, call, fallback, .l, n)
             , e = .l
             , i = seq_along(.l)
             , n = if(is.null(..n)) seq_along(.l) else ..n
             , SIMPLIFY = simplify)
    }
    lapply
    sys_call <- sys.call()
    print(sys_call)
    ## filter
    extractor <- sys_call[[3]]
    extract_l <- .apply(l, extractor, call = isTRUE, fallback = FALSE, simplify = TRUE)
    ## map
    selector <- sys_call[[4]]
    select_l <- .apply(l[extract_l], selector, call = list, ..n = seq_along(l)[extract_l])
    ## return()
    Filter(Negate(is.null), select_l) |>
        lapply(unlist, recursive = FALSE)
}
