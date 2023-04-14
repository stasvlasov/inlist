##' Select and apply expression to elements of table like lists.
##'
##' Each list element is attached to both `extractor` and `applicator` environment so its own named elements are available as variables prefixed with "." (dot character).
##'
##' - `.` - current list's element
##' - `..` - (filtered) list (in `extractor` environment it is the same as initial list)
##' - `.n` - (filtered) list element's index
##' - `.N` - (filtered) list's length
##' - `...` - initial list (`l`) 
##' - `..n` - initial list element's index
##' - `..N` - initial list's length
##' - `._(x, fb)` - function that returns the fallback `fb` if some of the variables in `x` are unbound (i.e., due to non existing list elements)
##' 
##' @param l list (or NULL)
##' @param extractor Predicate expression to filter list elements before applying evaluated in the environment of list's element
##' @param applicator Expression applied to each element of the list evaluated in the element's environment
##' @param fallback Optional. If provided use this value as a fall back in case some variables (prefixed with dot) are not fount in the list's element environment. Otherwise (the default) those elements will be ignored and not included to results
##' 
##' @return List of filtered with `extractor` elements with values returned by `applicator`. Unbound expressions are omitted (if `fallback` is not provided) so list might be shorter.
##' 
##' @export 
inlist <- function(l, extractor, applicator, fallback) {
    if(!is.list(l) & !is.null(l)) stop("inlist -- argument `l` should be either list or NULL")
    if(missing(fallback)) {
        fallback_void <- TRUE
        fallback <- NULL
    } else {
        fallback_void <- FALSE
    }
    sys_call <- as.list(sys.call())
    parent_frame <- parent.frame()
    ## eval in elements envir
    .eval <- function(envir, index, expr, fallback, call, .l, n = index, previx_dots = TRUE) {
        ## `(` is identity function
        ## prepend dot to names
        if(length(envir) > 0 & previx_dots) {
            names(envir) <- ifelse(names(envir) != "", paste0(".", names(envir)), "")
        }
        envir <- c(envir, list(. = envir
                             , .. = .l
                             , .n = index
                             , .N = length(.l)
                             , ... = l
                             , ..n = n
                             , ..N = length(l)
                             , ._ = \(x, fb = NULL) {
                                 .eval(envir
                                     , index
                                     , sys.call()[[2]] # x
                                     , fb
                                     , call = `(`, .l
                                     , previx_dots = FALSE)
                             }))
        vars_skip <- NULL
        if(grepl("[ +-<>=*^({[%!|&]\\._\\("
               , expr_txt <- deparse1(expr))) {
            expr_data <-
                parse(text = expr_txt) |>
                getParseData()
            while(!is.na(i <- which(expr_data$text == "._")[1])) {
                expr_data <- expr_data[-(1:i),]
                expr_data <- expr_data[expr_data$token != "expr",]
                expr_data <- split(expr_data, cumsum(expr_data$parent == expr_data$parent[1]))
                if(length(expr_data) %in% c(2,3)) {
                    ._first_arg <- expr_data[[1]][-1,]
                    vars_skip <- c(vars_skip, ._first_arg[._first_arg$token == "SYMBOL", "text"])
                    expr_data <- expr_data[[length(expr_data)]][-1,]
                } else {
                    stop("inlist -- wrong number of arguments in ._() function")
                }
            }
        }
        vars <- all.vars(expr)
        ## remove vars that are in ._
        vars <- vars[!(vars %in% vars_skip)]
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
    .apply <- function(.l, expr, call = `(`, fall = fallback, ..n = NULL) {
        mapply(
            \(e, i, n) .eval(e, i, expr, fall, call, .l, n)
             , e = .l
             , i = seq_along(.l)
             , n = if(is.null(..n)) seq_along(.l) else ..n
             , SIMPLIFY = FALSE)
    }
    ## get call
    sys_call <- sys.call()
    ## filter
    if(missing(extractor)) {
        extract_l <- TRUE
    } else {
        extractor <- sys_call[[3]]
        extract_l <- .apply(l, extractor, fall = FALSE, call = Negate(isFALSE)) |> unlist()
    }
    ## map
    if(missing(applicator)) {
        apply_l <- l[extract_l]
    } else {
        applicator <- sys_call[[4]]
        apply_l <- .apply(l[extract_l], applicator, call = list, ..n = seq_along(l)[extract_l])
    }
    ## return()
    if(fallback_void) {
        apply_l <- Filter(Negate(is.null), apply_l)
    }
    return(lapply(apply_l, unlist, recursive = FALSE))
}
