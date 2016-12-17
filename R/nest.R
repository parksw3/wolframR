##' Nested function calls
##'
##' Implementation of the Nest function from WolframAlpha language
##'
##' @importFrom pryr make_function
##' @examples
##' ## fibonacci sequence
##' nest(c(x, x[length(x)] + x[length(x)-1]), c(1, 1), 5)
##'
##' ## you can also create function
##' f <- nest((x+1)^2, NULL, 3)
##' @export
nest <- function(expr, x, n, xname = "x") {
    ## TODO: give errors...

    ## directly taken from the curve function
    sexpr <- substitute(expr)
    if (is.name(sexpr)) {
        expr <- call(as.character(sexpr), as.name(xname))
    } else {
        if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in%
                all.vars(sexpr)))
            stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'",
                          xname), domain = NA)
        expr <- sexpr
    }

    if (is.null(x)) {
        ## TODO: allow multi-variable
        dexpr_base <- dexpr <- deparse(expr)
        for(i in 1:(n-1)){
            dexpr <- gsub(xname, dexpr, dexpr_base)
        }
        pexpr <- parse(text = dexpr)

        var <- alist(x = )
        if(names(var)[1] != xname) names(var) <- xname

        make_function(var, pexpr[[1]], parent.frame())
    }else{
        for(i in 1:n){
            x0 <- eval(expr, envir = list(x = x), enclos = parent.frame())
            x <- x0
        }
        x
    }
}
