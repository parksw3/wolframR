##' get expression
##' @export
get_expression <- function(expr, xname = "x",
                           varname = "'expr'"){
    ## based on the curve function
    sexpr <- expr
    if (is.name(sexpr)) {
        expr <- call(as.character(sexpr), as.name(xname))
    } else {
        if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in%
                all.vars(sexpr)))
            stop(gettextf(paste0(varname, "must be a function, or a call or an expression containing '%s'"),
                          xname), domain = NA)
        expr <- sexpr
    }
    return(expr)
}
