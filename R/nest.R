##' Nested function calls
##'
##' Implementation of the Nest function from WolframAlpha language
##'
##' @importFrom pryr make_function
##' @examples
##' ## fibonacci sequence
##' nest(c(x, x[length(x)] + x[length(x)-1]), c(1, 1), 5)
##'
##' ## creating a function
##' f <- nest((x+1)^2, NULL, 3)
##'
##' ## using different variable names
##' exp3 <- nest(exp(a), NULL, 3, xname = "a")
##'
##' @export
nest <- function(expr, x, n, xname = "x") {
    sexpr <- substitute(expr)
    res <- nest_base(sexpr, substitute(x), n, xname)
    res[[length(res)]]
}

##' @export
nest_list <- function(expr, x, n, xname = "x"){
    sexpr <- substitute(expr)
    nest_base(sexpr, substitute(x), n, xname)
}

nest_base <- function(expr, x, n, xname = "x") {
    if(n < 2 || n %% 1 != 0){
        stop("'n' needs to be an integer greater than 1")
    }
    expr <- get_expression(expr, xname)
    ssexpr <- substitute(substitute(expr))
    res <- vector("list", n)
    arg_base <- arg <- alist(x = )
    if(xname != "x") names(arg) <- xname

    arg[[1]] <- x
    eval_base <- try(eval(eval(ssexpr, envir = arg, enclos = parent.frame())), silent = TRUE)
    eval_logi <- ifelse(!is(eval_base, "try-error"), TRUE, FALSE)

    if(!eval_logi & length(as.character(x)) > 1){
        stop("'x' has to be a single variable.")
    }
    names(arg_base) <- as.character(x)

    for(i in 1:n){
        arg[[1]] <- x <- eval(ssexpr, envir = arg, enclos = parent.frame())

        if(eval_logi){
            res[[i]] <- eval(x)
        }else{
            res[[i]] <- make_function(arg_base, x, .GlobalEnv)
        }
    }
    res
}

nest_while <- function(expr, x, cond, xname = "x"){

}
