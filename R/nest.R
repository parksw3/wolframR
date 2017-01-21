##' Apply a function recursively
##'
##' Implementation of the \code{Nest} function from Wolfram Alpha.
##' Order of arguments have been changed so that it is compatible with the pipe operator (see \code{\link[magrittr]{\%>\%}}).
##'
##' @importFrom pryr make_function
##' @param x an object or a variable name.
##' @param expr an expression or a function to be evaluated recursively.
##' @param n number of iterations.
##' @param m return results of the last \code{m} iterations. Setting \code{m = "all"} returns a list of all results.
##' This is equivalent to \code{NestList} function from WolframAlpha.
##' @param xname character string containing the name of the argument that will be used recursively.
##' @return a list or a vector of length \code{m} containing the results or a function; list of functions will be returned if \code{m > 1}.
##' @examples
##' ## Some of these examples are directly taken from the Wolfram Alpha documentation
##'
##' ## Newton iterations for \sqrt{2}
##' nest(1, (x + 2/x)/2, 5)
##'
##' ## Last two terms of the Fibonacci sequence
##' nest(c(1, 1), c(x[2], x[1] + x[2]), 10, m = "all")
##'
##' ## Gray codes of length 4
##' nest(0, c(x, length(x) + rev(x)), 4)
##'
##' ## Multi-log transformation
##' multi_log <- nest(x, log(x + 1), 4)
##'
##' ## Changing xname can imitate the use of \code{#}
##' ## Both codes return a function. How are they different?
##' f1 <- nest(x, x^y, 4, xname = "y")
##' f2 <- nest(x, y^x, 4, xname = "y")
##'
##' @export
nest <- function(x, expr, n, m = 1, xname = "x") {
    if(n < 1 | n %% 1 != 0)
        stop("'n' must be a positive integer.")

    .x <- substitute(x)
    expr <- test_expression(substitute(expr), xname)
    ssexpr <- substitute(substitute(expr))
    res <- vector("list", n)
    arg_base <- arg <- alist(x = )
    if(xname != "x") names(arg) <- xname

    arg[[1]] <- .x

    eval_base <- try(eval(eval(ssexpr, envir = arg, enclos = parent.frame())), silent = TRUE)
    eval_logi <- !is(eval_base, "try-error")

    if(!eval_logi){
        if (length(as.character(.x)) > 1){
            stop("'x' has to be a single variable.")
        }else{
            names(arg_base)[1] <- as.character(.x)
        }
    }
    for(i in 1:n){
        arg[[1]] <- .x <- eval(ssexpr, envir = arg, enclos = parent.frame())

        if(eval_logi){
            res[[i]] <- arg[[1]] <- eval(.x)
        }else{
            res[[i]] <- make_function(arg_base, .x, .GlobalEnv)
        }
    }

    v <- unlist(lapply(res, length))

    if(all(v == 1) & is.vector(res[[1]]))
        res <- unlist(res)

    if(m == 1)
        return(res[[n]])

    if(m == "all" | m > n)
        m <- n

    return(res[(n-m+1):n])
}

##' Apply a function recursively until the condition is satisfied
##'
##' Implementation of the \code{NestWhile} function from Wolfram Alpha.
##' Order of arguments have been changed so that it is compatible with the pipe operator (see \code{\link[magrittr]{\%>\%}}).
##' @param x an object or a variable name.
##' @param expr an expression or a function to be evaluated recursively.
##' @param cond a condition to be evaluated.
##' @param m return results of the last \code{m} iterations. Setting \code{m = "all"} returns a list of all results;
##' This is equivalent to \code{NestWhileList} function from WolframAlpha.
##' @param maxit maximum number of iterations.
##' @param n additional iterations to be completed after the condition has been satisfied.
##' @param xname character string containing the name of the argument that will be used recursively.
##' @examples
##' ## Some of these examples are directly taken from the Wolfram Alpha documentation
##'
##' ## number of terms required in taylor expansion to achieve a target error.
##' nest_while(0, x+1, exp(1) - sum(1/factorial(0:x)) > 1e-5)
##'
##' ## collatz sequence
##' nest_while(1214, ifelse(x %% 2 == 0, x/2, 3*x+1), x != 1, m = "all")
##'
##' ## Happy number
##' ## http://stackoverflow.com/questions/18675285/digit-sum-function-in-r
##' digitsum2 <- function(x) sum((floor(x / 10^(0:(nchar(x) - 1))) %% 10)^2)
##' res <- sapply(1:1000, function(x) nest_while(x, digitsum2, x >= 10, n = 6))
##' which(res == 1)
##'
##' @export
nest_while <- function(x, expr, cond, m = 1, maxit = 1e5, n = 0, xname = "x"){
    sexpr <- substitute(expr)
    expr <- test_expression(sexpr)
    scond <- substitute(cond)
    cond <- test_expression(scond, varname = "'cond'")
    cond_base <- try(eval(cond, envir = list(x = x), enclos = parent.frame()), silent = TRUE)
    if(is(cond_base, "try-error"))
        stop(paste0("'cond' cannot be evaluated at ", substitute(x), "."), domain = NA)

    if(!is.logical(cond_base))
        stop("'cond' must return TRUE/FALSE.")

    res <- res_base <- vector("list", 100)
    i <- 1
    res[[1]] <- x

    arg <- list(x = x)

    while(i < maxit & eval(cond, envir = arg, enclos = parent.frame())){
        i <- i + 1
        res[[i]] <- arg[[1]] <- eval(expr, envir = arg, enclos = parent.frame())
        if(i >= length(res)){
            res <- append(res, res_base)
        }
    }
    if(n > 0){
        for(j in 1:n){
            i <- i + 1
            res[[i]] <- arg[[1]] <- eval(expr, envir = arg, enclos = parent.frame())
            if(i >= length(res)){
                res <- append(res, res_base)
            }
        }
    }

    v <- unlist(lapply(res, length)); v <- v[v > 0]

    if(all(v == 1) & is.vector(res[[1]]))
        res <- unlist(res)

    if(m == 1){
        return(res[[i]])
    }else if(m == "all" | m > i){
        m <- i
    }

    return(res[(i-m+1):i])
}
