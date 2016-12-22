##' Nested function calls
##'
##' Implementation of the Nest function from WolframAlpha language
##'
##' @importFrom pryr make_function
##' @examples
##' ## fibonacci sequence
##' nest(c(1, 1), c(x[2], x[1] + x[2]), 10)
##' @export
nest <- function(x, expr, n, m = 1, xname = "x") {
    if(n < 2 || n %% 1 != 0){
        stop("'n' needs to be an integer greater than 1")
    }
    x <- substitute(x)
    expr <- test_expression(substitute(expr), xname)
    ssexpr <- substitute(substitute(expr))
    res <- vector("list", n)
    arg_base <- arg <- alist(x = )
    if(xname != "x") names(arg) <- xname

    arg[[1]] <- x
    eval_base <- try(eval(eval(ssexpr, envir = arg, enclos = parent.frame())), silent = TRUE)
    eval_logi <- !is(eval_base, "try-error")

    if(!eval_logi){
        if (length(as.character(x)) > 1){
            stop("'x' has to be a single variable.")
        }else{
            names(arg_base) <- as.character(x)
        }
    }

    for(i in 1:n){
        arg[[1]] <- x <- eval(ssexpr, envir = arg, enclos = parent.frame())

        if(eval_logi){
            res[[i]] <- arg[[1]] <- eval(x)
        }else{
            ## TODO: allow for additional arguments
            res[[i]] <- make_function(arg_base, x, .GlobalEnv)
        }
    }

    v <- unlist(lapply(res, length))

    if(all(v == 1) & is.vector(res[[1]]))
        res <- unlist(res)

    if(m == "all" | m > n)
        m <- n

    return(res[(n-m+1):n])
}

##' repeat until the condition is met
##'
##' @examples
##' ## collatz sequence
##' nest_while(1214, ifelse(x %% 2 == 0, x/2, 3*x+1), x != 1)
##' nest_while(c(3249, 2723, 4901), ifelse(x %% 2 == 0, x/2, ifelse(x == 1, 1, 3*x+1)), any(x != 1))
##' @export
nest_while <- function(x, expr, cond, m = 1, max = NA, n = 0, xname = "x"){
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

    while(eval(cond, envir = list(x = x), enclos = parent.frame())){
        i <- i + 1
        res[[i]] <- x <- eval(expr, envir = list(x = x), enclos = parent.frame())
        if(i >= length(res)){
            res <- append(res, res_base)
        }
    }
    ## TODO: work on m, max, and n...
    ## also write test_vector...
    res[1:i]
}
