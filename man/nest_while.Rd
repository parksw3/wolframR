% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nest.R
\name{nest_while}
\alias{nest_while}
\title{Apply a function recursively until the condition is satisfied}
\usage{
nest_while(x, expr, cond, m = 1, maxit = 1e+05, n = 0, xname = "x")
}
\arguments{
\item{x}{an object or a variable name.}

\item{expr}{an expression or a function to be evaluated recursively.}

\item{cond}{a condition to be evaluated.}

\item{m}{return results of the last \code{m} iterations. Setting \code{m = "all"} returns a list of all results;
This is equivalent to \code{NestWhileList} function from the Wolfram language.}

\item{maxit}{maximum number of iterations.}

\item{n}{additional iterations to be completed after the condition has been satisfied.}

\item{xname}{character string containing the name of the argument that will be used recursively.}
}
\description{
Implementation of the \code{NestWhile} function from the Wolfram langauge.
Syntax order has been changed so that the function is compatible with the pipe operator (see \code{\link[magrittr]{\%>\%}}).
}
\examples{
## Some of these examples are directly taken from the Wolfram documentation

## number of terms required in taylor expansion to achieve a target error.
nest_while(0, x+1, exp(1) - sum(1/factorial(0:x)) > 1e-5)

## collatz sequence
nest_while(1214, ifelse(x \%\% 2 == 0, x/2, 3*x+1), x != 1, m = "all")

## root finding algorithms
f <- function(x) x^5 - 4 * x^3 + 159
secant <- function(x) c(x[2], x[2] - f(x[2]) * diff(x)/diff(f(x)))
nest_while(c(2, 3), secant, abs(f(x[2])) > 10^-5)

## Happy number
## http://stackoverflow.com/questions/18675285/digit-sum-function-in-r
digitsum2 <- function(x) sum((floor(x / 10^(0:(nchar(x) - 1))) \%\% 10)^2)
res <- sapply(1:1000, function(x) nest_while(x, digitsum2, x!= 1 & x!= 4))
which(res == 1)

}

