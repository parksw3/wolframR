% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nest.R
\name{nest}
\alias{nest}
\title{Apply a function recursively}
\usage{
nest(x, expr, n, m = 1, xname = "x")
}
\arguments{
\item{x}{an object or a variable name.}

\item{expr}{an expression or a function to be evaluated recursively.}

\item{n}{number of iterations.}

\item{m}{return results of the last \code{m} iterations. Setting \code{m = "all"} returns a list of all results.
This is equivalent to \code{NestList} function from the Wolfram language.}

\item{xname}{character string containing the name of the argument that will be used recursively.}
}
\value{
a list or a vector of length \code{m} containing the results or a function; list of functions will be returned if \code{m > 1}.
}
\description{
Implementation of the \code{Nest} function from the Wolfram language.
Syntax order has been changed so that the function is compatible with the pipe operator (see \code{\link[magrittr]{\%>\%}}).
}
\examples{
## Some of these examples are directly taken from the Wolfram documentation

## Newton iterations for \\sqrt{2}
nest(1, (x + 2/x)/2, 5)

## Last two terms of the Fibonacci sequence
nest(c(1, 1), c(x[2], x[1] + x[2]), 10, m = "all")

## Gray codes of length 4
nest(0, c(x, length(x) + rev(x)), 4)

## Multi-log transformation
multi_log <- nest(x, log(x + 1), 4)

## Changing xname can imitate the use of '#' in the Wolfram language
## Both codes return a function. How are they different?
f1 <- nest(x, x^y, 4, xname = "y")
f2 <- nest(x, y^x, 4, xname = "y")

}

