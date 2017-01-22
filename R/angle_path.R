##' Draw angle path plots
##' Implementation of the \code{AnglePath} function from the Wolfram language.
##'
##' @param theta a numeric vector containing angle at each step.
##' @param length a numeric vector containing lenghths of each segment.
##' @param start a numeric vector containing starting poistion.
##' @param deg a logical indicating whether degree should be used instead of radian.
##' @param ... additional arguments to be passed to \code{plot}.
##' @seealso \code{\link{nest}}
##' @examples
##' ## Some of these examples are directly taken from the Wolfram documentation
##'
##' angle_path(seq(0, 10, 0.01))
##'
##' angle_path(c(30, 50, -40), c(3, 2, 1), deg = TRUE)
##'
##' ## Create a regular n-gon
##' n <- 5
##' angle_path(rep(360/n, n), deg = TRUE)
##'
##' ## Make a random walk where successive change direction by at most 20 degrees
##' angle_path(runif(1000, min = -20, max = 20), deg = TRUE)
##'
##' ## Dragon curve
##' angle_path(c(90, -90)[1 + nest(0, c(x, 0, rev(1-x)), 10)], deg = TRUE)
##' @export
angle_path <- function(theta, length = 1,
                       start = c(0, 0),
                       deg = FALSE,
                       plot = TRUE, ...) {
    if(deg){
        theta <- theta * pi / 180
    }
    theta_seq <- cumsum(theta)
    df <- rbind(start, length * data.frame(x = cos(theta_seq), y = sin(theta_seq)))
    df_path <- cumsum(df)
    if(plot)
        plot(df_path, type = "l", ...)

    invisible(df_path)
}
