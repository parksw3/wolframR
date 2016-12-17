##' Draw angle path plots
##'
##' @param theta a vector representing angles
##' @param length a vector representing lenghths of each segment
##' @seealso \code{\link{nest}}
##' @examples
##' angle_path(seq(0, 10, 0.01))
##' angle_path(c(90, -90)[1 + nest(c(x, 0, rev(1-x)), 0, 10)], deg = TRUE)
##' @export
angle_path <- function(theta, length = 1,
                       start = c(0, 0),
                       deg = FALSE, ...) {
    if(deg){
        theta <- theta * pi / 180
    }
    theta_seq <- cumsum(theta)
    df <- rbind(start, length * data.frame(x = cos(theta_seq), y = sin(theta_seq)))
    df_path <- cumsum(df)
    plot(df_path, type = "l", ...)
    invisible(df_path)
}
