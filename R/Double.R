#' Double
#'
#' @param number numeric vector
#'
#' @return renvoi le double de number boloss
#' @export
#'
#' @examples
#' Double(runif(4))
Double <- function(number) {
  #input check
  if (!is.numeric(number))
    stop("Double requires a numeric object")
  return(2L * number)
}




#' fuzzydouble
#'
#' @param x
#' @param mean
#' @param sd
#'
#' @return fuzzy multiplication
#' @export
#'
#' @examples
fuzzydouble <- function(x, mean = 0, sd = 1) {



  y <- 2 * x + stats::rnorm(n = length(x), mean = mean, sd = sd)

  fuzzydouble <- data.frame(x = x, y = y)

  class(fuzzydouble)<- c("fuzzydouble", class(fuzzydouble))
  return(fuzzydouble)
}



#' plot.FuzzyDouble
#'
#' @param x
#' @param xlab
#' @param ylab
#' @param ...
#' @param LineCol
#'
#' @return
#' @export
#'
#' @examples
plot.FuzzyDouble <-function(x, xlab = "x", ylab = "Double",..., LineCol = "red") {
  # xy standard plot
  graphics::plot(x$x, x$y, xlab = xlab, ylab = ylab,...)
  # Add the regression line
  graphics::lines(x$x, 2*x$x, col = LineCol)
}
