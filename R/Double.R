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

