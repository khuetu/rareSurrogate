#' Inverse logit transformation
#'
#' Computes the inverse logit of a numeric input.
#'
#' @param z Numeric vector.
#'
#' @return Numeric vector with values in \eqn{(0,1)}.
#'
#' @examples
#' expit(c(-1, 0, 1))
#'
#' @export
expit <- function(z) {
  1 / (1 + exp(-z))
}
