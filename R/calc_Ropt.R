#' Create surrogate-guided sample (SGS)
#'
#' Create a surrogate-guided validation sample from a complete dataset
#' using the surrogate variable `Ystar`.
#'
#' @param dat Complete dataset containing `Y` and `Ystar`.
#' @param n Validation sample size.
#' @param npv Negative predictive value of the surrogate.
#' @param ppv Positive predictive value of the surrogate.
#' @param prop_Y Target or assumed outcome prevalence. Default is `0.5`.
#'
#'
#' @return Vector of two scalars optimal proportion to validate from surrogate cases (with and without truncation)
#'
#' @export
calc_Ropt <- function(dat, n, npv, ppv, prop_Y = 0.5) {
  ### Check
  if (!all(c("Y", "Ystar") %in% names(dat))) {
    stop("`dat` must contain columns `Y` and `Ystar`.")
  }
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n <= 0) {
    stop("`n` must be a positive number.")
  }
  n <- as.integer(round(n))
  if (!is.numeric(ppv) || length(ppv) != 1 || is.na(ppv) || ppv < 0 || ppv > 1) {
    stop("`ppv` must be a single number in [0, 1].")
  }
  if (!is.numeric(npv) || length(npv) != 1 || is.na(npv) || npv < 0 || npv > 1) {
    stop("`npv` must be a single number in [0, 1].")
  }
  if (!is.numeric(prop_Y) || length(prop_Y) != 1 || is.na(prop_Y) ||
      prop_Y < 0 || prop_Y > 1) {
    stop("`prop_Y` must be a single number in [0, 1].")
  }

  denom <- ppv + npv - 1
  num <- prop_Y + npv - 1

  if (abs(denom) < 1e-6) {
    stop("`ppv + npv - 1` is too close to 0, so `Ropt` is undefined.")
  }

  ### optimal proportion sampled with Ystar = 1
  Ropt = num / denom
  if (!is.finite(Ropt)) {
    stop("Ropt is not finite. Check ppv, npv, and prop_Y.")
  }
  Ropt0 <- Ropt
  if (Ropt < 0 || Ropt > 1) {
    warning(
      paste0(
        "`Ropt` = ", round(Ropt, 4), " is outside [0, 1]. ",
        "This combination of `ppv`, `npv`, and `prop_Y` is not feasible."
      )
    )
    if (Ropt < 0) Ropt <- 0 ## if Ropt < 0 force to be 0
    if (Ropt > 1) Ropt <- 1 ## if Ropt > 1 force to be 1
  }
  return(c(Ropt0, Ropt))
}
