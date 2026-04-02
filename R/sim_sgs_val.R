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
#' @details
#' The optimal proportion of validated observations with `Ystar = 1` is
#' computed as
#' \deqn{
#' R_{opt} = \frac{p_Y + \mathrm{NPV} - 1}{\mathrm{PPV} + \mathrm{NPV} - 1},
#' }
#' where \eqn{p_Y} is the assumed outcome prevalence, `PPV` is the
#' positive predictive value, and `NPV` is the negative predictive value
#' of the surrogate. The function then samples validated observations
#' separately from the `Ystar = 1` and `Ystar = 0` strata according to
#' this allocation rule.
#'
#' @return Dataset with validation indicator `V_SGS` and partially
#' observed outcome `Y_SGS`.
#'
#' @examples
#' set.seed(206)
#' TPR = 0.4
#' TNR = 0.95
#' num_features <- 30
#' pX <- rexp(n = num_features, rate = 6) ## pX follows exponential
#' pX <- sort(pX) ## sort from least- to most-frequent features
#' betas <- c(-1.5, rep(x = c(-0.5, -0.75, 0.25), times = 7)[-21], rep(x = 1, times = 10))
#' data <- sim_data(N = 10000, betas = betas, beta_surr = 5, pX = pX, pYstar = 0.1) |> data.frame()
#' pY = mean(data$Y)
#' pYstar <-  0.12
#' PPV = TPR * pY / pYstar
#' NPV = TNR * (1 - pY) / (1 - pYstar)
#' data_sgs <- data |>
#' sim_sgs_val(n = 500,
#'             npv = NPV,
#'             ppv = PPV)
#' head(data_sgs)
#' @export
sim_sgs_val <- function(dat, n, npv, ppv, prop_Y = 0.5) {
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
  #message("Ropt = ", Ropt)
  dat$V_SGS = 1:nrow(dat) %in% ### Indicator of whether all row nums 1:N are contained within
    c(sample(x = which(dat$Ystar == 1),
             size = min(n * Ropt, length(which(dat$Ystar == 1)))),
      sample(x = which(dat$Ystar == 0),
             size = n - (min(n * Ropt, length(which(dat$Ystar == 1))))))
  dat$Y_SGS = dat$Y ### Initialize Y_SRS = Y for all N rows
  dat$Y_SGS[!dat$V_SGS] = NA ### Redact/make missing Y_SRS for (N - n) rows not chosen for validation
  return(dat)
}
