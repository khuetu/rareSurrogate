#' Grid search for parameters given target TNR and FPR (or TNR)
#'
#' Performs a grid search over candidate values of the intercept,
#' surrogate coefficient, and surrogate prevalence to identify settings
#' that produce simulated data with target outcome prevalence, true
#' positive rate (TPR), and false positive rate (FPR).
#'
#' @param N Data size
#' @param grid A data frame of candidate parameter combinations. Must
#' contain the columns `beta0`, `beta_surr`, and `pYstar`.
#' @param targets A named numeric vector of target values for
#' `prevalence`, `TPR`, and `FPR`.
#' @param betas_fixed Fixed coefficients in the logistic regression to model Y
#' @param pX Marginal feature frequencies (should be in decreasing order)
#'
#' @return A data frame containing the candidate parameter values, the
#' corresponding simulated performance measures, and loss values
#' comparing each candidate to the target metrics. Smaller loss values
#' indicate closer agreement with the targets.
#'
#' @importFrom dplyr rowwise mutate select arrange ungroup
#' @importFrom stats rbinom
#'
#' @examples
#' grid <- expand.grid(
#'   beta0      = seq(-4, 0, by = 1),
#'   beta_surr  = seq(-6, 6, by = 1),
#'   pYstar = seq(0.1, 0.95, by = 1)
#' )
#' targets <- c(prevalence = 0.1, TPR = 0.4, FPR = 0.95)
#' betas_fixed <- c(rep(x = c(-0.5, -0.75, 0.25), times = 7)[-21], rep(x = 1, times = 10))
#' pX    <- rexp(n = 30, rate = 6)
#' pX <- sort(pX, decreasing = TRUE) ## sort from least- to most-frequent features
#' results <- betas_gridsearch(N = 1000, grid, targets, betas_fixed, pX)
#' head(results, 10)
#' @export
betas_gridsearch <- function(N, grid, targets, betas_fixed, pX) {
  ## POSSIBLE IMPROVEMENT: SEED
  num_features <- length(pX)
  pX <- sort(pX, decreasing = TRUE) ## sort from least- to most-frequent features
  simulate_metrics <- function(N, beta0, beta_surr, pYstar) {
    # Generate Xmat exactly like your sim_data
    Xmat <- matrix(NA, nrow = N, ncol = num_features)
    for (i in 1:num_features) {
      Xmat[, i] <- rbinom(n = N, size = 1, prob = pX[i])
    }
    colnames(Xmat) <- paste0("X", 1:num_features)

    Ystar <- rbinom(n = N, size = 1, prob = pYstar)

    betas <- c(beta0, betas_fixed)
    mu    <- cbind(Int = 1, Xmat) %*% matrix(betas, ncol = 1) + beta_surr * Ystar
    Y     <- rbinom(n = N,
                    size = 1,
                    prob = expit(mu))

    prevalence <- mean(Y)
    TPR        <- mean(Ystar[Y == 1] == 1)
    FPR        <- mean(Ystar[Y == 0] == 1)

    c(prevalence = prevalence, TPR = TPR, FPR = FPR)
  }

  # Run grid
  results <- grid |>
    dplyr::rowwise() |>
    dplyr::mutate(
      metrics = list(simulate_metrics(N, .data$beta0, .data$beta_surr, .data$pYstar)),
      prevalence = .data$metrics["prevalence"],
      TPR = .data$metrics["TPR"],
      FPR = .data$metrics["FPR"]
    ) |>
    dplyr::select(-"metrics") |>
    dplyr::ungroup()

  results <- results |>
    dplyr::mutate(
      loss = (.data$prevalence - targets["prevalence"])^2 +
        (.data$TPR - targets["TPR"])^2 +
        (.data$FPR - targets["FPR"])^2,
      abs_loss = abs(.data$prevalence - targets["prevalence"]) +
        abs(.data$TPR - targets["TPR"]) +
        abs(.data$FPR - targets["FPR"])
    ) |>
    dplyr::arrange(.data$loss)

  return(results)
}
