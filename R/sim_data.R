#' Create complete data (without simulation)
#'
#' Simulate a complete dataset with binary predictors (X), surrogate variable (Ystar) and outcome (Y).
#'
#' @param N Dataset size
#' @param betas The beta coefficients to generate Y from X using logistic regression
#' @param beta_surr The beta for Ystar in the logistic regression to generate Y
#' @param pX Marginal feature frequencies (was generated to follow an exponential distribution, and should be in decreasing order, according to the paper by Tan and Heagerty (2019))
#' @param pYstar Marginal surrogate variable frequency
#'
#' @details
#' In Tan and Heagerty (2019), the conditional outcome was generated as
#' \deqn{
#' Y_i \mid (Z_{1i}, Z_{2i}, \tilde{X}_i^T) \sim \mathrm{Bernoulli}(P(Y_i = 1)),
#' }
#' where
#' \deqn{
#' \mathrm{logit}\left(E[Y_i \mid Z_{1i}, Z_{2i}, \tilde{X}_i]\right)
#' = \beta_0 + \beta_{z1} Z_{1i} + \beta_{z2} Z_{2i}
#' + \sum_{j=1}^p \beta_j X_{ij}.
#' }
#' For the first 20 most frequent features,
#' \deqn{
#' \beta_j = (-0.75, -0.5, 0.25, \ldots, -0.5, 0.25).
#' }
#' For the 10 features with frequencies closest to the outcome prevalence,
#' \eqn{\beta_j = 1}, and for the remaining features, \eqn{\beta_j = 0}.
#' This function implements a simplified version of that setup using a
#' surrogate variable `Ystar` and binary predictors `X`.
#'
#' @return A complete dataset containing predictors, linear predictor,
#' outcome, and surrogate variable (data frame)
#'
#' @examples
#' num_features <- 30
#' pX <- rexp(n = num_features, rate = 6) ## pX follows exponential
#' pX <- sort(pX, decreasing = TRUE) ## sort from least- to most-frequent features
#' betas <- c(-1.5, rep(x = c(-0.5, -0.75, 0.25), times = 7)[-21], rep(x = 1, times = 10))
#' data <- sim_data(N = 10000, betas = betas, beta_surr = 3, pX = pX, pYstar = 0.1) |> data.frame()
#' head(data)
#'
#' @export
sim_data <- function(N, betas, beta_surr, pX, pYstar) {
  num_features = length(pX)
  pX = sort(pX, decreasing = TRUE)
  ## Matrix of binary predictors
  Xmat = matrix(data = NA, nrow = N, ncol = num_features)
  ## Binary predictor
  for (i in 1:num_features) {
    Xmat[, i] = stats::rbinom(n = N,
                       size = 1, ### 1 trial --> Bernoulli
                       prob = pX[i])
  }
  colnames(Xmat) = paste0("X", 1:num_features)

  ## Surrogate binary outcome
  ### To simulate the data such that the surrogate outcome has certain TNR and TNR, modify the betas and pYstar
  ### P(Y*=1|Y=1) = Sensitivity (True Positive Rate)
  ### P(Y*=1|Y=0) = 1 - P(Y*=0|Y=0) = 1 - Specificity (False Positive Rate)
  Ystar <- stats::rbinom(n = N,
                  size = 1,
                  prob = pYstar)

  ## Binary outcome
  mu = cbind(Int = 1, Xmat) %*% matrix(data = betas, ncol = 1) + beta_surr * Ystar
  Y <- stats::rbinom(n = N,
              size = 1, ### 1 trial --> Bernoulli
              prob = expit(z = mu)) ### P(Y = 1|X) = expit(betas %*% Xmat)

  ## Return the complete (fully validated/extracted) dataset
  return(cbind(Xmat, mu = mu, Y, Ystar))
}
