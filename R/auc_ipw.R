#' Inverse probability weighted AUROC
#'
#' Computes an inverse probability weighted estimate of the area under the
#' receiver operating characteristic curve (AUROC) for partially validated
#' outcome data.
#'
#' @param pis Numeric vector of validation probabilities for the observed
#' outcomes.
#' @param probs Numeric vector of predicted probabilities or risk scores.
#' @param y Binary outcome vector for validated observations.
#'
#' @details
#' The estimator compares all case-control pairs and weights each pair by
#' the inverse product of their validation probabilities. This allows
#' AUROC estimation under outcome-dependent validation designs such as
#' surrogate-guided sampling.
#'
#' @return A numeric scalar giving the inverse probability weighted AUROC.
#'
#' @examples
#' set.seed(206)
#' num_features <- 30
#' pX <- rexp(n = num_features, rate = 6) ## pX follows exponential
#' pX <- sort(pX, decreasing = TRUE) ## sort from least- to most-frequent features
#' betas <- c(-1.5, rep(x = c(-0.5, -0.75, 0.25), times = 7)[-21], rep(x = 1, times = 10))
#' data <- sim_data(N = 10000, betas = betas, beta_surr = 3, pX = pX, pYstar = 0.1) |> data.frame()
#' PPV <- mean(data$Y[data$Ystar == 1] == 1)
#' NPV <- mean(data$Y[data$Ystar == 0] == 0)
#' data_sgs <- data |>
#' sim_sgs_val(n = 500,
#'             npv = NPV,
#'             ppv = PPV)
#' data_sgs = data_sgs |>
#'   dplyr::group_by(Ystar) |>
#'   dplyr::mutate(pi = sum(!is.na(Y_SGS)) / dplyr::n())
#' ipw_fit = glm(formula = V_SGS ~ Ystar, family = "binomial", data = data_sgs)
#' ipw_pi = predict(object = ipw_fit,
#'                 newdata = data_sgs[data_sgs$V_SGS, ],
#'                 type = "response")
#' sgs_formula <- as.formula(paste0("Y_SGS~Ystar+",
#'                           paste(paste0("X", 1:num_features),
#'                           collapse = "+")))
#' sgs_cc_fit <- glm(formula = sgs_formula, family = "binomial", data = data_sgs)
#' sgs_pred_probs <- predict(object = sgs_cc_fit, ### pooled fitted model
#'                           type = "response") ### to predict probabilities
#' auc_ipw(pis = ipw_pi, probs = sgs_pred_probs, y = data_sgs$Y_SGS[data_sgs$V_SGS])
#' @export
auc_ipw = function(pis, probs, y) {
  pair_pi = expand.grid(pi1 = pis,
                        pi2 = pis)
  pair_phat = expand.grid(phat1 = probs,
                          phat2 = probs)
  pair_Y = expand.grid(Y1 = y,
                       Y2 = y)

  num = sum(pair_pi[, 1] ^ (- 1) * pair_pi[, 2] ^ (- 1) *
              as.numeric(pair_phat[, 1] > pair_phat[, 2]) *
              as.numeric(pair_Y[, 1] > pair_Y[, 2]))
  denom = sum(pair_pi[, 1] ^ (- 1) * pair_pi[, 2] ^ (- 1) *
                as.numeric(pair_Y[, 1] > pair_Y[, 2]))

  return(num / denom)
}
