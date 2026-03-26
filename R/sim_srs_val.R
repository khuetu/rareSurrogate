#' Create Simple Random Sample (SRS)
#'
#' Create SRS sample from a complete dataset.
#'
#' @param dat The complete data to generate SRS samples from
#' @param n Sample size
#'
#' @return Dataset with validation indicator `V_SRS` and partially
#' observed outcome `Y_SRS`.
#'
#' @examples
#' num_features <- 30
#' pX <- rexp(n = num_features, rate = 6) ## pX follows exponential
#' betas <- c(-1.5, rep(x = c(-0.5, -0.75, 0.25), times = 7)[-21], rep(x = 1, times = 10))
#' data <- sim_data(N = 10000, betas = betas, beta_surr = 3, pX = pX, pYstar = 0.1) |> data.frame()
#'
#' data_srs <- data |> sim_srs_val(n = 500)
#' head(data_srs)
#'
#' @export
sim_srs_val <- function(dat, n) {
  dat$V_SRS = 1:nrow(dat) %in% ### Indicator of whether all row nums 1:N are contained within
    sample(x = 1:nrow(dat), ### Vector of n chosen for validation
           size = n, ### Phase II sample size
           replace = FALSE)
  dat$Y_SRS = dat$Y ### Initialize Y_SRS = Y for all N rows
  dat$Y_SRS[!dat$V_SRS] = NA ### Redact/make missing Y_SRS for (N - n) rows not chosen for validation
  return(dat)
}
