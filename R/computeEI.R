#' @title Compute Expected Improvement.
#'
#' @description Compute the expected improvement for the suggested parameter configurations of a specific classifier.
#'
#' @param cmin Minimum error rate achieved till now.
#' @param perf Expected Performance of the current configuration on each tree of the forest of SMAC algorithm.
#' @param B number of trees in the forest of trees of SMAC optimization algorithm (default = 10).
#'
#' @return Float Number of Expected Improvement value.
#'
#' @examples
#' computeEI(0.9, c(0.91, 0.95, 0.89, 0.88, 0.93), 5).
#'
#' @noRd
#'
#' @keywords internal

computeEI <- function(cmin, perf, B = 10){
  for(i in 1:B){
    perf[i] <- 1 - perf[i]
  }
  perfMean <- mean(perf)
  perfStdDev <- sqrt(var(perf))
  u <- (cmin - perfMean)/perfStdDev
  cdf <- pnorm(u, mean=0, sd=1)
  pdf <- dnorm(u, mean=0, sd=1)
  EI <- perfStdDev * (u * cdf + pdf)
  return (EI)
}
