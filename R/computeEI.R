#' @title Compute Expected Improvement
#' @description Compute Expected Improvement of a candidate algorithm
#' @param cmin minimum error rate found till now
#' @param perf error rate on each tree of the SMAC random Forest
#' @seealso \code{\link[utils]{head}}
#' @examples \dontrun{ computeEI(cmin, perf)
#' }
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
