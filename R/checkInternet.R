#' @title Check Internet Connectivity.
#'
#' @description Checking if user has Internet connectivity at the moment of execution to send results to the knowledge base / get recommendation from knowledge base.
#'
#' @return Boolean representing the Internet connectivity status.
#'
#' @examples
#' checkInternet().
#'
#' @noRd
#'
#' @keywords internal

checkInternet <- function() {
  out <- FALSE
  tryCatch({
    library(RCurl)
    out <- is.character(getURL("www.yahoo.com"))
  },
  error = function(e) {
    out <- FALSE
  }
  )
  out
}
