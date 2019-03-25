#' @title Check Internet Connectivity.
#'
#' @description Checking if user has Internet connectivity at the moment of execution to send results to the knowledge base / get recommendation from knowledge base.
#'
#' @return Boolean representing the Internet connectivity status.
#'
#' @examples
#' checkInternet().
#'
#' @importFrom RCurl getURL
#'
#' @noRd
#'
#' @keywords internal

checkInternet <- function() {
  out <- FALSE
  tryCatch({
    out <- is.character(getURL("www.yahoo.com"))
  },
  error = function(e) {
    out <- FALSE
  }
  )
  out
}
