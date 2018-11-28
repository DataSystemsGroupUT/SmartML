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
