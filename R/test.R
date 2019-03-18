library(R.utils)
foo <- function() {
  print("Tic")
  for (kk in 1:100) {
    print(kk)
    Sys.sleep(0.1)
  }
  print("Tac")
}

res <- NULL
tryCatch({
  res <- withTimeout({
    x <- 5
    foo()
  }, timeout = 1.5)
}, TimeoutException = function(ex) {
  message("Timeout. Skipping.")
})

print(x)
