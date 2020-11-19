#' @keywords internal hyperband
hyperband <- function(df, model, max_iter = 81, eta = 3, maxtime = 1000,
                      problem = 'classification', measure = 'classif.acc') {
  logeta = as_mapper(~ log(.x) / log(eta))
  s_max = trunc(logeta(max_iter))
  B = (s_max + 1) * max_iter
  nrs = map_dfc(s_max:0, .f = ~ calc_n_r(max_iter, eta, .x, B)) %>%
    t() %>%
    `colnames<-`(value = c("n", "r")) %>%
    as.data.table()
  nrs$s = s_max:0
  partial_halving <- function(n, r, s) {
    successive_halving(df = df, model = model,
                       params_config = replicate(n, get_random_hp_config(model, columns = ncol(df) - 1),
                                                 simplify = FALSE),
                       n = n, r = r, s_max = s, max_iter = max_iter, eta = eta,
                       problem = problem, measure = measure)
  }

  liszt = vector(mode = "list", length = max(nrs$s) + 1)
  if (model != 'ranger'){
    tryCatch({tmp <- withTimeout({
      for (row in 1:nrow(nrs)) {
          liszt[[row]] <- partial_halving(nrs[[row, 1]],
                                                  nrs[[row, 2]],
                                                  nrs[[row, 3]])
        print("Looped once")
      }
    }, timeout = maxtime, elapsed = maxtime)
    }, TimeoutException = function(ex) {
      err <- geterrmessage()
      if (startsWith(err, 'reached') == FALSE)
        print(paste('Error Found, ', err, ' Replace ', model, sep = ''))
      else
        print("Time Budget ended.")
    },
    finally = function(ex) {
      print("Hyperband successfully finished.")
    })
  }
  else{
    current <- Sys.time() %>% as.integer()
    for (row in 1:nrow(nrs)) {
      tryCatch({liszt[[row]] <- partial_halving(nrs[[row, 1]],
                                      nrs[[row, 2]],
                                      nrs[[row, 3]])
      }, Exception = function(ex) {
        err <- geterrmessage()
        print(paste('Error Found, ', err, ' Replace ', model, sep = ''))
      })
      now <- Sys.time() %>% as.integer()
      if ((now - current) > maxtime){
        print("Time Budget ended.")
        break
      }
      print("Looped once")
    }
  }
  return(liszt)
}
