#' @keywords internal hyperband
hyperband <- function(df, model, max_iter = 81, eta = 3, maxtime = 100) {

  logeta = as_mapper(~ log(.x) / log(eta))

  s_max = trunc(logeta(max_iter))

  B = (s_max + 1) * max_iter

  nrs = map_dfc(s_max:0, .f = ~ calc_n_r(max_iter, eta, .x, B)) %>%
    t() %>%
    `colnames<-`(value = c("n", "r")) %>%
    as_tibble()

  nrs$s = s_max:0

  partial_halving <- function(n, r, s) {

    successive_halving(df = df,
                       params_config = sample_n_params(n = n, model = model),
                       n = n,
                       r = r,
                       s_max = s,
                       max_iter = max_iter,
                       eta = eta)

  }

  tryCatch(expr = {withTimeout(expr = {

    liszt = vector(mode = "list",
                   length = max(nrs$s) + 1)

    for (row in 1:nrow(nrs)) {

      liszt[[row]] <- partial_halving(nrs[[row, 1]],
                                      nrs[[row, 2]],
                                      nrs[[row, 3]])

    }
  }, timeout = maxtime, cpu = maxtime)},

  TimeoutException = function(ex) {

    print("Budget ended.")

    return(liszt)

  },

  finally = function(ex) {

    print("Hyperband successfully finished.")

    return(liszt) }
  ,

  error = function(ex) {

    print(paste("Error found, replace ", model, sep = ""))

    print(geterrmessage())

    break

  })

  return(liszt)

}
