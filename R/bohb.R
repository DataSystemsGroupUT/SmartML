#' @importFrom dplyr distinct n group_by

#' @keywords internal
bohb <- function(df, model, max_iter = 81, eta = 3, bw = 3, random_frac = 1/3, maxtime, priors = data.frame(), kde_type = "single") {

  logeta = as_mapper(~ log(.x) / log(eta))

  s_max = trunc(logeta(max_iter))

  B = (s_max + 1) * max_iter

  nrs = map_dfc(s_max:0, .f = ~ calc_n_r(max_iter, eta, .x, B)) %>%
    t() %>%
    `colnames<-`(value = c("n", "r")) %>%
    as_tibble()

  nrs$s = s_max:0

  length_params <- length(jsons[[model]]$params)

  tryCatch(expr = {withTimeout(expr = {

    liszt = vector(mode = "list",
                   length = max(nrs$s) + 1)

    runs_df <- NULL

    current_sh_run <- NULL


  for (row in 1:nrow(nrs)) {

   if(row == 1) {

      print(paste("Iteration number", row))

      #print(paste("n = ", nrs[[row, 1]], " r = ", nrs[[row, 2]], " s_max = ", nrs[[row, 3]], sep = ""))

      current_sh_run <- successive_halving(df = df,
                                          params_config = sample_n_params(n = nrs[[row, 1]], model = model),
                                          n = nrs[[row, 1]],
                                          r = nrs[[row, 2]],
                                          s_max = nrs[[row, 3]],
                                          max_iter = max_iter,
                                          eta = eta,
                                          evaluations = priors)


      runs_df <- runs_df %>%
        bind_rows(current_sh_run$sh_runs)

      liszt[[row]] <- current_sh_run

      next

    }

   else if(row > 1){

    bayesian_opt_samples <- successive_resampling(df = runs_df,
                                                  model = model,
                                                  samples = max_iter,
                                                  n = round(max(nrs[[row, 1]] * (1 - random_frac), 1)),
                                                  bw = bw,
                                                  kde_type = kde_type)

    current_sh_run <- successive_halving(df = df,
                                        params_config = bayesian_opt_samples %>%
                                          bind_rows(sample_n_params(n = round(max(nrs[[row, 1]] * random_frac, 1)), model = model)),
                                        n = nrs[[row, 1]],
                                        r = nrs[[row, 2]],
                                        s_max = nrs[[row, 3]],
                                        max_iter = max_iter,
                                        eta = eta)

   }

    runs_df <- runs_df %>%
             bind_rows(current_sh_run$sh_runs)

    liszt[[row]] <- current_sh_run

  }

  }, timeout = maxtime, cpu = maxtime)},

  TimeoutException = function(ex) {

    print("Budget ended.")

    return(liszt)

  },

  finally = function(ex) {

    print("BOHB successfully finished.")

    return(liszt) }
  ,

  error = function(ex) {

    print(paste("Error found, replace ", model, sep = ""))

    print(geterrmessage())

    break

  })

  return(liszt)

}
