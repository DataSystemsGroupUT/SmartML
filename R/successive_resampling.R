#' @importFrom KernSmooth dpik bkde
#' @importFrom tidyr drop_na separate gather spread unite
#' @importFrom dplyr select mutate_if arrange top_frac case_when mutate filter
#' @importFrom truncnorm rtruncnorm dtruncnorm

#' @keywords internal
dpikSafe <- function(x, ...)
{
  result <- try(dpik(x, ...), silent = TRUE)
  if (class(result) == "try-error")
  {
    msg <- geterrmessage()
    if (grepl("scale estimate is zero for input data", msg))
    {
      warning("Using standard deviation as scale estimate, probably because IQR == 0")
      result <- try(dpik(x, scalest = "stdev", ...), silent = TRUE	)
      if (class(result) == "try-error") {
        msg <- geterrmessage()
        if (grepl("scale estimate is zero for input data", msg)) {
          warning("0 scale, bandwidth estimation failed. using 1e-3")
          result <- 1e-3
        }
      }
    } else
    {
      stop(msg)
    }
  }
  return(result)
}

#' @keywords internal
successive_resampling <- function(df, model, samples = 64, n = 27, bw = 3, kde_type = "single") {
  samples_filtered <- df %>% drop_na()
  params_list <- jsons[[model]]$params
  length_params <- length(params_list)
  biggest_budget_that_satisfies <- samples_filtered %>%
    mutate(acc = as.numeric(acc)) %>%
    group_by(budget) %>%
    mutate(size = n()) %>%
    ungroup() %>%
    filter(size > ((length_params + 1) * 20/3)) %>%
    filter(budget == max(budget)) %>%
    arrange(desc(acc)) %>%
    select(-size) %>%
    separate(col = params,
             into = jsons[[model]]$params,
             sep = ",") %>%
    select(-model, -rp) %>%
    mutate_if(is.character, .funs = ~ str_extract(.x, pattern = "(?<==).*$") %>% parse_number)
  l_samples <- biggest_budget_that_satisfies %>%
    top_frac(0.15, wt = acc) %>%
    select(-acc, -budget)

  g_samples <- biggest_budget_that_satisfies %>%
    top_frac(-0.85, wt = acc) %>%
    select(-acc, -budget)

  l_kde_bws <- suppressWarnings(map_dbl(l_samples, dpikSafe))
  g_kde_bws <- suppressWarnings(map_dbl(g_samples, dpikSafe))
  l_kde_means <- map2_dbl(.x = l_samples, .y = l_kde_bws, .f = ~ mean(bkde(x = .x, bandwidth = .y)$x))
  g_kde_means <- map2_dbl(.x = g_samples, .y = g_kde_bws, .f = ~ mean(bkde(x = .x, bandwidth = .y)$x))
  maxvals <- map_dbl(.x = params_list, .f = ~ readr::parse_number(jsons[[model]][[.x]]$maxVal))
  minvals <- map_dbl(.x = params_list, .f = ~ readr::parse_number(jsons[[model]][[.x]]$minVal))
  types   <- map_chr(.x = params_list, .f = ~ jsons[[model]][[.x]]$scale)
  partial_rtruncnorm <- function(n, a, b, mu, sigma, type) {
    case_when(type == "int"    ~ round(rtruncnorm(n = n, a = a, b = b, mean = mu, sd = sigma)),
              type == "double" | type == "exp" ~ rtruncnorm(n = n, a = a, b = b, mean = mu, sd = sigma))
  }

  partial_dtruncnorm <- function(x, a, b, mu, sigma) {
    dtruncnorm(x = x, a = a, b = b, mean = mu, sd = sigma)
  }

  batch_samples <- pmap_dfc(.l = list("a" = minvals,
                                      "b" = maxvals,
                                      "mu" = l_kde_means,
                                      "sigma" = l_kde_bws * bw,
                                      "type" = types),
                            .f = partial_rtruncnorm,
                            n = samples) %>%
    set_names(nm = params_list)

  batch_samples_densities_l <- pmap_dfc(.l = list("x" = batch_samples,
                                                  "a" = minvals,
                                                  "b" = maxvals,
                                                  "mu" = l_kde_means,
                                                  "sigma" = l_kde_bws),
                                        .f = partial_dtruncnorm)

  batch_samples_densities_g <- pmap_dfc(.l = list("x" = batch_samples,
                                                  "a" = minvals,
                                                  "b" = maxvals,
                                                  "mu" = g_kde_means,
                                                  "sigma" = g_kde_bws),
                                        .f = partial_dtruncnorm)

  evaluate_batch_convolution <- batch_samples_densities_l / batch_samples_densities_g

  rank_sample_density <- function(samp, kdensity, n) {
    samp <- samp %>% as.data.frame()
    samp$rank <- kdensity
    sorted_samp <- samp %>% arrange(desc(rank)) %>% head(n)
    subset(sorted_samp, select = -rank)
  }

  if(kde_type == "mixed") {
    EI <- evaluate_batch_convolution %>%
      reduce(.f = `*`) %>%
      map_if(.p = ~ ((is.nan(.x) | is.infinite(.x)) == T),
             .f = ~ runif(1, min = 1e-5, max = 1e-3)) %>%
      flatten_dbl()

    batch_samples$rank <- EI

    evaluated_batch <- batch_samples %>%
      arrange(desc(rank)) %>%
      top_n(n = n, wt = rank)

    evaluated_batch_step_two <- evaluated_batch %>%
      select(-rank) %>%
      gather(key, value) %>%
      mutate(params = paste(key, value, sep = " = ")) %>%
      .[["params"]]

    eval_batch_step_three <- evaluated_batch_step_two %>%
      matrix(nrow = n, ncol = length(params_list)) %>%
      as.data.frame() %>%
      unite(col = "params", sep = ",") %>%
      mutate(model = model) %>%
      select(model, params)

    return(eval_batch_step_three)

  } else if(kde_type == "single") {
    evaluated_batch <- map2_dfc(.x = batch_samples, .y = evaluate_batch_convolution,
                                .f = rank_sample_density, n = n)

    colnames(evaluated_batch) <- params_list
    final_df <- evaluated_batch %>%
      gather(key, value) %>%
      mutate(params = paste(key, value, sep = " = ")) %>%
      .[["params"]] %>%
      matrix(nrow = n, ncol = length(params_list)) %>%
      as.data.frame() %>%
      unite(col = "params", sep = ",") %>%
      mutate(model = model) %>%
      select(model, params)

    return(final_df)
  }

}
