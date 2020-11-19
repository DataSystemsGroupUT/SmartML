#' @importFrom data.table fcase
#' @import purrr

#' @keywords internal

param_sample <- function(model, hparam, columns = NULL) {
  param = jsons[[model]][[hparam]]
  type <- param$type
  type_scale <- param$scale

  if(type == "boolean") {
    param_estimation <- paste(base::sample(x = as.list(param$values), size = 1), sep = "")
    param_estimation <- ifelse(param_estimation == "FALSE", FALSE, TRUE)
    return(param_estimation)
  }
  else if(type == "discrete") {
    param_estimation <- paste(base::sample(x = as.list(param$values), size = 1), sep = "")
    return(param_estimation)
  }

  else {
    int_val <- ifelse(hparam == "mtry", as.numeric(columns) - 1, as.numeric(param$maxVal))
    param_estimation <- fcase(type_scale == "int", rdunif(1, a = as.numeric(param$minVal),
                                                          b = int_val),
                              type_scale == "any", runif(1,  min = as.numeric(param$minVal),
                                                         max = as.numeric(param$maxVal)),
                              type_scale == "double", runif(1,  min = as.numeric(param$minVal),
                                                            max = as.numeric(param$maxVal)),
                              type_scale == "exp", 2^rdunif(1,  a = as.numeric(param$minVal),
                                                            b = as.numeric(param$maxVal)))
    return(as.numeric(param_estimation))
  }

}

#' @keywords internal
get_random_hp_config <- function(model, columns = NULL) {
  param_db <- jsons[[model]]
  params_list <- param_db$params
  params_list_mapped <- map(.x = params_list,
                            .f = as_mapper( ~ param_sample(model = model,
                                                           hparam = .x,
                                                           columns = columns)))
  `names<-`(params_list_mapped, params_list)
}

#' @keywords internal
calc_n_r = function(max_iter = 81, eta = 3, s = 4, B = 405) {
  n = trunc(ceiling(trunc(B/max_iter/(s+1)) * eta**s))
  r = max_iter * eta^(-s)
  ans = c(n, r)
  ans
}
