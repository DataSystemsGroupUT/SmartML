#' @importFrom data.table fcase
#' @import purrr
#' @importFrom dplyr mutate_all

#' @keywords internal
param_sample <- function(param){

  type <- param$type

  type_scale <- param$scale

  if(type == "discrete") {

    param_estimation <- paste("'", base::sample(x = as.list(param$values), size = 1), "'", sep = "")

    param_estimation

  }

  else {

    param_estimation <- fcase(type_scale == "int", rdunif(1, a = as.numeric(param$minVal),
                                                          b = as.numeric(param$maxVal)),
                              type_scale == "any", runif(1,  min = as.numeric(param$minVal),
                                                         max = as.numeric(param$maxVal)),
                              type_scale == "double", runif(1,  min = as.numeric(param$minVal),
                                                            max = as.numeric(param$maxVal)),
                              type_scale == "exp", runif(1,  min = as.numeric(param$minVal),
                                                         max = as.numeric(param$maxVal)))

    param_estimation
  }

  param_estimation

}

#' @keywords internal
get_random_hp_config <- function(params) {

  params_list <- params$params

  params_list_mapped <- map(.x = params_list,
                            .f = as_mapper( ~ param_sample(params[[.x]])))

  `names<-`(params_list_mapped, params_list)

}

#' @keywords internal
make_paste_final <- function(model) {

  params_list <- get_random_hp_config(jsons[[model]])

  names_list <- names(params_list) %>%
    map(~ str_glue(.x, " = ")) %>%
    map2(params_list, ~paste(.x, .y, sep = "")) %>%
    paste(collapse = ",")

  names_list

}

#' @keywords internal
sample_n_params <- function(n, model) {

  ans <- map_chr(.x = rep(model, n), .f = make_paste_final) %>%
    data.frame(model = model,
               params = .) %>%
    mutate_all(.funs = as.character)

  ans

}

#' @keywords internal
eval_loss <- function(model, train_df, test_df, params) {

  params_list <- eval(parse(text = paste("list(", params, ")")))

  pred <- runClassifier_(trainingSet = train_df,
                        validationSet = test_df,
                        params = params_list,
                        classifierAlgorithm = model)

}

#' @keywords internal
calc_n_r = function(max_iter = 81, eta = 3, s = 4, B = 405) {

  n = trunc(ceiling(trunc(B/max_iter/(s+1)) * eta**s))

  r = max_iter * eta^(-s)

  ans = c(n, r)

  ans

}
