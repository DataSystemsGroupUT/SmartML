#' @keywords internal
#'
successive_halving <- function(df, model, params_config, n = 81, r = 1, eta = 3,
                               max_iter = 81, s_max = 5, evaluations = data.frame(),
                               problem = 'classification', measure = 'classif.acc') {

  final_df = params_config
  print('GOT HERE 0')
  if(problem == 'classification'){
    problem = 'classif'
    task = TaskClassif$new(id = 'sh', backend = df, target = 'class')
  }
  else{
    problem = 'regr'
    task = TaskRegr$new(id = 'sh', backend = df, target = 'class')
  }
  param_number = length(params_config)

  for (k in 0:s_max) {
    gc()
    n_i = n * (eta ** -k)
    r_i = r * (eta ** k)
    r_p = r_i / max_iter
    min_train_datapoints = (length(unique(df$class)) * 3) + 1
    min_prob_datapoints = min_train_datapoints / nrow(df$class)
    train_idxs <- sample(task$nrow, task$nrow * max(min(r_p, 0.8), min_prob_datapoints))
    test_idxs <- setdiff(seq_len(task$nrow), train_idxs)
    if (problem == 'classif')
      learners <- replicate(n = n_i, expr = {lrn(paste(problem, sep = '.', model),
                                                 predict_type = 'prob')})
    else
      learners <- replicate(n = n_i, expr = {lrn(paste(problem, sep = '.', model))})

    print('GOT HERE 1')
    j = 1
    for (i in learners) {
      cnt_field <- final_df[[j]]
      ## Some conditions to filter the parameter values
      if (model == 'svm' && final_df[[j]]$kernel != 'polynomial')
        cnt_field$degree <- NULL
      if ( (model == 'svm' && final_df[[j]]$kernel == 'linear') || (model == 'cv_glmnet' && final_df[[j]]$relax == FALSE))
        cnt_field$gamma <- NULL

      i$param_set$values = cnt_field
      j = j + 1
    }

    print('GOT HERE 2')
    for (l in learners) {
      l$train(task = task, row_ids = train_idxs)
    }

    print('GOT HERE 3')
    preds <- map(.x = learners, .f = ~ .x$predict(task, row_ids = test_idxs)$score(msr(measure)))


    final_df <- final_df %>%
      as.data.table() %>%
      t() %>%
      `colnames<-`(value = jsons[[model]]$params) %>%
      as.data.table()


    final_df[, acc := unlist(preds)]
    final_df[, budget := r_i]
    final_df[, budget := r_p]
    final_df[, model := unlist(learners)]
    setorder(final_df, -acc)
    evaluations <- rbindlist(list(evaluations, final_df))


    final_df <- final_df %>%
      head(max(n_i/eta, 1))


    if(k == s_max){
      return(list("answer" = final_df, "sh_runs" = evaluations))
    }

    final_df$acc = NULL
    final_df$budget = NULL
    final_df$model = NULL
    final_df <- purrr::transpose(final_df)

  }
}
