#' @import nloptr
#' @import bbotk

#' @keywords internal

ensembling = function(best_models, df_train, df_test,
                       problem = 'classification', measure = 'classif.acc'){
  lrns = c()
  for(i in 1:nrow(best_models)){
    lrns = c(lrns, po('learner_cv', best_models[[1]][[i]],
                      id = paste('lrn', as.character(i), sep='') ))
  }

  level0 = gunion(list(
    lrns))  %>>%
    po("featureunion", id = "union1")

  if(problem == 'classification'){
    problem = 'classif'
    ensemble = level0 %>>% LearnerClassifAvg$new(id = "classif.avg")
    task = TaskClassif$new(id = 'final_eval', backend = df_train, target = 'class')
  }
  else{
    problem = 'regr'
    ensemble = level0 %>>% LearnerRegrAvg$new(id = "regr.avg")
    task = TaskRegr$new(id = 'final_eval', backend = df_train, target = 'class')
  }

  ens_lrn = GraphLearner$new(ensemble)
  ens_lrn$predict_type = "prob"
  ens_lrn$train(task)
  perf <- ens_lrn$predict_newdata(df_test)$score(msr(measure))
  return (list("model" = ens_lrn, "performance" = perf))
}
