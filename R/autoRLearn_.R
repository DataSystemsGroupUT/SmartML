#' @title Advanced version of autoRLearn.
#'
#' @description Tunes the hyperparameters of the desired algorithm/s using either hyperband or BOHB.
#'
#' @param df_train Dataframe of the training dataset. Assumes it is in perfect shape with all numeric variables and factor response variable named "class".
#' @param df_test Dataframe of the test dataset. Assumes it is in perfect shape with all numeric variables and factor response variable named "class".
#' @param maxTime Float representing the maximum time the algorithm should be run (seconds).
#' @param models List of strings denoting which algorithms to use for the process:
#' \itemize{
#' \item "randomForest" - Random forests using the randomForest package
#' \item "ranger - Random forests using the ranger package (unstable)
#' \item "naiveBayes" - Naive bayes using the fastNaiveBayes package
#' \item "boosting" - Gradient boosting using xgboost
#' \item "l2-linear-classifier" - Linear primal Support vector machine from LibLinear
#' \item "svm" - RBF kernel svm from e1071
#' }
#' @param optimizationAlgorithm - String of which hyperparameter tuning algorithm to use:
#' \itemize{
#' \item "hyperband" - Hyperband with uniformly initiated parameters
#' \item "bohb" - Hyperband with bayesian optimization as described on F. Hutter et al 2018 paper BOHB. Has extra parameters bw and kde_type
#' }
#' @param bw - (only applies to BOHB) Double representing how much should the KDE bandwidth be widened. Higher values allow the algorithm to explore more hyperparameter combinations
#' @param max_iter - (affects both hyperband and BOHB) Integer representing the maximum number of iterations that one successive halving run can have
#' @param kde_type - (only applies to BOHB) String representing whether a model's hyperparameters should be tuned individually of each other or have their probability densities multiplied:
#' \itemize{
#' \item "single" - each hyperparameter has its own expected improvement calculated
#' \item "mixed" - all hyperparameters' probabilty densities are multiplied and only one mixed expected improvement is calculated
#' }
#' @return List of Results
#' \itemize{
#' \item \code{perf} - accuracy of the best performing model on the test data
#' \item \code{pred} - prediction on the test data using the best model
#' \item \code{model} - best model object
#' \item \code{best_models} - table with the best hyperparameters found for the selected models.
#' }

#' @importFrom R.utils withTimeout
#' @importFrom tictoc tic toc

#' @export autoRLearn_
autoRLearn_ <- function(df_train, df_test, maxTime = 10, models = c("randomForest", "naiveBayes", "boosting", "l2-linear-classifier", "svm"), optimizationAlgorithm = "hyperband", bw = 3, max_iter = 81, kde_type = "single") {

  total_time = maxTime * 60

  parameters_per_model <- map_int(models, .f = ~ length(jsons[[.x]]$params))

  times = (parameters_per_model * total_time) / (sum(parameters_per_model))

  print("Time distribution:")
  print(times)
  print("Models selected:")
  print(models)

  run_optimization = function(model, time) {

  results = NULL

  priors = data.frame()

  tic(model, "optimization time:")

    if(optimizationAlgorithm == "hyperband") {

      current <- Sys.time() %>% as.integer()

      end <- (Sys.time() %>% as.integer()) + time

      repeat {

      gc(verbose = F)

      tic("current hyperband runtime")

      print(paste("started", model))

      time_left <- max(end - (Sys.time() %>% as.integer()), 1)

      print(paste("There are:", time_left, "seconds left for this hyperband run"))

      res <- hyperband(df = df_train, model = model, max_iter = max_iter, maxtime = time_left)

      if(is_empty(flatten(res)) == F) {

        res <- res %>%
          map_dfr(.f = ~ .x[["answer"]]) %>%
          arrange(desc(acc)) %>%
          head(1)

        results <- c(list(res), results)

        print(paste('Best accuracy from hyperband this round: ', res$acc))

      }

      elapsed <- (Sys.time() %>% as.integer()) - current

      if(elapsed >= time) {

         break

      }

      }

    }

    else if(optimizationAlgorithm == "bohb") {

        current <- Sys.time() %>% as.integer()

        end <- (Sys.time() %>% as.integer()) + time

        repeat {

          gc(verbose = F)

          tic("current bohb time")

          print(paste("started", model))

          time_left <- max(end - (Sys.time() %>% as.integer()), 1)

          print(paste("There are:", time_left, "seconds left for this bohb run"))

          res <- bohb(df = df_train, model = model, bw = bw, max_iter = max_iter, maxtime = time_left, priors = priors, kde_type = kde_type)

          if(is_empty(flatten(res)) == F) {

          priors <- res %>%
              map_dfr(.f = ~ .x[["sh_runs"]])

          res <- res %>%
              map_dfr(.f = ~ .x[["answer"]]) %>%
              arrange(desc(acc)) %>%
              head(1)

            results <- c(list(res), results)

            print(paste('Best accuracy from hyperband this round: ', res$acc))

          }

          elapsed <- (Sys.time() %>% as.integer()) - current

          if(elapsed >= time) {

            break

          }

      }


    }

    else {

      errorCondition(message = "Only hyperband and bohb are valid optimization algorithms at this moment.")

      break

    }

    toc()

  results

  }

  print("Finished all optimizations.")

  ans = vector(mode = "list", length = length(models))


  for(i in 1:length(models)) {

    flag <- TRUE

    tryCatch(expr = {

    ans[[i]] <- run_optimization(models[[i]], times[[i]])

    }, error = function(e) {

      print("Error spotted, going to the next model")

      flag <<- FALSE

    })

    if (!flag) next

  }

  ans = ans %>%
    map(.f = ~ map_dfr(.x = .x, .f = ~ .x %>% select(model, params, acc))) %>%
    map_dfr(.f = ~ .x %>% arrange(desc(acc)) %>% head(1)) %>%
    arrange(desc(acc))

  best_model <- ans %>% head(1)

  final_evaluation <- eval_loss(model = best_model[["model"]], train_df = df_train, test_df = df_test, params = best_model[["params"]])

  final_evaluation$best_models <- ans

  print(paste("Winner:", best_model$model, "test accuracy:", final_evaluation$perf))

  final_evaluation

}
