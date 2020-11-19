#' @export evocate
evocate <- function(df_train, df_test, maxTime = 1, models = "xgboost",
                    optimizationAlgorithm = "hyperband", bw = 3, max_iter = 81, kde_type = "single",
                    problem = "classification", measure = "classif.acc", ensemble_size = 1) {

  total_time <- maxTime * 60
  parameters_per_model <- map_int(models, .f = ~ length(jsons[[.x]]$params))
  times <- (parameters_per_model * total_time) / (sum(parameters_per_model))

  cat("Models selected:", models, '\n', sep = ' ')
  cat("Time distribution:", times, '\n', sep = ' ')

  run_optimization <- function(model, time) {
    results <- NULL
    priors <- data.frame()
    tic(model, "optimization time:")

    if(optimizationAlgorithm == 'hyperband') {
      current <- Sys.time() %>% as.integer()
      end <- (Sys.time() %>% as.integer()) + time

      repeat {
        gc(verbose = F)
        tic('current hyperband runtime')
        print(paste('Started', model, ' model...'))
        # Compute the time left for this model
        time_left <- max(end - (Sys.time() %>% as.integer()), 1)
        print(paste("There are:", time_left, "seconds left for this hyperband run"))
        res <- hyperband(df = df_train, model = model, max_iter = max_iter,
                         maxtime = time_left, problem = problem, measure = measure)

        if(is_empty(flatten(res)) == F) {
          res <- res %>%
            map_dfr(.f = ~ .x[["answer"]]) %>%
            arrange(desc(acc)) %>%
            head(1)
          results <- c(list(res), results)
          print(paste('Best performance from hyperband this round: ', res$acc))
        }
        # Break if the remaining time exceeds the allowed time budget
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
        res <- bohb(df = df_train, model = model, bw = bw, max_iter = max_iter,
                    maxtime = time_left, priors = priors, kde_type = kde_type)

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

  print("Starting to run all optimizations.")
  ans <- vector(mode = "list", length = length(models))

  for(i in 1:length(models)) {
    flag <- TRUE
    tryCatch({
      ans[[i]] <- run_optimization(models[[i]], times[[i]])
    }, error = function(e) {
      cat('Error spotted: ')
      message(e)
      cat(' In ', models[[i]], ' model, going to the next model!\n')
      flag <<- FALSE
    })
    if (!flag) next
  }

  # Arrange Results according to the best performance
  ensemble_size <- min(max(1, length(ans[[1]])), ensemble_size)
  print(ensemble_size)
  tryCatch({best_model <- ans %>%
    map(.f = ~ map_dfr(.x = .x, .f = ~ .x %>% select(model, acc))) %>%
    map_dfr(.f = ~ .x %>% arrange(desc(acc)) %>% head(ensemble_size)) %>%
    arrange(desc(acc))
  print('----------------------####------------------------')
  # Return the best performing model
  results <- ensembling(best_model, df_train, df_test, problem = problem, measure = measure)
  return (results)
  }, error = function(e){
    cat('Error spotted: ')
    message(e)
    cat('Try increasing the time budget or use a different model.\n')
    return (-1)
  })

}
