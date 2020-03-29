#' @keywords internal
runClassifier_ <- function(trainingSet, validationSet, params, classifierAlgorithm, metric = "acc") {

  #training set features and classes
  xFeatures <- subset(trainingSet, select = -class)
  xClass <- c(subset(trainingSet, select = class)$'class')

  #print(levels(xClass))

  #validation set features and classes
  yFeatures <- subset(validationSet, select = -class)
  yClass <- c(subset(validationSet, select = class)$'class')

  #print(levels(yClass))

  #remove not available parameters
  if(typeof(params) == 'character'){
    classifierConf <- getClassifierConf(classifierAlgorithm)
    params <- initialize(classifierAlgorithm, classifierConf, params)
  }
  for(i in colnames(params)){
    if(is.na(params[[i]]) || params[[i]] == 'NA' || params[[i]] == 'EI'){
      params <- subset(params, select = -get(i))
    }
  }
        # build model
        if(classifierAlgorithm == 'svm'){
          if(exists('gamma', where=params) && !is.na(params$gamma))
            params$gamma <- (2^ as.double(params$gamma))
          if(exists('cost', where=params) && !is.na(params$cost))
            params$cost <- (2^ as.double(params$cost))
          if(exists('tolerance', where=params) && !is.na(params$tolerance))
            params$tolerance <- (2^ as.double(params$tolerance))
          if(!exists('kernel', where = params))
            params$kernel <- 'radial'
          invisible(capture.output(suppressWarnings(model <- do.call(svm,c(list(x = xFeatures, y = xClass, type = 'C-classification', scale = F), params)))))
          #check performance
          pred <- predict(model, yFeatures)
        }
        else if(classifierAlgorithm == 'l2-linear-classifier'){
          params$cost <- (2^as.numeric(params$cost))
          params$epsilon <- as.numeric(params$epsilon)
          model <- LiblineaR(target = as.factor(xClass), data = xFeatures, cost = params$cost, epsilon = params$epsilon, type = 2)
          pred <- predict(model, yFeatures)$predictions
        }
        else if(classifierAlgorithm == 'naiveBayes'){
          if(!exists('eps', where = params)) {
            params$laplace <- as.numeric(params$laplace)

            model <- fnb.train(x = xFeatures, y = as.factor(xClass), laplace = params$laplace)
          }
          if(exists('eps', where = params)) {

            params$laplace <- as.numeric(params$laplace)
            params$eps <- (2 ^ as.numeric(params$eps))
            learn <- cbind(xClass, xFeatures)
            model <- naiveBayes(as.factor(xClass) ~., data = learn, laplace = params$laplace, eps = params$eps)

          }

          pred <- predict(model, yFeatures)

        }
        else if(classifierAlgorithm == 'boosting'){
          params$eta <- (2^as.numeric(params$eta))
          params$max_depth <- as.numeric(params$max_depth)
          params$min_child_weight <- as.numeric(params$min_child_weight)
          params$gamma <- as.numeric(params$gamma)
          params$colsample_bytree <- as.numeric(params$colsample_bytree)

          xClass_dmat <- xClass %>% as.numeric() %>% map(.f = ~ .x - 1)
          xFeatures_dmat <- xFeatures %>% as.matrix()
          mode(xFeatures_dmat) = 'double'
          yFeatures_dmat <- yFeatures %>% as.matrix()
          mode(yFeatures_dmat) = 'double'

          learn <- xgb.DMatrix(data = xFeatures_dmat, label = xClass_dmat)
          model <- xgboost(data = learn,
                           nrounds = 5,
                           eta = params$eta,
                           max_depth = params$max_depth,
                           min_child_weight = params$min_child_weight,
                           gamma = params$gamma,
                           colsample_bytree = params$colsample_bytree,
                           objective = "multi:softprob",
                           num_class = length(unique(xClass_dmat)),
                           verbose = 0,
                           nthread = 1)

          pred_prep <- predict(model, yFeatures_dmat, nthreads = 1)

          pred_mat <- matrix(pred_prep, ncol = length(unique(xClass_dmat)), byrow = T)

          colnames(pred_mat) <- levels(trainingSet$class)

          pred <- apply(pred_mat, 1, function(x) colnames(pred_mat)[which.max(x)])

          levels(pred) <- levels(trainingSet$class)

        }
        else if(classifierAlgorithm == 'ranger'){
          params$max.depth <- as.numeric(params$max.depth)
          params$num.trees <- as.numeric(params$num.trees)
          params$mtry <- min(as.numeric(params$mtry), ncol(xFeatures))
          params$min.node.size <- as.numeric(params$min.node.size)
          learn <- cbind(xClass, xFeatures)
          model <- ranger(as.factor(xClass) ~ .,
                          data = learn,
                          max.depth = params$max.depth,
                          num.trees = params$num.trees,
                          mtry = params$mtry,
                          min.node.size = params$min.node.size,
                          num.threads = 1)
          pred <- predict(model, yFeatures, num.threads = 1)$prediction
        }
        else if(classifierAlgorithm == 'randomForest'){
          params$mtry <- as.numeric(params$mtry)
          params$ntree <- as.numeric(params$ntree)
          params$mtry <- min(params$mtry, ncol(xFeatures))
          model <- do.call(randomForest,c(list(x = xFeatures, y = as.factor(xClass)), params))
          pred <- predict(model, yFeatures)
        }
        if (classifierAlgorithm != 'boosting') {

          perf <- evaluateMet(yClass, pred, metric = metric)

        }
        else {

          perf <- evaluateMet(validationSet$class, pred %>% factor(levels = levels(validationSet$class)), metric = metric)

        }

  result <- list()
  result$perf <- perf

  result$model <- model
  result$pred <- pred

  return(result)
}
