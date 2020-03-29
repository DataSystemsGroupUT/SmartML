#' @title Fit a classifier model.
#'
#' @description Run the classifier on a training set and measure performance on a validation set.
#'
#' @param trainingSet Dataframe of the training set.
#' @param validationSet Dataframe of the validation Set.
#' @param params A string character of parameter configuration values for the current classifier to be tuned (parameters are separated by #) and can be obtained from \code{params} out of resulted list after running \code{autoRLearn} function.
#' @param classifierAlgorithm String character of the name of classifier algorithm used now.
#' \itemize{
#' \item "svm" - Support Vector Machines from e1071 package,
#' \item "naiveBayes" - naiveBayes from e1071 package,
#' \item "randomForest" - randomForest from randomForest package,
#' \item "lmt" -  LMT Weka classifier trees from RWeka package,
#' \item "lda" -  Linear Discriminant Analysis from MASS package,
#' \item "j48" - J48 Weka classifier Trees from RWeka package,
#' \item "bagging" - Bagging Classfier from ipred package,
#' \item "knn" - K nearest Neighbors from FNN package,
#' \item "nnet" - Simple neural net from nnet package,
#' \item "C50" - C50 decision tree from C5.0 pacakge,
#' \item "rpart" - rpart decision tree from rpart package,
#' \item "rda" - regularized discriminant analysis from klaR package,
#' \item "plsda" - Partial Least Squares And Sparse Partial Least Squares Discriminant Analysis from caret package,
#' \item "glm" - Fitting Generalized Linear Models from stats package,
#' \item "deepboost" - deep boost classifier from deepboost package.
#' }
#' @param metric Metric string character to be used in evaluation:
#' \itemize{
#' \item "acc" - Accuracy,
#' \item "avg-fscore" - Average of F-Score of each label,
#' \item "avg-recall" - Average of Recall of each label,
#' \item "avg-precision" - Average of Precision of each label,
#' \item "fscore" - Micro-Average of F-Score of each label,
#' \item "recall" - Micro-Average of Recall of each label,
#' \item "precision" - Micro-Average of Precision of each label
#' }
#' @param interp Boolean representing if interpretability is required or not (Default = 0).
#'
#' @return List of performance on validationSet named \code{perf}, model fitted on trainingSet named \code{m}, predictions on test set \code{pred}, and interpretability plots named \code{interpret} in case of interp = 1
#'
#' @examples
#' \dontrun{
#' result1 <- autoRLearn(10, 'sampleDatasets/shuttle/train.arff', 'sampleDatasets/shuttle/test.arff')
#' dataset <- datasetReader('/Datasets/irisTrain.csv', '/Datasets/irisTest.csv')
#' result2 <- runClassifier(dataset$Train, dataset$Test, result1$params, result1$clfs)
#' }
#'
#' @importFrom  e1071 svm naiveBayes
#' @importFrom  randomForest randomForest
#' @importFrom  FNN knn
#' @importFrom  ipred bagging
#' @importFrom  RWeka J48 LMT
#' @importFrom  C50 C5.0 C5.0Control
#' @importFrom  rpart rpart rpart.control
#' @importFrom  MASS lda
#' @importFrom  klaR rda
#' @importFrom  caret plsda
#' @importFrom  stats glm predict
#' @importFrom  nnet nnet
#' @importFrom  deepboost deepboost deepboost.predict
#' @importFrom  utils capture.output
#' @importFrom  mda fda mars bruto gen.ridge polyreg
#' @importFrom  fastNaiveBayes fnb.train
#' @importFrom  ranger ranger
#' @importFrom  xgboost xgboost xgb.DMatrix
#' @importFrom  LiblineaR LiblineaR
#' @export runClassifier
runClassifier <- function(trainingSet, validationSet, params, classifierAlgorithm, metric = "acc", interp = 0) {

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

 invisible(capture.output(
 capture.output(
 possibleError <- try({
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
        else if(classifierAlgorithm == 'plsda'){
          params$ncomp <- as.numeric(params$ncomp)
          model <- plsda(xFeatures, as.factor(xClass), ncomp = params$ncomp, probMethod = params$probMethod, type = "class")
          pred <- predict(model, yFeatures)
        }
        else if(classifierAlgorithm == 'knn'){
          params <- as.numeric(params)
          pred <- do.call(knn,c(list(train = xFeatures, test = yFeatures, cl=xClass), params))
        }
        else if(classifierAlgorithm == 'l2-linear-classifier'){
          params$cost <- (2^as.numeric(params$cost))
          params$epsilon <- as.numeric(params$epsilon)
          model <- LiblineaR(target = as.factor(xClass), data = xFeatures, cost = params$cost, epsilon = params$epsilon, type = 2)
          pred <- predict(model, yFeatures)$predictions
        }
        else if(classifierAlgorithm == 'neuralnet'){
          params <- as.numeric(params)
          learn <- cbind(xClass, xFeatures)
          model <- nnet(as.factor(xClass) ~., data = learn, size = params)
          pred <- predict(model, yFeatures)

        }
        else if(classifierAlgorithm == 'naiveBayes'){
          if(!exists('eps', where = params)) {
            params$laplace <- as.numeric(params$laplace)

            model <- fnb.train(x = xFeatures, y = as.factor(xClass), laplace = params$laplace)
          #   if(class(model) == "try-error") {
          #
          #   msg <- geterrmessage()
          #   if(grepl("Not enough rows. Should be at least 2 rows or more for each class", msg)) {
          #
          #       warning("Fast Naive Bayes failed! trying slow Naive Bayes.")
          #       model <- e1071::naiveBayes(as.factor(xClass) ~ ., data = cbind(xClass, xFeatures), laplace = params$laplace)
          #
          #     }
          #   }
          }
          if(exists('eps', where = params)) {

            params$laplace <- as.numeric(params$laplace)
            params$eps <- (2 ^ as.numeric(params$eps))
            learn <- cbind(xClass, xFeatures)
            model <- naiveBayes(as.factor(xClass) ~., data = learn, laplace = params$laplace, eps = params$eps)

          }

          #print(pred)

          pred <- predict(model, yFeatures)

        }

        else if(classifierAlgorithm == 'part'){
          params$C <- as.numeric(params$C)
          params$M <- as.numeric(params$M)
          learn <- cbind(xClass, xFeatures)
          model <- PART(as.factor(xClass) ~., data = learn, control = Weka_control(C = params$C, M = params$M, B = params$B))
          pred <- predict(model, yFeatures)
        }
        else if(classifierAlgorithm == 'lda'){
          params$tol <- (2 ^ as.numeric(params$tol))
          learn <- cbind(xClass, xFeatures)
          invisible(capture.output(suppressWarnings(model <- lda(as.factor(xClass) ~., data = learn, tol = params$tol, method = params$method))))
          pred <- predict(model, yFeatures)$class
        }
        else if(classifierAlgorithm == 'rpart'){
          params$minsplit <- as.numeric(params$minsplit)
          params$maxdepth <- as.numeric(params$maxdepth)
          params$xval <- as.numeric(params$xval)
          params$cp <- (2^ as.numeric(params$cp))
          learn <- cbind(xClass, xFeatures)
          model <- rpart(as.factor(xClass) ~., data = learn, control = rpart.control(minsplit = params$minsplit, maxdepth = params$maxdepth, xval = params$xval, cp = params$cp) )
          pred <- predict(model, yFeatures, type = "class")
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
        else if(classifierAlgorithm == 'fda'){
          learn <- cbind(xClass, xFeatures)
          params$dimension <- as.numeric(params$dimension)
          params$dimension <- min(params$dimension, length(unique(xClass))-1)
          if (params$method == 'mars')m <- mda::mars
          else if(params$method == 'bruto') m <- mda::bruto
          else if(params$method == 'gen.ridge') m <- mda::gen.ridge
          else m <- mda::polyreg
          model <- fda(as.factor(xClass) ~., data = learn, dimension = params$dimension, method = m)
          pred <- predict(model, yFeatures)
        }
        else if(classifierAlgorithm == 'j48'){
          params$C <- as.numeric(params$C)
          params$M <- as.numeric(params$M)
          learn <- cbind(xClass, xFeatures)
          model <- J48(as.factor(xClass) ~., data = learn, control = Weka_control(C = params$C, M = params$M, B = params$B) )
          pred <- predict(model, yFeatures)
        }
        else if(classifierAlgorithm == 'lmt'){
          params$M <- as.numeric(params$M)
          learn <- cbind(xClass, xFeatures)
          model <- LMT(as.factor(xClass) ~., data = learn, control = Weka_control(M = params$M) )
          pred <- predict(model, yFeatures)
        }
        else if(classifierAlgorithm == 'c50'){
          params$CF <- as.numeric(params$CF)
          params$minCases <- as.numeric(params$minCases)
          params$trials = as.numeric(params$trials)
          learn <- cbind(xClass, xFeatures)
          model <- C5.0(as.factor(xClass) ~., data = learn, trials = params$trials, control = C5.0Control(CF = params$CF, earlyStopping = params$earlyStopping, fuzzyThreshold = params$fuzzyThreshold, minCases = params$minCases))
          pred <- predict(model, yFeatures)
        }
        else if(classifierAlgorithm == 'bagging'){
          params$nbagg <- as.numeric(params$nbagg)
          params$xval <- as.numeric(params$xval)
          params$minsplit <- as.numeric(params$minsplit)
          params$cp <- as.numeric(params$cp)
          params$cp <- (2^ params$cp)
          params$maxdepth <- as.numeric(params$maxdepth)
          learn <- cbind(xClass, xFeatures)
          model <- bagging(as.factor(xClass) ~., data = learn, nbagg = params$nbagg, control = rpart.control(minsplit = params$minsplit, cp = params$cp, xval = params$xval, maxdepth = params$maxdepth) )
          pred <- predict(model, yFeatures, type = "class")
        }
        else if(classifierAlgorithm == 'deepboost'){
          learn <- cbind(xClass, xFeatures)
          params$num_iter <- as.numeric(params$num_iter)
          params$tree_depth <- as.numeric(params$tree_depth)
          params$beta <- as.numeric(params$beta)
          params$lambda <- as.numeric(params$lambda)
          model <- deepboost(as.factor(xClass) ~., data = learn, num_iter = params$num_iter, beta = params$beta, lambda = params$lambda, loss_type = params$loss_type, verbose = FALSE)
          pred <- deepboost.predict(model, yFeatures)
        }
        else if(classifierAlgorithm == 'rda'){
          params$gamma <- as.numeric(params$gamma)
          params$lambda <- as.numeric(params$lambda)
          learn <- cbind(xClass, xFeatures)
          model <- rda(as.factor(xClass) ~., data = learn, gamma = params$gamma, lambda = params$lambda)
          pred <- predict(model, yFeatures)$class
        }
        else if(classifierAlgorithm == 'glm'){
          learn <- cbind(xClass, xFeatures)
          model <- glm(xClass ~., data = learn, family = params$family)
          pred <- predict(model, yFeatures, type="terms")
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
  }, silent = T))))
  if(inherits(possibleError, "try-error")){

  if(classifierAlgorithm == "naiveBayes") {

  } else if(classifierAlgorithm != "naiveBayes") {

  print('Warning: Failed Run with current classifier.. Trying another parameter configuration.')

  }
  return(list(perf = 0))
  }
  result <- list()
  result$perf <- perf

  if(interp == 1 && classifierAlgorithm != 'knn'){ #no interpretability available for knn
    result$interpret <- interpret(model, validationSet)
  }
  else if(classifierAlgorithm == 'knn') #no model available for knn
    model <- 'knn'

  result$model <- model
  result$pred <- pred

  return(result)
}
