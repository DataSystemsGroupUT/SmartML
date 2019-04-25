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
#' @importFrom  deepboost deepboost
#' @importFrom  utils capture.output
#' @importFrom  mda fda mars bruto gen.ridge polyreg
#'
#' @export runClassifier

runClassifier <- function(trainingSet, validationSet, params, classifierAlgorithm, metric = "acc", interp = 0) {

  #training set features and classes
  xFeatures <- subset(trainingSet, select = -class)
  xClass <- c(subset(trainingSet, select = class)$'class')
  #validation set features and classes
  yFeatures <- subset(validationSet, select = -class)
  yClass <- c(subset(validationSet, select = class)$'class')
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
  possibleError <- try(
    {
      #build model
      if(classifierAlgorithm == 'svm'){
        if(exists('gamma', where=params) && !is.na(params$gamma))
          params$gamma <- (2^ as.double(params$gamma))
        if(exists('cost', where=params) && !is.na(params$cost))
          params$cost <- (2^ as.double(params$cost))
        if(exists('tolerance', where=params) && !is.na(params$tolerance))
          params$tolerance <- (2^ as.double(params$tolerance))
        model <- do.call(svm,c(list(x = xFeatures, y = xClass, type = 'C-classification'), params))
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

      else if(classifierAlgorithm == 'neuralnet'){
        params <- as.numeric(params)
        learn <- cbind(xClass, xFeatures)
        model <- nnet(as.factor(xClass) ~., data = learn, size = params)
        pred <- predict(model, yFeatures, type = "class")
      }
      else if(classifierAlgorithm == 'naiveBayes'){
        params$laplace <- as.numeric(params$laplace)
        params$eps <- (2 ^ as.numeric(params$eps))
        learn <- cbind(xClass, xFeatures)
        model <- naiveBayes(as.factor(xClass) ~., data = learn, laplace = params$laplace, eps = params$eps)
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
        pred <- predict(model, yFeatures)
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
      else if(classifierAlgorithm == 'randomForest'){
        params$mtry <- as.numeric(params$mtry)
        params$ntree <- as.numeric(params$ntree)
        params$mtry <- min(params$mtry, ncol(xFeatures))
        model <- do.call(randomForest,c(list(x = xFeatures, y = as.factor(xClass)), params))
        pred <- predict(model, yFeatures)
      }
      perf <- evaluateMet(yClass, pred, metric = metric)
    }))
  )
  if(inherits(possibleError, "try-error")){
    print('Warning: Failed Run with current classifier.. Trying another paramter configuration.')
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
