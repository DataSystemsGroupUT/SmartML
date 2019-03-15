#' @title Run smartML function for automatic Machine Learning.
#'
#' @description Run the smartML main function for automatic classifier algorithm selection, and hyper-parameter tuning.
#'
#' @param maxTime Float of the maximum time budget for hyper-parameter tuning process in minutes.
#' @param directory String of the training dataset directory (SmartML accepts file formats arff/(csv with columns headers) ).
#' @param classCol String of the name of the class label column in the dataset (default = 'class').
#' @param selectedFeats Vector of numbers of features columns to include from the training set and ignore the rest of columns - In case of empty vector, this means to include all features in the dataset file (default = c()).
#' @param vRatio Float of the validation set ratio that should be splitted out of the training set for the evaluation process (default = 0.1 --> 10%).
#' @param preProcessF string containing the name of the preprocessing algorithm (default = 'N' --> no preprocessing):
#' "boxcox": apply a Box–Cox transform and values must be non-zero and positive in all features,
#' "yeo-Johnson": apply a Yeo-Johnson transform, like a BoxCox, but values can be negative,
#' "zv": remove attributes with a zero variance (all the same value),
#' "center": subtract mean from values,
#' "scale": divide values by standard deviation,
#' "standardize": perform both centering and scaling,
#' "normalize": normalize values,
#' "pca": transform data to the principal components,
#' "ica": transform data to the independent components.
#' @param featuresToPreProcess Vector of number of features to perform the feature preprocessing on - In case of empty vector, this means to include all features in the dataset file (default = c()).
#' @param nComp Integer of Number of components needed if either "pca" or "ica" feature preprocessors are needed.
#' @param nModels Integer representing the number of best classifier algorithms that you want the tool to output.
#' @param options Integer representing either Classifier Algorithm Selection is needed only = 1 or Algorithm selection with its parameter tuning is required = 2 which is the default value.
#' @param featureTypes Vector of either 'numerical' or 'categorical' representing the types of features in the dataset (default = c() --> any factor or character features will be considered as categorical otherwise numerical).
#' @param interp Boolean representing if model interpretability (Feature Importance and Interaction) is needed or not (default = 0 --> No) This option will take more time budget if set to 1.
#'
#' @return List of Choosen parameter configurations for the \code{nModels} classifiers.
#'
#' @examples
#'
#' @export

autoRLearn <- function(maxTime, directory, classCol = 'class', selectedFeats = c(), vRatio = 0.1, preProcessF = 'N', featuresToPreProcess = c(), nComp = NA, nModels = 3, option = 2, featureTypes = c(), interp = 0) {
  library(tictoc)
  #Read Dataset
  dataset <- readDataset(directory, selectedFeats = selectedFeats, classCol = classCol, vRatio = vRatio, preProcessF = preProcessF, featuresToPreProcess = featuresToPreProcess, nComp = nComp)
  trainingSet <- dataset$TD
  #return()
  #Calculate Meta-Features for the dataset
  tic(quiet = TRUE)

  metaFeatures <- computeMetaFeatures(trainingSet, maxTime, featureTypes)

  #Convert Categorical to Numerical
  dataset <- convertCategorical(dataset)
  validationSet <- dataset$VD
  trainingSet <- dataset$TD
  foldedSet <- dataset$FD
  t <- toc(quiet = TRUE)
  #cat(sprintf("Time taken to extract Meta-Features %f\n", (t$toc-t$tic) ))

  #Generate candidate classifiers
  output <- getCandidateClassifiers(maxTime, metaFeatures, nModels)
  #return()
  algorithms <- output$c
  tRatio <- output$r
  algorithmsParams <- output$p
  #separator between models output
  separator <- '<hr/>'
  out <- c()
  #variables to hold best classifiers
  bestAlgorithm <- ''
  bestAlgorithmPerf <- 0
  bestAlgorithmParams <- list()
  #Only Candidate Classifiers
  if(option == 1){
    for(i in 1:length(algorithms)){
      classifierAlgorithm <- algorithms[i]
      cat('classifier Now: ', classifierAlgorithm, '\n')
      classifierConf <- getClassifierConf(classifierAlgorithm)
      classifierAlgorithmParams <- outClassifierConf(classifierAlgorithm, classifierConf, algorithmsParams[i])
      out <- paste(out, '<h4>Model(', as.character(i), '): <b>', classifierAlgorithm, '</h4><br/> Configuration: </b>', classifierAlgorithmParams, '<br/>', separator, '  <br/><br/>', collapse='')
    }
    return(out)
  }
  #loop over each classifier
  for(i in 1:length(algorithms)){
    classifierAlgorithm <- algorithms[i]
    classifierAlgorithmParams <- algorithmsParams[i]
    #Exception for deep Boost requires binary classes dataset
    if(classifierAlgorithm == 'deepboost' && metaFeatures$nClasses > 2)
      next
    #Read maxTime for the current classifier algorithm
    maxTime <- tRatio[i]
    #classifierAlgorithm <- 'svm'
    #Read the current classifier default parameter configuration
    classifierConf <- getClassifierConf(classifierAlgorithm)
    cat('\n\nClassifier Algorithm: ', classifierAlgorithm, '\n')
    #initialize step
    print('INITIALLIZE: ')
    #classifierAlgorithmParams <- 'linear#NA#-2#-13.3387946894724#NA'
    R <- initialize(classifierAlgorithm, trainingSet, validationSet, classifierConf, classifierAlgorithmParams)
    cntParams <- subset(R, select = -performance)
    #start hyperParameter tuning till maximum Time
    tic(quiet = TRUE)
    maxTime <- maxTime * 60
    timeTillNow <- 0
    #Regression Random Forest Trees for training set folds
    tree <- data.frame(fold=integer(), parent=integer(), params=character(), rightChild=integer(), leftChild=integer(), performance=double(), rowN = integer())
    bestParams <- cntParams
    bestPerf <- c()
    counter1 <- 1
    classifierFailureCounter <- 0

    repeat{
      #Fit Model
      print('FIT SMAC MODEL: ')
      output <- fitModel(bestParams, bestPerf, trainingSet, validationSet, foldedSet, classifierAlgorithm, tree)
      #Check if this classifer failed for more than 5 times, skip to the next classifier
      #cat("best performance: ", bestPerf, length(bestPerf), ' %%%%%%%%%%%%%%%%%%%%%%%%\n')
      if(length(bestPerf) > 0 && mean(bestPerf) == 0){
        classifierFailureCounter <- classifierFailureCounter + 1
        if(classifierFailureCounter > 5) break
      }

      tree <- output$t
      bestPerf <- output$p
      bestParams <- output$bp
      #Select Candidate Classifier Configurations
      print('GET CANDIDATE CONFIGURATIONS: ')
      candidateConfs <- selectConfiguration(R, classifierAlgorithm, tree, bestParams)
      #Intensify
      print('INTENSIFY: ')
      if(nrow(candidateConfs) > 0){
        print(timeTillNow)
        output <- intensify(R, bestParams, bestPerf, candidateConfs, foldedSet, trainingSet, validationSet, classifierAlgorithm, maxTime, timeTillNow)
        bestParams <- output$params
        bestPerf <- output$perf
        timeTillNow <- output$timeTillNow
        R <- output$r
      }
      #Check if execution time exceeded the allowed time or not
      t <- toc(quiet = TRUE)
      timeTillNow <- timeTillNow + t$toc - t$tic
      tic(quiet = TRUE)
      if(timeTillNow > maxTime){
        if(mean(bestPerf) > mean(bestAlgorithmPerf)){
          cat('cnt perf: ', bestPerf, '\n')
          cat('best perf: ', bestAlgorithmPerf, '\n')
          bestAlgorithmPerf <- bestPerf
          bestAlgorithm <- classifierAlgorithm
          bestAlgorithmParams <- bestParams
        }
        #Here we should add results from all models
        perf <- runClassifier(trainingSet = trainingSet, validationSet = validationSet, params = bestParams[,names(bestParams) != "EI" & names(bestParams) != "performance"], classifierAlgorithm = classifierAlgorithm)
        classifierConf <- getClassifierConf(classifierAlgorithm)
        classifierAlgorithmParams <- outClassifierConf(classifierAlgorithm, classifierConf, sprintf("'%s'", paste( unlist(bestParams[,names(bestParams) != "EI" & names(bestParams) != "performance"]), collapse='#')))
        out <- paste(out, '<h4>Model(', as.character(i), '): <b>', classifierAlgorithm, '</h4><br/>Validation Accuracy: </b>', as.character(perf*100), '%<br/><b>Configuration: </b>', classifierAlgorithmParams, '<br/>', separator, '  <br/><br/>', collapse='')
        break
      }
    }
  }
  cat('\n\n\n\n %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n')
  print('Best Classifier Algorithm')
  print(bestAlgorithm)
  print('classifier Parameters')
  bestAlgorithmParams <- bestAlgorithmParams[,names(bestAlgorithmParams) != "EI" & names(bestAlgorithmParams) != "performance"]
  print(bestAlgorithmParams)
  print('Dataset MetaFeatures')
  print(metaFeatures)

  #save results to Temporary File
  tmp <- runClassifier(trainingSet = trainingSet, validationSet = validationSet, params = bestAlgorithmParams, classifierAlgorithm = bestAlgorithm, interp = interp)
  finalResult <- list()

  if(interp == 0)
    finalResult$perf <- tmp
  else
    finalResult <- tmp

  finalResult$out <- out
  cat('VALIDATION SET ACCURACY: ', finalResult$perf, '\n')
  print('SAVE RESULTS TO TMP')
  sendToTmp(metaFeatures, bestAlgorithm, bestAlgorithmParams, finalResult$perf)
  print('Return Output to UI')
  results <- list(meta = metaFeatures, bestA = bestAlgorithm, bestP = bestAlgorithmParams, bestPerf = finalResult$perf)

  #check internet connection and send data in tmp file to database if connection exists
  return(finalResult$out)

  if(checkInternet() == TRUE){
    print("SEND TO DATABASE")
    sendToDatabase()
  }
}
