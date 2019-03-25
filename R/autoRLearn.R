#' @title Run smartML function for automatic Machine Learning.
#'
#' @description Run the smartML main function for automatic classifier algorithm selection, and hyper-parameter tuning.
#'
#' @param maxTime Float of the maximum time budget for reading dataset, preprocessing, calculating meta-features, Algorithm Selection & hyper-parameter tuning process only in minutes(Excluding Model Interpretability) - This is applicable in case of Option = 2 only.
#' @param directory String of the training dataset directory (SmartML accepts file formats arff/(csv with columns headers) ).
#' @param testDirectory String of the testing dataset directory (SmartML accepts file formats arff/(csv with columns headers) ).
#' @param classCol String of the name of the class label column in the dataset (default = 'class').
#' @param selectedFeats Vector of numbers of features columns to include from the training set and ignore the rest of columns - In case of empty vector, this means to include all features in the dataset file (default = c()).
#' @param vRatio Float of the validation set ratio that should be splitted out of the training set for the evaluation process (default = 0.1 --> 10\%).
#' @param preProcessF string containing the name of the preprocessing algorithm (default = 'N' --> no preprocessing):
#' \itemize{
#' \item "boxcox" - apply a Box–Cox transform and values must be non-zero and positive in all features,
#' \item "yeo-Johnson" - apply a Yeo-Johnson transform, like a BoxCox, but values can be negative,
#' \item "zv" - remove attributes with a zero variance (all the same value),
#' \item "center" - subtract mean from values,
#' \item "scale" - divide values by standard deviation,
#' \item "standardize" - perform both centering and scaling,
#' \item "normalize" - normalize values,
#' \item "pca" - transform data to the principal components,
#' \item "ica" - transform data to the independent components.
#' }
#' @param featuresToPreProcess Vector of number of features to perform the feature preprocessing on - In case of empty vector, this means to include all features in the dataset file (default = c()) - This vector should be a subset of \code{selectedFeats}.
#' @param nComp Integer of Number of components needed if either "pca" or "ica" feature preprocessors are needed.
#' @param nModels Integer representing the number of classifier algorithms that you want to select based on Meta-Learning and start to tune using Bayesian Optimization (default = 3).
#' @param option Integer representing either Classifier Algorithm Selection is needed only = 1 or Algorithm selection with its parameter tuning is required = 2 which is the default value.
#' @param featureTypes Vector of either 'numerical' or 'categorical' representing the types of features in the dataset (default = c() --> any factor or character features will be considered as categorical otherwise numerical).
#' @param interp Boolean representing if model interpretability (Feature Importance and Interaction) is needed or not (default = 0) This option will take more time budget if set to 1.
#' @param missingVal Vector of strings representing the missing values in dataset (default: c('NA', '?', ' ')).
#' @param missingOpr Boolean variable represents either delete instances with missing values or apply imputation using "MICE" library which helps you imputing missing values with plausible data values that are drawn from a distribution specifically designed for each missing datapoint- (default = 0 --> delete instances).
#'
#' @return List of Results
#' \itemize{
#' \item "option=1" - Choosen Classifier Algorithms Names \code{clfs} with their parameters configurations \code{params} in case of \code{option=2},
#' \item "option=2" - Best classifier algorithm name found \code{clfs} with its parameters configuration \code{params}, model variable \code{model}, and performance on TestingSet \code{perf}.
#' }
#'
#' @examples
#' \dontrun{
#' autoRLearn(1, 'sampleDatasets/car/train.arff', \
#' 'sampleDatasets/car/test.arff', option = 2, preProcessF = 'normalize')
#'
#' result <- autoRLearn(10, 'sampleDatasets/shuttle/train.arff', 'sampleDatasets/shuttle/test.arff')
#' }
#'
#' @importFrom tictoc tic toc
#' @importFrom R.utils withTimeout
#'
#' @export autoRLearn

autoRLearn <- function(maxTime, directory, testDirectory, classCol = 'class', selectedFeats = c(), vRatio = 0.1, preProcessF = 'N', featuresToPreProcess = c(), nComp = NA, nModels = 3, option = 2, featureTypes = c(), interp = 0, missingVal = c('NA', '?', ' '), missingOpr = 0) {
  #Read Dataset
  datasetReadError <- try(
  {
    #Read Training Dataset
    dataset <- readDataset(directory, testDirectory, selectedFeats = selectedFeats, classCol = classCol, vRatio = vRatio, preProcessF = preProcessF, featuresToPreProcess = featuresToPreProcess, nComp = nComp, missingVal = missingVal, missingOpr = missingOpr)
    trainingSet <- dataset$TD
    #Read Testing Dataset
    testDataset <- dataset$TED
    #Read all training Dataset without validation
    trainDataset <- dataset$FULLTD
  })
  if(inherits(datasetReadError, "try-error")){
    print('Failed Reading Dataset: Make sure that dataset directory is correct and it is a valid csv/arff file.')
    return(-1)
  }

  #Calculate Meta-Features for the dataset
  metaFeaturesError <- try(
  {
    metaFeatures <- computeMetaFeatures(trainingSet, maxTime, featureTypes)
  })
  if(inherits(metaFeaturesError, "try-error")){
    print('Failed Extracting Dataset MetaFeatures.')
    return(-1)
  }

  #Convert Categorical Features to Numerical Ones and split the dataset
  B <- 12
  #B <- max(10, int((metaFeatures$nInstances) / 1000)) #Number of folds to work on for the dataset and trees in SMAC forest model
  splitError <- try(
  {
    dataset <- convertCategorical(dataset, trainDataset, testDataset, B = B)
    validationSet <- dataset$VD #Validation set
    trainingSet <- dataset$TD #Training Set
    foldedSet <- dataset$FD #Folded sets of Training Data.
    #Convert for all TrainingSet
    trainDataset <- dataset$FULLTD
    #Convert for all TestingSet
    testDataset <- dataset$TED
  })
  if(inherits(splitError, "try-error")){
    print('Failed Splitting Dataset.')
    return(-1)
  }

  #Generate candidate classifiers
  candidateClfsError <- try(
  {
    nClassifiers <- 15
    output <- getCandidateClassifiers(maxTime, metaFeatures, min(c(nModels, nClassifiers)) )
    algorithms <- output$c #Classifier Algorithm names selected.
    tRatio <- output$r #Time ratio between all classifiers.
    algorithmsParams <- output$p #Initial Parameter configuration of each classifier.
  })
  if(inherits(candidateClfsError, "try-error")){
    print('Failed Generating Candidate Classifiers.')
    return(-1)
  }

  tryCatch({
    #Option 1: Only Candidate Classifiers with initial parameters will be resulted (No Hyper-parameter tuning)
    if(option == 1)
      return (list(Clfs = algorithms, params = algorithmsParams))

    #Option 2: Classifier Algorithm Selection + Parameter Tuning
    res <- withTimeout({
      #variables to hold best classifiers
      bestAlgorithm <- '' #bestClassifierName.
      bestAlgorithmPerf <- 0 #bestClassifierPerformance.
      bestAlgorithmParams <- list() #Parameters of best Classifier.

      #loop over each classifier
      for(i in 1:length(algorithms)){
        classifierAlgorithm <- algorithms[i]
        classifierAlgorithmParams <- algorithmsParams[i]

        #Read maxTime for the current classifier algorithm and convert to seconds
        maxClfTime <- tRatio[i] * 60
        #Read the current classifier default parameter configuration
        classifierConf <- getClassifierConf(classifierAlgorithm)
        cat('\n\nStart Tuning Classifier Algorithm: ', classifierAlgorithm, '\n')
        #initialize step
        R <- initialize(classifierAlgorithm, trainingSet, validationSet, classifierConf, classifierAlgorithmParams)
        cntParams <- R[, -which(names(R) == "performance")]
        #start hyperParameter tuning till maximum Time
        tic(quiet = TRUE)
        timeTillNow <- 0
        #Regression Random Forest Trees for training set folds
        tree <- data.frame(fold=integer(), parent=integer(), params=character(), rightChild=integer(), leftChild=integer(), performance=double(), rowN = integer())
        bestParams <- cntParams
        bestPerf <- c()
        counter1 <- 1
        classifierFailureCounter <- 0

        repeat{
          #Fit Model
          output <- fitModel(bestParams, bestPerf, trainingSet, validationSet, foldedSet, classifierAlgorithm, tree, B = B)
          #Check if this classifer failed for more than 5 times, skip to the next classifier
          if(length(bestPerf) > 0 && mean(bestPerf) == 0){
            classifierFailureCounter <- classifierFailureCounter + 1
            if(classifierFailureCounter > 5) break
          }
          tree <- output$t
          bestPerf <- output$p
          bestParams <- output$bp
          #Select Candidate Classifier Configurations
          candidateConfs <- selectConfiguration(R, classifierAlgorithm, tree, bestParams, B = B)
          #Intensify
          if(nrow(candidateConfs) > 0){
            output <- intensify(R, bestParams, bestPerf, candidateConfs, foldedSet, trainingSet, validationSet, classifierAlgorithm, maxClfTime, timeTillNow, B = B)
            bestParams <- output$params
            bestPerf <- output$perf
            timeTillNow <- output$timeTillNow
            R <- output$r
          }
          #Check if execution time exceeded the allowed time or not
          t <- toc(quiet = TRUE)
          timeTillNow <- timeTillNow + t$toc - t$tic
          tic(quiet = TRUE)
          if(timeTillNow > maxClfTime){
            if(mean(bestPerf) > mean(bestAlgorithmPerf)){
              bestAlgorithmPerf <- bestPerf
              bestAlgorithm <- classifierAlgorithm
              bestAlgorithmParams <- bestParams
            }
            break
          }

        }
      }

    }, timeout = maxTime * 60)
  }, TimeoutException = function(ex) {
    message("Time Budget allowed has finished.")
  })

  saveResultsError <- try(
  {
    bestAlgorithmParams <- bestAlgorithmParams[,names(bestAlgorithmParams) != "EI" & names(bestAlgorithmParams) != "performance"]
    #Run Classifier over all training set and check performance on testing set
    finalResult <- runClassifier(trainingSet = trainDataset, validationSet = testDataset, params = bestAlgorithmParams, classifierAlgorithm = bestAlgorithm, interp = interp)
    finalResult$clfs <- bestAlgorithm
    finalResult$params <- bestAlgorithmParams
    #save results to Temporary File
    sendToTmp(metaFeatures, bestAlgorithm, bestAlgorithmParams, finalResult$perf)
  })
  if(inherits(saveResultsError, "try-error")){
    print('No Results Found!...Try increasing the time budget.')
    return(-1)
  }

  #check internet connection and send data in tmp file to database if connection exists
  if(checkInternet() == TRUE){
    print("SEND TO DATABASE")
    #sendToDatabase()
  }
  return(finalResult)
}
