#' @title Run smartML function for automatic Machine Learning.
#'
#' @description Run the smartML main function for automatic classifier algorithm selection, and hyper-parameter tuning.
#'
#' @param maxTime Float of the maximum time budget for reading dataset, preprocessing, calculating meta-features, Algorithm Selection & hyper-parameter tuning process only in minutes(Excluding Model Interpretability).
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
#' @param featuresToPreProcess Vector of number of features to perform the feature preprocessing on - In case of empty vector, this means to include all features in the dataset file (default = c()) - This vector should be a subset of \code{selectedFeats}.
#' @param nComp Integer of Number of components needed if either "pca" or "ica" feature preprocessors are needed.
#' @param nModels Integer representing the number of classifier algorithms that you want to select based on Meta-Learning and start to tune using Bayesian Optimization (default = 3).
#' @param options Integer representing either Classifier Algorithm Selection is needed only = 1 or Algorithm selection with its parameter tuning is required = 2 which is the default value.
#' @param featureTypes Vector of either 'numerical' or 'categorical' representing the types of features in the dataset (default = c() --> any factor or character features will be considered as categorical otherwise numerical).
#' @param interp Boolean representing if model interpretability (Feature Importance and Interaction) is needed or not (default = 0) This option will take more time budget if set to 1.
#' @param missingValues Vector of strings representing the missing values in dataset (default: c('NA', '?', ' ')).
#' @param missingOpr Boolean variable represents either delete instances with missing values or apply imputation using "MICE" library which helps you imputing missing values with plausible data values that are drawn from a distribution specifically designed for each missing datapoint- (default = 0 --> delete instances).
#'
#' @return List of Choosen parameter configurations for the \code{nModels} classifiers.
#'
#' @examples
#' autoRLearn(10, '../sampleDatasets/car/train.arff')
#' autoRLearn(60, '../sampleDatasets/satImage/train.arff', classCol = 'label', nModels = 5)
#' autoRLearn(30, '../sampleDatasets/EEGEyeState/train.csv', preProcessF = 'standardize')
#' autoRLearn(25, '../sampleDatasets/shuttle/train.arff', preProcessF = 'pca', nComp = 2, nModels = 3)
#' autoRLearn(1, '../sampleDatasets/waveform/train.arff', options = 1)
#' autoRLearn(120, '../sampleDatasets/churn/train.arff', preProcessF = 'center', featuresToPreProcess = c(1,2,3,4), vRatio = 0.2)
#'
#' @export

autoRLearn <- function(maxTime, directory, classCol = 'class', selectedFeats = c(), vRatio = 0.1, preProcessF = 'N', featuresToPreProcess = c(), nComp = NA, nModels = 4, option = 2, featureTypes = c(), interp = 0, missingVal = C('NA', '?', ' '), missingOpr = 0) {
  library(tictoc)

  #Read Dataset
  datasetReadError <- try(
  {
    dataset <- readDataset(directory, selectedFeats = selectedFeats, classCol = classCol, vRatio = vRatio, preProcessF = preProcessF, featuresToPreProcess = featuresToPreProcess, nComp = nComp, missingVal = missingVal)
    trainingSet <- dataset$TD
  })
  if(inherits(datasetReadError, "try-error")){
    print('Failed Reading Dataset: Make sure that dataset directory is correct and it is a valid csv/arff file.')
    return(-1)
  }

  tryCatch({
    nClassifiers <- 15
    res <- withTimeout({

      #Calculate Meta-Features for the dataset
      metaFeatures <- computeMetaFeatures(trainingSet, maxTime, featureTypes)

      #Convert Categorical Features to Numerical Ones and split the dataset
      dataset <- convertCategorical(dataset)
      validationSet <- dataset$VD #Validation set
      trainingSet <- dataset$TD #Training Set
      foldedSet <- dataset$FD #Folded sets of Training Data.

      #Generate candidate classifiers
      output <- getCandidateClassifiers(maxTime, metaFeatures, min(c(nModels, nClassifiers)) )
      algorithms <- output$c #Classifier Algorithm names selected.
      tRatio <- output$r #Time ratio between all classifiers.
      algorithmsParams <- output$p #Initial Parameter configuration of each classifier.

      #variables to hold best classifiers
      bestAlgorithm <- '' #bestClassifierName.
      bestAlgorithmPerf <- 0 #bestClassifierPerformance.
      bestAlgorithmParams <- list() #Parameters of best Classifier.

      #Option 1: Only Candidate Classifiers with initial parameters will be resulted (No Hyper-parameter tuning)
      if(option == 1){
        return (list(Clfs = algorithms, params = algorithmsParams))
      }

      #Option 2: Classifier Algorithm Selection + Parameter Tuning
      #loop over each classifier
      for(i in 1:length(algorithms)){
        classifierAlgorithm <- algorithms[i]
        classifierAlgorithmParams <- algorithmsParams[i]

        #Exception for deep Boost requires binary classes dataset
        if(classifierAlgorithm == 'deepboost' && metaFeatures$nClasses > 2)
          next
        #Read maxTime for the current classifier algorithm
        maxTime <- tRatio[i]

        #Read the current classifier default parameter configuration
        classifierConf <- getClassifierConf(classifierAlgorithm)
        cat('\n\nStart Tuning Classifier Algorithm: ', classifierAlgorithm, '\n')
        #initialize step
        print('INITIALLIZE: ')
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
          if(length(bestPerf) > 0 && mean(bestPerf) == 0){
            classifierFailureCounter <- classifierFailureCounter + 1
            if(classifierFailureCounter > 5) break
          }

          tree <- output$t
          bestPerf <- output$p
          bestParams <- output$bp
          #Select Candidate Classifier Configurations
          candidateConfs <- selectConfiguration(R, classifierAlgorithm, tree, bestParams)
          #Intensify
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
            break
          }
        }
      }

    }, timeout = maxTime * 60)
  }, TimeoutException = function(ex) {
    message("Time Budget Allowed Vanished.")
  })

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
    #sendToDatabase()
  }
}
