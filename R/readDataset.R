#' @title Read Dataset File into Memory.
#'
#' @description Read the file of the dataset, and split it into training and validation sets.
#'
#' @param directory String of the directory to the file containing the training dataset.
#' @param testDirectory String of the directory to the file containing the testing dataset.
#' @param vRatio The split ratio of the dataset file into training, and validation sets default(10% Validation - 90% Training).
#' @param classCol String of the class column of the dataset.
#' @param preProcessF Vector of Strings of the preprocessing algorithm to apply.
#' @param featuresToPreProcess Vector of numbers of features columns to perform preprocessing - empty vector means all features.
#' @param nComp Number of components needed if either "pca" or "ica" feature preprocessors are needed.
#' @param missingOpr Boolean variable represents either delete instances with missing values or apply imputation using "MICE" library - (default = 0 --> delete instances).
#' @param metric Metric of string character to be used in evaluation:
#' @param balance Boolean variable represents if SMOTE class balancing is required or not (default FALSE).
#' \itemize{
#' \item "acc" - Accuracy,
#' \item "avg-fscore" - Average of F-Score of each label,
#' \item "avg-recall" - Average of Recall of each label,
#' \item "avg-precision" - Average of Precision of each label,
#' \item "fscore" - Micro-Average of F-Score of each label,
#' \item "recall" - Micro-Average of Recall of each label,
#' \item "precision" - Micro-Average of Precision of each label.
#' }
#'
#' @return List of the Training and Validation Sets splits.
#'
#' @examples readDataset('/Datasets/irisTrain.csv', '/Datasets/irisTest.csv', 0.1, c(), 'class', 'pca', c(), 2)
#'
#' @import RWeka
#' @import farff
#' @import caret
#' @import mice
#' @importFrom UBL SmoteClassif
#' @importFrom imputeMissings compute impute
#' @importFrom  utils read.csv
#' @importFrom stats complete.cases
#'
#' @noRd
#'
#' @keywords internal

readDataset <- function(directory, testDirectory, vRatio = 0.3, classCol, preProcessF, featuresToPreProcess, nComp, missingOpr, metric, balance) {
  #check if CSV or arff
  ext <- substr(directory, nchar(directory)-2, nchar(directory))
  #Read CSV file of data
  if(ext == 'csv'){
    con <- file(directory, "r")
    data <- read.csv(file = con, header = TRUE, sep = ",", stringsAsFactors = TRUE)
    close(con)
    con <- file(testDirectory, "r")
    dataTED <- read.csv(file = con, header = TRUE, sep = ",", stringsAsFactors = TRUE)
    close(con)
  }
  else{
    data <- readARFF(directory)
    dataTED <- readARFF(testDirectory)
  }

  #Sampling from large datasets
  maxSample = 1000000
  n = as.integer(maxSample / ncol(data))
  if(maxSample < nrow(data) * ncol(data)){
    sampleInds <- createDataPartition(y = data$class, times = 1, p = n/nrow(data), list = FALSE)
    data <- data[sampleInds,]
  }

  #change column name of classes to be "class"
  colnames(data)[which(names(data) == classCol)] <- "class"
  colnames(dataTED)[which(names(dataTED) == classCol)] <- "class"
  cInd <- grep("class", colnames(data)) #index of class column
  #function which returns function which will encode vectors with values of class column labels
  label_encoder <- function(vec){
    levels <- sort(unique(vec))
    function(x){
      match(x, levels)
    }
  }
  classEncoder <- label_encoder(data$class) # create class encoder
  data$class <- classEncoder(data$class) # encoding class labels of training set
  dataTED$class <- classEncoder(dataTED$class) # encoding class labels of testing set

  #check either to delete an instance with missing values or perform imputation
  if (missingOpr == FALSE){
    missingVals <- imputeMissings::compute(data, method = "median/mode")
    data <- impute(data, object = missingVals)
    dataTED <- impute(dataTED, object = missingVals)
  }
  else{
    data <-complete( mice(data, m = 1, threshold = 1, printFlag = FALSE))
    dataTED <- complete(mice(dataTED, m = 1, threshold = 1, printFlag = FALSE))
  }

  #remove ID features
  numericFlag <- unlist(lapply(data, is.numeric))
  rmvFlag = c()
  for(i in 1:ncol(data)){
    len = length(unique(data[,i]))
    if(numericFlag[i] == FALSE && ((len / nrow(data) > 0.5) || len == 1) )
      rmvFlag <- c(rmvFlag, i)
  }
  keepFlag = c(1:ncol(data))
  keepFlag = keepFlag[!keepFlag %in% rmvFlag]
  data <- data[, keepFlag]
  dataTED <- dataTED[, keepFlag]

  #Select all remaining features
  selectedFeats <- c(1:ncol(data))

  #perform preprocessing
  if(length(featuresToPreProcess ) == 0){
    numericFlag <- unlist(lapply(data, is.numeric))
    for(i in 1:ncol(data)){
      if(numericFlag[i] == TRUE && i != cInd)
        featuresToPreProcess <- c(featuresToPreProcess, i)
    }
  }
  if(length(preProcessF) != 0 && length(featuresToPreProcess) > 1){
    featuresToPreProcess <- featuresToPreProcess[!featuresToPreProcess %in% cInd] #remove class column from set of features to be preprocessed
    dataTmp = list(TD = data[,featuresToPreProcess], TED = dataTED[,featuresToPreProcess])
    #Add PCA if we have more than 100 features
    if(length(featuresToPreProcess) > 100 && any('pca' != preProcessF) )
      preProcessF <- c(preProcessF, 'pca')
    for(i in 1:length(preProcessF)){
      dataTmp <- featurePreProcessing(dataTmp$TD, dataTmp$TED, preProcessF[i], nComp)
    }

    #add other features that don't require feature preprocessing to the features obtained after preprocessing
    diffTmp <- setdiff(selectedFeats, c(cInd, featuresToPreProcess))
    dHead = c(colnames(dataTmp$TD), colnames(data)[diffTmp])

    dataTDTmp <- data.frame(cbind(dataTmp$TD, data[,diffTmp]))
    dataTEDTmp <- data.frame(cbind(dataTmp$TED, dataTED[,diffTmp]))
    colnames(dataTDTmp) <- dHead
    colnames(dataTEDTmp) <- dHead

    #add class column to the dataframe of the dataset
    dataTDTmp$class <- data$class
    dataTEDTmp$class <- dataTED$class
    data <- dataTDTmp
    dataTED <- dataTEDTmp
  }

  #Class Balancing using Smote for metrics other than accuracy and binary class problems
  if( balance == TRUE || (metric != 'acc' && length(unique(data$class)) == 2) ){
    data$class = factor(data$class)
    data <- SmoteClassif(class ~., data, dist = 'HEOM')
  }

  # Use 70% of the dataset as Training - 30% of the dataset as Validation by default
  #smp_size <- floor((1-vRatio) * nrow(data))
  # set the seed to make your partition reproducible
  #train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train_ind <- createDataPartition(y = data$class, times = 1, p = (1-vRatio), list = FALSE)
  trainingDataset <- data[train_ind, ]
  validationDataset <- data[-train_ind, ]
  return (list(TD = trainingDataset, VD = validationDataset, FULLTD = data, TED = dataTED))
}
