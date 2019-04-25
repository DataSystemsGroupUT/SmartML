#' @title Read Dataset File into Memory.
#'
#' @description Read the file of the dataset, and split it into training and validation sets.
#'
#' @param directory String of the directory to the file containing the training dataset.
#' @param testDirectory String of the directory to the file containing the testing dataset.
#' @param vRatio The split ratio of the dataset file into training, and validation sets default(10% Validation - 90% Training).
#' @param selectedFeats Vector of numbers of features to select from the dataset and ignore the rest of columns - empty vector means all features.
#' @param classCol String of the class column of the dataset.
#' @param preProcessF String of the preprocessing algorithm to apply.
#' @param featuresToPreProcess Vector of numbers of features columns to perform preprocessing - empty vector means all features.
#' @param nComp Number of components needed if either "pca" or "ica" feature preprocessors are needed.
#' @param missingOpr Boolean variable represents either delete instances with missing values or apply imputation using "MICE" library - (default = 0 --> delete instances).
#'
#' @return List of the Training and Validation Sets splits.
#'
#' @examples readDataset('/Datasets/irisTrain.csv', '/Datasets/irisTest.csv', 0.1, c(), 'class', 'pca', c(), 2)
#'
#' @import RWeka
#' @import farff
#' @import caret
#' @import mice
#' @importFrom  utils read.csv
#' @importFrom stats complete.cases
#'
#' @noRd
#'
#' @keywords internal

readDataset <- function(directory, testDirectory, vRatio = 0.3, selectedFeats, classCol, preProcessF, featuresToPreProcess, nComp, missingOpr) {
  #check if CSV or arff
  ext <- substr(directory, nchar(directory)-2, nchar(directory))
  #Read CSV file of data
  if(ext == 'csv'){
    con <- file(directory, "r")
    data <- read.csv(file = con, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    close(con)
    con <- file(testDirectory, "r")
    dataTED <- read.csv(file = con, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    close(con)
  }
  else{
    data <- readARFF(directory)
    dataTED <- readARFF(testDirectory)
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

  #check either to delete instance with missing values or perform imputation
  if (missingOpr == FALSE){
    data <- data[complete.cases(data), ]
    dataTED <- dataTED[complete.cases(dataTED), ]
  }
  else{
    data <-complete( mice(data, m = 1, threshold = 1, printFlag = FALSE))
    dataTED <- complete(mice(dataTED, m = 1, threshold = 1, printFlag = FALSE))
  }

  #select features only upon user request
  if(length(selectedFeats) == 0){
    selectedFeats <- c(1:ncol(data))
  }
  #perform preprocessing
  if(preProcessF != 'N'){
    if(length(featuresToPreProcess ) == 0)
      featuresToPreProcess <- selectedFeats

    featuresToPreProcess <- featuresToPreProcess[!featuresToPreProcess %in% cInd] #remove class column from set of features to be preprocessed
    dataTmp <- featurePreProcessing(data[,featuresToPreProcess], dataTED[,featuresToPreProcess], preProcessF, nComp)

    #add other features that don't require feature preprocessing to the features obtained after preprocessing
    diffTmp <- setdiff(selectedFeats, c(cInd, featuresToPreProcess))
    dataTDTmp <- cbind(dataTmp$TD, data[, diffTmp])
    dataTEDTmp <- cbind(dataTmp$TED, dataTED[, diffTmp])
    #add class column to the dataframe of the dataset
    dataTDTmp$class <- data$class
    dataTEDTmp$class <- dataTED$class
    data <- dataTDTmp
    dataTED <- dataTEDTmp
  }
  else{
    data <- data[, selectedFeats]
    dataTED <- dataTED[, selectedFeats]
  }
  # Use 90% of the dataset as Training - 10% of the dataset as Validation by default
  smp_size <- floor((1-vRatio) * nrow(data))
  # set the seed to make your partition reproducible
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  trainingDataset <- data[train_ind, ]
  validationDataset <- data[-train_ind, ]
  return (list(TD = trainingDataset, VD = validationDataset, FULLTD = data, TED = dataTED))
}
