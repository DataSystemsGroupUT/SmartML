#' @title Read Dataset File into Memory.
#'
#' @description Read the file of the training and testing dataset, and perform preprocessing and data cleaning if necessary.
#'
#' @param directory String of the directory to the file containing the training dataset.
#' @param testDirectory String of the directory to the file containing the testing dataset.
#' @param selectedFeats Vector of numbers of features columns to include from the training set and ignore the rest of columns - In case of empty vector, this means to include all features in the dataset file (default = c()).
#' @param classCol String of the name of the class label column in the dataset (default = 'class').
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
#' @param missingVal Vector of strings representing the missing values in dataset (default: c('NA', '?', ' ')).
#' @param missingOpr Boolean variable represents either delete instances with missing values or apply imputation using "MICE" library which helps you imputing missing values with plausible data values that are drawn from a distribution specifically designed for each missing datapoint- (default = 0 --> delete instances).
#'
#' @return List of the TrainingSet \code{Train} and TestingSet \code{Test}.
#'
#' @import RWeka
#' @import farff
#' @import caret
#' @import mice
#' @importFrom  utils read.csv
#' @importFrom stats complete.cases
#'
#' @examples
#' \dontrun{
#' dataset <- datasetReader('/Datasets/irisTrain.csv', '/Datasets/irisTest.csv')
#' }

datasetReader <- function(directory, testDirectory, selectedFeats = c(), classCol = 'class', preProcessF = 'N', featuresToPreProcess = c(), nComp = NA, missingVal = c('NA', '?', ' '), missingOpr = 0) {
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

  #Convert characters representing missing values to NA
  m1 <- as.matrix(data)
  m1[m1 %in% missingVal] <- NA
  m2 <- as.matrix(dataTED)
  m2[m2 %in% missingVal] <- NA

  #check either to delete instance with missing values or perform imputation
  if (missingOpr == 0){
    data <- data[complete.cases(m1), ]
    dataTED <- dataTED[complete.cases(m2), ]
  }
  else{
    data <- complete(mice(data, m = 1))
    dataTED <- complete(mice(dataTED, m = 1))
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
  return (list(Train = data, Test = dataTED))
}
