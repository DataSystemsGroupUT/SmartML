#' @title Read Dataset File into Memory.
#'
#' @description Read the file of the dataset, and split it into training and validation sets.
#'
#' @param directory String of the directory to the file containing the dataset.
#' @param vRatio The split ratio of the dataset file into training, and validation sets default(10% Validation - 90% Training).
#' @param selectedFeats Vector of numbers of features to select from the dataset and ignore the rest of columns - empty vector means all features.
#' @param classCol String of the class column of the dataset.
#' @param preProcessF String of the preprocessing algorithm to apply.
#' @param featuresToPreProcess Vector of numbers of features columns to perform preprocessing - empty vector means all features.
#' @param nComp Number of components needed if either "pca" or "ica" feature preprocessors are needed.
#'
#' @return List of the Training and Validation Sets splits.
#'
#' @examples readDataset('/Datasets/iris.csv', 0.1, c(), 'class', 'pca', c(), 2)
#'
#' @noRd
#'
#' @keywords internal

readDataset <- function(directory, vRatio = 0.1, selectedFeats, classCol, preProcessF, featuresToPreProcess, nComp) {
  library(RWeka)
  library(farff)
  library(caret)
  #check if CSV or arff
  ext <- substr(directory, nchar(directory)-2, nchar(directory))
  #Read CSV file of data
  if(ext == 'csv'){
    con <- file(directory, "r")
    data <- read.csv(file = con, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    close(con)
  }
  else
    data <- readARFF(directory)

  print(names(data))
  #change column name of classes to be "class"
  colnames(data)[which(names(data) == classCol)] <- "class"
  cInd <- grep("class", colnames(data)) #index of class column
  #remove instances with missing data
  data <- data[complete.cases(data), ]
  #select features only upon user request
  if(length(selectedFeats) == 0)
    selectedFeats <- c(1:ncol(data))
  #perform preprocessing
  if(preProcessF != 'N'){
    if(length(featuresToPreProcess ) == 0)
      featuresToPreProcess <- c(1:ncol(data))

    featuresToPreProcess <- featuresToPreProcess[!featuresToPreProcess %in% cInd] #remove class column from set of features to be preprocessed
    dataTmp <- data[,featuresToPreProcess]
    dataTmp <- featurePreProcessing(dataTmp, preProcessF, nComp)
    #add other features that don't require feature preprocessing to the features obtained after preprocessing
    diffTmp <- setdiff(selectedFeats, c(cInd, featuresToPreProcess))
    dataTmp <- cbind(dataTmp, data[, diffTmp])
    #add class column to the dataframe of the dataset
    dataTmp$class <- data$class
    data <- dataTmp
  }
  else
    data <- data[, selectedFeats]
  #cat('Number of Dataset Rows: ', nrow(data), '\n')
  #cat('Number of Dataset Columns: ', ncol(data), '\n')
  # Use 90% of the dataset as Training - 10% of the dataset as Validation
  smp_size <- floor((1-vRatio) * nrow(data))
  # set the seed to make your partition reproducible
  #set.seed(123)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  trainingDataset <- data[train_ind, ]
  validationDataset <- data[-train_ind, ]
  return (list(TD = trainingDataset, VD = validationDataset))
}
