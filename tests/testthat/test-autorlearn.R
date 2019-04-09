context("test-autorlearn")

test_that("option1", {
  result1 <- autoRLearn(1, 'E:/Fdrive/Masters/R/SmartML_Final_V01.00/SmartML/sampleDatasets/shuttle/train.arff', 'E:/Fdrive/Masters/R/SmartML_Final_V01.00/SmartML/sampleDatasets/shuttle/test.arff', option = 1, preProcessF = 'pca', nComp = 4, nModels = 2)
  result1$clfs  #Vector of recommended nModels classifiers
  result1$params #Vector of initial suggested parameter configurations of nModels recommended classifiers
})


test_that("option2", {
  result3 <- autoRLearn(0.1, 'E:/Fdrive/Masters/R/SmartML_Final_V01.00/SmartML/sampleDatasets/EEGEyeState/train.csv', 'E:/Fdrive/Masters/R/SmartML_Final_V01.00/SmartML/sampleDatasets/EEGEyeState/test.csv', vRatio = 0.2, selectedFeats = c(1,3,5,7,9,11,13,15), missingOpr = TRUE) # Option 2 runs for both classifier algorithm selection and parameter tuning for
})

