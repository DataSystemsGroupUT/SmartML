library(shiny)
library(farff)
library(iml)
library(e1071)

require(rsconnect)
require(lubridate)

source("autoRLearn.R")
source("checkInternet.R")
source("computeEI.R")
source("computeMetaFeatures.R")
source("convertCategorical.R")
source("featurePreProcessing.R")
source("fitModel.R")
source("getCandidateClassifiers.R")
source("getClassifierConf.R")
source("initialize.R")
source("intensify.R")
source("readDataset.R")
source("runClassifier.R")
source("selectConfiguration.R")
source("sendToDatabase.R")
source("sendToTmp.R")
source("outClassifierConf.R")
source("intrepretability.R")
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
options(shiny.maxRequestSize = 10*1024^2)
#shiny.maxRequestSize=30*1024^2
#outputTextSubmit fun
test.runSmartML <- function(str, dataPath, timeBudget, selectedFeatures, preProcessA, preProcessFeats, nModels, inputSc, featsTypes, interp){
  newStr <- autoRLearn(min(30, as.numeric(timeBudget)), dataPath, classCol = str, selectedFeats = selectedFeatures, preProcessF = preProcessA, featuresToPreProcess = preProcessFeats, nComp = NA, nModels = nModels, option = inputSc, featureTypes = featsTypes, interp = interp)
  newStr
}

#Get Meta Output
getMetaOutput <- function(metaFeatures, nModels){
  #Generate candidate classifiers
  output <- getCandidateClassifiers(10, metaFeatures, nModels)
  algorithms <- output$c
  tRatio <- output$r
  algorithmsParams <- output$p
  #Only Candidate Classifiers
  out <- c()
  for(i in 1:length(algorithms)){
    classifierAlgorithm <- algorithms[i]
    classifierConf <- getClassifierConf(classifierAlgorithm)
    classifierAlgorithmParams <- outClassifierConf(classifierAlgorithm, classifierConf, algorithmsParams[i])
    separator <- '<hr/>'
    out <- paste(out, '<h4>Model(', as.character(i), '): <b>', classifierAlgorithm, '</h4><br/> Configuration: </b>', classifierAlgorithmParams, '<br/>', separator, '  <br/><br/>', collapse='')
  }
  out
}

#scenario_metadata_form fun
test.fun2 <- function(myFilePath){
  newStr <- myFilePath
  newStr
}

#progress fun
test.fun3 <- function(applicationID){
  newStr <- 'here is the progress for ' <- applicationID
  newStr
}

#Get features Types
featsTypes <- function(dataset){
  featureType <- c()
  featureTypeOpp <- c()
  symbolsVector <- c()
  featsType <- lapply(dataset, class)
  lognInstances <- log(nrow(dataset))
  for(i in colnames(dataset)){
    if(featsType[[i]] != 'factor' && featsType[[i]] != 'character' && length(unique(dataset[[i]])) > lognInstances){
      featureType <- c(featureType, 'numerical')
      featureTypeOpp <- c(featureTypeOpp, 'categorical')
    }

    else{
      featureType <- c(featureType, 'categorical')
      featureTypeOpp <- c(featureTypeOpp, 'numerical')
    }
  }
  return(list(fType = featureType, fTypeOpp = featureTypeOpp))
}

shinyServer(function(input,output,session) {

  #collect meta features
  collectMeta <- function(){
    #1- number of instances
    nInstances <- as.numeric(input$nInstances)
    #2- log number of instances
    lognInstances <- log(nInstances)
    #3- number of features
    nFeatures <- as.numeric(input$nFeatures)
    #4- log number of features
    lognFeatures <- log(nFeatures)
    #5- number of classes
    nClasses <- as.numeric(input$nClasses)
    #6- number of categorical features
    nCatFeatures <- as.numeric(input$nCatFeatures)
    nNumFeatures <- as.numeric(input$nNumFeatures)
    #8- ratio of numerical to categorical features
    if(nNumFeatures > 0)
      ratioNumToCat <- nCatFeatures / nNumFeatures
    else
      ratioNumToCat <- 999999
    #9- class entropy
    classEntropy <- as.numeric(input$classEntropy)
    #10- class probability max
    classProbMax <- as.numeric(input$classProbMax)
    #11- class probability min
    classProbMin <- as.numeric(input$classProbMin)
    #12- class probability mean
    classProbMean <- as.numeric(input$classProbMean)
    #13- class probability std. dev
    classProbStdDev <- as.numeric(input$classProbStdDev)
    #14- Symbols Mean
    if(length(input$symbolsMean) > 0) symbolsMean <- as.numeric(input$symbolsMean)
    else symbolsMean <- 'NULL'
    #15- Symbols sum
    if(length(input$symbolsSum) > 0) symbolsSum <- as.numeric(input$symbolsSum)
    else symbolsSum <- 'NULL'
    #16- Symbols Std. Deviation
    if(length(input$symbolsStdDev) > 0) symbolsStdDev <- as.numeric(input$symbolsStdDev)
    else symbolsStdDev <- 'NULL'
    #17- skewness min
    if(length(input$featuresSkewMin) > 0) featuresSkewMin <- as.numeric(input$featuresSkewMin)
    else featuresSkewMin <- 0
    #18- skewness mean
    if(length(input$featuresSkewMean) > 0) featuresSkewMean <- as.numeric(input$featuresSkewMean)
    else featuresSkewMean <- 0
    #19- skewness max
    if(length(input$featuresSkewMean) > 0) featuresSkewMax <- as.numeric(input$featuresSkewMax)
    else featuresSkewMax <- 0
    #20- skewness std. dev.
    if(length(input$featuresSkewStdDev) > 0) featuresSkewStdDev <- as.numeric(input$featuresSkewStdDev)
    else featuresSkewStdDev <- 0
    #21- Kurtosis min
    if(length(input$featuresKurtMin) > 0) featuresKurtMin <- as.numeric(input$featuresKurtMin)
    else featuresKurtMin <- 0
    #22- Kurtosis max
    if(length(input$featuresKurtMax) > 0) featuresKurtMax <- as.numeric(input$featuresKurtMax)
    else featuresKurtMax <- 0
    #23- Kurtosis mean
    if(length(input$featuresKurtMean) > 0) featuresKurtMean <- as.numeric(input$featuresKurtMean)
    else featuresKurtMean <- 0
    #24- Kurtosis std. dev.
    if(length(input$featuresKurtStdDev) > 0) featuresKurtStdDev <- as.numeric(input$featuresKurtStdDev)
    else featuresKurtStdDev <- 0
    #25- Dataset Ratio (ratio of number features: number of instances)
    datasetRatio <- nFeatures / nInstances

    #Collecting Meta-Features in a dataFrame
    df <- data.frame(datasetRatio = datasetRatio, featuresKurtStdDev = featuresKurtStdDev,
                     featuresKurtMean = featuresKurtMean, featuresKurtMax = featuresKurtMax,
                     featuresKurtMin = featuresKurtMin, featuresSkewStdDev = featuresSkewStdDev,
                     featuresSkewMean = featuresSkewMean, featuresSkewMax = featuresSkewMax,
                     featuresSkewMin = featuresSkewMin, symbolsStdDev = symbolsStdDev, symbolsSum = symbolsSum,
                     symbolsMean = symbolsMean, classProbStdDev = classProbStdDev, classProbMean = classProbMean,
                     classProbMax = classProbMax, classProbMin = classProbMin, classEntropy = classEntropy,
                     ratioNumToCat = ratioNumToCat, nCatFeatures = nCatFeatures, nNumFeatures = nNumFeatures,
                     nInstances = nInstances, nFeatures = nFeatures, nClasses = nClasses,
                     lognFeatures = lognFeatures, lognInstances = lognInstances, maxTime = 0)
    df
  }


  readData <- function(directory){
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

    data <- head(data, 5)
  }

  data <- reactive ({
    readData(input$file$datapath)
  })


  observeEvent(input$file, {

    hide("needFileMessage")

  })

  observeEvent(input$preprocessingfeaturesSelectall, {
    updateCheckboxGroupInput(session, inputId = 'preprocessingfeatures', label = "", selected = colnames(data()), choices = colnames(data()))
  })

  #observeEvent(input$usermail, {

  #  hide("needUserMailMessage")

  #})

  observeEvent(input$featurePreprocessingMethod, {

    if(input$featurePreprocessingMethod != 'none')
      show("preprocess_feats_div")
    else
      hide("preprocess_feats_div")
  })

  observeEvent(input$preprocessingfeaturesSelectall, {

    if(input$featurePreprocessingMethod != 'none')
      show("preprocess_feats_div")

  })

  observeEvent(input$scenario, {
    if(input$scenario == "datasets")
      show("uploadBtn")
    else{
      hide("uploadBtn")
      updateRadioButtons(session, inputId = 'AlgorithmSelection', label = "", choices = c("Algorithm Selection Only"='algorithm'))
    }
    hide("needScenarioMessage")

  })


  observeEvent(input$application_id, {

    hide("needApplicationMessage")

  })


  observeEvent(input$AlgorithmSelection, {


    if (!is.null(input$AlgorithmSelection)  && input$AlgorithmSelection == "algorithm"){
      hide("interpretability_div")
      hide("time_budget_div")


    }else if (!is.null(input$AlgorithmSelection)  && input$AlgorithmSelection == "algorithmANDtuning"){
      show("interpretability_div")
      show("time_budget_div")
    }

  })

  observeEvent(input$welcome_option, {


    if (!is.null(input$welcome_option)  && input$welcome_option == "uploadData"){
      hide("get_result_form")
      hide("getResultOutput_div")
      show("upload_data_form")

    }else if (!is.null(input$welcome_option)  && input$welcome_option == "getResult"){

      hide("upload_data_form")
      show("get_result_form")
      updateTextInput(session , inputId = 'application_id', label = "Enter Your Application ID", value = "")

    }

  })


  observeEvent(input$file, {

    hide("scenario_metadata_form")
    hide("scenario_datasets_form")
    #updateTextInput(session, inputId = 'usermail', label = "User Mail", value = "")
    #enable("usermail")
    #updateRadioButtons(session, inputId = 'scenario', label = "Is it a full dataset or only Meta-Features?", choices = c("Datasets"='datasets',"Feature Meta Data"='featuresMeta'), selected = '', inline = FALSE)
    #enable("scenario")

  })


  observeEvent(input$getResultButton, {

    if(!isTruthy(input$application_id)){

      show("needApplicationMessage")

    }else if(isTruthy(input$application_id)){
      hide("scenario_metadata_form")
      hide("scenario_datasets_form")
      show("getResultOutput_div")

      output$getResultOutput <- renderText({

        #if(is.null(input$file)){return()}

        input$getResultButton

        isolate(paste("the progress fun3 is : " , test.fun3(applicationID = input$application_id)))

      })


    }

  })


  observeEvent(input$start, {

    if( (is.null(input$file) && input$scenario == "datasets") || is.null(input$scenario)){

      #if(is.null(input$usermail))
      #  show("needUserMailMessage")

      if(is.null(input$scenario))
        show("needScenarioMessage")

      if(is.null(input$file))
        show("needFileMessage")

    } else if ( !is.null(input$scenario)){

     # disable("usermail")
      disable("scenario")
      show("submitButton")

      if(input$scenario == "datasets"){
        show("scenario_datasets_form")
        show("SampleDataset")
        show("SampleDatasetHr")
      }
      else{
        show("inputMeta")
      }

      if(input$scenario == "datasets"){
      ############################
      #Class Column Selection
        show("labelSelectHelpText")
        updateSelectInput(session, inputId = "labelSelect", choices=colnames(data()) , selected = colnames(data())[ncol(data())] )
        show("labelSelect")
        show("labelSelectHr")

      #############################
      #Features to include in dataset
        show("featuresHelpText")
        updateCheckboxGroupInput(session,  inputId = 'features', selected = colnames(data()), choices = colnames(data()))
        show("features")
        show("featuresHr")

      #############################
      #Algorithm Selection only or Hyper-Parameter Tuning
        show("AlgorithmSelectionHelpText")
        updateRadioButtons(session, inputId = 'AlgorithmSelection', choices = c("Algorithm Selection Only"='algorithm',"Algorithm Selection and Hyperparameter Tuning"='algorithmANDtuning'), selected = NULL, inline = TRUE)
        show("AlgorithmSelection")
        show("AlgorithmSelectionHr")

      #############################
      #Missing value symbol
        show("missingvalueHelpText")
        updateTextInput(session, inputId = 'missingvalue',  value = "NA")
        show("missingvalue")
        show("missingvalueHr")

      #############################
      #Feature Preprocessing Algorithm Selection
        show("featurePreprocessingMethodHelpText")
        updateSelectInput(session, inputId = 'featurePreprocessingMethod',
                          choices = c("None"='none', "Normalization"='range', "BoxCox"='boxcox', "YeoJohnson"='yeo-Johnson'
                                     ,"Remove zero variance"='zv', "Centering"='center', "Scale"='scale'
                                     ,"Principal Component Analysis"='pca', "Independent Component Analysis"='ica') , selected = NULL )
        show("featurePreprocessingMethod")
        show("featurePreprocessingMethodHr")

      #############################
      #Feature Preprocessing Feature Selection
        show("preprocessingfeaturesHelpText")
        show("preprocessingfeaturesSelectall")
        updateCheckboxGroupInput(session, inputId = 'preprocessingfeatures', selected = NULL, choices = colnames(data()))
        show("preprocessingfeatures")
        show("preprocessingfeaturesHr")

      #############################
      #Interpretability
        show("InterpretabilityHelpText")
        updateSelectInput(session,  inputId = 'Interpretability',
                          choices= c("No"='No' ,"Yes"='Yes') , selected = 'No')
        show("Interpretability")
        show("InterpretabilityHr")
      #############################

        show("featureType")

      #############################
      #Time budget
        show("timeBudgetHelpText")
        updateTextInput(session, inputId = 'timeBudget',  value = "5")
        show("timeBudget")
        show("timeBudgetHr")
      }
      else{
        show("AlgorithmSelectionHelpText")
        updateRadioButtons(session, inputId = 'AlgorithmSelection', choices = c("Algorithm Selection Only"='algorithm'), selected = 'algorithm', inline = TRUE)
        disable("AlgorithmSelection")
        show("AlgorithmSelection")
        show("AlgorithmSelectionHr")
      }
      #############################
      #Number of Learning Algorithms
      #show("noLearningAlgHelpText")
      #updateRadioButtons(session, inputId = 'noLearningAlg', choices = c("1"='1',"2"='2',"3"='3',"4"='4',"5"='5'), selected = '3', inline = TRUE)
      #show("noLearningAlg")
      #show("noLearningAlgHr")

      #############################
      #Metric
      show("metricHelpText")
      updateSelectInput(session, inputId = 'metric',
                        choices= c("Accuracy"='Accuracy')
                        , selected = NULL )
      show("metric")
      show("metricHr")

      ##############################

      show("submit")

    } else if (!is.null(input$file) && !is.null(input$scenario) && input$scenario == "featuresMeta"){
     # disable("usermail")
      disable("scenario")
      show("scenario_metadata_form")

      output$outputText_scenario_metadata <- renderText({
        if(is.null(input$file)){return()}
        input$start
        isolate(paste("my file from fun2 is : " , test.fun2(myFilePath = input$file$datapath)))
      })

    }

  })


  output$featureType <- renderUI({
    if(is.null(input$start)) {return()}
    out <- featsTypes(data())
    list(
      helpText("Verify Types of Features (NumericaL/Categorical) "),
      lapply(1:ncol(data()), function(i) {
        selectInput(inputId = paste0(colnames(data())[i],'Type'), label = paste0(colnames(data())[i]),
                    choices = c(out$fType[i], out$fTypeOpp[i]))
      }),
      hr()
    )
  })


  ## Dataset code ##
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(input$file)){return()}
    data()

  })


  ## MainPanel tabset renderUI code ##
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded.
  # Until the file is loaded, app will not show the tabset.
  output$SampleDataset <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
        tabPanel("Sample Dataset", tableOutput("table"))
      )
  })

  observeEvent(input$submit, {
    if(input$scenario == 'datasets'){
      withProgress(message = "Computing results", detail = "fetching data", value = 0, {
        hide("scenario_datasets_form")
        hide("submitButton")
        show("getResultOutput_div")
        incProgress(0.15, detail = "Validating Input")

        input$submit
        #Class label
        str <- input$labelSelect

        #Number of Models to check
        nModels <- as.numeric(input$noLearningAlg)
        cat('no of learning algorithm:', nModels)

        #Feature Preprocessing Algorithm
        preProcessA <- input$featurePreprocessingMethod
        if(preProcessA == 'none' || is.null(preProcessA)) preProcessA <- 'N'

        #Features to Preprocessed
        preProcessFeatsInd <- input$preprocessingfeatures
        preProcessFeats <- c()
        for (coln in preProcessFeatsInd){
          preProcessFeats <- c(preProcessFeats, which(names(data()) == coln))
        }

        #Features to process
        selectedFeatsInd <- input$features
        selectedFeats <- c()
        for (coln in selectedFeatsInd){
          selectedFeats <- c(selectedFeats, which(names(data()) == coln))
        }

        #Model Interpretability
        interp <- input$Interpretability
        if(interp == 'Yes')
          interp <- 1
        else
          interp <- 0

        #Which scenario
        if(input$AlgorithmSelection == "algorithmANDtuning")
          inputSc <- 2
        else
          inputSc <- 1

        #features types selected
        featsTypes <- character()
        for(i in colnames(data())){
          fType <- input[[as.character(paste0(i,'Type'))]]
          featsTypes <- c(featsTypes, fType)
        }
        incProgress(0.15, detail = "Processing")
        out <- test.runSmartML(str, input$file$datapath, input$timeBudget, selectedFeats, preProcessA, preProcessFeats, nModels, inputSc, featsTypes, interp)
        incProgress(0.7, detail = "Collecting Results")
        print(out)
        print(colnames(out))
        output$getResultOutput <- renderUI({
        HTML(paste(out))
        })
        #if(interp == 1){
        #  output$interPlot <- renderPlot({plot(out$interact)})
        #}
      })
    }
    else{
      withProgress(message = "Computing results", detail = "fetching data", value = 0, {
        hide("submitButton")
        hide("inputMeta")
        show("getResultOutput_div")
        # Collect Meta Features
        metaFeatures <- collectMeta()
        #Number of Models to check
        nModels <- as.numeric(input$noLearningAlg)
        incProgress(0.1, detail = "Processing")
        out <- getMetaOutput(metaFeatures, nModels)
        incProgress(0.7, detail = "Collecting Results")
        req(output$getResultOutput <- renderUI({
          HTML(paste(out))
        }))
      })
    }

})
})
