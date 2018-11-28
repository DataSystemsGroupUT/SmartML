library(shiny)
require(rsconnect)
require(lubridate)
library(shinyjs)
library(shinythemes)
#library(shinycssloaders)

shinyUI(fluidPage(theme = shinytheme("flatly"),#theme = "bootstrap.css",
    useShinyjs(),
    sidebarLayout(

      sidebarPanel(
        useShinyjs(),
        #radioButtons(inputId = 'welcome_option', label = "Select What Do You Want", choices = c("Use"='uploadData',"Get Result"='getResult'), selected = '', inline = FALSE),
        #hr(),


        #hidden(

          div(
            id = "upload_data_form",

            #textInput(inputId = 'usermail', label = "User Mail", value = "", width = '100%'),
            #hidden(
            #  tags$div(id="needUserMailMessage", helpText("NO MAIL"))
            #),
            #hr(),
            radioButtons(inputId = 'scenario', label = "Do you want to upload a full dataset or only Meta-Features?", choices = c("Datasets"='datasets',"Feature Meta Data"='featuresMeta'), selected = 'datasets', inline = FALSE),
            hidden(
              tags$div(id="needScenarioMessage", helpText("NO SCENARIO"))
            ),
            hr(),
            hidden(
              div(
                id = "uploadBtn",
                fileInput("file","Upload the file", multiple = FALSE), # fileinput() function is used to get the file upload contorl option
                hidden(
                  tags$div(id="needFileMessage", helpText("NO File"))
                )
              )
            ),
            actionButton("start", "Start" , width = '100%')

          )
        #)
        ,

        hidden(
          div(
            id = "get_result_form",
            textInput(inputId = 'application_id', label = "Enter Your Application ID", value = "", width = '100%'),
            hidden(
              tags$div(id="needApplicationMessage", helpText("NO Application ID"))
            ),
            hr(),
            actionButton("getResultButton", "Get Result" , width = '100%')
          )
        )

      ),
      mainPanel(
        hidden(
          div(
            id = "inputMeta",
            div(
              style = 'overflow-y: scroll; max-height: 450px',
              textInput(inputId = 'nClasses', label = "Number of Instances", value = "", width = '100%'),
              textInput(inputId = 'nFeatures', label = "Number of Features", value = "", width = '100%'),
              textInput(inputId = 'nInstances', label = "Number of Classes", value = "", width = '100%'),
              textInput(inputId = 'nNumFeatures', label = "Number of Numerical Features", value = "", width = '100%'),
              textInput(inputId = 'nCatFeatures', label = "Number of Categorical Features", value = "", width = '100%'),
              textInput(inputId = 'classEntropy', label = "Class Entropy", value = "", width = '100%'),
              textInput(inputId = 'classProbMin', label = "Probability of Minimum Class", value = "", width = '100%'),
              textInput(inputId = 'classProbMax', label = "Probability of Maximum Class", value = "", width = '100%'),
              textInput(inputId = 'classProbMean', label = "Mean of Class Probability", value = "", width = '100%'),
              textInput(inputId = 'classProbStdDev', label = "Std Dev. of Class Probability", value = "", width = '100%'),
              textInput(inputId = 'symbolsMean', label = "Mean of number of symbols of categorical features", value = "", width = '100%'),
              textInput(inputId = 'symbolsSum', label = "Sum of number of symbols of categorical features", value = "", width = '100%'),
              textInput(inputId = 'symbolsStdDev', label = "Std. Dev of number of symbols of categorical features", value = "", width = '100%'),
              textInput(inputId = 'featuresSkewMin', label = "Minimum Skewness of Numerical Features", value = "", width = '100%'),
              textInput(inputId = 'featuresSkewMax', label = "Maximum Skewness of Numerical Features", value = "", width = '100%'),
              textInput(inputId = 'featuresSkewMean', label = "Mean Skewness of Numerical Features", value = "", width = '100%'),
              textInput(inputId = 'featuresSkewStdDev', label = "Std Dev. of Skewness of Numerical Features", value = "", width = '100%'),
              textInput(inputId = 'featuresKurtMin', label = "Minimum Kurtosis of Numerical Features", value = "", width = '100%'),
              textInput(inputId = 'featuresKurtMax', label = "Maximum Kurtosis of Numerical Features", value = "", width = '100%'),
              textInput(inputId = 'featuresKurtMean', label = "Mean Kurtosis of Numerical Features", value = "", width = '100%'),
              textInput(inputId = 'featuresKurtStdDev', label = "Std Dev. of Kurtosis of Numerical Features", value = "", width = '100%')
            ),
            ##############################
            tags$div(id="noLearningAlgHelpText", helpText("Number of required output models")),
            radioButtons(inputId = 'noLearningAlg', label = "", choices = c("1"='1',"2"='2',"3"='3',"4"='4',"5"='5'), selected = "3", inline = TRUE),
            tags$div(id="noLearningAlgHr", hr()),
            ##############################
            tags$div(id="metricHelpText", helpText("Define Metric to be used ")),
            selectInput(inputId = 'metric', label = "", choices= c("Accuracy"='Accuracy'), selected = 'Accuracy' , width = '30%' ),
            tags$div(id="metricHr", hr())
          ),

          div(
            ##############################
            id = "scenario_datasets_form",
            uiOutput("SampleDataset"),
            tags$div(id="SampleDatasetHr", hr()),
            ##############################

            tags$div(id="labelSelectHelpText", helpText("Class column")),
            selectInput("labelSelect",label = "", choices=colnames(data()) , width = '30%'),
            tags$div(id="labelSelectHr", hr()),

            ##############################

            tags$div(id="featuresHelpText", helpText("Input features of the model")),
            div(
              style = 'overflow-y: scroll; max-height: 250px',
              checkboxGroupInput(inputId = 'features', label = "", selected = colnames(data()), choices = colnames(data()))
            ),
            tags$div(id="featuresHr", hr()),

            ##############################

            tags$div(id="AlgorithmSelectionHelpText", helpText("Service Type ")),
            radioButtons(inputId = 'AlgorithmSelection', label = "", choices = c("Algorithm Selection Only"='algorithm',"Algorithm Selection and Hyperparameter Tuning"='algorithmANDtuning'), selected = NULL, inline = TRUE),
            tags$div(id="AlgorithmSelectionHr", hr()),


            ##############################

            hidden(
              div(
                id = "interpretability_div",
                tags$div(id="InterpretabilityHelpText", helpText("Will Model Interpretability be used? (It will take more time)")),
                selectInput(inputId = 'Interpretability', label = "", choices= c("No"='No' ,"Yes"='Yes') , selected = 'No' , width = '30%' ),
                tags$div(id="InterpretabilityHr", hr())
              ),

              ##############################


              hidden(
                div(
                  id = "time_budget_div",

                  tags$div(id="timeBudgetHelpText", helpText("Hyper-parameter Tuning Time budget in minutes")),
                  textInput(inputId = 'timeBudget', label = "", value = "15", width = '10%', placeholder = NULL),
                  tags$div(id="timeBudgetHr", hr())
                )
              )),

            ##############################

            tags$div(id="missingvalueHelpText", helpText("Symbol representing missing values in dataset ")),
            textInput(inputId = 'missingvalue', label = "", value = "NA", width = '10%', placeholder = NULL),
            tags$div(id="missingvalueHr", hr()),

            ##############################

            tags$div(id="featurePreprocessingMethodHelpText", helpText("Is feature Pre-processing needed?")),
            selectInput(inputId = 'featurePreprocessingMethod', label = "",
                        choices= c("No"='none', "Normalization"='range', "BoxCox"='boxcox', "YeoJohnson"='yeo-Johnson'
                                   ,"Remove zero variance"='zv', "Centering"='center', "Scale"='scale'
                                   ,"Principal Component Analysis"='pca', "Independent Component Analysis"='ica') , selected = NULL , width = '30%' ),
            tags$div(id="featurePreprocessingMethodHr", hr()),

            ##############################

            hidden(
              div(
                style = 'overflow-y: scroll; max-height: 250px',
                id = "preprocess_feats_div",

                tags$div(id="preprocessingfeaturesHelpText", helpText("Features to perform preprocessing on")),
                actionLink("preprocessingfeaturesSelectall","Select All"),
                checkboxGroupInput(inputId = 'preprocessingfeatures', label = "", selected = NULL, choices = colnames(data())),
                tags$div(id="preprocessingfeaturesHr", hr())
              )
            ),


            ##############################

            div(
              style = 'overflow-y: scroll; max-height: 250px',
              uiOutput("featureType")
            ),

            ##############################


            tags$div(id="noLearningAlgHelpText", helpText("Number of output models")),
            radioButtons(inputId = 'noLearningAlg', label = "", choices = c("1"='1',"2"='2',"3"='3',"4"='4',"5"='5'), selected = NULL, inline = TRUE),
            tags$div(id="noLearningAlgHr", hr()),

            ##############################

            tags$div(id="metricHelpText", helpText("Define Metric to be used ")),
            selectInput(inputId = 'metric', label = "",
                        choices= c("Accuracy"='Accuracy')
                        , selected = NULL , width = '30%' ),
            tags$div(id="metricHr", hr()),

            hidden(

              div(
                id = "outputTextSubmit_div",
                textOutput("outputTextSubmit")
              )
            )

          ),

          hidden(
            div(
              id = "submitButton",
              actionButton("submit", "Submit")
            )
          ),

          hidden(
            div(
              id = "getResultOutput_div",
              htmlOutput("getResultOutput"),
              tags$head(tags$style("#getResultOutput{color: #696969; font-size: 16px;}"))
            ),
            div(
              plotOutput("interPlot")
            )
          )

        )

      )


    )
  )
)
