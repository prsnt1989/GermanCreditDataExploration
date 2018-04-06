#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(gmodels)
library(DT)
library(shinythemes)
library(rsconnect)
library(dplyr)
library(mlr)
#library(dummies)

#credit_dataset <- read.csv("german_credit_full.csv",stringsAsFactors = TRUE)
#ml_credit_dataset <- read.csv("ml_credit_dataset.csv")
source("loading_data.R")



# Define UI for application that plots features of movies 
ui <- fluidPage(
 # theme=shinytheme("cerulean"),
  # Application title
  titlePanel("German Credit Data Exploration"),
  h3(tags$i("Prashant Mishra")),
  h3(tags$i(Sys.Date())),
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
     wellPanel(
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("CheckingAccountStatus", "CreditHistory", "Purpose", "SavingsAccountBonds","EmploymentDuration","EmploymentDuration","Personal","OtherDebtorsGuarantors","Property","OtherInstallmentPlans","Housing","Job"), 
                  selected = "CheckingAccountStatus"),
      selectInput(inputId = "fill", 
                  label = "Fill color by:",
                  choices = c("Class", "CheckingAccountStatus", "CreditHistory", "Purpose", "SavingsAccountBonds","EmploymentDuration","EmploymentDuration","Personal","OtherDebtorsGuarantors","Property","OtherInstallmentPlans","Housing","Job"), 
                  selected = "Class"),
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Duration", "Amount", "InstallmentRatePercentage", "ResidenceDuration", "Age","NumberExistingCredits","NumberPeopleMaintenance","Telephone","ForeignWorker"), 
                  selected = "Amount")
              ),
     
     
     
     
      wellPanel(
    h3('choose the model'),
    # the actioButton called rpart which is the name of the variable you need to use in the server component
    actionButton('rpart', label = 'Decision Tree',icon("leaf",lib="glyphicon"), 
                 style="color: #fff; background-color: #339933; border-color: #2e6da4"),
    actionButton('rf', label = 'Random Forest', icon("tree-conifer", lib="glyphicon"),
                 style="color: #fff; background-color: #33cc33; border-color: #2e6da4"),
    actionButton('multinom', label = 'Logistic Multinom', icon("random", lib="glyphicon"),
                 style="color: #fff; background-color: #0066ff; border-color: #2e6da4"),
    actionButton('C50', label = 'C50', icon("random", lib="glyphicon"),
                 style="color: #fff; background-color: #ffa500; border-color: #2e6da4"),
    actionButton('nn', label = 'Neural Net', icon("random", lib="glyphicon"),
                 style="color: #fff; background-color: #832525; border-color: #2e6da4"),
    actionButton('nb', label = 'Naive Bayes', icon("random", lib="glyphicon"),
                 style="color: #fff; background-color: #45025b; border-color: #2e6da4"),
    actionButton('ensemble', label = 'Ensemble', icon("certificate", lib="glyphicon"),
                 style="color: #181415; background-color: #ffffff; border-color: #2e6da4"),
    actionButton('benchmark', label = 'Benchmark Experiment', icon("certificate", lib="glyphicon"),
                 style="color: #181415; background-color: #ffffff; border-color: #2e6da4")
    
             )
    
    ),
       
      
  
    
    
    
    # Outputs
    mainPanel(
      
        
        #actionButton(inputId = "button", 
        #             label = "Show",value=TRUE)
      tabsetPanel(type="tabs",
                  tabPanel("barplot_data",
                           h3("Barplot of X-axis variable, for Good and Bad Class panel, color filled by variable chosen in the side panel"),
                           h5("Click on any part of the barplot to see the data associated at the bottom."),
                           br(),
                           plotOutput(outputId = "barplot",click = "barplot_click"),
                           wellPanel(
                             tweaks <- 
                               list(tags$head(tags$style(HTML("
                                                              .multicol { 
                                                              height: 150px;
                                                              -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                                              -moz-column-count: 5;    /* Firefox */ 
                                                              column-count: 5; 
                                                              -moz-column-fill: auto;
                                                              -column-fill: auto;
                                                              } 
                                                              ")) 
                               )),
                             controls <-
                               list(h3("Multicolumn checkboxGroupInput"),
                                    br(),
                                    tags$div(align = 'left', 
                                             class = 'multicol', 
                                             checkboxGroupInput(inputId  = "selected_var", 
                                                                label    = "Select variables:", 
                                                                choices  = names(credit_dataset),
                                                                selected = names(credit_dataset),
                                                                inline   = FALSE)))
                               ),
                           h3("Selected portion of the barplot"),
                           verbatimTextOutput("x_value"),
                           h3("Data associated with the selected portion of the barplot"),
                           h5("Use search filter to select any sub data"),
                           br(),
                           DT::dataTableOutput(outputId="showdata")),
                  tabPanel("scaled_barplot",
                           h3("Scaled Bar plot of X-axis variable, for Good and Bad Class panel, color filled by variable chosen in the side panel"),
                           br(),
                           plotOutput(outputId = "scaledbarplot",click = "scaledbarplot_click")),
                  tabPanel("boxplot",
                           h3("Box plot between X-axis and Y-axis variables, color filled with the variable chosen in the side bar panel."),
                           br(),
                           plotOutput(outputId = "boxplot",click = "boxplot_click")),
                  tabPanel("crosstab",
                           h3("CrossTab values of X-axis data with Class variable."),
                           br(),
                           verbatimTextOutput(outputId = "crosstab")),
                  tabPanel("fulldata",
                           h3("Full Data"),
                           br(),
                           DT::dataTableOutput(outputId="showfulldata")),
                  # tabPanel("TestData",
                  #          h3("Test Data"),
                  #          wellPanel(
                  #            selectInput(inputId = "CheckingAccountStatusChoice", 
                  #                        label = "CheckingAccountStatus",
                  #                        choices = levels(credit_dataset$CheckingAccountStatus), 
                  #                        selected = "none"),
                  #            selectInput(inputId = "CreditHistoryChoice", 
                  #                        label = "CreditHistory",
                  #                        choices = levels(credit_dataset$CreditHistory), 
                  #                        selected = "NoCredit.AllPaid"),
                  #            selectInput(inputId = "PurposeChoice", 
                  #                        label = "Purpose",
                  #                        choices = levels(credit_dataset$Purpose), 
                  #                        selected = "NewCar"),
                  #            selectInput(inputId = "SavingsAccountBondsChoice", 
                  #                        label = "SavingsAccountBonds",
                  #                        choices = levels(credit_dataset$SavingsAccountBonds), 
                  #                        selected = "Unknown"),
                  #            selectInput(inputId = "EmploymentDurationChoice", 
                  #                        label = "EmploymentDuration",
                  #                        choices = levels(credit_dataset$EmploymentDuration), 
                  #                        selected = "Unemployed"),
                  #            selectInput(inputId = "PersonalChoice", 
                  #                        label = "Personal",
                  #                        choices = levels(credit_dataset$Personal), 
                  #                        selected = "Female.NotSingle"),
                  #            selectInput(inputId = "OtherDebtorsGuarantorsChoice", 
                  #                        label = "OtherDebtorsGuarantors",
                  #                        choices = levels(credit_dataset$OtherDebtorsGuarantors), 
                  #                        selected = "None"),
                  #            selectInput(inputId = "PropertyChoice", 
                  #                        label = "Property",
                  #                        choices = levels(credit_dataset$Property), 
                  #                        selected = "Unknown"),
                  #            selectInput(inputId = "OtherInstallmentPlansChoice", 
                  #                        label = "OtherInstallmentPlans",
                  #                        choices = levels(credit_dataset$OtherInstallmentPlans), 
                  #                        selected = "None"),
                  #            selectInput(inputId = "HousingChoice", 
                  #                        label = "Housing",
                  #                        choices = levels(credit_dataset$Housing), 
                  #                        selected = "Rent"),
                  #            selectInput(inputId = "JobChoice", 
                  #                        label = "Job",
                  #                        choices = levels(credit_dataset$Job), 
                  #                        selected = "SkilledEmployee"),
                  #            selectInput(inputId = "TelephoneChoice", 
                  #                        label = "Telephone",
                  #                        choices = levels(credit_dataset$Telephone), 
                  #                        selected = "0"),
                  #            selectInput(inputId = "ForeignWorkerChoice", 
                  #                        label = "ForeignWorker",
                  #                        choices = levels(credit_dataset$ForeignWorker), 
                  #                        selected = "1"),
                  #            selectInput(inputId = "DurationChoice",
                  #                        label="Duration",choices = c('lt.1','1.to.2','2.to.3','3.to.4','4.to.5','5.to.6','6.to.7'),
                  #                        selected = "lt.1"),
                  #            selectInput(inputId = "AgeChoice",
                  #                        label="Age",choices = c('Student','Young','Adult','MidSenior','Senior'),
                  #                        selected = "Student"),
                  #            selectInput(inputId = "AmountChoice",
                  #                        label="Amount",choices = c('0.to.500','500.to.1000','1000.to.1500','1500.to.2500','2500.to.5000','5000.to.7500','7500.to.10000','10000.to.15000','15000.to.20000','20000.to.30000'),
                  #                        selected = "0.to.500"),
                  #            selectInput(inputId = "NumberExistingCreditsChoice",
                  #                        label="NumberExistingCredits",choices = c("1","2","3","4"),
                  #                        selected = "1"),
                  #            selectInput(inputId = "NumberPeopleMaintenanceChoice",
                  #                        label="NumberPeopleMaintenance",choices = c("1","2"),
                  #                        selected = "1"),
                  #            selectInput(inputId = "ResidenceDurationChoice",
                  #                        label="ResidenceDuration",choices = c("1","2","3","4"),
                  #                        selected = "1"),
                  #            selectInput(inputId = "InstallmentRatePercentageChoice",
                  #                        label="InstallmentRatePercentage",choices = c("1","2","3","4"),
                  #                        selected = "1"),
                  #           actionButton("update", "Update Table")
                  #          ),
                  #           
                  #           # sliderInput(inputId = "Duration",label = "Duration",min=1,max=100,step=1,value=12),
                  #           # sliderInput(inputId = "Age",label = "Age",min=18,max=100,step=1,value=20),
                  #           # sliderInput(inputId = "Amount",label = "Amount",min=100,max=20000,step=50,value=1000),
                  #           # sliderInput(inputId = "NumberExistingCredits",label = "NumberExistingCredits",min=1,max=4,step=1,value=2),
                  #           # sliderInput(inputId = "NumberPeopleMaintenance",label = "NumberPeopleMaintenance",min=1,max=2,step=1,value=1),
                  #           # sliderInput(inputId = "ResidenceDuration",label = "ResidenceDuration",min=1,max=4,step=1,value=2),
                  #           # sliderInput(inputId = "InstallmentRatePercentage",label = "InstallmentRatePercentage",min=1,max=4,step=1,value=2)),
                  #          h5("Test Dataset"),
                  #          DT::dataTableOutput(outputId="testdataset"),
                  #          actionButton('add', 'Add Row'),
                  #       #   DT::dataTableProxy(outputId = "testdataset")
                  #          #DT::dataTableOutput(outputId="dummytestdataset")
                  #          verbatimTextOutput("printtest")
                  # ),
                  tabPanel("machineLeaning",
                           h1('Machine Learning Models'),
                           tabsetPanel(type="tabs" ,
                                       
                                       tabPanel("Decision Tree Model result",
                                                h5("1. Model Name"),
                                                verbatimTextOutput("nameRpart"),
                                                h5("2. Model Performance with theoretical threshold (0.17)"),
                                                verbatimTextOutput("resultThRpart"),
                                                h5("3. Model Performance with default threshold (0.5)"),
                                                verbatimTextOutput("resultDefRpart"),
                                                h5("4. Confusion Matrix with theoretical threshold (0.17)"),
                                                verbatimTextOutput('summaryThRpart'),
                                                h5("5. Confusion Matrix with default threshold (0.5)"),
                                                verbatimTextOutput('summaryDefRpart'),
                                                h5("6. Threshold Performance Plot"),
                                                plotOutput('plotRpart'),
                                                h5("7. Predictions based on the Decision Tree Model"),
                                                verbatimTextOutput("predictionRpart")),
                                        
                                       
                                       tabPanel("Random Forest Model result",
                                                h5("1. Model Name"),
                                                verbatimTextOutput("nameRf"),
                                                h5("2. Model Performance with theoretical threshold (0.17)"),
                                                verbatimTextOutput("resultThRf"),
                                                h5("3. Model Performance with default threshold (0.5)"),
                                                verbatimTextOutput("resultDefRf"),
                                                h5("4. Confusion Matrix with theoretical threshold (0.17)"),
                                                verbatimTextOutput('summaryThRf'),
                                                h5("5. Confusion Matrix with default threshold (0.5)"),
                                                verbatimTextOutput('summaryDefRf'),
                                                h5("6. Threshold Performance Plot"),
                                                plotOutput('plotRf'),
                                                h5("7. Predictions based on the Random Forest Model"),
                                                verbatimTextOutput("predictionRf")),
                                       tabPanel("Logistic Model result",
                                                h5("1. Model Name"),
                                                verbatimTextOutput("nameMultinom"),
                                                h5("2. Model Performance with theoretical threshold (0.17)"),
                                                verbatimTextOutput("resultThMultinom"),
                                                h5("3. Model Performance with default threshold (0.5)"),
                                                verbatimTextOutput("resultDefMultinom"),
                                                h5("4. Confusion Matrix with theoretical threshold (0.17)"),
                                                verbatimTextOutput('summaryThMultinom'),
                                                h5("5. Confusion Matrix with default threshold (0.5)"),
                                                verbatimTextOutput('summaryDefMultinom'),
                                                h5("6. Threshold Performance Plot"),
                                                plotOutput('plotMultinom'),
                                                h5("7. Predictions based on the Logistic Model"),
                                                verbatimTextOutput("predictionMultinom")),
                                       tabPanel("Neural Net Model result",
                                                h5("1. Model Name"),
                                                verbatimTextOutput("nameNn"),
                                                h5("2. Model Performance with theoretical threshold (0.17)"),
                                                verbatimTextOutput("resultThNn"),
                                                h5("3. Model Performance with default threshold (0.5)"),
                                                verbatimTextOutput("resultDefNn"),
                                                h5("4. Confusion Matrix with theoretical threshold (0.17)"),
                                                verbatimTextOutput('summaryThNn'),
                                                h5("5. Confusion Matrix with default threshold (0.5)"),
                                                verbatimTextOutput('summaryDefNn'),
                                                h5("6. Threshold Performance Plot"),
                                                plotOutput('plotNn'),
                                                h5("7. Predictions based on the Neural Net Model"),
                                                verbatimTextOutput("predictionNn")),
                                       tabPanel("Naive Bayes Model result",
                                                h5("1. Model Name"),
                                                verbatimTextOutput("nameNb"),
                                                h5("2. Model Performance with theoretical threshold (0.17)"),
                                                verbatimTextOutput("resultThNb"),
                                                h5("3. Model Performance with default threshold (0.5)"),
                                                verbatimTextOutput("resultDefNb"),
                                                h5("4. Confusion Matrix with theoretical threshold (0.17)"),
                                                verbatimTextOutput('summaryThNb'),
                                                h5("5. Confusion Matrix with default threshold (0.5)"),
                                                verbatimTextOutput('summaryDefNb'),
                                                h5("6. Threshold Performance Plot"),
                                                plotOutput('plotNb'),
                                                h5("7. Predictions based on the Naive Bayes Model"),
                                                verbatimTextOutput("predictionNb")),
                                       tabPanel("C50 Model result",
                                                h5("1. Model Name"),
                                                verbatimTextOutput("nameC50"),
                                                h5("2. Model Performance with theoretical threshold (0.17)"),
                                                verbatimTextOutput("resultThC50"),
                                                h5("3. Model Performance with default threshold (0.5)"),
                                                verbatimTextOutput("resultDefC50"),
                                                h5("4. Confusion Matrix with theoretical threshold (0.17)"),
                                                verbatimTextOutput('summaryThC50'),
                                                h5("5. Confusion Matrix with default threshold (0.5)"),
                                                verbatimTextOutput('summaryDefC50'),
                                                h5("6. Threshold Performance Plot"),
                                                plotOutput('plotC50'),
                                                h5("7. Predictions based on the C50 Model"),
                                                verbatimTextOutput("predictionC50")),
                                       tabPanel("Ensemble Model result",
                                                h5("1. Model Name"),
                                                verbatimTextOutput("nameEnsemble"),
                                                h5("2. Model Performance with theoretical threshold (0.17)"),
                                                verbatimTextOutput("resultThEnsemble"),
                                                h5("3. Model Performance with default threshold (0.5)"),
                                                verbatimTextOutput("resultDefEnsemble"),
                                                h5("4. Confusion Matrix with theoretical threshold (0.17)"),
                                                verbatimTextOutput('summaryThEnsemble'),
                                                h5("5. Confusion Matrix with default threshold (0.5)"),
                                                verbatimTextOutput('summaryDefEnsemble'),
                                                h5("6. Threshold Performance Plot"),
                                                plotOutput('plotEnsemble'),
                                                h5("7. Predictions based on the Ensemble Model"),
                                                verbatimTextOutput("predictionEnsemble")),
                                       tabPanel("Benchmark Performance of all models",
                                                h5("1. Benchmark Performance of all models"),
                                                verbatimTextOutput("bmr"),
                                                h5("2. Benchmark Performance plot of all models for cost measure"),
                                                plotOutput("bmr_costplot"),
                                                h5("3. Benchmark Performance plot of all models for training time measure"),
                                                plotOutput("bmr_traintimeplot")),
                                       
                                       
                                                
                                       tabPanel("first 5 rows of the dataframe", verbatimTextOutput("head"))
                           )
                          )
      )
      
      
    #  checkboxGroupInput(inputId = "selected_var",
    #                     label = "Select variables:",
     #                    choices = names(credit_dataset),
     #                    selected = names(credit_dataset))
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  # Create scatterplot object the plotOutput function is expecting
  output$barplot <- renderPlot({
   # if(input$show_barplot){
      ggplot(data = credit_dataset, aes_string(x = input$x, fill = input$fill)) +
        geom_bar()+facet_wrap(~Class)+labs(title = paste0("Counts of each category of ",input$x," color coded by ",input$fill," for each class."))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #  }
    })
  output$scaledbarplot <- renderPlot({
  #  if(input$show_scaledbarplot){
    ggplot(data = credit_dataset, aes_string(x = input$x, fill = input$fill)) +
      geom_bar(position = "fill") + ylab("proportion")+facet_wrap(~Class)+labs(title = paste0("Proportional count of each category of ",input$x," color coded by ",input$fill," for each class"))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #  }
  })
  output$boxplot <- renderPlot({
  #  if(input$show_boxplot){
    ggplot(data = credit_dataset, aes_string(x = input$x, y = input$y, color=input$fill)) +
      geom_boxplot() + facet_wrap(~Class)+labs(title = paste0("Distribution of ",input$y," in each category of ",input$x," for each class, color coded with ",input$fill))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #  }
  })
  
  output$x_value <- renderPrint({
    if (is.null(input$barplot_click$x)) return()
    xval <- credit_dataset %>% pull(input$x)
    lvls <- levels(xval)
    lvls[round(input$barplot_click$x)]
  })
 # observeEvent(input$button, {
 #   cat("Showing table\n")
 # })
  clicked <- eventReactive(input$barplot_click,{
    input$barplot_click
  })
  credit_dataset_selected <- reactive({
    req(input$selected_var)
    if (is.null(clicked()$x)) return()
    panel = clicked()$panelvar1
    xval <- credit_dataset[[input$x]]
    keeprows <- round(clicked()$x) == as.numeric(xval) & credit_dataset$Class==panel
    selected_data <- credit_dataset[keeprows, ]
    selected_data %>% select(input$selected_var) 
  })
  
  output$showdata <- DT::renderDataTable({
 
    DT::datatable(data = credit_dataset_selected(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$showfulldata <- DT::renderDataTable({
    
    DT::datatable(data = credit_dataset, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$crosstab <- renderPrint({
  #  if(input$show_crosstab){
      x <- credit_dataset[[input$x]]
      y <- credit_dataset[["Class"]]
      ct <- CrossTable(x,y)
      print(ct, digits = 3, signif.stars = FALSE)
   # }
  })
  
 
  
  # test <- reactive(
  #   for (c in names(ml_credit_dataset)){
  #     co = strsplit(c,",",fixed=TRUE)[[1]][1]
  #     lev = paste0(strsplit(c,",",fixed=TRUE)[[1]][-1],collapse = ".")
  #     colid = paste0(co,"Choice")
  #     c(c = as.numeric(input$colid == lev))
  #   })
#     data.frame(
#          "CheckingAccountStatus" = c(input$CheckingAccountStatusChoice),
#          "Duration" = c(input$DurationChoice),
#          "CreditHistory" = c(input$CreditHistoryChoice),
#          "Purpose" = c(input$PurposeChoice),
#          "Amount" = c(input$AmountChoice),
#          "SavingsAccountBonds" = c(input$SavingsAccountBondsChoice),
#          "EmploymentDuration" = c(input$EmploymentDurationChoice),
#          "InstallmentRatePercentage" = c(input$InstallmentRatePercentageChoice),
#          "Personal" = c(input$PersonalChoice),
#          "OtherDebtorsGuarantors" = c(input$OtherDebtorsGuarantorsChoice),
#          "ResidenceDuration" = c(input$ResidenceDurationChoice),
#          "Property" = c(input$PropertyChoice),
#          "Age" = c(input$AgeChoice),
#          "OtherInstallmentPlans" = c(input$OtherInstallmentPlansChoice),
#          "Housing" = c(input$HousingChoice),
#          "NumberExistingCredits" = c(input$NumberExistingCreditsChoice),
#          "Job" = c(input$JobChoice),
#          "NumberPeopleMaintenance" = c(input$NumberPeopleMaintenanceChoice),
#          "Telephone" = c(input$TelephoneChoice),
#          "ForeignWorker" = c(input$ForeignWorkerChoice),stringsAsFactors = FALSE
#   )
# )
# 
# 
# 
#     observeEvent(input$update, {  
#       testset = test()
#       output$testdataset <- DT::renderDataTable(
#         testset,server=FALSE,selection=list(target='row+column'),
#         caption="Using a proxy object to manipulate the table",
#         extensions = c("Buttons"), 
#         options = list(dom = 'Bfrtip',
#                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
#         )
#       )
#       output$printtest <- renderPrint(str(testset))
#       
#     })
# proxy = dataTableProxy('testdataset')
# observeEvent(input$add,{
#   proxy %>% addRow(test())
# })
#  output$printtest <- renderPrint({test()}) 
#testdataframe <- as.data.frame(proxy)  
#output$testdf <- renderPrint({
#  testdataframe
#})
  set.seed(1234)
  #n=nrow(ml_credit_dataset)
  #credit.train = ml_credit_dataset[seq(1, n, by = 10),]
  #credit.test = ml_credit_dataset[seq(2, n, by = 10),]
  credit.task = makeClassifTask(data = ml_credit_dataset, target = "Class")
  credit.task = removeConstantFeatures(credit.task)

  rin = makeResampleInstance("CV", iters = 3, task = credit.task,stratify=TRUE)
  costs = matrix(c(0, 1, 5, 0), 2)
  colnames(costs) = rownames(costs) = getTaskClassLevels(credit.task)

  # Theoretical threshold
  th = costs[2,1]/(costs[2,1] + costs[1,2])

  #Credit cost measure
  credit.costs = makeCostMeasure(id = "credit.costs", name = "Credit costs", costs = costs,
                                 best = 0, worst = 5)

  #Benchmark performance of all models
   lrns <- list(makeLearner("classif.rpart", predict.type = "prob", predict.threshold = th),
                makeLearner("classif.multinom", predict.type = "prob", predict.threshold = th, trace = FALSE),
                makeLearner("classif.randomForest", predict.type = "prob", predict.threshold = th, fix.factors.prediction = TRUE),
                makeLearner("classif.naiveBayes", predict.type = "prob", predict.threshold = th),
                makeLearner("classif.nnet", predict.type = "prob", predict.threshold = th),
                makeLearner("classif.C50", predict.type = "prob", predict.threshold = th))
  
 
  measures <-  list(credit.costs, timetrain,mmce)
  

  

  observe({
    #Benchmark Experiment
    observeEvent(input$benchmark,{
      bmr <- benchmark(lrns, credit.task, rin,measures,show.info = FALSE)
      output$bmr <- renderPrint({ bmr
      })
      
      output$bmr_costplot <- renderPlot({
        plotBMRBoxplots(bmr, measure = credit.costs, style = "violin", pretty.names = FALSE) +
          aes(color = learner.id) +
          theme(strip.text.x = element_text(size = 8))
      })
      
      output$bmr_traintimeplot <- renderPlot({
        plotBMRSummary(bmr,measure = timetrain)
      })
    })
    
    # decision tree action button
    observeEvent(input$rpart, {
      lrnRpart = makeLearner("classif.rpart", predict.type = "prob", predict.threshold = th)
      rRpart = resample(lrnRpart, credit.task, resampling = rin, measures = list(credit.costs, mmce), show.info = FALSE)
      ldRpart = generateThreshVsPerfData(rRpart, measures = list(fpr, tpr, credit.costs, mmce))
      modRpart = mlr::train(lrnRpart,credit.task)
      test_dummy <- reactive(dummy.data.frame(test(), sep = "."))
      output$nameRpart <- renderPrint({
        "Decision Tree Model"
      })

      output$resultThRpart<-renderPrint({ rRpart
      })

      output$resultDefRpart<-renderPrint({
        performance(setThreshold(rRpart$pred, 0.5), measures = list(credit.costs, mmce))
      })

      output$summaryThRpart <- renderPrint({calculateConfusionMatrix(rRpart$pred)
      })

      output$summaryDefRpart <- renderPrint({calculateConfusionMatrix(setThreshold(rRpart$pred, 0.5))
      })

      output$plotRpart <- renderPlot({ plotThreshVsPerf(ldRpart, mark.th = th)
      })

      output$predictionRpart <- renderPrint({
        getRRPredictions(rRpart)
      })

    })


    #random forest action button
    observeEvent(input$rf, {
      lrnRf = makeLearner("classif.randomForest", predict.type = "prob", predict.threshold = th)
      rRf = resample(lrnRf, credit.task, resampling = rin, measures = list(credit.costs, mmce), show.info = FALSE)
      ldRf = generateThreshVsPerfData(rRf, measures = list(fpr, tpr, credit.costs, mmce))

      output$nameRf <- renderPrint({
        "Random Forest Model"
      })

      output$resultThRf<-renderPrint({ rRf
      })

      output$resultDefRf<-renderPrint({
        performance(setThreshold(rRf$pred, 0.5), measures = list(credit.costs, mmce))
      })

      output$summaryThRf <- renderPrint({calculateConfusionMatrix(rRf$pred)
      })

      output$summaryDefRf <- renderPrint({calculateConfusionMatrix(setThreshold(rRf$pred, 0.5))
      })

      output$plotRf <- renderPlot({plotThreshVsPerf(ldRf, mark.th = th)
      })

      output$predictionRf <- renderPrint({
        getRRPredictions(rRf)
      })
    })


    # C50 action button called nn
    observeEvent(input$C50, {
      lrnC50 = makeLearner("classif.C50", predict.type = "prob", predict.threshold = th)
      rC50 = resample(lrnC50, credit.task, resampling = rin, measures = list(credit.costs, mmce), show.info = FALSE)
      ldC50 = generateThreshVsPerfData(rC50, measures = list(fpr, tpr, credit.costs, mmce))

      output$nameC50 <- renderPrint({
        "C50 Model"
      })

      output$resultThC50<-renderPrint({ rC50
      })

      output$resultDefC50<-renderPrint({
        performance(setThreshold(rC50$pred, 0.5), measures = list(credit.costs, mmce))
      })

      output$summaryThC50 <- renderPrint({calculateConfusionMatrix(rC50$pred)
      })

      output$summaryDefC50 <- renderPrint({calculateConfusionMatrix(setThreshold(rC50$pred, 0.5))
      })

      output$plotC50 <- renderPlot({ plotThreshVsPerf(ldC50, mark.th = th)
      })
      output$predictionC50 <- renderPrint({
        getRRPredictions(rC50)
      })
    })

    #nn action button
    observeEvent(input$nn, {
      lrnNn = makeLearner("classif.nnet", predict.type = "prob", predict.threshold = th)
      rNn = resample(lrnNn, credit.task, resampling = rin, measures = list(credit.costs, mmce), show.info = FALSE)
      ldNn = generateThreshVsPerfData(rNn, measures = list(fpr, tpr, credit.costs, mmce))

      output$nameNn <- renderPrint({
        "Neural Net Model"
      })

      output$resultThNn<-renderPrint({ rNn
      })

      output$resultDefNn<-renderPrint({
        performance(setThreshold(rNn$pred, 0.5), measures = list(credit.costs, mmce))
      })

      output$summaryThNn <- renderPrint({calculateConfusionMatrix(rNn$pred)
      })

      output$summaryDefNn <- renderPrint({calculateConfusionMatrix(setThreshold(rNn$pred, 0.5))
      })

      output$plotNn <- renderPlot({plotThreshVsPerf(ldNn, mark.th = th)
      })
      output$predictionNn <- renderPrint({
        getRRPredictions(rNn)
      })
    })


    # Naive Bayes action button
    observeEvent(input$nb, {
      lrnNb = makeLearner("classif.naiveBayes", predict.type = "prob", predict.threshold = th)
      rNb = resample(lrnNb, credit.task, resampling = rin, measures = list(credit.costs, mmce), show.info = FALSE)
      ldNb = generateThreshVsPerfData(rNb, measures = list(fpr, tpr, credit.costs, mmce))

      output$nameNb <- renderPrint({
        "Naive Bayes Model"
      })

      output$resultThNb<-renderPrint({ rNb
      })

      output$resultDefNb<-renderPrint({
        performance(setThreshold(rNb$pred, 0.5), measures = list(credit.costs, mmce))
      })

      output$summaryThNb <- renderPrint({calculateConfusionMatrix(rNb$pred)
      })

      output$summaryDefNb <- renderPrint({calculateConfusionMatrix(setThreshold(rNb$pred, 0.5))
      })

      output$plotNb <- renderPlot({plotThreshVsPerf(ldNb, mark.th = th)
      })
      output$predictionNb <- renderPrint({
        getRRPredictions(rNb)
      })
    })

    # Ensemble action button
    observeEvent(input$ensemble, {
      lrnEnsemble = makeStackedLearner(base.learners = lrns, predict.type = "prob", method="hill.climb")
      rEnsemble = resample(lrnEnsemble, credit.task, resampling = rin, measures = list(credit.costs, mmce), show.info = FALSE)
      setThreshold(rEnsemble$pred, th)
      ldEnsemble = generateThreshVsPerfData(rEnsemble, measures = list(fpr, tpr, credit.costs, mmce))

      output$nameEnsemble <- renderPrint({
        "Ensemble Model"
      })

      output$resultThEnsemble<-renderPrint({ performance(setThreshold(rEnsemble$pred, th), measures = list(credit.costs, mmce))
      })

      output$summaryThEnsemble <- renderPrint({calculateConfusionMatrix(setThreshold(rEnsemble$pred, th))
      })

      output$resultDefEnsemble<-renderPrint({
        performance(setThreshold(rEnsemble$pred, 0.5), measures = list(credit.costs, mmce))
      })



      output$summaryDefEnsemble <- renderPrint({calculateConfusionMatrix(setThreshold(rEnsemble$pred, 0.5))
      })

      output$plotEnsemble <- renderPlot({plotThreshVsPerf(ldEnsemble, mark.th = th)
      })
      output$predictionEnsemble <- renderPrint({
        addRRMeasure(rEnsemble, list(credit.costs, mmce))
        getRRPredictions(rEnsemble)
      })
    })

    # Logistic multinom action button called multinom
    observeEvent(input$multinom, {
      lrnMultinom = makeLearner("classif.multinom", predict.type = "prob", predict.threshold = th)
      rMultinom = resample(lrnMultinom, credit.task, resampling = rin, measures = list(credit.costs, mmce), show.info = FALSE)
      ldMultinom = generateThreshVsPerfData(rMultinom, measures = list(fpr, tpr, credit.costs, mmce))

      output$nameMultinom <- renderPrint({
        "Logistic Model"
      })

      output$resultThMultinom<-renderPrint({ rMultinom
      })

      output$resultDefMultinom<-renderPrint({
        performance(setThreshold(rMultinom$pred, 0.5), measures = list(credit.costs, mmce))
      })

      output$summaryThMultinom <- renderPrint({calculateConfusionMatrix(rMultinom$pred)
      })

      output$summaryDefMultinom <- renderPrint({calculateConfusionMatrix(setThreshold(rMultinom$pred, 0.5))
      })

      output$plotMultinom <- renderPlot({
        plotThreshVsPerf(ldMultinom, mark.th = th)
      })
      output$predictionMultinom <- renderPrint({
        getRRPredictions(rMultinom)
      })
    })
    
    

    #print dataframe's sample head
    output$head <- renderPrint({
      head(ml_credit_dataset, 5)
    })
    
    
  })
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)
