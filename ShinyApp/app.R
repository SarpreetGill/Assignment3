library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(shinyalert)
library(data.table)
#library(ggplot2)
library(ggfortify)
library(arules)
library(arulesViz)


m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)

#setwd('D:/ProjetosGIT/Assignment3/ShinyApp')
#prepare_data_frame <- function(data_source) {
#individuals_transaction_class <- read.transactions('../Data/Working/transactiondata.csv', format = 'basket',sep=',')

#has_cancer.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.4, maxlen=15), appearance=list(default="lhs", rhs="HAS CANCER"))
#has_diabetes.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.7, maxlen=15), appearance=list(default="lhs", rhs="HAS DIABETES"))
#has_hypertension.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.8, maxlen=15), appearance=list(default="lhs", rhs="HAS HYPERTENSION"))

#has_cancer.association.rules_smallitemset <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.1, maxlen=3), appearance=list(default="lhs", rhs="HAS CANCER"))
#has_diabetes.association.rules_smallitemset <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.4, maxlen=3), appearance=list(default="lhs", rhs="HAS DIABETES"))
#has_hypertension.association.rules_smallitemset <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.4, maxlen=3), appearance=list(default="lhs", rhs="HAS HYPERTENSION"))

#no_cancer.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.9,maxlen=10), appearance=list(default="lhs", rhs="NO CANCER"))
#no_diabetes.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.9, maxlen=10), appearance=list(default="lhs", rhs="NO DIABETES"))
#no_hypertension.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.8, maxlen=10), appearance=list(default="lhs", rhs="NO HYPERTENSION"))

#top20cancerrules <- head(has_cancer.association.rules, n=20, by="confidence")
#top20cancerrules_smallitemset <- head(has_cancer.association.rules_smallitemset, n=20, by="confidence")

#top20diabetesrules <- head(has_diabetes.association.rules, n=20, by="confidence")
#top20diabetesrules_smallitemset <- head(has_diabetes.association.rules_smallitemset, n=20, by="confidence")

#top20hypertensionrules <- head(has_hypertension.association.rules, n=20, by="confidence")
#top20hypertensionrules_smallitemset <- head(has_hypertension.association.rules_smallitemset, n=20, by="confidence")



#model_rf
model_corr_rf <- function() {
  
  # LR
  model_corr_rf = readRDS("model_corr_rf.rds")
  
  return(model_corr_rf)
}

predict.model_rf <- function(data) {

  pred <- predict(model_corr_rf, data)
  
  result <- "NO" # Default result
  if (pred == "X1" ) {
    result <- "YES"
  }
  return(result)
  
}

# Define UI for application that draws a histogram
ui <- navbarPage(
  "NAHNES Tool",
  id = "nav",
  
  tabPanel(
    "Study patient",
        
        # App title ----
        #titlePanel("Investigate patient"),
  
    titlePanel("Enter the patients data. With only 5 parameters will tell you if the patient is at risk:"),
    
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options ----
        sidebarPanel(
          
          sliderInput("LBXGH", "Glycohemoglobin (%):",
                      min = 0, max = 100,
                      value = 50),
          
          # Input: Decimal interval with step value ----
          sliderInput("LBXSGL", "Glucose, refrigerated serum (mg/dL)",
                      min = 10, max = 1000,
                      value = 500, step = 10),
          
          # Input: Specification of range within an interval ----
          selectizeInput('RXDUSE', 'In the past 30 days, did you take medication?',
                         choices = c("Yes","No")),
          
          # Input: Custom currency format for with basic animation ----
          sliderInput("LBDHDDSI", "HDL-Cholesterol (mmol/L)",
                      min = 0, max = 5,
                      value = 1.5, step = 0.1,
                      animate = TRUE),
          
          # Input: Animation with custom interval (in ms) ----
          # to control speed, plus looping
          sliderInput("RIDAGEYR", "Age in years:",
                      min = 15, max = 99,
                      value = 50, step = 1,
                      animate = TRUE)
        ),
        
        
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          h2("This patient is likely to:"),
          textOutput("resHtml")
          
        )
        
      )   
  ),
  tabPanel("Interactive plots",
           fluidRow(column(
             12,
             h2("Data Explorer - Interactive Graph Visualization"),
             
             
             
           )),
           fluidRow(column(
             12,
             h2("Cancer"),
             
             
             #plotly_arules(has_cancer.association.rules)
             #%>%
            #   layout(autosize = F, width = "100%", height = 500, margin=m)
             
           ))
           ,
           fluidRow(column(
             12,
             h2("Diabetes"),
             
             #plotly_arules(has_diabetes.association.rules)
             #%>%
            #   layout(autosize = F, width = "100%", height = 500, margin=m)
             
             
           ))
           ,
           fluidRow(column(
             12,
             h2("Hypertension"),
             
             #plotly_arules(has_hypertension.association.rules)
             #%>%
            #   layout(autosize = F, width = "100%", height = 500, margin=m)
             
           ))
           
           ),
  
  tabPanel("Technical Manual",
           fluidRow(column(
             12,
             includeHTML("NAHNES.html")
           ))  ),
  conditionalPanel("false", icon("crosshair"))
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  output$resHtml <- "Test"
  
  # Reactive expression to create data frame of all input values ----
  
  sliderValues <- reactive({
    
    data = data.frame(
      LBXGH =as.numeric(input$LBXGH),
      LBXSGL= as.numeric(input$LBXSGL),
      RXDUSE= as.numeric( ifelse(input$RXDUSE=='YES', 2, 1)),
      LBDHDDSI= as.numeric(input$LBDHDDSI),
      RIDAGEYR = as.numeric(input$RIDAGEYR))
    
    #predic the model
    fncPredictModel(data)
    
  })
  
  # Show the values in an HTML table ----
  #output$values <- renderText({
  #  sliderValues()
  #})
  
  #kmeans.select_data.rds = readRDS("kmeans.select_data.rds")
  #kmeans.select_data.rds <- kmeans.select_data.rds %>%
  #  select(Lat, Long, k_cluster)
  
  fncPredictModel <- function(data) {
    
    pred_probs <- predict.model_rf(data)
    
    result <- "Has no Diabetes" # Default result
    if (pred =="Yes" ) {
      result <- "Has Diabetes"
    }
    
    resHtml <- paste(
      sep = ,
      "<h3>",result,"</h3>",
      "</b>"
    )
    
    output$resHtml <- renderText({
      resHtml
    })
    
  }
  
  #observe({
  #  
  #  data = data.frame(
  #    LBXGH =as.numeric(input$LBXGH),
  #    LBXSGL= as.numeric(input$LBXSGL),
  #    RXDUSE= as.numeric( ifelse(input$RXDUSE=='YES', 2, 1)),
  #    LBDHDDSI= as.numeric(input$LBDHDDSI),
  #    RIDAGEYR = as.numeric(input$RIDAGEYR))
  #  #predic the model
  #    
  #  fncPredictModel(data)
  #}) 
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
