library(shiny)
library(leaflet)
##library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
#library(ggplot2)
#library(plotly)
library(forcats)
library(shinyalert)
library(data.table)
#library(ggplot2)
#library(ggfortify)
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

has_cancer.association.rules <- readRDS("has_cancer.association.rules.rds")
has_diabetes.association.rules <- readRDS("has_diabetes.association.rules.rds")
has_hypertension.association.rules <- readRDS("has_hypertension.association.rules.rds")

predict.model_rf <- function(data) {

  model_corr_rf = readRDS("model_corr_rf.rds")
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
    
    titlePanel("Enter the patients data. With only 5 parameters will tell you if the patient is at risk:"),
    
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options ----
        sidebarPanel(
          
          sliderInput("LBXGH", "Glycohemoglobin (%):",
                      min = 0, max = 100,
                      value = 50,
                      animate = TRUE),
          
          # Input: Decimal interval with step value ----
          sliderInput("LBXSGL", "Glucose, refrigerated serum (mg/dL)",
                      min = 10, max = 1000,
                      value = 500, step = 10,
                      animate = TRUE),
          
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
          htmlOutput("resHtml"),
          #tableOutput("values")
          
          
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
             
             
             plotly_arules(has_cancer.association.rules)
             %>%
               layout(autosize = F, width = "100%", height = 500, margin=m)
             
           ))
           ,
           fluidRow(column(
             12,
             h2("Diabetes"),
             
             plotly_arules(has_diabetes.association.rules)
             %>%
               layout(autosize = F, width = "100%", height = 500, margin=m)
             
             
           ))
           ,
           fluidRow(column(
             12,
             h2("Hypertension"),
             
            # plotly_arules(has_hypertension.association.rules)
            # %>%
            #   layout(autosize = F, width = "100%", height = 500, margin=m)
             
           ))
           
           ),
  
  tabPanel("Technical Manual",
           fluidRow(column(
             12,
              htmlOutput("frame")
           ))  ),
  conditionalPanel("false", icon("crosshair"))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive expression to create data frame of all input values ----
  
  output$frame <- renderUI({
    my_test <- tags$iframe(src="./NAHNES.html", height=800, width="100%",frameBorder="0")
    print(my_test)
    my_test
  })
  
  fncPredictModel <- function(data) {
    
    pred <- predict.model_rf(data)
    
    result <- "Has no Diabetes" # Default result
    if (pred =="Yes" ) {
      result <- "Has Diabetes"
    }
    
    res <- paste(
      sep = ,
      "<h3><b>",result,"</b></h3>"
    )
    
    output$resHtml <- renderText({
      res
    })
    
  }
  
  
  observe({
    
    data = data.frame(
      
      "LBXGH" =as.numeric(input$LBXGH),
      "LBXSGL"= as.numeric(input$LBXSGL),
      "RXDUSE"= as.numeric( ifelse(input$RXDUSE=='Yes', 2, 1)),
      "LBDHDDSI"= as.numeric(input$LBDHDDSI),
      "RIDAGEYR" = as.numeric(input$RIDAGEYR))
    #predic the model
      
    fncPredictModel(data)
  }) 
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
