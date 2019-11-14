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

#setwd('D:/ProjetosGIT/Assignment3/ShinyApp')
#prepare_data_frame <- function(data_source) {
individuals_transaction_class <- read.transactions('../Data/Working/transactiondata.csv', format = 'basket',sep=',')

has_cancer.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.4, maxlen=15), appearance=list(default="lhs", rhs="HAS CANCER"))
has_diabetes.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.7, maxlen=15), appearance=list(default="lhs", rhs="HAS DIABETES"))
has_hypertension.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.8, maxlen=15), appearance=list(default="lhs", rhs="HAS HYPERTENSION"))

has_cancer.association.rules_smallitemset <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.1, maxlen=3), appearance=list(default="lhs", rhs="HAS CANCER"))
has_diabetes.association.rules_smallitemset <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.4, maxlen=3), appearance=list(default="lhs", rhs="HAS DIABETES"))
has_hypertension.association.rules_smallitemset <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.4, maxlen=3), appearance=list(default="lhs", rhs="HAS HYPERTENSION"))

no_cancer.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.9,maxlen=10), appearance=list(default="lhs", rhs="NO CANCER"))
no_diabetes.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.9, maxlen=10), appearance=list(default="lhs", rhs="NO DIABETES"))
no_hypertension.association.rules <- apriori(individuals_transaction_class, parameter = list(supp=0.001, conf=0.8, maxlen=10), appearance=list(default="lhs", rhs="NO HYPERTENSION"))


top20cancerrules <- head(has_cancer.association.rules, n=20, by="confidence")
top20cancerrules_smallitemset <- head(has_cancer.association.rules_smallitemset, n=20, by="confidence")

top20diabetesrules <- head(has_diabetes.association.rules, n=20, by="confidence")
top20diabetesrules_smallitemset <- head(has_diabetes.association.rules_smallitemset, n=20, by="confidence")

top20hypertensionrules <- head(has_hypertension.association.rules, n=20, by="confidence")
top20hypertensionrules_smallitemset <- head(has_hypertension.association.rules_smallitemset, n=20, by="confidence")



#model_rf
model_rf <- function() {
  
  # Kmeans
  model_rf = readRDS("model_rf.rds")
  
  return(model_rf)
}

predict.model_rf <- function(data) {

  pred <- predict(model_rf, data)
  return(pred)
}

# Define UI for application that draws a histogram
ui <- navbarPage(
  "NAHNES Tool",
  id = "nav",
  
  tabPanel(
    "Study patient",
        
        # App title ----
        #titlePanel("Investigate patient"),
  
    titlePanel("Enter the patients data:"),
    
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options ----
        sidebarPanel(
          
          sliderInput("LBXGH", "Glycohemoglobin (%):",
                      min = 0, max = 100,
                      value = 50),
          
          # Input: Decimal interval with step value ----
          sliderInput("LBXSGL", "Glucose, refrigerated serum (mg/dL)",
                      min = 0, max = 1,
                      value = 0.5, step = 0.1),
          
          # Input: Specification of range within an interval ----
          selectizeInput('RXDUSE', 'In the past 30 days, did you take medication?',
                         choices = c("Yes","No")),
          
          # Input: Custom currency format for with basic animation ----
          sliderInput("LBDHDDSI", "HDL-Cholesterol (mmol/L)",
                      min = 0, max = 100,
                      value = 0, step = 25,
                      animate = TRUE),
          
          # Input: Animation with custom interval (in ms) ----
          # to control speed, plus looping
          sliderInput("RIDAGEYR", "Age in years:",
                      min = 1, max = 120,
                      value = 1, step = 5,
                      animate =
                        animationOptions(interval = 300, loop = TRUE))
        ),
        
        
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          h2("This patient is likely to:"),
          h1("Have"),
          h2("Cancer"),
          # Output: Table summarizing the values entered ----
          #tableOutput("values")
          htmlOutput("resHtml"),
          tableOutput('values')
          
        )
        
      )   
  ),
  tabPanel("Interactive plots",
           fluidRow(column(
             12,
             h2("Interactive Graph Visualization"),
             
             
             
           )),
           fluidRow(column(
             12,
             h2("Cancer"),
             
             
             plotly_arules(has_cancer.association.rules)
             %>%
               layout(autosize = F, width = "100%", height = 500)
             
           ))
           ,
           fluidRow(column(
             12,
             h2("Diabetes"),
             
             plotly_arules(has_diabetes.association.rules)
             %>%
               layout(autosize = F, width = "100%", height = 500)
             
             
           ))
           ,
           fluidRow(column(
             12,
             h2("Hypertension"),
             
             plotly_arules(has_hypertension.association.rules)
             %>%
               layout(autosize = F, width = "100%", height = 500)
             
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
  
  
  # Reactive expression to create data frame of all input values ----
  
  sliderValues <- reactive({
    
    data.frame(
      Name = c("LBXGH",
               "LBXSGL",
               "RXDUSE",
               "LBDHDDSI",
               "RIDAGEYR"),
     Value = c(as.character(input$LBXGH),
               as.character(input$LBXSGL),
               input$RXDUSE,
               as.character(input$LBDHDDSI),
               as.character(input$RIDAGEYR))
    )
    
    #predic the model
    predict.model_rf(data)    
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  #kmeans.select_data.rds = readRDS("kmeans.select_data.rds")
  #kmeans.select_data.rds <- kmeans.select_data.rds %>%
  #  select(Lat, Long, k_cluster)
  
  fncPredictModel <- function(clng, clat) {
    #calculate the model with Lat and Long
    #and return the result.
    
    #res <- malariaModel();
    
    #pred <- predict.kmeans(clat, clng, malariaModel())
    
    htmltext <- paste(
      sep = '',
      "<h4>The coordinates:</h4>",
      "Lat: ",
      clat,
      "\n",
      "Long: ",
      clng,
      "Based on the location selected our model is predicting cluster number:</h4>",
      "",clusterNumer,"\n"
    )
    
    resHtml <- paste(
      sep = ,
      "<h4>The coordinates you selected:</h4>",
      "Lat: <b>",
      clat,
      "</b>",
      "<br>Long: <b>",
      clng,
      "<h4>Based on the location selected our model is predicting cluster number:</h4>",
      "<h3>",clusterNumer,"</h3>",
      "</b>"
    )
    
    output$summary <- renderPrint({
      htmltext
    })
    
    output$resHtml <- renderText({
      resHtml
    })
    
    result <- resHtml
  }
  
  observe({
    data = c(as.character(input$LBXGH),
              as.character(input$LBXSGL),
              input$RXDUSE,
              as.character(input$LBDHDDSI),
              as.character(input$RIDAGEYR))
  
    
    #predic the model
    predict.model_rf(data)    
    fncPredictModel(data)
  }) 
  

  
  ## Data Explorer ###########################################
  observe({

      })
  
  
  output$countrytable <- DT::renderDataTable({
  #  df <-  by_countries %>%
  #    filter(
  #      is.null(input$country) | Country %in% input$country,
  #      is.null(input$region) | Region %in% input$region,
  #      is.null(input$locality) | Locality %in% input$locality
  #    ) %>%
  #    mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <- DT::dataTableAjax(session, df)
    
    #DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    #DT::datatable(df, escape = FALSE)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
