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

#setwd('D:/ProjetosGIT/Assignment3/ShinyApp')
#prepare_data_frame <- function(data_source) {
mydata <-
  read.csv(
    "./data/working_malaria_data.csv",
    header = TRUE,
    na.strings = c("NA", "", "#NA")
  )
# return(mydata)
#}

#model_list = colnames(mydata[6:31])

model_list = c(
  "Random Forest" , 
  "Logistic Regression" )    

model_list = sort(model_list)
country_list = mydata[, 2]

country_list = sort(unique(country_list))
mydata_selected = mydata[1:8]
# Choices for drop-downs
#vars <- c(
#    "Anopheles" = "Anopheles",
#    "Country" = "Country",
#)

#model_rf
model_rf <- function() {
  
  # Kmeans
  model_rf = readRDS("model_rf.rds")
  
  return(model_rf)
}


predict.kmeans = function (lat, long, model) {
  new_point = data.frame("Lat"=rep(lat, max(model$cluster)), "Long"=rep(long, max(model$cluster)))
  centers = data.frame("Lat"=model$centers[c(1:4)],
                       "Long"=model$centers[c(5:8)])
  dist=sqrt((new_point - centers)^2)
  dist = mutate(dist, Dist=rowSums(dist),
                Group = row_number())
  dist = filter(dist, Dist==min(dist$Dist))
  return(dist$Group)
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
           
          # Output: Table summarizing the values entered ----
          #tableOutput("values")
          tableOutput('values')
          
        )
        
      )   
  ),
  tabPanel("Explore Data - Cool stuff",
           fluidRow(column(
             12,
             
           ))  ),
  
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
    
    listOrder <- order(sqrt((res[["centers"]][,"Lat"] - clat) ^ 2 + (res[["centers"]][,"Long"] - clng) ^ 2))
    
    #closest cluster
    clusterNumer = listOrder[1]
    
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
    #countriesBy <- input$countries
    speciesBy <- input$species
    #
    datafilter <- malaria_species[ malaria_species$Species == speciesBy,  ]
    
    leafletProxy("map", data = datafilter) %>%
      
      #addWebGLHeatmap(size=10,units='px')# %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearShapes() %>%
      clearControls() %>%
      #addMarkers(
      #  clusterOptions = markerClusterOptions()
      #)
      addCircleMarkers(~Long, ~Lat, radius=4, stroke = FALSE, fillOpacity=0.9, fillColor =  ~ pal(as.integer(k_cluster))) %>%
      addLegend(pal = pal, title = "Cluster Number", values = ~as.integer(k_cluster), opacity = 1.0)
    
  }) 
  
  
  
  output$timeseries <- renderPlot({
    # If no species are in view, don't plot
    if (is.null(input$species))
      return()
    
    # Time series plot###############
    time_series = ts(ifelse(malaria_dataset_clean[,c(10:35)]=="Yes",1,0),
                     start=c(1898), end=c(2016), frequency=1)
    #  #autoplot(plot_data[,1:26], facets =TRUE) # All
    autoplot(time_series[,c(input$species)],  xlab = "Time",
             main =paste("Evolution of ", input$species) )  + 
      scale_y_continuous(breaks=c(0.00,1.00),labels=c("No", "Yes")) +
      theme(plot.title = element_text(hjust = 0.5)) 
    
  })
  
  ## Data Explorer ###########################################
  observe({
    region <- if (is.null(input$country)) character(0) else {
      filter(by_countries, Country %in% input$country) %>%
        `$`('Region') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$region[input$region %in% region])
    updateSelectizeInput(session, "region", choices = region,
                         selected = stillSelected, server = TRUE)
  })
  
  
  output$countrytable <- DT::renderDataTable({
    df <-  by_countries %>%
      filter(
        is.null(input$country) | Country %in% input$country,
        is.null(input$region) | Region %in% input$region,
        is.null(input$locality) | Locality %in% input$locality
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    #DT::datatable(df, escape = FALSE)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
