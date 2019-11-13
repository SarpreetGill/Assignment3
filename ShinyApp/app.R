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

#setwd('D:/ProjetosGIT/Assignment-2/ShinyApp')
#prepare_data_frame <- function(data_source) {
mydata <-
  read.csv(
    "./data/working_malaria_data.csv",
    header = TRUE,
    na.strings = c("NA", "", "#NA")
  )
# return(mydata)
#}

#species_list = colnames(mydata[6:31])

species_list = c(
  "An.gambiae_complex" , 
  "An.gambiae.ss",
  "SS.M.Form..An.colluzzi.or.Mopti.forms.", 
  "SS.S.Form..savanah.or.Bamako.forms.",
  "An.arabiensis",
  "An..melas" , 
  "An..merus" , 
  "An.bwambae" ,                           
  "An.funestus..s.l",
  "An.funestus.s.s...specified.",  
  "An.rivulorum" ,  
  "An.leesoni",                            
  "An.parensis",   
  "An.vaneedeni",   
  "An.nili.s.l" ,   
  "An.moucheti.s.l",   
  "An.pharoensis",                         
  "An.hancocki" , 
  "An.mascarensis",  
  "An.marshalli",  
  "An.squamous" ,  
  "An.wellcomei" ,                         
  "An.rufipes" ,   
  "An.coustani.s.l" , 
  "An.ziemanni" ,  
  "An.paludis" )    

species_list = sort(species_list)
country_list = mydata[, 2]

country_list = sort(unique(country_list))
mydata_selected = mydata[1:8]
# Choices for drop-downs
#vars <- c(
#    "Anopheles" = "Anopheles",
#    "Country" = "Country",
#)

#Malaria model

malariaModel <- function() {
  
  # Kmeans
  kclust = readRDS("kmeans.rds")
  
  return(kclust)
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
  "Malaria-Mosquito",
  id = "nav",
  tabPanel(
    "Interactive map",
    div(
      class = "outer",
      
      tags$head(# Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width = "100%", height = "100%"),
      
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 60,
        left = "auto",
        right = 100,
        bottom = "auto",
        width = 330,
        height = "auto",
        
        h2("Mosquito Explorer"),
        
        selectInput("species", "Anopheles Species", species_list),
        #selectInput("countries", "Countries", country_list) ,
        #conditionalPanel("input.species == 'gambiae_c' || countries == 'superzip'",
        # Only prompt for threshold when coloring or sizing by superzip
        #numericInput("threshold", "SuperZIP threshold (top n percentile)", 5))#,
        plotOutput("vectors_per_species", height = 200),
        plotOutput("timeseries", height = 200)
      ),
      
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 130,
        left = "20",
        right = "auto",
        bottom = "auto",
        width = 350,
        height = "auto",
        
        h3("Model precidction"),
        
        # Output: Header + summary of distribution ----
        h4("Summary"),
        #verbatimTextOutput("summary"),
        
        htmlOutput("resHtml")
        
      ),
      
      tags$div(id = "cite", 'Data')
    )
  ),
  
  
  tabPanel("Data explorer",
           fluidRow(
             column(3,
                    selectInput("country", "Country",country_list, multiple=TRUE)
             ),
             column(3,
                    conditionalPanel("input.country",
                                     selectInput("region", "Region", c("All regions"=""), multiple=TRUE)
                    )
             ),
             column(3,
                    conditionalPanel("input.country",
                                     selectInput("locality", "Locality", c("All localities"=""), multiple=TRUE)
                    )
             )
           ),
           hr(),
           DT::dataTableOutput("countrytable")
  ),
  
  
  
  tabPanel("Help!",
           fluidRow(column(
             2,
             titlePanel("This is a short help page"),
             hr(),
             "
             
             The interactive map allows zoom-in, out, and explore the mosquito data for the continent.
             ",
             hr(),
             "
             Additionally the ShinyApp allows explore the data on the 'Data explorer' tab.
             
             ",
             hr(),
             "
             Click on the map to see the predicted cluster number calculated by the k-Means model.
             
             
             "
           ),
           column(
             2,"" 
           )),
           hr()
           ),
  conditionalPanel("false", icon("crosshair"))
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  ##### Malaria Dataset clean ######
  malaria_dataset_clean = read.csv("./data/working_malaria_data.csv", header = TRUE, na.strings = c("NA","","#NA"),fileEncoding ="latin1")[-1]
  

  malaria_species= melt(select(mydata, c(2,6,7,10:35)), id=c('Country','Lat','Long')) %>%
    filter(value=='Yes') %>% filter(!is.na(Lat)) %>%
    rename(Species = variable) %>% 
    select(-value)
 
  plot_species = ifelse(malaria_dataset_clean[,c(10:35)]=="Yes",1,0) %>%
    colSums %>% sort(decreasing=TRUE) %>% stack %>% rev %>% setNames(nm=c('Species', 'MEV')) %>%
    mutate(Species=Species %>% substr(1,13))
  
  by_countries = melt(select(malaria_dataset_clean, c(1,2,4,5,6,10:35)), id=c('Country',"Region",'Locality', 'Lat','Long')) %>%
    filter(value=='Yes')  %>% filter(!is.na(Region)) %>% filter(!is.na(Locality)) %>% filter(!is.na(Lat))  %>%
    rename(Species = variable) %>%
    select(-value)
  
  kmeans.select_data.rds = readRDS("kmeans.select_data.rds")
  kmeans.select_data.rds <- kmeans.select_data.rds %>%
                                select(Lat, Long, k_cluster)
  
  malaria_species <- merge(malaria_species, kmeans.select_data.rds, by=c("Lat","Long")) # NA's match
  
  geoAfrica <- geojsonio::geojson_read("Africa.geojson",
                                       what = "sp")
  
  pal <- colorNumeric("viridis", NULL)  
  #leaflet(geoAfrica) %>%
  #  addTiles() %>%
  #  addPolygons(
  #    stroke = FALSE,
  #    smoothFactor = 0.3,
  #    fillOpacity = 1,
  #    fillColor = ~ pal(log10(strtoi(ID))),
  #    label = ~ paste0(COUNTRY, ": ", formatC(ID, big.mark = ","))
  #  ) %>%
  #  addLegend(pal = pal,
  #            values = 1,
  #            opacity = 1.0)
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(geoAfrica) %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(lng = 12.35,
              lat = 7.36,
              zoom = 3) #%>%
      #addPolygons(
      #  stroke = FALSE,
      #  smoothFactor = 0.3,
      #  fillOpacity = 0.5,
      #  fillColor = ~ pal(log10(as.numeric(ID))),
      #  label = ~ paste0(COUNTRY, ": ", formatC(ID, big.mark = ","))
      #) %>%
      #addLegend(pal = pal, values = ~log10(as.numeric(ID)), opacity = 1.0,
      #          labFormat = labelFormat(transform = function(x) round(10^x)))
  })
  
  
  fncPredictModel <- function(clng, clat) {
    #calculate the model with Lat and Long
    #and return the result.
    
    res <- malariaModel();
    
    pred <- predict.kmeans(clat, clng, malariaModel())
    
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
  
  
  observeEvent(input$map_click, {
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    content <- fncPredictModel(clng, clat)
    leafletProxy('map') %>%
      addCircles(
        lng = clng,
        lat = clat,
        group = 'circles',
        weight = 5,
        radius = 100,
        color = 'brown',
        fillColor = 'orange',
        fillOpacity = 0.5,
        opacity = 1
      )  #%>%
      #addPopups(lng = clng, lat = clat, content)
    
  })
  
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

  
  output$vectors_per_species <- renderPlot({
    # If no zipcodes are in view, don't plot
    #if (nrow(zipsInBounds()) == 0)
    # return(NULL)

    #Most widespread species
    ggplot(plot_species, aes(x = reorder(Species, -MEV), y=MEV, fill=Species)) +
      geom_bar(stat="identity", width =1, colour = 'black', show.legend = FALSE) +
      labs(title="Malarian Entry Vectors Per Species",
           x="Species") +
      theme(axis.text.x = element_text(vjust=0.6, angle=90))
  
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
  
  
  
  observe({
    locality <- if (is.null(input$country)) character(0) else {
      by_countries %>%
        filter(Country %in% input$country,
               is.null(input$region) | Region %in% input$region) %>%
        `$`('Locality') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$locality[input$locality %in% locality])
    updateSelectizeInput(session, "locality", choices = locality,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      #zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      #showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
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
