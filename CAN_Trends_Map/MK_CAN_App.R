### Shiny R experimental web app showing MannKendall baseflow and streamflow trends for Canada ###
# Written by: Ethan McTavish

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(leaflet)
library(htmltools)
library(tidyverse)

# Load data -----
CAN_MK_Trends_SF <- readRDS("data/CAN_MK_Trends_SF.RDS")

CAN_MK_Trends_BF <- readRDS("data/CAN_MK_Trends_BF.RDS")

# Define UI for application
ui <- dashboardPage(
  skin = "green",
  # title ----
  dashboardHeader(title = "MannKendall Trends"),
  
  # sidebar ----
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                menuItem("Flow Map", tabName = "page1", icon = icon("tint", lib = "glyphicon")),
                menuItem("Data", tabName = "page2", icon = icon("list-alt", lib = "glyphicon")),
                conditionalPanel(
                  'input.sidebarid == "page1"', 
                  sliderTextInput(inputId = "Month",
                                  "Month of the Year:",
                                  choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                  selectInput(inputId = "Type",
                              "Select a flow type:",
                              choices = c("Streamflow", "Baseflow")),
                  useShinyalert(),
                  actionButton("button", "Toggle Land Use"),
                  
                ),
                conditionalPanel(
                  'input.sidebarid == "page2"', 
                  selectInput(inputId = "Dataset",
                              "Select a dataset:",
                              choices = c("Streamflow", "Baseflow"))
                  
                )
    )  
  ),
  
  # body ----
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"), #100vh - 80px was the  orig.
    tabItems(
      # page 1 ----
      tabItem(tabName = "page1", "Map content. This map shows the Mannkendall trends for Canada.", 
              tabPanel("Streamflow Trends", leafletOutput(outputId = "map"))),
      # page 2 ----
      tabItem(tabName = "page2", 
              "Data content. This data was used for the map.",
              br(), br(),
              tabPanel("Data", dataTableOutput(outputId = "data"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  pal_trend <- colorFactor(c("red","green", "white"), domain = c("Increasing", "Decreasing", "No_trend")) # sets the colours for the data points
  
  output$map = renderLeaflet({
    
    leaflet() %>% addTiles() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery (Default") %>%
      addProviderTiles(providers$Stamen.TerrainLabels, group = "Labels") %>%
      addCircleMarkers(data = CAN_MK_Trends_SF,
                       ~LONGITUDE, ~LATITUDE, 
                       fillColor = ~pal_trend(Trend),
                       color = "black",
                       stroke = TRUE, fillOpacity = 0.5, radius = 10,
                       group = "myMarkers") %>%
      addLayersControl(baseGroups = c("WorldImagery (Default)", "OpenStreetMap", "Labels")) %>%
      setView(lng = -106.3468, lat = 56.1304, zoom = 4) %>%
      addMiniMap(position = "topright",
                 toggleDisplay = TRUE,
                 minimized = FALSE) %>%
      addLegend("bottomright", 
                pal = pal_trend, 
                values = CAN_MK_Trends_SF$Trend,
                title = "Trends",
                opacity = 1) })
  
  # change datapoints based on user input
  toInput <- reactive({
    list(input$Month, input$Type)
  })
  
  observeEvent(toInput(), {
    proxy <- leafletProxy("map")
    
    if (input$Month == "Jan" && input$Type == "Streamflow") {
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 1)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    } 
    
    if (input$Month == "Jan" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 1)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    } 
    
    if (input$Month == "Feb" && input$Type == "Streamflow") {
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 2)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    } 
    
    if (input$Month == "Feb" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 2)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    } 
    
    if (input$Month == "Mar" && input$Type == "Streamflow") {
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 3)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Mar" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 3)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    } 
    if (input$Month == "Apr" && input$Type == "Streamflow"){
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 4)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Apr" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 4)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    } 
    
    if (input$Month == "May" && input$Type == "Streamflow"){
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 5)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "May" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 5)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    } 
    
    if (input$Month == "Jun" && input$Type == "Streamflow") {
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 6)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Jun" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 6)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    } 
    
    if (input$Month == "Jul" && input$Type == "Streamflow") {
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 7)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Jul" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 7)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Aug" && input$Type == "Streamflow") {
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 8)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Aug" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 8)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Sep" && input$Type == "Streamflow") {
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 9)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Sep" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 9)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Oct" && input$Type == "Streamflow") {
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 10)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Oct" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 10)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Nov" && input$Type == "Streamflow") {
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 11)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Nov" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 11)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    
    if (input$Month == "Dec" && input$Type == "Streamflow") {
      
      df_Month <- filter(CAN_MK_Trends_SF, Month == 12)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
    if (input$Month == "Dec" && input$Type == "Baseflow") {
      
      df_Month <- filter(CAN_MK_Trends_BF, Month == 12)
      
      labels = sprintf("<strong>Station Number =</strong> %s<br/><strong>tau =</strong> %g<br/><strong> sl =</strong> %g<br/><strong> Month =</strong> %g", 
                       df_Month$STATION_NUMBER, df_Month$tau, df_Month$sl, df_Month$Month) %>%
        lapply(htmltools::HTML)
      
      proxy %>% clearGroup("myMarkers") %>%
        addCircleMarkers(data = df_Month,
                         ~LONGITUDE, ~LATITUDE, 
                         fillColor = ~pal_trend(Trend),
                         color = "black",
                         stroke = TRUE, fillOpacity = 0.5, radius = 10,
                         label = labels,
                         group = "myMarkers")
    }
    
  })
  
  
  observeEvent(input$button, {
    shinyalert("Still needs to be added :(")
  })
  
  # change the dataset based on user input
  datasetInput <- reactive({
    
    if (input$Dataset == "Streamflow") {
      dataset <- CAN_MK_Trends_SF
    } else if (input$Dataset == "Baseflow") {
      dataset <- CAN_MK_Trends_BF
    }
    return(dataset)
  })
  
  output$data <- renderDataTable({
    datasetInput()
  }, options = list(lengthMenu = c(5,10,30,50), pageLength = 5, scrollX = TRUE))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
