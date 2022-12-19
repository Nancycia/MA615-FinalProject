library(ggplot2)
library(ggnewscale)
library(readr)
library(raster)
library(sf)
library(rgdal)
library(rgeos)
library(leaflet)
library(tidyverse)
library(shiny)
library(shinythemes)
library(dbplyr)

LR <- read.csv("https://raw.githubusercontent.com/Nancycia/MA615-FinalProject/main/LRshiny.csv")
LR <- LR[,-1]
LR$traveltime_mins <- round(LR$avg_time/60)
LR <- LR %>% 
  rename("from_stop_name" = "stop_name.x",
         "to_stop_name" = "stop_name.y",
         "stop1_lat" = "stop_lat.x",
         "stop2_lat" = "stop_lat.y",
         "stop1_lon" = "stop_lon.x",
         "stop2_lon" = "stop_lon.y")
HR <- read.csv("https://raw.githubusercontent.com/Nancycia/MA615-FinalProject/main/HRshiny.csv")
HR$traveltime_mins <- round(HR$avg_time/60)
HR <- HR %>% 
  rename("from_stop_name" = "stop_name.x",
         "to_stop_name" = "stop_name.y",
         "stop1_lat" = "stop_lat.x",
         "stop2_lat" = "stop_lat.y",
         "stop1_lon" = "stop_lon.x",
         "stop2_lon" = "stop_lon.y")
data <- rbind(LR,HR)

shinydata <- data %>%
  group_by(day_name,season,route_id,from_stop_name,to_stop_name) %>%
  summarise_at(vars("stop1_lat","stop1_lon","stop2_lat","stop2_lon","traveltime_mins"), mean)


LRfrom_stopdata <- LR %>% dplyr::select(route_id,from_stop_name, stop1_lat,stop1_lon)
LRto_stopdata <- LR %>% dplyr::select(route_id,to_stop_name, stop2_lat,stop2_lon)
HRfrom_stopdata <- HR %>% dplyr::select(route_id,from_stop_name, stop1_lat,stop1_lon)
HRto_stopdata <- HR %>% dplyr::select(route_id,to_stop_name, stop2_lat,stop2_lon)

from_stopdata <- rbind(LRfrom_stopdata,HRfrom_stopdata)
to_stopdata <- rbind(LRto_stopdata,HRto_stopdata)

fromstop <- from_stopdata %>%
  group_by(route_id,from_stop_name) %>%
  summarise_at(vars("stop1_lat","stop1_lon"), mean)
tostop <- to_stopdata %>%
  group_by(route_id,to_stop_name) %>%
  summarise_at(vars("stop2_lat","stop2_lon"), mean)

fromstop <- fromstop[complete.cases(fromstop$stop1_lat) | 
                       complete.cases(fromstop$stop1_lon),]
tostop <- tostop[complete.cases(tostop$stop2_lat) | 
                   complete.cases(tostop$stop2_lon),]

From <- unique(shinydata$from_stop_name)
To <- unique(shinydata$to_stop_name)
Season <- unique(shinydata$season)
Weekday <- unique(shinydata$day_name)

shinydata<- shinydata[complete.cases(shinydata$stop1_lat) | 
                        complete.cases(shinydata$stop1_lon),]
shinydata<- shinydata[complete.cases(shinydata$stop2_lat) | 
                        complete.cases(shinydata$stop2_lon),]

ui <- 
  navbarPage("Massachusetts Bay Transportation Authority", collapsible = TRUE, inverse = TRUE, 
             theme = shinytheme("cerulean"),
             tabPanel("Map", leafletOutput("map1")),
             tabPanel("Check travel time with selection",
                      fluidPage(
                        selectInput("stop1", "From:", From),
                        selectInput("stop2", "To:", To),
                        checkboxGroupInput("season","Which season you want to go?",Season),
                        checkboxGroupInput("weekday","Which day you want to go?",Weekday),
                        textOutput("text"),
                        leafletOutput("map2"),
                        dataTableOutput("datatable")
                      )))

server <- function(input, output) {
  shiny <- reactive({
  shinydata %>% filter(season%in%input$season, day_name%in%input$weekday, from_stop_name%in%input$stop1, to_stop_name%in%input$stop2)
})
  output$map1 <- renderLeaflet({ 
    leaflet() %>%
      addTiles %>%
      # Base map
      addProviderTiles(providers$CartoDB.Positron) %>% 
      # Centering view on Boston this time and zoom out a bit
      setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>%
      # Passing the column lon and lat as lng and lat inputs for
      # the function. Using the column reason for popups.
      addMarkers(lng = fromstop$stop1_lon, 
                 lat = fromstop$stop1_lat,
                 popup = fromstop$from_stop_name) %>%
      addMarkers(lng = tostop$stop2_lon, 
                 lat = tostop$stop2_lat,
                 popup = tostop$to_stop_name)})
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles %>%
      # Base map
      addProviderTiles(providers$CartoDB.Positron) %>% 
      # Centering view on Boston this time and zoom out a bit
      setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>%
      # Passing the column lon and lat as lng and lat inputs for
      # the function. Using the column reason for popups.
      addMarkers(lng = shiny()$stop1_lon, 
                 lat = shiny()$stop1_lat,
                 popup = shiny()$from_stop_name)%>%
      addMarkers(lng = shiny()$stop2_lon, 
                 lat = shiny()$stop2_lat,
                 popup = shiny()$to_stop_name)})
  output$datatable <- renderDataTable(filter(shinydata,season == input$season, day_name == input$weekday, from_stop_name == input$stop1, to_stop_name == input$stop2),options = list(pageLength = 5))
  output$text <- renderText({ 
    paste("The travel from ",input$stop1, "to",input$stop2, "in", input$season,"on",input$weekday)})
}
shinyApp(ui = ui, server = server)
