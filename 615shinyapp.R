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
LR <- read.csv("~/Desktop/LRshiny.csv")
LR <- LR[,-1]
LR <- LR %>% 
  rename("stop1_name" = "stop_name.x",
         "stop2_name" = "stop_name.y",
         "stop1_lat" = "stop_lat.x",
         "stop2_lat" = "stop_lat.y",
         "stop1_lon" = "stop_lon.x",
         "stop2_lon" = "stop_lon.y")
HR <- read.csv("~/Desktop/HRshiny.csv")
HR <- HR %>% 
  rename("stop1_name" = "stop_name.x",
         "stop2_name" = "stop_name.y",
         "stop1_lat" = "stop_lat.x",
         "stop2_lat" = "stop_lat.y",
         "stop1_lon" = "stop_lon.x",
         "stop2_lon" = "stop_lon.y")
data <- rbind(LR,HR)
mapdata <- data %>%
  group_by(stop1_name,stop2_name) %>%
  summarise_at(vars("stop1_lat","stop1_lon","stop2_lat","stop2_lon"), mean)
shinydata <- data %>%
  group_by(day_name,season,route_id,stop1_name,stop2_name) %>%
  summarise_at(vars("stop1_lat","stop1_lon","stop2_lat","stop2_lon","avg_time"), mean)




LRfrom_stopdata <- LR %>% dplyr::select(route_id,stop1_name, stop1_lat,stop1_lon)
LRto_stopdata <- LR %>% dplyr::select(route_id,stop2_name, stop2_lat,stop2_lon)
HRfrom_stopdata <- HR %>% dplyr::select(route_id,stop1_name, stop1_lat,stop1_lon)
HRto_stopdata <- HR %>% dplyr::select(route_id,stop2_name, stop2_lat,stop2_lon)

from_stopdata <- rbind(LRfrom_stopdata,HRfrom_stopdata)
to_stopdata <- rbind(LRto_stopdata,HRto_stopdata)

fromstop <- from_stopdata %>%
  group_by(route_id,stop1_name) %>%
  summarise_at(vars("stop1_lat","stop1_lon"), mean)
tostop <- to_stopdata %>%
  group_by(route_id,stop2_name) %>%
  summarise_at(vars("stop2_lat","stop2_lon"), mean)

fromstop <- fromstop[complete.cases(fromstop$stop1_lat) | 
                       complete.cases(fromstop$stop1_lon),]
tostop <- tostop[complete.cases(tostop$stop2_lat) | 
                   complete.cases(tostop$stop2_lon),]

From <- unique(shinydata$stop1_name)
To <- unique(shinydata$stop2_name)
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
             tabPanel("With location selection",
                      fluidPage(
                        selectInput("stop1", "From:", From),
                        selectInput("stop2", "To:", To),
                        checkboxGroupInput("season","Season?",Season),
                        checkboxGroupInput("weekday","What day?",Weekday),
                        leafletOutput("map2"))))

server <- function(input, output) {
  output$map1 <- renderLeaflet({ 
    leaflet() %>%
      addTiles %>%
      # Base map
      addProviderTiles(providers$CartoDB.Positron) %>% 
      # Centering view on NYC this time and zoom out a bit
      setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>%
      # Passing the column lon and lat as lng and lat inputs for
      # the function. Using the column reason for popups.
      addMarkers(lng = fromstop$stop1_lon, 
                 lat = fromstop$stop1_lat,
                 popup = fromstop$stop1_name) %>%
      addMarkers(lng = tostop$stop2_lon, 
                 lat = tostop$stop2_lat,
                 popup = tostop$stop2_name)})
  output$map2 <- renderLeaflet({
        leaflet() %>%
          addTiles %>%
          # Base map
          addProviderTiles(providers$CartoDB.Positron) %>% 
          # Centering view on NYC this time and zoom out a bit
          setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>%
          # Passing the column lon and lat as lng and lat inputs for
          # the function. Using the column reason for popups.
          addMarkers(lng = stop1_lon, 
                     lat = stop1_lat,
                     popup = stop1_name,
                     data= filter(shinydata,season == input$season, day_name == input$weekday, stop1_name == input$stop1,
                                  stop1_name == input$stop1)) %>%
          addMarkers(lng = stop2_lon, 
                     lat = shinydata$stop2_lat,
                     popup = shinydata$stop2_name,
                     data= filter(shinydata,season == input$season, day_name == input$weekday, stop2_name == input$stop2,
                     stop2_name == input$stop2))})
  output$text1 <- renderText({ 
    paste("transportation type", input$route_id,"travel time",input$avg_time)})
  }
shinyApp(ui = ui, server = server)


