# Initial mapping with leaflet 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinyWidgets)
library(leaflet)

# Creating a sample df

Test123 <- data.frame(
  points = c('1','2','3','4','5','6','7',
             'A', 'B', 'C','D', 'E', 'F', 'G'),
  lat = c('40.775750','40.576861','40.726972','40.586183','40.773294','40.776312','40.776772',
          '40.626536','40.716650','40.736762','40.656875','40.996988','40.776100','40.256215'),
  lng = c('-111.994693','-111.904780','-111.974869','-111.654955','-111.444140','-111.214225','-111.934669', 
          '-111.444495','-111.913580','-111.924668','-111.164755','-111.714840','-111.844928','-111.694115')
)
# creating groups in dataframe 

Test123$group[Test123$points == 'A'|Test123$points == 'B'|Test123$points =='C'|Test123$points =='D'|Test123$points =='E'|
                Test123$points =='F'|Test123$points =='G'] <- 'Letters'

Test123$group[Test123$points == '1'|Test123$points == '2'|Test123$points == '3'|Test123$points =='4'|Test123$points =='5'|
                Test123$points =='6'|Test123$points =='7'] <- 'Numbers'

Test123%>%
  group_by(group)

# Changing the column variables from character to numeric structure

Test123$lat = as.numeric(Test123$lat)
Test123$lng = as.numeric(Test123$lng)

str(Test123)


# UI

ui = fluidPage( 
  
  titlePanel("Map"),
  sidebarLayout(
    sidebarPanel(
      sliderInput('radius', 'Circle Radius (meters)', 
                  min = 0, max = 50000, value = 25000, step = 2500),
     radioButtons("radio", "Select:",
               choices = unique(Test123$group)%>%append('All'),
               selected = 'All') 
  ),
  mainPanel( 
    leafletOutput('map',height = '80vh'),
  
  )
  
)
)



# icons for radio buttons based on grouping value in df

icons_list <- iconList(
  Numbers = makeIcon(
    file.path(iconURL = 'C:','Users','jortega','Pictures','skull.png'),
    iconWidth =   20,
    iconHeight =  20),
  
  Letters = makeIcon(
    file.path(iconURL = 'C:','Users','jortega','apache-tomcat-9.0.75-GitHub','apache-tomcat-9.0.75','webapps',
              'epht-view','image','epht-icon.png'),
    iconWidth =  20,
    iconHeight =  20))


# FUNCTION

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    if(input$radio != 'All')
    {
      Test123 <- filter(Test123, group == input$radio)
    }
    
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = Test123,
                 lng = ~lng,
                 lat = ~lat,
                 popup = ~paste(paste('<b>', 'Points:','</b>', points),
                                paste('<b>', 'Latitude:','</b>', lat),
                                paste('<b>', 'Longitude:','</b>', lng),
                                sep = '<br/>'),
                 label = ~points,
                 icon = ~icons_list[group])%>%
      setView(lat = 40.7608, lng = -111.8910, zoom = 7)
      
  })
  
  observeEvent(input$radius, {
    leafletProxy('map')%>%
      clearShapes()%>%
      # I chose lat and lng on canon building with radius of the circle in meters
      addCircles(lat = 40.776772, lng = -111.934669, radius = input$radius, color = 'red',
                 fillColor = 'red',fillOpacity = 0.3)
  })
}
  
 
  


# RUN APP 
shinyApp(ui = ui, server = server)