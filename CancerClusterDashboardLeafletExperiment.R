# a working code for and app shiny project featuring whatever data we want to use a lot of packages were not initially installed for my R
# preliminary code not yet to be published

library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(bslib)
library(ggiraph)
library(tigris)
library(leaflet)
library(sf)
library(tidyverse)
library(shiny)
library(tigris)


## pulled: Cancer <- ho_cancer_std data from our internal data warehouse, the following code below can also be used for any data just adjusting variables and df

## option using read_sf
# unzip('C:\\Users\\jortega\\cb_2018_us_county_500k.zip')
# dat3 <- read_sf('cb_2018_us_county_500k.shp')

## option using 'tigris' package
# utah <- tigris::counties(state = "UT")
# zcta_data <- zctas(cb = F, state = "UT", year = 2010)


Cancer$county = as.character(Cancer$county)

Cancer$county[Cancer$county == '1'] <- 'Beaver'
Cancer$county[Cancer$county == '3'] <- 'Box Elder'
Cancer$county[Cancer$county == '5'] <- 'Cache'
Cancer$county[Cancer$county == '7'] <- 'Carbon'
Cancer$county[Cancer$county == '9'] <- 'Daggett'
Cancer$county[Cancer$county == '11'] <- 'Davis'
Cancer$county[Cancer$county == '13'] <- 'Duchesne'
Cancer$county[Cancer$county == '15'] <- 'Emery'
Cancer$county[Cancer$county == '17'] <- 'Garfield'
Cancer$county[Cancer$county == '19'] <- 'Grand'
Cancer$county[Cancer$county == '21'] <- 'Iron'
Cancer$county[Cancer$county == '23'] <- 'Juab'
Cancer$county[Cancer$county == '25'] <- 'Kane'
Cancer$county[Cancer$county == '27'] <- 'Millard'
Cancer$county[Cancer$county == '29'] <- 'Morgan'
Cancer$county[Cancer$county == '31'] <- 'Piute'
Cancer$county[Cancer$county == '33'] <- 'Rich'
Cancer$county[Cancer$county == '35'] <- 'Salt Lake'
Cancer$county[Cancer$county == '37'] <- 'San Juan'
Cancer$county[Cancer$county == '39'] <- 'Sanpete'
Cancer$county[Cancer$county == '41'] <- 'Sevier'
Cancer$county[Cancer$county == '43'] <- 'Summit'
Cancer$county[Cancer$county == '45'] <- 'Tooele'
Cancer$county[Cancer$county == '47'] <- 'Uintah'
Cancer$county[Cancer$county == '49'] <- 'Utah'
Cancer$county[Cancer$county == '51'] <- 'Wasatch'
Cancer$county[Cancer$county == '53'] <- 'Washington'
Cancer$county[Cancer$county == '55'] <- 'Wayne'
Cancer$county[Cancer$county == '57'] <- 'Weber'
Cancer$county[Cancer$county == '99'] <- 'Unknown'

Cancer$sitegrp1[Cancer$sitegrp1 == '17'] <- 'Cutaneous melanoma'

## pulled cancer data from our internal data warehouse, the following code below can also be used for any data just adjusting variables and df
## Creating age group variable consistent with age groups we use for cancer on our portal
## our cancer data does already have a column with 5 year age groups but I am adding a new variable using the age column but age5 could be renamed
## so that the data in the column actually reflects an age range instead of just a number

Cancer$age_group[Cancer$age >= '0' & Cancer$age <= '4'] <- '0-4'
Cancer$age_group[Cancer$age >= '5' & Cancer$age <= '14'] <- '5-14'
Cancer$age_group[Cancer$age >= '15' & Cancer$age <= '24'] <- '15-24'
Cancer$age_group[Cancer$age >= '25' & Cancer$age <= '34'] <- '25-34'
Cancer$age_group[Cancer$age >= '35' & Cancer$age <= '44'] <- '35-44'
Cancer$age_group[Cancer$age >= '45' & Cancer$age <= '54'] <- '45-54'
Cancer$age_group[Cancer$age >= '55' & Cancer$age <= '64'] <- '55-64'
Cancer$age_group[Cancer$age >= '65' & Cancer$age <= '74'] <- '65-74'
Cancer$age_group[Cancer$age >= '75' & Cancer$age <= '84'] <- '75-84'
Cancer$age_group[Cancer$age >= '85'] <- '85+'

##reformatting variable

Cancer$dxyear <- as.character(Cancer$dxyear)


## Creating cancer.test DF only showing a small range of dx years and making lat lng measurements numerical

cancer.test <- Cancer[Cancer$dxyear >= 2015 & Cancer$dxyear <= 2020,]
cancer.test <- cancer.test %>% group_by(county, dxyear) %>%
  arrange(dxyear, age_group, sitegrp1)
cancer.test$geo_x_dd <- as.numeric(cancer.test$geo_x_dd)
cancer.test$geo_y_dd <- as.numeric(cancer.test$geo_y_dd)

#read_sf package

#utah <- dat3 %>%
  #filter(STATEFP == '49')

pal <- colorFactor("Pastel1", domain = utah$NAME)
pal1<- colorFactor("Pastel1", domain = zcta_data$ZCTA5CE10)



## Dashboard beginning

header <- dashboardHeader(title ="Cancer Dashboard",
                          titleWidth = 500)
sidebar <- dashboardSidebar(
  sidebarMenu(id = "pages",
              menuItem("Historical Data by County", tabName = 'historical',
                       icon = icon('chart-line')),
              menuItem("Cancer by Site and Year", tabName = "tab2", icon = icon("heart")),
              menuItem('Map', tabName = 'map', icon = icon('globe')),
              menuItem('Table', tabName= 'table', icon = icon('table')),
              menuItem('Map With Markers', tabName = 'markers', icon = icon('globe')),
              menuItem('Zip Code Map', tabName = 'zip', icon = icon('globe'))
  )
)

##First Tab 

body <- dashboardBody(
  tabItems(
    tabItem("historical", fluidRow(titlePanel('County')),
            plotlyOutput("plot1",height = '700')),
    
    ##Second Tab
    
    tabItem("tab2", fluidRow(selectInput(
      inputId = "site",
      label = "Cancer Location",
      choices = unique(cancer.test$sitegrp1)%>%append('All'), 
      selected = 'All', 
      selectize = TRUE,
      multiple = FALSE
    ),
    
    
    selectInput(
      inputId = "year",
      label = "Year",
      choices = unique(cancer.test$dxyear)%>%append('All'),
      selected = 'All',
      selectize = TRUE,
      multiple = FALSE
    ),
    
    selectInput(
      inputId = "group",
      label = "Age Group",
      choices = unique(cancer.test$age_group) %>% append('All'),
      selected = 'All',
      selectize = TRUE,
      multiple = FALSE
    )),
    fluidRow(infoBoxOutput('infobox')),
    
    plotlyOutput("plot2")),
    
    ##Third Tab
    
    tabItem(tabName = 'map', 
            fluidRow(titlePanel('Map')),
            fluidRow(leafletOutput('map')),   
            fluidRow(column(width = 4,
                              selectInput('countyselect','County:',width = NULL, 
                                           choices = unique(cancer.test$county),
                                           selected = NULL)),
            
              column(width = 4,
                     selectInput("cancerselect", "Select:", width = NULL,
                                  choices = unique(cancer.test$sitegrp1)%>%append('All'),
                                  selected = '10')),
           
              column(width = 4,
                              selectInput('yearselect', 'Year:', width = NULL,
                                           choices = unique(cancer.test$dxyear) %>% append('All'),
                                           selected = 'All')))
            ),
    
    ##fourth tab showing a data table of df
    
    tabItem(tabName = 'table',
            dashboardHeader(title = 'Cancer', titleWidth = 750),
            dashboardBody(fluidRow(
              column(width = 4,
                     selectInput(
                       inputId = "county",
                       label = "County",
                       choices = unique(Cancer$county)
                     )),
              column(width = 4,
                     selectInput(
                       inputId = "site4",
                       label = "Cancer Location",
                       choices = unique(Cancer$sitegrp1)
                     )),
              sliderInput(
                inputId = "age",
                label = "age",
                min = 0,
                max = 100,
                value = 100,
                step = 10
              ),
              
              DT::DTOutput('table')))),
    
##fifth tab showing map with markers
    
    tabItem(tabName = 'markers', 
            fluidRow(titlePanel('Map w/markers')),
            fluidRow(leafletOutput('markers')),
            fluidRow(
              column(width = 4,
                     radioButtons("radio", "Select:", width = NULL,
                                  choices = unique(cancer.test$sitegrp1)%>%append('All'),
                                  selected = '10')),
              column(width = 4,
                     radioButtons('radio1', 'Year:', width = NULL,
                                  choices = unique(cancer.test$dxyear) %>% append('All'),
                                  selected = 'All')))
    ),

## sixth tab showing leaflet map with zip codes

tabItem(tabName = 'zip',
        fluidRow(titlePanel('Map by zip code')),
        fluidRow(leafletOutput('zip')),
        fluidRow(column(width = 4,
                        selectInput('zipselect','Zip Code:',width = NULL, 
                                    choices = unique(cancer.test$zip5) %>% append('All'),
                                    selected = 'All')),
                 
                 column(width = 4,
                        selectInput("cancerselect", "Select:", width = NULL,
                                    choices = unique(cancer.test$sitegrp1),
                                    selected = NULL)),
                 
                 column(width = 4,
                        selectInput('yearselect', 'Year:', width = NULL,
                                    choices = unique(cancer.test$dxyear) %>% append('All'),
                                    selected = 'All')))
)
    )
)



ui <- dashboardPage(header, sidebar, body)

server <-function(input, output, session){
  
  ##First Tab Graph
  
  output$plot1 <- renderPlotly({
    
    historical <- ggplot(Cancer, aes(dxyear, fill = county)) + geom_bar() +  
      facet_wrap(~county, scales = 'free') + theme(legend.position = "none")
    
    ggplotly(historical)
  }) 
  
  ##Second Tab Graph
  
  output$infobox <- renderValueBox({
    valueBox(
      paste0(input$site), "Site of Cancer", icon = icon("chart-simple"),
      color = "green"
    )
  })
  
  output$plot2 <- renderPlotly({
    if (input$site != 'All')
    {
      cancer.test <- filter(cancer.test, sitegrp1 == input$site)
    }
    if (input$year != 'All')
    {
      cancer.test <- filter(cancer.test, dxyear == input$year)
    }
    if (input$group != 'All')
    {
      cancer.test <- filter(cancer.test, age_group == input$group)
    }
    
    LHD <- ggplot(cancer.test, aes(county, fill=county)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) +
      labs(title='Cancer by County', y='Count', x='County')
    ggplotly(LHD)
  })
  
  ##Third Tab Graph (Current UCR data does not look to be geocoded so no data point on leaflet map until lat and long can be added to df)
  
  output$map <- renderLeaflet({
    
    if(input$cancerselect != 'All')
    {
      cancer.test <- filter(cancer.test, sitegrp1 == input$cancerselect)
    }
    if(input$yearselect != 'All')
    {
      cancer.test <- filter(cancer.test, dxyear == input$yearselect)
    }
    
    {
      cancer.test <-  filter(cancer.test, county == input$countyselect)
    }
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap)%>%
      addPolygons(data = utah,
                  fillColor = ~pal(NAME),
                  fillOpacity = 1,
                  weight = 1,
                  color = 'black',
                  layerId = ~NAME,
                  label = ~NAME,
                  popup = paste(paste('<b>', 'County:','</b>', input$countyselect),
                                paste('<b>', 'Cancer:','</b>', input$cancerselect),
                                paste('<b>', 'Count:','</b>', nrow(cancer.test)),
                                paste('<b>', 'Year:','</b>', input$yearselect),
                                sep = '<br/>'),
                  highlightOptions = highlightOptions(color='red', weight = 4),
                  labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = 'center',
                                              style = list('color'='black')))%>%
      setView(lat = 40.7608, lng = -111.8910, zoom = 6)
  })
  observe({
    
    ## the map needs to match the name of the map you're outputting above
    event <- input$map_shape_click
    print(event$id)
    updateSelectInput(session, inputId = "countyselect", selected = event$id
    )
    
  }) 

  
  ##fourth tab
  
  output$table <- DT::renderDT({
    
    # Filter data based on selected Style
    
    Cancer %>%
      filter(county == input$county) %>%
      filter(age <= input$age) %>%
      filter(sitegrp1 == input$site4)
  })
  
  ##fifth tab
  
  output$markers <- renderLeaflet({
    if(input$radio != 'All')
    {
      cancer.test <- filter(cancer.test, sitegrp1 == input$radio)
    }
    if(input$radio1 != 'All')
    {
      cancer.test <- filter(cancer.test, dxyear == input$radio1)
    }
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = cancer.test,
                 lng = ~geo_x_dd,
                 lat = ~geo_y_dd,
                 popup = ~sitegrp1,
                 label = ~dxyear,)%>%
      setView(lat = 40.7608, lng = -111.8910, zoom = 4)
  })
  
  
  ##sixth tab
  output$zip <- renderLeaflet({
    
    {
      cancer.test <- filter(cancer.test, sitegrp1 == input$cancerselect)
    }
    if(input$yearselect != 'All')
    {
      cancer.test <- filter(cancer.test, dxyear == input$yearselect)
    }
    if(input$zipselect != 'All')
    {
      zcta_data <-  filter(zcta_data, ZCTA5CE10 == input$zipselect)
    }
    {
      cancer.test <- filter(cancer.test, zip5 == input$zipselect )
    }
  leaflet() %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addPolygons(data=zcta_data,
                fillColor = ~pal1(zcta_data$ZCTA5CE10),
                fillOpacity = 0.5,
                weight = 1,
                layerId = ~ZCTA5CE10,
                label = ~ZCTA5CE10,
                popup = paste(paste('<b>', 'Zip Code:','</b>', input$zipselect),
                              paste('<b>', 'Cancer:','</b>', input$cancerselect),
                              paste('<b>', 'Count:','</b>', nrow(cancer.test)),
                              paste('<b>', 'Year:','</b>', input$yearselect),
                              sep = '<br/>'),
                highlightOptions = highlightOptions(color='red', weight = 4))
  })

  observe({
    
    ## the map needs to match the name of the map you're outputting above
    
    event <- input$zip_shape_click
    print(event$id)
    updateSelectInput(session, inputId = "zipselect", selected = event$id
    )
    
  }) 
  
}




shinyApp(ui=ui, server=server)

