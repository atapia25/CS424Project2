#NOTE: File must be saved in UTF-8 encoding to properly work
library(shiny)
library(farver)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(reshape)
library(reshape2)

egrid2018 <- read.csv("egrid2018_data_v3.csv", header = TRUE, sep = ",")
egrid2000 <- read.csv("egrid2000_data_v2.csv", header = TRUE, sep = ",")
egrid2010 <- read.csv("egrid2010_data_v2.csv", header = TRUE, sep = ",")

#there is an extra character added to the State column header, so remove it
egrid2018 <- egrid2018 %>%
  dplyr::rename(State = ï..State)
egrid2000 <- egrid2000 %>%
  dplyr::rename(State = ï..State)
egrid2010 <- egrid2010 %>%
  dplyr::rename(State = ï..State)

#2018 data
egrid2018v2 <- melt(data = egrid2018, id.vars = c("State", "Name", "Latitude",
                                                  "Longitude"))
egrid2018v2$variable <- as.character(egrid2018v2$variable)
egrid2018v2$Type <- factor(egrid2018v2$variable)
egrid2018v2$value <- gsub(',', '', egrid2018v2$value)
egrid2018v2$value <- as.numeric(egrid2018v2$value)

#2000 data
egrid2000v2 <- melt(data = egrid2000, id.vars = c("State", "Name", "Latitude",
                                                  "Longitude"))
egrid2000v2$variable <- as.character(egrid2000v2$variable)
egrid2000v2$Type <- factor(egrid2000v2$variable)

#2010 data
egrid2010v2 <- melt(data = egrid2010, id.vars = c("State", "Name", "Latitude",
                                                  "Longitude"))
egrid2010v2$variable <- as.character(egrid2010v2$variable)
egrid2010v2$Type <- factor(egrid2010v2$variable)
egrid2010v2$value <- gsub(',', '', egrid2010v2$value)
egrid2010v2$value <- as.numeric(egrid2010v2$value)

egrid2018v3 <- subset(egrid2018v2, value > 0)
#2000's data needs a lot of converting
egrid2000v3 <- subset(egrid2000v2, value > 1 & Longitude != "N/A"
                      & Latitude != "N/A")
egrid2000v3$Latitude <- as.numeric(egrid2000v3$Latitude)
egrid2000v3$Longitude <- as.numeric(egrid2000v3$Longitude)
egrid2000v3$Longitude <- egrid2000v3$Longitude * (-1)
egrid2000v3$value <- as.numeric(egrid2000v3$value)

egrid2010v3 <- subset(egrid2010v2, value > 0)

colorFactors <- colorFactor(c("red", "black", "#0072B2", "#E69F00", "#56B4E9",
                              "#CC79A7", "#D55E00", "#009E73", "#F0E442", "#999999"),
                            domain = egrid2018v3$Type)

type <- as.factor(egrid2018v3$Type)

egrid2018IL <- subset(egrid2018v3, State == "IL")

energy <- levels(type)

years <- c(2000, 2010, 2018)

allStates <- state.name
states <- setNames(as.list(state.abb), state.name)

ui <- navbarPage("CS 424 Project Two",
    tabPanel("Map of Plants in Illinois",
        column(2,
               fluidRow(
                checkboxGroupInput("checkGroup1",
                                   "Select the types of plants to view",
                                   choices = c("All", "Renewable", 
                                               "Nonrenewable", energy),
                                   selected = "All"),
                h2("Note: Renewable must be unchecked if you wish to view Nonrenewable
                   and both must be unchecked to view sources freely")
                )
              ),
        column(10,
               fluidRow(
                 box(title = "Map of Illinois in 2018", solidHeader = TRUE, status = "primary", width = 12,
                     leafletOutput("leaf2018IL", height = 800)
                  )
                )
              )
            ),
    tabPanel("State Comparison",
      column(1,
             fluidRow(
               selectInput("year1", "Select a year to view", years, 
                           selected = 2000),
               selectInput("state1", "Select a state to view", allStates,
                           selected = "Illinois"),
               checkboxGroupInput("checkGroupLeft", "Select which energy
                                  source to view", c("All", energy),
                                  selected = "All")
             )
          ),
      column(5,
             fluidRow(
               leafletOutput("leafLeft", height = 800)
             )
          ),
      column(5,
             fluidRow(
               leafletOutput("leafRight", height = 800)
             )
          ),
      column(1,
             fluidRow(
               selectInput("year2", "Select a year to view", years,
                           selected = 2018),
               selectInput("state2", "Select a state to view", allStates,
                           selected = "Illinois"),
               checkboxGroupInput("checkGroupRight", "Select which energy
                                  source to view", c("All", energy),
                                  selected = "All")
             )
          )
      )
)

server <- function(input, output, session) {
  
  
  
  #Illinois map
  ILReactive <- reactive({subset(egrid2018IL, egrid2018IL$Type %in% 
                                   input$checkGroup1)})
  
  #Left half of the screen
  LeftReactive2000 <- reactive({egrid2000v3[egrid2000v3$State == states[[input$state1]]
                                            & egrid2000v3$Type %in% input$checkGroupLeft,]})
  
  LeftReactive2010 <- reactive({egrid2010v3[egrid2010v3$State == states[[input$state1]]
                                            & egrid2010v3$Type %in% input$checkGroupLeft,]})
  
  LeftReactive2018 <- reactive({egrid2018v3[egrid2018v3$State == states[[input$state1]]
                                            & egrid2018v3$Type %in% input$checkGroupLeft,]})
  
  #Right half of the screen
  RightReactive2000 <- reactive({egrid2000v3[egrid2000v3$State == states[[input$state2]]
                                             & egrid2000v3$Type %in% input$checkGroupRight,]})
  
  RightReactive2010 <- reactive({egrid2010v3[egrid2010v3$State == states[[input$state2]]
                                             & egrid2010v3$Type %in% input$checkGroupRight,]})
  
  RightReactive2018 <- reactive({egrid2018v3[egrid2018v3$State == states[[input$state2]]
                                             & egrid2018v3$Type %in% input$checkGroupRight,]})
  
  datasetInput1 <- reactive({
    if (input$year1 == 2000)
    {
      dataset <- LeftReactive2000()
    }
    else if (input$year1 == 2010)
    {
      dataset <- LeftReactive2010()
    }
    else if (input$year1 == 2018)
    {
      dataset <- LeftReactive2018()
    }
    return(dataset)
  })
  
  #This is for the right half of the screen, changes datasets correctly
  datasetInput2 <- reactive({
    if (input$year2 == 2000)
    {
      dataset <- RightReactive2000()
    }
    else if (input$year2 == 2010)
    {
      dataset <- RightReactive2010()
    }
    else if (input$year2 == 2018)
    {
      dataset <- RightReactive2018()
    }
    return(dataset)
  })
  
  output$leaf2018IL <- renderLeaflet({
    ILMap <- ILReactive()
    if(input$checkGroup1 == "All")
    {
      #if the user just has All selected
      updateCheckboxGroupInput(session, "checkGroup1", 
                               selected = c("All", "Renewable", 
                                            "Nonrenewable", energy))
    }
    if(input$checkGroup1 == "Renewable" & input$checkGroup1 == "Nonrenewable")
    {
      updateCheckboxGroupInput(session, "checkGroup1",
                               selected = c("Renewable", "Nonrenewable",
                                            energy))
    }
    else
    {
      if(input$checkGroup1 == "Renewable")
      {
        updateCheckboxGroupInput(session, "checkGroup1",
                                 selected = c("Renewable", 
                                              "Biomass", "Geothermal",
                                              "Solar", "Wind"))
      }
      else if(input$checkGroup1 == "Nonrenewable")
      {
        updateCheckboxGroupInput(session, "checkGroup1",
                                 selected = c("Nonrenewable",
                                              "coal", "Gas", "Hydro", "Nuclear",
                                              "Oil", "Other"))
      }
    }
    map <- leaflet(ILMap)
    map <- addTiles(map)
    map <- addCircles(map,
                      lng = ILMap$Longitude,
                      lat = ILMap$Latitude,
                      color = colorFactors(ILMap$Type))
    map <- addResetMapButton(map)
    map <- addLegend(map, "topright", colorFactors, values = ILMap$Type)
    map
  })
  
  output$leafLeft <- renderLeaflet({
    plantData <- datasetInput1()
    if(input$checkGroupLeft == "All")
    {
      updateCheckboxGroupInput(session, "checkGroupLeft", 
                               selected = c("All", energy))
    }
    map <- leaflet(plantData)
    map <- addTiles(map)
    map <- addCircles(map,
                      lng = plantData$Longitude,
                      lat = plantData$Latitude,
                      radius = (plantData$value * 0.001),
                      color = colorFactors(plantData$Type),
                      popup = paste("Value:", plantData$value, "MWh <br>",
                                    "Name:", plantData$Name, "<br>",
                                    "Type of Energy:", plantData$Type))
    map <- addResetMapButton(map)
    map <- addLegend(map, "topright", colorFactors, values = plantData$Type)
    map
  })
  
  output$leafRight <- renderLeaflet({
    plantData <- datasetInput2()
    if(input$checkGroupRight == "All")
    {
      updateCheckboxGroupInput(session, "checkGroupRight", 
                               selected = c("All", energy))
    }
    map <- leaflet(plantData)
    map <- addTiles(map)
    map <- addCircles(map,
                      lng = plantData$Longitude,
                      lat = plantData$Latitude,
                      radius = (plantData$value * 0.001),
                      color = colorFactors(plantData$Type),
                      popup = paste("Value:", plantData$value, "MWh <br>",
                                    "Name:", plantData$Name, "<br>",
                                    "Type of Energy:", plantData$Type))
    map <- addResetMapButton(map)
    map <- addLegend(map, "topright", colorFactors, values = plantData$Type)
    map
  })
  
}

shinyApp(ui = ui, server = server)