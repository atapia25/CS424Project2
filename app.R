#NOTE: File must be saved in UTF-8 encoding to properly work
library(shiny)
#library(farver)
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
reverseStates <- setNames(as.list(state.name), state.abb)

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
                     leafletOutput("leaf2018IL", height = 800)
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
               selectInput("mapType1", "Select a type of map to view",
                           c("Default", "Gray", "Color"), selected = "Default"),
               checkboxGroupInput("checkGroupLeft", "Select which energy
                                  source to view", c("All", energy),
                                  selected = "All"),
               checkboxInput("linker", "Check to link the checkboxes on left
                             and right", value = FALSE)
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
               selectInput("mapType2", "Select a type of map to view",
                           c("Default", "Gray", "Color"), selected = "Default"),
               checkboxGroupInput("checkGroupRight", "Select which energy
                                  source to view", c("All", energy),
                                  selected = "All")
             )
          )
      ),
    tabPanel("US Map",
      column(2,
             fluidRow(
               selectInput("year3", "Select a year to view", years),
               selectInput("state3", "Select a state to view", c("All", allStates),
                           selected = "All"),
               checkboxGroupInput("checkGroupUS", "Select which energy source
                                  to view", c("All", energy)),
               sliderInput("slider1", "Adjust value to view plants that
                           generate energy more than n MWh", min = 1, 
                           max = 31097259, value = 1),
               sliderInput("slider2", "Adjust value to view plants that
                           generate energy less than n MWh", min = 1, 
                           max = 31097259, value = 31097259),
               h2("Do not worry if you see an error on the right screen. Select
                  one of the energy choices to display the leaflet map.")
             )
          ),
      column(10,
             fluidRow(
               leafletOutput("leafTotal", height = 800)
             )
          )
    ),
    tabPanel("About",
             h1("Information about data"),
             p("The data consists of data on electrical power generation
               throughout the US. The data is taken from the EPA website which
               can be found in this ",
               a("link.",
                 href = "https://www.epa.gov/egrid/download-data")),
             br(),
             p("The datasets that were specifically taken were from 2000, 2010,
               and 2018. The specific files needed are eGRID2018v2.xlsx,
               eGRID2000_plant.xls and eGRID2010_plant.xls. The latter two
               are in a .zip file"),
             br(),
             h2("Author of code"),
             p("The code for this Shiny App was written by Andres Tapia. At the time of this release,
              I am currently in my second semester of my third year at the University of Illinois
              at Chicago.")
        )
)

server <- function(input, output, session) {
  
  ### Part 1 Data ###
  #Illinois map
  ILReactive <- reactive({subset(egrid2018IL, egrid2018IL$Type %in% 
                                   input$checkGroup1)})
  
  ### Part 2 Data ###
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
  
  ### Part 3 Data ###
  PlantReactive2000 <- reactive({
    if (input$state3 == "All")
    {
      egrid2000v3[egrid2000v3$Type %in% input$checkGroupUS & 
                    egrid2000v3$value > input$slider1 &
                    egrid2000v3$value < input$slider2,]
    }
    else
    {
      egrid2000v3[egrid2000v3$State == states[[input$state3]]
                  & egrid2000v3$Type %in% input$checkGroupUS
                  & egrid2000v3$value > input$slider1
                  & egrid2000v3$value < input$slider2,]
    }
  })
  
  PlantReactive2010 <- reactive({
    if (input$state3 == "All")
    {
      egrid2010v3[egrid2010v3$Type %in% input$checkGroupUS &
                    egrid2010v3$value > input$slider1 &
                    egrid2010v3$value < input$slider2,]
    }
    else
    {
      egrid2010v3[egrid2010v3$State == states[[input$state3]]
                  & egrid2010v3$Type %in% input$checkGroupUS
                  & egrid2010v3$value > input$slider1
                  & egrid2010v3$value < input$slider2,]
    }
  })
  
  PlantReactive2018 <- reactive({
    if (input$state3 == "All")
    {
      egrid2018v3[egrid2018v3$Type %in% input$checkGroupUS &
                    egrid2018v3$value > input$slider1,]
    }
    else
    {
      egrid2018v3[egrid2018v3$State == states[[input$state3]]
                  & egrid2018v3$Type %in% input$checkGroupUS &
                    egrid2018v3$value > input$slider1,]
    }
  })
  ### ####
  
  #This is for the left part of the screen
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
  
  #This is for the third task which is to output the data for the whole US
  datasetInput3 <- reactive({
    if (input$year3 == 2000)
    {
      dataset <- PlantReactive2000()
    }
    else if (input$year3 == 2010)
    {
      dataset <- PlantReactive2010()
    }
    else if (input$year3 == 2018)
    {
      dataset <- PlantReactive2018()
    }
    return(dataset)
  })
  
  #This links the two checkboxes
  observe({
    x <- input$linker
    if(isTRUE(x))
    {
      updateCheckboxGroupInput(session, "checkGroupRight", 
                               selected = input$checkGroupLeft)
    }
  })
  
  ### Illinois Map ###
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
  
  #Left 
  output$leafLeft <- renderLeaflet({
    plantData <- datasetInput1()
    if(input$checkGroupLeft == "All")
    {
      updateCheckboxGroupInput(session, "checkGroupLeft", 
                               selected = c("All", energy))
    }
    map <- leaflet(plantData)
    if (input$mapType1 == "Default")
    {
      map <- addTiles(map)
    }
    else if (input$mapType1 == "Gray")
    {
      map <- addProviderTiles(map, providers$CartoDB.Positron)
    }
    else if (input$mapType1 == "Color")
    {
      map <- addProviderTiles(map, providers$Esri.NatGeoWorldMap)
    }
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
  
  #Right half of split screen
  output$leafRight <- renderLeaflet({
    plantData <- datasetInput2()
    if(input$checkGroupRight == "All")
    {
      updateCheckboxGroupInput(session, "checkGroupRight", 
                               selected = c("All", energy))
    }
    map <- leaflet(plantData)
    if (input$mapType2 == "Default")
    {
      map <- addTiles(map)
    }
    else if (input$mapType2 == "Gray")
    {
      map <- addProviderTiles(map, providers$CartoDB.Positron)
    }
    else if (input$mapType2 == "Color")
    {
      map <- addProviderTiles(map, providers$Esri.NatGeoWorldMap)
    }
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
  
  #For the total US
  output$leafTotal <- renderLeaflet({
    plantData <- datasetInput3()
    if(input$checkGroupUS == "All")
    {
      updateCheckboxGroupInput(session, "checkGroupUS", 
                               selected = c("All", energy))
    }
    map <- leaflet(plantData)
    map <- addTiles(map)
    map <- addCircles(map,
                      lng = plantData$Longitude,
                      lat = plantData$Latitude,
                      color = colorFactors(plantData$Type),
                      popup = paste("Value:", plantData$value, "MWh <br>",
                                    "Name:", plantData$Name, "<br>",
                                    "Type of Energy:", plantData$Type, "<br>",
                                    "State:", plantData$State))
    map <- addResetMapButton(map)
    map <- addLegend(map, "topright", colorFactors, values = plantData$Type)
    map
  })
}

shinyApp(ui = ui, server = server)