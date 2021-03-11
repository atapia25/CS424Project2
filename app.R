#NOTE: File must be saved in UTF-8 encoding to properly work
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(dplyr)

egrid2018 <- read.csv("egrid2018_data_v3.csv", header = TRUE, sep = ",")

egrid2018 <- egrid2018 %>%
  dplyr::rename(State = ï..State)

egrid2018v2 <- melt(data = egrid2018, id.vars = c("State", "Name", "Latitude",
                                                  "Longitude"))
egrid2018v2$variable <- as.character(egrid2018v2$variable)
egrid2018v2$Type <- factor(egrid2018v2$variable)

type <- as.factor(egrid2018v2$variable)

egrid2018v3 <- dcast(egrid2018v2, State+Name+Latitude+Longitude~variable)

egrid2018v3 <- subset(egrid2018v3, Hydro > 0 | Coal > 0 | Gas > 0 | Oil > 0 |
                        Biomass > 0 | Geothermal > 0 | Nuclear > 0)

egrid2018v4 <- subset(egrid2018v2, value > 0)

colorFactors <- colorFactor(c("red", "black", "#0072B2", "#E69F00", "#56B4E9",
                              "#CC79A7", "#D55E00", "#009E73", "#F0E442", "#999999"),
                            domain = egrid2018v4$Type)

egrid2018IL <- subset(egrid2018v4, State == "IL")

energy <- levels(type)

years <- c(2000, 2010, 2018)

ui <- dashboardPage(
    dashboardHeader(title = "CS 424 Project Two"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     #for the menu on the left hand side
                     sidebarMenu(
                       menuItem("Map of Illinois 2018", tabName = "task1"),
                       menuItem("State Comparison", tabName = "task2"),
                       menuItem("About", tabName = "about")
                     )
    ),
    dashboardBody(
      tabItem(tabName = "task1",
        column(2,
               fluidRow(
                checkboxGroupInput("checkGroup1",
                                   "Select the types of plants to view",
                                   choices = c("All", "Renewable", 
                                               "Nonrenewable", energy),
                                   selected = "All") 
                )
              ),
        column(10,
               fluidRow(
                 box(title = "Map of Illinois in 2018", solidHeader = TRUE, status = "primary", width = 12,
                     leafletOutput("leaf2018IL", height = 800)
                  )
                )
              )
            )
        )
)

server <- function(input, output, session) {
  
  
  
  #Illinois map
  ILReactive <- reactive({subset(egrid2018IL, egrid2018IL$Type %in% 
                                   input$checkGroup1)})
  
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
  
}

shinyApp(ui = ui, server = server)