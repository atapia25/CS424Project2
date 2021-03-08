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

years <- c(2000, 2010, 2018)

ui <- dashboardPage(
    dashboardHeader(title = "CS 424 Project Two"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     #for the menu on the left hand side
                     sidebarMenu(
                       menuItem("Map of Illinois 2018", tabName = "task1"),
                       menuItem("About", tabName = "about")
                     )
    ),
    dashboardBody(leafletOutput("leaf2018IL"))
)

server <- function(input, output, session) {
  
  output$leaf2018IL <- renderLeaflet({
    map <- leaflet(egrid2018IL)
    map <- addTiles(map)
    map <- addCircles(map,
                      lng = egrid2018IL$Longitude,
                      lat = egrid2018IL$Latitude,
                      color = colorFactors(egrid2018IL$Type))
    map <- addResetMapButton(map)
    map <- addLegend(map, "topright", colorFactors, values = egrid2018IL$Type)
    map
  })
  
}

shinyApp(ui = ui, server = server)