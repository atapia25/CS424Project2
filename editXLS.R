library(leaflet)
library(dplyr)
library(reshape)
library(reshape2)
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

map_color <- c("Biomass" = "red", "Coal" = "black", "Gas" = "#0072B2",
               "Geothermal" = "#E69F00", "Hydro" = "#56B4E9", 
               "Nuclear" = "#CC79A7", "Oil" = "#D55E00", "Other" = "#009E73",
               "Solar" = "#F0E442", "Wind" = "#999999")

egrid2018IL <- subset(egrid2018v4, State == "IL")

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = colorFactors(egrid2018v4$Type),
  library = 'ion',
  markerColor = 'white'
)

## Circle Markers work for now, but I would like to use Awesome Markers because it
## would be easier for the user to see instead of just dots on a map
#map <- leaflet(egrid2018IL)
#map <- addTiles(map)
#map <- addCircles(map,
#                  lng = egrid2018IL$Longitude,
#                  lat = egrid2018IL$Latitude,
#                  fillOpacity = 1.0,
#                  color = colorFactors(egrid2018IL$Type))
#map <- addLegend(map, "topright", colorFactors, values = egrid2018IL$Type)
#map