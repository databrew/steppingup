library(shinydashboard)
library(sp)
library(raster)
library(maptools)

if('map_data.RData' %in% dir('data/geo')){
  load('data/geo/map_data.RData')
} else {
  # Get a map of canada
  # can0 <- raster::getData(name = 'GADM', country = 'CAN', level = 0)
  # can1 <- raster::getData(name = 'GADM', country = 'CAN', level = 1)
  # can2 <- raster::getData(name = 'GADM', country = 'CAN', level = 2)
  # can3 <- raster::getData(name = 'GADM', country = 'CAN', level = 3)
  # Subset to just Ontario
  # ont1 <- can1[can1@data$NAME_1 == 'Ontario',]
  ont2 <- can2[can2@data$NAME_1 == 'Ontario',]
  # ont3 <- can3[can3@data$NAME_1 == 'Ontario',]
  
  # Create a crazy-ass looking version
  ont_crazy <- thinnedSpatialPoly(SP = ont2,
                                  minarea = 0,
                                  tolerance = 5,
                                  topologyPreserve = TRUE)

  # save(can0,can1,can2,can3,ont1,ont2,ont3,ont_crazy,ont_crazy_colors,
  save(ont2, ont_crazy,
       file = 'data/geo/map_data.RData')
}

crazy_map <- function(){
  # Add some colors
  lots_of_colors <- c(rainbow(100), grey(seq(0.001, 0.999, length = 100)))
  ont_crazy_colors <- sample(lots_of_colors, nrow(ont_crazy))
  plot(ont_crazy, col = ont_crazy_colors)}
