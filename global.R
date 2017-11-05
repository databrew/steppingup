library(shinydashboard)
library(sp)
library(raster)
library(maptools)
library(googledrive)
library(yaml)
library(stringr)
library(readr)
library(readxl)

# Define whether we want to fetch new data or not
# (useful after a data update, but slow, so generally set to false)
# (set to TRUE only for the first run)
fetch_new <- FALSE

# Get Canadian shapefile
if('map_data.RData' %in% dir('data/geo') & !fetch_new){
  load('data/geo/map_data.RData')
} else {
  # Get a map of canada
  can2 <- raster::getData(name = 'GADM', country = 'CAN', level = 2)
  # Subset to just Toronto
  ont2 <- can2[can2@data$NAME_1 == 'Ontario',]
  # Create a crazy-ass looking version
  ont_crazy <- thinnedSpatialPoly(SP = ont2,
                                  minarea = 0,
                                  tolerance = 5,
                                  topologyPreserve = TRUE)
  # Save for later
  save(ont2, ont_crazy,
       file = 'data/geo/map_data.RData')
}


# Download each file
if('all_drive_data.RData' %in% dir('data/drive') & !fetch_new){
  load('data/drive/all_drive_data.RData')
} else {
  g_files <- drive_find(pattern = 'databrew_youth_compass',
                        n_max = 100)
  1 # use .httr-oauth
  # Download each file
  for (i in 1:nrow(g_files)){
    file_name <- g_files$name[i]
    file_id <- g_files$id[i]
    message('Fetching ', file_name, ' from google drive.')
    drive_download(file = as_id(file_id), 
                   path = paste0('data/drive/', file_name),
                   overwrite = TRUE)
  }
  # Load in each file
  object_names <- c()
  for (i in 1:nrow(g_files)){
    # Get the file name
    this_file <- g_files$name[i]
    # Get the year (if applicable)
    year <- stringr::str_extract(this_file, "[[:digit:]]+")
    # If a csv, clean up the name and then read it in
    if(grepl('.csv', this_file, fixed = TRUE)){
      this_path <- paste0('data/drive/', this_file)
      this_name <- gsub('databrew_youth_compass', '', this_file)
      has_year <- !is.na(year)
      if(has_year){
        this_name <- gsub(paste0('_', year, '_'), '', this_name)
      }
      file_type <- unlist(strsplit(this_name, '.', fixed = TRUE))[2]
      this_name <- gsub(paste0('.', file_type),
                        '', 
                        this_name,
                        fixed = TRUE)
      # Add the year back ot the name
      if(has_year){
        this_name <- paste0(this_name, '_', year)
      }
      
      # Get the column names only
      if(grepl('special', this_path)){
        skipper <- 3
      } else {
        skipper <- 2
      }
      column_names <- read_csv(this_path,
                               col_names = FALSE, 
                               n_max = skipper)
      # Generate better column names  
      new_column_names <- c()
      for(j in 1:ncol(column_names)){
        unlisted <- unlist(column_names[,j])
        unlisted <- unlisted[!is.na(unlisted)]
        new_column_name <- paste0(unlisted, collapse = ' ')
        new_column_names[j] <- new_column_name
      }
      # Read in the full data
      full_data <- read_csv(this_path,
                            col_names = TRUE,
                            skip = skipper - 1)
      # Replace the column names
      names(full_data) <- new_column_names
      # Assign to the global environment
      message('Reading in ',
              this_file, 
              ' as ',
              this_name)
      
      assign(this_name, full_data)
      object_names[i] <- this_name
    }
  }
  object_names <- object_names[!is.na(object_names)]
  save(list = object_names,
       file = 'data/drive/all_drive_data.RData')
} 


crazy_map <- function(){
  # Add some colors
  lots_of_colors <- c(rainbow(100), grey(seq(0.001, 0.999, length = 100)))
  ont_crazy_colors <- sample(lots_of_colors, nrow(ont_crazy))
  plot(ont_crazy, col = ont_crazy_colors)
}
