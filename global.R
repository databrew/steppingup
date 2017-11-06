library(shinydashboard)
library(sp)
library(raster)
library(maptools)
library(googledrive)
library(yaml)
library(stringr)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)

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
      if(grepl('special|birth', this_path)){
        skipper <- 3
      } else {
        skipper <- 2
      }
      if(grepl('birth', this_path) &
         grepl('2011', this_path)){
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
      new_column_names <- gsub(' - Place of birth',
                               '_total', 
                               new_column_names)
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

# Combine into more meaningful datafames
clean_columns_age_sex <- function(x){
  x <- tolower(x)
  x <- gsub(' ', '_', x)
  x <- gsub('-', '', x)
  x <- gsub('__', '_', x)
  x <- gsub('_to_', '_', x)
  x <- gsub('_sex_', '_', x)
  sex <- ifelse(grepl('female', x), 'female', 
                   ifelse(grepl('male', x), 'male',
                          ifelse(grepl('total', x), 'total',
                              NA)))
  age <- ifelse(grepl('15_19', x), '15_19',
                ifelse(grepl('15_24', x), '15_24',
                       ifelse(grepl('20_24', x), '20_24',
                              ifelse(grepl('25_29', x), '25_29',
                                     ifelse(grepl('15_years_and_over', x), '15_up',
                                                  NA)))))
  new_name <- paste0(sex, '_', age)
  new_name <- ifelse(is.na(age), x, new_name)
  return(new_name)
}

# Clean up column names and combine
names(age_sex_2001) <- clean_columns_age_sex(names(age_sex_2001))
names(age_sex_2006) <- clean_columns_age_sex(names(age_sex_2006))
names(age_sex_2011) <- clean_columns_age_sex(names(age_sex_2011))

age_sex <-
  bind_rows(
    age_sex_2001 %>% mutate(year = 2011),
    age_sex_2006 %>% mutate(year = 2006),
    age_sex_2011 %>% mutate(year = 2011)
  )
# Make long
age_sex <-
  age_sex %>%
  gather(age_group, value, total_15_up:female_25_29)

# Separate age and sex
age_sex$sex <- unlist(lapply(strsplit(sort(unique(age_sex$age_group)), split = '_'), function(x){x[1]}))
age_sex$age_group <- unlist(lapply(strsplit(sort(unique(age_sex$age_group)), split = '_'), function(x){paste0(x[2], '_', x[3])}))

# Get rid of the no longer necessary files
rm(age_sex_2001, age_sex_2006, age_sex_2011)

# Clean up the birthplace data

# (have to define special function for birthplace 2011, different format)
clean_columns_birthplace <- function(x){
  sex <- ifelse(grepl('Female', x), 'female',
                ifelse(grepl('Male', x), 'male',
                       ifelse(grepl('Total', x), 'total',
                              ifelse(grepl('Geography', x), NA,
                                     'total'))))
  age <- ifelse(grepl('15 to 24', x), '15_24',
                ifelse(grepl('15 to 19', x), '15_19',
                       ifelse(grepl('20 to 24', x), '20_24',
                              ifelse(grepl('25 to 29', x), '25_29',
                                     ifelse(grepl('15 years and', x), '15_up',
                                            NA)))))
  place <- ifelse(grepl('in', x), 'Canada',
                  ifelse(grepl('out', x), 'Abroad', NA))
  new_name <- paste0(sex, '_', age)
  x <- tolower(x)
  new_name <- ifelse(is.na(age), x, new_name)
  return(new_name)
}
names(birthplace_2001) <- clean_columns_age_sex(names(birthplace_2001))
names(birthplace_2006) <- clean_columns_age_sex(names(birthplace_2006))
names(birthplace_2011) <- clean_columns_age_sex(names(birthplace_2011))

# NEED TO COMBINE AND REFORMAT


# Define a function for creating a crazy looking map
crazy_map <- function(){
  # Add some colors
  lots_of_colors <- c(rainbow(100), grey(seq(0.001, 0.999, length = 100)))
  ont_crazy_colors <- sample(lots_of_colors, nrow(ont_crazy))
  plot(ont_crazy, col = ont_crazy_colors)
}
