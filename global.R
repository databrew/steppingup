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
library(broom)


##########
# Source databrew package files
##########
db_files <- dir('R')
for (i in 1:length(db_files)){
  source(paste0('R/', db_files[i]))
}

##########
# Get Canadian shapefile
##########
# https://drive.google.com/file/d/1vew1zjUFJwR6sQJY0OxaczcddW_fJ1fH/view

if('map_data.RData' %in% dir('data/geo')){
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

# Get a fortified version of ont
ont_fortified <- broom::tidy(ont2)
ont_fortified <- ont_fortified %>% left_join(ont2@data %>%
                                               mutate(OBJECTID = as.character(OBJECTID)) %>%
                                               dplyr::select(OBJECTID, CCA_2) %>%
                                               dplyr::rename(id = OBJECTID,
                                                             geography = CCA_2)) %>%
  mutate(geography = as.character(geography))

##########
# Load all census data from census_data folder in data folder
##########
# first 3 are short form census data. usually they accompany the short form with
# the long form, but conservative govt took over in 2011 and they did the
# National Household survey (nhs), completely voluntary.
# Currently we have 9 csvs.
# "2001_census.csv"
# "2006_census.csv"
# "2011_census.csv"
# "2011_nhs_16_19.csv"
# "2011_nhs_20_24.csv"
# "2011_nhs_25_29.csv"
# "2011_nhs_employment.csv"
# "2011_nhs_employment_toronto.csv"

##########
# This function will be used in the get_data function to clean columns and make long
##########
clean_cols_long <- function(x, wide_column_start, dups, name){
  colnames(x) <- tolower(colnames(x))
  colnames(x) <- gsub('x', replacement = '', colnames(x))
  for(i in 1:ncol(x)){
    temp.name <- colnames(x)[i]
    if(grepl('.', temp.name, fixed = T)){
      temp.name_1 <- unlist(strsplit(temp.name, '.', fixed = T))
      temp.name_2 <- paste0(temp.name_1[temp.name_1 != ""], collapse = '_')
      temp.name <- temp.name_2
    }
    colnames(x)[i] <- temp.name
  }
  # make long format
  # find 6 column name and last colum name
  column_start <- colnames(x)[wide_column_start]
  last_column <- colnames(x)[ncol(x)]
  if(dups) {
    x <-  x[,!duplicated(colnames(x))]
    column_start <- colnames(x)[wide_column_start]
    last_column <- colnames(x)[ncol(x)]
  }
  x_long <- gather(x, key, value, column_start:last_column)
  #add a year column
  x_long$year <- as.character(unlist(lapply(strsplit(name, '_'), function(x) x[1])))
  return(x_long)
}

##########
# get non response column and clean geography column
##########

add_non_response <- function(data_frame){

  temp.non_response <- unlist(lapply(unique(strsplit(as.character(data_frame$geography),
                                                     '20000|0000')),
                                     function(x) x[length(x)]))
  temp.geo <- unlist(lapply(unique(strsplit(as.character(data_frame$geography),
                                            '20000|0000')),
                            function(x) x[1]))
  geo_final <- trimws(temp.geo, which = 'both')

  # any with no percent impute 100%
  temp.non_response[!grepl('%', temp.non_response)] <- '0'

  # clean the other
  temp.non_response <- gsub("(", "", temp.non_response, fixed = T)
  temp.non_response <- gsub(")", "", temp.non_response)
  temp.non_response <- gsub("%", "", temp.non_response)
  temp.non_response <- gsub("0 ", "", temp.non_response)
  temp.non_response <- trimws(temp.non_response, 'both')
  temp.non_response <- as.numeric(temp.non_response)

  # put back in data frame
  temp_final <- as.data.frame(cbind(non_response_rate = temp.non_response,
                                    geography = as.character(unique(data_frame$geography))),
                              stringsAsFactors = F)
  temp_final$non_response_rate <- as.numeric(temp_final$non_response)
  # combine with new geography to bring into join to data frame
  temp_final_join <- cbind(temp_final, geo_final)
  data_final <- left_join(data_frame, temp_final_join, by = 'geography')
  data_final$geography <- as.character(data_final$geo_final)
  data_final$value <- as.numeric(data_final$value)
  data_final$geo_final <- NULL

  return(data_final)
}

##########
# function that will take on argument "data_type" which is either census or nhs
# for the time being only use data_type ='census' and this will read in and clean
# and aggregate census data from 2001, 2006, 2011 and combine them into one large data set
# this will be the base data set for the app and my further analysis
# if you do data_type == 'nhs', then it will return a list the 5 nhs data sets we have, cleaned
# and all for 2011. They do not have geo coding census tracks so for now lets stick with just census.
##########

# first get vector of data set names to loop through later
data_names <- list.files('data/census_data')

# data_type = 'nhs'
get_data <- function(data_type) {
  # cread empty list to stor data
  data_list <- list()
  # get data type
  sub_names <- data_names[grepl(data_type, data_names)]
  # name = "2001_census.csv"
  # function that loops through each name in for census data and read into a list
  for (name in sub_names) {
    temp.dat <- read.csv(paste0('data/census_data/', name), stringsAsFactors = F)
    if(data_type == 'census'){
      if(grepl('2011', name)){
        temp.codes <-  gsub("^35$", replacement = "Ontario",
                            gsub(').*', '',
                                 gsub('20000', '', unlist(lapply(strsplit(temp.dat$Geography, '(',
                                                                          fixed = T),
                                                                 function(x) x[2])))))
      } else {
        #subset to Ontarios and 4 digit geo codes
        temp.codes <- gsub(pattern = ')', '',unlist(lapply(strsplit(temp.dat$Geography, '(', fixed = T),
                                                           function(x) x[length(x)])))
      }
      # get index for Ontario and 4 digit codes
      ontario_index <- temp.codes == 'Ontario'
      geo_index <- nchar(temp.codes) == 4
      # subset data by these indices
      temp.dat_on <- temp.dat[ontario_index,]
      temp.dat_geo <- temp.dat[geo_index,]
      # recombined to get a dataset with ontario aggregated and individual census tracks
      temp.dat <- rbind(temp.dat_on,
                        temp.dat_geo)
      # starts at 6 because in this data set only 5 varibles by 1
      temp.dat_long <- clean_cols_long(x = temp.dat, wide_column_start = 6, dups = F, name = name)
      #remove undeeded data
      # rm(temp.dat_geo, temp.dat_on)
      # rename columns
      colnames(temp.dat_long) <- c("geography", "age_group", "sex", "pob", "vis_min","special_ind","value","year")
      # remove white space from all columns
      temp.dat_long <- as.data.frame(apply(temp.dat_long, 2, function(x) trimws(x, 'both')), stringsAsFactors = F)

      # recode sex
      temp.dat_long$sex <- gsub('Females', 'Female', temp.dat_long$sex)
      temp.dat_long$sex <- gsub('Males', 'Male', temp.dat_long$sex)

      # recod pob
      temp.dat_long$pob <- gsub('birth', 'Birth', temp.dat_long$pob)
      temp.dat_long$pob <- gsub('inside', 'in', temp.dat_long$pob)

      data_list[[name]] <- temp.dat_long
    }
    if(data_type == 'nhs') {
      if(grepl('employment', name)) {
        # starts at 4 because in this data set only 4 varibles by 1
        temp.dat_long <- clean_cols_long(temp.dat, wide_column_start = 5, dups = F, name = name)
        colnames(temp.dat_long) <- c("geography", "age_group", "sex", "work_activity", "key","value","year")
      } else {
        # starts at 4 because in this data set only 4 varibles by 1
        temp.dat_long <- clean_cols_long(x = temp.dat, wide_column_start = 2, dups = T, name = name)
        colnames(temp.dat_long) <- c("geography", "key","value","year" )

        # remove white space from all columns
        temp.dat_long <- as.data.frame(apply(temp.dat_long, 2, function(x) trimws(x, 'both')), stringsAsFactors = F)

      }
      data_list[[name]] <- temp.dat_long
    }
    print(name)
  }
  if(data_type == 'census'){
    dat <- as.data.frame(do.call(rbind, data_list), stringsAsFactors = F)
    return(dat)
  } else {
    return(data_list)
  }
}



# apply the function and set "data_type" to census
census_all <- get_data(data_type = "census")

# add non response
census_all <- add_non_response(census_all)

# save data to to "data" folder
saveRDS(census_all, 'data/census_all.rda')



